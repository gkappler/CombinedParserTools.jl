
export regex_neg_lookahead, regex_tempered_greedy
regex_neg_lookahead(e,match=AnyChar()) =
    !Repeat(Sequence(NegativeLookahead(e),match))
regex_tempered_greedy(s,e, flags="s"; withend=true) =
    Sequence(
        2,s,
        !Repeat(NegativeLookahead(e),AnyChar()),
        ( withend ? e : Always()))
    # Regex("^"*regex_string(s)*"((?:(?!"*regex_string(e)*").)*)"*
    #       ( withend ? regex_string(e) : ""),flags)



export alternate, alternate_stop
alternate_stop(x,delim,stop;kw...) =
    alternate(seq(NegativeLookahead(stop), x; transform=2),
              seq(NegativeLookahead(stop), delim; transform=2);
              kw...)

alternate(x::Vector, delim; kw...) = alternate(Either(x...), delim; kw...)
"""
optimized repeated alternations of `x``delim`, optionally starting/ending with `delim`. `delim` `is agg`ed as right borders. 
`delim` can be discarded in agg(result,missing,delim).

if `agg` is nothing, default is to aggregate delim after match is `result_type(delim) <: result_type(x)`, if not missing.
"""
function alternate(x, delim;
                   agg = nothing,
                   kw...)
    @show T, S = result_type(typeof(x)), result_type(typeof(delim))
    af = if agg === nothing
        if S <: T
            ( r, xmatch, delimmatch ) -> begin
                xmatch !== missing && push!(r,xmatch)
                delimmatch !== missing && push!(r,delimmatch)
                r::Vector{T}
            end
        else
            ( r, xmatch, delimmatch ) -> begin
                xmatch !== missing && push!(r,xmatch)
                r::Vector{T}
            end 
        end
    else
        agg
    end
    function tf(v)::Vector{T}
        ## @show v,i
        r = T[]
        if isempty(v[2])
            af(r,v[1],v[3])
        else
            ms = v[2]
            af(r,v[1],ms[1][1])
            for i in 2:lastindex(ms)
                af(r, ms[i-1][2],ms[i][1])
            end
            af(r, ms[end][2],v[3])
        end
        r::Vector{T}
    end

    ## todo: factor out this transform condition!!
    r=Sequence(tf,
             Optional(x; default=missing),
             Repeat(seq(delim, x)),
               Optional(delim;default=missing))
    @show result_type(r)
    r
end


export Repeat_delim
Repeat_delim(x, delim; kw...) =
    Repeat_delim(
        promote_type(result_type(x),result_type(delim)),
        x, delim; kw...)
function Repeat_delim(
    T::Type, x, delim;
    log=false,repf=Repeat,
    transform=(v) -> v,
    transform_each=(v) -> v, kw...)
    x = parser(x)
    delim = parser(delim)
    function t(v)
        L = vcat(v...)
        transform(map(p -> transform_each(p,i),  L  ),i)
    end
    seq(Vector{T},
        Optional(delim; default=T[], log=log),
        repf(Vector{T},
             seq(x, delim; log=log); log=log,
             transform = (v) -> vcat([ [x...] for x in v ]...)),
        Optional(x; default=T[], log=log)
        ; log=log,
        ## todo: factor out this transform condition!!
        transform = (t)
        , kw...)
end


export Repeat_delim_par
function Repeat_delim_par(x, delim; repf=Repeat, transform=(v) -> v, transform_each=v -> v, kw...)
    x = parser(x)
    delim = parser(delim)
    T = result_type(typeof(x))
    D = result_type(typeof(delim))
    seq(Vector{T},
        Optional(v -> D[v], delim),
        repf(Sequence(1, x, delim);
             transform=(v) -> v),
        Optional(v -> T[v], x)
        ; 
        ## todo: factor out this transform condition!!
        transform = (v)  -> transform(
            map(p -> transform_each(p),
                vcat(v[2:end]...)),i)
        , kw...)
end



"""
tokenize(x, str; delta=200, errorfile=nothing)

Tokenize string or iterator `str` with parser `x`.
"""
function tokenize(x, str; partial=:error)
    i=firstindex(str)
    till=lastindex(str)
    r, i_ = tryparsenext(x, str, i, till, TextParse.default_opts)
    if i_<=till
        if partial isa AbstractString ## remove?
            make_org(s) = replace(s, r"^\*"m => " *")
            open(partial, "a") do io
                println(io, "* incomplete parsing stopped at $i_ ")
                println(io, "error at")
                println(io, make_org(str[min(i_,end):min(end, nextind(str,i_,200))]))
                println(io, "** data")
                println(io, make_org(str))
            end
        elseif partial == :warn
            @warn "incomplete parsing stopped at $i_ " str[min(i_,end):min(end, nextind(str,i_,200))]
        elseif partial == :error
            throw(PartialMatchException(i_, str, x))
        elseif partial == :nothing
            return nothing
        end
    end
    if isnull(r)
        if partial == :error
            error("no match")
        elseif partial == :warn
            @warn "no match"
        else
            nothing
        end
    else
        get(r)
    end
end



export splitter
splitter(S, parse; transform_split = v -> tokenize(S, v), kw...) =
    splitter(Regex(regex_string(S)), parse;
             transform_split = transform_split, kw...)

function splitter(## R::Type,
                  split::Transformation{Regex,S},
                  parse::CombinedParsers.ParserTypes;
                  log=false,
                  transform = (v) -> v) where {S}
    @warn "todo: using old regex splitting..."
    transform_split = split.transform ## (v,i) -> v
    T = result_type(parse)
    R = promote_type(S,T)
    function tpn(str, i, n, opts) ## from util.jl:_split
        ## @show str
        ## @show R
        strs = Vector{R}(undef, 0)#[]
        lstr = str[i:min(end,n)]
        r = eachmatch(split.parser, lstr)
        j = 0
        for m in r
            if j <= m.match.offset
                ## m.match.offset  is indexed at 0!!
                ## @show lstr nextind(lstr,j) m.match.offset m.match
                before = SubString(lstr,nextind(lstr,j),prevind(lstr, m.match.offset + (transform_split===nothing ? sizeof(m.match) : 1)))
                log && @info "before" before
                push!(strs, (tokenize(parse, before))) # , i+nextind(lstr,j))) ## todo pass pos!
            end
            if transform_split!==nothing
                log && @info "split" before
                push!(strs, ( transform_split(m, i))) # , i+j) )
            end
            j = m.match.offset + sizeof(m.match) # =.ncodeunits
        end
        ## j = prevind(lstr,j)
        if j <= n-i
            after = SubString(str,i+j,min(lastindex(str),n))
            log && @info "after" after
            push!(strs,
                  (tokenize(parse, after))) ## , i+j)) ## todo pass pos!
        end
        result = transform(strs,i)
        ## error()
        log && @info "split" lstr strs result i j n
        return Nullable(result), nextind(str,n)
    end
    CustomParser(tpn, R)
end

