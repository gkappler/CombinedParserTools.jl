
struct Greedy{Ps,A,F<:Function} <: CombinedParser{Any,Any}
    pairs::Ps
    alt::A
    transform::F
end
    
export greedy
function greedy(tokens...;
                alt = [],
                transform=(v,i) -> v)
    Greedy([tokens...], alt, transform)
end

function CombinedParsers.tryparsenext(tokf::Greedy, str, i, till, opts=TextParse.default_opts)
    T = result_type(tokf)
    sections=tokf.pairs
    RT(key, value) = if value[2] isa ParserTypes
        if Missing <: result_type(typeof(key))
            result_type(typeof(value[2]))
        else
            promote_type(result_type(typeof(key)), result_type(typeof(value[2])))
        end
    else
        result_type(typeof(key))
    end
    R = Dict([ value[1] => Vector{RT(key,value)}() for (key,value) in sections]...,
             [ key => Vector{result_type(typeof(value))}() for (key,value) in tokf.alt]...
             )
    hist = Any[]
    last_section = nothing
    last_content = nothing
    aggregator = :head
    function first_match(j)
        local repval, i__
        for (key, content) in sections
            repval, i__ = tryparsenext(key, str, j, till)
            !isnull(repval) && return key, content, repval, i__
        end
        return (first(sections)..., repval, i__)
    end
    head = nothing
    i_ = i ##isnull(1)
    while true
        key, content, r, i__ = first_match(i_)
        save = if isnull(r)
            cr, ci = if last_content === nothing || last_content === missing
                Nullable{T}(), i
            else
                tryparsenext(last_content, str, i_, till)
            end
            ai = 0
            while ai < lastindex(tokf.alt) && isnull(cr)
                ai = ai+1
                cr, ci = tryparsenext(tokf.alt[ai].second, str, i_, till)
            end
            if isnull(cr)
                return Nullable{T}(convert(T, tokf.transform(R,i))), i_
            elseif ai == 0
                push!(hist, get(cr))
                i__ = ci
                false
            else
                aggregator != :head && append!(R[aggregator],hist)
                hist = [get(cr)]
                (aggregator, last_content) = tokf.alt[ai]
                last_section = ai
            end
        else
            if last_section !== nothing
                append!(R[aggregator],hist)
            end
            hist = get(r) !== missing ? [get(r)] : Vector{RT(key,content)}()
            aggregator, last_content = content
            last_section = key
        end
        i_ = i__
    end
    error("unreachable")
end
