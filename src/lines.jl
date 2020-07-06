
export print_org_tree
print_org_tree(x) = print_org_tree(stdout,x)

print_org_tree(io::IO, x) =
    print(io,x)
print_org_tree(io::IO, x::AbstractToken) =
    x != Token(:whitespace, "\n") && print(io,x)
function print_org_tree(io::IO, tree::Vector)
    for b in tree
        print_org_tree(io, b)
    end
end
function print_org_tree(io::IO, b::Pair)
    l, inner = b
    print_org_tree(io::IO, NamedString(l), inner)
end
function print_org_tree(io::IO, b::NamedString{:name}, inner)
    print(io, "#+name: ", value(l), "\n")
    print_org_tree(io, inner)
end
function print_org_tree(io::IO, b::NamedString{:orgblock}, inner)
    print(io, "#+begin_", value(l), "\n")
    print_org_tree(io, inner)
    print(io,  "#+end_", value(l), "\n")
end
function print_org_tree(io::IO, b::NamedString{:orgdrawer}, inner)
    print(io, ":", value(l), ":\n")
    print_org_tree(io, inner)
    print(io, ":END:\n" )
end
# function print_org_tree(io::IO, b::NamedString{:inline}, inner)
#     print(io, ":", value(l), ":\n")
#     print_org_tree(io, inner)
# end
# function print_org_tree(io::IO, b::NamedString{:inline}, inner)
#     print_org_tree(io, inner)
# end



