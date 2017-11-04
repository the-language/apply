local null={}
local pairt={}
local vectort={}
local symbolt={}
local atomt={}
local errorv=nil
local serr=" SchemeRaise"
local function cons(x,y)return{pairt,x,y}end
local function is_null(x)return x==null end
local function is_pair(x)return type(x)=="table"and x[1]==pairt end
local function car(x)assert(is_pair(x))return x[2]end
local function cdr(x)assert(is_pair(x))return x[3]end
local function raise(x)
	errorv=x
	error(tostring(x)..serr)
end
local function weh(f,x)
	local s,r=pcall(x)
	if s then
		return r
	elseif string.sub(r,-#serr)==serr then
		local v=errorv
		errorv=nil
		return f(v)
	else
		return error(r)
	end
end
local function ig(x)end
local function list2lua(xs)
	local t={}
	local xs=xs
	while not is_null(xs) do
		t[#t+1]=car(xs)
		xs=cdr(xs)
	end
	return t
end
local function apply(f,xs)return f(unpack(list2lua(xs)))end
local function lst(xs)
	local r=null
	for i=#xs,1,-1 do
		r=cons(xs[i],r)
	end
	return r
end
local function isatom(x)return type(x)=="table"and x[1]==atomt end
local function atom(x)return{atomt,x}end
local function atomget(x)assert(isatom(x))return x[2]end
local function atomset(x,v)assert(isatom(x))x[2]=v end
local function atommap(f,x)assert(isatom(x)) local n=f(x[2]) x[2]=n return n end
local function vec(...)return{vectort,{...}}end
local function isvec(x)return type(x)=="table"and x[1]==vectort end
local function veclen(x)assert(isvec(x))return #x[2]end
local function vecref(v,k)assert(isvec(v))return v[2][k+1]end
local function lst2vec(l)return{vectort,list2lua(l)}end
local function vec2lst(v)assert(isvec(x))return lst(x[2])end
local function is_symbol(x)return type(x)=="table"and x[1]==symbolt end
local function symbol(x)return{symbolt,x}end
local function sym2str(x)assert(is_symbol(x))return x[2]end
local function eq(x,y)return x==y or (is_symbol(x) and is_symbol(y) and eq(sym2str(x),sym2str(y)))end

