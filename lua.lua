local null={}
local pairt={}
local vectort={}
local symbolt={}
local atomt={}
local linebuff=""
local function newline()print(linebuff)linebuff=""end
local function putstr(s)linebuff=linebuff..s end
local function is_table(x)return type(x)=="table"end

local function is_null(x)return x==null end
local function is_pair(x)return(is_table(x)and x[1]==pairt)end
local function cons(x,y)return{pairt,x,y}end
local function car(x)assert(is_pair(x))return x[2]end
local function cdr(x)assert(is_pair(x))return x[3]end

local function zerror(...)
	local s="SchemeError:"
	for i,v in ipairs{...} do
		s=s.." "..tostring(v)
	end
	error(s)
end
local errorv=nil
local serr=" SchemeRaise"
local function raise(x)
	errorv=x
	error(tostring(x)..serr)
end
local function withexceptionhandler(f,x)
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

local function is_procedure(x)return type(x)=="function"end
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

local function strappend(x,y)return x..y end
local function is_string(x)return type(x)=="string"end

local function is_symbol(x)return(is_table(x)and x[1]==symbolt)end
local function sym2str(x)assert(is_symbol(x))return x[2]end
local function symbol(x)assert(is_string(x))return{symbolt,x}end

local function is_boolean(x)return type(x)=="boolean"end

local function is_number(x)return type(x)=="number"end
local function num(x)return x+0 end
local function eq(x,y)return x==y or (is_symbol(x) and is_symbol(y) and eq(sym2str(x),sym2str(y)))end
local function add(x,y)return x+y end
local function sub(x,y)return x-y end
local function mul(x,y)return x*y end
local function quo(x,y)return x/y end
local function gt(x,y)return x>y end
local function lt(x,y)return x<y end
local function gteq(x,y)return x>=y end
local function lteq(x,y)return x<=y end

local function isatom(x)return(is_table(x)and x[1]==atomt)end
local function atom(x)return{atomt,x}end
local function atomget(x)assert(isatom(x))return x[2]end
local function atomset(x,v)assert(isatom(x))x[2]=v end
local function atommap(f,x)assert(isatom(x)) local n=f(x[2]) x[2]=n return n end

