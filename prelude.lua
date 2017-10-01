local null={}
local pairt={}
local vectort={}
local symbolt={}
local void={}
local atomt={}
local promiset={}
local function add(x,y)return x+y end
local function sub(x,y)return x-y end
local function mul(x,y)return x*y end
local function quo(x,y)return x/y end
local function or2(x,y)return x or y end
local function and2(x,y)return x and y end
local function notf(x)return not x end
local function is_null(x)return x==null end
local function is_table(x)return(type(x)=="table")end
local function is_string(x)return(type(x)=="string")end
local function is_pair(x)return(is_table(x)and x[1]==pairt)end
local function cons(x,y)return{pairt,x,y}end
local function car(x)assert(is_pair(x))return x[2]end
local function cdr(x)assert(is_pair(x))return x[3]end
local function vector(...)return{vectort,{...}}end
local function is_vector(x)return(is_table(x)and x[1]==vectort)end
local function vector_ref(x,n)
	assert(is_vector(x))
	local r=x[2][n+1]
	assert(r~=nil)
	return r
end
local function symbol(x)assert(is_string(x))return{symbolt,x}end
local function is_symbol(x)return(is_table(x)and x[1]==symbolt)end
local function sym2str(x)assert(is_symbol(x))return x[2]end
local function voidf()return void end
local function is_void(x)return x==void end
local function ig(x)end
local function veceq(x,y)
	if #x ~= #y then return false end
	for i=1,#x do
		if not eq(x[i],y[i]) then return false end
	end
	return true
end
local function eq(x,y)
	if x==y then return true end
	if not (is_table(x)and is_table(y))then return false end
	local t=x[1]
	if t~=y[1] then return false end
	if t==pairt then
		return eq(x[2],y[2]) and eq(x[3],y[3])
	elseif t==symbolt then
		return x[2]==y[2]
	elseif t==vectort then
		return veceq(x[2],y[2])
	end
	return false
end
local function atom(x)return{atomt,x}end
local function is_atom(x)return(is_table(x)and x[1]==atomt)end
local function atom_set(a,v)assert(is_atom(a))a[2]=v return void end
local function atom_map(a,f)assert(is_atom(a))a[2]=f(a[2])return void end
local function atom_get(a)assert(is_atom(a))return a[2]end
local function eq(x,y)return x==y end
local function gt(x,y)return x>y end
local function lt(x,y)return x<y end
local function gteq(x,y)return x>=y end
local function lteq(x,y)return x<=y end
local function is_promise(x)return(is_table(x)and x[1]==promiset)end
local function force(x)
	if x[3]==nil then
		x[3]=x[2]()
		x[2]=nil
	end
	return x[3]
end

