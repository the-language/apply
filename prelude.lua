local null={}
local pairt={}
local vectort={}
local function add(x,y)return x+y end
local function sub(x,y)return x-y end
local function mul(x,y)return x*y end
local function quo(x,y)return x/y end
local function or2(x,y)return x or y end
local function and2(x,y)return x and y end
local function notf(x)return not x end
local function is_null(x)return x==null end
local function is_table(x)return(type(x)=="table")end
local function is_pair(x)return(is_table(x)and x[1]==pairt)end
local function cons(x,y)return{pairt,x,y}end
local function car(x)assert(is_pair(x))return x[2]end
local function cdr(x)assert(is_pair(x))return x[3]end
local function vector(...)return{vectort,{...}}end
local function is_vector(x)return(is_table(x)and x[1]==vectort)end
local function vector_ref(x,n)
	assert(is_vector(x))
	local r=x[2][n]
	assert(r~=nil)
	return r
end
