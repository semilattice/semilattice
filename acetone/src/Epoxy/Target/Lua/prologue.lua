-- The contents of this source file are to be included before the generated Lua
-- source code.

--------------------------------------------------------------------------------

-- Arbitrary
local Rarbitrary

-- Abend
local RAbend
local Rabend

-- Lazy
local Rlazy

-- Ambient functor
local RmapAmbient
local RpureAmbient
local RjoinAmbient

-- Ambient environment
local RaskAmbient
local RlocalAmbient

-- Ambient exception
local RAmbientException
local RthrowAmbient
local RcatchAmbient

--------------------------------------------------------------------------------
-- Arbitrary

-- An arbitrary value, used when the value being used does not matter.
Rarbitrary = {}

--------------------------------------------------------------------------------
-- Abend

-- Metatable used to disambiguate abends from other errors.
RAbend = {}

function Rabend(m)
    local err = {message = m}
    setmetatable(err, RAbend)
    error(err, 0)
end

--------------------------------------------------------------------------------
-- Lazy

function Rlazy(t)
    return function(u)
        local r = t(u)
        t = function(u) return r end
        return r
    end
end

--------------------------------------------------------------------------------
-- Ambient functor

function RmapAmbient(f)
    return function(a)
        return function(g)
            return f(a(g))
        end
    end
end

function RpureAmbient(x)
    return function(g)
        return x
    end
end

function RjoinAmbient(a)
    return function(g)
        return a(g)(g)
    end
end

--------------------------------------------------------------------------------
-- Ambient environment

function RaskAmbient(g)
    return g
end

function RlocalAmbient(f)
    return function(a)
        return function(g)
            return a(f(g))
        end
    end
end

--------------------------------------------------------------------------------
-- Ambient exception

-- Metatable used to disambiguate exceptions from other errors. RcatchAmbient
-- should only catch errors thrown by RthrowAmbient, not abends or any other
-- errors.
RAmbientException = {}

function RthrowAmbient(e)
    return function(g)
        local err = {exception = e}
        setmetatable(err, RAmbientException)
        error(err, 0)
    end
end

function RcatchAmbient(a)
    return function(h)
        return function(g)
            local ok, r = pcall(a, g)
            if ok then
                return r
            elseif getmetatable(r) == RAmbientException then
                return h(r.exception)(g)
            else
                error(r, 0)
            end
        end
    end
end
