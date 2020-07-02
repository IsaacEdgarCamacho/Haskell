-- ¿Cómo podemos P
-- implementar la función sumatoria : N → N, donde
-- sumatoria(n) = 1+2+3+4.....+n

sumatoria::Integer->Integer
sumatoria 1 = 1
sumatoria n = n+sumatoria(n-1)