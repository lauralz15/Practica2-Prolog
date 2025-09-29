% Catálogo de vehículos

vehiculo(toyota, corolla, sedan, 24000, 2023).
vehiculo(toyota, rav4, suv, 28000, 2022).
vehiculo(toyota, hilux, pickup, 35000, 2021).
vehiculo(honda, civic, sedan, 25000, 2023).
vehiculo(honda, crv, suv, 30000, 2022).
vehiculo(nissan, sentra, sedan, 23000, 2023).
vehiculo(nissan, frontier, pickup, 33000, 2022).
vehiculo(nissan, '370z', deportivo, 45000, 2021).
vehiculo(volkswagen, jetta, sedan, 26000, 2023).
vehiculo(volkswagen, tiguan, suv, 34000, 2022).
vehiculo(mazda, cx5, suv, 29000, 2023).
vehiculo(mazda, mazda3, sedan, 24500, 2022).
vehiculo(chevrolet, camaro, deportivo, 42000, 2023).
vehiculo(chevrolet, silverado, pickup, 36000, 2022).
vehiculo(bmw, serie3, sedan, 40000, 2023).

% Filtrar por tipo y presupuesto
cumple_presupuesto(Referencia, PresupuestoMax) :-
    vehiculo(_, Referencia, _, Precio, _),
    Precio =< PresupuestoMax.

% Listar vehículos por marca
vehiculos_por_marca(Marca, Referencias) :-
    bagof(Ref, vehiculo(Marca, Ref, _, _, _), Referencias).

% Vehículos por tipo y fecha
vehiculos_por_tipo_fecha(Tipo, Fecha, Referencias) :-
    bagof(Ref, vehiculo(_, Ref, Tipo, _, Fecha), Referencias).

% Generar reporte con restricción de presupuesto 
generar_reporte(Marca, Tipo, Presupuesto, Resultado, ValorFinal) :-
    % Encontrar todos los vehículos que cumplen
    findall((Ref, Precio),
            (vehiculo(Marca, Ref, Tipo, Precio, _),
             Precio =< Presupuesto),
            Vehiculos),
    % Calcular el valor total
    sumar_precios(Vehiculos, ValorTotal),
    % Ajustar si supera el límite de $1,000,000
    (   ValorTotal > 1000000
    ->  ordenar_por_precio(Vehiculos, VehiculosOrdenados),
        ajustar_inventario(VehiculosOrdenados, 1000000, VehiculosAjustados),
        sumar_precios(VehiculosAjustados, ValorAjustado),  % Recalcular con la lista ajustada
        Resultado = VehiculosAjustados,
        ValorFinal = ValorAjustado
    ;   Resultado = Vehiculos,
        ValorFinal = ValorTotal
    ).

% Sumar precios de todos los carros de una marca
sumar_precios_marca(Marca, Suma) :-
    findall(Precio, vehiculo(Marca, _, _, Precio, _), ListaPrecios),
    sum_list(ListaPrecios, Suma).

% Sumar precios de una lista de referencias de carros
sumar_precios_referencias(Referencias, Suma) :-
    findall(Precio, (member(Ref, Referencias), vehiculo(_, Ref, _, Precio, _)), ListaPrecios),
    sum_list(ListaPrecios, Suma).
  
% Ordenar una lista dada de referencias de vehículos por su precio ascendente
ordenar_referencias_por_precio(Referencias, ReferenciasOrdenadas) :-
    findall((Ref, Precio),
            (member(Ref, Referencias), vehiculo(_, Ref, _, Precio, _)),
            Lista),
    predsort(comparar_precio, Lista, Ordenados),
    findall(Ref, member((Ref, _), Ordenados), ReferenciasOrdenadas).

% Comparador para predsort
comparar_precio(Orden, (_, P1), (_, P2)) :-
    compare(Orden, P1, P2).

% Dada una lista de referencias, devuelve la lista (Referencia, Precio) y la suma total
reporte_referencias(Referencias, ListaRefPrecio, Suma) :-
    findall((Ref, Precio),
            (member(Ref, Referencias), vehiculo(_, Ref, _, Precio, _)),
            ListaRefPrecio),
    findall(Precio,
            (member(Ref, Referencias), vehiculo(_, Ref, _, Precio, _)),
            ListaPrecios),
    sum_list(ListaPrecios, Suma).

% Dada una lista de referencias y un presupuesto, devuelve solo los carros que caben en el presupuesto
ajustar_inventario_referencias(Referencias, Presupuesto, ReferenciasAjustadas) :-
    findall((Ref, Precio),
            (member(Ref, Referencias), vehiculo(_, Ref, _, Precio, _)),
            Lista),
    predsort(comparar_precio, Lista, Ordenados),
    ajustar_inventario(Ordenados, Presupuesto, Ajustados),
    findall(Ref, member((Ref, _), Ajustados), ReferenciasAjustadas).

ajustar_inventario([], _, []).
ajustar_inventario([(Ref, Precio)|T], Presupuesto, [(Ref, Precio)|Resto]) :-
    Precio =< Presupuesto,
    NuevoPresupuesto is Presupuesto - Precio,
    ajustar_inventario(T, NuevoPresupuesto, Resto).
ajustar_inventario([(_, Precio)|_], Presupuesto, []) :-
    Precio > Presupuesto.

% Auxiliares generar_reporte

% Sumar precios de una lista de pares (Referencia, Precio)
sumar_precios([], 0).
sumar_precios([(_, Precio)|T], Total) :-
    sumar_precios(T, Resto),
    Total is Precio + Resto.

% Ordenar vehículos por precio (lista de pares Ref-Precio)
ordenar_por_precio(Vehiculos, Ordenados) :-
    predsort(comparar_precio, Vehiculos, Ordenados).
