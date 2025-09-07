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

% Vehículos por tipo y fecha (agrupados)
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

% Sumar precios de una lista de vehículos
sumar_precios([], 0).
sumar_precios([(_, Precio)|T], Total) :-
    sumar_precios(T, Resto),
    Total is Precio + Resto.

% Ordenar vehículos por precio (ascendente)
ordenar_por_precio(Vehiculos, Ordenados) :-
    predsort(comparar_precio, Vehiculos, Ordenados).

comparar_precio(Orden, (_, P1), (_, P2)) :-
    compare(Orden, P1, P2).

% Ajustar inventario sin descartar los demás vehículos (MEJORADO)
ajustar_inventario([], _, []).
ajustar_inventario([(Ref, Precio)|T], Limite, [(Ref, Precio)|Resultado]) :-
    Limite >= Precio,
    NuevoLimite is Limite - Precio,
    ajustar_inventario(T, NuevoLimite, Resultado).
ajustar_inventario([(_, Precio)|T], Limite, Resultado) :-
    Precio > Limite,
    ajustar_inventario(T, Limite, Resultado).  % Omitir vehículo caro pero continuar

