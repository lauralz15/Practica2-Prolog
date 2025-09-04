# Practica2-Prolog  
Catálogo de Vehículos en Prolog  
Autores: Laura Sofía Lizarazo y Juan José Sierra  

## Objetivos
- Implementar un catálogo de vehículos en Prolog.
- Consultar por presupuesto, marca, tipo y año.
- Generar reportes con límite de presupuesto total.

## Archivos incluidos
- `Practica22.pl`: código fuente en Prolog.
- Capturas de pantalla de los casos de prueba.
- `README.md`: explicación del proyecto.

## Predicados principales
- `cumple_presupuesto/2` → Filtra vehículos por presupuesto.
- `vehiculos_por_marca/2` → Lista vehículos de una marca.
- `vehiculos_por_tipo_fecha/3` → Agrupa vehículos por tipo y fecha.
- `generar_reporte/5` → Genera reportes con restricción de presupuesto.

## Casos de prueba

Caso 1: Toyota SUV < $30,000
```prolog
?- findall(Ref, (vehiculo(toyota, Ref, suv, Precio, _), Precio < 30000), R).
R = [rav4].
```

Caso 2: Toyota agrupados por tipo y fecha
```prolog
?- bagof((Tipo, Fecha, Ref), vehiculo(toyota, Ref, Tipo, _, Fecha), R).
R = [(sedan, 2023, corolla)] ;
R = [(suv, 2022, rav4)] ;
R = [(pickup, 2021, hilux)].
```

Caso 3: Sedanes sin superar $500,000
```prolog
?- generar_reporte(_, sedan, 500000, R, Valor).
R = [(corolla,24000),(civic,25000),(sentra,23000),
     (jetta,26000),(mazda3,24500),(serie3,40000)],
Valor = 162500.
```




