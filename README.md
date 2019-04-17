# simple-units
A Haskell library for simple arithmetic with SI units using type-checked dimensional analysis.

```haskell
>>> let newton = kilogram .* meter ./ (second .* second)
>>> 23*newton
23.0 kg*m/s^2
>>> let g = 6.67408e-11 * newton .* (meter .* meter) ./ (kilogram .* kilogram)
>>> g -- gravitational constant
6.67408e-11 m^3/kg*s^2
>>> let gravity m1 m2 r = g .* (m1 * kilogram) .* (m2 * kilogram) ./ (r*meter .* r*meter)
>>> let earth_mass = 5.972e24 * kilogram
>>> let mars_mass = 6.417e23 * kilogram
>>> let earth_radius = 6371e3 * meter
>>> let mars_radius = 3389.5e3 * meter
>>> let weight_on_earth mass = gravity mass earth_mass earth_radius
>>> let weight_on_mars mass = gravity mass mars_mass mars_radius
>>> weight_on_earth (80 * kilogram)
785.5719790179963 kg*m/s^2
>>> weight_on_mars (80 * kilogram)
298.22370259533704 kg*m/s^2
>>> weight_on_mars 1 ./ weight_on_earth 1
0.3796261966575378 <adimensional>
```
