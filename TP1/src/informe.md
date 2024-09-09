# HOLA
### holi

> La vida es muy corta hacete torta. -Tad Marburg

*énfasis* (cursiva)

**énfasis fuerte** (negrita)

## EJERCICIO 1
### Sintaxis abstracta
intexp ::= ...  
$~~~~~~~~~~~~~~$| intexp ++  
$~~~~~~~~~~~~~~$| intexp --

### Sintaxis concreta
intexp ::= ...  
$~~~~~~~~~~~~~~$| intexp '+' '+'  
$~~~~~~~~~~~~~~$| intexp '-' '-'

## EJERCICIO 4

$${x \in dom~\sigma \over \langle x++, \sigma \rangle \Downarrow_{exp} \langle ~\sigma~x + 1, [\sigma~|~x : ~\sigma~x + 1] \rangle}~VarInc$$

$${x \in dom~\sigma \over \langle x--, \sigma \rangle \Downarrow_{exp} \langle ~\sigma~x - 1, [\sigma~|~x : ~\sigma~x - 1] \rangle}~VarDec$$