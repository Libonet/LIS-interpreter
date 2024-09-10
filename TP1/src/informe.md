# HOLA
### holi

> La vida es. -Santiago Libonati

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

$${x \in dom~\sigma \over \langle x++, \sigma \rangle \Downarrow_{exp} \langle ~\sigma~x + 1, [\sigma~|~x : ~\sigma~x + 1] \rangle}~VARINC$$

$${x \in dom~\sigma \over \langle x--, \sigma \rangle \Downarrow_{exp} \langle ~\sigma~x - 1, [\sigma~|~x : ~\sigma~x - 1] \rangle}~VARDEC$$

## EJERCICIO 5
Queremos ver que si $t \rightsquigarrow t'$ y $t \rightsquigarrow t''$, entonces $t' = t''$.  
Para ello haremos inducción sobre la derivación $t \rightsquigarrow t'$.

HI) para toda subderivación de $t \rightsquigarrow t'$ se verifica la regla.

Si la última derivación de $t \rightsquigarrow t'$ usa la regla:

* $ASS$: Tenemos que t tiene la forma $\langle v=e, \sigma \rangle$ y t' tiene la forma $\langle skip, [\sigma'~|~v:n] \rangle$. Por la forma de t, en la derivación $t \rightsquigarrow t''$ la última regla aplicada solo puede haber sido $ASS$, ya que no hay otra regla donde t pueda ser una asignación. Luego, como $\Downarrow_{exp}$ es determinista, resulta $t'=t''$.

* $SEQ_1$: Tenemos que t tiene la forma $\langle skip; c_1, \sigma \rangle$ y t' tiene la forma $\langle c_1, \sigma \rangle$. Por la forma de t, en la derivación $t \rightsquigarrow t''$ la última regla aplicada solo puede haber sido $SEQ_1$, ya que no hay otra regla donde t pueda ser una secuenciación con un skip. Luego, $t'=t''$.

* $SEQ_2$: Tenemos que t tiene la forma $\langle c_0; c_1, \sigma \rangle$ y t' tiene la forma $\langle c_0'; c_1, \sigma' \rangle$. Por la forma de t, la última derivación en $t \rightsquigarrow t''$ debe ser aplicando la regla $SEQ_2$, donde la forma de t'' es $\langle c_0''; c_1, \sigma'' \rangle$. Nuestra HI es que si $\langle c_0, \sigma \rangle \rightsquigarrow \langle c_0', \sigma' \rangle$ y $\langle c_0, \sigma \rangle \rightsquigarrow \langle c_0'',\sigma'' \rangle$, entonces $\langle c_0', \sigma' \rangle = \langle c_0'', \sigma'' \rangle$. Luego por HI, $t' = \langle c_0'; c_1, \sigma' \rangle = \langle c_0''; c_1, \sigma'' \rangle = t''$

* $IF_1$: Tenemos que t tiene la forma $\langle \bold{if}~b~\bold{then}~c_0~\bold{else}~c_1, \sigma \rangle$ y t' tiene la forma $\langle c_0, \sigma' \rangle$. Por la forma de t, la última regla aplicada en la derivación $t \rightsquigarrow t''$ debe ser $IF_1$, ya que, como $\Downarrow_{exp}$ es determinista, $\langle b, \sigma' \rangle$ $\Downarrow_{exp}$ $\langle true, \sigma' \rangle$. Por lo tanto, debe ser $t'=t''$.

* $IF_2$: Tenemos que t tiene la forma $\langle \bold{if}~b~\bold{then}~c_0~\bold{else}~c_1, \sigma \rangle$ y t' tiene la forma $\langle c_0, \sigma' \rangle$. Por la forma de t, la última regla aplicada en la derivación $t \rightsquigarrow t''$ debe ser $IF_2$, ya que, como $\Downarrow_{exp}$ es determinista, $\langle b, \sigma' \rangle$ $\Downarrow_{exp}$ $\langle false, \sigma' \rangle$. Por lo tanto, debe ser $t'=t''$.

* $REPEAT$: Tenemos que t tiene la forma $\langle \bold{repeat}~c~\bold{until}~b~, \sigma \rangle$ y t' tiene la forma $\langle c; \bold{if}~b~\bold{then}~skip~\bold{else}~\bold{repeat}~c~\bold{until}~b~, \sigma \rangle$. Por la forma de t, la última regla aplicada en la derivación $t \rightsquigarrow t''$ debe ser $REPEAT$, y por ende $t'=t''$.

Hemos probado que para cada posible regla aplicada en la derivación $t \rightsquigarrow t'$ se verifica lo planteado, con lo cual queda probado que si $t \rightsquigarrow t'$ y $t \rightsquigarrow t''$, entonces $t' = t''$, esto es, la relación de evaluación en un paso $\rightsquigarrow$ es determinista.

## EJERCICIO 6

<x=x+1; y=x, \sigma>

Sea $x \in dom~\sigma:$


$${{x \in dom~\sigma 
\over 
\langle x++, \sigma \rangle \Downarrow_{exp} \langle ~\sigma~x + 1, [\sigma~|~x : ~\sigma~x + 1] \rangle}~VARINC
\over

\langle y=x++, \sigma \rangle \rightsquigarrow \langle ~skip, [\sigma~|~y : ~\sigma~x + 1] \rangle
}~ASS$$


---


$${{x \in dom~\sigma 
\over 
\langle x, \sigma \rangle \Downarrow_{exp} \langle ~\sigma~x, \sigma \rangle}~VAR
\over

\langle y=x, \sigma \rangle \rightsquigarrow \langle ~skip, [\sigma~|~y : ~\sigma~x] \rangle
}~ASS$$