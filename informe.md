---
title: "TP1: Lenguaje Imperativo Simple"
author: Grillo (G-5811/4), Libonati (L-3256/5), Maiza (M-7116/1)
date: 17/09/24
geometry: margin=2cm
output: pdf_document
header-includes:
  - \usepackage{graphicx}
  - \usepackage{ebproof}
  - \usepackage{amsmath}
  - \usepackage{amssymb}
---

## EJERCICIO 1
### Sintaxis abstracta

$$\begin{aligned}
\text{intexp} &::= ... \\ 
              &\text{| intexp ++} \\
              &\text{| intexp --} 
\end{aligned}
$$

### Sintaxis concreta

$$\begin{aligned}
\text{intexp} &::= ... \\ 
              &\text{| intexp '+' '+'} \\
              &\text{| intexp '-' '-'} 
\end{aligned}
$$

## EJERCICIO 4

$${x \in dom~\sigma \over \langle x++, \sigma \rangle \Downarrow_{exp} \langle ~\sigma~x + 1, [\sigma \mid x : ~\sigma~x + 1] \rangle}~VARINC$$

$${x \in dom~\sigma \over \langle x--, \sigma \rangle \Downarrow_{exp} \langle ~\sigma~x - 1, [\sigma \mid x : ~\sigma~x - 1] \rangle}~VARDEC$$

## EJERCICIO 5
Queremos ver que si $t \rightsquigarrow t'$ y $t \rightsquigarrow t''$, entonces $t' = t''$.  
Para ello haremos inducción sobre la derivación $t \rightsquigarrow t'$.

HI) para toda subderivación de $t \rightsquigarrow t'$ se verifica dicha propiedad.

Si la última derivación de $t \rightsquigarrow t'$ usa la regla:

* $ASS$: Tenemos que t tiene la forma $\langle v=e, \sigma \rangle$ y t' tiene la forma $\langle skip, [\sigma' \mid v:n] \rangle$. Por la forma de t, en la derivación $t \rightsquigarrow t''$ la última regla aplicada solo puede haber sido $ASS$, ya que no hay otra regla donde t pueda ser una asignación. Luego, como $\Downarrow_{exp}$ es determinista, resulta $t'=t''$.

* $SEQ_1$: Tenemos que t tiene la forma $\langle skip; c_1, \sigma \rangle$ y t' tiene la forma $\langle c_1, \sigma \rangle$. Por la forma de t, en la derivación $t \rightsquigarrow t''$ la última regla aplicada solo puede haber sido $SEQ_1$, ya que no hay otra regla donde t pueda ser una secuenciación con un skip. Luego, $t'=t''$.

* $SEQ_2$: Tenemos que t tiene la forma $\langle c_0; c_1, \sigma \rangle$ y t' tiene la forma $\langle c_0'; c_1, \sigma' \rangle$. Por la forma de t, la última derivación en $t \rightsquigarrow t''$ debe ser aplicando la regla $SEQ_2$, donde la forma de t'' es $\langle c_0''; c_1, \sigma'' \rangle$. Nuestra HI es que si $\langle c_0, \sigma \rangle \rightsquigarrow \langle c_0', \sigma' \rangle$ y $\langle c_0, \sigma \rangle \rightsquigarrow \langle c_0'',\sigma'' \rangle$, entonces $\langle c_0', \sigma' \rangle = \langle c_0'', \sigma'' \rangle$. Luego por HI, $t' = \langle c_0'; c_1, \sigma' \rangle = \langle c_0''; c_1, \sigma'' \rangle = t''$

* $IF_1$: Tenemos que t tiene la forma $\langle \bold{if}~b~\bold{then}~c_0~\bold{else}~c_1, \sigma \rangle$ y t' tiene la forma $\langle c_0, \sigma' \rangle$. Por la forma de t, la última regla aplicada en la derivación $t \rightsquigarrow t''$ debe ser $IF_1$, ya que, como $\Downarrow_{exp}$ es determinista, $\langle b, \sigma' \rangle$ $\Downarrow_{exp}$ $\langle true, \sigma' \rangle$. Por lo tanto, debe ser $t'=t''$.

* $IF_2$: Tenemos que t tiene la forma $\langle \bold{if}~b~\bold{then}~c_0~\bold{else}~c_1, \sigma \rangle$ y t' tiene la forma $\langle c_1, \sigma' \rangle$. Por la forma de t, la última regla aplicada en la derivación $t \rightsquigarrow t''$ debe ser $IF_2$, ya que, como $\Downarrow_{exp}$ es determinista, $\langle b, \sigma' \rangle$ $\Downarrow_{exp}$ $\langle false, \sigma' \rangle$. Por lo tanto, debe ser $t'=t''$.

* $REPEAT$: Tenemos que t tiene la forma $\langle \bold{repeat}~c~\bold{until}~b~, \sigma \rangle$ y t' tiene la forma $\langle c; \bold{if}~b~\bold{then}~skip~\bold{else}~\bold{repeat}~c~\bold{until}~b~, \sigma \rangle$. Por la forma de t, la última regla aplicada en la derivación $t \rightsquigarrow t''$ debe ser $REPEAT$, y por ende $t'=t''$.

Hemos probado que para cada posible regla aplicada en la derivación $t \rightsquigarrow t'$ se verifica lo planteado, con lo cual queda probado que si $t \rightsquigarrow t'$ y $t \rightsquigarrow t''$, entonces $t' = t''$, esto es, la relación de evaluación en un paso $\rightsquigarrow$ es determinista.

## EJERCICIO 6
Construiremos los árboles de derivación de cada programa utilizando las siguientes reglas de inferencia para la relación $\rightsquigarrow^*$:


```{=latex}
\begin{center}
\scalebox{1.5}{
    \begin{prooftree}
        \hypo{t \rightsquigarrow t'}
        \infer1[$RT_1~~~~~$]{t \rightsquigarrow^* t'}
    \end{prooftree}

    \begin{prooftree}
        \hypo{}
        \infer1[$RT_2~~~~~$]{t \rightsquigarrow^* t}
    \end{prooftree}

    \begin{prooftree}
        \hypo{t \rightsquigarrow^* t'}
        \hypo{t' \rightsquigarrow^* t''}
        \infer2[$RT_3$]{t \rightsquigarrow^* t''}
    \end{prooftree}
}
\end{center}
```


Para el programa **a)** tenemos el siguiente árbol:

```{=latex}
\begin{center}
\scalebox{.6}{
    \begin{prooftree}
        \hypo{x \in dom~\sigma}
        \infer1[\text{VAR}]{\langle x, \sigma \rangle \Downarrow_{exp} \langle \sigma x, \sigma \rangle}
        \hypo{}
        \infer1[\text{NVAL}]{\langle 1, \sigma \rangle \Downarrow_{exp} \langle 1, \sigma \rangle}
        \infer2[\text{PLUS}]{\langle x+1, \sigma \rangle \Downarrow_{exp} \langle \sigma x+1, \sigma \rangle}
        \infer1[\text{ASS}]{~~~~~~~~\langle x=x+1, \sigma \rangle \rightsquigarrow ~\langle skip, [\sigma \mid x : \sigma x+1] \rangle~~~~~~~~}
        \infer1[\text{$SEQ_2$}]{\langle x=x+1; y=x, \sigma \rangle \rightsquigarrow ~\langle skip; y=x, [\sigma \mid x : \sigma x+1] \rangle}
        \infer1[\text{$RT_1$}]{\langle x=x+1; y=x, \sigma \rangle \rightsquigarrow^* \langle skip; y=x, [\sigma \mid x : \sigma x+1] \rangle}
        \hypo{}
        \infer1[\text{$SEQ_1$}]{\langle skip; y=x, [\sigma \mid x : \sigma x+1] \rangle \rightsquigarrow ~\langle y=x, [\sigma \mid x : \sigma x+1] \rangle}
        \infer1[\text{$RT_1$}]{\langle skip; y=x, [\sigma \mid x : \sigma x+1] \rangle \rightsquigarrow^* \langle y=x, [\sigma \mid x : \sigma x+1] \rangle}
        \infer2[\text{$RT_3$}]{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\langle x=x+1; y=x, \sigma \rangle \rightsquigarrow^* \langle y=x, [\sigma \mid x : \sigma x+1] \rangle~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
        
        \hypo{x \in dom~[\sigma \mid x : \sigma x+1]}
        \infer1[\text{VAR}]{\langle x, [\sigma \mid x : \sigma x+1] \rangle \Downarrow_{exp} \langle \sigma x, [\sigma \mid x : \sigma x+1] \rangle}
        \infer1[\text{ASS}]{\langle y=x, [\sigma \mid x : \sigma x+1] \rangle \rightsquigarrow \langle skip, [\sigma \mid x : \sigma x+1, y : \sigma x+1] \rangle}
        \infer1[\text{$RT_1$}]{\langle y=x, [\sigma \mid x : \sigma x+1] \rangle \rightsquigarrow^* \langle skip, [\sigma \mid x : \sigma x+1, y : \sigma x+1] \rangle}
        \infer2[\text{$RT_3$}]{\langle x=x+1; y=x, \sigma \rangle \rightsquigarrow^* \langle skip, [\sigma \mid x : \sigma x+1, y : \sigma x+1] \rangle}
    \end{prooftree}
}
\end{center}
```

Y para el programa **b)** tenemos:

```{=latex}
\begin{center}
\scalebox{1.2}{
    \begin{prooftree}
        \hypo{x \in dom~\sigma}
        \infer1[\text{VARINC}]{\langle x++, \sigma \rangle \Downarrow_{exp} \langle \sigma x+1, [\sigma \mid x : \sigma x+1] \rangle}
        \infer1[\text{ASS}]{\langle y=x++, \sigma \rangle \rightsquigarrow \langle skip, [\sigma \mid x : \sigma x+1, y : \sigma x+1] \rangle}
        \infer1[\text{$RT_1$}]{\langle y=x++, \sigma \rangle \rightsquigarrow^* \langle skip, [\sigma \mid x : \sigma x+1, y : \sigma x+1] \rangle}
    \end{prooftree}
}
\end{center}
```

Luego, tenemos que $\forall \sigma \in \Sigma,~\langle x=x+1; y=x, \sigma \rangle \rightsquigarrow^* \langle skip, \sigma' \rangle$ sii $\langle y=x++, \sigma \rangle \rightsquigarrow^* \langle skip, \sigma' \rangle$

Por lo tanto, los programas con semánticamente equivalentes.
