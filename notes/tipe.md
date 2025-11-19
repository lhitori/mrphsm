---
title: TIPE
subtitle: \textit{Il va falloir trouver un titre}
author: Victor R
---

# 1. Le $\lambda$-calcul

## 1.1. Géneralités

::: {.box .def title="Définition 1. L'ensemble $\Lambda$"}

Soit $\mathcal{V}$ un ensemble de variables.
On définit l'ensemble $\Lambda$ par induction:

(i) $\forall v\in\mathcal{V},\ v\in\Lambda$
(ii) $\forall (x, s)\in\mathcal{V}\times\Lambda,\ (\lambda x\ .\ s)\in\Lambda$ *($\lambda$-abstraction)*
(iii) $\forall (s, t)\in\Lambda^2,\ (s\ t) \in\Lambda$ *(application)*

\
Les élements de $\Lambda$ sont appelés *$\lambda$-termes*.

:::

::: {.box title="Remarque."}

La notation $\lambda x\ .\ X$ est l'équivalent de $x\mapsto X$ avec $X$ qui dépend de $x$.

:::

::: {.box title="Remarque."}

La notation $(s\ t)$ correspond à la composition de $s$ par $t$

:::

## 1.2. $\alpha$-équivalence

::: {.box .def title="Définition 2."}

On définit une relation $\mathcal{R}_\alpha$ sur l'ensemble $\Lambda$ par:
$$\lambda x.\ u\ \mathcal{R}_\alpha\ \lambda y.\ u[x:=y] \iff x \neq y \textit{ et  x, y ne sont pas liées à u}$$


i.e. changer le nom d’une variable liée sans capturer de variable libre ne modifie pas le sens de l’expression.
:::

::: {.box .def title="Définition 3. $\alpha$-équivalence"}

On définit $\equiv_\alpha$ par

(i) $\equiv_\alpha$ est conservée par multiplication à gauche/droite par un élement de $\Lambda$
(ii) $\equiv_\alpha$ est conservée par passage à la $\lambda$-abstraction, i.e.

$$\forall (s, t)\in\Lambda^2,\ s\equiv_\alpha t \implies \lambda x.\ u\ \equiv_\alpha\ \lambda x.\ v$$


:::

## 1.3. $\beta$-réduction

::: {.box .def title="Définition k. $\beta$-réduction"}

TODO

:::

# 2. Les assistants de preuves

# 2.1. Contexte et cadre

# 2.2. Exemples

# 3. $\mathcal{P} \leftrightarrow \Lambda$


