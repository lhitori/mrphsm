---
title: TIPE - $\boxed{\Phi(p \rightarrow q) \overset{?}= \Phi(p) \Rightarrow \Phi(q)}$
subtitle:  _il va falloir rapidement trouver un titre_
author: Victor R
---

\newpage

# 1. Le $\lambda$-calcul

## 1.1. Cadre et géneralités

**TODO: mettre une phrase d'introduction/de motivation sur le lambda calcul**

::: {.box .def title="Définition 1. L'ensemble $\Lambda$"}

Soit $\mathcal{V}$ un ensemble de variables.
On définit l'ensemble $\Lambda$ par induction:

(i) $\forall v\in\mathcal{V},\ v\in\Lambda$
(ii) $\forall (x, s)\in\mathcal{V}\times\Lambda,\ (\lambda x.\ s)\in\Lambda$ *($\lambda$-abstraction)*
(iii) $\forall (s, t)\in\Lambda^2,\ (s\ t) \in\Lambda$ *(application)*

\
Les élements de $\Lambda$ sont appelés *$\lambda$-termes*.

:::

::: {.box .rem title="Remarques."}

La notation $\lambda x.\ X$ (ii) est l'équivalent en langage mathématiques de $x\mapsto X$ avec $X$ qui dépend de $x$
\

La notation $(s\ t)$ (iii) correspond à la composition de $s$ par $t$

:::

## 1.2. $\alpha$-équivalence

::: {.box .rem title="Rappels. (Relation binaire)"}

Une *relation binaire* sur $\Lambda$ est la donnée d'une partie $\mathcal{R}\subseteq\Lambda\times\Lambda$.

Pour tout $(s, t)\in\mathcal{R}$, on dit que *s est en relation avec t* et on note $s\mathcal{R}t$.

:::

::: {.box .def title="Définition 2."}

On définit une relation $\mathcal{R}_\alpha$ sur l'ensemble $\Lambda$ par:
$$\lambda x.\ u\ \mathcal{R}_\alpha\ \lambda y.\ u[x\leftarrow y] \iff x \neq y \textit{ et  x, y ne sont pas liées à u}$$
:::

::: {.box .def title="Définition 3. $\alpha$-équivalence"}

On définit $\equiv_\alpha$ la plus petite relation qui contient $\mathcal{R_\alpha}$ et telle que:

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

# 3. Le lien entre 1. et 2.

# 3.1.

# 3.2.

# 3.3. Présentation de mon travail

