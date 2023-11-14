# Joint Modeling of Longitudinal and Survival Data

Joint modeling of longitudinal and survival data is a statistical approach used to simultaneously analyze two types of data: longitudinal data (repeated measurements over time) and survival data (time-to-event data). This methodology is particularly common in public health research, where you often observe both the progression of a disease over time (longitudinal data) and the occurrence of an event of interest like death or disease relapse (survival data).

#### Key Components:
**Longitudinal Data Component:** This part of the model deals with repeated measurements taken over time on a subject. In the public health context, these measurements like blood pressure, tumor size, or cholesterol levels, but in the context of the pacman data these are `distance_to_ghost`, `points_remaining`, and other reward/threat/conflict values that change over the trial. The focus is on how these measurements change over time for each trial. A common approach to model this is using linear mixed models, which can handle the within-subject correlation inherent in repeated measurements.

**Survival Data Component:** This aspect focuses on the time until an event of interest occurs (e.g., death, or here, time of turnaround). Happily, while these models were developed to handle missingness, we don't have to deal with censorship problems. Survival models, like Cox proportional hazards models, are commonly used to handle this kind of data.


## Current To-Do


