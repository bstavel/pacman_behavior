# Joint Modeling of Longitudinal and Survival Data

Joint modeling of longitudinal and survival data is a statistical approach used to simultaneously analyze two types of data: longitudinal data (repeated measurements over time) and survival data (time-to-event data). This methodology is particularly common in public health research, where you often observe both the progression of a disease over time (longitudinal data) and the occurrence of an event of interest like death or disease relapse (survival data).

#### Key Components:
**Longitudinal Data Component:** This part of the model deals with repeated measurements taken over time on a subject. In the public health context, these measurements like blood pressure, tumor size, or cholesterol levels, but in the context of the pacman data these are `distance_to_ghost`, `points_remaining`, and other reward/threat/conflict values that change over the trial. The focus is on how these measurements change over time for each trial. A common approach to model this is using linear mixed models, which can handle the within-subject correlation inherent in repeated measurements.

**Survival Data Component:** This aspect focuses on the time until an event of interest occurs (e.g., death, or here, time of turnaround). Happily, while these models were developed to handle missingness, we don't have to deal with censorship problems. Survival models, like Cox proportional hazards models, are commonly used to handle this kind of data.


## Current To-Do

1. **Test models' predictive validity:** Right now these models are looking promising, but I need to move from being excited about them to really trying to break them. It is still possible that too much depends on the general time information that the model receives rather than the context of the predicting variables themselves. 
    * *How much does the accuracy of the prediction depend on the time window for prediction?*
        - create train/test sets with a wide range of turn times
        - visualization with a data point with every error
    * *Formalize comparison with null/permuted models*
        - While I am already using some permuted models, it is difficult to create a good shuffle because the role of time. Need to think about this more.
2. **Make functions for each step, including likely saving out the model after it is fit to save time**
    * These functions should be built with the idea of parallelizing across subjects eventually.
4. **Consider alternate forms of the longitudinal data component**
    * It is weird that the models work so well when `distance_to_ghost` is not that well fit by a linear model. It also seems like it would be natural to use a function that could capture the oscillations of the ghost, given that people know the ghost bobs back and forth. Finally, it might be helpful to figure this out because theta power also won't be well fit by a linear model once we get to including this step.
5. **Add the remaining subjects** This is the near-term goal for this project. Did I just get lucky with `subject_38`? It is tempting to rush to this step, but I think working out the other steps first will preventably keep the picture clear. Will def want to parallelize this step, and will want to think careful about what I want to save out when. I think having one batch script fit all the models and perhaps save them out would be a good approach. And then I can lok at how I want to load in and save the various discrimination/calibration plots later. Or maybe just save out all those figures too? It would be nice as an html which I haven't tried to make work with a parralel job, but should be doable. Will be advantageous to have my new computer by this point 😬
6. **True/null model comparison/other variables/success criteria** Not even gonna think about this yet. TODO
    


