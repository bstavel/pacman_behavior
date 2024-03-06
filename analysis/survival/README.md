# Joint Modeling of Longitudinal and Survival Data

Joint modeling of longitudinal and survival data is a statistical approach used to simultaneously analyze two types of data: longitudinal data (repeated measurements over time) and survival data (time-to-event data). This methodology is particularly common in public health research, where you often observe both the progression of a disease over time (longitudinal data) and the occurrence of an event of interest like death or disease relapse (survival data).

#### Key Components:
**Longitudinal Data Component:** This part of the model deals with repeated measurements taken over time on a subject. In the public health context, these measurements like blood pressure, tumor size, or cholesterol levels, but in the context of the pacman data these are `distance_to_ghost`, `points_remaining`, and other reward/threat/conflict values that change over the trial. The focus is on how these measurements change over time for each trial. A common approach to model this is using linear mixed models, which can handle the within-subject correlation inherent in repeated measurements.

**Survival Data Component:** This aspect focuses on the time until an event of interest occurs (e.g., death, or here, time of turnaround). Happily, while these models were developed to handle missingness, we don't have to deal with censorship problems. Survival models, like Cox proportional hazards models, are commonly used to handle this kind of data.


## Current To-Do

**December 2023**

<del>
    1. <strong>Test models' predictive validity:</strong> Right now these models are looking promising, but I need to move from being excited about them to really trying to break them. It is still possible that too much depends on the general time information that the model receives rather than the context of the predicting variables themselves.
    <ul>
        <li><em>How much does the accuracy of the prediction depend on the time window for prediction?</em></li>
            <ul>
                <li>create train/test sets with a wide range of turn times</li>
                <li>visualization with a data point with every error</li>
            </ul>
        <li><em>Formalize comparison with null/permuted models</em>
            <ul>
                <li>While I am already using some permuted models, it is difficult to create a good shuffle because of the role of time. Need to think about this more.</li>
            </ul>
        </li>
    </ul>
   2. <strong>Make functions for each step, including likely saving out the model after it is fit to save time</strong>
       <ul>
           <li>These functions should be built with the idea of parallelizing across subjects eventually.</li>
       </ul>
</del>
    4. <strong>Consider alternate forms of the longitudinal data component: </strong> It is weird that the models work so well when `distance_to_ghost` is not that well fit by a linear model. It also seems like it would be natural to use a function that could capture the oscillations of the ghost, given that people know the ghost bobs back and forth. Finally, it might be helpful to figure this out because theta power also won't be well fit by a linear model once we get to including this step.
<br>
<del>
    5. <strong>Add the remaining subjects</strong> This is the near-term goal for this project. Did I just get lucky with <code>subject_38</code>? It is tempting to rush to this step, but I think working out the other steps first will preventably keep the picture clear. Will definitely want to parallelize this step, and will want to think carefully about what I want to save out when. I think having one batch script fit all the models and perhaps save them out would be a good approach. And then I can look at how I want to load in and save the various discrimination/calibration plots later. Or maybe just save out all those figures too? It would be nice as an html which I haven't tried to make work with a parallel job, but should be doable. Will be advantageous to have my new computer by this point 😬
</del>
<br>
<br>

**January 2024**
1. **Add Other Models for Comparison:** Thus far, it looks like my original model is performing significantly better than a permuted model over all the participants. In particular, the distribution of AUC scores for orginal model is significantly higher than the permuted model (mean diff: .08; CI [.02, .13]) (YAY). Of course, there are a subset of subjects where either the original model is equivalent to the permuted model and a subset where the original model performs worse. I want to add two other models to see if it is always these set of subjects that are being poorly fit. The first to-do will be to fit these additional models. In particular, my intial, core set of models will be:
    * `standard_model` This is a model that includes both `distance_to_ghost` and `points_remaining`
    * `reward_only` This model only inlcudes `points_remaining`
    * `threat_only` This model only includes `distance_to_ghost`
    * `permuted_model` This model includes accurate time information and permuted/shuffled versions of both `distance_to_ghost` and `points_remaining`
2. **Consider alternate forms of the longitudinal data component:** It is weird that the models work so well when `distance_to_ghost` is not that well fit by a linear model. It also seems like it would be natural to use a function that could capture the oscillations of the ghost, given that people know the ghost bobs back and forth. Finally, it might be helpful to figure this out because theta power also won't be well fit by a linear model once we get to including this step. This was in the TO-DO from last Decemeber but I didn't get to it. I think it will be more interpretable anyways when I have the other model variations to compare it to. That way I can see if it helps all the models or just one of the predictors specifically.
3. **Explore the poorly fit subjects:** See if I can see different patterns of behavior in the poorly fit subjects. Should compare with Jules work this past fall to see if these subjects match any of the behavioral phenotypes she found      

