# vioplot 0.3.0

## Major changes

- formula inputs

vioplot is not compatible with all inputs of boxplot or beanplot, including formula inputs (implemented as S3 methods.

- plot customisation

Various features of violins can be tweaked with plotting parameters, such as colours and shapes of aspects of the violin. These can be applied to all violins with a single (scalar) input or applied separately to each violin with multiple (vector) inputs 

- defaults

This version is fully compatible with inputs to vioplot 0.2. The only difference in behaviour changing the default colour from a glaring magenta to a monochrome grey (more appropriate in a wider range of professional settings). Code written for previous versions should run without breaking or changes in behaviour apart from the default colour.
