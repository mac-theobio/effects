FIXME:

Thanks. I actually don't think that was the problem, so I feel a
little bit better about myself (I tried lots of things and may have
pushed at a random time). In any case, thanks for solving.

Take a look at the anchorplot again when you can.

We should definitely avoid saying things like:

> , values=c("pred"="red", "eff"="blue", "zero"="black")

since it's not DRY. I don't think it should ever be necessary.

I think it would be better if the default linetypes were based on
estimate vs. confidence limit, rather than on which model we're using.
That does mean we're more reliant on color, though... I still think
it's better for now. Maybe make a type column which is either "est" or
"bound" and use lty 1 and 3 respectively. Am I making sense?

Also, is there a way to add the anchor point to the grid, so that the
CI lines always meet at the anchor (when the anchor is achieved)? If
you look at the current anchorplot, you will see that the "zero"
bounds look a little weird at the intersection; this could be solved
by making sure the anchor is on the grid, I think.

You will need to install https://github.com/JLSteenwyk/ggpubfigs to
use the current anchorplot.

When do we expect joint model work well
