# Export the closed testing strategy as a gMCPLite graph object

Export the original or adapted graph and weights of an adagraph design
to an graphMCP object as used by gMCPLite. This allows to use the
testing and graphing methods of gMCPLite for the testing strategy.

## Usage

``` r
export_gmcp_lite(design, adapted = design$adaptations)
```

## Arguments

- design:

  the adagraph_design object that should be exported

- adapted:

  boolean indicating if the adapted version of the testing strategy
  should be exported. Defaults to TRUE if an adaptation has been made,
  FALSE else

## Value

An object with S4 class `graphMCP`, with matrix and weights of the input
design
