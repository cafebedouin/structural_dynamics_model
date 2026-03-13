
# FCR Override Ablation Results


## Flash (B) / FCR enabled

- **Constraints**: 7626
- **FCR detected**: 5996 (78.6%)
- **Descent rate**: 19.8% (H¹=0)
- **Gauge-variance rate**: 80.2%
- **Superselection gap**: H¹=1 empty, H¹=2 empty — HOLDS

### Post-override type distribution (analytical perspective)

| Type            | Count |       % |
|-----------------|-------|--------|
| tangled_rope    |  2411 |  31.6% |
| snare           |  3921 |  51.4% |
| mountain        |  1202 |  15.8% |
| rope            |    79 |   1.0% |
| scaffold        |    12 |   0.2% |
| unknown         |     1 |   0.0% |

### H¹ distribution

| H¹ | Count | % |
|---:|------:|--:|
| 0 | 1509 | 19.8% |
| 3 | 3020 | 39.6% |
| 4 | 127 | 1.7% |
| 5 | 2796 | 36.7% |
| 6 | 174 | 2.3% |

## Flash (B) / FCR disabled

- **Constraints**: 7626
- **FCR detected**: 5996 (78.6%)
- **Descent rate**: 19.8% (H¹=0)
- **Gauge-variance rate**: 80.2%
- **Superselection gap**: H¹=1 empty, H¹=2 empty — HOLDS

### Post-override type distribution (analytical perspective)

| Type            | Count |       % |
|-----------------|-------|--------|
| tangled_rope    |  1731 |  22.7% |
| snare           |  3921 |  51.4% |
| mountain        |  1202 |  15.8% |
| rope            |    79 |   1.0% |
| scaffold        |   205 |   2.7% |
| unknown         |   488 |   6.4% |

### H¹ distribution

| H¹ | Count | % |
|---:|------:|--:|
| 0 | 1509 | 19.8% |
| 3 | 2933 | 38.5% |
| 4 | 111 | 1.5% |
| 5 | 2899 | 38.0% |
| 6 | 174 | 2.3% |

## Haiku (A) / FCR enabled

- **Constraints**: 10246
- **FCR detected**: 8760 (85.5%)
- **Descent rate**: 68.5% (H¹=0)
- **Gauge-variance rate**: 31.5%
- **Superselection gap**: H¹=1 empty, H¹=2 empty — HOLDS

### Post-override type distribution (analytical perspective)

| Type            | Count |       % |
|-----------------|-------|--------|
| tangled_rope    |  6286 |  61.4% |
| snare           |  2567 |  25.1% |
| mountain        |  1328 |  13.0% |
| rope            |    64 |   0.6% |
| unknown         |     1 |   0.0% |

### H¹ distribution

| H¹ | Count | % |
|---:|------:|--:|
| 0 | 7021 | 68.5% |
| 3 | 711 | 6.9% |
| 4 | 294 | 2.9% |
| 5 | 1460 | 14.2% |
| 6 | 760 | 7.4% |

## Haiku (A) / FCR disabled

- **Constraints**: 10246
- **FCR detected**: 8760 (85.5%)
- **Descent rate**: 68.4% (H¹=0)
- **Gauge-variance rate**: 31.6%
- **Superselection gap**: H¹=1 empty, H¹=2 empty — HOLDS

### Post-override type distribution (analytical perspective)

| Type            | Count |       % |
|-----------------|-------|--------|
| tangled_rope    |  6051 |  59.1% |
| snare           |  2577 |  25.2% |
| mountain        |  1328 |  13.0% |
| rope            |    64 |   0.6% |
| scaffold        |   195 |   1.9% |
| unknown         |    31 |   0.3% |

### H¹ distribution

| H¹ | Count | % |
|---:|------:|--:|
| 0 | 7009 | 68.4% |
| 3 | 702 | 6.9% |
| 4 | 294 | 2.9% |
| 5 | 1481 | 14.5% |
| 6 | 760 | 7.4% |


## Convergence Comparison


### FCR enabled

| Type            |  Flash % |  Haiku % |  Delta |
|-----------------|----------|----------|--------|
| mountain        |    15.8% |    13.0% |  -2.8% |
| rope            |     1.0% |     0.6% |  -0.4% |
| scaffold        |     0.2% |     0.0% |  -0.2% |
| snare           |    51.4% |    25.1% | -26.4% |
| tangled_rope    |    31.6% |    61.4% | +29.7% |
| unknown         |     0.0% |     0.0% |  -0.0% |

### FCR disabled

| Type            |  Flash % |  Haiku % |  Delta |
|-----------------|----------|----------|--------|
| mountain        |    15.8% |    13.0% |  -2.8% |
| rope            |     1.0% |     0.6% |  -0.4% |
| scaffold        |     2.7% |     1.9% |  -0.8% |
| snare           |    51.4% |    25.2% | -26.3% |
| tangled_rope    |    22.7% |    59.1% | +36.4% |
| unknown         |     6.4% |     0.3% |  -6.1% |


## Key Finding

- Flash tangled_rope: 31.6% (enabled) -> 22.7% (disabled)
- Haiku tangled_rope: 61.4% (enabled) -> 59.1% (disabled)
- Convergence gap (enabled): 29.7pp
- Convergence gap (disabled): 36.4pp

**Convergence BREAKS without FCR** (gap > 20pp). The FCR override is the primary attractor mechanism.
