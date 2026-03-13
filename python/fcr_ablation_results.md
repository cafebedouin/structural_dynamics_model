
# FCR Override Ablation Results


## Flash (B) / FCR enabled

- **Constraints**: 887
- **FCR detected**: 696 (78.5%)
- **Descent rate**: 20.4% (H¹=0)
- **Gauge-variance rate**: 79.6%
- **Superselection gap**: H¹=1 empty, H¹=2 empty — HOLDS

### Type distribution — analytical perspective

| Type            | Count |       % |
|-----------------|-------|--------|
| tangled_rope    |   287 |  32.4% |
| snare           |   448 |  50.5% |
| mountain        |   139 |  15.7% |
| rope            |    10 |   1.1% |
| scaffold        |     2 |   0.2% |
| unknown         |     1 |   0.1% |

### Type distribution — modal (orbit majority)

*33 constraints (3.7%) had 2-2 ties, resolved by TIE_PRECEDENCE (snare > tangled_rope > ...)*

| Type            | Count |       % |
|-----------------|-------|--------|
| tangled_rope    |   385 |  43.4% |
| snare           |   312 |  35.2% |
| mountain        |   139 |  15.7% |
| rope            |    10 |   1.1% |
| scaffold        |    40 |   4.5% |
| unknown         |     1 |   0.1% |

### H¹ distribution

| H¹ | Count | % |
|---:|------:|--:|
| 0 | 181 | 20.4% |
| 3 | 353 | 39.8% |
| 4 | 14 | 1.6% |
| 5 | 320 | 36.1% |
| 6 | 19 | 2.1% |

## Flash (B) / FCR disabled

- **Constraints**: 887
- **FCR detected**: 696 (78.5%)
- **Descent rate**: 20.4% (H¹=0)
- **Gauge-variance rate**: 79.6%
- **Superselection gap**: H¹=1 empty, H¹=2 empty — HOLDS

### Type distribution — analytical perspective

| Type            | Count |       % |
|-----------------|-------|--------|
| tangled_rope    |   199 |  22.4% |
| snare           |   448 |  50.5% |
| mountain        |   139 |  15.7% |
| rope            |    10 |   1.1% |
| scaffold        |    29 |   3.3% |
| unknown         |    62 |   7.0% |

### Type distribution — modal (orbit majority)

*31 constraints (3.5%) had 2-2 ties, resolved by TIE_PRECEDENCE (snare > tangled_rope > ...)*

| Type            | Count |       % |
|-----------------|-------|--------|
| tangled_rope    |   318 |  35.9% |
| snare           |   312 |  35.2% |
| mountain        |   139 |  15.7% |
| rope            |    12 |   1.4% |
| scaffold        |    67 |   7.6% |
| unknown         |    39 |   4.4% |

### H¹ distribution

| H¹ | Count | % |
|---:|------:|--:|
| 0 | 181 | 20.4% |
| 3 | 343 | 38.7% |
| 4 | 12 | 1.4% |
| 5 | 332 | 37.4% |
| 6 | 19 | 2.1% |

## Haiku (A) / FCR enabled

- **Constraints**: 960
- **FCR detected**: 812 (84.6%)
- **Descent rate**: 68.3% (H¹=0)
- **Gauge-variance rate**: 31.7%
- **Superselection gap**: H¹=1 empty, H¹=2 empty — HOLDS

### Type distribution — analytical perspective

| Type            | Count |       % |
|-----------------|-------|--------|
| tangled_rope    |   580 |  60.4% |
| snare           |   242 |  25.2% |
| mountain        |   130 |  13.5% |
| rope            |     7 |   0.7% |
| unknown         |     1 |   0.1% |

### Type distribution — modal (orbit majority)

*97 constraints (10.1%) had 2-2 ties, resolved by TIE_PRECEDENCE (snare > tangled_rope > ...)*

| Type            | Count |       % |
|-----------------|-------|--------|
| tangled_rope    |   639 |  66.6% |
| snare           |   141 |  14.7% |
| mountain        |   120 |  12.5% |
| rope            |    16 |   1.7% |
| scaffold        |    23 |   2.4% |
| piton           |     1 |   0.1% |
| naturalized     |    19 |   2.0% |
| unknown         |     1 |   0.1% |

### H¹ distribution

| H¹ | Count | % |
|---:|------:|--:|
| 0 | 656 | 68.3% |
| 3 | 68 | 7.1% |
| 4 | 27 | 2.8% |
| 5 | 139 | 14.5% |
| 6 | 70 | 7.3% |

## Haiku (A) / FCR disabled

- **Constraints**: 960
- **FCR detected**: 812 (84.6%)
- **Descent rate**: 68.2% (H¹=0)
- **Gauge-variance rate**: 31.8%
- **Superselection gap**: H¹=1 empty, H¹=2 empty — HOLDS

### Type distribution — analytical perspective

| Type            | Count |       % |
|-----------------|-------|--------|
| tangled_rope    |   556 |  57.9% |
| snare           |   243 |  25.3% |
| mountain        |   130 |  13.5% |
| rope            |     7 |   0.7% |
| scaffold        |    20 |   2.1% |
| unknown         |     4 |   0.4% |

### Type distribution — modal (orbit majority)

*97 constraints (10.1%) had 2-2 ties, resolved by TIE_PRECEDENCE (snare > tangled_rope > ...)*

| Type            | Count |       % |
|-----------------|-------|--------|
| tangled_rope    |   601 |  62.6% |
| snare           |   142 |  14.8% |
| mountain        |   120 |  12.5% |
| rope            |    16 |   1.7% |
| scaffold        |    43 |   4.5% |
| piton           |     1 |   0.1% |
| naturalized     |    19 |   2.0% |
| unknown         |    18 |   1.9% |

### H¹ distribution

| H¹ | Count | % |
|---:|------:|--:|
| 0 | 655 | 68.2% |
| 3 | 67 | 7.0% |
| 4 | 27 | 2.8% |
| 5 | 141 | 14.7% |
| 6 | 70 | 7.3% |


## Convergence Comparison (modal type)


### FCR enabled

| Type            |  Flash % |  Haiku % |  Delta |
|-----------------|----------|----------|--------|
| mountain        |    15.7% |    12.5% |  -3.2% |
| naturalized     |     0.0% |     2.0% |  +2.0% |
| piton           |     0.0% |     0.1% |  +0.1% |
| rope            |     1.1% |     1.7% |  +0.5% |
| scaffold        |     4.5% |     2.4% |  -2.1% |
| snare           |    35.2% |    14.7% | -20.5% |
| tangled_rope    |    43.4% |    66.6% | +23.2% |
| unknown         |     0.1% |     0.1% |  -0.0% |

### FCR disabled

| Type            |  Flash % |  Haiku % |  Delta |
|-----------------|----------|----------|--------|
| mountain        |    15.7% |    12.5% |  -3.2% |
| naturalized     |     0.0% |     2.0% |  +2.0% |
| piton           |     0.0% |     0.1% |  +0.1% |
| rope            |     1.4% |     1.7% |  +0.3% |
| scaffold        |     7.6% |     4.5% |  -3.1% |
| snare           |    35.2% |    14.8% | -20.4% |
| tangled_rope    |    35.9% |    62.6% | +26.8% |
| unknown         |     4.4% |     1.9% |  -2.5% |


## Convergence Comparison (analytical perspective)


### FCR enabled

| Type            |  Flash % |  Haiku % |  Delta |
|-----------------|----------|----------|--------|
| mountain        |    15.7% |    13.5% |  -2.1% |
| rope            |     1.1% |     0.7% |  -0.4% |
| scaffold        |     0.2% |     0.0% |  -0.2% |
| snare           |    50.5% |    25.2% | -25.3% |
| tangled_rope    |    32.4% |    60.4% | +28.1% |
| unknown         |     0.1% |     0.1% |  -0.0% |

### FCR disabled

| Type            |  Flash % |  Haiku % |  Delta |
|-----------------|----------|----------|--------|
| mountain        |    15.7% |    13.5% |  -2.1% |
| rope            |     1.1% |     0.7% |  -0.4% |
| scaffold        |     3.3% |     2.1% |  -1.2% |
| snare           |    50.5% |    25.3% | -25.2% |
| tangled_rope    |    22.4% |    57.9% | +35.5% |
| unknown         |     7.0% |     0.4% |  -6.6% |


## Key Finding

### Modal type (orbit majority — paper §5.2 unit)

- Flash tangled_rope: 43.4% (enabled) -> 35.9% (disabled)
- Haiku tangled_rope: 66.6% (enabled) -> 62.6% (disabled)
- Convergence gap (enabled): 23.2pp
- Convergence gap (disabled): 26.8pp

**Convergence BREAKS without FCR** (gap > 20pp). The FCR override is the primary attractor mechanism.

### Analytical perspective (for reference)

- Flash tangled_rope: 32.4% (enabled) -> 22.4% (disabled)
- Haiku tangled_rope: 60.4% (enabled) -> 57.9% (disabled)
- Convergence gap (enabled): 28.1pp
- Convergence gap (disabled): 35.5pp
