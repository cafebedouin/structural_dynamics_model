# Index Variance Analysis

## Summary Statistics

- **Total constraints analyzed:** 1034
- **Constraints with multiple index configs:** 1014 (98.1%)
- **High variance (>0.5):** 859 (83.1%)
- **Stable (ratio=1.0):** 567 (54.8%)

## Variance Distribution

| Ratio Range | Count | % of Corpus | Examples |
|-------------|-------|-------------|----------|
| 1.0 (stable)    |   567 |   54.8% | abstraction_boundary_overrun, abstraction_leaka... |
| 0.7-0.9         |   179 |   17.3% | absorbing_markov_chain_trap, ai_compute_capital... |
| 0.5-0.6         |   167 |   16.2% | ai_performance_watermark, alzheimers_levetirace... |
| 0.3-0.4         |    99 |    9.6% | asean_ceasefire_2011, automatic_enrollment_defa... |
| <0.3            |    22 |    2.1% | banach_fixed_point_theorem, cantor_set_topology... |
| null            |     0 |    0.0% | - |

## Domain Breakdown

| Domain | N | Avg Variance | High Variance % |
|--------|---|--------------|----------------|
| bio_industrial       |   1 |         1.00 |          100.0% |
| biological           |   1 |         1.00 |          100.0% |
| biological/cognitive/educational |   1 |         1.00 |          100.0% |
| biological/environmental |   1 |         1.00 |          100.0% |
| biological/healthcare |   1 |         1.00 |          100.0% |
| biological/linguistic/medical |   1 |         1.00 |          100.0% |
| biological/philosophy/social |   1 |         1.00 |          100.0% |
| biological/political/economic |   1 |         1.00 |          100.0% |
| biological/technological |   1 |         1.00 |          100.0% |
| biological/technological/economic |   1 |         1.00 |          100.0% |
| cognitive/informational/technological |   1 |         1.00 |          100.0% |
| cognitive/organizational/economic |   1 |         1.00 |          100.0% |
| cognitive/social/economic |   1 |         1.00 |          100.0% |
| cognitive/technological/social |   1 |         1.00 |          100.0% |
| corporate_governance |   1 |         1.00 |          100.0% |
| economic, environmental, political |   1 |         1.00 |          100.0% |
| economic/cognitive   |   1 |         1.00 |          100.0% |
| economic/cultural    |   1 |         1.00 |          100.0% |
| economic/educational |   1 |         1.00 |          100.0% |
| economic/environmental |   1 |         1.00 |          100.0% |
| economic/legal       |   1 |         1.00 |          100.0% |
| economic/logistical  |   1 |         1.00 |          100.0% |
| economic/organizational/technological |   1 |         1.00 |          100.0% |
| economic/political/legal |   1 |         1.00 |          100.0% |
| economic/political/social |   1 |         1.00 |          100.0% |
| economic/political/technological |   2 |         1.00 |          100.0% |
| economic/psychological/technological |   1 |         1.00 |          100.0% |
| economic/scientific  |   1 |         1.00 |          100.0% |
| economic/social/legal |   1 |         1.00 |          100.0% |
| economic/social/logistical |   1 |         1.00 |          100.0% |
| economic/social/political |   1 |         1.00 |          100.0% |
| economic/social/technological |   5 |         1.00 |          100.0% |
| economic/technological/environmental |   1 |         1.00 |          100.0% |
| economic/technological/infrastructural |   1 |         1.00 |          100.0% |
| economic/technological/logistical |   1 |         1.00 |          100.0% |
| economic/technological/political |   1 |         1.00 |          100.0% |
| economic/technological/regulatory |   1 |         1.00 |          100.0% |
| environmental/cultural |   1 |         1.00 |          100.0% |
| environmental/economic |   2 |         1.00 |          100.0% |
| environmental/economic/technological |   1 |         1.00 |          100.0% |
| environmental/political |   1 |         1.00 |          100.0% |
| environmental/social |   1 |         1.00 |          100.0% |
| geopolitical/information |   1 |         1.00 |          100.0% |
| geopolitical/maritime |   1 |         1.00 |          100.0% |
| geopolitical/political |   1 |         1.00 |          100.0% |
| health/economic      |   1 |         1.00 |          100.0% |
| health/medical       |   1 |         1.00 |          100.0% |
| health/scientific/economic |   1 |         1.00 |          100.0% |
| informational/psychological/sociological |   1 |         1.00 |          100.0% |
| informational/social/economic |   1 |         1.00 |          100.0% |
| infrastructure/logistical/technological |   1 |         1.00 |          100.0% |
| institutional        |   1 |         1.00 |          100.0% |
| institutional/technological |   2 |         1.00 |          100.0% |
| legal/geopolitical/environmental |   1 |         1.00 |          100.0% |
| legal/institutional  |   2 |         1.00 |          100.0% |
| legal/social         |   1 |         1.00 |          100.0% |
| linguistic/technological/social |   1 |         1.00 |          100.0% |
| logistical/technological/economic |   1 |         1.00 |          100.0% |
| logistics/physical_infrastructure/economic |   1 |         1.00 |          100.0% |
| mathematical/biological |   1 |         1.00 |          100.0% |
| mathematical/philosophical |   1 |         1.00 |          100.0% |
| medical/health       |   1 |         1.00 |          100.0% |
| medical/legal/institutional |   1 |         1.00 |          100.0% |
| medical/neurological |   1 |         1.00 |          100.0% |
| medical/technological/legal |   1 |         1.00 |          100.0% |
| military/social      |   1 |         1.00 |          100.0% |
| military/special_operations |   1 |         1.00 |          100.0% |
| organizational/economic |   1 |         1.00 |          100.0% |
| organizational/legal/socio-economic |   1 |         1.00 |          100.0% |
| organizational/legal/technological |   1 |         1.00 |          100.0% |
| organizational/political |   1 |         1.00 |          100.0% |
| organizational/political/technological |   2 |         1.00 |          100.0% |
| organizational/social |   1 |         1.00 |          100.0% |
| organizational/technological |   1 |         1.00 |          100.0% |
| organizational/technological/cognitive |   1 |         1.00 |          100.0% |
| philosophical        |   1 |         1.00 |          100.0% |
| philosophical/evolutionary |   1 |         1.00 |          100.0% |
| philosophical/political |   1 |         1.00 |          100.0% |
| philosophical/religious |   1 |         1.00 |          100.0% |
| philosophical/social/technological |   2 |         1.00 |          100.0% |
| physics/economics/mathematics |   1 |         1.00 |          100.0% |
| political/economic/social |   1 |         1.00 |          100.0% |
| political/economic/social/technological |   1 |         1.00 |          100.0% |
| political/environmental |   1 |         1.00 |          100.0% |
| political/environmental/technological |   1 |         1.00 |          100.0% |
| political/espionage  |   2 |         1.00 |          100.0% |
| political/geopolitical |   1 |         1.00 |          100.0% |
| political/institutional |   1 |         1.00 |          100.0% |
| political/intelligence |   1 |         1.00 |          100.0% |
| political/legal/economic |   1 |         1.00 |          100.0% |
| political/legal/social |   2 |         1.00 |          100.0% |
| political/military/technological |   1 |         1.00 |          100.0% |
| political/organizational |   3 |         1.00 |          100.0% |
| political/organizational/informational |   1 |         1.00 |          100.0% |
| political/organizational/legal |   1 |         1.00 |          100.0% |
| political/security   |   1 |         1.00 |          100.0% |
| political/social/geopolitical |   1 |         1.00 |          100.0% |
| political/social/governance |   1 |         1.00 |          100.0% |
| political/social/technological |   2 |         1.00 |          100.0% |
| political/technological/legal |   1 |         1.00 |          100.0% |
| psychological/biological |   1 |         1.00 |          100.0% |
| psychological/economic |   1 |         1.00 |          100.0% |
| psychological/economic/social |   1 |         1.00 |          100.0% |
| psychology/behavioral_science |   1 |         1.00 |          100.0% |
| religious/economic/political |   1 |         1.00 |          100.0% |
| religious/ethical    |   1 |         1.00 |          100.0% |
| religious/philosophical/social |   1 |         1.00 |          100.0% |
| religious/political/social |   1 |         1.00 |          100.0% |
| religious/social     |   2 |         1.00 |          100.0% |
| religious/social/philosophical |   1 |         1.00 |          100.0% |
| scientific/biological |   1 |         1.00 |          100.0% |
| scientific/sociological |   1 |         1.00 |          100.0% |
| snare                |   2 |         1.00 |          100.0% |
| social/academic/economic |   1 |         1.00 |          100.0% |
| social/artistic/biological |   1 |         1.00 |          100.0% |
| social/artistic/psychological |   1 |         1.00 |          100.0% |
| social/cognitive     |   3 |         1.00 |          100.0% |
| social/cognitive/technological |   1 |         1.00 |          100.0% |
| social/cultural      |   2 |         1.00 |          100.0% |
| social/economic/biological |   1 |         1.00 |          100.0% |
| social/economic/educational |   1 |         1.00 |          100.0% |
| social/economic/informational |   1 |         1.00 |          100.0% |
| social/economic/linguistic |   1 |         1.00 |          100.0% |
| social/economic/religious |   1 |         1.00 |          100.0% |
| social/environmental/biological |   1 |         1.00 |          100.0% |
| social/existential   |   1 |         1.00 |          100.0% |
| social/informational/technological |   2 |         1.00 |          100.0% |
| social/legal/technological |   1 |         1.00 |          100.0% |
| social/linguistic/bureaucratic |   1 |         1.00 |          100.0% |
| social/medical       |   1 |         1.00 |          100.0% |
| social/organizational/religious |   1 |         1.00 |          100.0% |
| social/organizational/technological |   1 |         1.00 |          100.0% |
| social/political/biological |   1 |         1.00 |          100.0% |
| social/political/health |   1 |         1.00 |          100.0% |
| social/political/informational |   1 |         1.00 |          100.0% |
| social/political/linguistic |   1 |         1.00 |          100.0% |
| social/political/nationalist |   1 |         1.00 |          100.0% |
| social/political/organizational |   1 |         1.00 |          100.0% |
| social/political/religious |   2 |         1.00 |          100.0% |
| social/psychological/biological |   1 |         1.00 |          100.0% |
| social/psychological/economic |   1 |         1.00 |          100.0% |
| social/psychological/religious |   1 |         1.00 |          100.0% |
| social/religious/philosophical |   1 |         1.00 |          100.0% |
| social/religious/technological |   2 |         1.00 |          100.0% |
| social/scientific    |   1 |         1.00 |          100.0% |
| social/technological/biological |   2 |         1.00 |          100.0% |
| social/technological/economic |   2 |         1.00 |          100.0% |
| social/technological/educational |   1 |         1.00 |          100.0% |
| social/technological/political |   1 |         1.00 |          100.0% |
| social/technological/psychological |   1 |         1.00 |          100.0% |
| social/technological/religious |   1 |         1.00 |          100.0% |
| social/wellness/technological |   1 |         1.00 |          100.0% |
| socio_political      |   1 |         1.00 |          100.0% |
| tangled_rope         |   2 |         1.00 |          100.0% |
| technological / social_policy |   1 |         1.00 |          100.0% |
| technological/AI/cybernetic |   2 |         1.00 |          100.0% |
| technological/AI/economic |   2 |         1.00 |          100.0% |
| technological/AI/governance |   1 |         1.00 |          100.0% |
| technological/AI/informational |   2 |         1.00 |          100.0% |
| technological/academic/informational |   1 |         1.00 |          100.0% |
| technological/analytical |   1 |         1.00 |          100.0% |
| technological/cognitive/organizational |   1 |         1.00 |          100.0% |
| technological/computational |   2 |         1.00 |          100.0% |
| technological/cybernetic/organizational |   1 |         1.00 |          100.0% |
| technological/cybernetic/security |   1 |         1.00 |          100.0% |
| technological/economic/biological |   2 |         1.00 |          100.0% |
| technological/economic/cybernetic |   1 |         1.00 |          100.0% |
| technological/educational/economic |   2 |         1.00 |          100.0% |
| technological/environmental |   1 |         1.00 |          100.0% |
| technological/geopolitical |   2 |         1.00 |          100.0% |
| technological/informational/cognitive |   1 |         1.00 |          100.0% |
| technological/infrastructural/economic |   1 |         1.00 |          100.0% |
| technological/labor  |   1 |         1.00 |          100.0% |
| technological/legal/economic |   2 |         1.00 |          100.0% |
| technological/logic  |   1 |         1.00 |          100.0% |
| technological/logistical/economic |   1 |         1.00 |          100.0% |
| technological/mathematics |   1 |         1.00 |          100.0% |
| technological/military/legal |   1 |         1.00 |          100.0% |
| technological/organizational |   2 |         1.00 |          100.0% |
| technological/political/social |   1 |         1.00 |          100.0% |
| technological/scientific/legal |   1 |         1.00 |          100.0% |
| technological/scientific/social |   1 |         1.00 |          100.0% |
| technological/security/biological |   1 |         1.00 |          100.0% |
| technological/security/political |   1 |         1.00 |          100.0% |
| technological/semiconductors |   1 |         1.00 |          100.0% |
| technological/social/biological |   1 |         1.00 |          100.0% |
| technological/social/cognitive |   1 |         1.00 |          100.0% |
| technological/social/legal |   1 |         1.00 |          100.0% |
| technological/social/philosophical |   1 |         1.00 |          100.0% |
| technological/social/political |   2 |         1.00 |          100.0% |
| unknown_novel        |   1 |         1.00 |          100.0% |
| social/political     |   9 |         0.97 |          100.0% |
| technological/cognitive |   6 |         0.96 |          100.0% |
| political/technological |  11 |         0.95 |          100.0% |
| economic/technological |  32 |         0.94 |          100.0% |
| economic/geopolitical |   4 |         0.94 |          100.0% |
| political/economic/technological |   4 |         0.92 |          100.0% |
| social/economic/technological |   3 |         0.92 |          100.0% |
| technological/social/economic |   3 |         0.92 |          100.0% |
| technological/social |  26 |         0.90 |           96.2% |
| psychological/social |   5 |         0.90 |          100.0% |
| technological/economic |  44 |         0.89 |           93.2% |
| social               |  17 |         0.89 |           94.1% |
| social/political/technological |   3 |         0.89 |          100.0% |
| technological/institutional |   3 |         0.89 |          100.0% |
| political            |  54 |         0.88 |           94.4% |
| biological/technological/social |   2 |         0.88 |          100.0% |
| philosophical/social |   2 |         0.88 |          100.0% |
| political/social/economic |   2 |         0.88 |          100.0% |
| social/technological |  12 |         0.87 |           91.7% |
| social/economic      |   9 |         0.86 |          100.0% |
| economic/social      |  22 |         0.86 |           86.4% |
| social/psychological |  17 |         0.86 |           94.1% |
| political/social     |  19 |         0.85 |           84.2% |
| investigation/testing |   2 |         0.83 |          100.0% |
| mathematical/economic |   2 |         0.83 |          100.0% |
| political/technological/social |   2 |         0.83 |          100.0% |
| social/familial      |   2 |         0.83 |          100.0% |
| social/philosophical |   2 |         0.83 |          100.0% |
| economic             |  68 |         0.82 |           95.6% |
| economic/political   |  41 |         0.82 |           90.2% |
| political/economic   |  16 |         0.81 |           81.2% |
| economic/legal/technological |   1 |         0.80 |          100.0% |
| economic/technological/healthcare |   2 |         0.80 |          100.0% |
| technological/biological |   5 |         0.80 |           80.0% |
| political/legal      |  10 |         0.79 |           80.0% |
| technological/political |  12 |         0.79 |           91.7% |
| geopolitical/economic |  13 |         0.75 |           92.3% |
| biological/economic/social |   1 |         0.75 |          100.0% |
| ecological/economic  |   1 |         0.75 |          100.0% |
| economic, technological, geopolitical |   1 |         0.75 |          100.0% |
| economic/technological/legal |   1 |         0.75 |          100.0% |
| geopolitical/military |   3 |         0.75 |           66.7% |
| geopolitical/technological/economic |   1 |         0.75 |          100.0% |
| legal / institutional |   1 |         0.75 |          100.0% |
| political, economic, environmental |   1 |         0.75 |          100.0% |
| political/regulatory |   1 |         0.75 |          100.0% |
| political/religious  |   1 |         0.75 |          100.0% |
| political/social/religious |   1 |         0.75 |          100.0% |
| psychological/professional/biochemical |   1 |         0.75 |          100.0% |
| religious/linguistic/political |   1 |         0.75 |          100.0% |
| religious/social/psychological |   1 |         0.75 |          100.0% |
| scientific/political/economic |   2 |         0.75 |           50.0% |
| social/environmental |   1 |         0.75 |          100.0% |
| social/ethical       |   1 |         0.75 |          100.0% |
| social/institutional |   1 |         0.75 |          100.0% |
| social/intellectual  |   1 |         0.75 |          100.0% |
| social/political/philosophical |   1 |         0.75 |          100.0% |
| social/professional  |   1 |         0.75 |          100.0% |
| systems_engineering/economics/ecology |   1 |         0.75 |          100.0% |
| technological / economic / healthcare |   1 |         0.75 |          100.0% |
| technological/military |   1 |         0.75 |          100.0% |
| technological/scientific |   9 |         0.73 |           77.8% |
| legal/economic       |   2 |         0.71 |          100.0% |
| technological        | 108 |         0.69 |           61.1% |
| political/military   |   4 |         0.69 |           75.0% |
| geopolitical/technological |   4 |         0.68 |           75.0% |
| Political / Scientific |   1 |         0.67 |          100.0% |
| Social/Psychological |   1 |         0.67 |          100.0% |
| astrophysical/cosmological |   1 |         0.67 |          100.0% |
| atmospheric_science  |   1 |         0.67 |          100.0% |
| biological/medical   |   1 |         0.67 |          100.0% |
| ecological/economic/social |   1 |         0.67 |          100.0% |
| economic/financial   |   1 |         0.67 |          100.0% |
| economic/fiscal      |   1 |         0.67 |          100.0% |
| economic/social/biological |   1 |         0.67 |          100.0% |
| economic/social/cognitive |   1 |         0.67 |          100.0% |
| health/agriculture   |   1 |         0.67 |          100.0% |
| health/technological |   1 |         0.67 |          100.0% |
| legal/economic/social |   1 |         0.67 |          100.0% |
| legal/technological  |   1 |         0.67 |          100.0% |
| magical/social       |   1 |         0.67 |          100.0% |
| mathematics/technological |   1 |         0.67 |          100.0% |
| philosophical/logical |   1 |         0.67 |          100.0% |
| philosophical/technological |   1 |         0.67 |          100.0% |
| scientific (mathematical physics / quantum chaos) |   2 |         0.67 |           50.0% |
| social/behavioral    |   1 |         0.67 |          100.0% |
| social/biological    |   1 |         0.67 |          100.0% |
| social/humanitarian/criminal |   1 |         0.67 |          100.0% |
| social/religious     |   1 |         0.67 |          100.0% |
| technological/cryptographic |   1 |         0.67 |          100.0% |
| technological/economic/environmental |   1 |         0.67 |          100.0% |
| technological/legal  |   3 |         0.67 |           66.7% |
| geopolitical         |  21 |         0.65 |           81.0% |
| economic/technological/social |   2 |         0.62 |           50.0% |
| scientific           |   4 |         0.62 |           75.0% |
| geopolitical/legal   |   3 |         0.62 |           66.7% |
| economic/environmental/political |   1 |         0.60 |          100.0% |
| political/technological/economic |   1 |         0.60 |          100.0% |
| scientific/political |   1 |         0.60 |          100.0% |
| mathematical/physical |   5 |         0.58 |           60.0% |
| political/social/legal |   2 |         0.58 |           50.0% |
| technological/mathematical |   9 |         0.52 |           44.4% |
| analytical/logic     |   1 |         0.50 |            0.0% |
| biological/scientific |   2 |         0.50 |           50.0% |
| economic/policy      |   1 |         0.50 |            0.0% |
| linguistic/cultural  |   1 |         0.50 |            0.0% |
| mountain             |   1 |         0.50 |            0.0% |
| philosophical/cognitive |   1 |         0.50 |            0.0% |
| physics/cosmology    |   1 |         0.50 |            0.0% |
| scientific/cosmological |   1 |         0.50 |            0.0% |
| scientific/technological |   1 |         0.50 |            0.0% |
| social/linguistic/political |   1 |         0.50 |            0.0% |
| technology/physics   |   1 |         0.50 |            0.0% |
| legal/political      |   2 |         0.47 |           50.0% |
| scientific/physical  |   5 |         0.47 |           20.0% |
| mathematical/logical |  15 |         0.46 |           26.7% |
| mathematical/technological |   9 |         0.39 |           11.1% |
| mathematical         |  22 |         0.37 |            4.5% |
| Physics/Astrophysics |   1 |         0.33 |            0.0% |
| biological/chemical  |   1 |         0.33 |            0.0% |
| economic/industrial  |   1 |         0.33 |            0.0% |
| economic/industrial/geopolitical |   1 |         0.33 |            0.0% |
| epistemological      |   1 |         0.33 |            0.0% |
| epistemological/political |   1 |         0.33 |            0.0% |
| linguistic/cognitive |   1 |         0.33 |            0.0% |
| logic/epistemology   |   1 |         0.33 |            0.0% |
| logical/economic     |   1 |         0.33 |            0.0% |
| mathematical/statistical |   1 |         0.33 |            0.0% |
| mathematical/topological |   1 |         0.33 |            0.0% |
| mathematics/topological |   1 |         0.33 |            0.0% |
| physical             |   2 |         0.33 |            0.0% |
| sociological/artistic |   1 |         0.33 |            0.0% |
| technological (mathematics) |   1 |         0.33 |            0.0% |
| mathematical/computational |   3 |         0.28 |            0.0% |
| mathematical / logical |   1 |         0.25 |            0.0% |
| scientific (mathematical physics / quantum mechanics) |   1 |         0.25 |            0.0% |
| statistical/economic/social |   1 |         0.25 |            0.0% |
| technological/physical |   1 |         0.25 |            0.0% |

## Key Findings

1. **Domain variance spread:** bio_industrial shows highest variance (1.00), while technological/physical shows lowest (0.25)

2. **High stability:** 54.8% of constraints are completely stable across index configs

3. **Perspective-dependent constraints:** 10 constraints show strong perspective-dependence

## High Variance Examples

Constraints that change type frequently based on index configuration:

| Constraint ID | Variance | Configs | Types | Domain | Claimed Type |
|---------------|----------|---------|-------|--------|-------------|
| abstraction_boundary_overrun   | 1.00 |       3 |     3 | technological/computational | tangled_rope |
| abstraction_leakage            | 1.00 |       3 |     3 | technological/cognitive | tangled_rope |
| academic_fashion_modernism_2026 | 1.00 |       3 |     3 | social/technological/educational | piton        |
| academic_peer_review_gatekeeping | 1.00 |       3 |     3 | economic/social/technological | tangled_rope |
| academic_tenure_system         | 1.00 |       3 |     3 | economic/social | tangled_rope |
| access_arbitrage               | 1.00 |       2 |     2 | tangled_rope | tangled_rope |
| ad_fus_coordination            | 1.00 |       4 |     4 | medical/neurological | tangled_rope |
| ad_synaptic_deficit            | 1.00 |       3 |     3 | biological/technological | tangled_rope |
| adaptive_lag_trap              | 1.00 |       4 |     4 | economic/technological/regulatory | tangled_rope |
| adversarial_surface_inflation  | 1.00 |       4 |     4 | technological/cybernetic/security | tangled_rope |

### Detailed Examples

**1. abstraction_boundary_overrun**
- Domain: technological/computational
- Variance: 1.00
- Produces 3 different types across 3 index configurations
- Type distribution: {'snare': 1, 'rope': 1, 'tangled_rope': 1}

**2. abstraction_leakage**
- Domain: technological/cognitive
- Variance: 1.00
- Produces 3 different types across 3 index configurations
- Type distribution: {'snare': 1, 'rope': 1, 'tangled_rope': 1}

**3. academic_fashion_modernism_2026**
- Domain: social/technological/educational
- Variance: 1.00
- Produces 3 different types across 3 index configurations
- Type distribution: {'snare': 1, 'rope': 1, 'piton': 1}

## Suspicious Stability

Constraints with many index configs but low variance (possible modeling issues):

| Constraint ID | Configs | Types | Variance | Domain |
|---------------|---------|-------|----------|--------|
| treaty_land_entrenchment       |       5 |     1 |     0.20 | legal/political |

**Note:** These constraints have many perspective configurations but produce the same type. This might indicate:
- The constraint is genuinely invariant (e.g., physical laws)
- Index dimensions are not affecting classification
- Potential data quality issue

## Data Completeness

| Field | % Complete | Impact |
|-------|-----------|--------|
| classifications | 100.0% | Core data for variance analysis |
| variance_ratio | 100.0% | Calculated from classifications |
| domain | 100.0% | Affects domain breakdown analysis |
