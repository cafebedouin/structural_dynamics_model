% ============================================================================
% CONSTRAINT STORY: workforce_feminization_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_workforce_feminization_paradox, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: workforce_feminization_paradox
 *   human_readable: Workforce Feminization Paradox in Medicine
 *   domain: health_workforce_economics/organizational_behavior/gender_labor
 *
 * SUMMARY:
 *   The workforce feminization paradox describes a structural bind in
 *   physician workforce economics: as medicine achieves gender parity in
 *   training (55.4% female matriculants as of 2020), differential attrition
 *   rates amplify rather than resolve workforce sustainability challenges.
 *   Women constitute 63.9% of inactive physicians despite representing
 *   approximately 50% of the active workforce, and career length
 *   differentials (women average 4-6 fewer active years) compound as cohort
 *   feminization increases. The constraint exhibits tangled rope structure:
 *   genuine coordination function exists (diversifying the physician
 *   workforce improves cultural competence, patient-physician demographic
 *   matching, and access for underserved populations), but substantial
 *   extraction operates through the interaction of gendered caregiving
 *   expectations, workplace inflexibility, and specialty prestige dynamics.
 *   The paradox is amplified by a vindicated proposition — the pipeline
 *   equity sufficiency doctrine — which holds that achieving gender parity in
 *   medical school admissions is sufficient to resolve workforce gender
 *   equity. This doctrine is empirically vindicated by matriculation data
 *   (equity achieved) but structurally falsified by career trajectory data
 *   (equity not sustained). The constraint's theater ratio (0.48) reflects
 *   the gap between equity narrative (performative celebration of
 *   matriculation parity) and structural reality (unaddressed attrition
 *   differential). Medical education institutions capture reputational gains
 *   from diversity metrics without bearing downstream costs; healthcare
 *   delivery organizations benefit from workforce flexibility and lower
 *   compensation in feminized specialties; female physicians bear extraction
 *   through career truncation and part-time penalty while their professional
 *   identity prevents exit.
 *
 * KEY AGENTS:
 *   - Female Physicians (Biographical Trajectory): Primary victim (powerless/identity_locked) — bears extraction through career truncation, part-time penalty, and specialty devaluation; identity fusion with both professional commitment and caregiving role prevents exit from either
 *   - Physician Workforce Sustainability: Primary victim (powerless/trapped) — abstract collective good that cannot organize; bears compounding attrition as feminization increases without compensating retention mechanisms
 *   - Healthcare Access Continuity: Secondary victim (powerless/trapped) — patient populations experience reduced access as workforce shortage amplifies; no exit option
 *   - Medical Education Institutions: Primary beneficiary (institutional/arbitrage) — captures reputational gains from gender equity metrics without bearing attrition costs; can shift narrative focus when crisis becomes visible
 *   - Healthcare Delivery Organizations: Primary beneficiary (institutional/arbitrage) — benefits from workforce flexibility and lower compensation expectations in feminized specialties
 *   - Gender Equity Narrative: Secondary beneficiary (institutional/arbitrage) — vindicated by matriculation parity while structural extraction remains unaddressed
 *   - Male Physicians in Feminizing Specialties: Mixed position (moderate/constrained) — constrained by specialty prestige erosion but benefit from relative career stability
 *   - Physician Advocacy Organizations: Organized agents (organized/mobile) — see paradox as temporary with policy solutions; sunset logic assumes structural reforms will resolve differential within 15-20 years
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(workforce_feminization_paradox, 0.58).
domain_priors:suppression_score(workforce_feminization_paradox, 0.62).
domain_priors:theater_ratio(workforce_feminization_paradox, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(workforce_feminization_paradox, extractiveness, 0.58).
narrative_ontology:constraint_metric(workforce_feminization_paradox, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(workforce_feminization_paradox, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(workforce_feminization_paradox, tangled_rope).
narrative_ontology:human_readable(workforce_feminization_paradox, "Workforce Feminization Paradox in Medicine").
narrative_ontology:topic_domain(workforce_feminization_paradox, "health_workforce_economics/organizational_behavior/gender_labor").

domain_priors:requires_active_enforcement(workforce_feminization_paradox).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(workforce_feminization_paradox, medical_education_institutions).
narrative_ontology:constraint_beneficiary(workforce_feminization_paradox, healthcare_delivery_organizations).
narrative_ontology:constraint_beneficiary(workforce_feminization_paradox, gender_equity_narrative).
narrative_ontology:constraint_victim(workforce_feminization_paradox, physician_workforce_sustainability).
narrative_ontology:constraint_victim(workforce_feminization_paradox, female_physicians_biographical_trajectory).
narrative_ontology:constraint_victim(workforce_feminization_paradox, healthcare_access_continuity).
narrative_ontology:constraint_vindicates(workforce_feminization_paradox, pipeline_equity_sufficiency_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FEMALE PHYSICIAN BIOGRAPHICAL TRAJECTORY (SNARE) — Identity-locked by professional commitment and caregiving role fusion. The constraint extracts through career truncation and part-time penalty while presenting as resolved equity. Cannot exit medicine without abandoning professional identity; cannot exit caregiving expectations without violating internalized gender norms. Maximum extraction from biographical time horizon — the career arc itself is the extraction site.
constraint_indexing:constraint_classification(workforce_feminization_paradox, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: PHYSICIAN WORKFORCE SUSTAINABILITY (SNARE) — Trapped by demographic mathematics. As feminization increases and female exit rates remain elevated, total attrition compounds. The workforce commons cannot organize or exit the structural bind. Experiences pure extraction — each cohort's higher female proportion amplifies total loss without compensating mechanism.
constraint_indexing:constraint_classification(workforce_feminization_paradox, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: MALE PHYSICIANS IN FEMINIZING SPECIALTIES (TANGLED ROPE) — Constrained by specialty prestige erosion and compensation decline as fields feminize, but also benefit from relative career stability and lower caregiving penalty. Mixed experience: coordination function exists (specialty diversification) but extraction operates through gendered devaluation of feminized work.
constraint_indexing:constraint_classification(workforce_feminization_paradox, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: MEDICAL EDUCATION INSTITUTIONS (ROPE) — Primary beneficiary. Captures reputational gains from gender equity metrics (55.4% female matriculants) without bearing downstream attrition costs. Arbitrage-level exit: can shift narrative focus when workforce crisis becomes visible. Experiences constraint as coordination: training diverse workforce solves legitimate access problem while extraction flows elsewhere.
constraint_indexing:constraint_classification(workforce_feminization_paradox, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: HEALTHCARE DELIVERY ORGANIZATIONS (ROPE) — Benefit from flexible workforce (part-time female physicians fill scheduling gaps) and lower compensation expectations in feminized specialties. Arbitrage exit: can adjust staffing models and specialty mix as attrition patterns shift. Coordination function genuine: workforce flexibility enables service coverage.
constraint_indexing:constraint_classification(workforce_feminization_paradox, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: PHYSICIAN ADVOCACY ORGANIZATIONS (SCAFFOLD) — Organized agents (AMA, specialty societies, women-in-medicine groups) see the paradox as temporary coordination failure with policy solutions: paid parental leave, part-time partnership tracks, childcare subsidies, schedule flexibility. Mobile exit: can shift advocacy focus if structural reforms fail. Sunset logic: policy interventions will resolve the attrition differential within 15-20 years.
constraint_indexing:constraint_classification(workforce_feminization_paradox, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the constraint exhibits both genuine coordination (diversifying physician workforce improves cultural competence and patient-physician matching) and substantial extraction (gendered attrition amplifies workforce shortage while equity narrative obscures the structural bind). The pipeline equity doctrine is vindicated by matriculation rates but falsified by career trajectory data. Analytical classification: tangled rope with high extractiveness.
constraint_indexing:constraint_classification(workforce_feminization_paradox, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(workforce_feminization_paradox_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(workforce_feminization_paradox, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(workforce_feminization_paradox, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(workforce_feminization_paradox, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(workforce_feminization_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial. Female physicians experience career truncation (4-6 fewer active years), part-time penalty (lower compensation and partnership rates), and specialty devaluation as fields feminize. The extraction compounds over time: as cohort feminization increases from 35% (1990) to 55.4% (2020), total workforce attrition amplifies because the higher-attrition subgroup grows. The value reflects that extraction is not total — some female physicians maintain full-time careers, some specialties resist devaluation, and genuine coordination benefits exist — but the structural bind is severe and worsening. Suppression (0.62): Moderate-high. Barriers include workplace inflexibility (limited part-time partnership tracks, inadequate parental leave, schedule rigidity), specialty prestige dynamics (feminized fields lose compensation and status), caregiving role expectations (internalized and externally imposed), and identity lock (professional commitment prevents exit from medicine; gender role internalization prevents exit from caregiving). Suppression has increased over the interval as workforce demands intensified (longer hours, higher patient loads) while caregiving expectations remained stable. Theater ratio (0.48): Moderate. The gap between equity narrative and structural reality is substantial but not total. Medical schools genuinely celebrate diversity and implement pipeline programs, but the performative component is the claim that matriculation parity resolves the equity problem — this obscures the attrition differential and shifts responsibility for retention onto individual women rather than structural reform. The theater has increased as the matriculation equity milestone was achieved (2010s) while career trajectory inequity persisted unaddressed.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how structural position determines classification. Female physicians at biographical time see snare — identity-locked by professional commitment and caregiving role fusion, they experience pure extraction through career truncation while the equity narrative obscures the bind. The workforce commons sees snare from generational time — trapped by demographic mathematics, total attrition compounds as feminization increases. Medical education institutions see rope from immediate time — they coordinate workforce diversity (genuine function) and capture reputational gains without bearing costs. Healthcare delivery organizations see rope from immediate time — workforce flexibility and lower costs in feminized specialties are genuine benefits to them. Physician advocacy organizations see scaffold from generational time — organized agents with policy solutions and sunset logic (15-20 years to resolve through structural reform). The analytical observer sees tangled rope from civilizational time — genuine coordination function (workforce diversity improves care) coexists with substantial extraction (gendered attrition amplifies shortage while equity narrative obscures structural bind). The gap between the beneficiaries' rope and the victims' snare is the extraction gradient. The gap between the advocacy organizations' scaffold and the victims' snare is the difference between those with agency to build exit paths and those trapped by identity and demographics.
 *
 * DIRECTIONALITY LOGIC:
 *   Female physicians (biographical trajectory) are primary victims with identity_locked exit — directionality is high (d ≈ 0.80-0.85) because extraction operates through career truncation and the identity lock prevents exit from either medicine or caregiving role. The engine derives high d from victim status + identity_locked exit, producing high effective extraction. Physician workforce sustainability is a trapped abstract collective — directionality is maximum (d ≈ 0.95) because the commons cannot organize or exit and bears full compounding attrition. Medical education institutions and healthcare delivery organizations are primary beneficiaries with arbitrage exit — directionality is low (d ≈ 0.10-0.15) because they capture gains (reputation, flexibility, lower costs) without bearing attrition costs, and can shift strategy if the crisis becomes reputationally costly. The engine derives low d from beneficiary status + arbitrage exit, producing low or negative effective extraction (net subsidy). Male physicians in feminizing specialties have mixed position — directionality is moderate (d ≈ 0.45-0.50) because they experience both specialty devaluation (extraction) and relative career stability (benefit). Physician advocacy organizations are organized with mobile exit — directionality is moderate-low (d ≈ 0.30-0.35) because they have agency to shift focus and see the constraint as solvable, reducing experienced extraction. The analytical observer sees the full structure — directionality is moderate (d ≈ 0.50) reflecting the tangled rope classification where genuine coordination and substantial extraction coexist.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that the coordination function (diversifying physician workforce) and the extraction mechanism (gendered attrition differential) are structurally distinct and operate simultaneously. The coordination is genuine: patient-physician demographic matching improves outcomes, cultural competence increases with workforce diversity, and access to underserved populations improves when the physician workforce reflects community demographics. The extraction is also genuine: female physicians bear career truncation and part-time penalty, the workforce commons bears compounding attrition, and the equity narrative obscures the structural bind while beneficiaries capture gains. The tangled rope classification captures this duality. The constraint is NOT pure coordination (rope) because identifiable victims exist and extraction is substantial. The constraint is NOT pure extraction (snare) because the coordination function is real and some agents genuinely benefit from the diversity itself rather than from the extraction. The mandatrophy is resolved by recognizing that 'workforce feminization' names two structurally distinct phenomena: (1) increasing gender diversity in training, which solves a legitimate coordination problem, and (2) differential attrition interacting with that diversity, which creates an extraction mechanism. The analytical observer's tangled rope classification reflects this structural duality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attrition_rate_convergence,
    'Will female physician attrition rates converge with male rates as cohort effects mature, or does the differential reflect stable structural features of gendered caregiving allocation?',
    'Longitudinal cohort analysis tracking 2010-2025 matriculants through 20-year career arcs; comparison of attrition rates across cohorts with varying policy environments (paid leave, part-time tracks)',
    'If convergence: scaffold perspective confirmed — policy interventions resolve the paradox. If stable differential: snare perspective confirmed — extraction is structural, not transitional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attrition_rate_convergence, empirical, 'Whether female attrition differential is cohort effect or structural feature').

omega_variable(
    specialty_feminization_causality,
    'Does specialty prestige decline because fields feminize, or do fields feminize because prestige has already declined for other reasons?',
    'Time-lagged correlation analysis: prestige rankings vs gender composition over 30-year window; identification of specialties that feminized without prestige loss (counterexamples)',
    'If feminization causes decline: extraction mechanism confirmed — gendered devaluation is active. If decline precedes feminization: correlation is spurious — other factors drive both.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(specialty_feminization_causality, empirical, 'Causal direction of specialty feminization and prestige decline').

omega_variable(
    part_time_penalty_magnitude,
    'What proportion of female physician attrition is voluntary preference for part-time work vs involuntary exit due to incompatible full-time expectations?',
    'Survey data on exit reasons; comparison of part-time availability across specialties and practice settings; analysis of return-to-practice rates after career interruption',
    'If mostly voluntary: coordination function is larger (flexibility is genuine benefit). If mostly involuntary: extraction is larger (part-time is forced adaptation to incompatible structure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(part_time_penalty_magnitude, empirical, 'Voluntary vs involuntary nature of part-time physician work').

omega_variable(
    pipeline_equity_sufficiency,
    'Is achieving gender parity in medical school admissions sufficient to achieve workforce equity, or does the attrition differential require additional structural intervention?',
    'Workforce projection modeling: compare 2040 physician demographics under current attrition rates vs hypothetical equalized attrition; policy experiment tracking workforce outcomes in systems with comprehensive retention interventions',
    'If sufficient: the paradox is self-resolving through cohort replacement. If insufficient: pipeline equity narrative is false summit — naturalizes attrition as inevitable rather than constructed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pipeline_equity_sufficiency, conceptual, 'Whether pipeline equity alone resolves workforce gender equity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(workforce_feminization_paradox, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wfp_theater_1990, workforce_feminization_paradox, theater_ratio, 0, 0.25).
narrative_ontology:measurement(wfp_theater_2000, workforce_feminization_paradox, theater_ratio, 10, 0.32).
narrative_ontology:measurement(wfp_theater_2010, workforce_feminization_paradox, theater_ratio, 20, 0.41).
narrative_ontology:measurement(wfp_theater_2020, workforce_feminization_paradox, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(wfp_extract_1990, workforce_feminization_paradox, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(wfp_extract_2000, workforce_feminization_paradox, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(wfp_extract_2010, workforce_feminization_paradox, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(wfp_extract_2020, workforce_feminization_paradox, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(wfp_suppress_1990, workforce_feminization_paradox, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(wfp_suppress_2000, workforce_feminization_paradox, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(wfp_suppress_2010, workforce_feminization_paradox, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(wfp_suppress_2020, workforce_feminization_paradox, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(workforce_feminization_paradox, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is downstream of gendered_caregiving_penalty (the broader structural constraint of gendered caregiving allocation in labor markets). The feminization paradox is the specific instantiation of that penalty within physician workforce economics, where the interaction of high training investment, identity lock, and demographic shift amplifies the extraction mechanism. The upstream constraint has its own extractiveness reflecting the general labor market penalty; this constraint has its own extractiveness reflecting the physician-specific amplification through career length differential and specialty devaluation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
