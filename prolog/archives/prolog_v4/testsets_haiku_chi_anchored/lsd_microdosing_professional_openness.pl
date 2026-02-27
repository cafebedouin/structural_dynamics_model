% ============================================================================
% CONSTRAINT STORY: lsd_microdosing_professional_openness
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lsd_microdosing_professional_openness, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: lsd_microdosing_professional_openness
 *   human_readable: The Permanent Openness Shift from Microdosing
 *   domain: psychological/professional/biochemical
 *
 * SUMMARY:
 *   The normalization of LSD microdosing in creative and innovation sectors
 *   creates a structural tension between genuine cognitive benefits
 *   (increased idea generation, reduced conformity pressure, enhanced
 *   cross-domain association) and the emergence of pharmacological coercion:
 *   as the practice diffuses from experimental adopters to professional norm,
 *   workers face implicit career penalties for non-adoption, particularly in
 *   fields where openness-to-experience is valued (product design, software
 *   architecture, strategy consulting). The constraint exhibits mixed
 *   coordination and extraction properties. From the employer's perspective,
 *   microdosing solves a real collective action problem: how to increase
 *   novelty and reduce groupthink in ideation without incurring the social
 *   cost of explicit non-conformity. From the risk-averse worker's
 *   perspective, the normalization of openness-shift creates a snare: the
 *   career benefit of non-adoption (conscientiousness, attention to process)
 *   becomes professionally penalized as the market for creative output shifts
 *   its selection criteria. The constraint's extractiveness (0.52) reflects
 *   this asymmetry: the openness-shift is permanent (or appears so over the
 *   5-10 year career horizon) and comes with biochemical and psychological
 *   costs that fall unevenly on workers whose neurobiology doesn't respond to
 *   LSD (estimated 15-20% of population) or who prefer not to use it. The
 *   suppression (0.68) reflects barriers to exit: workers cannot safely
 *   refuse without signaling low creative potential or openness; employers
 *   cannot regulate use without invading medical privacy or creating perverse
 *   incentives for undisclosed use. Theater ratio (0.58) reflects that much
 *   of the discourse around microdosing in professional contexts is
 *   aspirational: claims about cognitive enhancement often outpace clinical
 *   evidence; success stories are highlighted while failure cases remain
 *   private; employers market 'cognitive diversity' while systematically
 *   selecting for openness phenotypes.
 *
 * KEY AGENTS:
 *   - Creative Professionals: Beneficiary + arbitrage (institutional) — capture early-adopter advantage, access to innovation networks, professional status elevation in high-openness sectors
 *   - Risk-Averse Workers: Primary victim + trapped (powerless) — face implicit career penalties for non-adoption; cannot exit without professional reset; neurobiology may not respond to LSD
 *   - Tech/Creative Sector Employers: Primary beneficiary + arbitrage (institutional) — solve coordination problem of maintaining creative output; reduce groupthink without explicit non-conformity cost
 *   - Neurodiversity Advocacy Coalition: Secondary victim/beneficiary + constrained (organized) — benefit from openness-shift reducing conformity pressure; harmed by normalization creating implicit screening for openness phenotype
 *   - Microdosing Practitioners: Mixed (moderate + mobile) — experience both genuine cognitive improvement and biochemical dependence; can exit but face career reset
 *   - Regulatory & Occupational Health Systems: Secondary victim + constrained (institutional) — mandated to monitor long-term neurological outcomes, prevent undisclosed use in safety-critical fields; suppressed by limited enforcement capacity
 *   - Traditional Pharmacological Safety Paradigm: Piton (institutional + arbitrage) — clinical trial framework, informed consent structures, dose-response validation persist through institutional inertia despite limited real-world applicability to self-directed microdosing
 *   - Clinical Research and Cognitive Diversity Protocols: Organized agents (organized + constrained) — building alternative pathways through formal studies, occupational licensing restrictions, neurodiversity-informed workplace design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lsd_microdosing_professional_openness, 0.52).
domain_priors:suppression_score(lsd_microdosing_professional_openness, 0.68).
domain_priors:theater_ratio(lsd_microdosing_professional_openness, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lsd_microdosing_professional_openness, extractiveness, 0.52).
narrative_ontology:constraint_metric(lsd_microdosing_professional_openness, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(lsd_microdosing_professional_openness, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lsd_microdosing_professional_openness, tangled_rope).
narrative_ontology:human_readable(lsd_microdosing_professional_openness, "The Permanent Openness Shift from Microdosing").
narrative_ontology:topic_domain(lsd_microdosing_professional_openness, "psychological/professional/biochemical").

domain_priors:requires_active_enforcement(lsd_microdosing_professional_openness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lsd_microdosing_professional_openness, creative_professionals).
narrative_ontology:constraint_beneficiary(lsd_microdosing_professional_openness, research_institutions).
narrative_ontology:constraint_beneficiary(lsd_microdosing_professional_openness, pharmaceutical_development_sectors).
narrative_ontology:constraint_victim(lsd_microdosing_professional_openness, risk_averse_workers).
narrative_ontology:constraint_victim(lsd_microdosing_professional_openness, regulatory_enforcement_capacity).
narrative_ontology:constraint_victim(lsd_microdosing_professional_openness, baseline_cognitive_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RISK-AVERSE PROFESSIONAL (SNARE) — Worker whose neurobiology or personality predisposes them to conscientiousness and risk-aversion. As microdosing becomes normalized in creative/innovation sectors, the openness-shift becomes an implicit job requirement for advancement. Cannot exit without career penalty. No alternative pathway that maintains professional status while preserving their baseline cognitive style. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.78.
constraint_indexing:constraint_classification(lsd_microdosing_professional_openness, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CREATIVE/TECH SECTOR EMPLOYERS (ROPE) — Organizations benefit from genuine coordination: microdosing enables collaborative ideation, reduces groupthink, increases psychological safety in brainstorming. Solves the collective action problem of 'how do we maintain creative output without risk of total proposal rejection?' Arbitrage option allows non-adoption (outsource to creative agencies, hire only high-openness candidates). d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.07. Net beneficiary; effective extraction is negative.
constraint_indexing:constraint_classification(lsd_microdosing_professional_openness, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: NEURODIVERSITY ADVOCACY COALITION (TANGLED ROPE) — Organized agents (disability advocates, neurotypical-diversity proponents) experience both coordination and extraction. Coordination: microdosing creates more inclusive ideation environments (reduces conformity pressure). Extraction: the normalization of openness-shift extracts from those who cannot achieve it biochemically or prefer not to. Constrained exit due to institutional embeddedness. d≈0.62, f(d)≈0.78, σ=1.2 → χ≈0.49.
constraint_indexing:constraint_classification(lsd_microdosing_professional_openness, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: MICRODOSING PRACTITIONER (TANGLED ROPE) — Individual who adopts microdosing for professional advantage. Experiences coordination benefit (genuine cognitive improvement, increased idea generation) AND extraction (becoming dependent on pharmacological augmentation for career competitiveness; risk of tolerance/degradation). Mobile exit: can quit, but faces professional reset. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.34. Symmetric costs and benefits.
constraint_indexing:constraint_classification(lsd_microdosing_professional_openness, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY & OCCUPATIONAL HEALTH SYSTEMS (SNARE) — Regulatory agencies (FDA, DEA, occupational health boards) face structural extraction: must monitor for undisclosed microdosing in high-stakes professions (aircraft pilots, surgeons, operators), track long-term neurological outcomes, enforce scheduling, conduct epidemiological surveillance. High suppression: limited enforcement capacity vs. distributed practice; cannot compel employer screening without professional backlash. Constrained exit: mandated to regulate regardless of enforcement feasibility. d≈0.88, f(d)≈1.30, σ=1.1 → χ≈0.75.
constraint_indexing:constraint_classification(lsd_microdosing_professional_openness, snare,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADITIONAL PHARMACOLOGICAL SAFETY PARADIGM (PITON) — Institutional framework (clinical trials, informed consent, dose-response validation) that once enforced pharmaceutical access control. Now largely performative for microdosing: therapeutic validation exists but safety monitoring is theater (self-reported dosing, unverified purity, undisclosed use). Persists through professional inertia and liability architecture despite low functional verification. theater_ratio=0.58 reflects partial degradation — some regulatory structure remains (pharmaceutical industry testing) but individual practitioner compliance is largely unmonitored. d≈0.05, f(d)≈-0.11, σ=1.2 → χ≈-0.06.
constraint_indexing:constraint_classification(lsd_microdosing_professional_openness, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: CLINICAL RESEARCH & COGNITIVE DIVERSITY PROTOCOLS (SCAFFOLD) — Emerging research agenda (clinical microdosing studies, neurodiversity-informed workplace design) creates alternative pathway: formal protocols with sunset clause. As evidence accumulates on long-term neurological outcomes and neuroplasticity effects, either (a) microdosing is formally approved with occupational restrictions (pilot certification, surgeon licensing), or (b) cognitive diversity frameworks replace pharmacological augmentation with organizational design. d≈0.45, f(d)≈0.48, σ=1.2 → χ≈0.30. Low effective extraction because research pathway has structural sunset: either resolves to regulation or is replaced by non-pharmacological alternatives within 15-20 years.
constraint_indexing:constraint_classification(lsd_microdosing_professional_openness, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the psychopharmacological enhancement of personality traits reflects an immutable constraint: cognitive enhancement always carries trade-offs (increased openness at cost of risk-aversion; higher creative fluency at cost of executability). This perspective sees the constraint as inherent to the biochemistry of neurotransmitter systems. However, the structural data (ε=0.52, suppression=0.68, theater=0.58) contradicts the mountain classification — this is a contingent institutional arrangement (career norms, market competition, regulatory capture) not a law of nature. The false summit detector catches naturalization of policy.
constraint_indexing:constraint_classification(lsd_microdosing_professional_openness, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lsd_microdosing_professional_openness_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lsd_microdosing_professional_openness, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lsd_microdosing_professional_openness, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(lsd_microdosing_professional_openness, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(lsd_microdosing_professional_openness, TR),
    TR >= 0.70.

:- end_tests(lsd_microdosing_professional_openness_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The permanent openness-shift extracts from risk-averse workers through career penalties, but is not maximal extraction because: (a) some workers genuinely benefit from cognitive augmentation, (b) market competition rewards both openness and conscientiousness in different sectors, (c) alternative career pathways exist (risk/operations roles, organizational design) though with lower prestige. The value reflects the real career asymmetry while acknowledging that openness-shift is also a genuine productivity gain for some. Suppression (0.68): Moderate-high. Significant barriers to exit/resistance include: occupational norms making non-adoption risky; privacy barriers preventing meaningful occupational screening; physiological non-response in some populations; psychological aversion to pharmaceutical self-modification; regulatory capture where safety guidelines are unenforced in professional contexts. But suppression is not total because alternative career tracks exist and some employers actively recruit for diverse cognitive styles. Theater ratio (0.58): Moderate. Pharmaceutical safety and efficacy discourse is partly theatrical: clinical evidence is mixed (some studies show improvement, others show trade-offs); employer marketing of 'enhanced creativity' is aspirational; success narratives are amplified while failure cases remain hidden; long-term neurological safety is not yet empirically established.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range from snare to rope depending on the observer's structural position. Risk-averse workers see a snare (career penalty for non-adoption, trapped exit). Creative professionals see rope or scaffold (genuine cognitive improvement, path to organizational alternatives). Employers see rope (solves coordination problem of novelty without explicit non-conformity cost). Regulatory systems see snare (cannot enforce meaningful restrictions without incurring compliance costs). The analytical observer risks seeing a mountain (openness-increase is a 'natural law' of psychopharmacology) but the structural data reveals this as a false summit: the constraint is contingent on occupational norms, market competition, and pharmaceutical availability, not on inherent psychobiology. The key perspectival gap is between beneficiaries (who see genuine coordination and productivity gain) and risk-averse victims (who see career coercion despite no change to their actual job performance). The Piton perspective reveals that traditional pharmacological safety frameworks (clinical trials, informed consent) have largely degraded into theater for professional microdosing — they provide regulatory legitimacy but not real-world safety monitoring.
 *
 * DIRECTIONALITY LOGIC:
 *   Creative professionals + arbitrage: d≈0.08, f(d)≈-0.11. Net beneficiary. Risk-averse workers + trapped: d≈0.92, f(d)≈1.38. Maximum extraction. Neurodiversity advocates + constrained: d≈0.62, f(d)≈0.78. Significant extraction but mixed with coordination benefit. Microdosing practitioners + mobile: d≈0.50, f(d)≈0.65. Symmetric costs/benefits. Regulatory systems + constrained: d≈0.88, f(d)≈1.30. High extraction due to unenforceable mandate. Traditional pharma framework + arbitrage: d≈0.05, f(d)≈-0.11. Net beneficiary (maintains institutional legitimacy while doing little real verification). Clinical research coalition + constrained: d≈0.45, f(d)≈0.48. Low effective extraction because research pathway provides exit mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The core tension is whether microdosing represents genuine coordination (solving the real problem of creativity under organizational conformity pressure) or pure extraction (imposing biochemical conformity as a replacement for social conformity). The tangled rope classification resolves this by recognizing both dimensions: (1) COORDINATION FUNCTION: Microdosing genuinely reduces groupthink and increases idea diversity in brainstorming — this solves a real collective action problem that organizations face. (2) ASYMMETRIC EXTRACTION: The career normalization of openness-shift extracts from risk-averse workers, neurodiversity-underrepresented populations, and those whose biochemistry doesn't respond to LSD — this creates a new hierarchy based on pharmacological responsiveness rather than skill. The classification prevents mischaracterization in two directions: (a) SNARE ERROR: Naïve analysis might classify this as pure snare (coercive biochemical conformity), which misses the genuine organizational benefit and underestimates the autonomy of adopters who choose microdosing. (b) ROPE ERROR: Uncritical analysis might classify this as pure rope (benign coordination mechanism), which misses the career coercion and unequal burden distribution. The tangled rope classification acknowledges that microdosing is both socially beneficial (reduces conformity pressure on conformists) AND socially extractive (penalizes non-conformists through different means). Regulatory response: Mandatrophy resolves when occupational licensing (pilot certification, surgeon credentialing) makes the extraction explicit rather than implicit — transparency allows workers to make informed choices about drug adoption vs. career path, converting hidden coercion into transparent trade-off.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    permanent_neuroplasticity_threshold,
    'What dosing regimen and duration produce irreversible increases in baseline openness vs. temporary pharmacological state-dependence?',
    'Long-term neuroimaging studies (fMRI, structural connectivity) tracking personality trait stability after cessation; longitudinal cohorts with 5-10 year follow-up; discontinuation protocols measuring trait reversion',
    'If permanent at standard doses: constraint becomes truly extractive (biological lock-in). If temporary: constraint is coordination mechanism with reversible cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(permanent_neuroplasticity_threshold, empirical, 'Whether microdosing produces permanent personality change or temporary state-dependence').

omega_variable(
    unconscious_selection_for_openness_bias,
    'Does the normalization of microdosing in creative sectors create selection pressure that systematically excludes high-conscientiousness, risk-averse professionals regardless of drug adoption?',
    'Occupational demographic tracking: hiring patterns in tech/creative sectors; career advancement rates for conscientious non-users vs. microdosing users; skill-adjusted compensation analysis',
    'If selection pressure is dominant: constraint is a snare (structural exclusion). If microdosing itself drives openness: constraint is tangled rope (mixed coordination and extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unconscious_selection_for_openness_bias, empirical, 'Whether sector normalization creates selection bias independent of drug effects').

omega_variable(
    creative_quality_degradation_lag,
    'Does the openness-increase from microdosing improve long-term creative output or merely reduce filtering/quality-control mechanisms, with degradation appearing only after 3-5 years?',
    'Portfolio analysis: track creative output quality (blind peer review, market success) for microdosing practitioners over 5-10 year horizon; compare to non-microdosing controls matched on initial creativity; measure idea-to-viable-product conversion rates',
    'If quality improves: microdosing is genuine coordination enhancement. If degradation lags: constraint is extractive (hidden cost shifted to future periods).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(creative_quality_degradation_lag, empirical, 'Whether openness increase translates to sustained creative quality or masks filtering degradation').

omega_variable(
    occupational_safety_boundary_cases,
    'In high-stakes professions (aviation, surgery, military decision-making), what is the empirical relationship between openness-increase and critical error rates under time pressure?',
    'Simulation studies and epidemiological analysis in safety-critical domains; adverse event tracking in regulated professions with undisclosed microdosing; cognitive task performance under deadline stress',
    'If boundary-case failures exist: constraint is snare in safety-critical sectors. If performance improves across the board: constraint is rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(occupational_safety_boundary_cases, empirical, 'Whether openness increase trades away critical risk-aversion in safety-sensitive contexts').

omega_variable(
    regulatory_enforcement_point_collapse,
    'Can regulatory agencies maintain meaningful occupational restrictions (pilot licensing, surgeon certification) on microdosing without producing incentives for undetected use?',
    'Policy simulation: model enforcement costs vs. benefit-driven adoption rates; track undisclosed use prevalence in regulated professions; compare to narcotics enforcement data',
    'If enforcement collapses: suppression is maximal, constraint becomes pure snare. If enforcement is maintainable: constraint is tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_enforcement_point_collapse, conceptual, 'Whether regulatory enforcement of occupational restrictions is structurally feasible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lsd_microdosing_professional_openness, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lsd_micro_tr_t0, lsd_microdosing_professional_openness, theater_ratio, 0, 0.38).
narrative_ontology:measurement(lsd_micro_tr_t5, lsd_microdosing_professional_openness, theater_ratio, 5, 0.48).
narrative_ontology:measurement(lsd_micro_tr_t10, lsd_microdosing_professional_openness, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(lsd_micro_be_t0, lsd_microdosing_professional_openness, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(lsd_micro_be_t5, lsd_microdosing_professional_openness, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(lsd_micro_be_t10, lsd_microdosing_professional_openness, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lsd_microdosing_professional_openness, information_standard).
narrative_ontology:affects_constraint(lsd_microdosing_professional_openness, pharmacological_cognitive_enhancement_arms_race).
narrative_ontology:affects_constraint(lsd_microdosing_professional_openness, neurodiversity_workplace_selection_bias).
narrative_ontology:affects_constraint(lsd_microdosing_professional_openness, occupational_licensing_biochemical_requirement).

% DUAL FORMULATION NOTE:
% The permanent openness shift from microdosing is part of a constraint family: (1) The general pharmacological cognitive enhancement arms race (whether microdosing or other agents) that affects market competition. (2) The specific neurodiversity workplace selection bias that emerges when openness-increase becomes a job requirement. (3) The occupational licensing boundary case (pilots, surgeons) where openness-shift intersects with safety-critical decision-making. Each has different ε values reflecting different levels of empirical contestation and institutional embeddedness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lsd_microdosing_professional_openness, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
