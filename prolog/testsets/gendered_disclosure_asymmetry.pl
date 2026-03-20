% ============================================================================
% CONSTRAINT STORY: gendered_disclosure_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_disclosure_asymmetry, []).

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
 *   constraint_id: gendered_disclosure_asymmetry
 *   human_readable: Gendered Disclosure Asymmetry in Relational Architecture
 *   domain: social_psychology/gender_studies/relational_architecture
 *
 * SUMMARY:
 *   The gendered disclosure asymmetry describes a coordination mechanism in
 *   which men and women employ structurally different strategies to protect
 *   interiority within relational contexts. Men protect through silence of
 *   both inner and outer life — withholding disclosure of both emotional
 *   states and biographical details. Women protect through performative
 *   feeling — sharing emotional content that satisfies relational
 *   expectations while masking actual interiority. Both strategies solve the
 *   same coordination problem: how to navigate relational demands without
 *   exposing core self. The asymmetry is not extraction of one gender by
 *   another but parallel adaptations to patriarchal relational architecture.
 *   The constraint exhibits low extraction (0.18) because both strategies are
 *   functional, though with moderate overhead. Theater ratio (0.48) reflects
 *   that performative feeling involves more theatrical content than silence,
 *   but both strategies contain performative elements. The constraint is
 *   declining in extractiveness over the 30-year interval as alternative
 *   disclosure strategies emerge, supporting the scaffold perspective from
 *   organized agents.
 *
 * KEY AGENTS:
 *   - Men Protecting Interiority: Beneficiary (moderate/constrained) — silence strategy coordinates relational expectations while protecting vulnerability; constrained by masculine norms but not trapped
 *   - Women Protecting Interiority: Beneficiary (moderate/constrained) — performative feeling strategy coordinates emotional labor expectations while protecting core self; constrained by feminine norms but not trapped
 *   - Relational Coordination Function: Beneficiary (institutional/mobile) — the asymmetry enables predictable relational roles that reduce negotiation costs in mixed-gender contexts
 *   - Gender-Nonconforming Individuals: Victim (moderate/constrained) — binary disclosure strategies create illegibility for those outside the gender binary; must adopt ill-fitting performative strategies
 *   - Feminist and Men's Liberation Movements: Organized agents (organized/mobile) — building alternative disclosure pathways through vulnerability literacy and emotional granularity; see constraint as temporary with sunset
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees asymmetry as low-extraction coordination mechanism with moderate overhead and declining trajectory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_disclosure_asymmetry, 0.18).
domain_priors:suppression_score(gendered_disclosure_asymmetry, 0.32).
domain_priors:theater_ratio(gendered_disclosure_asymmetry, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_disclosure_asymmetry, extractiveness, 0.18).
narrative_ontology:constraint_metric(gendered_disclosure_asymmetry, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(gendered_disclosure_asymmetry, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_disclosure_asymmetry, rope).
narrative_ontology:human_readable(gendered_disclosure_asymmetry, "Gendered Disclosure Asymmetry in Relational Architecture").
narrative_ontology:topic_domain(gendered_disclosure_asymmetry, "social_psychology/gender_studies/relational_architecture").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_disclosure_asymmetry, men_protecting_interiority).
narrative_ontology:constraint_beneficiary(gendered_disclosure_asymmetry, women_protecting_interiority).
narrative_ontology:constraint_beneficiary(gendered_disclosure_asymmetry, relational_coordination_function).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MEN PROTECTING INTERIORITY (ROPE) — Silence of both inner and outer life is a coordination mechanism that protects vulnerability while maintaining social legibility. Constrained by gender norms but not trapped — alternative masculinities exist at biographical timescales. Low extraction: the constraint solves a genuine coordination problem (how to navigate relational demands without exposing core self) with moderate overhead.
constraint_indexing:constraint_classification(gendered_disclosure_asymmetry, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: WOMEN PROTECTING INTERIORITY (ROPE) — Performative feeling that masks actual interiority is a coordination mechanism that satisfies relational expectations while protecting core self. Constrained by gender norms but not trapped — alternative femininities exist at biographical timescales. Low extraction: the constraint solves a genuine coordination problem (how to meet emotional labor demands without full exposure) with moderate overhead.
constraint_indexing:constraint_classification(gendered_disclosure_asymmetry, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RELATIONAL COORDINATION FUNCTION (ROPE) — The asymmetry enables predictable relational roles that reduce negotiation costs in mixed-gender contexts. Both strategies protect interiority while maintaining social coordination. Low extraction from institutional perspective: the constraint coordinates expectations efficiently, though with some performative overhead.
constraint_indexing:constraint_classification(gendered_disclosure_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: GENDER-NONCONFORMING INDIVIDUALS (TANGLED ROPE) — The binary disclosure strategies create coordination benefits (legible social scripts) but also extract from those who don't fit either pattern. Must choose a performative strategy that doesn't match their actual relationship to interiority, or face social illegibility. Moderate extraction: genuine coordination function exists but asymmetric costs fall on those outside the binary.
constraint_indexing:constraint_classification(gendered_disclosure_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: FEMINIST AND MEN'S LIBERATION MOVEMENTS (SCAFFOLD) — Organized agents see the asymmetry as a temporary coordination mechanism with a sunset: as gender norms evolve, alternative disclosure strategies are emerging that don't require either silence or performative feeling. Vulnerability literacy and emotional granularity are building pathways beyond the binary. Low extraction with declining trajectory: the constraint is being actively dismantled through norm change.
constraint_indexing:constraint_classification(gendered_disclosure_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, the asymmetry is a low-extraction coordination mechanism that emerged to solve the problem of protecting interiority within patriarchal relational structures. Both strategies are functional adaptations with moderate overhead. The constraint coordinates disclosure expectations efficiently, though at the cost of reinforcing gender binaries and limiting expressive range.
constraint_indexing:constraint_classification(gendered_disclosure_asymmetry, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_disclosure_asymmetry_tests).
:- end_tests(gendered_disclosure_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. Both disclosure strategies solve genuine coordination problems with moderate overhead. The asymmetry is not extraction of one gender by another but parallel adaptations. Men's silence protects interiority at the cost of relational distance; women's performative feeling protects interiority at the cost of emotional labor. Neither strategy is purely extractive — both enable relational coordination while preserving core self. The declining trajectory (0.22 → 0.18) reflects emergence of alternative strategies that reduce overhead. Suppression (0.32): Moderate-low. Gender norms constrain disclosure strategies but do not trap individuals — alternative masculinities and femininities exist and are increasingly accessible at biographical timescales. Exit options are constrained rather than trapped. Theater ratio (0.48): Moderate. Performative feeling involves substantial theatrical content (sharing emotions that satisfy expectations while masking actual interiority), but silence also contains performative elements (strategic withholding is itself a performance). The ratio reflects that both strategies involve performance, with performative feeling having higher theatrical content. The increasing trajectory (0.35 → 0.48) reflects that as gender norms become more contested, the performative aspects of both strategies become more visible.
 *
 * PERSPECTIVAL GAP:
 *   The constraint classifies as rope from most perspectives because it solves a genuine coordination problem with low extraction. Men and women both benefit from their respective strategies, which protect interiority while maintaining social legibility. The relational coordination function benefits from predictable disclosure patterns. The analytical observer sees low-extraction coordination with moderate overhead. The perspectival gap emerges at the gender-nonconforming perspective, which sees tangled_rope: the binary strategies create coordination benefits but extract from those outside the binary through illegibility costs. The scaffold perspective from organized agents reflects that alternative disclosure strategies are emerging with a real sunset — vulnerability literacy and emotional granularity are building pathways beyond the binary. The constraint's low extractiveness and declining trajectory support the rope classification, with the tangled_rope perspective revealing asymmetric costs for a specific subpopulation.
 *
 * DIRECTIONALITY LOGIC:
 *   Both men and women are beneficiaries of their respective strategies — each strategy solves a genuine coordination problem (protecting interiority while maintaining relational legibility) with moderate overhead. The constraint does not extract from one gender to benefit another; it coordinates expectations in a way that enables both to protect core self. Gender-nonconforming individuals are victims because the binary strategies create illegibility for those who don't fit either pattern — they must adopt performative strategies that don't match their actual relationship to interiority, or face social costs. The relational coordination function is a beneficiary because the asymmetry reduces negotiation costs in mixed-gender contexts — predictable disclosure patterns enable efficient relational coordination. Organized agents (feminist and men's liberation movements) see the constraint as temporary because they are actively building alternative pathways that don't require either silence or performative feeling.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that gendered disclosure asymmetry is primarily a coordination mechanism (rope) rather than extraction. The key insight is that both strategies protect interiority — they are parallel adaptations to the same relational architecture, not extraction of one gender by another. The moderate overhead (theater ratio 0.48, suppression 0.32) reflects genuine coordination costs, not extractive rent-seeking. The tangled_rope classification from the gender-nonconforming perspective reveals that the constraint does have extractive elements for those outside the binary, but this is asymmetric extraction from a subpopulation rather than systemic extraction. The scaffold perspective from organized agents shows that the constraint has a real sunset — alternative strategies are emerging that reduce overhead. The declining extractiveness trajectory (0.22 → 0.18) supports this. The constraint is not a naturalized mountain (disclosure asymmetry is not biologically determined) nor a snare (neither gender is trapped by the other's strategy). It is a low-extraction coordination mechanism with a declining trajectory and asymmetric costs for a specific subpopulation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interiority_protection_effectiveness,
    'Do the gendered disclosure strategies actually protect interiority, or do they create different forms of exposure?',
    'Longitudinal psychological studies measuring self-concept stability, boundary violations, and experienced authenticity across disclosure strategies; qualitative analysis of what gets protected vs exposed under each strategy',
    'If protective: rope classification confirmed — genuine coordination function. If exposing: strategies may be theater masking extraction, raising extractiveness and shifting toward tangled_rope or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interiority_protection_effectiveness, empirical, 'Whether disclosure strategies effectively protect interiority').

omega_variable(
    performative_feeling_labor_cost,
    'What is the metabolic and psychological cost of sustained performative feeling compared to sustained silence?',
    'Comparative studies of emotional labor costs: cortisol levels, cognitive load, burnout rates, and long-term psychological outcomes for individuals employing each strategy',
    'If costs are symmetric: rope classification holds. If performative feeling has significantly higher costs: extractiveness increases, potentially shifting women''s perspective toward tangled_rope or snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(performative_feeling_labor_cost, empirical, 'Comparative metabolic cost of performative feeling vs silence').

omega_variable(
    alternative_strategy_viability,
    'Are alternative disclosure strategies (vulnerability literacy, emotional granularity beyond the binary) actually viable at scale, or do they require social contexts that don''t yet exist?',
    'Ethnographic studies of communities attempting alternative disclosure norms; measurement of coordination costs and relational stability in experimental contexts; tracking of norm diffusion rates',
    'If viable: scaffold perspective confirmed — sunset is real. If not viable: scaffold is aspirational, and the constraint may be more durable (rope from more perspectives, or mountain if alternatives are structurally impossible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_strategy_viability, empirical, 'Viability of alternative disclosure strategies at scale').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_disclosure_asymmetry, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gda_tr_t0, gendered_disclosure_asymmetry, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gda_tr_t15, gendered_disclosure_asymmetry, theater_ratio, 15, 0.42).
narrative_ontology:measurement(gda_tr_t30, gendered_disclosure_asymmetry, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(gda_be_t0, gendered_disclosure_asymmetry, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(gda_be_t15, gendered_disclosure_asymmetry, base_extractiveness, 15, 0.2).
narrative_ontology:measurement(gda_be_t30, gendered_disclosure_asymmetry, base_extractiveness, 30, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_disclosure_asymmetry, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is part of a broader family of gendered relational norms. Related constraints include emotional_labor_asymmetry (women perform disproportionate emotional labor in mixed-gender contexts), masculine_stoicism_norm (men face social penalties for emotional disclosure), and relational_legibility_requirements (social contexts require gender-conforming disclosure patterns for legibility). Each has its own epsilon value and should be modeled as a separate story if analyzed in detail.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
