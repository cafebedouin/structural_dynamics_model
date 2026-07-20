% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__degrowth_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_harm_prevention__degrowth_reading
 *   human_readable: Global North Degrowth Imperative for Climate Harm Prevention
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint instantiates the degrowth reading of the contested
 *   climate_harm_prevention kernel: the claim that legitimate climate
 *   response requires planned economic contraction in the Global North
 *   because mitigation compatible with continued growth is physically or
 *   politically impossible. It is one of three sibling readings, alongside
 *   mitigation_priority (technological transition within growth) and
 *   adaptation_priority (resilience-building accepting higher warming). The
 *   constraint structurally coordinates global mitigation by enforcing
 *   biophysical limits while asymmetrically extracting from present Global
 *   North consumption for the benefit of the Global South and future
 *   generations. It is claimed as a necessary coordination mechanism grounded
 *   in planetary boundaries; the authored metrics describe a highly
 *   extractive, actively enforced arrangement that meets substantial
 *   resistance.
 *
 * KEY AGENTS:
 *   - global_south: Primary beneficiary (organized/constrained) â gains climate stability and development space
 *   - future_generations: Primary beneficiary (powerless/trapped) â gains preserved ecological capacity, no present voice
 *   - global_north_consumers: Primary target (moderate/constrained) â bears costs of planned contraction
 *   - international_climate_regime: Agenda setter (institutional/constrained) â administers and enforces the contraction framework
 *   - green_growth_advocates: Excluded voice (moderate/constrained) â argues for growth-compatible alternatives, ruled illegitimate
 *   - climate_economists: Analytical observer (analytical) â assesses physical and economic necessity of contraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, 0.72).
domain_priors:suppression_score(climate_harm_prevention__degrowth_reading, 0.68).
domain_priors:theater_ratio(climate_harm_prevention__degrowth_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__degrowth_reading, "Global North Degrowth Imperative for Climate Harm Prevention").
narrative_ontology:topic_domain(climate_harm_prevention__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__degrowth_reading, 'c0375ed4-270c-426d-bf35-e1e40596f716').
narrative_ontology:cs_kernel_codification('c0375ed4-270c-426d-bf35-e1e40596f716', distributed).
narrative_ontology:cs_authority_grounding('c0375ed4-270c-426d-bf35-e1e40596f716', expertise).
narrative_ontology:cs_interpretation_layer_present('c0375ed4-270c-426d-bf35-e1e40596f716').
narrative_ontology:cs_reading_relation('c0375ed4-270c-426d-bf35-e1e40596f716', climate_harm_prevention__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('c0375ed4-270c-426d-bf35-e1e40596f716', climate_harm_prevention__adaptation_priority, influences).
narrative_ontology:cs_axiom('c0375ed4-270c-426d-bf35-e1e40596f716', foundational, planned_contraction_imperative).
narrative_ontology:cs_axiom_status(planned_contraction_imperative, holdable).
narrative_ontology:cs_axiom_grounding('c0375ed4-270c-426d-bf35-e1e40596f716', planned_contraction_imperative, empirically_contingent).
narrative_ontology:cs_axiom('c0375ed4-270c-426d-bf35-e1e40596f716', foundational, global_north_responsibility_differential).
narrative_ontology:cs_axiom_status(global_north_responsibility_differential, holdable).
narrative_ontology:cs_axiom_grounding('c0375ed4-270c-426d-bf35-e1e40596f716', global_north_responsibility_differential, deontological).
narrative_ontology:cs_reference_frame('c0375ed4-270c-426d-bf35-e1e40596f716', planetary_boundary_steady_state).
narrative_ontology:cs_drift_state('c0375ed4-270c-426d-bf35-e1e40596f716', high_carbon_growth_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('c0375ed4-270c-426d-bf35-e1e40596f716', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__degrowth_reading, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, global_south).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, global_north_consumers).
narrative_ontology:constraint_vindicates(climate_harm_prevention__degrowth_reading, planetary_boundary_theory).
narrative_ontology:constraint_vindicates(climate_harm_prevention__degrowth_reading, climate_justice_differential_responsibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from reduced climate harm and a rebalanced global resource footprint as Global North contraction lowers emissions and extraction pressures. Seeks development space and climate finance but cannot unilaterally enforce the constraint; depends on Northern compliance with contraction mandates.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_south, beneficiary,
    organized, generational, constrained, global).

% Receives climate stability and preserved ecological capacity at the cost of others' present consumption. Have no present voice, vote, or exit from the atmospheric and institutional choices made by current generations.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Bear the direct costs of planned economic contraction through reduced consumption, deferred infrastructure, and foregone growth. Politically diffuse and individually constrained; aggregate resistance is high but exit from national contraction policies is structurally limited.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_north_consumers, payer,
    moderate, biographical, constrained, global).

% Administers the legitimate response framework through conditional finance, technology transfer rules, and compliance mechanisms that enforce or incentivize Global North contraction. Constrained by nation-state sovereignty and competing growth-oriented agendas.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, international_climate_regime, agenda_setter,
    institutional, generational, constrained, global).

% Argue that technological innovation and efficiency can achieve mitigation within a growth framework. Structurally excluded from legitimacy by this reading's core premise that growth-compatible mitigation is physically or politically impossible.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, green_growth_advocates, excluded,
    moderate, biographical, constrained, global).

% Analyze carbon budgets, planetary boundaries, and macroeconomic pathways to assess whether contraction is physically necessary or politically contingent. Their models inform but do not determine the constraint's normative claims.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, climate_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preventing catastrophic climate harm by reducing Global North emissions and resource consumption to levels compatible with planetary boundaries, coordinating global mitigation action around biophysical limits rather than market preferences.
% TRANSFER_FUNCTION: Moves present consumption, economic output, and developmental flexibility from Global North consumers to Global South populations and future generations, via planned contraction enforced through international policy architecture.
% ABSENT_VOICES: Green growth advocates and technological optimists who argue for emissions reduction within a growth framework; present-generation Global North populations who would prefer consumption preservation; fossil fuel and high-carbon industry interests excluded from the legitimacy framework by the contraction imperative.
% DISAPPEARANCE_RATIONALE: If the degrowth imperative vanished overnight, global climate policy would reorganize around growth-compatible technological transition and efficiency frameworks. The distribution of costs would shift away from present Global North consumption, the political legitimacy of contraction would collapse, and the international climate regime would revert to market-based and innovation-centric mitigation pathways.
% FOUNDING_PROBLEM: Unlimited economic growth in the Global North drives greenhouse gas emissions and resource extraction that exceed planetary boundaries, destabilizing the climate system and disproportionately harming the Global South and future generations.
% FOUNDING_PROBLEM_CORROBORATION: IPCC physical science working group and ecological economics literature attest to emissions overshoot and planetary boundary transgression. However, the specific claim that mitigation within a growth framework is impossibleârather than merely politically difficultâis contested by mainstream growth-oriented economists and policy institutions outside the beneficiary set.
narrative_ontology:disappearance_verdict(climate_harm_prevention__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__degrowth_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_harm_prevention__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__degrowth_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.72) is high because planned contraction demands substantial and sustained reduction in Global North consumption and output. Suppression (0.68) reflects the active exclusion of growth-centric alternatives from legitimate policy space. Theater ratio (0.30) is moderate: much degrowth advocacy is substantive and costly to its proponents, but international climate diplomacy contains performative elements where contraction demands exceed implemented policy. Accessibility collapse (0.60) captures the partial closure of green growth alternatives under this reading, though technological optimism persists in practice. Resistance (0.78) is very high due to concentrated political opposition in the Global North. The measurement series share a single time grid (0â50) to prevent misaligned drift detection.
 *
 * PERSPECTIVAL GAP:
 *   The agenda setter and beneficiary seats experience this constraint as necessary coordination around irreducible biophysical limits; the payer seat experiences it as enforced extraction of present welfare for diffuse future and distant benefit. The engine computes this divergence from structural data: global_south and future_generations derive low directionality as beneficiaries, while global_north_consumers derive high directionality as victims with constrained exit. The claimed type (tangled_rope) and metrics are authored independently; if the engine computes a higher extraction profile from the Global North seat, that divergence is the intended measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (global_south, future_generations) receive climate stability and ecological preservation; their directionality is toward the beneficiary end. Victims (global_north_consumers) pay through foregone consumption and economic contraction with constrained exit; their directionality is toward the target end. The agenda setter (international_climate_regime) is structurally intermediate: it enforces without directly collecting the extracted surplus, deriving moderate directionality. Green_growth_advocates are excluded from legitimacy, receiving maximal directionality as structurally suppressed targets of the constraint's boundary maintenance.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling this arrangement as either pure coordination (rope) or pure extraction (snare). It captures both the genuine coordination functionâpreventing catastrophic climate harm through collective action on planetary boundariesâand the asymmetric extraction inherent in assigning contraction costs to one population for the benefit of others. A snare classification would require the coordination story to be cover; the authored metrics and founding problem status (live) support that the coordination function is genuine, even as extraction is high. A scaffold classification would require a sunset clause; degrowth as steady-state has no declared sunset.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the degrowth reading of climate harm prevention a necessary implication of physical constraints, or one contested political position among several live alternatives?',
    'Comparative assessment of integrated assessment models and planetary boundary analyses against historical emissions data; evaluate whether green growth pathways are physically excluded or merely politically obstructed.',
    'If physical exclusion is established, the constraint edges toward mountain-like necessity; if political contingency is established, it remains a tangled_rope or potentially reclassifies depending on beneficiary capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Physical necessity vs political contingency of degrowth reading within kernel').

omega_variable(
    growth_mitigation_physical_impossibility,
    'Is absolute decoupling of emissions from economic growth physically impossible at the speed and scale required, or merely improbable under current institutions?',
    'Meta-analysis of decoupling trends, resource throughput accounts, and integrated assessment model sensitivity to growth assumptions; natural experiments from jurisdictions attempting rapid green transition.',
    'If decoupling is physically possible, the degrowth reading''s core premise is falsified and extraction is less justified; if impossible, the coordination function strengthens relative to the extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_mitigation_physical_impossibility, empirical, 'Whether growth-compatible mitigation violates physical limits').

omega_variable(
    coordination_extraction_boundary,
    'Does the degrowth constraint genuinely coordinate around biophysical limits, or does it use climate physics as a cover for pre-existing redistribution aims?',
    'Counterfactual analysis of advocacy coalitions: would the same actors and institutions demand Global North contraction if green growth were physically viable? Review of historical demands from climate justice movements for consistency across scenarios.',
    'If the coordination story is cover for redistribution, the constraint reclassifies toward snare; if the coordination function is primary and independently motivated, it remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the constraint''s coordination function is separable from its extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__degrowth_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_harm_prevention__degrowth_reading_tr_t0, climate_harm_prevention__degrowth_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(climate_harm_prevention__degrowth_reading_tr_t10, climate_harm_prevention__degrowth_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(climate_harm_prevention__degrowth_reading_tr_t20, climate_harm_prevention__degrowth_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(climate_harm_prevention__degrowth_reading_tr_t30, climate_harm_prevention__degrowth_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(climate_harm_prevention__degrowth_reading_tr_t40, climate_harm_prevention__degrowth_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(climate_harm_prevention__degrowth_reading_tr_t50, climate_harm_prevention__degrowth_reading, theater_ratio, 50, 0.3).

% Extraction over time
narrative_ontology:measurement(climate_harm_prevention__degrowth_reading_be_t0, climate_harm_prevention__degrowth_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(climate_harm_prevention__degrowth_reading_be_t10, climate_harm_prevention__degrowth_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(climate_harm_prevention__degrowth_reading_be_t20, climate_harm_prevention__degrowth_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(climate_harm_prevention__degrowth_reading_be_t30, climate_harm_prevention__degrowth_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(climate_harm_prevention__degrowth_reading_be_t40, climate_harm_prevention__degrowth_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(climate_harm_prevention__degrowth_reading_be_t50, climate_harm_prevention__degrowth_reading, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(climate_harm_prevention__degrowth_reading_su_t0, climate_harm_prevention__degrowth_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(climate_harm_prevention__degrowth_reading_su_t10, climate_harm_prevention__degrowth_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(climate_harm_prevention__degrowth_reading_su_t20, climate_harm_prevention__degrowth_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(climate_harm_prevention__degrowth_reading_su_t30, climate_harm_prevention__degrowth_reading, suppression_requirement, 30, 0.64).
narrative_ontology:measurement(climate_harm_prevention__degrowth_reading_su_t40, climate_harm_prevention__degrowth_reading, suppression_requirement, 40, 0.67).
narrative_ontology:measurement(climate_harm_prevention__degrowth_reading_su_t50, climate_harm_prevention__degrowth_reading, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__degrowth_reading, global_infrastructure).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, adaptation_priority).

% DUAL FORMULATION NOTE:
% This constraint is the degrowth reading of the climate_harm_prevention kernel, decomposed from the colloquial label 'climate response' per the epsilon-invariance principle. Sibling readings include mitigation_priority (growth-compatible technological transition) and adaptation_priority (resilience prioritization accepting higher warming). Each reading carries a distinct epsilon, beneficiary/victim structure, and type classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
