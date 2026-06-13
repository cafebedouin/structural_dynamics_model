% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__expansive_preventive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__expansive_preventive_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article_51_self_defense__expansive_preventive_reading
 *   human_readable: Article 51 Self-Defense: Expansive Preventive Reading
 *   domain: international_law/security_studies/constitutional_interpretation
 *
 * SUMMARY:
 *   This constraint story instantiates the EXPANSIVE PREVENTIVE READING of
 *   Article 51 self-defense — the interpretation that permits militarily
 *   capable states to use force preemptively or preventively against
 *   non-state actors and emerging threats when the acting state determines
 *   necessity without external validation. This is ONE reading of a contested
 *   kernel; the kernel itself (UN Charter Article 51) admits multiple
 *   structurally distinct interpretations. The expansive reading benefits
 *   militarily capable states and defense sectors while imposing costs on
 *   target-region populations, non-capable states, and the multilateral
 *   authority structure. The constraint is claimed as a snare because the
 *   necessity determination is self-judged, exit options for targets are
 *   trapped, and the primary function appears to serve force authorization
 *   rather than coordination — though this claim is itself part of the
 *   contest.
 *
 * KEY AGENTS:
 *   - Militarily capable states: set the doctrine, invoke self-defense unilaterally, bear no enforcement consequences from peers.
 *   - Target-region populations: powerless, experience strikes justified under the doctrine, cannot participate in necessity determination.
 *   - Defense sector: powerful, profit from sustained military procurement driven by ongoing threat narratives and preemptive strike operations.
 *   - Multilateral veto authority (UN Security Council): institutional seat that loses practical enforcement power when capable states act unilaterally.
 *   - Non-capable states: bear sovereignty costs and lose internal control when strikes occur on their territory.
 *   - Narrow-interpretation advocates: excluded from doctrine-setting conversations; their legal objections are noted but do not constrain state practice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, 0.82).
domain_priors:suppression_score(article_51_self_defense__expansive_preventive_reading, 0.78).
domain_priors:theater_ratio(article_51_self_defense__expansive_preventive_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__expansive_preventive_reading, snare).
narrative_ontology:human_readable(article_51_self_defense__expansive_preventive_reading, "Article 51 Self-Defense: Expansive Preventive Reading").
narrative_ontology:topic_domain(article_51_self_defense__expansive_preventive_reading, "international_law/security_studies/constitutional_interpretation").

domain_priors:requires_active_enforcement(article_51_self_defense__expansive_preventive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__expansive_preventive_reading, 'df6a0528-e9ff-433d-aed0-1f3e982cdcd8').
narrative_ontology:cs_kernel_codification('df6a0528-e9ff-433d-aed0-1f3e982cdcd8', fixed_text).
narrative_ontology:cs_authority_grounding('df6a0528-e9ff-433d-aed0-1f3e982cdcd8', extraction).
narrative_ontology:cs_interpretation_layer_present('df6a0528-e9ff-433d-aed0-1f3e982cdcd8').
narrative_ontology:cs_reading_relation('df6a0528-e9ff-433d-aed0-1f3e982cdcd8', article_51_self_defense__narrow_armed_attack_reading, forecloses).
narrative_ontology:cs_reading_relation('df6a0528-e9ff-433d-aed0-1f3e982cdcd8', article_51_self_defense__unable_unwilling_doctrine_reading, coexists_with).
narrative_ontology:cs_axiom('df6a0528-e9ff-433d-aed0-1f3e982cdcd8', foundational, unilateral_necessity_determination_permissible).
narrative_ontology:cs_axiom_status(unilateral_necessity_determination_permissible, holdable).
narrative_ontology:cs_axiom_grounding('df6a0528-e9ff-433d-aed0-1f3e982cdcd8', unilateral_necessity_determination_permissible, deontological).
narrative_ontology:cs_axiom('df6a0528-e9ff-433d-aed0-1f3e982cdcd8', foundational, non_state_actor_preemption_authorized).
narrative_ontology:cs_axiom_status(non_state_actor_preemption_authorized, holdable).
narrative_ontology:cs_axiom_grounding('df6a0528-e9ff-433d-aed0-1f3e982cdcd8', non_state_actor_preemption_authorized, empirically_contingent).
narrative_ontology:cs_reference_frame('df6a0528-e9ff-433d-aed0-1f3e982cdcd8', expansive_self_defense_authority).
narrative_ontology:cs_drift_state('df6a0528-e9ff-433d-aed0-1f3e982cdcd8', contemporary_post_terrorism_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('df6a0528-e9ff-433d-aed0-1f3e982cdcd8', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__expansive_preventive_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, militarily_capable_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, defense_sector_corporations).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, target_region_populations).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, multilateral_veto_authority).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, non_state_actors).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__expansive_preventive_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(article_51_self_defense__expansive_preventive_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__expansive_preventive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__expansive_preventive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at interval end) because the constraint permits unilateral force application based on self-determined necessity, creating asymmetric authority. Capable states extract security advantage, resource access, and regional dominance; targets extract nothing. Suppression is similarly high (0.78) because the doctrine's persistence depends on maintaining the authority to override multilateral checks and excluding actors who would constrain it. Theater ratio is moderate (0.41) and rising: early in the interval, genuine counter-terrorism operations dominated the doctrine's application; over time, the ratio of strikes against nascent groups, allied-territory operations, and resource-driven interventions has grown, indicating performative security framing covering extraction. The measurements are authored on one shared time grid across all three metrics; the rising extractiveness and theater ratio together indicate Goodhart drift — the coordination function (responding to transnational threats) has atrophied relative to the extraction function (unilateral force authority and resource access). Accessibility_collapse is low (0.38) because alternatives exist (narrow interpretation, unable-unwilling doctrine, multilateral authorization) and remain live positions — the expansive reading must actively suppress these alternatives to persist. Resistance is high (0.72) because substantial international opposition exists from scholars, humanitarian organizations, and non-capable states.
 *
 * PERSPECTIVAL GAP:
 *   Militarily capable states and defense strategists inhabit a security-threat frame where the expansive reading is necessary adaptation. International law scholars and humanitarian organizations inhabit a multilateral-order frame where the reading is institutional degradation and cover for extraction. Target-region populations inhabit a coercion frame where the doctrine is experienced as unilateral military authority. These are not different measurements of the same constraint — they are different structural relationships to it. The engine computes these relationships from power, exit_options, beneficiary/victim declarations, and role; the perspectival gap emerges from the structural data, not from measurement ambiguity.
 *
 * DIRECTIONALITY LOGIC:
 *   Militarily capable states are beneficiaries (d near 0.0): they collect unilateral force authority, set the necessity threshold, and face minimal consequences. Defense sector corporations are beneficiaries (d near 0.1): they profit from procurement demand but depend on state authority — their exit is arbitrage (they can serve multiple states). Target-region populations are targets (d near 1.0): they are trapped in jurisdictions where strikes are authorized, powerless to influence necessity determination, and bear casualty costs. Non-capable states are targets (d near 0.8): they lose sovereignty control over their territory and have constrained diplomatic recourse. Multilateral veto authority is a target (d near 0.85): it is bypassed when capable states act unilaterally and loses legitimacy as a result. Non-state actors are targets (d near 1.0): they are designated as threats based on the acting state's unilateral assessment and face death or dispossession. The beneficiary/victim structure is asymmetric: a small number of powerful beneficiaries extract from a large number of powerless and constrained-exit victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (transnational terrorism, non-state actor threats) remains contested — some parties attest it is live and justifies the expansive reading; others attest the reading has metastasized beyond the founding problem's scope. The constraint's operation shows mandatrophy indicators: theater_ratio is rising (performative security framing increasing relative to functional counter-terrorism), base_extractiveness is rising (unilateral force authority expanding to cases farther from imminent threats), and accessibility_collapse is low (alternatives remain available but suppressed rather than foreclosed). The doctrine began as a response to a genuine coordination problem (how to permit defense against non-state threats the narrow reading did not cover) and has evolved into a mechanism for unilateral force authority and resource extraction. The rising extractiveness curve and moderate theater ratio suggest the founding problem's salience has declined relative to the extraction function — classic mandatrophy pattern.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_self_judgment_ambiguity,
    'Is the expansive reading''s permission for unilateral necessity determination a structural feature of the doctrine, or a functional choice by militarily capable states to avoid external constraints?',
    'Comparative institutional analysis: states that have submitted preventive strikes to multilateral review (seeking UNSC authorization even when not legally required) versus states that invoke the doctrine and refuse review. Pattern of behavior and stated rationales would indicate whether self-judgment is intrinsic to the reading or a choice enabled by power asymmetry.',
    'If self-judgment is intrinsic, the doctrine structurally requires a unilateral determination mechanism and cannot be constrained by multilateral oversight without foreclosing it. If it is a choice, the beneficiaries are strategically avoiding oversight, and alternative implementations (permitting multilateral review) are available.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_self_judgment_ambiguity, conceptual, 'Whether necessity self-determination is a structural requirement of the expansive reading or a functional choice by powerful states.').

omega_variable(
    founding_problem_salience_drift,
    'Has the founding problem of transnational terrorism and non-state actor threats remained the primary justification for the doctrine''s application, or has geopolitical interest (resource access, regional hegemony, military-industrial demand) become the driver?',
    'Longitudinal analysis of strike authorizations: correlation between stated threat characteristics (imminence, non-state actor nexus, territorial sanctuary) and actual targeting patterns. Comparison of strike frequency and targets across periods of high and low terrorism threat. Post-strike analysis of whether strikes prevented actual attacks or served broader strategic interests.',
    'If founding problem remains salient, the constraint can claim coordination function (adapting self-defense law to modern threats). If geopolitical interest has become the driver, the constraint functions primarily as extraction — authorization mechanism for unilateral force serving power-consolidation, not threat response.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_salience_drift, empirical, 'Whether the doctrine continues to serve its founding problem or has evolved into a mechanism for geopolitical extraction.').

omega_variable(
    capability_asymmetry_and_interpretation,
    'Would non-capable states and international-law advocates accept the expansive reading if they held militarily dominant power, or is the interpretation itself an expression of power-driven interpretation rather than principle-driven legal reading?',
    'Historical comparison: how states justified force uses before attaining military dominance, versus how they justify force uses after. Hypothetical analysis: if power distributions were inverted, what interpretation would the currently dominant states advocate?',
    'If interpretation tracks power (dominant states advocate expansive readings that benefit them; weak states advocate narrow readings), then the constraint is power-rationalization, not legal principle. If interpretation is principle-driven, we would expect consistency across power positions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capability_asymmetry_and_interpretation, preference, 'Whether the expansive reading reflects legal principle or power-driven interpretation.').

omega_variable(
    multilateral_authority_recovery_path,
    'What would it take for the multilateral authority structure (UN Security Council, international courts) to recover practical enforcement power over self-defense interpretations?',
    'Analysis of scenarios: would a shift in great-power balance (loss of dominance by currently capable states) automatically shift interpretation, or would institutional reforms be required? Would a new treaty constraint on self-defense be enforceable if written by weaker coalition against dominant states?',
    'If multilateral authority is recoverable through institutional reform, the constraint is a choice by dominant states to bypass the system, not an inevitable feature of the international order. If recovery requires power-balance shift, the constraint will persist as long as power asymmetry holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(multilateral_authority_recovery_path, conceptual, 'Whether the multilateral order can be restored without changes to great-power distribution.').

omega_variable(
    kernel_reading_contest_empirical_foundation,
    'This reading is one instantiation of a contested kernel. The contest between the three readings (expansive_preventive, narrow_armed_attack, unable_unwilling) turns on different premises about what Article 51 permits. Is there an empirical fact that would resolve which reading is correct, or is the contest fundamentally conceptual-political?',
    'Examination of negotiation records and travaux préparatoires from the UN Charter''s drafting. If the negotiators explicitly discussed preventive self-defense and rejected it, the narrow reading has historical grounding. If preventive self-defense was contemplated, the expansive reading has historical basis. If the question was never raised (most likely), the contest remains open.',
    'If the contest is resolvable by historical evidence, the evidence becomes the ground for privileging one reading over others. If the contest is fundamentally open (the kernel is genuinely ambiguous), all three readings remain live positions, and power determines which one is implemented in practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_empirical_foundation, empirical, 'Whether the kernel contest is empirically resolvable or permanently open.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__expansive_preventive_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_51_self_defense__expansive_preventive_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(arti_tr_t3, article_51_self_defense__expansive_preventive_reading, theater_ratio, 3, 0.31).
narrative_ontology:measurement(arti_tr_t6, article_51_self_defense__expansive_preventive_reading, theater_ratio, 6, 0.34).
narrative_ontology:measurement(arti_tr_t12, article_51_self_defense__expansive_preventive_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(arti_tr_t18, article_51_self_defense__expansive_preventive_reading, theater_ratio, 18, 0.4).
narrative_ontology:measurement(arti_tr_t24, article_51_self_defense__expansive_preventive_reading, theater_ratio, 24, 0.41).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(arti_be_t3, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 3, 0.71).
narrative_ontology:measurement(arti_be_t6, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 6, 0.75).
narrative_ontology:measurement(arti_be_t12, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 12, 0.79).
narrative_ontology:measurement(arti_be_t18, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 18, 0.81).
narrative_ontology:measurement(arti_be_t24, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 24, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(arti_su_t3, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 3, 0.74).
narrative_ontology:measurement(arti_su_t6, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 6, 0.75).
narrative_ontology:measurement(arti_su_t12, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 12, 0.77).
narrative_ontology:measurement(arti_su_t18, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 18, 0.78).
narrative_ontology:measurement(arti_su_t24, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 24, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__expansive_preventive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_51_self_defense__expansive_preventive_reading, 0.18).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, article_51_self_defense__narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, article_51_self_defense__unable_unwilling_doctrine_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the Article 51 self-defense kernel family, comprising three structurally distinct readings of the same Charter provision. Each reading instantiates a different constraint with its own ε, beneficiary/victim structure, and type. The expansive_preventive_reading permits unilateral force based on self-judged necessity (high extraction, high suppression, snare classification). The narrow_armed_attack_reading restricts force to actual imminent attacks by attributable states (lower extraction, coordination-heavy, rope/mountain classification). The unable_unwilling_doctrine_reading permits strikes against non-state actors in uncontrolled host-state territory (moderate extraction, tangled-rope classification). All three readings claim the same charter text as authority; they differ in what that text is interpreted to permit. The constraint stories link via network.affects_constraints to indicate structural kinship and to enable contamination analysis: if one reading's legitimacy degrades, that may strengthen sibling readings through institutional reaction, or it may normalize the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_51_self_defense__expansive_preventive_reading, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
