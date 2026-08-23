% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__exogenous_override_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: imposition_mechanism_kernel__exogenous_override_reading
 *   human_readable: State-Imposed Normative Override via Coercive Monopoly
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint models the exogenous override reading of the imposition
 *   mechanism kernel: new norms imposed by state coercion where legitimacy
 *   derives from the monopoly on violence rather than cultural acceptance.
 *   The standing arrangement is a state that has conquered, colonized, or
 *   revolutionarily seized a population and imposes a normative order
 *   (language, religion, law, ritual, identity categories) through sustained
 *   enforcement. Compliance is extracted, not elicited. The constraint is the
 *   standing arrangement of imposed normativity itself — the apparatus of
 *   coercive normalization.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, 0.78).
domain_priors:suppression_score(imposition_mechanism_kernel__exogenous_override_reading, 0.82).
domain_priors:theater_ratio(imposition_mechanism_kernel__exogenous_override_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__exogenous_override_reading, snare).
narrative_ontology:human_readable(imposition_mechanism_kernel__exogenous_override_reading, "State-Imposed Normative Override via Coercive Monopoly").
narrative_ontology:topic_domain(imposition_mechanism_kernel__exogenous_override_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__exogenous_override_reading, '7fa09aec-809e-4c54-8aa7-86531edc1abb').
narrative_ontology:cs_kernel_codification('7fa09aec-809e-4c54-8aa7-86531edc1abb', implicit).
narrative_ontology:cs_authority_grounding('7fa09aec-809e-4c54-8aa7-86531edc1abb', extraction).
narrative_ontology:cs_interpretation_layer_present('7fa09aec-809e-4c54-8aa7-86531edc1abb').
narrative_ontology:cs_reading_relation('7fa09aec-809e-4c54-8aa7-86531edc1abb', imposition_mechanism_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('7fa09aec-809e-4c54-8aa7-86531edc1abb', imposition_mechanism_kernel__hybrid_legitimation_reading, influences).
narrative_ontology:cs_axiom('7fa09aec-809e-4c54-8aa7-86531edc1abb', foundational, violence_monopoly_suffices_for_normative_authority).
narrative_ontology:cs_axiom_status(violence_monopoly_suffices_for_normative_authority, holdable).
narrative_ontology:cs_axiom_grounding('7fa09aec-809e-4c54-8aa7-86531edc1abb', violence_monopoly_suffices_for_normative_authority, instrumental).
narrative_ontology:cs_axiom('7fa09aec-809e-4c54-8aa7-86531edc1abb', secondary, cultural_consent_is_irrelevant_to_binding_normativity).
narrative_ontology:cs_axiom_status(cultural_consent_is_irrelevant_to_binding_normativity, holdable).
narrative_ontology:cs_axiom_grounding('7fa09aec-809e-4c54-8aa7-86531edc1abb', cultural_consent_is_irrelevant_to_binding_normativity, conventional).
narrative_ontology:cs_reference_frame('7fa09aec-809e-4c54-8aa7-86531edc1abb', coercive_state_as_primary_norm_source).
narrative_ontology:cs_drift_state('7fa09aec-809e-4c54-8aa7-86531edc1abb', late_imperial_or_post_revolutionary_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7fa09aec-809e-4c54-8aa7-86531edc1abb', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, central_state_apparatus).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, aligned_elite_factions).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, state_ideology_administrators).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, subject_population).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, traditional_cultural_authorities).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, dissenting_intellectuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, state_ideology_administrators).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__exogenous_override_reading, state_monopoly_on_violence_legitimizes_normative_order).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__exogenous_override_reading, coercive_imposition_can_generate_binding_norms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commands the monopoly on violence and uses it to impose new normative frameworks. Extracts compliance through surveillance, legal sanction, and resource control. Legitimacy is claimed from the fact of effective control rather than cultural resonance. Can redirect enforcement resources at will.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, central_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Gain preferential access to state resources, status, and enforcement protection by performing adherence to imposed norms. Their cooperation is purchased with material and symbolic rewards. Exit is available through defection to rival power centers or foreign patronage, but costly in lost privileges.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, aligned_elite_factions, beneficiary,
    powerful, biographical, mobile, national).

% Staff the bureaucratic apparatus that translates coercive mandates into normalized social practice (education, media, ritual). They benefit from career advancement and institutional capture but bear the cost of performing belief they may not hold. Professional identity fuses with the imposed framework, making exit identity-threatening.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, state_ideology_administrators, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__exogenous_override_reading, state_ideology_administrators, payer).

% Bear the full compliance costs: behavioral modification, surveillance exposure, sanction risk, and the cognitive load of performing alien norms. No meaningful exit — migration is blocked or prohibitively costly, resistance invites disproportionate retaliation. Compliance is performative and conditional on monitoring visibility.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, subject_population, payer,
    powerless, biographical, trapped, national).

% Religious leaders, kinship elders, customary law holders whose authority derives from the endogenous cultural order. The imposed norms directly displace their legitimacy and extraction base. They cannot exit their role without dissolving their identity and community standing. Resistance is ongoing but fragmented.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, traditional_cultural_authorities, payer,
    moderate, generational, identity_locked, regional).

% Articulate the illegitimacy of the imposed order and the viability of alternatives. Systematically denied platforms, subjected to professional exclusion, imprisonment, or exile. Their exclusion is structural — the constraint's coherence depends on their silence.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, dissenting_intellectuals, excluded,
    moderate, biographical, constrained, national).

% Historians, anthropologists, political theorists analyzing the imposition from outside the coercive envelope. They see the full structure: the extraction, the resistance, the performative compliance, and the contested legitimacy. No material stake in the outcome.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, external_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement coordinates mass behavioral conformity across a heterogeneous population by substituting state enforcement for the slow work of cultural consensus. It solves the coordination problem of rapid normative unification at scale.
% TRANSFER_FUNCTION: Transfers legitimacy capital, material resources, and status from the subject population and traditional authorities to the state apparatus and its aligned elites. The population pays in compliance, surveillance exposure, and cognitive alienation; the state collects unified normative control and the rents of being the sole legitimate norm-source.
% ABSENT_VOICES: The subject population's authentic normative preferences are structurally absent — they would reject the imposed norms if the coercive threat were removed. Traditional cultural authorities would articulate alternative legitimacy sources. Both are silenced by the same enforcement machinery that imposes the norms.
% DISAPPEARANCE_RATIONALE: If the coercive imposition vanished overnight, the imposed norms would not persist as binding. The population would revert to pre-existing or emerging endogenous normative frameworks. Traditional authorities would reassert legitimacy claims. The state would lose its primary instrument of normative control and face a legitimacy vacuum requiring either negotiation or collapse.
% FOUNDING_PROBLEM: The state faced a heterogeneous population with competing normative orders that impeded centralized administration, resource extraction, and military mobilization. It needed a single legible normative framework to make the population governable at scale.
% FOUNDING_PROBLEM_CORROBORATION: State chronicles and administrative records attest the governability problem was real and the imposition solved it. Subjugated population oral histories, resistance movement manifestos, and comparative historical scholarship from outside the state tradition attest the 'governability' framing masks a domination project — the problem was not incoherence but the state's inability to extract from coherent alternative orders.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(imposition_mechanism_kernel__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__exogenous_override_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_mechanism_kernel__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   High extractiveness (0.78) reflects that the population bears massive compliance costs while the state and aligned elites capture the benefits of unified control. High suppression (0.82) reflects that alternatives are not merely discouraged but actively eliminated — rival normative authorities are displaced, dissent is criminalized, exit is blocked. Theater ratio (0.45) captures the growing performative layer: as raw coercion becomes politically costly, the state invests in ideological apparatus (education, media, ritual) to manufacture the appearance of consent. Accessibility collapse (0.75) is high but not total — endogenous normative alternatives persist in hidden transcripts and resistant communities. Resistance (0.65) is substantial and persistent, confirming the constraint is experienced as extraction, not coordination.
 *
 * PERSPECTIVAL GAP:
 *   From the state's seat, the constraint appears as necessary coordination — creating a governable population from chaos. From the subject population's seat, it is pure extraction — compliance purchased with violence. From traditional authorities' seat, it is existential displacement. The engine computes these divergent seat-types from the single structural description; the claimed_type (snare) reflects the target-seat reality, which is the majority experience.
 *
 * DIRECTIONALITY LOGIC:
 *   The central state apparatus is the structural beneficiary (d ~ 0.1) — it sets the agenda, collects the compliance rents, and controls enforcement. Aligned elites and ideology administrators are secondary beneficiaries with partial capture (d ~ 0.25-0.35). The subject population is the primary target (d ~ 0.95) — trapped, bearing full costs, no exit. Traditional authorities are identity-locked targets (d ~ 0.85) — their structural position is constituted by the order being destroyed. Dissenting intellectuals are excluded (d ~ 0.9) — their structural role is to be silenced. The engine computes per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (governability of a heterogeneous population) was real but the solution (coercive normative unification) has outlived its administrative necessity in many cases — modern states retain imposed norms long after bureaucratic legibility could be achieved through pluralistic frameworks. The mandate has atrophied into domination. The constraint persists because the state apparatus extracts status, resources, and legitimacy from being the sole norm-source; dismantling it would require the state to surrender its monopoly on normative authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exogenous_vs_endogenous_boundary,
    'Is the exogenous override reading a distinct imposition mechanism, or does every imposition contain endogenous elements that the override reading systematically obscures?',
    'Comparative historical analysis of cases coded as ''pure override'' — trace whether pre-existing cultural substrates, elite collaboration, or population agency shaped the imposed norms'' specific content and reception.',
    'If endogenous elements are structurally necessary to the imposition''s operation, the exogenous override reading is a partial framing that masks a hybrid mechanism. The kernel would be better modeled as a constraint family with a dominant hybrid member.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exogenous_vs_endogenous_boundary, conceptual, 'Whether the exogenous override reading captures a pure mechanism or a framing that erases endogenous structure.').

omega_variable(
    legitimacy_source_ambiguity,
    'Does the state''s monopoly on violence *generate* legitimacy for imposed norms, or merely *enforce compliance* while legitimacy remains a separate, unachieved variable?',
    'Longitudinal study of compliance persistence after monitoring relaxation: if compliance collapses, violence enforced behavior but did not generate legitimacy. If compliance persists, some legitimacy transfer occurred.',
    'If violence only enforces without legitimizing, the constraint''s claimed coordination function is illusory — it is pure extraction (snare) with no rope component. If legitimacy transfers, a tangled_rope component exists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, empirical, 'Whether coercive imposition produces genuine legitimacy or only performed compliance.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the imposition_mechanism_kernel frame the question as ''how norms become binding'' (neutral) or ''how power imposes norms'' (loaded toward override)?',
    'Analyze whether the kernel''s formulation privileges state-centered agency. Compare with kernels framed from the population''s perspective (e.g., ''how populations adopt or resist normative change'').',
    'If the kernel framing is state-centered, the exogenous override reading is structurally favored — the kernel itself embeds a bias. This would make the reading family asymmetrical: the override reading answers the kernel''s implicit question; the climb reading answers a different question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel''s framing predetermines the reading distribution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__exogenous_override_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(impo_tr_t20, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(impo_tr_t40, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(impo_tr_t60, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 60, 0.45).
narrative_ontology:measurement(impo_tr_t80, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 80, 0.48).
narrative_ontology:measurement(impo_tr_t100, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(impo_be_t20, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 20, 0.82).
narrative_ontology:measurement(impo_be_t40, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement(impo_be_t60, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 60, 0.75).
narrative_ontology:measurement(impo_be_t80, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 80, 0.76).
narrative_ontology:measurement(impo_be_t100, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 100, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(impo_su_t20, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 20, 0.88).
narrative_ontology:measurement(impo_su_t40, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 40, 0.85).
narrative_ontology:measurement(impo_su_t60, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 60, 0.82).
narrative_ontology:measurement(impo_su_t80, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 80, 0.8).
narrative_ontology:measurement(impo_su_t100, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 100, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__exogenous_override_reading, endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__exogenous_override_reading, hybrid_legitimation_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__exogenous_override_reading, state_administrative_legibility).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__exogenous_override_reading, cultural_resistance_networks).

% DUAL FORMULATION NOTE:
% The imposition_mechanism_kernel decomposes into three constraint stories: exogenous_override_reading (this file, snare — high extraction, coercive), endogenous_climb_reading (rope — coordination via cultural adoption), hybrid_legitimation_reading (tangled_rope — symbolic transfer + incentives). The ε values differ substantially: override reading ε≈0.78, climb reading ε≈0.15, hybrid reading ε≈0.45. They share the referent 'normative binding force' but the readings instantiate different constraints with different beneficiary/victim structures. This story is the override reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imposition_mechanism_kernel__exogenous_override_reading, organized, 0.35).
constraint_indexing:directionality_override(imposition_mechanism_kernel__exogenous_override_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
