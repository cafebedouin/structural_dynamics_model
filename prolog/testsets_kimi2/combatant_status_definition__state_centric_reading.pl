% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__state_centric_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: combatant_status_definition__state_centric_reading
 *   human_readable: State-Centric Combatant Status Definition (GC III Art. 4)
 *   domain: international_law/humanitarian_law
 *
 * SUMMARY:
 *   This constraint instantiates the state-centric reading of the
 *   combatant_status_definition kernel: under Geneva Convention III Article
 *   4, combatant status and the attendant POW protections are reserved for
 *   members of formal state militaries meeting specific organizational
 *   criteria. Non-state actors are categorically excluded. The reading treats
 *   this exclusion as a necessary feature of interstate legal order; its
 *   siblings (national_liberation_reading and functional_protection_reading)
 *   treat the same text as either expandable to organized non-state groups or
 *   superseded by status-independent humane-treatment minima.
 *
 * KEY AGENTS:
 *   - state_parties_geneva: Primary agenda-setter (institutional/global) â drafts, ratifies, and enforces the treaty framework reserving combatant status to state militaries.
 *   - state_militaries: Primary beneficiary (organized/global) â receive full POW protections and reciprocal immunity under the state-centric definition.
 *   - non_state_fighters: Primary target (powerless/regional) â categorically denied POW status under this reading; subject to domestic prosecution and execution upon capture.
 *   - icrc: Analytical observer (institutional/global) â monitors compliance, advocates for protective scope expansion, but does not set the legal definition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, 0.72).
domain_priors:suppression_score(combatant_status_definition__state_centric_reading, 0.75).
domain_priors:theater_ratio(combatant_status_definition__state_centric_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__state_centric_reading, "State-Centric Combatant Status Definition (GC III Art. 4)").
narrative_ontology:topic_domain(combatant_status_definition__state_centric_reading, "international_law/humanitarian_law").

domain_priors:requires_active_enforcement(combatant_status_definition__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__state_centric_reading, 'e2da4304-7f8b-4763-b0df-2020f5444213').
narrative_ontology:cs_kernel_codification('e2da4304-7f8b-4763-b0df-2020f5444213', formalized).
narrative_ontology:cs_authority_grounding('e2da4304-7f8b-4763-b0df-2020f5444213', lineage).
narrative_ontology:cs_interpretation_layer_present('e2da4304-7f8b-4763-b0df-2020f5444213').
narrative_ontology:cs_reading_relation('e2da4304-7f8b-4763-b0df-2020f5444213', combatant_status_definition__national_liberation_reading, forecloses).
narrative_ontology:cs_reading_relation('e2da4304-7f8b-4763-b0df-2020f5444213', combatant_status_definition__functional_protection_reading, coexists_with).
narrative_ontology:cs_axiom('e2da4304-7f8b-4763-b0df-2020f5444213', foundational, combatant_status_requires_state_organization).
narrative_ontology:cs_axiom_status(combatant_status_requires_state_organization, holdable).
narrative_ontology:cs_axiom_grounding('e2da4304-7f8b-4763-b0df-2020f5444213', combatant_status_requires_state_organization, conventional).
narrative_ontology:cs_axiom('e2da4304-7f8b-4763-b0df-2020f5444213', foundational, state_reciprocity_as_sole_legitimate_basis_for_pow_immunity).
narrative_ontology:cs_axiom_status(state_reciprocity_as_sole_legitimate_basis_for_pow_immunity, holdable).
narrative_ontology:cs_axiom_grounding('e2da4304-7f8b-4763-b0df-2020f5444213', state_reciprocity_as_sole_legitimate_basis_for_pow_immunity, conventional).
narrative_ontology:cs_reference_frame('e2da4304-7f8b-4763-b0df-2020f5444213', westphalian_reciprocal_immunity).
narrative_ontology:cs_drift_state('e2da4304-7f8b-4763-b0df-2020f5444213', contemporary_asymmetric_warfare_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('e2da4304-7f8b-4763-b0df-2020f5444213', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__state_centric_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_militaries).
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_parties_geneva).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, non_state_fighters).
narrative_ontology:constraint_vindicates(combatant_status_definition__state_centric_reading, westphalian_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(combatant_status_definition__state_centric_reading, state_monopoly_on_lawful_force).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted, ratified, and actively maintain the Geneva Conventions framework. They assert that combatant status is contingent on formal incorporation into state military structures meeting Article 4 criteria. They resist expansion of POW protections to non-state actors through treaty reservations, non-ratification of AP I, and domestic legislation authorizing prosecution of captured insurgents.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, state_parties_geneva, agenda_setter,
    institutional, generational, constrained, global).

% Receive full POW protections and reciprocal immunity upon capture as lawful combatants, provided they operate under a state party's command and meet the formal criteria. Their operational doctrine assumes this legal shield; they do not choose the constraint but depend on it for personnel security.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, state_militaries, beneficiary,
    organized, biographical, constrained, global).

% Captured non-state fighters are categorically denied POW status under this reading and are channeled into domestic criminal prosecution or military commissions. They cannot access the Article 4 criteria because they lack state military organization, regardless of their own discipline, command structure, or adherence to the laws of war. Exit is impossible once captured.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, non_state_fighters, payer,
    powerless, immediate, trapped, regional).

% Monitors compliance with IHL, visits detainees, and publishes interpretive guidance. It observes that the state-centric definition leaves a protection gap for non-state fighters but does not possess treaty-making authority to alter the combatant status criteria.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, icrc, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__state_centric_reading, state_parties_geneva).
narrative_ontology:fixing_cost_class(combatant_status_definition__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Regularizes interstate armed conflict by granting reciprocal POW immunity to organized state militaries, reducing reprisals against captured uniformed soldiers and establishing a predictable legal framework for sovereign states at war.
% TRANSFER_FUNCTION: Moves legal immunity, procedural protections, and detention privileges from captured non-state fighters to state militaries and state legal regimes; non-state fighters bear the cost of domestic prosecution and denial of POW status.
% ABSENT_VOICES: Non-state armed group commanders and representatives were structurally excluded from the 1949 diplomatic conferences; subsequent liberation movements and insurgent organizations remain outside the treaty modification process despite being directly governed by the constraint.
% DISAPPEARANCE_RATIONALE: If the state-centric combatant definition disappeared overnight, state militaries would lose guaranteed POW reciprocity frameworks, non-state fighters would gain presumptive POW protections or mandatory status determination tribunals, and domestic prosecution regimes for captured insurgents would face collapse under international legal pressure.
% FOUNDING_PROBLEM: Post-World War II need to codify protections for captured uniformed state soldiers and establish clear reciprocal obligations among sovereign states to treat each other's lawful combatants as POWs rather than criminals.
% FOUNDING_PROBLEM_CORROBORATION: Military historians and state diplomatic archives attest to the post-1949 interstate reciprocity concern. However, humanitarian law scholars and post-colonial legal historians outside the direct beneficiary set contest that the arrangement now functions more as sovereign privilege than as necessary protection, citing the changed character of armed conflict.
narrative_ontology:disappearance_verdict(combatant_status_definition__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__state_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(combatant_status_definition__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__state_centric_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(combatant_status_definition__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.72) is high because the constraint systematically strips a large and growing class of actors (non-state fighters) of fundamental legal protections, transferring the cost of that exclusion to state prosecutorial regimes and the fighters themselves. Suppression (0.75) is high because the constraint's persistence depends on states actively resisting alternative definitions (non-ratification of AP I, domestic legislation, military commissions). Theater ratio (0.28) is moderate-low: the formal criteria (uniform, fixed distinctive sign, command structure) retain some operational meaning but increasingly function as ceremonial gatekeeping in asymmetric warfare. Accessibility collapse (0.78) is high because once the legal framework is internalized, there is no alternative pathway for non-state fighters to obtain POW status within this reading. Resistance (0.60) reflects sustained critique from humanitarian lawyers, some states, and international tribunals.
 *
 * PERSPECTIVAL GAP:
 *   From the state-party and state-military seats, the constraint is necessary interstate coordination that protects captured soldiers and regularizes warfare; from the non-state fighter seat, the identical legal text operates as an active, enforced exclusion that exposes them to criminal sanction based solely on their lack of state sponsorship. The engine computes this divergence from the structural asymmetry in beneficiary/victim declarations and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   State parties and state militaries are structural beneficiaries: the constraint subsidizes their sovereignty and operational security (low directionality). Non-state fighters are the declared victims: they bear the full extraction of prosecutorial exposure and denial of POW privileges (high directionality). The ICRC occupies an analytical seat with negligible directionality. No overrides are needed because the structural derivation correctly captures these relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The state-centric reading resists mandatrophy by maintaining that its founding problemâinterstate warfare and reciprocal soldier protectionâremains live. However, the temporal measurements show extraction accumulating as the character of armed conflict shifts toward non-state and asymmetric forms, suggesting the coordination function is increasingly serving as cover for sovereign privilege. The R5 mismatch consumer will note that founding_problem_status is contested and disappearance_verdict is world_rearranges, which cross-checks against the tangled_rope classification rather than a pure piton reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_location,
    'The disagreement between the state-centric reading and its siblings is located at the structural requirement of state organization: does lawful combatant immunity derive from membership in a state military (this reading), from functional criteria of organization and control (national_liberation_reading), or from the fact of detention alone (functional_protection_reading)?',
    'Jurisprudential analysis of treaty interpretation practice, state consent to AP I Article 1(4), and customary law formation.',
    'Resolving this locates whether the high extractiveness from non-state fighters is an intrinsic legal entailment of the state-centric framework or a contested policy choice masking as legal necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_location, conceptual, 'Locates the structural element where kernel readings diverge').

omega_variable(
    extraction_coordination_separability,
    'Can the genuine coordination function (reciprocal POW protections for state militaries) be preserved without the asymmetric extraction from non-state fighters?',
    'Comparative analysis of jurisdictions or tribunals that grant non-state fighters POW-equivalent protections while maintaining state-military reciprocity.',
    'If separable, the state-centric definition uses coordination as cover for extraction; if inseparable, the extraction is the structural price of the coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_coordination_separability, empirical, 'Whether coordination and extraction are structurally separable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__state_centric_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t0, combatant_status_definition__state_centric_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(comb_tr_t15, combatant_status_definition__state_centric_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(comb_tr_t28, combatant_status_definition__state_centric_reading, theater_ratio, 28, 0.2).
narrative_ontology:measurement(comb_tr_t40, combatant_status_definition__state_centric_reading, theater_ratio, 40, 0.23).
narrative_ontology:measurement(comb_tr_t55, combatant_status_definition__state_centric_reading, theater_ratio, 55, 0.25).
narrative_ontology:measurement(comb_tr_t75, combatant_status_definition__state_centric_reading, theater_ratio, 75, 0.28).

% Extraction over time
narrative_ontology:measurement(comb_be_t0, combatant_status_definition__state_centric_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(comb_be_t15, combatant_status_definition__state_centric_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(comb_be_t28, combatant_status_definition__state_centric_reading, base_extractiveness, 28, 0.55).
narrative_ontology:measurement(comb_be_t40, combatant_status_definition__state_centric_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(comb_be_t55, combatant_status_definition__state_centric_reading, base_extractiveness, 55, 0.68).
narrative_ontology:measurement(comb_be_t75, combatant_status_definition__state_centric_reading, base_extractiveness, 75, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t0, combatant_status_definition__state_centric_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(comb_su_t15, combatant_status_definition__state_centric_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(comb_su_t28, combatant_status_definition__state_centric_reading, suppression_requirement, 28, 0.6).
narrative_ontology:measurement(comb_su_t40, combatant_status_definition__state_centric_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(comb_su_t55, combatant_status_definition__state_centric_reading, suppression_requirement, 55, 0.72).
narrative_ontology:measurement(comb_su_t75, combatant_status_definition__state_centric_reading, suppression_requirement, 75, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, combatant_status_definition__national_liberation_reading).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, combatant_status_definition__functional_protection_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the combatant_status_definition kernel, decomposed per the Îµ-invariance principle because the sibling readings (national_liberation, functional_protection) produce structurally distinct beneficiary/victim profiles and Îµ values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
