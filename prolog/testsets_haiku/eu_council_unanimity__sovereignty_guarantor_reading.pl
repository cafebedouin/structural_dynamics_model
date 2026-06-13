% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__sovereignty_guarantor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__sovereignty_guarantor_reading, []).

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
 *   constraint_id: eu_council_unanimity__sovereignty_guarantor_reading
 *   human_readable: EU Council Unanimity Rule: Sovereignty Guarantor Reading
 *   domain: institutional_design/international_relations
 *
 * SUMMARY:
 *   The EU Council's unanimity requirement embodies the principle that member
 *   states retain sovereign equality and cannot be bound by decisions they
 *   fundamentally reject. This reading frames the veto as a legitimate
 *   rights-exercise protecting smaller and medium states from being
 *   systematically outvoted on core matters — taxation, social policy,
 *   security commitments. The constraint operationalizes the idea that the
 *   European Union is a covenant of sovereign equals, not a federal
 *   hierarchy. Extracted value is moderate (0.35) because coordination costs
 *   are real; all states must negotiate with holdouts, compromises are
 *   slower, and veto threats do extract side-payments. But this reading
 *   claims that the extraction is coordinate with the coordination function,
 *   not parasitic on it. The three sibling readings contest this: the
 *   veto_trap reading sees extraction as the primary function (minoritarian
 *   blocking for gain), and the diplomatic_capital reading emphasizes
 *   consensus-building legitimacy over sovereign protection.
 *
 * KEY AGENTS:
 *   - Small and medium states: use the veto as protective shield against majoritarian coalitions; the constraint is their guarantee that they cannot be overridden on sovereignty-core questions like taxation, immigration, defense.
 *   - Large states: hold the same formal veto power but rarely invoke it because their ex-ante negotiating leverage (economic weight, diplomatic networks, military capacity) gives them influence over outcomes before the veto point. The veto is a backstop.
 *   - Coalition seeking collective action: must negotiate with every state to achieve unanimity; pays the cost of delays and compromises.
 *   - Supranational institutions: cannot override Council decisions; limited in their agenda-setting power by the unanimity constraint.
 *   - Non-member states and excluded actors: experience EU decisions as externally set without voice in the process.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__sovereignty_guarantor_reading, 0.35).
domain_priors:suppression_score(eu_council_unanimity__sovereignty_guarantor_reading, 0.12).
domain_priors:theater_ratio(eu_council_unanimity__sovereignty_guarantor_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__sovereignty_guarantor_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__sovereignty_guarantor_reading, "EU Council Unanimity Rule: Sovereignty Guarantor Reading").
narrative_ontology:topic_domain(eu_council_unanimity__sovereignty_guarantor_reading, "institutional_design/international_relations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__sovereignty_guarantor_reading, '9a0a0af7-fb5c-43b0-b8a2-a4606384f05e').
narrative_ontology:cs_kernel_codification('9a0a0af7-fb5c-43b0-b8a2-a4606384f05e', formalized).
narrative_ontology:cs_authority_grounding('9a0a0af7-fb5c-43b0-b8a2-a4606384f05e', lineage).
narrative_ontology:cs_interpretation_layer_present('9a0a0af7-fb5c-43b0-b8a2-a4606384f05e').
narrative_ontology:cs_reading_relation('9a0a0af7-fb5c-43b0-b8a2-a4606384f05e', eu_council_unanimity__veto_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('9a0a0af7-fb5c-43b0-b8a2-a4606384f05e', eu_council_unanimity__diplomatic_capital_reading, coexists_with).
narrative_ontology:cs_axiom('9a0a0af7-fb5c-43b0-b8a2-a4606384f05e', foundational, sovereign_equality_categorical).
narrative_ontology:cs_axiom_status(sovereign_equality_categorical, holdable).
narrative_ontology:cs_axiom_grounding('9a0a0af7-fb5c-43b0-b8a2-a4606384f05e', sovereign_equality_categorical, deontological).
narrative_ontology:cs_axiom('9a0a0af7-fb5c-43b0-b8a2-a4606384f05e', foundational, veto_as_rights_exercise_not_threat).
narrative_ontology:cs_axiom_status(veto_as_rights_exercise_not_threat, holdable).
narrative_ontology:cs_axiom_grounding('9a0a0af7-fb5c-43b0-b8a2-a4606384f05e', veto_as_rights_exercise_not_threat, deontological).
narrative_ontology:cs_reference_frame('9a0a0af7-fb5c-43b0-b8a2-a4606384f05e', treaty_based_sovereign_consent_baseline).
narrative_ontology:cs_drift_state('9a0a0af7-fb5c-43b0-b8a2-a4606384f05e', contemporary_efficiency_pressure, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('9a0a0af7-fb5c-43b0-b8a2-a4606384f05e', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, small_and_medium_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, all_member_states).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__sovereignty_guarantor_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(eu_council_unanimity__sovereignty_guarantor_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__sovereignty_guarantor_reading_tests).
:- end_tests(eu_council_unanimity__sovereignty_guarantor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) rather than low because coordination costs genuinely exist — unanimity forces longer negotiation, side-payments, and compromises. But it is not high because the primary mechanism is not extraction of concentrated rents; it is allocation of protective veto power. The constraint is designed to prevent majoritarian extraction, not to enable minoritarian extraction (though the veto_trap reading argues the two become indistinguishable in practice). Theater is very low (0.08) because the sovereignty-protection function is the primary stated purpose, and performance of sovereignty claims is minimal — states do not ceremonially invoke the veto for show. Suppression is low (0.12) because the constraint does not rely on coercing acceptance; it provides opt-out rights. The accessibility of alternatives is high (0.78 collapse) because member states genuinely have only one practical choice if they want EU membership and participation — they cannot exit for marginal disagreements without bearing catastrophic costs. Resistance is low (0.22) because the constraint is formally accepted by all parties and codified in treaty law; the resistance that exists comes from coalition actors frustrated by veto-blocking, not from states refusing to participate in the unanimity process itself.
 *
 * PERSPECTIVAL GAP:
 *   From the small and medium states' perspective, unanimity is a genuine rope — a coordination mechanism that includes them and protects them from majoritarian harm. From the large states' perspective, unanimity is a constraint on their agenda-setting power but not a threat, because they have substitute leverage. From the perspective of coalitions trying to move policy forward, unanimity is a bottleneck that extracts concessions. From non-member states' perspective, it is an exclusionary rule that governs decision-making they have no voice in. The engine should compute different types for each seat based on power and exit options; this story declares the reading (sovereignty guarantor) but does not adjudicate the seat-specific computations.
 *
 * DIRECTIONALITY LOGIC:
 *   Small and medium states are beneficiaries (d near 0.0) because the constraint protects them from bearing costs they reject. Large states are also formally beneficiaries (d near 0.1-0.2) because they hold the same veto, but their actual directionality is less extractive because their ex-ante power gives them influence independent of the veto; their effective d might be computed closer to 0.2-0.3 once the engine accounts for their alternative leverage. Coalitions seeking collective action are payers (d near 0.7) because they bear the coordination costs and must satisfy holdout states. The constraint's directionality is asymmetric by seat: what looks like rope (symmetric, protective) to small states may compute as a different type when seat-specific factors are weighted in — large states may extract more value than their formal role suggests, making their seat-specific type closer to tangled_rope (coordinating for some outcomes, extracting for others). This divergence is the core analytical puzzle and is properly a matter of per-seat computation by the engine.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy by asserting the founding problem (sovereign equality protection against majoritarian coercion) is still live, not dead. The constraint's mandate is to prevent majoritarian harm to minority states, and that mandate persists as long as member states fear being overridden on core sovereignty questions. The tension is not between mandate and function — the function and mandate align. The tension is between this reading's claim and the veto_trap reading's claim about what the constraint actually does. That is a substantive disagreement about interpretation, not mandatrophy. Mandatrophy would emerge if the constraint persisted despite unanimous agreement that sovereignty protection is no longer necessary — if all parties said 'we don't need this anymore' but the rule stayed in place. The current situation is that agreement is contested: some parties (small states, constitutional traditionalists) defend unanimity as necessary for sovereignty; others (large states, efficiency advocates) argue it is obsolete. That contest is not mandatrophy — it is the normal working of a contested institutional principle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_veto_trap_reading,
    'Does the measured extractiveness (0.35 at interval end) reflect legitimate sovereign defense, or does it capture structural vulnerability to minoritarian extraction through veto threats (the veto_trap_reading interpretation)?',
    'Longitudinal analysis of actual veto usage: tally vetoes invoked to defend core sovereignty vs. vetoes invoked to extract concessions unrelated to the blocking state''s core interests. High rates of extraction-motivated vetoes would support the veto_trap reading; vetoes concentrated on sovereignty-core questions would support the sovereignty_guarantor reading.',
    'If vetoes are predominantly extractive, the constraint reclassifies as snare (minoritarian blocking for gain). If predominantly sovereign-defense, it remains rope but the extracted value would be reallocated to legitimate coordination cost rather than asymmetric extraction. The two readings diverge fundamentally on this axis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_veto_trap_reading, empirical, 'Whether measured extraction reflects sovereignty protection or minoritarian rent-seeking.').

omega_variable(
    kernel_contest_diplomatic_capital_reading,
    'Does unanimity''s coordination function (forcing iterative negotiation, building legitimacy through consensus-building process) account for a substantial share of its stabilization value, or is that framing post-hoc rationalization for a constraint driven by sovereignty protection?',
    'Comparative institutional analysis: does unanimity produce measurably higher compliance and public support for decisions compared to simulated majority-vote outcomes on the same policy questions? Does the consensus-building process generate information revelation that improves decision quality, or does it simply delay decisions to extract side-payments?',
    'Strong evidence that consensus-building improves outcomes would support the diplomatic_capital reading; evidence that delays are extractive and outputs are compromised rather than improved would support the sovereignty_guarantor reading (coordination is secondary to protection). The readings coexist if both effects are present.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_diplomatic_capital_reading, empirical, 'Whether unanimity''s value derives from consensus-building process or from sovereignty protection, or both.').

omega_variable(
    asymmetry_large_vs_small_state_leverage,
    'The constraint is formally symmetric (each state has one veto), but do large states extract more value from the unanimity requirement than small states, due to their greater ex-ante negotiating power? If so, does this constitute hidden asymmetric extraction riding on a symmetric rule?',
    'Game-theoretic analysis of veto point payoffs: compute the Shapley value or coalition-bargaining solution for Council decisions under unanimity vs. qualified majority rule, stratified by state size. Compare predicted and observed concessions patterns to determine whether large states systematically extract more value than the formal symmetry would suggest.',
    'If large states extract substantial asymmetric value, the constraint may operate as tangled_rope for large states (beneficiary) and rope for small states (pure protection with no hidden cost), rather than uniform rope. This would mean the computation diverges by seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetry_large_vs_small_state_leverage, empirical, 'Whether formal symmetry masks asymmetric extraction favoring large states.').

omega_variable(
    founding_problem_obsolescence_contestation,
    'This reading declares founding_problem_status = live. Do major EU institutional actors and member states actually still believe the original sovereignty-protection mandate is necessary, or do they view it as outgrown and primarily invoked rhetorically by defenders of the status quo?',
    'Analysis of major policy reform proposals and constitutional debates: do institutional proposals to move beyond unanimity (e.g., qualified majority on foreign policy, fiscal coordination) argue that the founding problem is obsolete, or that new coordination mechanisms can protect sovereignty better than unanimity? Do small states argue for unanimity retention on sovereignty grounds or primarily on protective-minority grounds?',
    'If the founding problem is genuinely perceived as obsolete by most parties and unanimity is defended primarily for protective reasons (minority protection rather than sovereign equality), the status shifts toward ''contested'' or ''dead'' and the constraint''s legitimacy story weakens. If the founding problem is still invoked as foundational, the live status holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_obsolescence_contestation, conceptual, 'Whether the founding problem (sovereignty protection against majoritarian coercion) remains the operative legitimacy ground or is now rhetorically deployed to defend an obsolete institutional form.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__sovereignty_guarantor_reading, 1957, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t1957, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 1957, 0.03).
narrative_ontology:measurement(eu_c_tr_t1986, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 1986, 0.05).
narrative_ontology:measurement(eu_c_tr_t2001, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2001, 0.07).
narrative_ontology:measurement(eu_c_tr_t2015, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2015, 0.08).
narrative_ontology:measurement(eu_c_tr_t2026, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2026, 0.08).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t1957, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 1957, 0.22).
narrative_ontology:measurement(eu_c_be_t1986, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 1986, 0.28).
narrative_ontology:measurement(eu_c_be_t2001, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2001, 0.33).
narrative_ontology:measurement(eu_c_be_t2015, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(eu_c_be_t2026, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2026, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t1957, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 1957, 0.05).
narrative_ontology:measurement(eu_c_su_t1986, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 1986, 0.08).
narrative_ontology:measurement(eu_c_su_t2001, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2001, 0.11).
narrative_ontology:measurement(eu_c_su_t2015, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2015, 0.12).
narrative_ontology:measurement(eu_c_su_t2026, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2026, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__sovereignty_guarantor_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(eu_council_unanimity__sovereignty_guarantor_reading, 0.12).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity__veto_trap_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity__diplomatic_capital_reading).

% DUAL FORMULATION NOTE:
% The EU Council unanimity rule is a contested kernel with three structurally distinct readings. Each reading instantiates a different constraint with different ε values, beneficiary structures, and types. The sovereignty_guarantor_reading (this story) presents unanimity as protective: moderate extraction (0.35) reflecting coordination costs, no systematic rent-seeking. The veto_trap_reading frames the same rule as minoritarian extraction: higher extractiveness, higher suppression, snare classification. The diplomatic_capital_reading emphasizes consensus-building legitimacy: lower extractiveness as coordination value, rope with high legitimacy offset. These are not three perspectives on one constraint — they are three different constraints instantiated by the same institutional rule. They are linked via network.affects_constraints to enable contamination analysis: if one reading's interpretation gains institutional acceptance (e.g., courts rule that veto use is extractive and must be regulated), the other readings' empirical premises are challenged. The three readings correspond to the three competing diagnoses of what unanimity does: protects sovereignty, enables extraction, or builds legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_council_unanimity__sovereignty_guarantor_reading, moderate, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
