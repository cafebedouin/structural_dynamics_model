% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__non_ratifier_enforcement_reading, []).

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
 *   constraint_id: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
 *   human_readable: Freedom of Navigation as Customary International Law (Non-Ratifier Enforcement Reading)
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint story describes the 'non-ratifier enforcement' reading of
 *   maritime sovereignty boundaries, where major naval powers assert that
 *   freedom of navigation principles are customary international law (CIL)
 *   and thus binding on all states, regardless of their ratification of the
 *   UN Convention on the Law of the Sea (UNCLOS). This reading allows naval
 *   powers to conduct operations in the Exclusive Economic Zones (EEZs) of
 *   coastal states without seeking permission, often against the coastal
 *   states' interpretation of their own sovereign rights. The constraint is
 *   actively enforced through naval presence and diplomatic actions, creating
 *   a dynamic where coastal states are victims of this interpretation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.65).
domain_priors:suppression_score(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.75).
domain_priors:theater_ratio(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "Freedom of Navigation as Customary International Law (Non-Ratifier Enforcement Reading)").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 'e1b14cb4-af5a-46a6-b1e0-7788c5ebc8c1').
narrative_ontology:cs_kernel_codification('e1b14cb4-af5a-46a6-b1e0-7788c5ebc8c1', formalized).
narrative_ontology:cs_authority_grounding('e1b14cb4-af5a-46a6-b1e0-7788c5ebc8c1', extraction).
narrative_ontology:cs_interpretation_layer_present('e1b14cb4-af5a-46a6-b1e0-7788c5ebc8c1').
narrative_ontology:cs_reading_relation('e1b14cb4-af5a-46a6-b1e0-7788c5ebc8c1', unclos_sovereignty_boundary__strict_eez_reading, influences).
narrative_ontology:cs_reading_relation('e1b14cb4-af5a-46a6-b1e0-7788c5ebc8c1', unclos_sovereignty_boundary__historical_rights_reading, coexists_with).
narrative_ontology:cs_axiom('e1b14cb4-af5a-46a6-b1e0-7788c5ebc8c1', foundational, freedom_of_navigation_as_cil).
narrative_ontology:cs_axiom_status(freedom_of_navigation_as_cil, holdable).
narrative_ontology:cs_axiom_grounding('e1b14cb4-af5a-46a6-b1e0-7788c5ebc8c1', freedom_of_navigation_as_cil, conventional).
narrative_ontology:cs_axiom('e1b14cb4-af5a-46a6-b1e0-7788c5ebc8c1', secondary, military_activities_in_eez_are_innocent_passage).
narrative_ontology:cs_axiom_status(military_activities_in_eez_are_innocent_passage, holdable).
narrative_ontology:cs_axiom_grounding('e1b14cb4-af5a-46a6-b1e0-7788c5ebc8c1', military_activities_in_eez_are_innocent_passage, conventional).
narrative_ontology:cs_reference_frame('e1b14cb4-af5a-46a6-b1e0-7788c5ebc8c1', post_westphalian_high_seas_freedom).
narrative_ontology:cs_drift_state('e1b14cb4-af5a-46a6-b1e0-7788c5ebc8c1', contemporary_unclos_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e1b14cb4-af5a-46a6-b1e0-7788c5ebc8c1', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, major_naval_powers).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_states_asserting_exclusive_eez_rights).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, international_shipping_industry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states assert the right to conduct naval operations, intelligence gathering, and commercial transit in Exclusive Economic Zones (EEZs) of non-ratifying states, or even ratifying states, based on customary international law (CIL) principles of freedom of navigation, independent of UNCLOS. They benefit from maintaining maximum operational flexibility and minimizing restrictions on their naval and commercial fleets. They actively enforce this interpretation through naval presence and diplomatic challenges.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, major_naval_powers, agenda_setter,
    institutional, generational, arbitrage, global).

% These states interpret their EEZ rights as granting them exclusive jurisdiction over all activities, including military and intelligence operations, within 200 nautical miles of their coastlines, even if they have not ratified UNCLOS or if their interpretation differs from major naval powers. They bear the cost of perceived sovereignty infringement and the inability to fully control activities in their claimed EEZs, often lacking the naval capacity to enforce their claims against major powers.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_states_asserting_exclusive_eez_rights, payer,
    moderate, biographical, constrained, regional).

% Benefits from the broadest possible interpretation of freedom of navigation, as it minimizes transit times, reduces regulatory hurdles, and lowers operational costs. They are less concerned with the specific legal basis (treaty vs. CIL) than with the practical ability to move goods efficiently across oceans.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, international_shipping_industry, beneficiary,
    organized, biographical, mobile, global).

% States that have ratified UNCLOS and generally adhere to its provisions. They observe the contest between major naval powers and coastal states, as the outcome affects the stability and interpretation of the very treaty they uphold. Their position is often to advocate for UNCLOS as the primary legal framework, but they may also benefit from CIL interpretations that align with their own interests.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_ratifying_states, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for the movement of vessels across international waters, ensuring predictable transit routes and minimizing conflicts over maritime jurisdiction, particularly in areas claimed as EEZs.
% TRANSFER_FUNCTION: Transfers the right to conduct certain activities (e.g., military exercises, intelligence gathering) in EEZs from coastal states to major naval powers, effectively limiting coastal state sovereignty claims in favor of broader international access.
% ABSENT_VOICES: Small island developing states and landlocked states, whose interests in maritime resource management and transit rights are often marginalized in disputes between major naval powers and larger coastal states. They would advocate for clearer, more equitable, and universally enforceable maritime boundaries.
% DISAPPEARANCE_RATIONALE: If the principles of freedom of navigation, whether customary or treaty-based, vanished overnight, maritime commerce and military operations would descend into chaos. Every coastal state would assert maximalist claims, leading to frequent confrontations, blockades, and a collapse of global shipping and naval mobility. The world's oceans would become a patchwork of contested zones.
% FOUNDING_PROBLEM: Historically, the lack of clear international consensus on maritime jurisdiction beyond territorial seas led to frequent disputes over fishing rights, naval access, and resource exploration, hindering global trade and increasing geopolitical tensions.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, maritime historians, and naval strategists widely corroborate the historical problem of undefined maritime jurisdiction. The ongoing disputes over EEZ rights and freedom of navigation, as evidenced by diplomatic protests and naval incidents, confirm the problem remains live, albeit in a more refined form than pre-UNCLOS eras.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because coastal states lose effective control over significant portions of their claimed maritime zones, impacting resource management and security. Suppression (0.75) is high due to the overwhelming naval power of the enforcing states, which effectively negates the ability of most coastal states to challenge these operations. Theater ratio (0.20) is relatively low, as the naval presence is a genuine enforcement mechanism, not merely performative. The claimed type is 'tangled_rope' because it provides a coordination function (predictable global transit) but simultaneously extracts sovereignty from coastal states through asymmetric enforcement.
 *
 * PERSPECTIVAL GAP:
 *   Major naval powers perceive this as a 'rope' – a necessary coordination mechanism for global security and commerce. Coastal states, however, experience it as a 'snare' or 'tangled_rope,' where their sovereign rights are curtailed by the superior force of others. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Major naval powers are clear beneficiaries and agenda-setters, as they define and enforce the CIL interpretation that grants them operational freedom (low d). Coastal states asserting exclusive EEZ rights are victims, bearing the costs of diminished sovereignty and control (high d). The international shipping industry benefits from the stability and freedom of transit, aligning with the naval powers' interpretation (low d). UNCLOS ratifying states act as observers, their directionality varying based on their specific interests and capacity to enforce their own interpretations.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate, to ensure freedom of navigation, remains live. However, the 'non-ratifier enforcement' reading layers an extractive mechanism onto this mandate. The classification as a 'tangled_rope' prevents mislabeling it as pure coordination (a 'rope') by highlighting the asymmetric extraction from coastal states, while also acknowledging its genuine, albeit contested, coordination function. It avoids a 'snare' classification by recognizing the CIL basis as a form of coordination, even if coercively applied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_law_scope_ambiguity,
    'To what extent are specific freedom of navigation principles truly customary international law, universally binding, versus specific interpretations advanced by powerful states?',
    'Analysis of state practice and opinio juris (state belief that a practice is legally obligatory) from a broader range of states, particularly those not aligned with major naval powers, over an extended period.',
    'If the CIL basis is weaker than asserted, the constraint shifts closer to a pure ''snare'' for coastal states, as the coordination justification diminishes. If robust, it reinforces the ''tangled_rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_scope_ambiguity, empirical, 'Ambiguity regarding the universal acceptance and scope of CIL freedom of navigation principles.').

omega_variable(
    eez_sovereignty_interpretation_divergence,
    'Is the UNCLOS framework for EEZs inherently ambiguous regarding military activities, or is the ''non-ratifier enforcement'' reading a deliberate reinterpretation to serve specific state interests?',
    'Detailed legal analysis of UNCLOS text, preparatory works, and subsequent state practice, focusing on the intent behind EEZ provisions concerning military operations. Comparison with interpretations from international tribunals.',
    'If UNCLOS is clear, this reading is a ''snare'' for coastal states, as it overrides established treaty law. If genuinely ambiguous, it remains a ''tangled_rope'' reflecting a contestable but plausible interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eez_sovereignty_interpretation_divergence, conceptual, 'Divergence in interpretation of EEZ rights under UNCLOS and CIL.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(uncl_tr_t1995, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(uncl_tr_t2005, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2005, 0.18).
narrative_ontology:measurement(uncl_tr_t2015, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2015, 0.19).
narrative_ontology:measurement(uncl_tr_t2024, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 1982, 0.5).
narrative_ontology:measurement(uncl_be_t1995, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(uncl_be_t2005, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(uncl_be_t2015, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2015, 0.63).
narrative_ontology:measurement(uncl_be_t2024, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 1982, 0.6).
narrative_ontology:measurement(uncl_su_t1995, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 1995, 0.65).
narrative_ontology:measurement(uncl_su_t2005, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(uncl_su_t2015, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2015, 0.73).
narrative_ontology:measurement(uncl_su_t2024, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary__strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary__historical_rights_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, global_maritime_trade_routes).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'unclos_sovereignty_boundary' kernel, focusing on the assertion of customary international law for freedom of navigation by non-ratifying (or selectively applying) naval powers. It directly influences and is influenced by other readings of the same kernel, particularly the 'strict_eez_reading' and 'historical_rights_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
