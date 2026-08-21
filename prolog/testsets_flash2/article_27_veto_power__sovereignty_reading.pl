% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__sovereignty_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: article_27_veto_power__sovereignty_reading
 *   human_readable: UNSC Article 27 Veto Power (Sovereignty Reading)
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   This constraint models the UN Security Council's Article 27 veto power as
 *   an instantiation of the Westphalian principle of state sovereignty,
 *   specifically applied to great powers with global enforcement capacity.
 *   From this reading, the veto is not a privilege but a structural
 *   inevitability reflecting the physical reality that no state can be bound
 *   by international law without its consent, especially when it possesses
 *   the means to resist. It is classified as a Mountain because its
 *   persistence derives from the irreducible distribution of power among
 *   states, not from an actively maintained human construct. The low
 *   extractiveness reflects that it merely formalizes an existing power
 *   dynamic, rather than creating new extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__sovereignty_reading, 0.05).
domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, 0.95).
domain_priors:theater_ratio(article_27_veto_power__sovereignty_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__sovereignty_reading, mountain).
narrative_ontology:human_readable(article_27_veto_power__sovereignty_reading, "UNSC Article 27 Veto Power (Sovereignty Reading)").
narrative_ontology:topic_domain(article_27_veto_power__sovereignty_reading, "international_relations/institutional_design/constitutional_law").

domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__sovereignty_reading, 'f2970bba-6350-4a8c-8a29-be1cc0c71971').
narrative_ontology:cs_kernel_codification('f2970bba-6350-4a8c-8a29-be1cc0c71971', fixed_text).
narrative_ontology:cs_authority_grounding('f2970bba-6350-4a8c-8a29-be1cc0c71971', lineage).
narrative_ontology:cs_interpretation_layer_present('f2970bba-6350-4a8c-8a29-be1cc0c71971').
narrative_ontology:cs_reading_relation('f2970bba-6350-4a8c-8a29-be1cc0c71971', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('f2970bba-6350-4a8c-8a29-be1cc0c71971', article_27_veto_power__oligopoly_reading, coexists_with).
narrative_ontology:cs_axiom('f2970bba-6350-4a8c-8a29-be1cc0c71971', foundational, state_sovereignty_is_absolute).
narrative_ontology:cs_axiom_status(state_sovereignty_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('f2970bba-6350-4a8c-8a29-be1cc0c71971', state_sovereignty_is_absolute, deontological).
narrative_ontology:cs_axiom('f2970bba-6350-4a8c-8a29-be1cc0c71971', foundational, power_distribution_is_irreducible).
narrative_ontology:cs_axiom_status(power_distribution_is_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('f2970bba-6350-4a8c-8a29-be1cc0c71971', power_distribution_is_irreducible, empirically_contingent).
narrative_ontology:cs_reference_frame('f2970bba-6350-4a8c-8a29-be1cc0c71971', westphalian_system_of_states).
narrative_ontology:cs_drift_state('f2970bba-6350-4a8c-8a29-be1cc0c71971', contemporary_global_challenges, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('f2970bba-6350-4a8c-8a29-be1cc0c71971', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__sovereignty_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__sovereignty_reading, p5_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article_27_veto_power__sovereignty_reading, non_p5_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As permanent members of the UN Security Council, these states possess the veto power, which ensures no resolution can be passed against their vital interests. This reading sees their position as a reflection of their inherent sovereign power and global enforcement capacity, not a privilege granted by the UN Charter.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, p5_states, beneficiary,
    institutional, generational, arbitrage, global).

% These states are subject to international law and Security Council resolutions, but lack the power to block actions that may go against their interests if a P5 state supports them. They experience the veto as a limit on collective action and a source of frustration, but this reading attributes it to the structural reality of power.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, non_p5_states, payer,
    moderate, biographical, constrained, global).

% Analyze the legal and political implications of the veto power. This reading emphasizes the historical and philosophical roots of state sovereignty as the ultimate constraint on international governance, viewing the veto as a necessary consequence of the distribution of power.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, international_legal_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that international law, as expressed through the Security Council, does not compel great powers into actions that violate their fundamental sovereignty, thereby preventing direct confrontation among them.
% TRANSFER_FUNCTION: Prevents the transfer of sovereign decision-making authority from P5 states to the collective international body, preserving their unilateral capacity to act on matters of vital national interest.
% ABSENT_VOICES: States advocating for a more equitable distribution of power in international institutions, or for a system where international law can bind all states equally, are present in the General Assembly but lack the structural power to alter the veto mechanism.
% DISAPPEARANCE_RATIONALE: If the veto power vanished overnight, the international system would immediately face a crisis of legitimacy and enforcement. Great powers would likely withdraw from or disregard the Security Council, leading to a breakdown of collective security mechanisms and a return to more overt power politics, as the underlying reality of their sovereign capacity would reassert itself outside the UN framework.
% FOUNDING_PROBLEM: The problem of how to create an international security organization that could effectively address global threats without infringing upon the fundamental sovereignty of the most powerful states, particularly those with nuclear weapons, whose non-participation would render any such organization ineffective or dangerous.
% FOUNDING_PROBLEM_CORROBORATION: P5 states consistently attest that the founding problem remains live, citing the continued necessity of safeguarding national interests against external compulsion. Independent international relations scholars and historians corroborate that the veto was a pragmatic necessity reflecting the post-WWII distribution of power, and that any attempt to remove it without addressing the underlying power dynamics would lead to institutional collapse rather than enhanced cooperation.
narrative_ontology:disappearance_verdict(article_27_veto_power__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(article_27_veto_power__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__sovereignty_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__sovereignty_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, ExtMetricName, E),
    domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(article_27_veto_power__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is near-zero (0.05) because this reading views the veto as a reflection of an underlying, unchangeable reality of state power, not a mechanism for rent extraction. Any global institution empowered to compel great-power action would face the same coordination failure or collapse. Suppression is high (0.95) because the veto effectively suppresses any attempt to bind P5 states against their will, a 'natural' consequence of their sovereign power. Theater ratio is low (0.05) as there is little performative maintenance; the veto's function is direct and structural. Accessibility collapse is near-total (0.98) because alternatives to this power distribution are seen as non-existent or immediately self-defeating. Resistance is minimal (0.02) because, from this perspective, resistance is futile against a structural reality.
 *
 * PERSPECTIVAL GAP:
 *   Other readings (coordination, oligopoly) would classify the veto differently, seeing it as a constructed mechanism with higher extractiveness or a coordination tool. This sovereignty reading, however, asserts the veto's Mountain-like nature, arguing that its function is to prevent the international system from attempting to enforce rules that would inevitably be resisted by powerful sovereign states, leading to greater instability. The engine's classification will highlight the divergence between this claimed Mountain and other readings' classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   P5 states are beneficiaries in the sense that the veto formalizes their existing sovereign power, but this reading frames it as a structural given rather than a 'benefit' they actively extract. Non-P5 states are 'payers' in that they bear the costs of limited collective action, but this is seen as an unavoidable consequence of the international system's structure. The directionality for P5 states is thus near 0.0 (full beneficiary) and for non-P5 states near 1.0 (full target), but this is a reflection of their structural position relative to an immutable reality.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    is_veto_a_structural_inevitability,
    'Is the P5 veto power a structural inevitability given the distribution of global power and the nature of state sovereignty, or is it a contingent institutional design choice?',
    'Counterfactual historical analysis: could a UN Charter have been designed and sustained without a P5 veto, given the geopolitical realities of 1945 and beyond? Empirical observation of alternative international institutions'' ability to bind great powers.',
    'If a structural inevitability, the Mountain classification is robust. If a contingent choice, the constraint is more likely a Snare or Tangled Rope, with higher extractiveness and identifiable beneficiaries/victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(is_veto_a_structural_inevitability, conceptual, 'Ambiguity between structural inevitability and institutional design choice.').

omega_variable(
    sovereignty_vs_collective_security,
    'To what extent does the principle of absolute state sovereignty (as embodied by the veto) fundamentally conflict with the principle of collective security, and which principle is ultimately ''mountain-like'' in international relations?',
    'Philosophical and legal analysis of the foundations of international law, combined with empirical study of state behavior in crises where these principles clash. Resolution depends on which foundational principle is deemed more irreducible.',
    'If sovereignty is the ultimate Mountain, the veto is a necessary reflection. If collective security is achievable without such a veto, the veto becomes an extractive mechanism preventing a more optimal coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_collective_security, preference, 'Conceptual tension between sovereignty and collective security as foundational principles.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__sovereignty_reading, 0, 79).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_27_veto_power__sovereignty_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(arti_tr_t15, article_27_veto_power__sovereignty_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement(arti_tr_t30, article_27_veto_power__sovereignty_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(arti_tr_t45, article_27_veto_power__sovereignty_reading, theater_ratio, 45, 0.05).
narrative_ontology:measurement(arti_tr_t60, article_27_veto_power__sovereignty_reading, theater_ratio, 60, 0.05).
narrative_ontology:measurement(arti_tr_t79, article_27_veto_power__sovereignty_reading, theater_ratio, 79, 0.05).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_27_veto_power__sovereignty_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(arti_be_t15, article_27_veto_power__sovereignty_reading, base_extractiveness, 15, 0.05).
narrative_ontology:measurement(arti_be_t30, article_27_veto_power__sovereignty_reading, base_extractiveness, 30, 0.05).
narrative_ontology:measurement(arti_be_t45, article_27_veto_power__sovereignty_reading, base_extractiveness, 45, 0.05).
narrative_ontology:measurement(arti_be_t60, article_27_veto_power__sovereignty_reading, base_extractiveness, 60, 0.05).
narrative_ontology:measurement(arti_be_t79, article_27_veto_power__sovereignty_reading, base_extractiveness, 79, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_27_veto_power__sovereignty_reading, suppression_requirement, 0, 0.95).
narrative_ontology:measurement(arti_su_t15, article_27_veto_power__sovereignty_reading, suppression_requirement, 15, 0.95).
narrative_ontology:measurement(arti_su_t30, article_27_veto_power__sovereignty_reading, suppression_requirement, 30, 0.95).
narrative_ontology:measurement(arti_su_t45, article_27_veto_power__sovereignty_reading, suppression_requirement, 45, 0.95).
narrative_ontology:measurement(arti_su_t60, article_27_veto_power__sovereignty_reading, suppression_requirement, 60, 0.95).
narrative_ontology:measurement(arti_su_t79, article_27_veto_power__sovereignty_reading, suppression_requirement, 79, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__coordination_reading).
narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__oligopoly_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the UN Security Council's Article 27 veto power. This 'sovereignty_reading' emphasizes the veto as a reflection of inherent state power, distinct from the 'coordination_reading' (veto as war prevention) and the 'oligopoly_reading' (veto as power entrenchment).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
