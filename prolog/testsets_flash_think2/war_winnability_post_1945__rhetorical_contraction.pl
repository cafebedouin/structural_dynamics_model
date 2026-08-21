% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__rhetorical_contraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__rhetorical_contraction, []).

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
 *   constraint_id: war_winnability_post_1945__rhetorical_contraction
 *   human_readable: Nuclear War Winnability: Rhetorical Contraction
 *   domain: Strategic Studies / Nuclear Deterrence Theory / International Relations
 *
 * SUMMARY:
 *   This constraint describes the post-1945 phenomenon where the concept of
 *   'winnability' in nuclear war became a rhetorical taboo in public
 *   discourse, while remaining an active, albeit constrained, area of
 *   operational planning within classified strategic circles. The constraint
 *   is presented as a 'rope' (coordinating deterrence stability) by its
 *   beneficiaries, but its high extractiveness and suppression metrics
 *   reflect its actual operation as a 'snare' or 'tangled_rope' from the
 *   perspective of democratic oversight and the public. The divergence
 *   between the claimed type and the descriptive metrics is intentional, as
 *   the engine measures this gap.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, 0.78).
domain_priors:suppression_score(war_winnability_post_1945__rhetorical_contraction, 0.85).
domain_priors:theater_ratio(war_winnability_post_1945__rhetorical_contraction, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, extractiveness, 0.78).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__rhetorical_contraction, rope).
narrative_ontology:human_readable(war_winnability_post_1945__rhetorical_contraction, "Nuclear War Winnability: Rhetorical Contraction").
narrative_ontology:topic_domain(war_winnability_post_1945__rhetorical_contraction, "Strategic Studies / Nuclear Deterrence Theory / International Relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__rhetorical_contraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__rhetorical_contraction, '634c7510-ec03-4635-a2f1-a26e8332cb5e').
narrative_ontology:cs_kernel_codification('634c7510-ec03-4635-a2f1-a26e8332cb5e', formalized).
narrative_ontology:cs_authority_grounding('634c7510-ec03-4635-a2f1-a26e8332cb5e', extraction).
narrative_ontology:cs_interpretation_layer_present('634c7510-ec03-4635-a2f1-a26e8332cb5e').
narrative_ontology:cs_reading_relation('634c7510-ec03-4635-a2f1-a26e8332cb5e', war_winnability_post_1945__deterrence_unthinkable, influences).
narrative_ontology:cs_reading_relation('634c7510-ec03-4635-a2f1-a26e8332cb5e', war_winnability_post_1945__countervailing_thinkable, influences).
narrative_ontology:cs_axiom('634c7510-ec03-4635-a2f1-a26e8332cb5e', foundational, nuclear_war_is_unwinnable_rhetorically).
narrative_ontology:cs_axiom_status(nuclear_war_is_unwinnable_rhetorically, holdable).
narrative_ontology:cs_axiom_grounding('634c7510-ec03-4635-a2f1-a26e8332cb5e', nuclear_war_is_unwinnable_rhetorically, conventional).
narrative_ontology:cs_axiom('634c7510-ec03-4635-a2f1-a26e8332cb5e', secondary, operational_planning_for_victory_is_necessary).
narrative_ontology:cs_axiom_status(operational_planning_for_victory_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('634c7510-ec03-4635-a2f1-a26e8332cb5e', operational_planning_for_victory_is_necessary, instrumental).
narrative_ontology:cs_reference_frame('634c7510-ec03-4635-a2f1-a26e8332cb5e', post_hiroshima_deterrence_consensus).
narrative_ontology:cs_drift_state('634c7510-ec03-4635-a2f1-a26e8332cb5e', contemporary_strategic_environment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('634c7510-ec03-4635-a2f1-a26e8332cb5e', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, strategic_planners).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, political_leaders).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, democratic_public).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, democratic_oversight_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for developing and maintaining nuclear war plans, they benefit from the rhetorical taboo as it allows them operational flexibility and reduces public scrutiny of potentially controversial strategies. Their exit is constrained by national security mandates.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, strategic_planners, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the public perception of nuclear war as unthinkable, which helps maintain social stability and legitimizes deterrence policies without requiring them to publicly reconcile the contradiction with operational planning. They can shift rhetoric as needed.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, political_leaders, beneficiary,
    powerful, biographical, mobile, national).

% Bears the cost of ignorance and lack of agency regarding nuclear war planning. They are trapped by the rhetorical taboo, which limits their ability to engage in informed debate or demand accountability for strategic decisions.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, democratic_public, payer,
    powerless, biographical, trapped, global).

% Legislative bodies and watchdog organizations that are nominally responsible for scrutinizing strategic planning. They are constrained by classification barriers and the political sensitivity of challenging the rhetorical taboo, limiting their effective oversight.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, democratic_oversight_institutions, payer,
    organized, biographical, constrained, national).

% Scholars and analysts who critically examine nuclear strategy and deterrence theory. They observe and document the rhetorical-operational gap but often lack the power to directly influence policy or break the public taboo.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, academic_critics, observer,
    moderate, biographical, mobile, global).

% Advocates for disarmament or alternative security paradigms. Their arguments are often marginalized or dismissed within the dominant discourse, as they challenge the very premises maintained by the rhetorical contraction.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, anti_nuclear_activists, excluded,
    organized, immediate, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_winnability_post_1945__rhetorical_contraction, strategic_planners).
narrative_ontology:fixing_cost_class(war_winnability_post_1945__rhetorical_contraction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains public perception of nuclear deterrence stability by framing nuclear war as categorically unwinnable, thereby discouraging escalation and managing public anxiety.
% TRANSFER_FUNCTION: Transfers public accountability and informed consent from the democratic public to strategic planners and political leaders, in exchange for perceived stability and reduced public pressure on nuclear policy.
% ABSENT_VOICES: Anti-nuclear activists and critical academics who challenge the coherence of deterrence theory or the necessity of operational planning for winnability are structurally marginalized; their arguments are often deemed irresponsible or naive within the dominant discourse.
% DISAPPEARANCE_RATIONALE: If the rhetorical taboo vanished, public debate on nuclear strategy would intensify, potentially leading to demands for greater transparency, changes in doctrine, or even disarmament. This would fundamentally alter the political landscape of nuclear deterrence and force a public reckoning with operational realities.
% FOUNDING_PROBLEM: After Hiroshima and Nagasaki, the existential threat of nuclear war created a need to manage public fear and maintain deterrence stability, while still preparing for potential conflict and maintaining a credible threat.
% FOUNDING_PROBLEM_CORROBORATION: Strategic documents and public statements from the Cold War era corroborate the initial framing. Academic analyses and declassified documents from outside the immediate planning circles attest to the persistence of operational planning despite the public rhetoric, indicating the problem of reconciling these two realities remains live.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__rhetorical_contraction, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__rhetorical_contraction, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__rhetorical_contraction, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(war_winnability_post_1945__rhetorical_contraction, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__rhetorical_contraction, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__rhetorical_contraction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__rhetorical_contraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the public is denied full information and agency regarding nuclear strategy, while democratic oversight is undermined. Suppression is very high due to the active maintenance of the rhetorical taboo through classification, political messaging, and marginalization of dissenting voices. The theater ratio is high because the public rhetoric of 'unwinnability' is largely performative, masking the underlying reality of continuous operational planning for various conflict scenarios. Accessibility collapse is significant as the taboo makes it difficult to access alternative narratives or challenge the official line. Resistance is low because the taboo is widely accepted, and challenging it is often framed as irresponsible.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of strategic planners and political leaders, this constraint is a necessary 'rope' for maintaining deterrence and managing public fear in the nuclear age. From the perspective of the democratic public and oversight institutions, it operates as a 'snare' or 'tangled_rope,' extracting accountability and suppressing informed debate under the guise of stability.
 *
 * DIRECTIONALITY LOGIC:
 *   Strategic planners and political leaders are the primary beneficiaries, gaining operational flexibility and public stability without accountability. The democratic public and oversight institutions are the victims, bearing the costs of limited information and reduced agency. Academic critics observe this dynamic, while anti-nuclear activists are excluded from the mainstream discourse.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rhetorical_operational_gap_necessity,
    'Is the gap between public rhetoric and operational planning regarding nuclear war winnability a necessary evil for deterrence stability, or an unnecessary extraction of democratic accountability?',
    'Comparative analysis of nuclear postures in states with varying levels of transparency and public debate, assessing their impact on deterrence stability and democratic health.',
    'If necessary, the extraction is a ''cost of coordination'' (Tangled Rope); if unnecessary, it is pure extraction (Snare), and the constraint''s legitimacy is severely undermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rhetorical_operational_gap_necessity, conceptual, 'Ambiguity regarding the functional necessity of the rhetorical-operational gap.').

omega_variable(
    deterrence_stability_mechanism,
    'Does the rhetorical taboo genuinely enhance deterrence stability by managing public perception, or does it merely obscure the risks and moral hazards of continuous operational planning for nuclear war?',
    'Historical analysis of near-miss incidents and crisis escalation dynamics, alongside expert testimony on the psychological effects of public narratives on decision-makers.',
    'If it genuinely enhances stability, the coordination function is stronger; if it merely obscures risks, the coordination function is weaker, and the constraint leans more towards pure extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_stability_mechanism, empirical, 'Uncertainty about the actual impact of the rhetorical taboo on deterrence stability.').

omega_variable(
    public_ignorance_cost,
    'What is the true cost of public ignorance regarding nuclear war planning, in terms of democratic accountability, potential for miscalculation, and the erosion of public trust?',
    'Longitudinal studies on public attitudes towards nuclear weapons, analysis of policy decisions made without full public input, and ethical frameworks for democratic governance in existential risk scenarios.',
    'A high cost would amplify the constraint''s extractiveness and suppression, pushing it further towards a Snare; a low cost would suggest the public is less impacted than assumed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_ignorance_cost, preference, 'The societal cost of maintaining public ignorance about nuclear war planning.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__rhetorical_contraction, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1945, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1945, 0.3).
narrative_ontology:measurement(war__tr_t1965, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1965, 0.5).
narrative_ontology:measurement(war__tr_t1985, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1985, 0.6).
narrative_ontology:measurement(war__tr_t2005, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2005, 0.63).
narrative_ontology:measurement(war__tr_t2025, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2025, 0.65).

% Extraction over time
narrative_ontology:measurement(war__be_t1945, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1945, 0.5).
narrative_ontology:measurement(war__be_t1965, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1965, 0.65).
narrative_ontology:measurement(war__be_t1985, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1985, 0.75).
narrative_ontology:measurement(war__be_t2005, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2005, 0.77).
narrative_ontology:measurement(war__be_t2025, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1945, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1945, 0.7).
narrative_ontology:measurement(war__su_t1965, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1965, 0.8).
narrative_ontology:measurement(war__su_t1985, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1985, 0.85).
narrative_ontology:measurement(war__su_t2005, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2005, 0.84).
narrative_ontology:measurement(war__su_t2025, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__rhetorical_contraction, identity_coordination).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, deterrence_stability_doctrine).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, nuclear_arms_control_regimes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
