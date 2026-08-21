% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__positivist_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: us_constitution_1787__positivist_reading
 *   human_readable: U.S. Constitution (Positivist Reading)
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'positivist reading' of the U.S.
 *   Constitution, which holds that constitutional meaning is derived
 *   primarily from the plain text of the document, supplemented by
 *   democratically enacted amendments. Judicial interpretation is seen as
 *   constrained to the text, limiting judicial activism and emphasizing the
 *   formal amendment process as the legitimate means of fundamental legal
 *   change. This reading contrasts with originalist (framers' intent) and
 *   living (evolving societal values) interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__positivist_reading, 0.35).
domain_priors:suppression_score(us_constitution_1787__positivist_reading, 0.55).
domain_priors:theater_ratio(us_constitution_1787__positivist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__positivist_reading, rope).
narrative_ontology:human_readable(us_constitution_1787__positivist_reading, "U.S. Constitution (Positivist Reading)").
narrative_ontology:topic_domain(us_constitution_1787__positivist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__positivist_reading, '83947eef-2158-4ce7-96bc-0fa70b8290c3').
narrative_ontology:cs_kernel_codification('83947eef-2158-4ce7-96bc-0fa70b8290c3', fixed_text).
narrative_ontology:cs_authority_grounding('83947eef-2158-4ce7-96bc-0fa70b8290c3', lineage).
narrative_ontology:cs_interpretation_layer_present('83947eef-2158-4ce7-96bc-0fa70b8290c3').
narrative_ontology:cs_reading_relation('83947eef-2158-4ce7-96bc-0fa70b8290c3', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('83947eef-2158-4ce7-96bc-0fa70b8290c3', us_constitution_1787__living_reading, coexists_with).
narrative_ontology:cs_axiom('83947eef-2158-4ce7-96bc-0fa70b8290c3', foundational, constitutional_text_is_supreme).
narrative_ontology:cs_axiom_status(constitutional_text_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('83947eef-2158-4ce7-96bc-0fa70b8290c3', constitutional_text_is_supreme, conventional).
narrative_ontology:cs_axiom('83947eef-2158-4ce7-96bc-0fa70b8290c3', foundational, amendment_is_sole_legitimate_change).
narrative_ontology:cs_axiom_status(amendment_is_sole_legitimate_change, holdable).
narrative_ontology:cs_axiom_grounding('83947eef-2158-4ce7-96bc-0fa70b8290c3', amendment_is_sole_legitimate_change, conventional).
narrative_ontology:cs_reference_frame('83947eef-2158-4ce7-96bc-0fa70b8290c3', rule_of_law_textual_supremacy).
narrative_ontology:cs_drift_state('83947eef-2158-4ce7-96bc-0fa70b8290c3', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('83947eef-2158-4ce7-96bc-0fa70b8290c3', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__positivist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, citizens).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, judicial_system).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, legislature).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, executive_branch).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, judicial_activists).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, fringe_political_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, citizens).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, executive_branch).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the stability and predictability of a text-bound legal framework, and have a formal avenue for democratic input through the amendment process. They bear the costs of abiding by the laws and participating in the political process.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, citizens, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__positivist_reading, citizens, payer).

% Interprets the constitutional text and enforces its limits on other branches and on itself. Gains legitimacy and authority from adhering to the text and the established amendment process. Its professional identity is bound by this interpretive approach.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, judicial_system, agenda_setter,
    institutional, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__positivist_reading, judicial_system, beneficiary).

% Operates within the framework provided by the constitutional text, which defines its powers and limits. It is the primary body for proposing and ratifying amendments, making it central to constitutional change under this reading.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, legislature, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__positivist_reading, legislature, agenda_setter).

% Executes laws within the powers and limits defined by the constitutional text. Benefits from the clear delineation of authority but is constrained by it.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, executive_branch, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__positivist_reading, executive_branch, payer).

% Their preferred method of constitutional evolution through broad judicial interpretation is constrained by this reading's emphasis on the text and formal amendments. Their professional identity as judges or legal scholars is challenged by these limits.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, judicial_activists, payer,
    powerful, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__positivist_reading, judicial_activists, excluded).

% Seek fundamental changes to the political system that often fall outside the established constitutional text and amendment process. They are constrained by the legal framework and excluded from legitimate avenues of change within it.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, fringe_political_movements, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__positivist_reading, fringe_political_movements, excluded).

% Analyze and critique the constitutional text, its interpretations, and the amendment process. They provide academic commentary and influence legal discourse but do not directly enforce or change the constraint.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_1787__positivist_reading, diffuse).
narrative_ontology:fixing_cost_class(us_constitution_1787__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, authoritative framework for national governance by defining governmental powers, individual rights, and a structured process for fundamental legal change, ensuring legal predictability and continuity.
% TRANSFER_FUNCTION: Transfers the authority for fundamental legal change from unconstrained judicial or popular will to a text-bound interpretive process and a formal, supermajoritarian amendment procedure.
% ABSENT_VOICES: Those who believe constitutional meaning is purely a matter of evolving social consensus (living constitutionalists) or those who advocate for revolutionary change outside the established legal framework would object to the strict textual and procedural limits.
% DISAPPEARANCE_RATIONALE: If the U.S. Constitution, understood as a text-bound and democratically amendable document, vanished overnight, the entire legal and political system would lose its foundational legitimacy and structure. This would lead to widespread instability, a collapse of governmental authority, and a complete reorganization of governance, likely into competing factions.
% FOUNDING_PROBLEM: To establish a durable, legitimate framework for a federal republic that balances governmental power with individual rights, and provides a clear, stable process for its own evolution, preventing both tyranny and anarchy.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, political scientists, and international observers generally corroborate the ongoing need for a stable constitutional framework to maintain a functioning republic, even while debating the specifics of its interpretation and amendment.
narrative_ontology:disappearance_verdict(us_constitution_1787__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__positivist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__positivist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(us_constitution_1787__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__positivist_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__positivist_reading_tests).
:- end_tests(us_constitution_1787__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is set at a moderate 0.35, reflecting that while the positivist reading aims for stable governance (a coordination function), its strict textual limits can impose costs on those seeking more flexible or rapid constitutional evolution. Suppression is 0.55, as active judicial enforcement is required to maintain the textual boundaries and resist alternative interpretive methods. The theater ratio is low (0.15) because the textual and amendment processes are generally functional, not merely performative. Accessibility collapse is 0.6, as it significantly limits non-textual avenues for fundamental legal change, and resistance is 0.3, as the framework is broadly accepted, though specific applications are debated.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 'judicial system' and 'legislature', this reading provides a clear, legitimate framework for their operations, appearing as a 'Rope' that coordinates governance. However, 'judicial activists' and 'fringe_political_movements' experience it as more extractive, as it suppresses their preferred avenues for change, potentially computing as a 'Tangled Rope' or even 'Snare' from their seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'judicial system', 'legislature', 'executive_branch', and 'citizens' are beneficiaries, gaining stability and legitimacy from the clear framework. 'Judicial activists' and 'fringe_political_movements' are victims, as their preferred methods of constitutional change or political action are constrained or excluded by the textual and procedural limits of this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of providing stable, legitimate governance and a structured process for change remains live. The positivist reading seeks to prevent mandatrophy by anchoring constitutional meaning in a fixed text and a formal amendment process, thereby resisting interpretations that might drift from its original function. However, the difficulty of the amendment process can create pressure for other forms of change, leading to contestation over whether the constraint's function has shifted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_meaning_ambiguity,
    'What constitutes ''what the text says'' in practice? Is it plain meaning, original public meaning, or a historically informed plain meaning?',
    'Analysis of judicial precedent and legal scholarship to identify the dominant interpretive methodology employed by adherents of this reading.',
    'If ''plain meaning'' is consistently interpreted through a lens akin to original public meaning, the positivist reading might structurally converge with aspects of originalism, altering its distinctiveness and potentially its extractiveness for those seeking more contemporary interpretations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_meaning_ambiguity, conceptual, 'Ambiguity in the practical application of ''textual'' interpretation.').

omega_variable(
    amendment_process_efficacy,
    'Is the formal amendment process truly a viable and democratic mechanism for constitutional change, or is its difficulty so prohibitive that it effectively locks in existing structures?',
    'Empirical analysis of amendment proposals, ratification rates, and the political feasibility of achieving supermajority consensus in contemporary political conditions.',
    'If the amendment process is found to be effectively non-functional, the positivist reading''s claim to democratic legitimacy for constitutional change would be undermined, potentially increasing its effective extractiveness for citizens and justifying alternative (non-textual) interpretive pressures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_process_efficacy, empirical, 'Effectiveness of the amendment process as a democratic mechanism.').

omega_variable(
    judicial_deference_consistency,
    'Does the judiciary consistently adhere to textual constraints, or does it engage in ''covert activism'' by subtly reinterpreting the text to achieve desired policy outcomes?',
    'Longitudinal study of judicial decisions, comparing outcomes with strict textual interpretations and identifying patterns of deviation or expansive readings under the guise of textual fidelity.',
    'If covert activism is widespread, the actual operation of the constraint would be more extractive than claimed, as judicial power would be exercised beyond its declared textual limits, potentially reclassifying the constraint from a ''Rope'' to a ''Tangled Rope'' from the perspective of those whose interests are affected by such interpretations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_deference_consistency, empirical, 'Consistency of judicial deference to textual limits.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__positivist_reading, 1787, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1787, us_constitution_1787__positivist_reading, theater_ratio, 1787, 0.15).
narrative_ontology:measurement(us_c_tr_t1850, us_constitution_1787__positivist_reading, theater_ratio, 1850, 0.15).
narrative_ontology:measurement(us_c_tr_t1900, us_constitution_1787__positivist_reading, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(us_c_tr_t1950, us_constitution_1787__positivist_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_1787__positivist_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_1787__positivist_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1787, us_constitution_1787__positivist_reading, base_extractiveness, 1787, 0.25).
narrative_ontology:measurement(us_c_be_t1850, us_constitution_1787__positivist_reading, base_extractiveness, 1850, 0.28).
narrative_ontology:measurement(us_c_be_t1900, us_constitution_1787__positivist_reading, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement(us_c_be_t1950, us_constitution_1787__positivist_reading, base_extractiveness, 1950, 0.32).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_1787__positivist_reading, base_extractiveness, 2000, 0.34).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_1787__positivist_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1787, us_constitution_1787__positivist_reading, suppression_requirement, 1787, 0.4).
narrative_ontology:measurement(us_c_su_t1850, us_constitution_1787__positivist_reading, suppression_requirement, 1850, 0.45).
narrative_ontology:measurement(us_c_su_t1900, us_constitution_1787__positivist_reading, suppression_requirement, 1900, 0.48).
narrative_ontology:measurement(us_c_su_t1950, us_constitution_1787__positivist_reading, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_1787__positivist_reading, suppression_requirement, 2000, 0.53).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_1787__positivist_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__positivist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__living_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'us_constitution_1787' kernel. Each reading represents a different structural constraint, with differing ε values and stakeholder dynamics, and is modeled as a separate constraint story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
