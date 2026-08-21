% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: magna_carta_constraint_authority__parliamentary_sovereignty_reading
 *   human_readable: Magna Carta's Restraints under Parliamentary Sovereignty
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'parliamentary sovereignty'
 *   reading of Magna Carta's constraint authority. In this reading, Magna
 *   Carta's historical restraints on the Crown survive only insofar as they
 *   have been absorbed into parliamentary statute law. Parliament, as the
 *   supreme legislative body, inherits the authority to revise or repeal any
 *   such provision. This framework ensures legislative supremacy and
 *   democratic accountability but can leave minority rights vulnerable if not
 *   explicitly protected by statute. The constraint is claimed as a Rope by
 *   its beneficiaries (Parliament, majority electorate) but operates as a
 *   Tangled Rope due to its extractive potential over unprotected minorities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.65).
domain_priors:suppression_score(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.7).
domain_priors:theater_ratio(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "Magna Carta's Restraints under Parliamentary Sovereignty").
narrative_ontology:topic_domain(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "constitutional_history/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__parliamentary_sovereignty_reading, '3089c6e7-222c-4d09-9d0a-c4205017df03').
narrative_ontology:cs_kernel_codification('3089c6e7-222c-4d09-9d0a-c4205017df03', formalized).
narrative_ontology:cs_authority_grounding('3089c6e7-222c-4d09-9d0a-c4205017df03', lineage).
narrative_ontology:cs_interpretation_layer_present('3089c6e7-222c-4d09-9d0a-c4205017df03').
narrative_ontology:cs_reading_relation('3089c6e7-222c-4d09-9d0a-c4205017df03', magna_carta_constraint_authority__feudal_obsolescence_reading, influences).
narrative_ontology:cs_reading_relation('3089c6e7-222c-4d09-9d0a-c4205017df03', magna_carta_constraint_authority__living_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('3089c6e7-222c-4d09-9d0a-c4205017df03', foundational, parliamentary_supremacy).
narrative_ontology:cs_axiom_status(parliamentary_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('3089c6e7-222c-4d09-9d0a-c4205017df03', parliamentary_supremacy, conventional).
narrative_ontology:cs_axiom('3089c6e7-222c-4d09-9d0a-c4205017df03', foundational, statutory_absorption_of_charter_rights).
narrative_ontology:cs_axiom_status(statutory_absorption_of_charter_rights, holdable).
narrative_ontology:cs_axiom_grounding('3089c6e7-222c-4d09-9d0a-c4205017df03', statutory_absorption_of_charter_rights, conventional).
narrative_ontology:cs_reference_frame('3089c6e7-222c-4d09-9d0a-c4205017df03', post_glorious_revolution_settlement).
narrative_ontology:cs_drift_state('3089c6e7-222c-4d09-9d0a-c4205017df03', contemporary_human_rights_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('3089c6e7-222c-4d09-9d0a-c4205017df03', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliament).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, majority_electorate).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, minority_groups_unprotected_by_legislation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the supreme legislative body, Parliament inherits the authority to interpret, revise, or repeal any provision derived from Magna Carta. It sets the legal framework and benefits from the clarity of its sovereign power.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliament, agenda_setter,
    institutional, generational, mobile, national).

% Benefits from a clear, democratic process for law-making and revision, where their elected representatives hold ultimate authority. Their interests are generally reflected in legislation, though they may be constrained by specific laws.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, majority_electorate, beneficiary,
    organized, biographical, constrained, national).

% Bear the costs of parliamentary sovereignty when their rights or interests are not explicitly protected by statute, or when existing protections are repealed. They have limited recourse against parliamentary acts, as there is no higher constitutional court to appeal to.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, minority_groups_unprotected_by_legislation, payer,
    powerless, generational, trapped, national).

% Interprets and applies parliamentary statutes, including those that incorporate or derive from Magna Carta. While bound by parliamentary supremacy, the judiciary plays a role in shaping the practical application of these laws.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, judiciary, observer,
    institutional, generational, constrained, national).

% Analyze the historical evolution and contemporary implications of parliamentary sovereignty, including its relationship to historical documents like Magna Carta. They provide critical commentary and alternative framings.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, constitutional_scholars, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliament).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__parliamentary_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, revisable legal framework for governance, preventing arbitrary rule by the Crown and ensuring legislative supremacy, thereby coordinating the exercise of state power.
% TRANSFER_FUNCTION: Transfers ultimate legal authority from the Crown (and any notion of immutable higher law) to Parliament, allowing for the revision or repeal of any restraint. This can lead to a transfer of rights or protections away from minorities if not legislatively protected.
% ABSENT_VOICES: Advocates for entrenched constitutional rights or higher law principles that Parliament cannot easily override; they would argue for a more robust, judicially enforceable set of fundamental rights, but their arguments are often subordinated to the principle of parliamentary supremacy.
% DISAPPEARANCE_RATIONALE: If the principle of parliamentary sovereignty and the statutory absorption of historical constraints vanished overnight, the entire legal and political system of the UK would face a constitutional crisis. The source of all legal authority would be questioned, leading to profound reorganization of governance and rights.
% FOUNDING_PROBLEM: To establish a clear, supreme source of law and governance after centuries of conflict between Crown and Parliament, and to provide a flexible mechanism for legal evolution in response to changing societal needs.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, political scientists, and historical texts widely corroborate the historical problem of defining sovereign authority and the role of Parliament in the UK's constitutional development. The ongoing debates about the balance between parliamentary power and individual rights attest to its continued relevance.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__parliamentary_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__parliamentary_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_constraint_authority__parliamentary_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_constraint_authority__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.65) reflects the potential for Parliament to legislate in ways that disadvantage minorities, even if it generally acts for the common good. Suppression (0.70) is high because there are limited legal avenues to challenge parliamentary acts, reinforcing its supremacy. The theater ratio (0.10) is low, indicating that the system is functional and actively enforced, not merely performative. Accessibility collapse (0.60) is moderate, as alternatives to parliamentary law are limited, but political resistance and advocacy can still influence legislative outcomes. Resistance (0.20) is low, as parliamentary sovereignty is a deeply ingrained and widely accepted principle in the UK's constitutional order.
 *
 * PERSPECTIVAL GAP:
 *   From Parliament's perspective, this constraint is a legitimate and democratic mechanism for governance, ensuring the will of the people is supreme. From the perspective of unprotected minority groups, the same structure can appear highly extractive and suppressive, as their fundamental rights are contingent on legislative protection rather than being entrenched by a higher law. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Parliament and the majority electorate are the primary beneficiaries, as they control the legislative agenda and benefit from a clear, supreme source of law. Minority groups unprotected by specific legislation are the primary victims, as their rights can be curtailed or ignored without higher constitutional recourse. The judiciary and constitutional scholars act as observers, interpreting and analyzing the system without directly controlling its legislative output.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fundamental_rights_entrenchment_ambiguity,
    'To what extent can ''fundamental rights'' be considered truly entrenched or protected under a system of parliamentary sovereignty, given Parliament''s power to revise or repeal any statute?',
    'Comparative legal analysis with jurisdictions that have entrenched constitutional rights, or a future constitutional reform that introduces a higher-law framework for rights protection.',
    'If rights are found to be insufficiently entrenched, the effective extractiveness for minority groups is higher than currently measured, potentially shifting the constraint closer to a Snare for those seats. If parliamentary sovereignty is seen as compatible with robust rights protection through political convention, extractiveness remains as measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fundamental_rights_entrenchment_ambiguity, conceptual, 'The conceptual tension between parliamentary sovereignty and the entrenchment of fundamental rights.').

omega_variable(
    judicial_review_scope_ambiguity,
    'What is the true scope and effectiveness of judicial review in challenging parliamentary acts, particularly those that might infringe on rights derived from historical principles?',
    'Analysis of landmark court cases where parliamentary acts are challenged, and the extent to which judicial rulings influence or constrain legislative action without directly overturning it.',
    'If judicial review is found to have a more significant, albeit indirect, constraining effect on Parliament, the suppression metric for affected groups might be slightly lower, and their exit options less ''trapped''. If judicial review is largely symbolic, the current metrics hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_review_scope_ambiguity, empirical, 'The practical limits of judicial review on parliamentary sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 1688, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1688, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1688, 0.1).
narrative_ontology:measurement(magn_tr_t1788, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1788, 0.1).
narrative_ontology:measurement(magn_tr_t1888, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1888, 0.1).
narrative_ontology:measurement(magn_tr_t1988, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1988, 0.1).
narrative_ontology:measurement(magn_tr_t2024, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(magn_be_t1688, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1688, 0.55).
narrative_ontology:measurement(magn_be_t1788, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1788, 0.58).
narrative_ontology:measurement(magn_be_t1888, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1888, 0.6).
narrative_ontology:measurement(magn_be_t1988, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1988, 0.63).
narrative_ontology:measurement(magn_be_t2024, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1688, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1688, 0.6).
narrative_ontology:measurement(magn_su_t1788, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1788, 0.62).
narrative_ontology:measurement(magn_su_t1888, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1888, 0.65).
narrative_ontology:measurement(magn_su_t1988, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1988, 0.68).
narrative_ontology:measurement(magn_su_t2024, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, uk_human_rights_act).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, devolution_settlements_uk).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'magna_carta_constraint_authority' kernel, focusing on parliamentary sovereignty. It is linked to its sibling readings, 'living_constitutionalism_reading' and 'feudal_obsolescence_reading', which offer alternative interpretations of Magna Carta's enduring authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
