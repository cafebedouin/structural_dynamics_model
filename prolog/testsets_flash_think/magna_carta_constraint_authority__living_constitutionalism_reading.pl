% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__living_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__living_constitutionalism_reading, []).

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
 *   constraint_id: magna_carta_constraint_authority__living_constitutionalism_reading
 *   human_readable: Magna Carta's Living Constitutionalism of Due Process
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This constraint story models the 'living constitutionalism' reading of
 *   Magna Carta, which asserts its enduring authority through juridical
 *   precedent and evolutionary interpretation, binding all subsequent rulers.
 *   It is presented as a Rope, reflecting its function in coordinating
 *   governance around inherited restraint and due process, with
 *   low-to-moderate extractiveness. This reading directly contrasts with
 *   interpretations that view Magna Carta as historically obsolete or
 *   entirely subsumed by parliamentary sovereignty.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__living_constitutionalism_reading, 0.25).
domain_priors:suppression_score(magna_carta_constraint_authority__living_constitutionalism_reading, 0.15).
domain_priors:theater_ratio(magna_carta_constraint_authority__living_constitutionalism_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__living_constitutionalism_reading, rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__living_constitutionalism_reading, "Magna Carta's Living Constitutionalism of Due Process").
narrative_ontology:topic_domain(magna_carta_constraint_authority__living_constitutionalism_reading, "constitutional_history/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__living_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__living_constitutionalism_reading, 'a6ce7bad-19f3-48be-8df9-039d5181d6b8').
narrative_ontology:cs_kernel_codification('a6ce7bad-19f3-48be-8df9-039d5181d6b8', fixed_text).
narrative_ontology:cs_authority_grounding('a6ce7bad-19f3-48be-8df9-039d5181d6b8', lineage).
narrative_ontology:cs_interpretation_layer_present('a6ce7bad-19f3-48be-8df9-039d5181d6b8').
narrative_ontology:cs_reading_relation('a6ce7bad-19f3-48be-8df9-039d5181d6b8', magna_carta_constraint_authority__feudal_obsolescence_reading, forecloses).
narrative_ontology:cs_reading_relation('a6ce7bad-19f3-48be-8df9-039d5181d6b8', magna_carta_constraint_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('a6ce7bad-19f3-48be-8df9-039d5181d6b8', foundational, rule_of_law_binds_sovereign).
narrative_ontology:cs_axiom_status(rule_of_law_binds_sovereign, holdable).
narrative_ontology:cs_axiom_grounding('a6ce7bad-19f3-48be-8df9-039d5181d6b8', rule_of_law_binds_sovereign, deontological).
narrative_ontology:cs_axiom('a6ce7bad-19f3-48be-8df9-039d5181d6b8', foundational, charter_evolves_through_interpretation).
narrative_ontology:cs_axiom_status(charter_evolves_through_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('a6ce7bad-19f3-48be-8df9-039d5181d6b8', charter_evolves_through_interpretation, conventional).
narrative_ontology:cs_reference_frame('a6ce7bad-19f3-48be-8df9-039d5181d6b8', inherited_constitutional_restraint).
narrative_ontology:cs_drift_state('a6ce7bad-19f3-48be-8df9-039d5181d6b8', contemporary_legal_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a6ce7bad-19f3-48be-8df9-039d5181d6b8', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, subjects_citizens).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, judiciary).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, royal_prerogative).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, executive_discretion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, monarch_executive).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__living_constitutionalism_reading, rule_of_law_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__living_constitutionalism_reading, due_process_principle).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__living_constitutionalism_reading, constitutional_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically the primary target of the constraint, now bound by its principles. While still holding significant power, their actions are subject to legal review and constitutional limits, preventing arbitrary rule. They bear the cost of limited discretion.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, monarch_executive, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__living_constitutionalism_reading, monarch_executive, agenda_setter).

% Benefit from the protections of due process, lawful judgment, and limits on arbitrary power. Their rights are secured through the ongoing interpretation and application of these principles, providing stability and predictability.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, subjects_citizens, beneficiary,
    organized, generational, mobile, national).

% Acts as the primary interpreter and enforcer of Magna Carta's principles, evolving their meaning through juridical precedent. Their authority is enhanced by their role in upholding these foundational legal traditions.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, judiciary, agenda_setter,
    institutional, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__living_constitutionalism_reading, judiciary, beneficiary).

% Legislates within a framework influenced by Magna Carta's principles. While possessing legislative supremacy in some traditions, the underlying constitutional values shape the political and legal environment in which it operates.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, parliament, agenda_setter,
    institutional, generational, mobile, national).

% Analyze and debate the historical context, contemporary relevance, and future evolution of Magna Carta's principles. They contribute to the interpretive tradition but do not directly enforce the constraint.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, legal_scholars, observer,
    analytical, civilizational, analytical, global).

% The original beneficiaries and enforcers of Magna Carta, their specific feudal grievances are now obsolete. They are excluded from the contemporary interpretive conversation, though their historical actions laid the groundwork for the constraint.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, feudal_barons, excluded,
    powerless, generational, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__living_constitutionalism_reading, diffuse).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__living_constitutionalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, evolving framework for legitimate governance by binding rulers to law and due process, ensuring political stability and protecting subjects' rights through juridical precedent.
% TRANSFER_FUNCTION: Transfers power from the arbitrary discretion of the monarch/executive to a system of law and precedent, granting fundamental rights and protections to subjects and empowering the judiciary as an interpretive authority.
% ABSENT_VOICES: Those who advocate for absolute monarchical or executive power, or those who believe all law should be purely statutory and subject to simple parliamentary repeal, are structurally excluded from the interpretive framework of living constitutionalism. Their arguments are not given equal weight within this legal tradition.
% DISAPPEARANCE_RATIONALE: If Magna Carta's principles of due process and lawful restraint, as interpreted through a living constitutional lens, vanished overnight, the foundational understanding of governance in many common law systems would collapse. This would lead to a reassertion of arbitrary power, a loss of civil liberties, and a fundamental reorganization of legal and political structures.
% FOUNDING_PROBLEM: Arbitrary exercise of royal power, lack of predictable legal process, and unchecked executive authority leading to instability and injustice for subjects and feudal lords alike.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians, political theorists, and contemporary civil liberties advocates corroborate the ongoing relevance of these founding problems. They cite historical abuses and modern challenges to due process and executive overreach, demonstrating that the core problem of limiting arbitrary power remains pertinent, even if its specific manifestations change.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__living_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__living_constitutionalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(magna_carta_constraint_authority__living_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__living_constitutionalism_reading_tests).
:- end_tests(magna_carta_constraint_authority__living_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.25) because the constraint primarily functions to limit arbitrary power, rather than to extract resources from its subjects. Suppression is low (0.15) as its persistence relies on widespread acceptance and judicial enforcement, not active coercion against dissenters. Theater ratio is low (0.10) because its principles are genuinely applied and interpreted, not merely performed. The slight increase in extractiveness and suppression towards the end of the interval reflects modern challenges to constitutional norms and the need for renewed vigilance in upholding due process.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 'monarch_executive', the constraint imposes limits on their power, potentially seen as an extraction of prerogative. From the 'subjects_citizens' and 'judiciary' seats, it is a beneficial coordination mechanism that secures rights and provides a stable legal foundation. The 'living constitutionalism' reading emphasizes the ongoing utility and adaptability of these principles, bridging historical context with contemporary relevance.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'monarch_executive' seat is a payer, bearing the cost of limited discretion. 'Subjects_citizens' and the 'judiciary' are beneficiaries, gaining protections and enhanced authority, respectively. 'Parliament' operates within the framework, sometimes acting as an agenda-setter for its evolution. 'Feudal_barons' are historically excluded, their specific grievances no longer relevant. The constraint's directionality is largely towards limiting executive power for the benefit of the broader populace and the stability of the legal system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_vs_living_interpretation,
    'To what extent is Magna Carta''s authority derived from its original 13th-century context versus its continuous reinterpretation and application in modern legal systems?',
    'Comparative legal analysis of jurisdictions with different constitutional traditions (e.g., UK vs. US) and historical studies of judicial precedent. If its principles are consistently applied to novel situations without direct statutory re-enactment, it supports the ''living'' interpretation.',
    'If its authority is primarily historical, the constraint''s effective scope and binding force would be significantly reduced, potentially reclassifying it closer to a Piton or even a Mountain (as a historical artifact). If its living interpretation is robust, its Rope classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_vs_living_interpretation, conceptual, 'Ambiguity between originalist and living constitutionalist interpretations of Magna Carta''s authority.').

omega_variable(
    judicial_vs_parliamentary_supremacy,
    'In common law systems, does the judiciary''s evolutionary interpretation of Magna Carta''s principles genuinely bind Parliament, or is Parliament ultimately sovereign and able to override these precedents through statute?',
    'Analysis of constitutional crises, judicial review outcomes, and legislative responses to court rulings. If Parliament consistently defers to or incorporates judicial interpretations, it supports the binding nature. If Parliament frequently overrides or ignores such interpretations, it supports parliamentary supremacy.',
    'If parliamentary supremacy is absolute, the constraint''s ''living'' aspect is weakened, and its classification might shift towards a Scaffold (if its principles are merely temporary legislative guides) or a Piton (if its judicial application becomes purely theatrical). If judicial interpretation holds genuine binding force, the Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_vs_parliamentary_supremacy, empirical, 'The tension between judicial interpretation of constitutional principles and parliamentary sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__living_constitutionalism_reading, 1215, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1215, 0.15).
narrative_ontology:measurement(magn_tr_t1500, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1500, 0.12).
narrative_ontology:measurement(magn_tr_t1700, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1700, 0.1).
narrative_ontology:measurement(magn_tr_t1900, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(magn_tr_t2025, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1215, 0.3).
narrative_ontology:measurement(magn_be_t1500, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1500, 0.28).
narrative_ontology:measurement(magn_be_t1700, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1700, 0.25).
narrative_ontology:measurement(magn_be_t1900, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1900, 0.22).
narrative_ontology:measurement(magn_be_t2025, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 2025, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1215, 0.2).
narrative_ontology:measurement(magn_su_t1500, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1500, 0.18).
narrative_ontology:measurement(magn_su_t1700, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1700, 0.15).
narrative_ontology:measurement(magn_su_t1900, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1900, 0.12).
narrative_ontology:measurement(magn_su_t2025, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 2025, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__living_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority__feudal_obsolescence_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'magna_carta_constraint_authority' kernel. This 'living constitutionalism' reading emphasizes its ongoing, evolving legal force, contrasting with the 'feudal obsolescence' and 'parliamentary sovereignty' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
