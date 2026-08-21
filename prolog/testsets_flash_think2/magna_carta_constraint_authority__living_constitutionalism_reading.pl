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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: magna_carta_constraint_authority__living_constitutionalism_reading
 *   human_readable: Magna Carta: Living Constitutionalism Reading
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This constraint represents the 'living constitutionalism' reading of
 *   Magna Carta, asserting that it establishes inherited due process and
 *   lawful restraint that binds all subsequent rulers through juridical
 *   precedent and evolutionary interpretation. In this reading, Magna Carta
 *   is not an obsolete feudal document but a foundational text whose
 *   principles adapt and apply to contemporary governance, limiting royal
 *   prerogative and executive discretion while shielding subjects with due
 *   process rights. The constraint is claimed as a Rope, reflecting its
 *   function in coordinating governance around inherited restraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__living_constitutionalism_reading, 0.35).
domain_priors:suppression_score(magna_carta_constraint_authority__living_constitutionalism_reading, 0.25).
domain_priors:theater_ratio(magna_carta_constraint_authority__living_constitutionalism_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__living_constitutionalism_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__living_constitutionalism_reading, rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__living_constitutionalism_reading, "Magna Carta: Living Constitutionalism Reading").
narrative_ontology:topic_domain(magna_carta_constraint_authority__living_constitutionalism_reading, "constitutional_history/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__living_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__living_constitutionalism_reading, '23942386-4eec-49ee-b9ee-cf719896adfb').
narrative_ontology:cs_kernel_codification('23942386-4eec-49ee-b9ee-cf719896adfb', fixed_text).
narrative_ontology:cs_authority_grounding('23942386-4eec-49ee-b9ee-cf719896adfb', lineage).
narrative_ontology:cs_interpretation_layer_present('23942386-4eec-49ee-b9ee-cf719896adfb').
narrative_ontology:cs_reading_relation('23942386-4eec-49ee-b9ee-cf719896adfb', magna_carta_constraint_authority__feudal_obsolescence_reading, forecloses).
narrative_ontology:cs_reading_relation('23942386-4eec-49ee-b9ee-cf719896adfb', magna_carta_constraint_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('23942386-4eec-49ee-b9ee-cf719896adfb', foundational, inherited_constitutional_restraint).
narrative_ontology:cs_axiom_status(inherited_constitutional_restraint, holdable).
narrative_ontology:cs_axiom_grounding('23942386-4eec-49ee-b9ee-cf719896adfb', inherited_constitutional_restraint, deontological).
narrative_ontology:cs_axiom('23942386-4eec-49ee-b9ee-cf719896adfb', foundational, evolutionary_interpretation_of_rights).
narrative_ontology:cs_axiom_status(evolutionary_interpretation_of_rights, holdable).
narrative_ontology:cs_axiom_grounding('23942386-4eec-49ee-b9ee-cf719896adfb', evolutionary_interpretation_of_rights, conventional).
narrative_ontology:cs_reference_frame('23942386-4eec-49ee-b9ee-cf719896adfb', foundational_inherited_restraint).
narrative_ontology:cs_drift_state('23942386-4eec-49ee-b9ee-cf719896adfb', contemporary_constitutional_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('23942386-4eec-49ee-b9ee-cf719896adfb', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__living_constitutionalism_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, subjects_citizens).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__living_constitutionalism_reading, judiciary).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, royal_prerogative).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__living_constitutionalism_reading, executive_discretion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the legal protections, due process, and stability provided by the constitutional framework, which limits arbitrary governmental power. Their ability to directly alter these foundational principles is constrained by the legal system.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, subjects_citizens, beneficiary,
    organized, generational, constrained, national).

% Acts as the primary interpreter and enforcer of Magna Carta's principles, ensuring their application through juridical precedent and evolutionary interpretation. Its legitimacy is deeply tied to upholding these foundational laws.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, judiciary, agenda_setter,
    institutional, civilizational, identity_locked, national).

% Represents the historical and theoretical powers of the monarch, which are significantly curtailed and bound by the legal framework established by Magna Carta's principles. Its scope is limited by law.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, royal_prerogative, payer,
    institutional, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(magna_carta_constraint_authority__living_constitutionalism_reading, royal_prerogative).

% The practical exercise of governmental power, which is subject to the rule of law and due process requirements derived from Magna Carta. Its actions must conform to established legal principles.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, executive_discretion, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_non_agent(magna_carta_constraint_authority__living_constitutionalism_reading, executive_discretion).

% The legislative body whose authority is understood to operate within the broader constitutional framework, even as it shapes and refines statutory law. Its sovereignty is often debated in relation to inherited constitutional principles.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, parliament, agenda_setter,
    institutional, generational, constrained, national).

% Engage in critical analysis and debate over Magna Carta's historical context, evolving meaning, and contemporary application, influencing judicial and public understanding of its principles.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__living_constitutionalism_reading, legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a foundational framework for lawful governance, ensuring predictability and limiting arbitrary power, thereby coordinating the relationship between rulers and the ruled through a shared understanding of legal restraint.
% TRANSFER_FUNCTION: Transfers authority from unchecked royal power and executive discretion to a system of law and precedent, providing subjects with legal protections and due process rights.
% ABSENT_VOICES: Proponents of absolute monarchy or unlimited executive power would object, as their claims to unfettered authority are curtailed by the principles of inherited due process and lawful restraint. Their historical arguments are largely superseded in this reading.
% DISAPPEARANCE_RATIONALE: If Magna Carta's principles, as interpreted through living constitutionalism, vanished overnight, the foundational concept of inherited due process and lawful restraint would disappear. This would lead to a significant reorganization of constitutional law and political power, potentially towards more arbitrary rule and a loss of established rights.
% FOUNDING_PROBLEM: Arbitrary royal power, lack of consistent legal protections for subjects, and the need to establish a framework for legitimate governance that binds rulers to law.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and constitutional theorists widely corroborate the historical problem of arbitrary royal power. Contemporary human rights discourse and international legal frameworks attest to the ongoing 'live' status of the need for due process and lawful restraint, citing Magna Carta's enduring influence as a foundational text in modern jurisprudence.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__living_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__living_constitutionalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(magna_carta_constraint_authority__living_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__living_constitutionalism_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

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
 *   Base extractiveness is low-to-moderate (0.35) because while it constrains state power, it primarily functions as a coordinating framework for legitimate governance rather than a mechanism for extraction from subjects. Suppression is low (0.25) as its persistence relies on legal enforcement and judicial interpretation, not overt coercion of the populace. Theater ratio is low (0.1) because its principles are genuinely functional and actively applied in legal systems, not merely performative. Accessibility collapse is moderate (0.4) as it establishes a strong legal framework but doesn't eliminate all alternative forms of governance or legal interpretation. Resistance is low (0.15) as its foundational principles are widely accepted, though specific applications may be contested.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the judiciary, this constraint is a vital framework for upholding the rule of law and protecting rights, evolving through interpretation. From the perspective of executive power, it represents a necessary but sometimes inconvenient limitation on their discretion. The engine computes this divergence from the structural data, showing how the same constraint is experienced differently by those it binds and those it empowers.
 *
 * DIRECTIONALITY LOGIC:
 *   Subjects/citizens are beneficiaries, gaining protections and stability. The judiciary acts as an agenda-setter, interpreting and enforcing the constraint, its identity bound to its role. Royal prerogative and executive discretion are victims, as their scope is directly curtailed. Parliament is also an agenda-setter, operating within and shaping the constitutional framework. Legal scholars serve as observers, analyzing its evolution.
 *
 * MANDATROPHY ANALYSIS:
 *   This 'living constitutionalism' reading inherently resists mandatrophy by asserting the constraint's adaptive capacity. Instead of becoming an obsolete relic (as in the 'feudal obsolescence' reading), its principles are continually reinterpreted and applied to new contexts, ensuring its mandate remains 'live' by evolving with societal needs. The ongoing relevance prevents its function from atrophying into mere theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feudal_relevance_ambiguity,
    'Is Magna Carta''s original feudal context entirely superseded by its modern interpretation, or does its historical specificity limit its contemporary applicability?',
    'Detailed historical-legal analysis comparing specific clauses'' original intent with their modern juridical application, identifying points of complete semantic shift versus continuous evolution.',
    'If the feudal context is found to be entirely superseded, it strengthens the ''living constitutionalism'' reading by demonstrating its adaptive capacity. If it significantly limits applicability, it lends credence to the ''feudal obsolescence'' reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feudal_relevance_ambiguity, conceptual, 'Whether the historical context of Magna Carta fundamentally constrains its modern relevance.').

omega_variable(
    parliamentary_supremacy_boundary,
    'Does Magna Carta''s authority genuinely bind Parliament, or is Parliament ultimately sovereign and capable of legislating away its principles?',
    'A constitutional crisis where Parliament attempts to explicitly repeal or fundamentally undermine a core Magna Carta principle, leading to a definitive judicial or political resolution on the limits of parliamentary sovereignty.',
    'If Parliament is found to be ultimately sovereign, it weakens the ''living constitutionalism'' reading''s claim of inherited restraint. If Parliament is found to be bound, it strengthens this reading''s claim of a higher constitutional order.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(parliamentary_supremacy_boundary, conceptual, 'The ultimate boundary of parliamentary sovereignty in relation to inherited constitutional principles.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__living_constitutionalism_reading, 1215, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement(magn_tr_t1688, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1688, 0.1).
narrative_ontology:measurement(magn_tr_t1776, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1776, 0.1).
narrative_ontology:measurement(magn_tr_t1948, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(magn_tr_t2024, magna_carta_constraint_authority__living_constitutionalism_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1215, 0.3).
narrative_ontology:measurement(magn_be_t1688, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1688, 0.32).
narrative_ontology:measurement(magn_be_t1776, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1776, 0.33).
narrative_ontology:measurement(magn_be_t1948, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 1948, 0.34).
narrative_ontology:measurement(magn_be_t2024, magna_carta_constraint_authority__living_constitutionalism_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1215, 0.2).
narrative_ontology:measurement(magn_su_t1688, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1688, 0.22).
narrative_ontology:measurement(magn_su_t1776, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1776, 0.23).
narrative_ontology:measurement(magn_su_t1948, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 1948, 0.24).
narrative_ontology:measurement(magn_su_t2024, magna_carta_constraint_authority__living_constitutionalism_reading, suppression_requirement, 2024, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__living_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, rule_of_law_doctrine).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__living_constitutionalism_reading, due_process_rights).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
