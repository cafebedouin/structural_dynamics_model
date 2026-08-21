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
 *   human_readable: Magna Carta's Authority under Parliamentary Sovereignty
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This constraint represents the 'parliamentary sovereignty' reading of the
 *   Magna Carta's authority. In this view, Magna Carta's historical
 *   restraints on the Crown survive only to the extent they have been
 *   absorbed into parliamentary statute law. Parliament, as the supreme legal
 *   authority, inherits the power to revise or repeal any of these
 *   provisions. This reading emphasizes the flexibility of the UK
 *   constitution and the ultimate power of the democratically elected
 *   legislature, but it also implies that rights and protections are
 *   ultimately subject to parliamentary will, potentially leaving minorities
 *   vulnerable. The constraint is claimed as a Rope by its proponents
 *   (Parliament), but its operation, particularly concerning vulnerable
 *   groups, reveals it as a Tangled Rope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.62).
domain_priors:suppression_score(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.72).
domain_priors:theater_ratio(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "Magna Carta's Authority under Parliamentary Sovereignty").
narrative_ontology:topic_domain(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "constitutional_history/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__parliamentary_sovereignty_reading, '9344e4dc-3033-4e4f-85c3-6b9b1dfe488f').
narrative_ontology:cs_kernel_codification('9344e4dc-3033-4e4f-85c3-6b9b1dfe488f', fixed_text).
narrative_ontology:cs_authority_grounding('9344e4dc-3033-4e4f-85c3-6b9b1dfe488f', lineage).
narrative_ontology:cs_interpretation_layer_present('9344e4dc-3033-4e4f-85c3-6b9b1dfe488f').
narrative_ontology:cs_reading_relation('9344e4dc-3033-4e4f-85c3-6b9b1dfe488f', magna_carta_constraint_authority__feudal_obsolescence_reading, influences).
narrative_ontology:cs_reading_relation('9344e4dc-3033-4e4f-85c3-6b9b1dfe488f', magna_carta_constraint_authority__living_constitutionalism_reading, forecloses).
narrative_ontology:cs_axiom('9344e4dc-3033-4e4f-85c3-6b9b1dfe488f', foundational, parliamentary_supremacy).
narrative_ontology:cs_axiom_status(parliamentary_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('9344e4dc-3033-4e4f-85c3-6b9b1dfe488f', parliamentary_supremacy, conventional).
narrative_ontology:cs_axiom('9344e4dc-3033-4e4f-85c3-6b9b1dfe488f', foundational, statutory_absorption).
narrative_ontology:cs_axiom_status(statutory_absorption, holdable).
narrative_ontology:cs_axiom_grounding('9344e4dc-3033-4e4f-85c3-6b9b1dfe488f', statutory_absorption, conventional).
narrative_ontology:cs_reference_frame('9344e4dc-3033-4e4f-85c3-6b9b1dfe488f', parliamentary_supremacy_framework).
narrative_ontology:cs_drift_state('9344e4dc-3033-4e4f-85c3-6b9b1dfe488f', contemporary_uk_legal_system, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9344e4dc-3033-4e4f-85c3-6b9b1dfe488f', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliament).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, majority_electorate).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, unprotected_minorities).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, individuals_affected_by_parliamentary_revision).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, the_crown).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the supreme legal authority, Parliament absorbs and re-enacts Magna Carta's provisions into statute, retaining the power to revise or repeal them. It benefits from the flexibility and ultimate authority this grants over the constitutional framework.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliament, agenda_setter,
    institutional, generational, mobile, national).

% Benefits from a flexible constitutional system that can adapt to contemporary needs through democratic processes, as Parliament is theoretically accountable to them. They are the ultimate source of Parliament's legitimacy in this reading.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, majority_electorate, beneficiary,
    organized, biographical, mobile, national).

% Bear the risk of parliamentary action that may erode historical protections or introduce new constraints without adequate safeguards, as their rights are subject to the will of the parliamentary majority.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, unprotected_minorities, payer,
    powerless, generational, constrained, national).

% Individuals whose specific rights or liberties, historically associated with Magna Carta, are altered or removed by parliamentary statute. They face the direct consequences of Parliament's revisory power.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, individuals_affected_by_parliamentary_revision, payer,
    moderate, biographical, constrained, national).

% Its historical prerogatives, once constrained by Magna Carta, are now further constrained by Parliament's ultimate authority. The Crown's role is largely ceremonial, operating within the bounds set by parliamentary statute.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, the_crown, payer,
    institutional, generational, constrained, national).

% Interprets and applies parliamentary statutes, including those that incorporate or revise Magna Carta's principles. It is bound by parliamentary sovereignty and cannot strike down primary legislation, but can influence its application.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, judiciary, observer,
    institutional, generational, analytical, national).

% Analyze and critique the constitutional framework, including the implications of parliamentary sovereignty for historical documents like Magna Carta. They provide intellectual input but have no direct power to alter the constraint.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliament).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__parliamentary_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, revisable legal framework for governance, mediating historical constraints through a representative body to ensure legal flexibility and democratic accountability.
% TRANSFER_FUNCTION: Transfers ultimate legal authority from historical, immutable documents to the current legislative body, potentially transferring rights and protections from minorities to the majority through statutory revision.
% ABSENT_VOICES: Historical proponents of immutable constitutionalism, those advocating for a higher law that binds Parliament, and international human rights advocates who might argue for supra-parliamentary protections for minorities.
% DISAPPEARANCE_RATIONALE: If Parliament's authority to revise or repeal Magna Carta's provisions (or any statute) vanished, the entire UK constitutional framework would collapse. The principle of parliamentary sovereignty, central to this reading, would be undermined, leading to a crisis of legal legitimacy and a fundamental reorganization of governmental power.
% FOUNDING_PROBLEM: To reconcile historical constraints on monarchical power with the evolving needs of a modern state and the principle of popular sovereignty, ensuring a flexible legal system capable of adapting to societal changes.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and political theorists outside of Parliament itself corroborate this ongoing tension, noting the continuous debate over the balance of power and rights in the UK, particularly concerning the limits of parliamentary authority and the protection of fundamental liberties.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__parliamentary_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.62, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.62) reflects the potential for Parliament to use its power to the detriment of certain groups, even if it also provides coordination for the majority. Suppression (0.72) is high because Parliament's legal supremacy makes it difficult to resist or circumvent its decisions, particularly for those without strong political representation. The theater ratio (0.15) is low because Parliament's legislative actions are genuinely functional and consequential, not merely performative. The increasing extractiveness and suppression over the interval reflect a growing awareness of the potential for parliamentary power to be used in ways that disproportionately affect minorities, even as the system remains stable.
 *
 * PERSPECTIVAL GAP:
 *   From Parliament's perspective, this constraint is a legitimate and necessary mechanism for democratic governance and constitutional evolution, a 'Rope' that coordinates the nation. From the perspective of unprotected minorities, it can function as a 'Snare' or 'Tangled Rope,' where their rights are subject to the will of the majority without higher constitutional protection. The engine's classification as Tangled Rope captures this dual nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Parliament and the majority electorate are beneficiaries, as they gain flexibility and ultimate authority. Unprotected minorities, individuals affected by specific revisions, and the historical prerogatives of the Crown are the targets, bearing the costs of this flexible, majoritarian system. The judiciary and legal scholars act as observers, interpreting and analyzing the system without direct power to alter its fundamental structure.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    parliamentary_will_vs_minority_rights,
    'To what extent does parliamentary sovereignty genuinely represent the ''will of the people'' versus enabling majoritarian tyranny over unprotected minorities?',
    'Empirical analysis of legislative outcomes over time, focusing on the impact on minority groups, and comparative constitutional studies of systems with and without entrenched constitutional rights.',
    'If parliamentary action consistently disadvantages minorities without redress, the constraint''s effective extractiveness and suppression would be higher, pushing it closer to a Snare for those groups. If robust mechanisms for minority protection are found, it would lean more towards a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parliamentary_will_vs_minority_rights, empirical, 'The balance between parliamentary power and minority protection.').

omega_variable(
    magna_carta_modern_relevance,
    'Is Magna Carta''s historical significance entirely absorbed into statute, or does it retain an independent, symbolic, or moral authority that subtly influences parliamentary action?',
    'Content analysis of parliamentary debates and judicial reasoning, examining references to Magna Carta beyond its statutory enactments, and public opinion surveys on its perceived constitutional status.',
    'If Magna Carta retains independent moral force, the ''parliamentary sovereignty'' reading might underestimate the informal constraints on Parliament, making the constraint less extractive than it appears. If its influence is purely statutory, the reading holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(magna_carta_modern_relevance, conceptual, 'The enduring non-statutory influence of Magna Carta.').

omega_variable(
    parliamentary_sovereignty_vs_international_law,
    'How does the principle of parliamentary sovereignty interact with and potentially conflict with international human rights law and treaties, which Parliament has ratified?',
    'Legal analysis of cases where domestic parliamentary statutes conflict with international obligations, and the extent to which courts or Parliament itself prioritize one over the other.',
    'If international law is consistently overridden by parliamentary statute, the constraint''s suppression of alternative protections is higher. If international law creates effective external constraints, the effective suppression is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parliamentary_sovereignty_vs_international_law, empirical, 'Interaction of parliamentary sovereignty with international legal obligations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 1900, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1900, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(magn_tr_t1940, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1940, 0.15).
narrative_ontology:measurement(magn_tr_t1980, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(magn_tr_t2020, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(magn_be_t1900, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1900, 0.55).
narrative_ontology:measurement(magn_be_t1940, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1940, 0.58).
narrative_ontology:measurement(magn_be_t1980, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(magn_be_t2020, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 2020, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1900, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1900, 0.65).
narrative_ontology:measurement(magn_su_t1940, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1940, 0.68).
narrative_ontology:measurement(magn_su_t1980, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(magn_su_t2020, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 2020, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__parliamentary_sovereignty_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
