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
 *   This constraint story models the authority of Magna Carta within a
 *   framework of parliamentary sovereignty. Under this reading, Magna Carta's
 *   principles are not an immutable higher law but survive only insofar as
 *   they are absorbed into and maintained by parliamentary statute.
 *   Parliament, as the supreme legislative body, can revise or repeal any of
 *   these provisions. This creates a 'tangled rope' where historical
 *   restraints exist but are ultimately controlled by a body that can also
 *   extract from those it governs, particularly minority groups unprotected
 *   by majoritarian legislation. The constraint is claimed as a
 *   'tangled_rope' because it genuinely coordinates the exercise of power by
 *   establishing a clear legislative authority, but this authority
 *   simultaneously enables asymmetric extraction from those whose rights are
 *   not explicitly protected by current statute.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.58).
domain_priors:suppression_score(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.65).
domain_priors:theater_ratio(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__parliamentary_sovereignty_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "Magna Carta's Authority under Parliamentary Sovereignty").
narrative_ontology:topic_domain(magna_carta_constraint_authority__parliamentary_sovereignty_reading, "constitutional_history/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__parliamentary_sovereignty_reading, '2bace036-2af2-4d09-989c-dcd2912963a4').
narrative_ontology:cs_kernel_codification('2bace036-2af2-4d09-989c-dcd2912963a4', formalized).
narrative_ontology:cs_authority_grounding('2bace036-2af2-4d09-989c-dcd2912963a4', lineage).
narrative_ontology:cs_interpretation_layer_present('2bace036-2af2-4d09-989c-dcd2912963a4').
narrative_ontology:cs_reading_relation('2bace036-2af2-4d09-989c-dcd2912963a4', magna_carta_constraint_authority__living_constitutionalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('2bace036-2af2-4d09-989c-dcd2912963a4', magna_carta_constraint_authority__feudal_obsolescence_reading, coexists_with).
narrative_ontology:cs_axiom('2bace036-2af2-4d09-989c-dcd2912963a4', foundational, parliamentary_supremacy_over_common_law).
narrative_ontology:cs_axiom_status(parliamentary_supremacy_over_common_law, holdable).
narrative_ontology:cs_axiom_grounding('2bace036-2af2-4d09-989c-dcd2912963a4', parliamentary_supremacy_over_common_law, conventional).
narrative_ontology:cs_axiom('2bace036-2af2-4d09-989c-dcd2912963a4', foundational, statute_as_sole_source_of_binding_law).
narrative_ontology:cs_axiom_status(statute_as_sole_source_of_binding_law, holdable).
narrative_ontology:cs_axiom_grounding('2bace036-2af2-4d09-989c-dcd2912963a4', statute_as_sole_source_of_binding_law, conventional).
narrative_ontology:cs_reference_frame('2bace036-2af2-4d09-989c-dcd2912963a4', glorious_revolution_settlement).
narrative_ontology:cs_drift_state('2bace036-2af2-4d09-989c-dcd2912963a4', contemporary_human_rights_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2bace036-2af2-4d09-989c-dcd2912963a4', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__parliamentary_sovereignty_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliament).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__parliamentary_sovereignty_reading, majority_electorate).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, minority_groups).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__parliamentary_sovereignty_reading, individuals_unprotected_by_statute).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the supreme legislative authority, Parliament can absorb, revise, or repeal any provision of Magna Carta through statute. It benefits from the flexibility to adapt law to contemporary needs, but also from the historical legitimacy Magna Carta confers.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, parliament, agenda_setter,
    institutional, generational, mobile, national).

% Benefits from a system where the will of the elected representatives can directly shape law, including the principles inherited from Magna Carta. Their interests are generally reflected in parliamentary action, though individual members may be unprotected.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, majority_electorate, beneficiary,
    organized, biographical, constrained, national).

% Bear the cost of a system where their rights, if not explicitly codified in statute, can be overridden by parliamentary action. They lack direct recourse if Parliament chooses to legislate in a way that diminishes protections historically associated with Magna Carta.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, minority_groups, payer,
    powerless, generational, trapped, national).

% Individuals whose fundamental rights, though historically linked to Magna Carta, are not explicitly protected by current statute. They are vulnerable to parliamentary revision or repeal, with limited legal avenues for challenge outside of the legislative process.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, individuals_unprotected_by_statute, payer,
    powerless, biographical, identity_locked, local).

% Interprets and applies statute law, including those parts of Magna Carta absorbed into it. Under parliamentary sovereignty, the judiciary cannot strike down primary legislation, but can interpret its application. They observe the tension between historical principles and legislative will.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__parliamentary_sovereignty_reading, judiciary, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, unified source of legal authority through parliamentary statute, ensuring that fundamental principles (including those from Magna Carta) are subject to democratic revision and adaptation, rather than being fixed by ancient texts or judicial interpretation alone.
% TRANSFER_FUNCTION: Transfers ultimate legal authority from historical documents or inherent rights to the elected Parliament, allowing for the revision or repeal of any constraint, potentially from minority groups or individuals to the majority represented in Parliament.
% ABSENT_VOICES: Advocates for entrenched constitutional rights or a higher law that binds Parliament would object. Their voices are present in academic and advocacy circles but lack direct legislative power to challenge parliamentary supremacy.
% DISAPPEARANCE_RATIONALE: If the principle of parliamentary sovereignty vanished overnight, the entire legal and constitutional framework of the UK would collapse. The authority of all statutes, including those incorporating Magna Carta, would be questioned, leading to profound constitutional crisis and a complete reordering of legal power.
% FOUNDING_PROBLEM: The problem of arbitrary monarchical power and the need to establish a clear, revisable source of ultimate legal authority that reflects the will of the people through their representatives.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and political theorists outside of Parliament corroborate that the problem of establishing and maintaining a legitimate, adaptable source of ultimate legal authority remains live, even as the specific threats have evolved from monarchical absolutism to potential majoritarian overreach.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__parliamentary_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 0.58, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.58) reflects the potential for Parliament to legislate in ways that diminish historical protections, particularly for minorities. Suppression (0.65) is moderate-high because there are limited legal avenues to challenge parliamentary statute, and exit options for affected groups are severely constrained. The theater ratio (0.20) is low, as Parliament's legislative function is genuine, though the invocation of Magna Carta can sometimes be performative. The increasing extractiveness and suppression over time reflect the gradual consolidation of parliamentary power and the diminishing role of non-statutory constitutional principles.
 *
 * PERSPECTIVAL GAP:
 *   From Parliament's perspective, this is a legitimate and adaptable system of governance. From the perspective of minority groups, it represents a potential for majoritarian tyranny where historical protections can be eroded. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Parliament and the majority electorate are beneficiaries, as they control and benefit from the flexibility of the system. Minority groups and individuals unprotected by statute are payers, as their rights are contingent on parliamentary will. The judiciary acts as an observer, interpreting the law as it stands without the power to overturn primary legislation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    parliamentary_intent_vs_effect,
    'Does Parliament''s intent to uphold historical liberties always align with the actual effect of its legislation on minority rights?',
    'Empirical analysis of legislative outcomes and their impact on various social groups, particularly those historically vulnerable, over extended periods.',
    'If a consistent divergence is found, it would strengthen the ''tangled rope'' classification by highlighting the structural extraction from unprotected groups, even if not explicitly intended by the legislature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parliamentary_intent_vs_effect, empirical, 'Alignment of legislative intent with actual impact on rights.').

omega_variable(
    source_of_legitimacy_ambiguity,
    'Is the ultimate source of constitutional legitimacy in the UK derived from parliamentary sovereignty, or from a deeper, unwritten constitutional tradition that includes Magna Carta?',
    'Conceptual analysis of legal philosophy and historical jurisprudence, examining how different legal theorists and judges ground constitutional authority.',
    'If a deeper tradition is acknowledged, it would challenge the absolute nature of parliamentary sovereignty, potentially shifting the constraint towards a ''rope'' or even ''mountain'' for certain fundamental principles, by limiting Parliament''s revisionary power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(source_of_legitimacy_ambiguity, conceptual, 'Whether legitimacy is solely parliamentary or from a broader tradition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__parliamentary_sovereignty_reading, 1688, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1688, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1688, 0.1).
narrative_ontology:measurement(magn_tr_t1800, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1800, 0.15).
narrative_ontology:measurement(magn_tr_t1900, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 1900, 0.18).
narrative_ontology:measurement(magn_tr_t2024, magna_carta_constraint_authority__parliamentary_sovereignty_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(magn_be_t1688, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1688, 0.4).
narrative_ontology:measurement(magn_be_t1800, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1800, 0.48).
narrative_ontology:measurement(magn_be_t1900, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 1900, 0.52).
narrative_ontology:measurement(magn_be_t2024, magna_carta_constraint_authority__parliamentary_sovereignty_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1688, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1688, 0.5).
narrative_ontology:measurement(magn_su_t1800, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1800, 0.55).
narrative_ontology:measurement(magn_su_t1900, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 1900, 0.6).
narrative_ontology:measurement(magn_su_t2024, magna_carta_constraint_authority__parliamentary_sovereignty_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, uk_human_rights_act_authority).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__parliamentary_sovereignty_reading, constitutional_reform_act_authority).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'magna_carta_constraint_authority' kernel. This 'parliamentary_sovereignty_reading' focuses on the legislative absorption and revisability of Magna Carta's principles, contrasting with 'living_constitutionalism_reading' (judicial evolution) and 'feudal_obsolescence_reading' (historical irrelevance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
