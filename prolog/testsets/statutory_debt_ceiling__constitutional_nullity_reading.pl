% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__constitutional_nullity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__constitutional_nullity_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: statutory_debt_ceiling__constitutional_nullity_reading
 *   human_readable: Statutory Debt Ceiling as Constitutionally Void (14th Amendment Section 4)
 *   domain: constitutional_law/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the constitutional_nullity_reading of
 *   the statutory debt ceiling kernel. Under this reading, the debt ceiling
 *   is not a coordination mechanism, extraction tool, or temporary scaffold —
 *   it is a void statute in conflict with the 14th Amendment Section 4's
 *   self-executing guarantee of the public debt's validity. The reading
 *   asserts that any statutory limit on borrowing required to pay
 *   appropriated obligations is unconstitutional, making the ceiling legally
 *   inoperative. The theater_ratio is very high (0.95) because what Congress
 *   calls 'debt ceiling votes' are performative: they create the appearance
 *   of fiscal discipline while having no legal force under the Constitution.
 *   The extractiveness and suppression are zero because, on this reading, the
 *   constraint does not actually constrain anyone — it is theatrically
 *   maintained but legally void.
 *
 * KEY AGENTS:
 *   - treasury_department: executes borrowing as appropriations require, unconstrained by the (void) ceiling
 *   - congress: nominally sets the ceiling but is bound by the Constitution to fund its appropriations
 *   - federal_courts: would enforce the constitutional nullity by striking down ceiling enforcement
 *   - legislative_minority: would use the ceiling as leverage but is excluded by the reading's assertion of constitutional preemption
 *   - financial_markets: benefit from Treasury's unfettered borrowing authority to service debts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__constitutional_nullity_reading, 0.0).
domain_priors:suppression_score(statutory_debt_ceiling__constitutional_nullity_reading, 0.0).
domain_priors:theater_ratio(statutory_debt_ceiling__constitutional_nullity_reading, 0.95).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 0.95).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__constitutional_nullity_reading, mountain).
narrative_ontology:human_readable(statutory_debt_ceiling__constitutional_nullity_reading, "Statutory Debt Ceiling as Constitutionally Void (14th Amendment Section 4)").
narrative_ontology:topic_domain(statutory_debt_ceiling__constitutional_nullity_reading, "constitutional_law/political_economy").

domain_priors:emerges_naturally(statutory_debt_ceiling__constitutional_nullity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__constitutional_nullity_reading, 'edb120b4-882b-4f7f-b2d4-3e8bebd40260').
narrative_ontology:cs_kernel_codification('edb120b4-882b-4f7f-b2d4-3e8bebd40260', formalized).
narrative_ontology:cs_authority_grounding('edb120b4-882b-4f7f-b2d4-3e8bebd40260', lineage).
narrative_ontology:cs_interpretation_layer_present('edb120b4-882b-4f7f-b2d4-3e8bebd40260').
narrative_ontology:cs_reading_relation('edb120b4-882b-4f7f-b2d4-3e8bebd40260', statutory_debt_ceiling__coordination_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('edb120b4-882b-4f7f-b2d4-3e8bebd40260', statutory_debt_ceiling__extraction_snare_reading, coexists_with).
narrative_ontology:cs_axiom('edb120b4-882b-4f7f-b2d4-3e8bebd40260', foundational, fourteenth_amendment_section_four_self_executes).
narrative_ontology:cs_axiom_status(fourteenth_amendment_section_four_self_executes, holdable).
narrative_ontology:cs_axiom_grounding('edb120b4-882b-4f7f-b2d4-3e8bebd40260', fourteenth_amendment_section_four_self_executes, deontological).
narrative_ontology:cs_axiom('edb120b4-882b-4f7f-b2d4-3e8bebd40260', foundational, statutory_ceiling_void_as_applied_to_appropriated_obligations).
narrative_ontology:cs_axiom_status(statutory_ceiling_void_as_applied_to_appropriated_obligations, holdable).
narrative_ontology:cs_axiom_grounding('edb120b4-882b-4f7f-b2d4-3e8bebd40260', statutory_ceiling_void_as_applied_to_appropriated_obligations, deontological).
narrative_ontology:cs_reference_frame('edb120b4-882b-4f7f-b2d4-3e8bebd40260', constitutional_supremacy_of_appropriations_authorization).
narrative_ontology:cs_drift_state('edb120b4-882b-4f7f-b2d4-3e8bebd40260', contemporary_political_standoff_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('edb120b4-882b-4f7f-b2d4-3e8bebd40260', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, treasury_department).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, constitutional_supremacy_doctrine).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__constitutional_nullity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(statutory_debt_ceiling__constitutional_nullity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, ExtMetricName, E),
    domain_priors:suppression_score(statutory_debt_ceiling__constitutional_nullity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(statutory_debt_ceiling__constitutional_nullity_reading),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is zero because the reading asserts the statute is legally inoperative — there is nothing to extract from and no party is constrained by a void rule. Suppression is zero for the same reason: a constitutionally null statute cannot suppress anyone; the apparent 'suppression' is the ceremonial performance of enforcing a dead law. Theater_ratio is very high (0.95) because the mechanism persists entirely as theater — Congress holds votes, Treasury manages the ceiling in administrative practice, and political actors invoke it rhetorically, but all of this activity occurs in the space of constitutional nullity. The ceiling is maintained in the ceremonial sense (repeating the vote, managing day-to-day compliance with an inoperant rule) but has no constitutional force. Accessibility_collapse is very high (0.98) because, if the reading is true, there is literally no alternative to appropriated borrowing — the Constitution does not permit a statute to block it. Resistance is high (0.72) because legislative minorities, fiscal hawks, and those who believe in legislative discipline actively resist this reading and defend the ceiling's legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (federal courts) and the ceremonial-performer seats (Congress, Treasury) experience this constraint radically differently. From the courts' analytical position, the constraint is a nullity that courts should declare void. From Congress and Treasury's operational position, the constraint exists as a ceremonial/administrative fact — they manage it as if it were operative while knowing it is contested. The engine computes each seat's type from the structural data; the divided perspective is built into the stakeholder situation declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Treasury and financial markets sit near the full-beneficiary end (d near 0.0): they benefit from the constitutional nullity reading because it makes borrowing authority unambiguous. The legislative minority sits near the full-target end (d near 1.0) under this reading because they are structurally foreclosed from using the ceiling as leverage — the reading asserts they have no such authority. Congress as a whole is near symmetric (d near 0.5) because Congress is bound by the same constitutional constraint it nominally created the statute under. Federal courts occupy the analytical seat (d = 0.5 by default for observers). No directionality overrides are needed; the structural data derives cleanly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (procedural inefficiency of per-bond votes) is dead. The founding problem was solved by administrative procedures, not by the ceiling's continued operation. Under this reading, the ceiling persists as pure theater — ceremonial votes and administrative compliance with a constitutionally null statute. The reading PREVENTS the misclassification that would occur if the constraint were classified as rope or scaffold based on its nominal coordination role; the nullity reading clarifies that the coordination was never actually achieved by the ceiling itself, only by procedural modernization independent of it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fourteenth_amendment_self_execution,
    'Is 14th Amendment Section 4 (''The validity of the public debt of the United States... shall not be questioned'') a self-executing constitutional bar on debt-ceiling enforcement, or does it require Congressional implementing legislation?',
    'Federal court ruling on the constitutionality of debt ceiling enforcement; originalist and living-constitution interpretive traditions diverge on whether the clause is self-executing or requires activation.',
    'If self-executing: the ceiling is unconstitutional on its face as applied to appropriations-backed borrowing, making this reading''s claimed type (mountain) correct. If requiring legislation: the reading collapses toward snare or scaffold, depending on what Congress does legislatively.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fourteenth_amendment_self_execution, conceptual, 'Whether 14th Amendment Section 4 operates as self-executing constitutional bar or requires Congressional implementation.').

omega_variable(
    natural_law_vs_judicial_declaration,
    'Is the constitutional nullity a natural fact (the statute is void ab initio, prior to any court ruling) or does it require judicial declaration to become operative?',
    'Federal court ruling and constitutional scholar consensus on whether the clause''s supremacy is self-evidently operative or requires courts to enforce it.',
    'If natural fact: Treasury could unilaterally ignore the ceiling and courts would enforce the Constitution. If requiring declaration: the nullity is contested until adjudicated, and Treasury operates under legal uncertainty. This feeds the theater_ratio and resistance metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_judicial_declaration, conceptual, 'Whether constitutional void is self-evident or requires judicial enforcement.').

omega_variable(
    constitutional_necessity_doctrine,
    'Does the Constitution establish that some debt is necessary to execute lawful appropriations (making the ceiling void as applied to such debt), or is the Constitution silent on whether Congress can appropriate without guaranteeing borrowing authority?',
    'Originalist and living-constitution textual analysis; review of Framers'' intent on federal borrowing authority; analysis of Article I Section 8 (borrowing power) in relation to Section 9 (payment of debts) and the 14th Amendment.',
    'If necessity is constitutional: the reading holds. If the Constitution is silent and Congress could appropriate without guaranteeing borrowing: the reading collapses and a snare or scaffold reading becomes tenable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_necessity_doctrine, conceptual, 'Whether the Constitution establishes constitutional necessity for debt incurred by appropriations.').

omega_variable(
    false_summit_beneficiary_ambiguity,
    'Treasury and the ''constitutional_supremacy_doctrine'' are listed as beneficiaries. Is Treasury a real actor that ''benefits'' from the ceiling''s nullity, or is the beneficiary actually just the doctrine itself? Does naming a doctrine as beneficiary obscure real institutional power dynamics?',
    'Audit whether Treasury actually experiences the nullity as a benefit (operational freedom to borrow) or whether only the abstract doctrine does. If only the doctrine benefits, remove Treasury as beneficiary and keep only the vindicated_proposition.',
    'If the beneficiary should be removed: this reading is a cleaner mountain, with no FSM trigger. If Treasury genuinely benefits operationally: the reading becomes candidate for false-summit reclassification, because a mountain that benefits identifiable institutional actors may be a constructed constraint masquerading as natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_beneficiary_ambiguity, empirical, 'Whether institutional beneficiaries exist or only the abstract constitutional doctrine benefits.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__constitutional_nullity_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 0, 0.92).
narrative_ontology:measurement(stat_tr_t5, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 5, 0.93).
narrative_ontology:measurement(stat_tr_t10, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 10, 0.94).
narrative_ontology:measurement(stat_tr_t15, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 15, 0.95).
narrative_ontology:measurement(stat_tr_t20, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 20, 0.95).
narrative_ontology:measurement(stat_tr_t25, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 25, 0.95).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(stat_be_t5, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 5, 0.0).
narrative_ontology:measurement(stat_be_t10, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 10, 0.0).
narrative_ontology:measurement(stat_be_t15, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 15, 0.0).
narrative_ontology:measurement(stat_be_t20, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 20, 0.0).
narrative_ontology:measurement(stat_be_t25, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 25, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 0, 0.0).
narrative_ontology:measurement(stat_su_t5, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 5, 0.0).
narrative_ontology:measurement(stat_su_t10, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 10, 0.0).
narrative_ontology:measurement(stat_su_t15, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 15, 0.0).
narrative_ontology:measurement(stat_su_t20, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 20, 0.0).
narrative_ontology:measurement(stat_su_t25, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 25, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__constitutional_nullity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(statutory_debt_ceiling__constitutional_nullity_reading, 0.0).
narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling__coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling__extraction_snare_reading).

% DUAL FORMULATION NOTE:
% The statutory debt ceiling kernel admits three structurally distinct constraint readings: (1) constitutional_nullity_reading (this file) — the ceiling is void under the 14th Amendment, zero extractiveness, mountain type; (2) coordination_scaffold_reading — the ceiling is a legitimate procedural coordination mechanism, likely transitory, with genuine beneficiaries; (3) extraction_snare_reading — the ceiling is weaponized leverage enabling legislative minority extraction. These three readings are NOT different measurements of the same constraint — they instantiate different ε values and different beneficiary/victim structures from the same contested constitutional kernel. Each reading has its own constraint_story.json file. All three are linked via network.affects_constraints so the corpus records the constitutional contest as a family of three structurally distinct constraints, each with a different ε, arising from one kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
