% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__anti_caste_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourteenth_amendment_equal_protection__anti_caste_reading, []).

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
 *   constraint_id: fourteenth_amendment_equal_protection__anti_caste_reading
 *   human_readable: 14th Amendment Equal Protection (Anti-Caste Reading)
 *   domain: constitutional_law/political_philosophy/civil_rights
 *
 * SUMMARY:
 *   This constraint represents the 'anti-caste' reading of the 14th
 *   Amendment's Equal Protection Clause, which mandates active state
 *   intervention to dismantle systemic racial, gender, and status
 *   hierarchies. It views equality not merely as formal non-discrimination
 *   but as substantive equity requiring corrective action. This reading is a
 *   specific interpretation of a highly contested constitutional kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, 0.65).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__anti_caste_reading, 0.4).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__anti_caste_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__anti_caste_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__anti_caste_reading, "14th Amendment Equal Protection (Anti-Caste Reading)").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__anti_caste_reading, "constitutional_law/political_philosophy/civil_rights").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__anti_caste_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__anti_caste_reading, 'ad617b15-bb33-43ca-9136-81f6925ecbed').
narrative_ontology:cs_kernel_codification('ad617b15-bb33-43ca-9136-81f6925ecbed', fixed_text).
narrative_ontology:cs_authority_grounding('ad617b15-bb33-43ca-9136-81f6925ecbed', lineage).
narrative_ontology:cs_interpretation_layer_present('ad617b15-bb33-43ca-9136-81f6925ecbed').
narrative_ontology:cs_reading_relation('ad617b15-bb33-43ca-9136-81f6925ecbed', fourteenth_amendment_equal_protection__formal_equality_reading, coexists_with).
narrative_ontology:cs_axiom('ad617b15-bb33-43ca-9136-81f6925ecbed', foundational, equality_requires_substantive_equity).
narrative_ontology:cs_axiom_status(equality_requires_substantive_equity, holdable).
narrative_ontology:cs_axiom_grounding('ad617b15-bb33-43ca-9136-81f6925ecbed', equality_requires_substantive_equity, deontological).
narrative_ontology:cs_axiom('ad617b15-bb33-43ca-9136-81f6925ecbed', foundational, state_has_duty_to_dismantle_hierarchy).
narrative_ontology:cs_axiom_status(state_has_duty_to_dismantle_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('ad617b15-bb33-43ca-9136-81f6925ecbed', state_has_duty_to_dismantle_hierarchy, deontological).
narrative_ontology:cs_reference_frame('ad617b15-bb33-43ca-9136-81f6925ecbed', post_civil_war_reconstruction_intent).
narrative_ontology:cs_drift_state('ad617b15-bb33-43ca-9136-81f6925ecbed', contemporary_judicial_challenges, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ad617b15-bb33-43ca-9136-81f6925ecbed', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__anti_caste_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, historically_subordinated_groups).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, institutions_perpetuating_hierarchy).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, dominant_groups_resisting_remedy).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__anti_caste_reading, structural_inequality_doctrine).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__anti_caste_reading, corrective_justice_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups benefit from state action designed to dismantle systemic hierarchies, receiving targeted support and protections. Their ability to exit systemic disadvantage is constrained by historical and ongoing structural barriers, making state intervention critical.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, historically_subordinated_groups, beneficiary,
    organized, generational, constrained, national).

% These institutions (e.g., discriminatory housing authorities, biased educational systems) bear the costs of corrective state action, being compelled to change practices, reallocate resources, or face legal challenges. Their exit options are constrained by legal mandates and public pressure.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, institutions_perpetuating_hierarchy, payer,
    institutional, biographical, constrained, national).

% Members of dominant groups who perceive themselves as disadvantaged by affirmative state action bear costs in terms of altered access or opportunities. Their resistance often manifests in legal challenges or political opposition, but they cannot easily exit the legal framework.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, dominant_groups_resisting_remedy, payer,
    powerful, biographical, constrained, national).

% Government agencies and courts tasked with implementing anti-caste policies. They set the agenda for corrective action, enforce compliance, and adjudicate disputes. Their actions are constrained by political will, legal challenges, and resource limitations.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, state_actors_implementing_remedy, agenda_setter,
    institutional, generational, constrained, national).

% Advocates for a formal equality reading of the 14th Amendment, who argue against state-sponsored corrective action based on group identity. They are excluded from the anti-caste reading's policy-making process but actively contest its legitimacy in courts and public discourse.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, formal_equality_advocates, excluded,
    organized, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state and societal efforts to identify and dismantle systemic racial, gender, and status hierarchies, ensuring that all citizens have an equal opportunity to participate in society free from structural disadvantage.
% TRANSFER_FUNCTION: Transfers resources, opportunities, and power from institutions and practices that perpetuate hierarchy to historically subordinated groups, aiming to rectify past and present injustices.
% ABSENT_VOICES: Advocates for a purely formal equality reading are actively excluded from the policy-making and interpretive processes of the anti-caste framework, though they are present in legal challenges. Their arguments for 'colorblind' policies are directly opposed to the anti-caste approach.
% DISAPPEARANCE_RATIONALE: If this reading vanished, state-mandated corrective actions would cease, leading to a re-entrenchment of existing hierarchies. The legal landscape for civil rights would revert to a more limited, formalistic approach, profoundly altering the lived experience of historically subordinated groups and the responsibilities of the state.
% FOUNDING_PROBLEM: The 14th Amendment was enacted to address the systemic inequalities and caste-like structures that persisted after the abolition of slavery, ensuring equal citizenship and protection under the law for all, particularly formerly enslaved people.
% FOUNDING_PROBLEM_CORROBORATION: Historians and civil rights organizations widely corroborate that the founding problem of systemic hierarchy and its impact on citizenship remains live. Legal scholars and social scientists provide extensive empirical evidence of ongoing structural inequalities, supporting the need for active dismantling.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__anti_caste_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__anti_caste_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__anti_caste_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment_equal_protection__anti_caste_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fourteenth_amendment_equal_protection__anti_caste_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the significant reordering of resources and opportunities required to dismantle entrenched hierarchies, which is 'extracted' from those who benefit from the status quo. Suppression (0.4) is moderate; while state action is required, it faces substantial resistance and is not universally enforced without contest. Theater ratio (0.2) is low, as the efforts are generally genuine, though implementation can be imperfect. Accessibility collapse (0.3) is low because alternatives (e.g., formal equality arguments) are actively pursued by opponents. Resistance (0.7) is high due to ongoing legal and political challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of historically subordinated groups, this reading is a necessary rope or scaffold for achieving genuine equality. From the perspective of dominant groups resisting remedy, it is a snare that unfairly extracts from them. The state actors implementing it experience it as a tangled rope, balancing coordination with enforcement against resistance.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated groups are clear beneficiaries (d near 0.0) as the constraint aims to improve their structural position. Institutions perpetuating hierarchy and dominant groups resisting remedy are targets (d near 1.0) as they bear the costs of change. State actors implementing remedy are agenda-setters (d near 0.5), balancing coordination and enforcement. Formal equality advocates are excluded, their arguments directly opposed to this reading's premise.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively addresses a live founding problem of systemic hierarchy. Its persistence is tied to the ongoing existence of structural inequality, preventing it from becoming a piton. The high resistance and contested status indicate it is far from a settled mountain, and its active enforcement against entrenched interests prevents it from being mislabeled as a simple rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_efficacy_of_remedies,
    'Are the state corrective actions mandated by this reading empirically effective at dismantling hierarchy and achieving substantive equality, or do they produce unintended consequences or new forms of stratification?',
    'Longitudinal social science research and policy evaluation tracking outcomes for targeted groups and broader societal impacts.',
    'If remedies are found ineffective or counterproductive, the legitimacy of this reading''s policy prescriptions would be undermined, potentially shifting support towards alternative approaches or weakening its enforcement. If highly effective, its claims would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_efficacy_of_remedies, empirical, 'Whether anti-caste remedies achieve their intended goals.').

omega_variable(
    scope_of_hierarchy_definition,
    'What constitutes a ''hierarchy'' or ''subordinated group'' under this reading? Is the definition stable, or does it expand to include new forms of social stratification, potentially diluting the focus on historical injustices?',
    'Judicial interpretation and legislative action clarifying the scope and criteria for identifying relevant hierarchies and groups.',
    'An overly broad or unstable definition could lead to ''category creep,'' making the constraint''s application diffuse and politically contentious. A clear, consistent definition would enhance its coherence and enforceability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_hierarchy_definition, conceptual, 'Clarity and stability of ''hierarchy'' definition.').

omega_variable(
    tension_with_formal_equality,
    'Can the anti-caste reading coexist with a formal equality reading within a single coherent constitutional framework, or does one necessarily undermine the other''s foundational premises?',
    'Ongoing legal and philosophical debate, and the outcomes of judicial challenges that attempt to reconcile or choose between these interpretations.',
    'If irreconcilable, the constitutional system faces a fundamental choice, potentially leading to significant shifts in legal doctrine. If a coherent synthesis is possible, it would reduce the conceptual tension and political polarization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tension_with_formal_equality, conceptual, 'Compatibility of anti-caste and formal equality readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__anti_caste_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(four_tr_t1954, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1954, 0.1).
narrative_ontology:measurement(four_tr_t1970, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(four_tr_t1990, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(four_tr_t2010, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(four_tr_t2024, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(four_be_t1954, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1954, 0.4).
narrative_ontology:measurement(four_be_t1970, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(four_be_t1990, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(four_be_t2010, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(four_be_t2024, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(four_su_t1954, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1954, 0.3).
narrative_ontology:measurement(four_su_t1970, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(four_su_t1990, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(four_su_t2010, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(four_su_t2024, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
