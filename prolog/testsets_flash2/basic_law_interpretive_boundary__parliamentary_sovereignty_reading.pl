% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
 *   human_readable: Knesset's Ultimate Authority over Basic Laws (Parliamentary Sovereignty Reading)
 *   domain: constitutional_law/comparative_constitutionalism/judicial_review_theory
 *
 * SUMMARY:
 *   This constraint represents the 'parliamentary sovereignty' reading of the
 *   Knesset's authority over Basic Laws in Israel. In this reading, the
 *   Knesset, as the directly elected legislative body, holds ultimate and
 *   unconstrained power to interpret and amend Basic Laws by a simple
 *   majority, including the power to override any judicial review. This
 *   perspective views the Basic Laws as ordinary legislation, albeit with
 *   constitutional status, fully subject to the will of the legislature. This
 *   is one reading of the 'basic_law_interpretive_boundary' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.05).
domain_priors:suppression_score(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.1).
domain_priors:theater_ratio(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, mountain).
narrative_ontology:human_readable(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "Knesset's Ultimate Authority over Basic Laws (Parliamentary Sovereignty Reading)").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "constitutional_law/comparative_constitutionalism/judicial_review_theory").

domain_priors:emerges_naturally(basic_law_interpretive_boundary__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, '2c54474c-9eb3-41b3-9e83-68d57ee077c5').
narrative_ontology:cs_kernel_codification('2c54474c-9eb3-41b3-9e83-68d57ee077c5', formalized).
narrative_ontology:cs_authority_grounding('2c54474c-9eb3-41b3-9e83-68d57ee077c5', lineage).
narrative_ontology:cs_interpretation_layer_present('2c54474c-9eb3-41b3-9e83-68d57ee077c5').
narrative_ontology:cs_reading_relation('2c54474c-9eb3-41b3-9e83-68d57ee077c5', basic_law_interpretive_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('2c54474c-9eb3-41b3-9e83-68d57ee077c5', basic_law_interpretive_boundary__balanced_contestation_reading, forecloses).
narrative_ontology:cs_axiom('2c54474c-9eb3-41b3-9e83-68d57ee077c5', foundational, knesset_is_supreme_legislative_authority).
narrative_ontology:cs_axiom_status(knesset_is_supreme_legislative_authority, holdable).
narrative_ontology:cs_axiom_grounding('2c54474c-9eb3-41b3-9e83-68d57ee077c5', knesset_is_supreme_legislative_authority, deontological).
narrative_ontology:cs_axiom('2c54474c-9eb3-41b3-9e83-68d57ee077c5', foundational, basic_laws_are_ordinary_legislation_with_constitutional_status).
narrative_ontology:cs_axiom_status(basic_laws_are_ordinary_legislation_with_constitutional_status, holdable).
narrative_ontology:cs_axiom_grounding('2c54474c-9eb3-41b3-9e83-68d57ee077c5', basic_laws_are_ordinary_legislation_with_constitutional_status, conventional).
narrative_ontology:cs_reference_frame('2c54474c-9eb3-41b3-9e83-68d57ee077c5', unconstrained_parliamentary_sovereignty).
narrative_ontology:cs_drift_state('2c54474c-9eb3-41b3-9e83-68d57ee077c5', contemporary_judicial_activism_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2c54474c-9eb3-41b3-9e83-68d57ee077c5', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majority_coalition).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, electorate_majority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, supreme_court_judiciary).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, minority_groups).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, parliamentary_supremacy_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, majoritarian_democracy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the elected legislative body, it holds the power to interpret and amend Basic Laws by simple majority, and to override judicial review. This reading grants it unconstrained legislative power within the constitutional framework.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majority_coalition, agenda_setter,
    institutional, biographical, mobile, national).

% Under this reading, the judiciary's role in reviewing Basic Laws is advisory, and its decisions can be overridden by a simple Knesset majority. Its authority is subordinate to the legislature's will.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, supreme_court_judiciary, payer,
    institutional, generational, constrained, national).

% Benefits from the direct translation of its electoral mandate into law without judicial impediment, reflecting the principle of majoritarian democracy.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, electorate_majority, beneficiary,
    organized, biographical, mobile, national).

% Their rights and protections, if not explicitly enshrined in Basic Laws in a way that prevents simple majority amendment, are vulnerable to legislative changes without judicial recourse.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, minority_groups, payer,
    powerless, generational, trapped, national).

% Observes the Israeli constitutional system, potentially raising concerns about human rights and democratic norms if judicial review is effectively nullified, but has no direct enforcement power over the Knesset.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, international_legal_community, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that the will of the elected legislature, representing the majority of the electorate, is the ultimate determinant of law and constitutional interpretation, providing clear lines of authority.
% TRANSFER_FUNCTION: Transfers ultimate interpretive and legislative power over Basic Laws from any potential judicial or external constraint directly to the Knesset, and by extension, to the current governing majority.
% ABSENT_VOICES: Advocates for robust judicial review, constitutional supremacy, and minority rights would object, arguing that this reading undermines checks and balances and endangers vulnerable populations. They are present in public discourse but lack the institutional power to alter this reading's structural implications.
% DISAPPEARANCE_RATIONALE: If this reading of parliamentary sovereignty vanished, the balance of power between the Knesset and the Supreme Court would fundamentally shift, likely empowering judicial review and introducing new constraints on legislative action, leading to a significant rearrangement of the legal and political landscape.
% FOUNDING_PROBLEM: The need to establish a clear and unambiguous source of ultimate legal authority in a parliamentary democracy, ensuring that the elected representatives have the final say in shaping the nation's laws and constitutional framework.
% FOUNDING_PROBLEM_CORROBORATION: Proponents within the Knesset and segments of the electorate attest that the problem of ensuring legislative supremacy remains live, particularly in the face of perceived judicial overreach. This view is contested by legal scholars and opposition parties who argue for a more balanced distribution of power, but the core principle of parliamentary sovereignty is deeply rooted in the political tradition.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, ExtMetricName, E),
    domain_priors:suppression_score(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(basic_law_interpretive_boundary__parliamentary_sovereignty_reading),
    narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very low (0.05) because, from this reading's perspective, the Knesset is merely exercising its inherent, unconstrained sovereign power; it is not 'extracting' from a higher authority it is meant to be constrained by. Suppression is also low (0.1) as there are no structural mechanisms within this reading to suppress the Knesset's will. Theater ratio is minimal (0.05) because the actions taken are direct expressions of this claimed sovereignty, not performative maintenance of an atrophied function. Accessibility collapse is high (0.9) because, if this reading holds, there are virtually no legal alternatives to the Knesset's ultimate authority. Resistance is low (0.15) because, within this framework, challenges to Knesset's ultimate authority are seen as challenges to the constitutional order itself, rather than legitimate resistance to an extractive constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Knesset majority, this is a fundamental principle of democratic governance, ensuring the will of the people is supreme. From the perspective of the judiciary or minority groups, this reading could be seen as highly extractive, as it removes checks on legislative power and potentially exposes fundamental rights to simple majority rule. However, this story is authored strictly from the 'parliamentary sovereignty' reading, where such 'extraction' is not recognized as such, but rather as the legitimate exercise of sovereign power.
 *
 * DIRECTIONALITY LOGIC:
 *   The Knesset majority coalition and the electorate majority are clear beneficiaries, as their legislative will is unconstrained. The Supreme Court judiciary and minority groups are targets, as their power and protections are subordinated to the Knesset's will. The international legal community is an observer, with no direct structural impact on this reading's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint, under the parliamentary sovereignty reading, is presented as a foundational principle, not a temporary arrangement. Therefore, the concept of mandatrophy (an arrangement outliving its function) does not apply in the same way it would to a constructed constraint. Its persistence is tied to the enduring belief in legislative supremacy. The classification as a Mountain reflects its claimed naturalness and immutability within this specific constitutional interpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_sovereignty,
    'Is the Knesset''s ultimate authority over Basic Laws a genuine ''natural law'' of this constitutional system, or a constructed interpretation that benefits identifiable agents?',
    'Analysis of historical constitutional debates, founding documents, and comparative constitutional practice to determine if this interpretation is universally accepted as an irreducible feature or if it''s a contested political choice.',
    'If found to be a constructed interpretation, the constraint would be reclassified from Mountain to a more extractive type (e.g., Tangled Rope or Snare), reflecting the benefits to the Knesset majority and the costs to other actors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_sovereignty, conceptual, 'Ambiguity between inherent constitutional principle and political construction.').

omega_variable(
    judicial_review_legitimacy,
    'Does the ''parliamentary sovereignty'' reading genuinely reflect the original intent and evolving constitutional practice regarding judicial review, or does it suppress a legitimate role for the judiciary?',
    'Legal-historical analysis of the Basic Laws'' drafting, subsequent judicial interpretations, and public acceptance of judicial review''s scope. Comparative analysis with other parliamentary democracies.',
    'If it''s found to suppress a legitimate judicial role, the ''suppression'' metric would increase, and the constraint''s classification would shift towards a more extractive type, as it would be actively suppressing an alternative constitutional interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_review_legitimacy, empirical, 'The extent to which judicial review is a legitimate, suppressed alternative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t1948, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 1948, 0.05).
narrative_ontology:measurement(basi_tr_t1970, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(basi_tr_t1990, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(basi_tr_t2010, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(basi_tr_t2024, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(basi_be_t1948, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 1948, 0.05).
narrative_ontology:measurement(basi_be_t1970, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 1970, 0.05).
narrative_ontology:measurement(basi_be_t1990, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 1990, 0.05).
narrative_ontology:measurement(basi_be_t2010, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2010, 0.05).
narrative_ontology:measurement(basi_be_t2024, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t1948, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 1948, 0.1).
narrative_ontology:measurement(basi_su_t1970, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 1970, 0.1).
narrative_ontology:measurement(basi_su_t1990, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 1990, 0.1).
narrative_ontology:measurement(basi_su_t2010, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2010, 0.1).
narrative_ontology:measurement(basi_su_t2024, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary__balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'basic_law_interpretive_boundary' kernel, each representing a distinct interpretation of the Knesset's authority over Basic Laws. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
