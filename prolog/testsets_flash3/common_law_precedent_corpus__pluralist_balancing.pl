% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__pluralist_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__pluralist_balancing, []).

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
 *   constraint_id: common_law_precedent_corpus__pluralist_balancing
 *   human_readable: Common Law Precedent (Pluralist Balancing Reading)
 *   domain: legal/jurisprudence
 *
 * SUMMARY:
 *   This constraint describes the 'pluralist balancing' reading of common law
 *   precedent, where the weight of prior rulings varies by legal domain and
 *   context, requiring judges to balance stability with adaptation on a
 *   case-by-case basis. This reading acknowledges the dynamic nature of law,
 *   but its inherent flexibility can lead to unpredictable outcomes and
 *   increased costs for litigants. It is one of several competing
 *   interpretations of how precedent should function within the common law
 *   system.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__pluralist_balancing, 0.65).
domain_priors:suppression_score(common_law_precedent_corpus__pluralist_balancing, 0.7).
domain_priors:theater_ratio(common_law_precedent_corpus__pluralist_balancing, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, extractiveness, 0.65).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__pluralist_balancing, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__pluralist_balancing, "Common Law Precedent (Pluralist Balancing Reading)").
narrative_ontology:topic_domain(common_law_precedent_corpus__pluralist_balancing, "legal/jurisprudence").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__pluralist_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__pluralist_balancing, '626a090d-2de3-4709-a781-4962186e905a').
narrative_ontology:cs_kernel_codification('626a090d-2de3-4709-a781-4962186e905a', formalized).
narrative_ontology:cs_authority_grounding('626a090d-2de3-4709-a781-4962186e905a', lineage).
narrative_ontology:cs_interpretation_layer_present('626a090d-2de3-4709-a781-4962186e905a').
narrative_ontology:cs_reading_relation('626a090d-2de3-4709-a781-4962186e905a', common_law_precedent_corpus__strict_stare_decisis, coexists_with).
narrative_ontology:cs_reading_relation('626a090d-2de3-4709-a781-4962186e905a', common_law_precedent_corpus__evolutionary_framework, coexists_with).
narrative_ontology:cs_axiom('626a090d-2de3-4709-a781-4962186e905a', foundational, contextual_precedent_weight).
narrative_ontology:cs_axiom_status(contextual_precedent_weight, holdable).
narrative_ontology:cs_axiom_grounding('626a090d-2de3-4709-a781-4962186e905a', contextual_precedent_weight, conventional).
narrative_ontology:cs_axiom('626a090d-2de3-4709-a781-4962186e905a', foundational, balancing_stability_and_adaptation).
narrative_ontology:cs_axiom_status(balancing_stability_and_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('626a090d-2de3-4709-a781-4962186e905a', balancing_stability_and_adaptation, instrumental).
narrative_ontology:cs_reference_frame('626a090d-2de3-4709-a781-4962186e905a', dynamic_common_law_tradition).
narrative_ontology:cs_drift_state('626a090d-2de3-4709-a781-4962186e905a', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('626a090d-2de3-4709-a781-4962186e905a', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, appellate_courts).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, legal_profession).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, litigants).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, lower_courts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These courts interpret and apply precedent, deciding when to follow, distinguish, or overturn prior rulings. They benefit from the flexibility to adapt law to new circumstances while maintaining a semblance of stability, but also bear the burden of justifying departures from precedent.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, appellate_courts, agenda_setter,
    institutional, generational, constrained, national).

% Bound by the rulings of higher courts, lower courts must apply precedent even when its weight is ambiguous or its application leads to perceived injustice. They bear the cost of navigating complex and sometimes contradictory precedents, with limited power to challenge them.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, lower_courts, payer,
    organized, biographical, constrained, regional).

% Parties to legal disputes who rely on the predictability of law but often face high costs and uncertain outcomes due to the variable weight of precedent and the need for extensive legal analysis to argue for or against its application in their specific case.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, litigants, payer,
    powerless, immediate, trapped, local).

% Lawyers and scholars who specialize in interpreting and arguing precedent. They benefit from the complexity and interpretive demands of the system, which creates a continuous need for their expertise, but also face the challenge of advising clients in an environment of variable legal certainty.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, legal_profession, beneficiary,
    organized, biographical, constrained, national).

% The legislative body can create new statutes that override or clarify common law precedents. They observe the evolution of common law and intervene when judicial interpretations diverge too far from public policy or create unintended consequences, but do not directly administer precedent.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, legislature, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for judicial decision-making that balances legal stability (predictability) with the need for adaptation to new social, economic, and technological realities, preventing arbitrary rulings and ensuring a degree of consistency over time.
% TRANSFER_FUNCTION: Transfers interpretive authority and the associated costs of legal uncertainty from individual litigants and lower courts to appellate courts and the legal profession, in exchange for a dynamic, albeit complex, body of law.
% ABSENT_VOICES: Citizens seeking clear, unambiguous legal guidance often find the pluralist balancing approach opaque and unpredictable. Their voices, advocating for simpler, more codified law, are often marginalized in the highly specialized discourse of legal interpretation.
% DISAPPEARANCE_RATIONALE: If the pluralist balancing approach to precedent vanished, the legal system would descend into chaos. Each case would be decided de novo, leading to inconsistent rulings, a collapse of legal predictability, and an inability for individuals and businesses to plan their affairs, necessitating a complete overhaul of judicial process.
% FOUNDING_PROBLEM: The need for a legal system that could evolve with society while maintaining a core of fairness and predictability, avoiding both rigid stagnation and arbitrary judicial fiat.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and practitioners widely attest that the tension between stability and adaptation remains a live problem in jurisprudence. While the specific mechanisms of balancing are debated, the underlying need for such a framework is broadly acknowledged outside of any single benefiting party.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__pluralist_balancing, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__pluralist_balancing, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__pluralist_balancing, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(common_law_precedent_corpus__pluralist_balancing, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__pluralist_balancing, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__pluralist_balancing_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__pluralist_balancing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__pluralist_balancing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) stems from the high transaction costs and uncertainty imposed on litigants and lower courts by the need for extensive, context-specific legal analysis. Suppression (0.70) is high because lower courts and litigants have limited options to challenge or bypass the interpretive authority of appellate courts. Theater ratio (0.20) is relatively low, as the balancing act is a genuine, active process, though it can sometimes be used to rationalize desired outcomes. The metrics reflect the structural delta: medium rigidity, multi-tier extractiveness, and unpredictable domain-switching costs.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of appellate courts, this reading of precedent is a necessary and sophisticated mechanism for legal evolution. From the perspective of litigants, it can appear as an arbitrary and costly system where outcomes are difficult to predict, and the 'balancing' often favors institutional stability over individual justice. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Appellate courts and the legal profession are beneficiaries, gaining interpretive authority and demand for their expertise, respectively. Litigants and lower courts are payers, bearing the costs of uncertainty and the burden of navigating complex legal arguments. The legislature acts as an observer, capable of intervening but not directly participating in the day-to-day application of precedent.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_flexibility_vs_arbitrariness,
    'At what point does the ''pluralist balancing'' of precedent''s weight transition from necessary interpretive flexibility to arbitrary judicial discretion?',
    'Empirical analysis of judicial decisions over time, measuring the variance in outcomes for similar cases across different domains and the consistency of justifications for distinguishing or overturning precedent.',
    'If the variance is high and justifications are inconsistent, it would suggest the system is more arbitrary than flexible, increasing extractiveness and suppression for litigants. If consistent, it would support the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_flexibility_vs_arbitrariness, empirical, 'Distinguishing genuine balancing from unconstrained discretion.').

omega_variable(
    domain_specificity_justification,
    'Are the distinctions between legal domains, which justify varying precedent weight, genuinely structural or are they socially constructed and maintained for institutional convenience?',
    'Comparative legal analysis across jurisdictions with different domain categorizations, and historical analysis of how legal domains have been defined and re-defined over time.',
    'If domain distinctions are arbitrary, the ''pluralist balancing'' becomes a mechanism for institutional actors to selectively apply or disregard precedent, increasing extraction from those who cannot navigate these shifting boundaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_specificity_justification, conceptual, 'The structural vs. constructed nature of legal domain boundaries.').

omega_variable(
    litigant_access_to_interpretive_resources,
    'To what extent does the complexity of ''pluralist balancing'' disproportionately burden litigants with fewer resources to access high-quality legal expertise?',
    'Socio-economic analysis of litigation outcomes, correlating legal resource disparities with success rates in cases involving complex precedent interpretation.',
    'If resource disparities strongly predict outcomes, the constraint''s effective extractiveness and suppression for powerless litigants are higher than the base metrics suggest, indicating a deeper structural inequality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(litigant_access_to_interpretive_resources, empirical, 'Impact of interpretive complexity on access to justice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__pluralist_balancing, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0, 0.15).
narrative_ontology:measurement(comm_tr_t10, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 10, 0.17).
narrative_ontology:measurement(comm_tr_t20, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 20, 0.18).
narrative_ontology:measurement(comm_tr_t30, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 30, 0.19).
narrative_ontology:measurement(comm_tr_t40, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 40, 0.2).
narrative_ontology:measurement(comm_tr_t50, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(comm_be_t10, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(comm_be_t20, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(comm_be_t30, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(comm_be_t40, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(comm_be_t50, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(comm_su_t10, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(comm_su_t20, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(comm_su_t30, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(comm_su_t40, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(comm_su_t50, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__pluralist_balancing, enforcement_mechanism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus__strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus__evolutionary_framework).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'common_law_precedent_corpus' kernel. Each reading represents a distinct structural claim about how precedent operates, with different extractiveness and stakeholder dynamics. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
