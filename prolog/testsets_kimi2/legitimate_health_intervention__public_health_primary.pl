% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__public_health_primary, []).

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
 *   constraint_id: legitimate_health_intervention__public_health_primary
 *   human_readable: Public Health Primary Legitimacy Reading
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint story instantiates the public_health_primary reading of
 *   the contested kernel legitimate_health_intervention. Under this reading,
 *   the legitimacy of coercive health interventionsâvaccination mandates,
 *   access restrictions, employment conditioningâderives solely from
 *   measurable reductions in population-level morbidity and mortality.
 *   Individual refusal is reframed as externality imposition rather than
 *   autonomous choice. The constraint coordinates population-level
 *   protection, especially for the immunocompromised, while extracting bodily
 *   autonomy and economic participation from unvaccinated adults through
 *   active enforcement. The sibling readings (bodily_autonomy_primary,
 *   proportionality_reading) are treated as separate constraints per the
 *   epsilon-invariance principle.
 *
 * KEY AGENTS:
 *   - public_health_authority: agenda_setter (institutional/constrained) â sets mandates and enforces compliance, derives legitimacy from morbidity metrics
 *   - immunocompromised_population: beneficiary (powerless/trapped) â receives protection without administering the constraint
 *   - unvaccinated_adults: payer (moderate/constrained) â bears costs of exclusion and bodily coercion
 *   - civil_liberties_observers: observer (organized/analytical) â challenges the enforcement apparatus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, 0.72).
domain_priors:suppression_score(legitimate_health_intervention__public_health_primary, 0.78).
domain_priors:theater_ratio(legitimate_health_intervention__public_health_primary, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, extractiveness, 0.72).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__public_health_primary, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__public_health_primary, "Public Health Primary Legitimacy Reading").
narrative_ontology:topic_domain(legitimate_health_intervention__public_health_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__public_health_primary, '58148aa9-fae6-4d9f-9ac9-3993d3a8af68').
narrative_ontology:cs_kernel_codification('58148aa9-fae6-4d9f-9ac9-3993d3a8af68', formalized).
narrative_ontology:cs_authority_grounding('58148aa9-fae6-4d9f-9ac9-3993d3a8af68', expertise).
narrative_ontology:cs_interpretation_layer_present('58148aa9-fae6-4d9f-9ac9-3993d3a8af68').
narrative_ontology:cs_reading_relation('58148aa9-fae6-4d9f-9ac9-3993d3a8af68', legitimate_health_intervention__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('58148aa9-fae6-4d9f-9ac9-3993d3a8af68', legitimate_health_intervention__proportionality_reading, influences).
narrative_ontology:cs_axiom('58148aa9-fae6-4d9f-9ac9-3993d3a8af68', foundational, population_health_supersedes_individual_consent).
narrative_ontology:cs_axiom_status(population_health_supersedes_individual_consent, holdable).
narrative_ontology:cs_axiom_grounding('58148aa9-fae6-4d9f-9ac9-3993d3a8af68', population_health_supersedes_individual_consent, empirically_contingent).
narrative_ontology:cs_axiom('58148aa9-fae6-4d9f-9ac9-3993d3a8af68', secondary, unvaccinated_status_as_externality).
narrative_ontology:cs_axiom_status(unvaccinated_status_as_externality, holdable).
narrative_ontology:cs_axiom_grounding('58148aa9-fae6-4d9f-9ac9-3993d3a8af68', unvaccinated_status_as_externality, empirically_contingent).
narrative_ontology:cs_reference_frame('58148aa9-fae6-4d9f-9ac9-3993d3a8af68', population_health_maximization).
narrative_ontology:cs_drift_state('58148aa9-fae6-4d9f-9ac9-3993d3a8af68', post_emergency_normalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('58148aa9-fae6-4d9f-9ac9-3993d3a8af68', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__public_health_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, immunocompromised_population).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, unvaccinated_adults).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes vaccination mandates and access restrictions, enforcing compliance through employment termination and exclusion from public accommodations. Derives institutional legitimacy from published morbidity and mortality reductions. Frames individual refusal as a quantifiable externality imposed on the collective.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, public_health_authority, agenda_setter,
    institutional, generational, constrained, national).

% Cannot mount protective immune responses to many vaccines and depend on community transmission suppression for survival. Receive protection from reduced disease circulation without administering or enforcing the constraint.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, immunocompromised_population, beneficiary,
    powerless, biographical, trapped, national).

% Face loss of employment, exclusion from public spaces, and social sanctions for refusing vaccination. Bear the direct cost of compliance or the cost of economic and social exclusion. Framed by the constraint as vectors of externality rather than rights-bearing decision-makers.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, unvaccinated_adults, payer,
    moderate, biographical, constrained, national).

% Monitor, litigate, and publicly challenge the coercive enforcement apparatus, arguing that bodily autonomy and constitutional protections limit state power regardless of epidemiological outcomes.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, civil_liberties_observers, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieves population-level disease suppression that individual voluntary action cannot secure, particularly protecting those who cannot be directly immunized.
% TRANSFER_FUNCTION: Moves bodily autonomy, economic participation, and spatial mobility from unvaccinated adults to population-level morbidity reduction, enforced through employment termination and access restrictions.
% ABSENT_VOICES: Bodily autonomy advocates and unvaccinated individuals are heard in opposition or litigation but are structurally excluded from the policy design frame; their objections are treated as noise or externality rather than legitimate input.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, disease incidence would rise among the unvaccinated and immunocompromised, the institutional apparatus of public health enforcement would lose its primary legitimacy anchor, and the social compact around collective health would reorganize around individual choice and voluntary measures.
% FOUNDING_PROBLEM: Preventing epidemic disease transmission in densely connected populations where individual non-compliance creates externalities that disproportionately harm the immunocompromised and vulnerable.
% FOUNDING_PROBLEM_CORROBORATION: Independent epidemiologists and historians of public health attest the externality problem; constitutional courts and civil liberties organizations outside the immunocompromised beneficiary set attest that the current enforcement intensity and scope exceed what the founding problem justifies.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_health_intervention__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__public_health_primary, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because enforcement mechanisms impose severe costs on individual refusers; suppression (0.78) is higher still because the constraint persists only through active employment termination and access restrictions, not voluntary uptake. Theater ratio (0.35 and rising) indicates growing performative enforcement post-emergency: some restrictions persist after the acute epidemiological signal has faded, suggesting early Goodhart drift. Accessibility collapse (0.70) reflects that meaningful alternatives (unvaccinated participation in normal economic life) are largely closed. Resistance (0.58) captures sustained anti-mandate litigation and political mobilization. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The public_health_authority seat experiences the constraint as legitimate, life-saving coordination; the unvaccinated_adult seat experiences it as state extraction of bodily autonomy backed by economic sanctions. The immunocompromised seat experiences protection. The engine computes this divergence from the structural asymmetry in declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   public_health_authority derives institutional authority and expanded capacity from the constraint, placing its directionality near the beneficiary end (low d). immunocompromised_population is a pure beneficiary (very low d). unvaccinated_adults are pure targets (high d): they bear extraction directly, and their exit options are limited to compliance, exclusion, or costly jurisdictional departure. civil_liberties_observers are analytical with no directional stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not a snare because the population-level coordination functionâprotecting the immunocompromised through reduced transmissionâis structurally genuine and not merely cover. It is not a rope because the enforcement is asymmetrically borne by unvaccinated adults. Mandatrophy would be signaled by a founding_problem_status of dead combined with persistent world_rearranges disappearance; here the status is contested, and the temporal series show extraction remains coupled to enforcement capacity, though theater_ratio rise suggests drift toward performance. The classification as tangled_rope captures the hybrid reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    externality_causation_validity,
    'Does measurable population-level morbidity reduction causally depend on the specific enforcement mechanisms (employment termination, access restrictions), or would high voluntary uptake achieve equivalent coordination?',
    'Comparative natural experiments across jurisdictions with voluntary campaigns versus mandate regimes, holding baseline healthcare capacity constant.',
    'If voluntary uptake suffices, the enforcement mechanism is largely extractive overhead on a real coordination problem; if mandates are uniquely necessary, extraction is the price of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_causation_validity, empirical, 'Whether enforcement is causally necessary for the coordination outcome.').

omega_variable(
    kernel_reading_contest,
    'This constraint is the public_health_primary reading of legitimate_health_intervention; if bodily_autonomy_primary were dominant, unvaccinated adults would shift from the victim set to autonomous rights-holders. Which reading governs determines classification of the same empirical population.',
    'Jurisdictional mapping of which kernel reading is encoded in positive law and administrative practice.',
    'The kernel is not epsilon-invariant; sibling readings produce mutually exclusive stakeholder classifications for the same natural-language policy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel reading indeterminacy affecting victim and beneficiary classification.').

omega_variable(
    enforcement_proportionality,
    'Have the enforcement mechanisms decoupled from the underlying epidemiological signal, persisting beyond measurable morbidity reduction thresholds?',
    'Time-series analysis correlating enforcement intensity with disease prevalence, variant severity, and healthcare utilization.',
    'If decoupled, the constraint trends toward snare; if tightly coupled, it remains a tangled rope with genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_proportionality, empirical, 'Enforcement decoupling from epidemiological signal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__public_health_primary, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_tr_t0, legitimate_health_intervention__public_health_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_tr_t6, legitimate_health_intervention__public_health_primary, theater_ratio, 6, 0.2).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_tr_t12, legitimate_health_intervention__public_health_primary, theater_ratio, 12, 0.3).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_tr_t18, legitimate_health_intervention__public_health_primary, theater_ratio, 18, 0.35).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_tr_t24, legitimate_health_intervention__public_health_primary, theater_ratio, 24, 0.38).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_tr_t30, legitimate_health_intervention__public_health_primary, theater_ratio, 30, 0.4).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_tr_t36, legitimate_health_intervention__public_health_primary, theater_ratio, 36, 0.42).

% Extraction over time
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_be_t0, legitimate_health_intervention__public_health_primary, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_be_t6, legitimate_health_intervention__public_health_primary, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_be_t12, legitimate_health_intervention__public_health_primary, base_extractiveness, 12, 0.72).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_be_t18, legitimate_health_intervention__public_health_primary, base_extractiveness, 18, 0.75).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_be_t24, legitimate_health_intervention__public_health_primary, base_extractiveness, 24, 0.7).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_be_t30, legitimate_health_intervention__public_health_primary, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_be_t36, legitimate_health_intervention__public_health_primary, base_extractiveness, 36, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_su_t0, legitimate_health_intervention__public_health_primary, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_su_t6, legitimate_health_intervention__public_health_primary, suppression_requirement, 6, 0.75).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_su_t12, legitimate_health_intervention__public_health_primary, suppression_requirement, 12, 0.88).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_su_t18, legitimate_health_intervention__public_health_primary, suppression_requirement, 18, 0.85).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_su_t24, legitimate_health_intervention__public_health_primary, suppression_requirement, 24, 0.8).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_su_t30, legitimate_health_intervention__public_health_primary, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_su_t36, legitimate_health_intervention__public_health_primary, suppression_requirement, 36, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
