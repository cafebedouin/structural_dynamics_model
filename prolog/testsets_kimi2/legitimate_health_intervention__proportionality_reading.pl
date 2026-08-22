% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__proportionality_reading, []).

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
 *   constraint_id: legitimate_health_intervention__proportionality_reading
 *   human_readable: Proportionality-Based Legitimacy for Public Health Interventions
 *   domain: public health policy / medical ethics / constitutional law
 *
 * SUMMARY:
 *   This constraint instantiates the proportionality_reading of the
 *   legitimate_health_intervention kernel: a formalized legal doctrine
 *   holding that public health coercion is legitimate only when intervention
 *   severity is proportionate to disease threat level, with individual
 *   autonomy and population harm both weighted by disease characteristics
 *   (transmissibility, case-fatality rate). The constraint operates as a
 *   conditional authorization structureâcoercion is permitted and enforced
 *   when empirical threat crosses a judicially reviewed threshold, but the
 *   victim set and epsilon scale with the severity of the disease in
 *   question. The standing arrangement under contest is the proportionality
 *   framework as it actually functions in constitutional and public health
 *   law, not the idealized version.
 *
 * KEY AGENTS:
 *   - public_health_authorities (agenda_setter, institutional/constrained): design interventions and construct the evidentiary threat profile that proportionality review evaluates
 *   - constitutional_courts (agenda_setter, institutional/analytical): interpret and apply proportionality tests, authorizing or invalidating coercive measures
 *   - individuals_subject_to_intervention (payer, moderate/constrained): bear the autonomy costs of quarantine, mandatory vaccination, or other mandates permitted by proportionality findings
 *   - communities_at_risk (beneficiary, moderate/constrained): receive protective benefit from disease-control measures whose legitimacy is secured through proportionality review
 *   - civil_liberties_advocates (observer, organized/mobile): challenge proportionality determinations and argue that review has become performative during emergencies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__proportionality_reading, 0.55).
domain_priors:suppression_score(legitimate_health_intervention__proportionality_reading, 0.6).
domain_priors:theater_ratio(legitimate_health_intervention__proportionality_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__proportionality_reading, "Proportionality-Based Legitimacy for Public Health Interventions").
narrative_ontology:topic_domain(legitimate_health_intervention__proportionality_reading, "public health policy / medical ethics / constitutional law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__proportionality_reading, '73f1c403-a326-4ed3-9d14-a2f1eaa91b43').
narrative_ontology:cs_kernel_codification('73f1c403-a326-4ed3-9d14-a2f1eaa91b43', formalized).
narrative_ontology:cs_authority_grounding('73f1c403-a326-4ed3-9d14-a2f1eaa91b43', lineage).
narrative_ontology:cs_interpretation_layer_present('73f1c403-a326-4ed3-9d14-a2f1eaa91b43').
narrative_ontology:cs_reading_relation('73f1c403-a326-4ed3-9d14-a2f1eaa91b43', legitimate_health_intervention__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('73f1c403-a326-4ed3-9d14-a2f1eaa91b43', legitimate_health_intervention__public_health_primary, influences).
narrative_ontology:cs_axiom('73f1c403-a326-4ed3-9d14-a2f1eaa91b43', foundational, proportionality_governs_health_intervention_legitimacy).
narrative_ontology:cs_axiom_status(proportionality_governs_health_intervention_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('73f1c403-a326-4ed3-9d14-a2f1eaa91b43', proportionality_governs_health_intervention_legitimacy, conventional).
narrative_ontology:cs_axiom('73f1c403-a326-4ed3-9d14-a2f1eaa91b43', foundational, disease_severity_scales_autonomy_weight).
narrative_ontology:cs_axiom_status(disease_severity_scales_autonomy_weight, holdable).
narrative_ontology:cs_axiom_grounding('73f1c403-a326-4ed3-9d14-a2f1eaa91b43', disease_severity_scales_autonomy_weight, conventional).
narrative_ontology:cs_reference_frame('73f1c403-a326-4ed3-9d14-a2f1eaa91b43', classical_proportionality_review).
narrative_ontology:cs_drift_state('73f1c403-a326-4ed3-9d14-a2f1eaa91b43', post_covid_emergency_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('73f1c403-a326-4ed3-9d14-a2f1eaa91b43', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, communities_at_risk).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, individuals_subject_to_intervention).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design disease-control interventions and construct the epidemiological threat profile that courts evaluate under proportionality review. They must justify severity by citing transmissibility, case-fatality rate, and healthcare capacity. Their exit is constrained by legal mandates, political oversight, and the requirement to maintain institutional legitimacy.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Review whether public health interventions satisfy proportionality tests: legitimate aim, suitability, necessity, and proportionality stricto sensu. Their rulings create binding precedents that determine which disease characteristics justify which intervention severities. They operate through interpretive tradition and legal reasoning, not empirical fieldwork.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Are subjected to mandatory vaccination, quarantine, isolation, or movement restrictions when courts deem the intervention proportionate to disease threat. They may challenge measures in court or seek exemptions, but emergency procedures and judicial deference make successful exit rare during active outbreaks. Their autonomy is the direct cost of the proportionality authorization.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, individuals_subject_to_intervention, payer,
    moderate, immediate, constrained, local).

% Receive protective benefit from population-level disease control that proportionality review authorizes. Their elevated risk profile is the empirical input that raises the threat level and justifies overriding individual autonomy. They do not administer the constraint but depend on its output for reduced exposure.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, communities_at_risk, beneficiary,
    moderate, biographical, constrained, national).

% Monitor proportionality jurisprudence and litigate or campaign against measures they judge disproportionate. They argue that emergency proportionality review has become a performative rubber stamp and that disease characteristics are manipulated to justify predetermined policies. They represent autonomy interests not captured by population-level risk calculus.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, civil_liberties_advocates, observer,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared legal framework for determining when state coercion in public health is legitimate, resolving coordination failure between the state's protective capacity and individual rights claims by setting a conditional threshold based on disease severity.
% TRANSFER_FUNCTION: Moves decisional authority over bodily autonomy from individuals to public health institutions and courts when empirical disease threat crosses a proportionality threshold; moves legal legitimacy from the judiciary to executive health agencies when courts defer to epidemiological assessments.
% ABSENT_VOICES: Individuals who would refuse even demonstrably proportionate interventions on philosophical, religious, or bodily-integrity grounds are often excluded from the proportionality calculus once threat levels are established; future populations who bear the precedent cost of expanded emergency powers are not represented in the current review.
% DISAPPEARANCE_RATIONALE: If the proportionality framework vanished overnight, public health interventions would lose their dominant legitimacy architecture. States would face a binary choice between unchecked coercion (inviting mass resistance and legal chaos) and paralysis (abandoning population-level protection). The legal and political landscape of emergency powers would reorganize around either an autonomy-absolute or public-health-supremacy framing.
% FOUNDING_PROBLEM: How to legitimize necessary but coercive public health measures during infectious disease emergencies without authorizing unlimited state power over the body.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional courts and regional human rights tribunals attest to the need for limiting state power from outside the public health beneficiary set. Civil liberties organizations corroborate that unlimited emergency authority remains a live threat. Public health institutions self-assert the continued necessity of the problem.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_health_intervention__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__proportionality_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.55 (mid-range, rising over the interval) because proportionality review authorizes real autonomy extraction from individuals subject to intervention, but the amount of extraction is bounded by the threat-level condition. Suppression is 0.60 because the constraint's operation depends on enforcement of mandates against non-compliant individuals. Theater ratio rises to 0.30 because during emergencies judicial deference often converts proportionality review into a legitimizing ritual rather than a genuine constraint. Resistance is high (0.70) because anti-mandate movements and civil liberties groups actively contest the framework. Accessibility collapse is moderate (0.45) because the pure-autonomy and pure-public-health alternatives remain politically visible. Measurements share one time grid to prevent misaligned drift dating.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (public health authorities, courts) experience the constraint as a necessary coordination mechanism that prevents arbitrary state action while permitting protective measures. The payer seat (individuals subject to intervention) experiences the same structure as the legal mechanism that licenses overriding their bodily autonomy. The beneficiary seat (communities at risk) experiences it as protective subsidy. The engine computes these divergent seat-level classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Communities_at_risk occupy the beneficiary end of the directionality spectrum: the constraint subsidizes their protection by authorizing population-level interventions. Individuals_subject_to_intervention occupy the target end: their autonomy is the resource extracted when proportionality is satisfied. Public health authorities and courts sit near the beneficiary side as administrators who gain legitimacy and enforcement capacity from the framework. Civil liberties advocates sit near symmetric or mildly target-side as they bear the cost of contesting state action without directly receiving the coordination benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a pure Rope would miss the real extraction of autonomy from mandated individuals. Classifying it as a pure Snare would miss the genuine coordination function: proportionality review solves the collective-action problem of establishing shared legitimacy standards for emergency coercion, preventing both paralysis and unlimited state power. Tangled Rope is the only category that preserves both the coordination role (beneficiaries exist, a real collective problem is solved) and the asymmetric extraction (victims exist, their autonomy is taken). The conditional structureâwhere victimhood and epsilon scale with disease severityâis what distinguishes this from a uniform extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the proportionality_reading of the kernel legitimate_health_intervention. How do sibling readings (public_health_primary, bodily_autonomy_primary) alter the structural classification?',
    'Comparative analysis across the three authored constraint stories for this kernel; examining victim-set variation and epsilon scaling.',
    'If the proportionality reading is the only one with conditional victim sets, it confirms the kernel''s inherent framing-sensitivity; if all readings converge on similar victim structures, the kernel may be a single constraint mislabeled as three.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame omega: position of this reading within the contested kernel').

omega_variable(
    disease_severity_extraction_variation,
    'Does the constraint''s effective extractiveness genuinely vary with disease transmissibility and case-fatality rate, or is proportionality invoked uniformly regardless of empirical threat level?',
    'Cross-jurisdictional comparative review of proportionality jurisprudence: measure intervention severity against empirical CFR/R0 for the disease at issue in each case.',
    'If proportionality is applied uniformly, the conditional structure is theater and extraction is higher than authored; if it genuinely scales, the authored epsilon reflects the constraint''s intended operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disease_severity_extraction_variation, empirical, 'Whether disease characteristics actually modulate extraction or serve as post-hoc rationalization').

omega_variable(
    proportionality_review_efficacy,
    'Does proportionality review function as a genuine constraint on state power, or as a legitimizing veneer that rubber-stamps predetermined public health measures?',
    'Quantitative analysis of judicial outcomes: rate of intervention invalidation on proportionality grounds, controlling for disease severity and political context.',
    'If review is largely performative, theater_ratio and suppression rise toward snare territory; if genuinely constraining, the coordination function dominates and extraction is bounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_review_efficacy, empirical, 'Whether proportionality limits extraction or legitimizes it').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__proportionality_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(leg_health_prop_tr_t0, legitimate_health_intervention__proportionality_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(leg_health_prop_tr_t8, legitimate_health_intervention__proportionality_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(leg_health_prop_tr_t16, legitimate_health_intervention__proportionality_reading, theater_ratio, 16, 0.15).
narrative_ontology:measurement(leg_health_prop_tr_t24, legitimate_health_intervention__proportionality_reading, theater_ratio, 24, 0.2).
narrative_ontology:measurement(leg_health_prop_tr_t32, legitimate_health_intervention__proportionality_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(leg_health_prop_tr_t40, legitimate_health_intervention__proportionality_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(leg_health_prop_be_t0, legitimate_health_intervention__proportionality_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(leg_health_prop_be_t8, legitimate_health_intervention__proportionality_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(leg_health_prop_be_t16, legitimate_health_intervention__proportionality_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(leg_health_prop_be_t24, legitimate_health_intervention__proportionality_reading, base_extractiveness, 24, 0.48).
narrative_ontology:measurement(leg_health_prop_be_t32, legitimate_health_intervention__proportionality_reading, base_extractiveness, 32, 0.52).
narrative_ontology:measurement(leg_health_prop_be_t40, legitimate_health_intervention__proportionality_reading, base_extractiveness, 40, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(leg_health_prop_su_t0, legitimate_health_intervention__proportionality_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(leg_health_prop_su_t8, legitimate_health_intervention__proportionality_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(leg_health_prop_su_t16, legitimate_health_intervention__proportionality_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(leg_health_prop_su_t24, legitimate_health_intervention__proportionality_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(leg_health_prop_su_t32, legitimate_health_intervention__proportionality_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(leg_health_prop_su_t40, legitimate_health_intervention__proportionality_reading, suppression_requirement, 40, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, public_health_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel legitimate_health_intervention. The proportionality reading mediates between absolute autonomy and population-supremacy readings by making legitimacy conditional on disease severity. Each reading carries a distinct epsilon, victim structure, and directional profile; they form a constraint family linked through mutual affects_constraints edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
