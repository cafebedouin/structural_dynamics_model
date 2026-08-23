% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__proportionality_reading, []).

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
 *   constraint_id: coercion_legitimacy_boundary__proportionality_reading
 *   human_readable: Proportionate Coercion Legitimacy Framework (Disease-Scaled)
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   The proportionality reading of infectious disease coercion holds that
 *   state-mandated medical intervention is legitimate only when a pathogen
 *   meets severity and transmissibility thresholds. Under this framework,
 *   measles vaccination mandates are justified by high R0 and serious
 *   sequelae, while seasonal flu coercion is not. The constraint is
 *   institutionalized in constitutional public health law and operates as a
 *   scalar adjudication mechanism. It is claimed as a coordination mechanism
 *   protecting vulnerable populations, but extracts bodily autonomy from
 *   mandated individuals during active outbreaks. The constraint has no
 *   sunset clause and is actively enforced through school exclusions,
 *   quarantine orders, and employment conditioning.
 *
 * KEY AGENTS:
 *   - public_health_authorities: Agenda-setter (institutional/constrained) â adjudicates severity thresholds and enforces mandates
 *   - vulnerable_populations: Beneficiary (powerless/trapped) â protected by herd immunity effects
 *   - mandated_individuals: Payer (moderate/constrained) â bear liberty restrictions and bodily intervention costs
 *   - civil_liberties_organizations: Observer (organized/analytical) â litigate and monitor proportionality claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__proportionality_reading, 0.55).
domain_priors:suppression_score(coercion_legitimacy_boundary__proportionality_reading, 0.58).
domain_priors:theater_ratio(coercion_legitimacy_boundary__proportionality_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__proportionality_reading, "Proportionate Coercion Legitimacy Framework (Disease-Scaled)").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__proportionality_reading, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__proportionality_reading, '63d48613-e2f8-4993-9eb3-d05538ce03aa').
narrative_ontology:cs_kernel_codification('63d48613-e2f8-4993-9eb3-d05538ce03aa', formalized).
narrative_ontology:cs_authority_grounding('63d48613-e2f8-4993-9eb3-d05538ce03aa', lineage).
narrative_ontology:cs_interpretation_layer_present('63d48613-e2f8-4993-9eb3-d05538ce03aa').
narrative_ontology:cs_reading_relation('63d48613-e2f8-4993-9eb3-d05538ce03aa', coercion_legitimacy_boundary__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('63d48613-e2f8-4993-9eb3-d05538ce03aa', coercion_legitimacy_boundary__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('63d48613-e2f8-4993-9eb3-d05538ce03aa', foundational, coercion_legitimacy_scales_with_severity).
narrative_ontology:cs_axiom_status(coercion_legitimacy_scales_with_severity, holdable).
narrative_ontology:cs_axiom_grounding('63d48613-e2f8-4993-9eb3-d05538ce03aa', coercion_legitimacy_scales_with_severity, conventional).
narrative_ontology:cs_reference_frame('63d48613-e2f8-4993-9eb3-d05538ce03aa', constitutional_proportionality_doctrine).
narrative_ontology:cs_drift_state('63d48613-e2f8-4993-9eb3-d05538ce03aa', post_covid_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('63d48613-e2f8-4993-9eb3-d05538ce03aa', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, mandated_individuals).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__proportionality_reading, proportionality_doctrine).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__proportionality_reading, police_power_limits).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the epidemiological thresholds for disease severity and transmission that trigger coercive measures. They issue vaccination mandates, quarantine orders, and school exclusions when those thresholds are met, and defend those decisions in courts and legislatures using case law and outbreak data.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Include immunocompromised patients, infants too young for vaccination, and others who depend on herd immunity for protection. They benefit from reduced pathogen circulation when mandates are applied to severe diseases, and typically do not face the direct coercive measures themselves.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, local).

% Ordinary citizens and workers who are required to undergo vaccination, accept quarantine, or face exclusion from public spaces and employment when public health authorities classify a disease as above the severity threshold. They bear the direct costs of liberty restriction and unwanted medical intervention.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, mandated_individuals, payer,
    moderate, biographical, constrained, national).

% Monitor threshold-setting processes, litigate against mandates they view as disproportionate, and advocate for exemption pathways. They operate within the legal system but contest the expansion of coercion to pathogens they argue fall below the true severity threshold.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, civil_liberties_organizations, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates population-level immunity and outbreak prevention by solving free-rider problems in vaccination for high-threat pathogens, aligning individual medical behavior with collective risk thresholds.
% TRANSFER_FUNCTION: Transfers bodily autonomy and liberty from individuals to public health authorities during declared severe outbreaks, moving compliance and medical risk from the individual to the collective framework.
% ABSENT_VOICES: Individuals and movements holding the bodily_autonomy_primary readingâwho reject all medical coercion regardless of severityâare structurally absent from proportionality-based policymaking. Their objections are treated as legally irrelevant once a severity threshold is crossed, though they remain active in public discourse.
% DISAPPEARANCE_RATIONALE: If the proportionality framework vanished, infectious disease policy would lose its scalar adjudication mechanism. Jurisdictions would likely collapse toward categorical prohibition of coercion (empowering bodily autonomy advocates) or untethered collective optimization (empowering public health maximalists), rearranging the legal and institutional landscape for all outbreak response.
% FOUNDING_PROBLEM: Epidemics of severe infectious disease where voluntary compliance is insufficient to prevent catastrophic spread and vulnerable populations face high mortality or morbidity (e.g., pre-vaccine measles, smallpox).
% FOUNDING_PROBLEM_CORROBORATION: Medical historians and epidemiologists attest to pre-vaccine era measles and smallpox mortality. Civil liberties organizations acknowledge the historical problem but contest that proportionate coercion is the correct or ongoing solution, providing corroboration from outside the primary beneficiary set.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__proportionality_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(coercion_legitimacy_boundary__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(coercion_legitimacy_boundary__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) is moderate because coercion is conditional on severity and bounded by legal review; it is not permanent or universal. Suppression (0.58) reflects the active enforcement infrastructure (exclusions, fines, quarantine) that operates when thresholds are met. Theater ratio (0.25) is relatively low because severity assessment is largely functional, though some performative compliance exists. Accessibility collapse (0.42) is moderate: alternatives (voluntary uptake) are partially viable but collapse during outbreaks. Resistance (0.68) is high because anti-mandate movements and civil liberties groups actively contest the framework, particularly post-COVID. Temporal measurements show extraction and suppression spiking during the COVID-19 interval (~t=40) and partially reverting, indicating the constraint's scalar nature is stress-tested by novel pathogens.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities experience the constraint as a necessary coordination tool with clear legal guardrails; mandated individuals experience it as state extraction of bodily autonomy justified by collective risk calculations. The engine computes this divergence from structural data: the agenda-setter has institutional power and constrained exit (bound by law), while payers have moderate power but constrained exit during emergency declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities and vulnerable populations are structural beneficiaries (low d), receiving coordinated protection and expanded authority. Mandated individuals are structural targets (high d), bearing the concentrated costs of liberty restriction. Civil liberties organizations occupy an analytical seat with arbitrage-grade exit (they can litigate but remain within the legal framework).
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality framework contains an internal anti-mandatrophy mechanism: as severity declines, the coercion justification dissolves. However, institutional inertia and threshold manipulation risk creating piton-like persistence. The temporal series shows post-COVID partial reversion, suggesting the anti-mandatrophy mechanism is structurally present but politically contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is one reading of the coercion_legitimacy_boundary kernel (proportionality_reading). The sibling bodily_autonomy_primary reading would structurally eliminate all coercion and dissolve the beneficiary/victim asymmetry; the sibling public_health_primary reading would remove the severity scalar and authorize broader coercion. Where is the disagreement located: in the empirical assessment of severity, or in the normative weight given to autonomy?',
    'Comparative analysis of legal regimes and their outcomes across jurisdictions, tracking whether disagreements resolve with better data or persist as rights-based disputes.',
    'If empirical, the constraint''s epsilon is mutable with better data; if normative, the constraint is a deontological rope/tangled_rope boundary dispute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Structural location of disagreement between kernel readings').

omega_variable(
    threshold_manipulation_risk,
    'Can the severity and transmission thresholds that trigger coercion be captured by institutional actors to lower the threshold and expand the victim set?',
    'Independent audit of threshold-setting processes and epidemiological integrity across public health agencies.',
    'If captured, the constraint''s extractiveness rises toward snare characteristics; if robust, it remains a bounded tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_manipulation_risk, empirical, 'Risk of institutional capture of severity thresholds').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__proportionality_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_tr_t0, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_tr_t10, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_tr_t20, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_tr_t30, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_tr_t40, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_tr_t50, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 50, 0.3).

% Extraction over time
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_be_t0, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_be_t10, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 10, 0.43).
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_be_t20, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_be_t30, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_be_t40, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_be_t50, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 50, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_su_t0, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_su_t10, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_su_t20, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_su_t30, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_su_t40, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(coercion_legitimacy_boundary__proportionality_reading_su_t50, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__proportionality_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
