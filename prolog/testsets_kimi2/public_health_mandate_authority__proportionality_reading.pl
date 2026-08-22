% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__proportionality_reading, []).

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
 *   constraint_id: public_health_mandate_authority__proportionality_reading
 *   human_readable: Public Health Mandate Authority â Proportionality Reading
 *   domain: public health law / constitutional rights / bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the proportionality reading of the
 *   public_health_mandate_authority kernel. Under this reading, state power
 *   to compel medical or behavioral interventions during health emergencies
 *   is legitimate only when calibrated to threat severity, availability of
 *   less restrictive alternatives, magnitude of coercion, and duration. The
 *   framework is presented as a safeguard against overreach, but its
 *   operation creates a dynamic victim boundary: both the unvaccinated (who
 *   bear direct coercion when proportionality permits mandates) and the
 *   immunocompromised (who bear risk exposure when proportionality
 *   assessments underestimate threat or alternatives) can sit in the victim
 *   set depending on how the balancing factors are weighted. The constraint
 *   is authored as a tangled rope: it carries a genuine coordination function
 *   (enabling collective protective action during genuine crises) but
 *   asymmetrically extracts bodily autonomy and liberty from mandate targets,
 *   and its contingent nature structurally under-protects the most vulnerable
 *   when threat assessments are contested.
 *
 * KEY AGENTS:
 *   - public_health_authorities: agenda_setter (institutional/constrained) â defines threat assessments and invokes proportionality to justify mandates
 *   - general_public: beneficiary (organized/constrained) â receives herd protection when mandates are proportionate
 *   - unvaccinated_individuals: payer (moderate/constrained) â direct targets of coercion when proportionality threshold is met
 *   - immunocompromised_population: payer (powerless/trapped) â biologically unable to exit risk; dependent on community protection that proportionality framework may or may not authorize
 *   - judiciary: observer (institutional/analytical) â adjudicates proportionality but rarely strikes down emergency measures
 *   - civil_liberties_advocates: excluded (organized/constrained) â challenge mandates but are marginalized in emergency framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__proportionality_reading, 0.56).
domain_priors:suppression_score(public_health_mandate_authority__proportionality_reading, 0.48).
domain_priors:theater_ratio(public_health_mandate_authority__proportionality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__proportionality_reading, "Public Health Mandate Authority â Proportionality Reading").
narrative_ontology:topic_domain(public_health_mandate_authority__proportionality_reading, "public health law / constitutional rights / bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__proportionality_reading, '11637925-4003-4109-8b1b-88f7c4390857').
narrative_ontology:cs_kernel_codification('11637925-4003-4109-8b1b-88f7c4390857', formalized).
narrative_ontology:cs_authority_grounding('11637925-4003-4109-8b1b-88f7c4390857', lineage).
narrative_ontology:cs_interpretation_layer_present('11637925-4003-4109-8b1b-88f7c4390857').
narrative_ontology:cs_reading_relation('11637925-4003-4109-8b1b-88f7c4390857', public_health_mandate_authority__public_health_primary, influences).
narrative_ontology:cs_reading_relation('11637925-4003-4109-8b1b-88f7c4390857', public_health_mandate_authority__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('11637925-4003-4109-8b1b-88f7c4390857', foundational, proportionality_governs_mandate_legitimacy).
narrative_ontology:cs_axiom_status(proportionality_governs_mandate_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('11637925-4003-4109-8b1b-88f7c4390857', proportionality_governs_mandate_legitimacy, conventional).
narrative_ontology:cs_axiom('11637925-4003-4109-8b1b-88f7c4390857', foundational, collective_protection_may_override_autonomy_conditionally).
narrative_ontology:cs_axiom_status(collective_protection_may_override_autonomy_conditionally, holdable).
narrative_ontology:cs_axiom_grounding('11637925-4003-4109-8b1b-88f7c4390857', collective_protection_may_override_autonomy_conditionally, conventional).
narrative_ontology:cs_reference_frame('11637925-4003-4109-8b1b-88f7c4390857', calibrated_emergency_power).
narrative_ontology:cs_drift_state('11637925-4003-4109-8b1b-88f7c4390857', post_pandemic_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('11637925-4003-4109-8b1b-88f7c4390857', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, general_public).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, unvaccinated_individuals).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, immunocompromised_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines epidemic threat assessments and invokes proportionality tests (severity, alternatives, coercion magnitude, duration) to justify mandates. They retain flexible authority to compel vaccination, quarantine, or masking, subject to judicial review. They do not collect direct revenue but capture expanded institutional mandate and legitimacy.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Receives reduced transmission and herd-protection benefits when proportionality assessments authorize mandates. Bears diffuse costs of restricted mobility and privacy intrusion during emergency periods. Cannot easily exit the jurisdiction or the public health system.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, general_public, beneficiary,
    organized, biographical, constrained, national).

% Direct targets of coercion when proportionality threshold is met. Subject to employment exclusion, travel bars, or compulsory vaccination based on a balancing test they do not control. Legal challenges are costly and rarely succeed during declared emergencies.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, unvaccinated_individuals, payer,
    moderate, biographical, constrained, national).

% Biologically unable to achieve full protection through personal vaccination. Dependent on community-level mandates for shielding. When proportionality assessments underestimate threat severity or overestimate alternatives, mandates are withheld and this population bears unchosen exposure risk. Their vulnerability is fixed; the framework's contingency falls on them asymmetrically.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, immunocompromised_population, payer,
    powerless, biographical, trapped, national).

% Adjudicates proportionality claims, weighing scientific evidence against rights assertions. In practice rarely strikes down emergency measures once declared, performing balancing review that largely ratifies executive threat assessments.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, judiciary, observer,
    institutional, generational, analytical, national).

% Challenge mandate expansions in litigation and public commentary. Routinely marginalized once emergency framing is adopted; their arguments are heard but seldom alter proportionality outcomes in crisis periods.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, civil_liberties_advocates, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__proportionality_reading, public_health_authorities).
narrative_ontology:fixing_cost_class(public_health_mandate_authority__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal architecture for calibrating state coercion to actual public health threat levels, enabling collective protective action during genuine emergencies while nominally requiring that mandates be necessary, proportionate, and time-limited.
% TRANSFER_FUNCTION: Transfers bodily autonomy, freedom of movement, and privacy from mandate targets and the general population to public health authorities during assessed emergencies, conditioned on a four-factor balancing test.
% ABSENT_VOICES: Individuals with severe adverse reactions to vaccines are rarely heard in proportionality calculations; civil liberties advocates are procedurally included but substantively excluded once emergency framing locks in.
% DISAPPEARANCE_RATIONALE: Without the proportionality framework, public health mandates would collapse into either unchecked collective-power absolutism (public_health_primary) or categorical bodily-autonomy prohibition (bodily_autonomy_primary); the legal architecture for conditional, calibrated coercion would vanish, forcing a binary institutional rearrangement.
% FOUNDING_PROBLEM: How to authorize necessary collective health protections during emergencies without establishing a permanent, unlimited state power to compel medical intervention and override bodily autonomy.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars outside the public health apparatus contest whether proportionality currently limits or merely ritualizes coercion; public health authorities attest the problem is live, while civil liberties scholars argue post-pandemic practice shows the framework has been captured by emergency logic. No neutral corroborator outside the dispute attests unambiguously.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(public_health_mandate_authority__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__proportionality_reading, 0.56, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.56) is authored at moderate-high because the framework structurally permits rather than prevents coercion; its value lies in limiting extraction, not eliminating it. Suppression (0.48) reflects moderate legal enforcement â mandates are enforced but less brutally than unchecked emergency power. Theater_ratio (0.28) captures the real but imperfect balancing: courts perform proportionality review but rarely invalidate measures, suggesting some ritualization. Accessibility_collapse (0.45) indicates that while alternatives are nominally considered, they often collapse in practice once emergency is declared. Resistance (0.55) is substantial because the sliding scale generates litigation and political contest at every application. The temporal series tracks the COVID-19 pandemic era: extraction and suppression spiked as proportionality was stress-tested, then modestly recalibrated as courts reflected on overreach.
 *
 * PERSPECTIVAL GAP:
 *   From the public_health_authorities seat, the framework is a necessary brake that prevents worse overreach while preserving response capacity â they experience it as coordination with cost. From the unvaccinated_individuals seat, it is a conditional license for coercion that extracts bodily autonomy whenever a court finds threat severity sufficient â they experience it as extraction. The immunocompromised_population seat is split: when mandates are authorized they experience subsidy (community protection), but when proportionality assessments exclude mandates they experience exposure as unchosen risk. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are declared beneficiaries with constrained-but-institutional power â directionality near the beneficiary end (low d). General_public are beneficiaries but with constrained exit, placing them closer to symmetric. Unvaccinated_individuals are declared victims (payers) with constrained exit â moderate-high d. Immunocompromised_population are declared victims with trapped exit (cannot exit their biological vulnerability) and powerless status, placing them near the full-target end (high d) because the framework's contingency falls on them most heavily when assessments misfire.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality framework is designed to prevent mandatrophy by sunsetting coercion as threats recede and requiring least-restrictive alternatives. However, the R5 genealogy interview suggests the founding problem â unlimited emergency power â is contested in its current status. Proportionality can mask mandatrophy by ritualizing review without altering outcomes (theater_ratio rising under stress). The measurement series shows extraction receding modestly post-emergency, but not returning to baseline, suggesting partial institutionalization of conditional coercion â a drift toward tangled-rope steady state rather than scaffolded transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    immunocompromised_structural_position,
    'Is the immunocompromised population structurally a beneficiary or victim of the proportionality framework?',
    'Longitudinal health-outcome data comparing immunocompromised infection and mortality rates under strict versus loose proportionality regimes, controlling for pathogen severity.',
    'If victim, the framework''s effective extraction is higher than its coordination story claims; if beneficiary, the current victim classification overstates extraction and directionality should shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immunocompromised_structural_position, empirical, 'Ambiguity of immunocompromised position under proportionality').

omega_variable(
    proportionality_as_constraint_or_legitimation,
    'Does proportionality doctrine genuinely limit state power, or does it primarily legitimate coercion by providing a ritualized balancing test?',
    'Comparative strike-down rates for public health mandates in proportionality jurisdictions versus non-proportionality jurisdictions; qualitative analysis of judicial reasoning to identify substantive versus performative engagement with alternatives.',
    'If legitimation, theater_ratio and base_extractiveness are higher than surface reading suggests, and the constraint trends toward snare-like dynamics; if genuine constraint, the tangled-rope classification is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_as_constraint_or_legitimation, conceptual, 'Whether proportionality limits power or ritualizes it').

omega_variable(
    threat_severity_objectivity,
    'Is threat severity assessment in proportionality analysis empirically grounded or politically constructed?',
    'Audit of official threat assessments against independent epidemiological benchmarks and retrospective excess-mortality data.',
    'If politically constructed, the constraint''s extraction is arbitrary and the victim boundary becomes a function of political economy rather than public health, increasing effective extraction for all payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_severity_objectivity, empirical, 'Objectivity of threat severity calibration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__proportionality_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(phma_prop_tr_t0, public_health_mandate_authority__proportionality_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(phma_prop_tr_t5, public_health_mandate_authority__proportionality_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(phma_prop_tr_t10, public_health_mandate_authority__proportionality_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(phma_prop_tr_t15, public_health_mandate_authority__proportionality_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(phma_prop_tr_t20, public_health_mandate_authority__proportionality_reading, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(phma_prop_be_t0, public_health_mandate_authority__proportionality_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(phma_prop_be_t5, public_health_mandate_authority__proportionality_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(phma_prop_be_t10, public_health_mandate_authority__proportionality_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(phma_prop_be_t15, public_health_mandate_authority__proportionality_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(phma_prop_be_t20, public_health_mandate_authority__proportionality_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(phma_prop_su_t0, public_health_mandate_authority__proportionality_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(phma_prop_su_t5, public_health_mandate_authority__proportionality_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(phma_prop_su_t10, public_health_mandate_authority__proportionality_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(phma_prop_su_t15, public_health_mandate_authority__proportionality_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(phma_prop_su_t20, public_health_mandate_authority__proportionality_reading, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__proportionality_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
