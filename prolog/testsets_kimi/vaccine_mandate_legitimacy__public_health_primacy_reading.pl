% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__public_health_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__public_health_primacy_reading, []).

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
 *   constraint_id: vaccine_mandate_legitimacy__public_health_primacy_reading
 *   human_readable: Vaccine Mandate Legitimacy â Public Health Primacy Reading
 *   domain: public_health_policy/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint instantiates the public_health_primacy reading of the
 *   vaccine_mandate_legitimacy kernel. Under this reading, the state's duty
 *   to prevent collective harm justifies overriding individual bodily
 *   autonomy, and unvaccinated status is treated as an externality to be
 *   managed through mandate authority. The constraint coordinates epidemic
 *   suppression while asymmetrically extracting autonomy and access from the
 *   unvaccinated population. Sibling readings include bodily_autonomy_primacy
 *   (which categorically rejects state coercion over medical decisions) and
 *   risk_stratification (which accepts only actuarially targeted mandates).
 *   This story does not adjudicate the kernel; it models one structurally
 *   distinct reading.
 *
 * KEY AGENTS:
 *   - Public health authority (agenda_setter/institutional): sets and enforces mandates; gains institutional authority.
 *   - Unvaccinated refusers (payer/moderate): bear coercive costs of exclusion and job loss; high directionality toward extraction.
 *   - Vaccinated majority (beneficiary/organized): receives coordination benefit without bearing coercion costs.
 *   - Civil liberties organizations (excluded/organized): autonomy advocates structurally absent from emergency framing.
 *   - Medical ethicists (observer/institutional): analytical seat evaluating proportionality.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.68).
domain_priors:suppression_score(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.75).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__public_health_primacy_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__public_health_primacy_reading, "Vaccine Mandate Legitimacy â Public Health Primacy Reading").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__public_health_primacy_reading, "public_health_policy/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__public_health_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__public_health_primacy_reading, 'f0324032-aec1-4510-a523-e9a5e800acfe').
narrative_ontology:cs_kernel_codification('f0324032-aec1-4510-a523-e9a5e800acfe', formalized).
narrative_ontology:cs_authority_grounding('f0324032-aec1-4510-a523-e9a5e800acfe', lineage).
narrative_ontology:cs_interpretation_layer_present('f0324032-aec1-4510-a523-e9a5e800acfe').
narrative_ontology:cs_reading_relation('f0324032-aec1-4510-a523-e9a5e800acfe', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('f0324032-aec1-4510-a523-e9a5e800acfe', vaccine_mandate_legitimacy__risk_stratification_reading, influences).
narrative_ontology:cs_axiom('f0324032-aec1-4510-a523-e9a5e800acfe', foundational, state_may_compel_medical_intervention_for_collective_harm).
narrative_ontology:cs_axiom_status(state_may_compel_medical_intervention_for_collective_harm, holdable).
narrative_ontology:cs_axiom_grounding('f0324032-aec1-4510-a523-e9a5e800acfe', state_may_compel_medical_intervention_for_collective_harm, conventional).
narrative_ontology:cs_axiom('f0324032-aec1-4510-a523-e9a5e800acfe', foundational, unvaccinated_status_constitutes_externality).
narrative_ontology:cs_axiom_status(unvaccinated_status_constitutes_externality, holdable).
narrative_ontology:cs_axiom_grounding('f0324032-aec1-4510-a523-e9a5e800acfe', unvaccinated_status_constitutes_externality, empirically_contingent).
narrative_ontology:cs_reference_frame('f0324032-aec1-4510-a523-e9a5e800acfe', police_power_tradition).
narrative_ontology:cs_drift_state('f0324032-aec1-4510-a523-e9a5e800acfe', post_pandemic_normalization, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f0324032-aec1-4510-a523-e9a5e800acfe', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_authority).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_majority).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, unvaccinated_refusers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets vaccination mandates under emergency and public health statutes; enforces compliance through exclusion orders, employer directives, and credentialing systems. Gains institutional scope, budget, and legal precedent from the mandate framework.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Bear the direct costs of mandate compliance or face termination, exclusion from public spaces, travel bans, and social sanction. Experience state coercion as the operational mechanism for converting unvaccinated status into a manageable externality.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, unvaccinated_refusers, payer,
    moderate, biographical, constrained, national).

% Receive reduced perceived risk and restored access to public spaces contingent on others' compelled compliance; benefit from the mandate's coordination function without bearing its coercive costs.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_majority, beneficiary,
    organized, biographical, mobile, national).

% Would advance bodily autonomy and proportionality objections but are structurally sidelined in emergency public health deliberations where harm-prevention framing dominates and dissent is treated as misinformation or externality.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, civil_liberties_organizations, excluded,
    organized, generational, analytical, national).

% Evaluate the proportionality of coercion relative to outcome; occupy an analytical seat assessing whether the mandate satisfies the least-restrictive-means test and whether the externality framing is medically sound.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, medical_ethicists, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_authority).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__public_health_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preventing infectious disease transmission and protecting healthcare system capacity by achieving vaccination coverage through state coercion where voluntary uptake is insufficient to avert collective harm.
% TRANSFER_FUNCTION: Transfers bodily autonomy, freedom of movement, and economic access from unvaccinated individuals to the collective (mediated by the state/public health authority), in exchange for reduced transmission risk and restored social functioning for the broader population.
% ABSENT_VOICES: Unvaccinated individuals and bodily autonomy advocates are treated as externalities rather than legitimate interlocutors; their objections are excluded from the harm-prevention calculus and framed as public health risks themselves.
% DISAPPEARANCE_RATIONALE: If the mandate authority vanished, unvaccinated individuals would regain access to workplaces and public spaces, the public health bureaucracy would lose its primary coercive lever for coverage targets, and the social contract around pandemic response would shift from compelled to voluntary mitigation.
% FOUNDING_PROBLEM: Contagious disease outbreak with transmissibility and severity sufficient to threaten healthcare system collapse or mass casualty, where voluntary vaccination uptake is inadequate to prevent that outcome.
% FOUNDING_PROBLEM_CORROBORATION: Public health officials and epidemiologists attest to the threat from within the benefiting framework. Civil liberties organizations and some minority communities dispute that the threat justified blanket mandates as opposed to targeted measures. Independent retrospective all-cause mortality and transmission studies provide mixed corroboration depending on variant, timing, and population.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__public_health_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__public_health_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__public_health_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__public_health_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_legitimacy__public_health_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint concentrates material costsâjob loss, spatial exclusion, loss of medical self-determinationâon the unvaccinated minority. Suppression (0.75) is high because the mandate persists only through active enforcement: credential checks, employer mandates, and travel restrictions. Theater ratio (0.25) is moderate-low; the public health threat was genuine, though a portion of enforcement activity shifted toward performative compliance signaling as the emergency normalized. Accessibility collapse (0.60) reflects that testing, natural immunity recognition, and alternative mitigations were systematically deprioritized in favor of vaccination-only frameworks. Resistance (0.70) captures sustained legal, political, and grassroots opposition from the targeted population and civil liberties coalitions.
 *
 * PERSPECTIVAL GAP:
 *   The public health authority seat should compute as coordination (it sees itself solving a collective-action problem in which free-riding produces mass harm). The unvaccinated refuser seat should compute as extraction (it experiences state power removing bodily autonomy without individualized consent). The vaccinated majority sits near the beneficiary pole: they receive the public good without paying the coercive price. The engine derives this divergence from the structural data rather than from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (public_health_authority, vaccinated_majority) drive low directionality for those seats: the authority is subsidized by expanded power, and the majority is subsidized by risk reduction. Victim declaration (unvaccinated_refusers) and constrained exit options drive high directionality for that seat. The civil liberties organizations are excluded rather than victims of direct extraction, receiving no directionality assignment in the engine's beneficiary/victim derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure extraction (snare) by acknowledging the genuine coordination functionâepidemic control and healthcare system protectionâwhile also preventing mislabeling it as pure coordination (rope) by naming the asymmetric victimization of the unvaccinated. The mandate does not meet scaffold criteria because it carries no credible sunset clause tied to the obsolescence of the founding threat. If the founding problem is dead but the mandate persists, the mandatrophy flag would trigger pitonward drift; the authored founding_problem_status of contested leaves that ambiguity open for the engine's temporal analysis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    externality_empirical_status,
    'Does unvaccinated status for this disease at this time actually constitute an externality proportionate to the coercion applied, given transmission dynamics and vaccine properties?',
    'Retrospective population-level transmission studies comparing mandated and non-mandated jurisdictions with matched demographics, and within-jurisdiction time-series analysis of transmission by vaccination status.',
    'If the externality claim is substantially weaker than asserted at mandate implementation, the constraint''s extractiveness was higher than its coordination value, and the classification edge shifts snareward; if proportionate, the tangled_rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_empirical_status, empirical, 'Whether the unvaccinated externality claim empirically justifies the mandate scale.').

omega_variable(
    kernel_reading_boundary,
    'Does this constraint remain stable if the sibling bodily_autonomy reading gains institutional authority, or does the constraint''s epsilon value collapse when the harm-prevention frame is no longer dominant?',
    'Comparative jurisdictional analysis: measure mandate persistence and enforcement intensity in legal systems where bodily autonomy has stronger constitutional entrenched status versus systems where public health primacy dominates.',
    'If epsilon is frame-dependent, the constraint is not structurally robust and the kernel is better modeled as competing constraints with observer-relative classification; if stable, the constraint is a genuine reading with invariant structural features.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Frame stability of the public health primacy reading against competing kernel readings.').

omega_variable(
    mandate_proportionality_temporal_shift,
    'Did the proportionality calculus shift over the interval such that the constraint''s extraction increased while its coordination value remained constant or declined?',
    'Time-resolved measurements of hospitalization averted per unit of coercion, compared against the authored base_extractiveness and suppression trajectories.',
    'A rising extraction-to-coordination ratio over time would indicate Goodhart drift and support a theater-ratio interpretation; a stable ratio supports the authored tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_proportionality_temporal_shift, empirical, 'Temporal shift in the mandate''s proportionality and extraction ratio.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__public_health_primacy_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vax_mph_tr_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vax_mph_tr_t6, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(vax_mph_tr_t12, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(vax_mph_tr_t18, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 18, 0.25).
narrative_ontology:measurement(vax_mph_tr_t24, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement(vax_mph_tr_t30, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(vax_mph_tr_t36, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 36, 0.25).

% Extraction over time
narrative_ontology:measurement(vax_mph_be_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(vax_mph_be_t6, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(vax_mph_be_t12, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 12, 0.72).
narrative_ontology:measurement(vax_mph_be_t18, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 18, 0.7).
narrative_ontology:measurement(vax_mph_be_t24, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(vax_mph_be_t30, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(vax_mph_be_t36, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 36, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vax_mph_su_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(vax_mph_su_t6, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 6, 0.7).
narrative_ontology:measurement(vax_mph_su_t12, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 12, 0.82).
narrative_ontology:measurement(vax_mph_su_t18, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 18, 0.78).
narrative_ontology:measurement(vax_mph_su_t24, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(vax_mph_su_t30, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(vax_mph_su_t36, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 36, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__public_health_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy__risk_stratification_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the vaccine_mandate_legitimacy kernel. Each reading has a distinct epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
