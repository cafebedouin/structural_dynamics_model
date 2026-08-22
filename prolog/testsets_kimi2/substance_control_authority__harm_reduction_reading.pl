% ============================================================================
% CONSTRAINT STORY: substance_control_authority__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__harm_reduction_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: substance_control_authority__harm_reduction_reading
 *   human_readable: State Harm Reduction Authority (Decriminalization with Public Health Control)
 *   domain: public_health_policy/criminal_justice
 *
 * SUMMARY:
 *   This constraint story models the harm reduction reading of the substance
 *   control authority kernel: state authority that decriminalizes drug
 *   possession and channels drug users through public health interventions
 *   (needle exchange, supervised consumption, medication-assisted treatment)
 *   while maintaining criminal penalties for supply and preserving state
 *   gatekeeping over drug-using populations. Users exit criminal
 *   victimization but enter medicalized surveillance; third parties bear
 *   externalized risks of disease transmission and property crime
 *   concentrated around service sites. The constraint occupies a middle
 *   position between prohibition and full legalization, actively suppressing
 *   both extremes. As a kernel reading, it is one of three structurally
 *   distinct constraints (with prohibition and legalization readings) that
 *   share a policy domain but instantiate different beneficiary/victim
 *   structures and epsilon values.
 *
 * KEY AGENTS:
 *   - drug_users: Primary target (powerless/identity_locked) â bear health harms and medicalization despite decriminalization
 *   - third_party_communities: Secondary target (moderate/constrained) â bear externalized disorder and disease risks
 *   - public_health_system: Primary beneficiary (institutional/constrained) â expands jurisdiction and funding through service contracts
 *   - state_authority: Agenda setter (institutional/arbitrage) â maintains policy middle ground and social control legitimacy
 *   - prohibitionist_constituencies: Excluded voice (organized/constrained) â reject accommodation, resist expansion
 *   - legalization_advocates: Excluded voice (moderate/mobile) â reject medical gatekeeping, demand market autonomy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__harm_reduction_reading, 0.56).
domain_priors:suppression_score(substance_control_authority__harm_reduction_reading, 0.48).
domain_priors:theater_ratio(substance_control_authority__harm_reduction_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__harm_reduction_reading, "State Harm Reduction Authority (Decriminalization with Public Health Control)").
narrative_ontology:topic_domain(substance_control_authority__harm_reduction_reading, "public_health_policy/criminal_justice").

domain_priors:requires_active_enforcement(substance_control_authority__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__harm_reduction_reading, 'a01ca82d-7665-45f6-81bf-db622226c246').
narrative_ontology:cs_kernel_codification('a01ca82d-7665-45f6-81bf-db622226c246', formalized).
narrative_ontology:cs_authority_grounding('a01ca82d-7665-45f6-81bf-db622226c246', lineage).
narrative_ontology:cs_interpretation_layer_present('a01ca82d-7665-45f6-81bf-db622226c246').
narrative_ontology:cs_reading_relation('a01ca82d-7665-45f6-81bf-db622226c246', substance_control_authority__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('a01ca82d-7665-45f6-81bf-db622226c246', substance_control_authority__legalization_reading, influences).
narrative_ontology:cs_axiom('a01ca82d-7665-45f6-81bf-db622226c246', foundational, decriminalization_health_superiority).
narrative_ontology:cs_axiom_status(decriminalization_health_superiority, holdable).
narrative_ontology:cs_axiom_grounding('a01ca82d-7665-45f6-81bf-db622226c246', decriminalization_health_superiority, empirically_contingent).
narrative_ontology:cs_axiom('a01ca82d-7665-45f6-81bf-db622226c246', foundational, state_medicalization_legitimacy).
narrative_ontology:cs_axiom_status(state_medicalization_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('a01ca82d-7665-45f6-81bf-db622226c246', state_medicalization_legitimacy, conventional).
narrative_ontology:cs_reference_frame('a01ca82d-7665-45f6-81bf-db622226c246', public_health_police_power).
narrative_ontology:cs_drift_state('a01ca82d-7665-45f6-81bf-db622226c246', contemporary_legalization_pressure, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a01ca82d-7665-45f6-81bf-db622226c246', '').
narrative_ontology:cs_kernel_id(substance_control_authority__harm_reduction_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, public_health_system).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, state_authority).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, drug_users).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, drug_users).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, third_party_communities).
narrative_ontology:constraint_vindicates(substance_control_authority__harm_reduction_reading, public_health_superiority_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear persistent health harms and subjection to state medicalization and surveillance despite exiting criminal prosecution; receive decriminalization protection and health services in exchange for engagement with state-mandated programs; stigma and dependency create high exit barriers from the service system.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, drug_users, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, drug_users, beneficiary).

% Bear concentrated externalities of drug use including property crime, public disorder, and communicable disease exposure; experience siting of harm reduction services without meaningful consent; lack mobility to exit neighborhoods where services concentrate.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, third_party_communities, payer,
    moderate, biographical, constrained, local).

% Expands professional jurisdiction and budget through state-contracted harm reduction services; operates under performance metrics set by state authority; gains mission legitimacy but loses autonomy to user-centered or radical models.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, public_health_system, beneficiary,
    institutional, generational, constrained, national).

% Maintains monopoly on drug policy legitimacy by occupying the middle ground between prohibition and legalization; enforces decriminalization boundaries while retaining criminal penalties for supply; captures political credit for compassion while preserving social control.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, state_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Advocates for restoration of criminal penalties for possession; excluded from policy-making tables in harm reduction jurisdictions but retains capacity to mobilize electoral backlash and resist service expansion.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, prohibitionist_constituencies, excluded,
    organized, generational, constrained, national).

% Advocates for full legalization of production, sale, and use; excluded because the harm reduction framework deliberately stops at decriminalization of possession to preserve state control and public health gatekeeping.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, legalization_advocates, excluded,
    moderate, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains engagement between drug-using populations and health services to reduce overdose mortality and infectious disease transmission, replacing criminal justice encounters with public health contact.
% TRANSFER_FUNCTION: Moves authority over drug use from criminal justice institutions to public health agencies; moves drug users from carceral risk to medical surveillance; moves externalized disease and disorder risks from users to surrounding communities; moves tax revenue into state-contracted harm reduction services.
% ABSENT_VOICES: Prohibitionist constituencies who reject any accommodation of drug use; legalization advocates who reject medical gatekeeping and state surveillance; drug users who prefer autonomous non-medicalized use.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would trigger either re-criminalization (returning users to carceral risk) or de facto legalization (collapsing public health gatekeeping); service infrastructure would dissolve; community risk profiles and overdose rates would shift rapidly.
% FOUNDING_PROBLEM: Prohibition produced mass incarceration, untreated addiction, and preventable overdose deaths without reducing drug availability, overwhelming criminal justice and emergency health systems.
% FOUNDING_PROBLEM_CORROBORATION: Public health researchers and international health organizations (WHO, UNAIDS) corroborate the failure of prohibition from outside the benefiting state agencies; however, law enforcement unions and conservative political constituencies dispute that the problem warranted state accommodation of drug use, and legalization advocates dispute that the founding problem is best solved by medicalized state management.
narrative_ontology:disappearance_verdict(substance_control_authority__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__harm_reduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__harm_reduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_authority__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__harm_reduction_reading, 0.56, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__harm_reduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_authority__harm_reduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_authority__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.56) reflects significant but not total extraction: users lose autonomy to medicalization and communities bear externalized risks, but genuine health benefits (overdose reversal, disease prevention) reduce extraction relative to pure carceral models. Suppression (0.48) is moderate-high because the constraint must actively suppress both prohibitionist enforcement resurgence and unregulated legal supply to maintain the decriminalization boundary. Theater ratio (0.28) captures performative compassion elements without denying real service delivery. Accessibility collapse (0.45) indicates that once adopted, the model collapses alternative policy paths (prohibition and legalization become politically inaccessible in that jurisdiction). Resistance (0.38) reflects ongoing pushback from both flanks.
 *
 * PERSPECTIVAL GAP:
 *   The drug user seat experiences a mixed target/beneficiary structure: decriminalization lowers directionality relative to the prohibition reading, but medicalization and service engagement raise it relative to full legalization. The public health seat experiences low directionality (coordinator/beneficiary) while third-party communities experience moderate directionality (bear costs without consent). State authority sits at low directionality (beneficiary of legitimacy and control). The engine will compute divergent per-seat types: likely tangled_rope from the user and community seats, rope-like from the public health seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (public_health_system, state_authority, drug_users) derive from structural gains: funding, legitimacy, and decriminalization protection. Victim declarations (drug_users, third_party_communities) derive from structural costs: medicalization/surveillance and externalized disorder/disease. Drug_users sit at the boundary between beneficiary and payer due to the partial victim set; their identity_locked exit options push their effective directionality toward the target end despite the decriminalization benefit. Third_party_communities are pure payers with constrained exit, producing high directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling: it preserves the genuine coordination function (overdose reduction, disease control) that a snare reading would erase, while naming the asymmetric extraction (medicalization, community externalization, state authority accumulation) that a rope reading would hide. If the public health services atrophied into purely symbolic programs while state surveillance hardened, the constraint would drift toward piton or snare; the temporal measurements are authored flat-to-rising to reflect current stability with extraction accumulation risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medicalization_autonomy_ambiguity,
    'Does the harm reduction reading genuinely empower drug users, or does it substitute medicalization and state surveillance for criminalization as a gentler form of coercion?',
    'Comparative ethnography and user autonomy metrics across decriminalization models, measuring voluntary versus compulsory service engagement and user self-determination in treatment choices.',
    'If medicalization dominates, drug_users are more target than beneficiary, raising effective extraction and pushing the constraint toward snare; if empowerment dominates, the coordination function is stronger and the rope element is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medicalization_autonomy_ambiguity, conceptual, 'Empowerment versus medicalization under state harm reduction').

omega_variable(
    kernel_boundary_stability,
    'Is the harm reduction reading structurally stable, or does it function as a transitional scaffold inevitably collapsing toward either prohibition revival or full legalization?',
    'Longitudinal policy trajectory analysis in jurisdictions with more than ten years of harm reduction, tracking statutory changes toward supply legalization or re-criminalization of possession.',
    'If transitional, reclassification to scaffold or piton is warranted; if stable, tangled_rope remains appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_boundary_stability, conceptual, 'Stability of the decriminalization boundary between prohibition and legalization').

omega_variable(
    third_party_risk_attribution,
    'Are disease transmission and crime risks borne by third parties intrinsic to drug use, or artifacts of the decriminalization-without-legalization design that leaves supply unregulated?',
    'Cross-jurisdictional comparison of community outcomes under full legalization, decriminalization, and prohibition regimes, controlling for baseline socioeconomic factors.',
    'If risks stem from policy design, third-party victimization is higher and the coordination story is weaker; if intrinsic, the constraint''s extraction from communities is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_risk_attribution, empirical, 'Source of third-party risks under decriminalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__harm_reduction_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sca_hr_tr_t0, substance_control_authority__harm_reduction_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sca_hr_tr_t5, substance_control_authority__harm_reduction_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(sca_hr_tr_t10, substance_control_authority__harm_reduction_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(sca_hr_tr_t15, substance_control_authority__harm_reduction_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(sca_hr_tr_t20, substance_control_authority__harm_reduction_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(sca_hr_tr_t25, substance_control_authority__harm_reduction_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(sca_hr_be_t0, substance_control_authority__harm_reduction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sca_hr_be_t5, substance_control_authority__harm_reduction_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(sca_hr_be_t10, substance_control_authority__harm_reduction_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(sca_hr_be_t15, substance_control_authority__harm_reduction_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(sca_hr_be_t20, substance_control_authority__harm_reduction_reading, base_extractiveness, 20, 0.53).
narrative_ontology:measurement(sca_hr_be_t25, substance_control_authority__harm_reduction_reading, base_extractiveness, 25, 0.56).

% Suppression requirement over time
narrative_ontology:measurement(sca_hr_su_t0, substance_control_authority__harm_reduction_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(sca_hr_su_t5, substance_control_authority__harm_reduction_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(sca_hr_su_t10, substance_control_authority__harm_reduction_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(sca_hr_su_t15, substance_control_authority__harm_reduction_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement(sca_hr_su_t20, substance_control_authority__harm_reduction_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(sca_hr_su_t25, substance_control_authority__harm_reduction_reading, suppression_requirement, 25, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
