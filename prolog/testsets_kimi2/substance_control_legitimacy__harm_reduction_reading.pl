% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__harm_reduction_reading, []).

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
 *   constraint_id: substance_control_legitimacy__harm_reduction_reading
 *   human_readable: Harm Reduction State Substance Control Regime
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the harm_reduction_reading of the
 *   substance_control_legitimacy kernel. The state claims authority over
 *   substance use not through criminal law but through a public health duty
 *   to minimize harm. Users are medicalized rather than incarcerated, yet the
 *   regime retains coercive toolsâtreatment mandates, civil commitment, and
 *   monitoringâthat extract autonomy. A persistent black market in supply
 *   persists alongside the decriminalization of use, indicating the
 *   coordination function is partial. The claimed type is tangled_rope:
 *   genuine public health coordination (needle exchanges, supervised
 *   consumption, overdose prevention) operates alongside asymmetric
 *   extraction of users' liberty through mandated treatment. Metrics are
 *   authored independently of the claim.
 *
 * KEY AGENTS:
 *   - public_health_authority: Primary agenda-setter (institutional/constrained) â administers treatment mandates, controls public health budgets, and defines medical eligibility.
 *   - substance_users: Primary target (powerless/constrained) â subject to medicalization and treatment mandates; freed from criminal penalties but subjected to coercive health interventions.
 *   - addiction_treatment_providers: Beneficiary (organized/constrained) â receive state contracts and referrals; depend on public health framing for revenue and professional legitimacy.
 *   - criminal_justice_system: Excluded institutional actor (institutional/constrained) â lost user jurisdiction to health authorities; retains supply enforcement and resists frame transfer.
 *   - civil_liberties_advocates: Observer (organized/mobile) â monitor coercion in mandated treatment and civil commitment expansion.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__harm_reduction_reading, 0.45).
domain_priors:suppression_score(substance_control_legitimacy__harm_reduction_reading, 0.55).
domain_priors:theater_ratio(substance_control_legitimacy__harm_reduction_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__harm_reduction_reading, "Harm Reduction State Substance Control Regime").
narrative_ontology:topic_domain(substance_control_legitimacy__harm_reduction_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__harm_reduction_reading, '2973f81a-8523-4489-acf7-9d5a88bb663e').
narrative_ontology:cs_kernel_codification('2973f81a-8523-4489-acf7-9d5a88bb663e', formalized).
narrative_ontology:cs_authority_grounding('2973f81a-8523-4489-acf7-9d5a88bb663e', expertise).
narrative_ontology:cs_interpretation_layer_present('2973f81a-8523-4489-acf7-9d5a88bb663e').
narrative_ontology:cs_reading_relation('2973f81a-8523-4489-acf7-9d5a88bb663e', substance_control_legitimacy__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('2973f81a-8523-4489-acf7-9d5a88bb663e', substance_control_legitimacy__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('2973f81a-8523-4489-acf7-9d5a88bb663e', foundational, substance_use_as_public_health_condition).
narrative_ontology:cs_axiom_status(substance_use_as_public_health_condition, holdable).
narrative_ontology:cs_axiom_grounding('2973f81a-8523-4489-acf7-9d5a88bb663e', substance_use_as_public_health_condition, empirically_contingent).
narrative_ontology:cs_axiom('2973f81a-8523-4489-acf7-9d5a88bb663e', foundational, state_may_coerce_treatment_to_minimize_harm).
narrative_ontology:cs_axiom_status(state_may_coerce_treatment_to_minimize_harm, holdable).
narrative_ontology:cs_axiom_grounding('2973f81a-8523-4489-acf7-9d5a88bb663e', state_may_coerce_treatment_to_minimize_harm, instrumental).
narrative_ontology:cs_reference_frame('2973f81a-8523-4489-acf7-9d5a88bb663e', public_health_minimization_duty).
narrative_ontology:cs_drift_state('2973f81a-8523-4489-acf7-9d5a88bb663e', contemporary_decriminalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2973f81a-8523-4489-acf7-9d5a88bb663e', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, public_health_authority).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, addiction_treatment_providers).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, substance_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the legal and budgetary framework for harm reduction: sets eligibility for supervised consumption sites, treatment referrals, and civil commitment criteria. Justifies mandates as therapeutic necessity and duty of care. Receives institutional budget and statutory authority from the public health framing of substance use.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, public_health_authority, agenda_setter,
    institutional, generational, constrained, national).

% Subject to medicalization rather than criminal charges, but facing treatment mandates, civil commitment, and ongoing monitoring. Avoids incarceration for possession but loses autonomy to compulsory health interventions. Exit means refusing treatment and risking recommitment or falling back into the black market.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, substance_users, payer,
    powerless, biographical, constrained, local).

% Receive state contracts, mandated referrals, and public funding for beds, counseling, and substitution therapy. Professional legitimacy and revenue depend on the public health frame and the steady flow of users into the treatment pipeline. Cannot easily exit the state funding model without losing their client base.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, addiction_treatment_providers, beneficiary,
    organized, biographical, constrained, regional).

% Lost primary jurisdiction over substance users to health authorities but retains enforcement against supply. Would prefer a full prohibition frame that returns users to the carceral pipeline. Still operates in the shadows of the regime through policing of the persistent black market.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, criminal_justice_system, excluded,
    institutional, generational, constrained, national).

% Monitor the coercion content of treatment mandates and civil commitment expansion. Issue reports on the gap between voluntary harm reduction rhetoric and mandatory treatment practice. Neither collect from nor pay into the constraint, but publish analysis that can influence political will.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, civil_liberties_advocates, observer,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public health response to substance use: reducing overdose mortality, infectious disease transmission, and social harm by replacing criminal penalties with medical screening, treatment referral, supervised consumption infrastructure, and substitution therapy distribution.
% TRANSFER_FUNCTION: Moves authority and funding from criminal justice agencies to public health bureaucracies; moves substance users from carceral settings to treatment and monitoring systems; transfers tax revenue to addiction treatment providers and harm reduction services.
% ABSENT_VOICES: Prohibitionists who view decriminalization as moral hazard and demand criminal penalties for use; legalization advocates who reject state medicalization of autonomy and oppose any mandated treatment; non-treatment-seeking users who do not identify as ill and resist the disease model.
% DISAPPEARANCE_RATIONALE: If the public health authority frame vanished overnight, substance use would revert to criminal justice enforcement or advance toward unregulated markets; the treatment infrastructure, supervised consumption sites, and needle exchanges would lose state support and collapse or move underground.
% FOUNDING_PROBLEM: Prohibition produced high mortality, infectious disease, and social harm while failing to reduce substance use; mass incarceration of users compounded the damage without addressing the underlying behavior.
% FOUNDING_PROBLEM_CORROBORATION: Public health researchers, epidemiologists, and human rights organizations outside the treatment industry corroborate the failure of criminalization. Prohibitionist policymakers and some law enforcement agencies contest this narrative, arguing criminalization suppresses use and that harm reduction enables it.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__harm_reduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__harm_reduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_legitimacy__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__harm_reduction_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__harm_reduction_reading_tests).
:- end_tests(substance_control_legitimacy__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because treatment mandates and civil commitment extract significant autonomy but stop short of the carceral extraction of full prohibition. Suppression is moderate-high (0.55) because persistence requires enforcing mandates against non-compliant users and suppressing non-medical supply. Theater ratio is moderate-low (0.30): most activity is functional (overdose prevention, disease control) but a growing share is performative public health rhetoric masking paternalistic control. Accessibility collapse (0.40) is partial: alternatives like full legalization or autonomous harm reduction are intellectually available but institutionally blocked. Resistance (0.50) is moderate: prohibitionists resist decriminalization, civil libertarians resist mandates, and some users reject medicalization. The temporal series show gradual intensification as mandates expand and the black market adapts.
 *
 * PERSPECTIVAL GAP:
 *   The public health authority seat perceives the constraint as necessary coordination saving lives; the substance user seat experiences it as state control relocated from the courthouse to the clinic. The treatment provider seat benefits from the resource flow while dependent on the state's medicalization frame. The engine computes this divergence from structural data: the agenda-setter has constrained exit (institutional mission lock-in) while the payer has constrained exit (legal coercion), but the directionality of flows is opposite.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authority and treatment providers are structural beneficiaries: they receive budgetary authority, professional clientele, and institutional mission from the constraint (low d, subsidized by the arrangement). Substance users are structural victims: they lose autonomy to treatment mandates and monitoring (high d, amplified extraction). Criminal justice is excluded from the benefiting coalition but retains supply-side enforcement capacity. Civil liberties advocates observe from outside the beneficiary structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâcriminalization producing worse health outcomesâremains live in that the black market persists and overdose mortality continues. However, the arrangement has developed inertial features: treatment providers now depend on the mandate stream, and public health authorities have built bureaucracies around the medicalization frame. The R5 status is contested, preventing automatic piton classification. The temporal measurements show slow extraction accumulation, suggesting drift rather than obsolescence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is the harm_reduction_reading of kernel substance_control_legitimacy. How would its classification change if instantiated as the prohibition_reading (criminalization) or legalization_reading (autonomy)?',
    'Cross-family comparison of compiled constraint stories: prohibition would criminalize users (reversing victim role and raising suppression/extractiveness), while legalization would remove the state agenda-setter role and set extractiveness near zero with voluntary coordination only.',
    'The kernel''s readings produce mutually exclusive stakeholder role assignments and opposing directionality derivations, confirming they are distinct constraints. Treating the kernel as a single constraint would collapse these into an incoherent classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Sibling reading structural delta for harm reduction kernel').

omega_variable(
    treatment_mandate_extraction,
    'Are treatment mandates under harm reduction a necessary coordination mechanism to engage hard-to-reach populations, or a coercive extraction of bodily autonomy that exceeds the non-criminalization premise?',
    'Comparative outcome study of jurisdictions with voluntary versus mandated harm reduction: if health outcomes are equivalent or better under voluntary models, the mandate is extractive excess.',
    'If voluntary models match outcomes, the constraint''s extractiveness is inflated by unnecessary coercion, pushing it toward snare; if mandates are essential, the extraction is the price of coordination, supporting tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treatment_mandate_extraction, empirical, 'Whether treatment mandates are necessary coordination or coercive extraction').

omega_variable(
    black_market_coordination_failure,
    'Does the persistent black market under decriminalization indicate an incomplete coordination function, or does the constraint intentionally preserve illicit supply as a shadow enforcement target?',
    'Supply-side policy analysis: if the regime maintains prohibition on supply while decriminalizing use, the black market is a designed feature preserving criminal justice relevance, not a coordination failure.',
    'If the black market is preserved by design, the constraint''s coordination is partial and its extraction is asymmetrically loaded on users; if accidental, the constraint may be a scaffold transitioning toward fuller regulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_coordination_failure, conceptual, 'Black market persistence as coordination failure or design feature').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__harm_reduction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(subs_tr_t4, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(subs_tr_t8, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(subs_tr_t12, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(subs_tr_t16, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(subs_be_t4, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(subs_be_t8, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 8, 0.37).
narrative_ontology:measurement(subs_be_t12, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement(subs_be_t16, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 16, 0.43).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(subs_su_t4, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 4, 0.43).
narrative_ontology:measurement(subs_su_t8, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 8, 0.47).
narrative_ontology:measurement(subs_su_t12, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(subs_su_t16, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__harm_reduction_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy__legalization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the substance_control_legitimacy kernel, decomposed per the epsilon-invariance principle. The prohibition and legalization readings instantiate structurally distinct constraints with different beneficiary-victim structures and epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
