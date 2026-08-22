% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__legalization_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: substance_control_kernel__legalization_reading
 *   human_readable: Regulated Legal Substance Market with Externality Taxation
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This story instantiates the legalization reading of the substance control
 *   kernel: substance use itself is treated as an individual liberty matter
 *   outside legitimate state interest, and state intervention is legitimate
 *   only where it prevents or prices third-party harm (impaired driving,
 *   secondhand exposure, sales to minors) and captures externality costs
 *   through taxation. Under this reading, the user who consumes without
 *   harming others exits the victim set entirely — a structural departure
 *   from the prohibition reading, where the user is themselves the primary
 *   target of state coercion. New victims enter: third parties bearing
 *   uncompensated externalities, and market participants excluded from the
 *   legal channel by licensing costs or partial-legalization jurisdictional
 *   gaps. A new beneficiary class emerges — the licensed industry — alongside
 *   the state as tax collector. This is a distinct constraint from
 *   prohibition_reading and harm_reduction_reading, not a different
 *   observable angle on the same one: the beneficiary/victim sets, the
 *   enforcement target, and the extraction referent all differ structurally
 *   between readings, satisfying the ε-invariance decomposition requirement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__legalization_reading, 0.42).
domain_priors:suppression_score(substance_control_kernel__legalization_reading, 0.35).
domain_priors:theater_ratio(substance_control_kernel__legalization_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__legalization_reading, "Regulated Legal Substance Market with Externality Taxation").
narrative_ontology:topic_domain(substance_control_kernel__legalization_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__legalization_reading, '34453259-334e-4407-8550-b05b8268519d').
narrative_ontology:cs_kernel_codification('34453259-334e-4407-8550-b05b8268519d', distributed).
narrative_ontology:cs_authority_grounding('34453259-334e-4407-8550-b05b8268519d', distributed).
narrative_ontology:cs_reading_relation('34453259-334e-4407-8550-b05b8268519d', substance_control_kernel__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('34453259-334e-4407-8550-b05b8268519d', substance_control_kernel__harm_reduction_reading, influences).
narrative_ontology:cs_axiom('34453259-334e-4407-8550-b05b8268519d', foundational, consensual_use_outside_state_interest).
narrative_ontology:cs_axiom_status(consensual_use_outside_state_interest, holdable).
narrative_ontology:cs_axiom_grounding('34453259-334e-4407-8550-b05b8268519d', consensual_use_outside_state_interest, deontological).
narrative_ontology:cs_axiom('34453259-334e-4407-8550-b05b8268519d', foundational, state_intervention_limited_to_externality_capture).
narrative_ontology:cs_axiom_status(state_intervention_limited_to_externality_capture, holdable).
narrative_ontology:cs_axiom_grounding('34453259-334e-4407-8550-b05b8268519d', state_intervention_limited_to_externality_capture, instrumental).
narrative_ontology:cs_reference_frame('34453259-334e-4407-8550-b05b8268519d', criminalized_use_baseline).
narrative_ontology:cs_drift_state('34453259-334e-4407-8550-b05b8268519d', post_state_legalization_wave, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('34453259-334e-4407-8550-b05b8268519d', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__legalization_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, licensed_cannabis_alcohol_industry).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, state_tax_authorities).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, adult_recreational_users).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, dui_crash_victims).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, secondhand_exposure_bystanders).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, unlicensed_market_participants).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, low_income_heavy_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Purchase substances through licensed retail without criminal liability for possession or use. Pay embedded excise taxes at point of sale. Their exit from state scrutiny is real: no arrest risk, no criminal record, ability to move between jurisdictions with different regimes.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, adult_recreational_users, beneficiary,
    moderate, biographical, mobile, regional).

% Operates licensed production, distribution, and retail under the legalization regime, lobbying for favorable tax rates and against re-criminalization. Captures the consumer surplus that would otherwise flow to illicit sellers or remain foregone under prohibition. Shapes regulatory design through trade associations.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, licensed_cannabis_alcohol_industry, beneficiary,
    organized, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__legalization_reading, licensed_cannabis_alcohol_industry, agenda_setter).

% Collects excise and sales tax revenue from the legal market and licensing fees from operators; sets tax rates and enforcement priorities. Has a direct fiscal interest in market volume, creating tension with public health goals that would reduce consumption.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, state_tax_authorities, beneficiary,
    institutional, generational, analytical, regional).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__legalization_reading, state_tax_authorities, agenda_setter).

% Bear the externality cost of impaired driving directly: injury, death, or property loss caused by another party's substance use. They did not consent to the risk and have no exit from sharing roads with users; their only recourse is post-hoc civil or criminal liability against the impaired party, which does not undo the harm.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, dui_crash_victims, payer,
    powerless, immediate, trapped, local).

% Neighbors, coworkers, children, and shared-space occupants exposed to secondhand smoke or public intoxication effects they did not choose. Zoning and public-use restrictions offer partial mitigation but exposure in mixed housing or public spaces persists; exit means relocating, which is not available to renters or the economically constrained.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, secondhand_exposure_bystanders, payer,
    powerless, immediate, constrained, local).

% Small-scale growers, sellers, and users who cannot afford licensing costs, lack capital to enter the regulated market, or operate in jurisdictions with partial legalization. They remain criminally exposed even as the substance itself is legal for licensed actors elsewhere, and the legal market's existence undercuts sympathy for their continued prosecution.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, unlicensed_market_participants, payer,
    powerless, biographical, trapped, local).

% Users with dependency patterns who bear a disproportionate share of excise taxes (regressive relative to income) and face limited access to treatment despite the legal market's tax revenue nominally funding public health programs. Their consumption is treated as a revenue stream rather than a condition requiring intervention.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, low_income_heavy_users, payer,
    powerless, biographical, constrained, local).

% Track morbidity, addiction rates, and externality costs (emergency response, treatment demand) generated by the legal market, and advocate for tax-funded mitigation programs. Can document whether legalization revenue actually offsets the harms it generates.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, public_health_agencies, observer,
    institutional, generational, analytical, national).

% Enforce the narrowed remaining prohibitions: DUI statutes, unlicensed sales, public-use violations, and sales to minors. Their enforcement burden shifts from possession/use policing toward externality policing, but does not disappear.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, law_enforcement_agencies, agenda_setter,
    institutional, biographical, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__legalization_reading, licensed_cannabis_alcohol_industry).
narrative_ontology:fixing_cost_class(substance_control_kernel__legalization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a licensed, taxed, quality-controlled market that channels substance production and sale away from unregulated actors, while concentrating state intervention on externalities (impaired driving, exposure to non-consenting third parties, underage sales) rather than on the underlying use itself.
% TRANSFER_FUNCTION: Moves consumer spending from illicit sellers to licensed industry and state tax authorities; moves the residual externality costs (crashes, exposure, dependency-related public health burden) onto third parties and heavy users who receive no offsetting share of the tax revenue proportional to the harm they bear.
% ABSENT_VOICES: DUI crash victims and secondhand exposure bystanders are rarely organized into the regulatory conversation that sets tax rates and public-use rules; their interests are represented, if at all, by public health agencies and law enforcement rather than by themselves directly. Unlicensed market participants, particularly in partial-legalization jurisdictions, have no voice in a policy debate that treats the legal/illegal boundary as settled.
% DISAPPEARANCE_RATIONALE: If the legalization regime disappeared overnight, the licensed industry would lose its legal basis and revenue, the state would lose a substantial tax stream, users would face renewed criminal exposure, and production/distribution would likely revert toward unregulated or black-market channels with less quality control and no externality-tax funding for mitigation — a substantial rearrangement, not a return to a natural baseline.
% FOUNDING_PROBLEM: Prohibition-era criminalization of substance use generated mass incarceration, black-market violence, adulterated/unsafe product, and no revenue capture for the state, while failing to reduce use; the legalization reading was built to redirect state intervention toward actual third-party harms and to convert an underground economy into a taxed, regulated one.
% FOUNDING_PROBLEM_CORROBORATION: Public health agencies and independent economists outside the licensed industry corroborate that criminalization's harms (incarceration, unsafe supply) were real and substantially reduced post-legalization in studied jurisdictions; however, the same outside observers report that externality costs (DUI rates in some jurisdictions, dependency-related health burden) have not been fully offset by tax revenue allocation, and low-income heavy users report the promised treatment-funding benefit has been unevenly delivered — the founding problem of criminalization harm is substantially addressed, but the harm-reduction and externality-capture promises remain only partially realized.
narrative_ontology:disappearance_verdict(substance_control_kernel__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__legalization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__legalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_kernel__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__legalization_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__legalization_reading_tests).
:- end_tests(substance_control_kernel__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: real extraction occurs — regressive tax incidence on heavy users, externality costs pushed onto non-consenting third parties without full compensation — but it is substantially lower than under prohibition because consensual use no longer triggers criminal penalty. Suppression (0.35) is markedly lower than a prohibition regime's suppression profile, reflecting the narrowed scope of state coercion to externality-generating conduct (DUI, underage sales, unlicensed commerce) rather than use itself. Accessibility collapse (0.30) is low-moderate: legal purchase channels are broadly accessible to adults with means, though licensing barriers still collapse access for would-be small producers. Resistance (0.45) reflects ongoing contestation from residual prohibitionist constituencies, public health advocates seeking more redistribution of tax revenue toward treatment, and unlicensed-market participants resisting continued criminalization at the margins.
 *
 * DIRECTIONALITY LOGIC:
 *   Adult recreational users and the licensed industry sit near the beneficiary end: users gain liberty and market access, industry captures rents from legal exclusivity and brand/scale advantages over informal competitors. The state is a direct fiscal beneficiary via tax capture. DUI crash victims and secondhand exposure bystanders are structural targets — they bear costs generated by others' legally sanctioned conduct, with no consent and limited recourse, placing them near the full-target end of directionality despite not being parties to the substance transaction at all. Unlicensed market participants are trapped between two regimes: legal for licensed actors, criminal for them, which is treated as a genuine victim class in this reading rather than an incidental gap. Low-income heavy users occupy an ambiguous middle: nominally 'liberated' beneficiaries who in practice pay a regressive tax share disproportionate to any offsetting treatment access.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — mass incarceration and unregulated, unsafe black-market supply under prohibition — is substantially resolved by this reading's own evidence and by independent public-health corroboration; that is a genuine coordination achievement, not merely inertia. But the reading's mandate has partially outrun its original justification: externality-tax revenue was promised as the mechanism that would fund treatment and offset third-party harm, and the corroboration record shows that promise only partially delivered. This is not a full mandatrophy resolution (the arrangement is not purely vestigial performance) but it documents a live gap between the reading's justificatory claim and its operational delivery, which the tangled_rope classification (coordination for users plus concentrated extraction from externality-bearers) is built to capture rather than obscure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    externality_tax_adequacy,
    'Does the excise tax revenue captured under legalization actually offset the externality costs (DUI enforcement/emergency response, secondhand exposure mitigation, dependency treatment) it is meant to price, or does it fall short and leave a residual uncompensated transfer onto third parties?',
    'Comparative fiscal audit tracking legalization tax revenue allocation against documented externality costs (crash data, emergency service utilization, treatment demand) across legalized jurisdictions over multiple years.',
    'If revenue substantially covers externality costs, the tangled_rope''s extraction component shrinks toward a rope; if it falls persistently short, the gap between liberty framing and actual cost allocation supports treating this as more extractive than currently scored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_tax_adequacy, empirical, 'Whether externality taxation genuinely internalizes third-party costs or merely gestures at doing so.').

omega_variable(
    reading_boundary_at_dependency,
    'At what point does a ''liberty exercise'' by a dependent user stop being freely consensual in the sense the legalization reading requires, given that dependency itself may compromise the voluntariness the liberty framing assumes?',
    'Clinical and behavioral-economics research on addiction''s effect on decision-making autonomy, cross-referenced against this reading''s own liberty premise.',
    'If dependency substantially undermines voluntariness for a large share of heavy users, the legalization reading''s core axiom (informed adult choice) applies to a smaller population than the reading assumes, which would support closer kinship with the harm_reduction_reading for that subpopulation rather than a clean liberty framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_at_dependency, conceptual, 'Whether the liberty axiom holds uniformly across the user population or breaks down for dependent users.').

omega_variable(
    partial_legalization_residue,
    'In jurisdictions with partial or patchwork legalization, is the continued criminal exposure of unlicensed participants a temporary transitional artifact or a structurally permanent feature of this reading as implemented?',
    'Longitudinal tracking of licensing-cost barriers and enforcement patterns against unlicensed actors in jurisdictions years after initial legalization, to see whether the gap closes or persists.',
    'A persistent gap supports classifying unlicensed_market_participants as a durable victim class rather than a transitional one, reinforcing the tangled_rope reading; a closing gap would support scaffold-like transitional framing for that specific sub-arrangement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(partial_legalization_residue, empirical, 'Whether licensing-barrier criminalization is transitional or a permanent structural feature.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__legalization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__legalization_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(subs_tr_t4, substance_control_kernel__legalization_reading, theater_ratio, 4, 0.36).
narrative_ontology:measurement(subs_tr_t8, substance_control_kernel__legalization_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement(subs_tr_t12, substance_control_kernel__legalization_reading, theater_ratio, 12, 0.31).
narrative_ontology:measurement(subs_tr_t16, substance_control_kernel__legalization_reading, theater_ratio, 16, 0.29).
narrative_ontology:measurement(subs_tr_t20, substance_control_kernel__legalization_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__legalization_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(subs_be_t4, substance_control_kernel__legalization_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(subs_be_t8, substance_control_kernel__legalization_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(subs_be_t12, substance_control_kernel__legalization_reading, base_extractiveness, 12, 0.44).
narrative_ontology:measurement(subs_be_t16, substance_control_kernel__legalization_reading, base_extractiveness, 16, 0.43).
narrative_ontology:measurement(subs_be_t20, substance_control_kernel__legalization_reading, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__legalization_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(subs_su_t4, substance_control_kernel__legalization_reading, suppression_requirement, 4, 0.48).
narrative_ontology:measurement(subs_su_t8, substance_control_kernel__legalization_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(subs_su_t12, substance_control_kernel__legalization_reading, suppression_requirement, 12, 0.38).
narrative_ontology:measurement(subs_su_t16, substance_control_kernel__legalization_reading, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(subs_su_t20, substance_control_kernel__legalization_reading, suppression_requirement, 20, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__legalization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(substance_control_kernel__legalization_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, harm_reduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of substance_control_kernel. prohibition_reading treats use itself as the target of coercion (moral transgression model, high suppression, users as primary victims); harm_reduction_reading treats use as a health condition warranting intervention independent of cessation (public health framing, low suppression, users as beneficiaries of care rather than liberty). legalization_reading (this file) treats use as a liberty default, removes users from the victim set, and relocates victimhood to non-consenting third parties bearing externalities and to market participants excluded by licensing structure. Each reading carries a distinct ε, distinct beneficiary/victim sets, and a distinct claimed_type; they are linked via affects_constraints because policy movement in one reading's jurisdiction (e.g., a state adopting legalization) structurally affects the resourcing and legitimacy conditions available to the other readings elsewhere.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
