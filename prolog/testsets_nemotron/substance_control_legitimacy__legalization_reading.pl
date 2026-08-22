% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__legalization_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: substance_control_legitimacy__legalization_reading
 *   human_readable: Legalization Reading of Substance Control Legitimacy
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the legalization_reading of the
 *   substance_control_legitimacy kernel. The reading asserts that competent
 *   adults possess bodily autonomy over substance use, limiting legitimate
 *   state authority to preventing third-party harm (impaired driving,
 *   secondhand exposure, externalized costs). The standing arrangement is the
 *   post-2012 legalization framework in multiple U.S. states and other
 *   jurisdictions: regulated commercial markets for cannabis (and in some
 *   cases other substances) with taxation, licensing, and DUI enforcement.
 *   The ε-referent is this standing arrangement assessed from the
 *   legalization reading's lights — not the prohibition status quo ante, not
 *   the harm_reduction_reading's decriminalization-without-commercialization
 *   model. The reading claims rope-type coordination (replacing illicit
 *   market chaos with regulated order) but the metrics reveal substantial
 *   extractiveness (corporate profit, tax dependence, externalized
 *   third-party harms) and active enforcement (DUI laws, licensing
 *   enforcement, marketing restrictions that are selectively enforced).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, 0.38).
domain_priors:suppression_score(substance_control_legitimacy__legalization_reading, 0.22).
domain_priors:theater_ratio(substance_control_legitimacy__legalization_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__legalization_reading, rope).
narrative_ontology:human_readable(substance_control_legitimacy__legalization_reading, "Legalization Reading of Substance Control Legitimacy").
narrative_ontology:topic_domain(substance_control_legitimacy__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__legalization_reading, 'c2bc3a03-2cd4-4dac-90b0-9353c6666b21').
narrative_ontology:cs_kernel_codification('c2bc3a03-2cd4-4dac-90b0-9353c6666b21', distributed).
narrative_ontology:cs_authority_grounding('c2bc3a03-2cd4-4dac-90b0-9353c6666b21', distributed).
narrative_ontology:cs_reading_relation('c2bc3a03-2cd4-4dac-90b0-9353c6666b21', substance_control_legitimacy__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('c2bc3a03-2cd4-4dac-90b0-9353c6666b21', substance_control_legitimacy__harm_reduction_reading, influences).
narrative_ontology:cs_axiom('c2bc3a03-2cd4-4dac-90b0-9353c6666b21', foundational, bodily_autonomy_includes_substance_use).
narrative_ontology:cs_axiom_status(bodily_autonomy_includes_substance_use, holdable).
narrative_ontology:cs_axiom_grounding('c2bc3a03-2cd4-4dac-90b0-9353c6666b21', bodily_autonomy_includes_substance_use, deontological).
narrative_ontology:cs_axiom('c2bc3a03-2cd4-4dac-90b0-9353c6666b21', foundational, state_authority_limited_to_third_party_harm).
narrative_ontology:cs_axiom_status(state_authority_limited_to_third_party_harm, holdable).
narrative_ontology:cs_axiom_grounding('c2bc3a03-2cd4-4dac-90b0-9353c6666b21', state_authority_limited_to_third_party_harm, deontological).
narrative_ontology:cs_axiom('c2bc3a03-2cd4-4dac-90b0-9353c6666b21', secondary, regulated_commercial_markets_reduce_illicit_harm).
narrative_ontology:cs_axiom_status(regulated_commercial_markets_reduce_illicit_harm, holdable).
narrative_ontology:cs_axiom_grounding('c2bc3a03-2cd4-4dac-90b0-9353c6666b21', regulated_commercial_markets_reduce_illicit_harm, empirically_contingent).
narrative_ontology:cs_reference_frame('c2bc3a03-2cd4-4dac-90b0-9353c6666b21', pre_prohibition_common_law_autonomy).
narrative_ontology:cs_drift_state('c2bc3a03-2cd4-4dac-90b0-9353c6666b21', post_2012_legalization_wave, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('c2bc3a03-2cd4-4dac-90b0-9353c6666b21', '2026-08-04T12:00:00Z').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__legalization_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, competent_adult_users).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, legal_cannabis_corporations).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, tax_authorities).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, harm_reduction_service_providers).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, third_parties_exposed_to_impaired_driving).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, third_parties_exposed_to_secondhand_exposure).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, vulnerable_populations_targeted_by_corporate_marketing).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, communities_bearing_externalized_costs).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__legalization_reading, bodily_autonomy_principle).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__legalization_reading, harm_principle_as_state_limit).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__legalization_reading, regulated_markets_reduce_illicit_harm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain legal access to regulated substances with quality controls and known potency. No longer face criminal penalties for possession/use. Can choose products, compare prices, and exit to illicit market if legal prices are too high (arbitrage-grade exit). Bear costs through taxation and potential health consequences, but these are voluntary transactions.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, competent_adult_users, beneficiary,
    organized, biographical, arbitrage, national).

% Capture the legal market's economic surplus through branding, lobbying for favorable regulations, and economies of scale. Shape regulatory frameworks through industry associations and campaign contributions. Can relocate operations across jurisdictions (mobile exit) but benefit from barriers to entry they help construct. Extract value from users through pricing power and from taxpayers through regulatory capture.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, legal_cannabis_corporations, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__legalization_reading, legal_cannabis_corporations, agenda_setter).

% Collect excise taxes, sales taxes, and corporate income taxes from the legal market. Revenue funds public services but creates fiscal dependency on continued consumption. No exit — the state is the ultimate agenda-setter. The tax take validates the legalization framework politically.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, tax_authorities, beneficiary,
    institutional, generational, analytical, national).

% Receive public funding and legitimacy to operate needle exchanges, safe consumption sites, and treatment programs. Their professional standing and funding streams depend on the legalization framework treating use as health not crime. Constrained exit — their mission ties them to this population; leaving means abandoning clients.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, harm_reduction_service_providers, beneficiary,
    organized, biographical, constrained, regional).

% Bear risk of injury/death from impaired drivers with no consent to the risk and no practical exit. The constraint's enforcement (DUI laws, checkpoints) is reactive — harm occurs before enforcement activates. No individual can opt out of sharing roads with impaired drivers.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, third_parties_exposed_to_impaired_driving, payer,
    powerless, immediate, trapped, local).

% Involuntarily inhale secondhand smoke/vapor in multi-unit housing, public spaces, and workplaces. Can sometimes relocate (constrained exit) but at significant personal cost. Children and dependents have no exit. Health externalities accumulate over years.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, third_parties_exposed_to_secondhand_exposure, payer,
    powerless, biographical, constrained, local).

% Low-income, minority, and youth populations disproportionately targeted by legal-market advertising and product design (flavors, high-potency products). Identity-locked: marketing exploits cultural identity and social vulnerability; exit requires overcoming structural predation, not just personal choice. Bear addiction costs, health harms, and financial extraction.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, vulnerable_populations_targeted_by_corporate_marketing, payer,
    powerless, biographical, identity_locked, national).

% Absorb costs of increased emergency response, healthcare utilization, lost productivity, and social service demand from legal-market use. Municipal budgets strained; property values affected near dense retail zones. Can advocate for zoning/taxation (constrained political exit) but cannot fully internalize costs to producers.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, communities_bearing_externalized_costs, payer,
    moderate, generational, constrained, regional).

% Monitor population-level outcomes: use rates, harm trajectories, equity impacts, corporate behavior. No stake in the arrangement's persistence; their role is to measure whether the legalization reading's claims (reduced illicit market, managed harm, autonomy respected) hold empirically over time.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, public_health_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the coordination problem of substance access by replacing an unregulated illicit market with a regulated legal market: quality control, known potency, age verification, tax collection, and harm reduction infrastructure are provided centrally rather than through criminal networks.
% TRANSFER_FUNCTION: Moves tax revenue and consumer surplus from users to state (taxes) and corporations (profits). Moves risk of impaired driving and secondhand exposure from users to non-consenting third parties. Moves enforcement costs from criminal justice system to regulatory apparatus. Moves marketing externalities from corporations to vulnerable populations.
% ABSENT_VOICES: Future generations who will inherit the regulatory framework and corporate power structures; children of users who cannot consent to household exposure; illicit market actors displaced by legalization (some of whom transition to more harmful activities); communities in production regions bearing environmental costs of legal cultivation.
% DISAPPEARANCE_RATIONALE: If the legalization reading vanished overnight, the legal market would collapse, reverting to illicit supply or prohibition enforcement. Users would lose legal access and face criminal penalties again. Corporations would lose legal revenue streams. Tax authorities would lose billions in revenue. Harm reduction infrastructure would lose legitimacy and funding. Third-party harms would shift but not disappear — impaired driving would persist via illicit supply. The world rearranges because multiple institutional and economic structures have coalesced around this reading.
% FOUNDING_PROBLEM: The drug war's founding problem: criminalization of substance use created mass incarceration, racial disparity, unsafe supply, empowered criminal organizations, and failed to reduce use — while denying competent adults autonomy over their own bodies.
% FOUNDING_PROBLEM_CORROBORATION: The drug war's harms are documented by the ACLU, Human Rights Watch, the UN Office on Drugs and Crime, and decades of criminological research — sources outside the legalization advocacy coalition. However, prohibition_reading proponents (DEA, some law enforcement associations, international narcotics control bodies) contest that the founding problem is solved, arguing legalization creates new harms that exceed the old ones.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__legalization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__legalization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(substance_control_legitimacy__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__legalization_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__legalization_reading_tests).
:- end_tests(substance_control_legitimacy__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects corporate profit extraction from users (especially vulnerable populations), tax authority fiscal capture, and externalized costs borne by third parties — not the coordination function itself. Suppression (0.22) is low relative to prohibition but nonzero: DUI enforcement, licensing barriers, and marketing rules require active maintenance. Theater ratio (0.15) is low because the coordination function (quality control, age verification, tax collection) is genuinely operational, though corporate lobbying creates performative regulatory capture. Accessibility collapse (0.35) is moderate: the illicit market persists as an exit option, and home cultivation provides an alternative in some jurisdictions. Resistance (0.48) is significant: federal prohibition creates legal conflict, neighboring states maintain prohibition, and public health advocates contest corporate capture.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute divergent seat types: from the competent_user seat, the constraint appears as rope (genuine coordination benefit, low extraction). From the legal_corporation seat, it appears as tangled_rope (coordination function + asymmetric extraction via market power). From the third_party_impaired_driving seat, it appears as snare (pure extraction of safety, no coordination benefit, trapped). From the vulnerable_population seat, it appears as tangled_rope trending snare (identity-locked extraction via marketing). The legalization reading's claim of 'rope' reflects the user/beneficiary perspective; the metrics capture the structural aggregate across all seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent adult users are primary beneficiaries (d ~ 0.15) — they gain legal access, quality assurance, and arbitrage-grade exit to illicit market. Legal cannabis corporations are beneficiaries with agenda-setter power (d ~ 0.1) — they capture surplus and shape rules. Tax authorities are beneficiaries (d ~ 0.05) — fiscal capture without operational risk. Harm reduction providers are beneficiaries (d ~ 0.2) — constrained exit, mission-locked. Third parties exposed to impaired driving are full targets (d ~ 0.95) — trapped, no consent, reactive enforcement. Secondhand exposure victims are targets (d ~ 0.8) — constrained exit, cumulative harm. Vulnerable populations targeted by marketing are identity-locked targets (d ~ 0.85) — structural predation fused with identity. Communities bearing externalities are moderate payers (d ~ 0.6) — organized political voice but cannot fully internalize costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (drug war harms) is contested as live/dead. The legalization reading claims the problem persists (racial disparity in arrests continues, illicit market persists, new substances emerge). Prohibition_reading claims the problem is dead (legalization creates worse harms). Harm_reduction_reading claims the problem is live but misdiagnosed — the solution is decriminalization + health services, not commercial markets. The constraint avoids mandatrophy only if the founding problem remains live AND the arrangement continues to solve it better than alternatives. Corporate capture and externalized third-party harms suggest drift toward mandatrophy: the arrangement persists because beneficiaries (corporations, tax authorities) profit, not because it optimally solves the founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legalization_reading_kernel_framing,
    'Is this constraint a reading of the substance_control_legitimacy kernel, or a standalone constraint?',
    'Compare structural parameters (beneficiaries, victims, extractiveness) across all three declared readings. If they share a referent (the standing substance control arrangement) but instantiate different ε and victim sets, they are kernel readings. If they describe different arrangements, they are independent constraints.',
    'If kernel readings, the engine''s constraint family analysis applies: cross-reading contamination, drift_state comparison, axiom foreclosure logic. If independent, each is analyzed in isolation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legalization_reading_kernel_framing, conceptual, 'Commitment-system framing: whether the three declared positions are readings of one kernel or independent constraints.').

omega_variable(
    corporate_capture_vs_coordination,
    'Does the legal market''s coordination function (quality control, age verification, tax collection) require corporate commercialization, or could a non-commercial regulated model (state monopoly, nonprofit distribution) achieve the same coordination with less extraction?',
    'Natural experiments: compare outcomes in jurisdictions with commercial markets (Colorado, Washington) vs. state monopoly models (Canada''s early cannabis framework, Uruguay) vs. nonprofit social club models (Spain''s cannabis associations). Measure extraction (price vs. cost), youth access, illicit market displacement, and third-party harms.',
    'If coordination is achievable without corporate extraction, the legalization reading''s claimed rope-type is contaminated by a separable extractive layer — the constraint decomposes into a coordination rope (state-regulated distribution) and an extractive tangled_rope (corporate commercialization). This would trigger ε-invariance decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corporate_capture_vs_coordination, empirical, 'Whether corporate commercialization is structurally necessary for the coordination function or an extractive overlay.').

omega_variable(
    third_party_harm_measurement,
    'How should third-party harms (impaired driving, secondhand exposure, community externalities) be weighed against user autonomy benefits in the constraint''s extractiveness assessment?',
    'Longitudinal studies comparing pre/post legalization: impaired driving fatalities per VMT, secondhand exposure biomarkers in non-users, healthcare utilization in high-density retail zones, stratified by socioeconomic status. Counterfactual modeling of harm under prohibition vs. legalization vs. decriminalization.',
    'If third-party harms are substantial and rising, the constraint''s extractiveness is understated by user-centric metrics — the reading''s claimed rope-type masks snare-type dynamics for non-consenting parties. This would support reclassification toward tangled_rope or snare for affected seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(third_party_harm_measurement, empirical, 'Whether third-party harms under legalization constitute extractive externalities that alter the constraint''s classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__legalization_reading, 2012, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(substance_control_legitimacy__legalization_reading_tr_t2012, substance_control_legitimacy__legalization_reading, theater_ratio, 2012, 0.05).
narrative_ontology:measurement(substance_control_legitimacy__legalization_reading_tr_t2014, substance_control_legitimacy__legalization_reading, theater_ratio, 2014, 0.08).
narrative_ontology:measurement(substance_control_legitimacy__legalization_reading_tr_t2016, substance_control_legitimacy__legalization_reading, theater_ratio, 2016, 0.1).
narrative_ontology:measurement(substance_control_legitimacy__legalization_reading_tr_t2018, substance_control_legitimacy__legalization_reading, theater_ratio, 2018, 0.12).
narrative_ontology:measurement(substance_control_legitimacy__legalization_reading_tr_t2020, substance_control_legitimacy__legalization_reading, theater_ratio, 2020, 0.13).
narrative_ontology:measurement(substance_control_legitimacy__legalization_reading_tr_t2022, substance_control_legitimacy__legalization_reading, theater_ratio, 2022, 0.14).
narrative_ontology:measurement(substance_control_legitimacy__legalization_reading_tr_t2024, substance_control_legitimacy__legalization_reading, theater_ratio, 2024, 0.15).
narrative_ontology:measurement(substance_control_legitimacy__legalization_reading_tr_t2026, substance_control_legitimacy__legalization_reading, theater_ratio, 2026, 0.15).

% Extraction over time
narrative_ontology:measurement(substance_control_legitimacy__legalization_reading_be_t2012, substance_control_legitimacy__legalization_reading, base_extractiveness, 2012, 0.15).
narrative_ontology:measurement(substance_control_legitimacy__legalization_reading_be_t2014, substance_control_legitimacy__legalization_reading, base_extractiveness, 2014, 0.22).
narrative_ontology:measurement(substance_control_legitimacy__legalization_reading_be_t2016, substance_control_legitimacy__legalization_reading, base_extractiveness, 2016, 0.28).
narrative_ontology:measurement(substance_control_legitimacy__legalization_reading_be_t2018, substance_control_legitimacy__legalization_reading, base_extractiveness, 2018, 0.32).
narrative_ontology:measurement(substance_control_legitimacy__legalization_reading_be_t2020, substance_control_legitimacy__legalization_reading, base_extractiveness, 2020, 0.35).
narrative_ontology:measurement(substance_control_legitimacy__legalization_reading_be_t2022, substance_control_legitimacy__legalization_reading, base_extractiveness, 2022, 0.37).
narrative_ontology:measurement(substance_control_legitimacy__legalization_reading_be_t2024, substance_control_legitimacy__legalization_reading, base_extractiveness, 2024, 0.38).
narrative_ontology:measurement(substance_control_legitimacy__legalization_reading_be_t2026, substance_control_legitimacy__legalization_reading, base_extractiveness, 2026, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(substance_control_legitimacy__legalization_reading_su_t2012, substance_control_legitimacy__legalization_reading, suppression_requirement, 2012, 0.1).
narrative_ontology:measurement(substance_control_legitimacy__legalization_reading_su_t2014, substance_control_legitimacy__legalization_reading, suppression_requirement, 2014, 0.15).
narrative_ontology:measurement(substance_control_legitimacy__legalization_reading_su_t2016, substance_control_legitimacy__legalization_reading, suppression_requirement, 2016, 0.18).
narrative_ontology:measurement(substance_control_legitimacy__legalization_reading_su_t2018, substance_control_legitimacy__legalization_reading, suppression_requirement, 2018, 0.2).
narrative_ontology:measurement(substance_control_legitimacy__legalization_reading_su_t2020, substance_control_legitimacy__legalization_reading, suppression_requirement, 2020, 0.21).
narrative_ontology:measurement(substance_control_legitimacy__legalization_reading_su_t2022, substance_control_legitimacy__legalization_reading, suppression_requirement, 2022, 0.22).
narrative_ontology:measurement(substance_control_legitimacy__legalization_reading_su_t2024, substance_control_legitimacy__legalization_reading, suppression_requirement, 2024, 0.22).
narrative_ontology:measurement(substance_control_legitimacy__legalization_reading_su_t2026, substance_control_legitimacy__legalization_reading, suppression_requirement, 2026, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__legalization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(substance_control_legitimacy__legalization_reading, 0.15).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, substance_control_legitimacy__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, substance_control_legitimacy__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, impaired_driving_enforcement).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, secondhand_exposure_regulation).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, cannabis_corporate_lobbying).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, drug_war_mass_incarceration).

% DUAL FORMULATION NOTE:
% The substance_control_legitimacy kernel decomposes into three readings with distinct ε and victim sets: prohibition_reading (ε ~ 0.85, users as victims), harm_reduction_reading (ε ~ 0.25, users as beneficiaries, no corporate layer), legalization_reading (ε ~ 0.38, users as beneficiaries, corporate extraction layer, third-party victims). The legalization reading's commercial market creates corporate power that influences the harm_reduction reading's political viability and the prohibition_reading's enforcement focus. All three share the referent (the standing substance control arrangement) but instantiate different constraints per ε-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_legitimacy__legalization_reading, powerless, 0.95).
constraint_indexing:directionality_override(substance_control_legitimacy__legalization_reading, powerless, 0.85).
constraint_indexing:directionality_override(substance_control_legitimacy__legalization_reading, organized, 0.15).
constraint_indexing:directionality_override(substance_control_legitimacy__legalization_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
