% ============================================================================
% CONSTRAINT STORY: substance_control_authority__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: substance_control_authority__harm_reduction_reading
 *   human_readable: State Authority to Accept Drug Use While Minimizing Health Harms (Harm Reduction Reading)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   The harm-reduction reading of state substance control authority
 *   represents a fundamental reframing of the state's relationship to drug
 *   use: from criminalization (prohibition reading) through acceptance with
 *   managed health outcomes (harm reduction) to potential full legalization
 *   (legalization reading). Under this reading, the state explicitly accepts
 *   that drug use will occur and that criminal punishment is ineffective or
 *   counterproductive; the state's authority shifts from enforcing abstinence
 *   to minimizing health and social harms through public health services,
 *   decriminalization, and evidence-based treatment. This creates a tangled
 *   rope structure: genuine coordination benefits (reduced overdose deaths,
 *   improved public health, restoration of civil rights to people who use
 *   drugs) coexist with unresolved extraction mechanisms (third parties bear
 *   disease transmission risks, geographic concentration of services
 *   externalizes visible disorder, public health systems become de facto
 *   social services without adequate funding). The constraint exhibits a
 *   declining suppression trajectory (from 0.72 to 0.42) as criminalization
 *   enforcement machinery is dismantled, replaced by service-based
 *   coordination. Theater ratio remains moderate (0.48) because the
 *   constraint involves genuine health interventions (evidence-based
 *   treatment) alongside performative commitment to 'acceptance' that often
 *   masks continued social stigma and geographic segregation of services.
 *   Extractiveness grows modestly (0.18 to 0.38) as the initial period of
 *   rapid service expansion encounters the realities of concentrated
 *   externalities and underfunded systems.
 *
 * KEY AGENTS:
 *   - People Who Use Drugs: Primary beneficiary (moderate/mobile) — exit criminal victim set; access treatment and civil rights; remain health-harm victims but through service pathway rather than carceral control
 *   - Third Parties (Disease Transmission & Crime): Primary victims (powerless/trapped) — bear disease and crime externalities; geographic concentration of services creates visible markers of drug use in their communities
 *   - Marginalized Communities with Concentrated Services: Secondary victims (powerless/trapped) — service infrastructure zoning creates externalized harms (visible drug use, needle litter, social disorder) in lower-income neighborhoods
 *   - Public Health Systems: Institutional beneficiary (institutional/constrained) — gain coordination function (health-centered approach) but face unsustainable scope creep and service demand
 *   - Criminal Justice System: Institutional authority losing legitimacy (institutional/arbitrage) — traditional drug enforcement mandate is degraded by harm-reduction norms; transforms to dealer-focused prosecution while user-level authority erodes
 *   - Drug Policy Reform Coalitions: Organized actors (organized/constrained) — see harm reduction as transitional scaffold toward further reform; build political pressure for continued movement
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing harm reduction as the only rational response while foreclosing alternatives like legalization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__harm_reduction_reading, 0.38).
domain_priors:suppression_score(substance_control_authority__harm_reduction_reading, 0.42).
domain_priors:theater_ratio(substance_control_authority__harm_reduction_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__harm_reduction_reading, "State Authority to Accept Drug Use While Minimizing Health Harms (Harm Reduction Reading)").
narrative_ontology:topic_domain(substance_control_authority__harm_reduction_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__harm_reduction_reading, 'c8c2e474-2575-4272-bf23-e96d1af40165').
narrative_ontology:cs_kernel_codification('c8c2e474-2575-4272-bf23-e96d1af40165', distributed).
narrative_ontology:cs_authority_grounding('c8c2e474-2575-4272-bf23-e96d1af40165', lineage).
narrative_ontology:cs_interpretation_layer_present('c8c2e474-2575-4272-bf23-e96d1af40165').
narrative_ontology:cs_reading_relation('c8c2e474-2575-4272-bf23-e96d1af40165', substance_control_authority__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('c8c2e474-2575-4272-bf23-e96d1af40165', substance_control_authority__legalization_reading, influences).
narrative_ontology:cs_axiom('c8c2e474-2575-4272-bf23-e96d1af40165', foundational, drug_use_inevitability).
narrative_ontology:cs_axiom_status(drug_use_inevitability, holdable).
narrative_ontology:cs_axiom_grounding('c8c2e474-2575-4272-bf23-e96d1af40165', drug_use_inevitability, empirically_contingent).
narrative_ontology:cs_axiom('c8c2e474-2575-4272-bf23-e96d1af40165', foundational, minimization_duty_over_elimination_duty).
narrative_ontology:cs_axiom_status(minimization_duty_over_elimination_duty, holdable).
narrative_ontology:cs_axiom_grounding('c8c2e474-2575-4272-bf23-e96d1af40165', minimization_duty_over_elimination_duty, deontological).
narrative_ontology:cs_reference_frame('c8c2e474-2575-4272-bf23-e96d1af40165', health_centered_drug_policy).
narrative_ontology:cs_drift_state('c8c2e474-2575-4272-bf23-e96d1af40165', contemporary_post_evidence_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c8c2e474-2575-4272-bf23-e96d1af40165', '').
narrative_ontology:cs_kernel_id(substance_control_authority__harm_reduction_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, public_health_systems).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, communities_with_reduced_overdose).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, third_parties_disease_transmission).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, communities_bearing_crime_externalities).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, taxpayers_funding_services).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PEOPLE WHO USE DRUGS (ROPE) — Exit from criminal victim set removes incarceration barrier; health harms remain but are managed through accessible services rather than carceral suppression. Experience the constraint as genuine coordination: public health framing enables treatment access, harm reduction supplies, and community reintegration. Moderate power through political mobilization and lived expertise. Mobile exit option reflects ability to access services in harm-reduction jurisdictions.
constraint_indexing:constraint_classification(substance_control_authority__harm_reduction_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 2: THIRD PARTIES / DISEASE & CRIME EXTERNALITY BEARERS (TANGLED ROPE) — Harm reduction creates genuine coordination benefit (reduced overdose deaths in community, lower incarceration-driven crime) but shifts burden. Disease transmission (HIV, Hepatitis C) remains a risk despite needle exchange programs; communities with high drug use may experience elevated property crime and street-level disorder even as incarceration drops. Trapped because geographic mobility is low for vulnerable populations; extraction is substantial but not total because genuine public health improvements are visible (reduced OD deaths).
constraint_indexing:constraint_classification(substance_control_authority__harm_reduction_reading, tangled_rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: MARGINALIZED COMMUNITIES / CONCENTRATED BURDEN (SNARE) — Harm reduction concentrates service infrastructure and drug use in specific neighborhoods, creating visible open-air use, needle litter, and social disorder in lower-income areas. Third parties with no mobility (renters, merchants, residents without exit capacity) experience extraction: zoning decisions that concentrate services, externalized social costs, and minimal compensation. Victims rather than beneficiaries despite the policy's public health intent.
constraint_indexing:constraint_classification(substance_control_authority__harm_reduction_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: PUBLIC HEALTH SYSTEMS & SERVICE PROVIDERS (TANGLED ROPE) — Genuine coordination function (delivering evidence-based care, reducing overdose deaths, building community trust) alongside extraction (unsustainable burden on underfunded services, scope creep beyond treatment into housing/employment/social support, professional liability for clients' subsequent harms). Institutional power but constrained by budget cycles, intergovernmental politics, and legitimacy demands. Effective extraction chi is moderate because genuine health coordination exists alongside resource extraction.
constraint_indexing:constraint_classification(substance_control_authority__harm_reduction_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DRUG POLICY REFORM COALITIONS (SCAFFOLD) — Organized agents (public health advocates, incarcerated people's rights groups, evidence-based treatment providers) see harm reduction as a transitional framework toward either decriminalization (full reform) or, paradoxically, toward normalized acceptance of managed use without the prohibition/legalization binary. Sunset clause: harm reduction is intentionally temporary — a bridge between criminal enforcement and either full decriminalization or regulated legal markets. Low effective extraction because the coalition has agency and sees an exit path.
constraint_indexing:constraint_classification(substance_control_authority__harm_reduction_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CRIMINAL JUSTICE SYSTEM / LAW ENFORCEMENT (PITON) — Traditional drug enforcement apparatus loses legitimacy and enforcement capacity as harm reduction norms displace prohibition. Police authority to criminalize drug possession becomes performative theater — enforcement persists in specific jurisdictions despite harm-reduction evidence, maintained through institutional inertia rather than functional authority. The criminal justice system sees its own mandate as degraded (treating addiction as disease rather than crime) and attempts to reassert control through targeting dealers rather than users — a shift that maintains the appearance of enforcement while ceding ground on user-level authority. Piton classification: theater ratio high, extractiveness low (enforcement machinery persists but is no longer the primary control mechanism).
constraint_indexing:constraint_classification(substance_control_authority__harm_reduction_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, drug use is presented as inevitable human behavior (neurobiological fact that some people will use drugs regardless of legal status) and overdose death is presented as immutable consequence of unregulated supply. This reading naturalizes the harm-reduction framework as the only rational response to unavoidable harms. However, the structural data reveals this as a false summit: the declaration of 'inevitability' is itself a policy choice that forecloses criminalization alternatives and presupposes the legitimacy of state management of drug use through public health rather than through prohibition or legalization.
constraint_indexing:constraint_classification(substance_control_authority__harm_reduction_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__harm_reduction_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(substance_control_authority__harm_reduction_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(substance_control_authority__harm_reduction_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(substance_control_authority__harm_reduction_reading, TR),
    TR >= 0.70.

:- end_tests(substance_control_authority__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.38 final): Moderate. The harm-reduction reading creates real benefits for people who use drugs (exit from criminal system, access to treatment, dignity restoration) but simultaneously shifts burdens to third parties. The extractiveness increases over the measurement interval (0.18 → 0.38) as initial rapid service expansion encounters the structural realities of concentrated externalities (disease transmission, visible disorder, community disruption) that are not fully resolved by decriminalization. The baseline period (t=0) shows lower extractiveness because the early rhetoric emphasizes only the benefits to PWUD without fully accounting for third-party costs. By t=10, as services mature and externalities become visible, extractiveness settles at 0.38 — substantial but not severe, because genuine public health coordination exists alongside the extraction. SUPPRESSION (0.42 final): Moderate-high, sharply declining trajectory (0.72 → 0.42). Suppression measures the barriers to exit and alternatives that would allow people to avoid the constraint. Under prohibition, suppression is high (criminalization prevents exit from drug markets; incarceration traps people in the criminal system). Harm reduction dramatically reduces this suppression by decriminalizing drug use and providing exit pathways through treatment. However, suppression does not drop to zero because: (1) geographic segregation of services means people in non-harm-reduction jurisdictions face barriers to access; (2) stigma remains high; (3) employment and housing barriers for people with drug use histories persist. The declining trajectory reflects the policy transition from high-coercion enforcement to lower-coercion service-based approach. THEATER RATIO (0.48 final): Moderate, slightly rising (0.35 → 0.48). Theater measures performative vs. functional activity. Early harm reduction involves genuine functional work (establishing services, training staff, building community trust). But as the constraint matures, theater elements emerge: harm reduction becomes a policy banner that masks continued criminalization of dealers; public commitment to 'accepting drug use' coexists with continued stigma and geographic segregation; metrics of success (reduced overdose deaths) can be achieved through service expansion that doesn't fully resolve underlying social factors (poverty, trauma, exclusion from legitimate opportunity). The moderate theater ratio reflects that the constraint is neither purely performative (services are real and effective) nor purely functional (political and social elements persist).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of DR classifications from different structural positions. People who use drugs see coordination and exit (Rope) — decriminalization genuinely solves their most acute problem (criminal punishment). Public health systems see tangled rope (genuine coordination function with scope creep and underfunding). Marginalized communities bearing concentrated harms see snare (benefit accrues to broader society, costs concentrated on those unable to exit). Criminal justice system sees piton (its enforcement mandate erodes while maintaining performative drug control). Drug policy reform coalitions see scaffold (intentional transitional structure with sunset toward further reform). The analytical observer risks seeing mountain (presenting harm reduction as the inevitable rational response to unavoidable drug use) but the structural data reveals this as a false summit: the 'inevitability' presupposes specific policy choices and forecloses the legalization and stronger prohibition readings. The perspectival gaps are not mere disagreement about outcomes but reflect genuinely different structural relationships to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is determined by beneficiary/victim status combined with exit options. People who use drugs as beneficiaries with mobile exit (can relocate to harm-reduction jurisdictions, can access treatment) derive d ≈ 0.35 (moderate power through mobility and political voice), producing χ that reflects genuine coordination benefits. Third parties as victims with trapped exit (cannot relocate from communities with concentrated services, cannot escape disease/crime externalities) derive d ≈ 0.85 (high extraction target), producing χ that reflects substantial experienced extraction. Public health systems as institutional actors with constrained exit (can seek funding elsewhere but are bound by mandate to serve) derive d ≈ 0.55 (moderate extraction), producing χ that reflects tangled rope. The criminal justice system as institutional actor with arbitrage options (can pivot to dealer prosecution, can maintain enforcement elsewhere) derives d ≈ 0.20 (beneficiary position despite legitimacy loss), producing low effective χ from the piton perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by acknowledging that the harm-reduction reading is structurally TANGLED ROPE, not pure coordination and not pure extraction. Mandatrophy would force a choice between 'is this coordination (Rope) or extraction (Snare)?' — but the harm-reduction constraint is genuinely both. People who use drugs experience coordination (exit from criminalization + access to treatment). Third parties experience extraction (disease and crime externalities concentrated on trapped populations). Public health systems experience tangled rope (genuine health function + unsustainable burden). The mandatrophy is resolved by accepting that the constraint operates differently across agent types and that 'the' classification is not a single type but a perspectival presheaf. The false summit at the analytical level reveals that naturalizing harm reduction as 'the only rational response' forecloses the legalization and stronger prohibition readings, masking the policy choice as natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    service_infrastructure_concentration_externality,
    'Does concentrating harm-reduction service infrastructure (needle exchanges, supervised consumption sites, medication-assisted treatment) in specific neighborhoods constitute unavoidable geography or extractive zoning that displaces visible drug use to vulnerable communities?',
    'Comparative analysis of service location decisions across harm-reduction jurisdictions; cost-benefit analysis of service siting vs. dispersion vs. mobile services; longitudinal resident displacement and property value data in neighborhoods with concentrated services',
    'If unavoidable: harm reduction is tangled rope from third-party perspective (genuine public health benefit + structural externality). If extractive zoning: harm reduction is snare for marginalized communities (benefits accrue to broader society, costs concentrated on trapped populations).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(service_infrastructure_concentration_externality, empirical, 'Whether service concentration is unavoidable infrastructure or extractive zoning').

omega_variable(
    third_party_disease_transmission_magnitude,
    'What proportion of HIV/Hepatitis C transmission in harm-reduction jurisdictions occurs through needle sharing despite access to sterile supplies, vs. through other routes (sexual contact, occupational exposure)?',
    'Epidemiological cohort studies in harm-reduction vs. prohibition-enforcement jurisdictions; molecular phylogenetics of viral strains to identify transmission routes; needle-sharing behavior surveillance in populations with sterile supply access',
    'If needle-sharing transmission remains high despite harm reduction: disease externality is substantial and unmitigated (snare/extraction confirmed). If transmission drops to background rates: harm reduction genuinely addresses the disease vector and third-party burden is minimal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(third_party_disease_transmission_magnitude, empirical, 'Magnitude of disease transmission despite needle exchange access').

omega_variable(
    legalization_counterfactual_externality,
    'If drug use were fully legalized with market regulation, would third-party harms (property crime, disease transmission, social disorder) be lower, equal, or higher than under harm reduction?',
    'Synthetic control analysis comparing jurisdictions with harm reduction, legalization, and prohibition; natural experiments in jurisdictions transitioning between regimes; economic analysis of crime and disease under regulated vs. unregulated drug markets',
    'If legalization produces lower third-party harms: harm reduction is not the optimal policy and represents a compromise reading constrained by political feasibility rather than evidence. If harm reduction produces equal or lower harms: harm reduction is structurally justified and the reading''s core claim is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legalization_counterfactual_externality, empirical, 'Comparative third-party harms under legalization vs. harm reduction').

omega_variable(
    kernel_reading_distinction,
    'Is this harm-reduction reading genuinely distinct from the legalization reading, or does it represent an intermediate position on a continuum toward legalization?',
    'Analysis of core normative commitments: harm reduction presumes state authority to accept drug use AND to minimize harms through services; legalization presumes state authority to regulate drug markets as commerce. If harm reduction ever decriminalizes fully and permits commercial regulated markets, it has moved toward legalization. Conversely, if legalization imposes health-monitoring and service requirements resembling harm reduction, the readings converge.',
    'If distinct: harm reduction and legalization are coexisting readings that influence each other but do not foreclose each other. If continuous: harm reduction is a transitional reading between prohibition and legalization (the scaffold perspective becomes the core).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Whether harm reduction is distinct from legalization or intermediate on a continuum').

omega_variable(
    crime_externality_causation,
    'What proportion of property crime and street-level disorder in communities with high drug use is caused by drug use itself (pharmacological effects, need for income to purchase drugs) vs. by criminalization and incarceration (destabilized social networks, criminal markets, coercive control)? Does decriminalization reduce this crime externality or does it merely relocate visible markers of disorder?',
    'Quasi-experimental design comparing crime trends pre/post decriminalization in matched jurisdictions; analysis of crime composition (drug-related property crime vs. other property crime vs. violent crime) across regimes; qualitative documentation of criminogenic mechanisms in prohibition vs. harm reduction',
    'If crime externality is largely endogenous to criminalization: decriminalization genuinely reduces third-party harms (rope classification for third parties confirmed). If crime externality persists or shifts to less visible forms: harm reduction does not resolve the externality, only obscures it (snare/extraction confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crime_externality_causation, empirical, 'Causal mechanism of crime externality under drug use vs. criminalization').

omega_variable(
    reading_authority_source,
    'What grounds the legitimacy of the harm-reduction reading of state authority? Is it grounded in public health evidence (empirical), in human rights principles (deontological), in pragmatic acceptance of unavoidable drug use (instrumental), or in medical/public health professional expertise (expertise)?',
    'Historical and rhetorical analysis of harm-reduction advocacy; examination of which legitimacy claims are contested (e.g., some religious traditions reject pragmatic acceptance of drug use; some law-enforcement perspectives reject the medical framing)',
    'If grounded in empirical evidence: the reading is vulnerable to refutation by contrary evidence (e.g., discovery of more effective prohibition mechanisms). If grounded in deontological human rights claims: the reading is more robust to empirical challenge. If grounded in expertise: the reading is vulnerable to professional legitimacy contests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_authority_source, conceptual, 'Epistemic grounding of harm-reduction reading''s legitimacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__harm_reduction_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scahr_tr_t0, substance_control_authority__harm_reduction_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(scahr_tr_t5, substance_control_authority__harm_reduction_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement(scahr_tr_t10, substance_control_authority__harm_reduction_reading, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(scahr_be_t0, substance_control_authority__harm_reduction_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(scahr_be_t5, substance_control_authority__harm_reduction_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(scahr_be_t10, substance_control_authority__harm_reduction_reading, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(scahr_su_t0, substance_control_authority__harm_reduction_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(scahr_su_t5, substance_control_authority__harm_reduction_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(scahr_su_t10, substance_control_authority__harm_reduction_reading, suppression_requirement, 10, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__harm_reduction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, substance_control_authority__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, substance_control_authority__legalization_reading).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, opioid_supply_chain_contamination).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, incarceration_recidivism_cycles).

% DUAL FORMULATION NOTE:
% The substance_control_authority kernel has three readings: prohibition_reading (criminalization regime), legalization_reading (market regulation regime), and harm_reduction_reading (THIS STORY — service-based decriminalization). Each reading is a separate constraint with its own ε value because the core claim (what authority means) and the evidence base differ structurally. The three readings coexist and influence each other: harm reduction provides evidence against prohibition's deterrence claim; legalization provides evidence for alternatives to harm reduction's service-intensive model; prohibition provides evidence that criminalization creates additional harms. Network edges link all three readings so that challenges to one reading's empirical claims (e.g., legalization evidence that regulated markets reduce crime) affect the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_authority__harm_reduction_reading, powerless, 0.85).
constraint_indexing:directionality_override(substance_control_authority__harm_reduction_reading, moderate, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
