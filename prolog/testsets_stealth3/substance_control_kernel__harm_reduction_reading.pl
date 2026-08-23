% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__harm_reduction_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: substance_control_kernel__harm_reduction_reading
 *   human_readable: Harm-Reduction Governance Regime: Substance Use as Managed Health Condition
 *   domain: public health policy / criminal justice / political economy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the substance-control kernel: the
 *   harm-reduction reading, in which substance use is governed as a health
 *   condition and pragmatic intervention is justified independently of
 *   whether use ceases. Under this reading users exit the criminal victim set
 *   for personal use but enter a paternalistic health-management relation
 *   (summons, referral, compulsory treatment on refusal, drug-court
 *   conditions); enforcement recedes on the use side while the supply chain
 *   remains fully criminalized; overdose mortality and disease transmission
 *   become the primary observables; and the state shifts toward
 *   service-provider. The ε referent is this standing harm-reduction
 *   arrangement itself, assessed by the reading's own lights — NOT the
 *   prohibition arrangement it replaced and NOT the legalized market some of
 *   its proponents anticipate; those are separate stories with separate
 *   victim sets and separate ε. The claim/metric split is deliberate: the
 *   reading is CLAIMED here as tangled_rope (genuine epidemic-coordination
 *   function entangled with asymmetric extraction through the same
 *   structure), while the metrics are authored descriptively of the regime's
 *   actual operation. KEY AGENTS (by structural relationship): -
 *   people_who_use_drugs: Dual-positioned principal
 *   ([moderate]/[constrained]) — collects services and decriminalized status,
 *   bears paternalistic supervision - illicit_supply_chain_workers:
 *   Concentrated enforcement target ([powerless]/[trapped]) — the retained
 *   criminal victim set - public_health_authorities: Agenda-setter and
 *   institutional beneficiary ([institutional]/[constrained]) — administers
 *   and grows with the regime - addiction_treatment_service_providers:
 *   Institutional beneficiary ([institutional]/[identity_locked]) — budget
 *   scales with regime continuance - law_enforcement_agencies:
 *   Dual-positioned institutional actor ([institutional]/[constrained]) —
 *   loses use-side mandate, retains supply file -
 *   host_neighborhood_residents: Localized payer ([organized]/[constrained])
 *   — concentrated site costs, diffuse benefits elsewhere -
 *   rural_substance_users: Excluded voice ([powerless]/[trapped]) — reached
 *   by data systems, unreached by services - general_taxpayers: Diffuse payer
 *   ([moderate]/[constrained]) - academic_harm_reduction_evaluators:
 *   Analytical observer ([analytical]/[analytical])
 *
 * KEY AGENTS:
 *   - people_who_use_drugs: dual-positioned principal — services received, paternalistic supervision borne
 *   - illicit_supply_chain_workers: concentrated enforcement target under retained supply criminalization
 *   - public_health_authorities: agenda-setter administering guidelines, licensing, and surveillance
 *   - addiction_treatment_service_providers: contract-funded sector whose budget scales with the regime
 *   - law_enforcement_agencies: lose use-side volumes, retain supply interdiction and forfeiture
 *   - host_neighborhood_residents: bear concentrated site-adjacent costs
 *   - rural_substance_users: excluded from service geography and planning tables
 *   - general_taxpayers: diffuse financiers of the apparatus
 *   - academic_harm_reduction_evaluators: analytical observers anchoring the evidence base
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__harm_reduction_reading, 0.47).
domain_priors:suppression_score(substance_control_kernel__harm_reduction_reading, 0.41).
domain_priors:theater_ratio(substance_control_kernel__harm_reduction_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, extractiveness, 0.47).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__harm_reduction_reading, "Harm-Reduction Governance Regime: Substance Use as Managed Health Condition").
narrative_ontology:topic_domain(substance_control_kernel__harm_reduction_reading, "public health policy / criminal justice / political economy").

domain_priors:requires_active_enforcement(substance_control_kernel__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__harm_reduction_reading, '80cb27bb-455e-49d7-9833-df4c9d55c6d1').
narrative_ontology:cs_kernel_codification('80cb27bb-455e-49d7-9833-df4c9d55c6d1', formalized).
narrative_ontology:cs_authority_grounding('80cb27bb-455e-49d7-9833-df4c9d55c6d1', expertise).
narrative_ontology:cs_interpretation_layer_present('80cb27bb-455e-49d7-9833-df4c9d55c6d1').
narrative_ontology:cs_reading_relation('80cb27bb-455e-49d7-9833-df4c9d55c6d1', substance_control_kernel__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('80cb27bb-455e-49d7-9833-df4c9d55c6d1', substance_control_kernel__legalization_reading, influences).
narrative_ontology:cs_axiom('80cb27bb-455e-49d7-9833-df4c9d55c6d1', foundational, intervention_legitimate_without_abstinence).
narrative_ontology:cs_axiom_status(intervention_legitimate_without_abstinence, holdable).
narrative_ontology:cs_axiom_grounding('80cb27bb-455e-49d7-9833-df4c9d55c6d1', intervention_legitimate_without_abstinence, instrumental).
narrative_ontology:cs_axiom('80cb27bb-455e-49d7-9833-df4c9d55c6d1', foundational, care_not_sanction_for_personal_use).
narrative_ontology:cs_axiom_status(care_not_sanction_for_personal_use, holdable).
narrative_ontology:cs_axiom_grounding('80cb27bb-455e-49d7-9833-df4c9d55c6d1', care_not_sanction_for_personal_use, deontological).
narrative_ontology:cs_reference_frame('80cb27bb-455e-49d7-9833-df4c9d55c6d1', evidence_led_health_stewardship).
narrative_ontology:cs_drift_state('80cb27bb-455e-49d7-9833-df4c9d55c6d1', contemporary_fentanyl_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('80cb27bb-455e-49d7-9833-df4c9d55c6d1', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__harm_reduction_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, addiction_treatment_service_providers).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, public_health_authorities).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, illicit_supply_chain_workers).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, host_neighborhood_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, law_enforcement_agencies).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, law_enforcement_agencies).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, general_taxpayers).
narrative_ontology:constraint_vindicates(substance_control_kernel__harm_reduction_reading, decoupling_care_from_abstinence_reduces_mortality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use opioids, stimulants, and other controlled substances. Under this regime they can carry naloxone, exchange syringes, consume under supervision, and obtain medication-assisted treatment without committing to abstinence. The same regime can summon them before health commissions, refer them to compulsory treatment on refusal, enroll them in drug-court supervision where liberty is conditioned on compliance and urine screening, and register them in case-management databases. Leaving the regime's reach generally means leaving the jurisdiction or ending use, which dependency makes rare; moving between cities resets access but not status.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, people_who_use_drugs, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__harm_reduction_reading, people_who_use_drugs, payer).

% Operate clinics, supervised consumption sites, outreach teams, and medication programs funded by public contracts that scale with the regime's continuation. Staff, facilities, and missions are built around service delivery to people who use drugs; winding the regime down would strand capital, careers, and client relationships. Organizational identity has fused with the served population.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, addiction_treatment_service_providers, beneficiary,
    institutional, generational, identity_locked, national).

% Design clinical guidelines, license and inspect supervised sites, distribute naloxone stockpiles, and publish surveillance data. They set the operational agenda for the whole arrangement, and their budgets and jurisdictional remit expand with it. Civil-service mandates and political oversight bound how far they can move ahead of statute, and they cannot simply abandon the portfolio.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__harm_reduction_reading, public_health_authorities, beneficiary).

% Grow, transport, cut, and retail prohibited substances, and are frequently people who use themselves. Personal use is removed from the criminal docket under this regime, but everything upstream remains a criminal matter, so enforcement effort concentrates on them. A record forecloses licensed work and housing, the income is survival-level, and exiting the trade usually means destitution.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, illicit_supply_chain_workers, payer,
    powerless, immediate, trapped, global).

% Lose arrest volumes, pretext-stop authority, and a traditional measure of institutional success as personal use leaves the criminal docket. They retain and refocus the supply file, asset forfeiture, and newly funded duties such as naloxone carriage and site liaison. Rank-and-file culture experiences the shift as a demotion of mandate even as parts of the budget redirect toward them.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, law_enforcement_agencies, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__harm_reduction_reading, law_enforcement_agencies, beneficiary).

% Live and run businesses around supervised consumption sites and open drug scenes. They absorb discarded equipment, loitering, and visible disorder while the mortality and transmission benefits are counted city-wide. Neighborhood associations petition councils, contest siting decisions, and can force facility relocation; moving away themselves is possible but costly.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, host_neighborhood_residents, payer,
    organized, biographical, constrained, local).

% Use substances far from the urban centers where services concentrate. Supervised sites, syringe programs, and prescribers are thin or absent in their counties, so the arrangement's protections largely bypass them even as its referral mechanisms and data systems still reach them. No one represents them at the tables where service maps are drawn.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, rural_substance_users, excluded,
    powerless, biographical, trapped, regional).

% Finance the service network through appropriations and insurance levies. They see line items but little outcome detail, and they cannot opt out of funding. Whether the spend reads as bargain or burden depends on which mortality and cost-offset figures one credits.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, general_taxpayers, payer,
    moderate, biographical, constrained, national).

% Run cohort studies and site evaluations, publish mortality and transmission findings, and advise ministries. Their evidence base anchors the regime's public justification, and their careers and research funding ride partly on continued evaluation demand — a fact that cuts both ways for their independence.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, academic_harm_reduction_evaluators, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__harm_reduction_reading, addiction_treatment_service_providers).
narrative_ontology:fixing_cost_class(substance_control_kernel__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages the collective-health problem posed by widespread substance use: centralized naloxone distribution, supervised consumption, sterile equipment provision, and medication-assisted treatment reduce overdose mortality and blood-borne disease transmission at scales individual users, families, and scattered clinics cannot achieve alone.
% TRANSFER_FUNCTION: Moves tax revenue and grant funds into a network of treatment and harm-reduction service organizations; moves users' compliance (attendance, disclosure, biological monitoring) to state agencies and contracted providers; retains criminal sanction applied to supply-chain participants; redirects some police capacity from use-side arrest to emergency response and interdiction.
% ABSENT_VOICES: Rural users outside service catchments; people serving sentences under the retained supply-side penalties; users who reject the client/patient frame and want autonomy rather than enrollment; unlicensed suppliers who would compete with the sanctioned channel. All would object if seated; none currently is.
% DISAPPEARANCE_RATIONALE: Overnight removal would collapse naloxone distribution, supervised consumption, and medication-assisted treatment; overdose deaths and HIV/HCV incidence would climb within months as users returned to unmanaged street supply; the funded service sector would unwind; police would re-absorb use-side enforcement; neighborhoods hosting former sites would see rapid visible change. Arrangements across health systems, criminal dockets, and municipal budgets depend on the regime's continuance.
% FOUNDING_PROBLEM: Injection-driven HIV and hepatitis C epidemics, and later mass opioid overdose mortality, were spreading because criminalization drove users away from health contact: people avoided syringe services, hid use, and died without witnesses. The arrangement was built to sever health outcomes from use-status — to treat the body whatever the legal theory of the behavior.
% FOUNDING_PROBLEM_CORROBORATION: National coroner and medical-examiner mortality series and WHO/UNAIDS surveillance independently attest that the founding problems — overdose deaths and injection-driven blood-borne transmission — remain live. One caveat stated plainly: much corroborating epidemiology is produced by researchers whose funding streams connect to implementing agencies, so corroboration is strong but not wholly outside the beneficiary orbit.
narrative_ontology:disappearance_verdict(substance_control_kernel__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__harm_reduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_kernel__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__harm_reduction_reading, 0.47, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__harm_reduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_kernel__harm_reduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_kernel__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored moderate (0.47) and plateauing: the regime's early decades shed the heavy criminal extraction of prohibition-era use enforcement, but the residual structure — compulsory-treatment pathways, drug-court conditionality, supply-side felony exposure, and a service apparatus financed by continued caseloads — hardened rather than dissolved after roughly year 24. Suppression (0.41) is a raw structural property, unscaled by power or scope: it reflects active machinery that still exists (commission summons, court conditionality, supply policing, prosecution of unlicensed provision), not merely attitudinal stigma. Theater ratio (0.31) is rising from a low base: process metrics — kits distributed, encounters logged, screenings completed — increasingly stand in for the outcome metrics (mortality, incidence) the regime exists to move, a textbook Goodhart drift as funding follows countable activity. Accessibility collapse is low (0.35): both sibling arrangements remain live political alternatives and neither the legal-market option nor the punitive option is foreclosed by the regime's existence. Resistance is moderate (0.52): sustained opposition from abstinence-oriented advocates, site-siting opponents, and law-and-order constituencies, counterweighted by broad expert consensus. The temporal series run on ONE shared grid (t=0,6,12,18,24,30,36, spanning approximately 1989–2025) with all three metrics authored at every point, so no scalar substitution injects end-state values into earlier periods. Claim and metrics are authored independently; the engine computes per-seat classifications from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   The seats diverge sharply and the engine should compute it. From the provider and authority seats the arrangement is coordination they built, staff, and believe in — near-pure rope. From the supply-chain worker's seat it is indistinguishable from the prohibition it replaced: the same police, the same courts, the same cells, now concentrated exclusively on people like them — effectively snare-grade. From the user's seat it is genuinely mixed: naloxone and methadone arrive alongside summonses and urine screens, placing them between beneficiary and target rather than at either pole. Host-neighborhood residents experience a locally concentrated cost with geographically diffuse compensation, which no aggregate metric averages away. Identity-lock operates differently across seats: provider organizations have fused organizational identity with the served population (winding down reads as betrayal, not downsizing), and users risk internalizing the client role such that exit feels like self-abandonment — the second mechanism is flagged as an omega because it is internalized rather than structural. Inter-institutionally, police and health authorities occupy the same nominal power level yet pull in opposite directions on every expansion decision, differentiated entirely by constraint-specific factors: forfeiture flows and arrest-volume metrics versus mortality dashboards and licensure authority.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation. people_who_use_drugs appears in BOTH arrays deliberately: services subsidize them while compulsory-referral and court-conditionality extract from them, and their constrained exit (dependency bars relocation or cessation for most) keeps them from arbitraging the mix — the derived d should land near symmetric, not at either pole. addiction_treatment_service_providers and public_health_authorities sit at the beneficiary end: budgets and remit scale with the regime. illicit_supply_chain_workers sit at the full-target end: powerless, trapped, and the sole remaining criminal victim class, they absorb the enforcement intensity the use-side decriminalization released. host_neighborhood_residents carry a real but localized cost position. law_enforcement_agencies are the deliberate complication: nominally they lost (use-side mandate) and gained (supply file, naloxone-response funding, forfeiture) simultaneously, which is why they carry paired roles — the derivation from the arrays alone would flatten this, and the commentary flags the flattening rather than papering over it with overrides. Taxpayers sit mildly on the cost side with no exit. The evaluator seat is analytical throughout.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — injection-driven epidemics and mass overdose mortality — is verifiably LIVE (coroner series, WHO surveillance), so this is not a mandatrophy case today and no mandatrophy_resolved declaration is made. The risk runs the other direction: the measurements show theater_ratio climbing monotonically while extractiveness plateaus, the signature of a coordination function gradually acquiring a self-perpetuating service apparatus around it. If epidemic mortality fell decisively while funding followed caseload rather than need, the regime would drift toward maintaining the managed condition rather than reducing harm — the mismatch consumer watching founding_problem_status x disappearance_verdict would catch the flip when 'live' stops being corroborated from outside the beneficiary set. Classifying this as tangled_rope rather than snare preserves the analytic separation the corpus needs: the epidemic-coordination function is real and measurable, and calling the whole structure pure extraction would erase the mortality reductions that are its strongest defense; calling it pure coordination would erase the supply-chain prisoner, the compelled patient, and the budget-cycle incentive that all ride the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_decomposition,
    'This story instantiates only the harm-reduction reading of the substance-control kernel; how would the classification shift if the standing arrangement under contest were the prohibition or legalization reading instead?',
    'Author the sibling stories (prohibition_reading, legalization_reading) each with its own ε referent, victim set, and primary observables, then compare per-seat classifications across the family.',
    'Under the prohibition reading, users join illicit_supply_chain_workers in the criminal victim set and ε rises sharply; under the legalization reading, the paternalistic intervention layer drops out, the user seat moves toward autonomy, and ε falls toward externality-pricing levels. Cross-family comparison, not intra-story hedging, resolves the contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Committer-frame routing: one kernel, three readings, three separate ε-invariant constraints.').

omega_variable(
    paternalism_extraction_boundary,
    'Where does the regime''s health intervention stop protecting and start extracting from users — voluntary service uptake versus commission summons, compulsory referral on refusal, drug-court conditionality, and monitored case management?',
    'Compare user-reported autonomy costs and outcomes across jurisdictions matched on service level but varying in coercion intensity; natural experiments where compulsory pathways were suspended or struck down.',
    'If the coercive layer is separable from the service layer, ε for this reading falls and the regime trends toward rope; if current implementations fuse them, the user seat sits nearer the full-target end than the beneficiary/victim duality alone suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paternalism_extraction_boundary, empirical, 'Locating the care/coercion boundary inside the user''s dual position.').

omega_variable(
    supply_side_criminalization_ratchet,
    'Is retained supply-side criminalization a functional guard the reading requires, or a ratchet that preserves enforcement budgets and forfeiture flows independent of any protective effect?',
    'Track supply-arrest rates, interdiction expenditure, and forfeiture receipts in the longest-operating harm-reduction jurisdictions; test whether enforcement intensity tracks measured supply-side harm or institutional budget cycles.',
    'If ratchet-driven, the supply-side residue is extraction riding the health frame and the regime accumulates snare features at the supply-worker seat over time; if functional, it prices as coordination overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_side_criminalization_ratchet, empirical, 'Whether the criminalized remainder of the supply chain is function or rent.').

omega_variable(
    provider_budget_capture,
    'Does the service-provider sector''s dependence on regime continuance distort program design toward indefinitely managing the condition rather than resolving it?',
    'Audit provider funding structures against outcome incentives; compare jurisdictions using salaried state-employed clinicians against contract-funded nonprofit networks on outcome-versus-caseload sensitivity.',
    'If capture is real, the concentration of gains at the provider seat deepens and the regime''s persistence becomes partly self-interested rather than evidence-led, accelerating the theater-ratio drift already visible in the measurements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(provider_budget_capture, empirical, 'Whether the receipt seat''s budget cycle bends the regime''s goals.').

omega_variable(
    client_identity_internalization,
    'Do users come to experience the client/patient role as identity, such that remaining inside the arrangement is partly internalized attachment rather than chosen benefit — making observed stay exceed what the services alone explain?',
    'Longitudinal qualitative follow-up of users who relocate to jurisdictions without the regime: track self-description, service re-engagement, and reported reasons.',
    'If internalization is substantial, effective suppression for the user seat exceeds the structural measure — users carry the regime''s grip with them after exit — and the identity_locked characterization would fit more users than the constrained atom currently conveys.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(client_identity_internalization, conceptual, 'Structural versus internalized binding of the user seat to the client role.').

omega_variable(
    cs_framing_underdetermination,
    'The declared framing grounds the regime''s authority in medical/public-health expertise; an alternative framing locates the operative authority in the state''s inherited police-power and narcotics-law lineage, with clinical bodies as an interpretive layer — do the two framings produce different commitment-system classifications?',
    'Re-run the commitment-system classification with authority_grounding variants tracing to the narcotics-law tradition and compare pattern outputs against the declared expertise framing.',
    'If the police-power frame fits better, the regime inherits prohibition''s authority lineage rather than expertise legitimacy, and the drift vector reads as lineage erosion rather than practice lag — changing which remedial pressures the engine predicts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Framing under-determination: expertise stewardship versus police-power inheritance beneath it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__harm_reduction_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__harm_reduction_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(subs_tr_t0, observed).
narrative_ontology:measurement(subs_tr_t6, substance_control_kernel__harm_reduction_reading, theater_ratio, 6, 0.14).
narrative_ontology:measurement_basis(subs_tr_t6, observed).
narrative_ontology:measurement(subs_tr_t12, substance_control_kernel__harm_reduction_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement_basis(subs_tr_t12, observed).
narrative_ontology:measurement(subs_tr_t18, substance_control_kernel__harm_reduction_reading, theater_ratio, 18, 0.22).
narrative_ontology:measurement_basis(subs_tr_t18, observed).
narrative_ontology:measurement(subs_tr_t24, substance_control_kernel__harm_reduction_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement_basis(subs_tr_t24, observed).
narrative_ontology:measurement(subs_tr_t30, substance_control_kernel__harm_reduction_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement_basis(subs_tr_t30, observed).
narrative_ontology:measurement(subs_tr_t36, substance_control_kernel__harm_reduction_reading, theater_ratio, 36, 0.31).
narrative_ontology:measurement_basis(subs_tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__harm_reduction_reading, base_extractiveness, 0, 0.61).
narrative_ontology:measurement_basis(subs_be_t0, observed).
narrative_ontology:measurement(subs_be_t6, substance_control_kernel__harm_reduction_reading, base_extractiveness, 6, 0.57).
narrative_ontology:measurement_basis(subs_be_t6, observed).
narrative_ontology:measurement(subs_be_t12, substance_control_kernel__harm_reduction_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement_basis(subs_be_t12, observed).
narrative_ontology:measurement(subs_be_t18, substance_control_kernel__harm_reduction_reading, base_extractiveness, 18, 0.5).
narrative_ontology:measurement_basis(subs_be_t18, observed).
narrative_ontology:measurement(subs_be_t24, substance_control_kernel__harm_reduction_reading, base_extractiveness, 24, 0.48).
narrative_ontology:measurement_basis(subs_be_t24, observed).
narrative_ontology:measurement(subs_be_t30, substance_control_kernel__harm_reduction_reading, base_extractiveness, 30, 0.47).
narrative_ontology:measurement_basis(subs_be_t30, observed).
narrative_ontology:measurement(subs_be_t36, substance_control_kernel__harm_reduction_reading, base_extractiveness, 36, 0.47).
narrative_ontology:measurement_basis(subs_be_t36, observed).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__harm_reduction_reading, suppression_requirement, 0, 0.74).
narrative_ontology:measurement_basis(subs_su_t0, observed).
narrative_ontology:measurement(subs_su_t6, substance_control_kernel__harm_reduction_reading, suppression_requirement, 6, 0.68).
narrative_ontology:measurement_basis(subs_su_t6, observed).
narrative_ontology:measurement(subs_su_t12, substance_control_kernel__harm_reduction_reading, suppression_requirement, 12, 0.61).
narrative_ontology:measurement_basis(subs_su_t12, observed).
narrative_ontology:measurement(subs_su_t18, substance_control_kernel__harm_reduction_reading, suppression_requirement, 18, 0.55).
narrative_ontology:measurement_basis(subs_su_t18, observed).
narrative_ontology:measurement(subs_su_t24, substance_control_kernel__harm_reduction_reading, suppression_requirement, 24, 0.49).
narrative_ontology:measurement_basis(subs_su_t24, observed).
narrative_ontology:measurement(subs_su_t30, substance_control_kernel__harm_reduction_reading, suppression_requirement, 30, 0.44).
narrative_ontology:measurement_basis(subs_su_t30, observed).
narrative_ontology:measurement(subs_su_t36, substance_control_kernel__harm_reduction_reading, suppression_requirement, 36, 0.41).
narrative_ontology:measurement_basis(subs_su_t36, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__harm_reduction_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, legalization_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'substance control'. The label conflates three structurally distinct claims with different ε, different victim sets, and different observables: prohibition_reading (punishment regime; users in the criminal victim set; order as observable), this file — harm_reduction_reading (service-provider regime; users dual-positioned; mortality/transmission as observables), and legalization_reading (externality-priced liberty; users as autonomous consumers; third-party harm as observable). Family linkage runs prohibition_reading -> harm_reduction_reading -> legalization_reading historically: the prohibition arrangement is what harm reduction was built inside and against, and harm reduction's institutionalized health-framing and service infrastructure now reshape the environment in which legalization arguments operate. Each file links the others through network.affects_constraints; no file hedges ε across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
