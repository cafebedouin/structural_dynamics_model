% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: substance_control_kernel__harm_reduction_reading
 *   human_readable: Harm-Reduction Governance of Substance Use (Public-Health Reading)
 *   domain: public health policy / criminal justice / political economy
 *
 * SUMMARY:
 *   Since the late 1980s, a growing set of jurisdictions has governed
 *   psychoactive substance use through a public-health frame: use is
 *   classified as a health condition, services aim to reduce death and
 *   disease without requiring cessation as a condition of care, and the state
 *   presents itself as service provider rather than punisher. The arrangement
 *   as it actually operates is a hybrid: sterile supplies, naloxone
 *   distribution, opioid agonist treatment, and supervised consumption sit
 *   alongside conditioned access (urine screening, appointment compliance,
 *   housing and benefit contingencies), a still-criminalized supply chain,
 *   and retained supply-side policing. This story authors ONE reading of the
 *   substance_control_kernel — the harm_reduction_reading — as a clean,
 *   epsilon-invariant constraint. The epsilon referent is the standing
 *   harm-reduction arrangement itself, assessed by this reading's own lights:
 *   the retained supply criminalization and the paternalistic conditioning
 *   count as costs borne by users and low-level suppliers, not as features of
 *   some other regime. The sibling readings (prohibition_reading,
 *   legalization_reading) are separate constraints with different victim sets
 *   and observables; the family is linked via network.affects_constraints and
 *   the decomposition is documented in network.dual_formulation_note.
 *
 * KEY AGENTS:
 *   - active_substance_users: Primary target (powerless/trapped) — receives life-saving services but bears paternalistic conditioning, service-linked records, and the overdose risk of a criminalized supply
 *   - low_level_suppliers: Secondary target (powerless/trapped) — bears the retained supply-side criminalization while organizational layers above are rarely reached
 *   - public_health_authorities: Agenda setter (institutional/constrained) — designs the framework, licenses services, sets compliance conditions, receives the mandate and appropriations
 *   - treatment_service_providers: Primary beneficiary (organized/mobile) — delivers contracted services and receives the program funding stream
 *   - organized_supply_networks: Structural beneficiary (powerful/arbitrage) — earns margins protected by the bar on legal competition
 *   - supply_side_enforcement_agencies: Secondary beneficiary (institutional/constrained) — retain mission, budgets, and seizure authority after user-facing enforcement receded
 *   - general_taxpayers: Diffuse payer (moderate/constrained) — fund both the health apparatus and the residual enforcement apparatus
 *   - drug_user_activists: Excluded voice (organized/constrained) — demand seating in framework design and an end to conditioned access; consulted episodically, rarely empowered
 *   - public_health_researchers: Analytical observer (analytical/analytical) — measure mortality, transmission, and program effectiveness; no stake in program survival
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__harm_reduction_reading, 0.58).
domain_priors:suppression_score(substance_control_kernel__harm_reduction_reading, 0.48).
domain_priors:theater_ratio(substance_control_kernel__harm_reduction_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__harm_reduction_reading, "Harm-Reduction Governance of Substance Use (Public-Health Reading)").
narrative_ontology:topic_domain(substance_control_kernel__harm_reduction_reading, "public health policy / criminal justice / political economy").

domain_priors:requires_active_enforcement(substance_control_kernel__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__harm_reduction_reading, '1d6b0fb7-5431-48fb-aaf8-e8439f9960d0').
narrative_ontology:cs_kernel_codification('1d6b0fb7-5431-48fb-aaf8-e8439f9960d0', distributed).
narrative_ontology:cs_authority_grounding('1d6b0fb7-5431-48fb-aaf8-e8439f9960d0', distributed).
narrative_ontology:cs_reading_relation('1d6b0fb7-5431-48fb-aaf8-e8439f9960d0', substance_control_kernel__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('1d6b0fb7-5431-48fb-aaf8-e8439f9960d0', substance_control_kernel__legalization_reading, forecloses).
narrative_ontology:cs_axiom('1d6b0fb7-5431-48fb-aaf8-e8439f9960d0', foundational, persons_who_use_are_patients_not_offenders).
narrative_ontology:cs_axiom_status(persons_who_use_are_patients_not_offenders, holdable).
narrative_ontology:cs_axiom_grounding('1d6b0fb7-5431-48fb-aaf8-e8439f9960d0', persons_who_use_are_patients_not_offenders, empirically_contingent).
narrative_ontology:cs_axiom('1d6b0fb7-5431-48fb-aaf8-e8439f9960d0', foundational, intervention_warrant_independent_of_abstinence_goal).
narrative_ontology:cs_axiom_status(intervention_warrant_independent_of_abstinence_goal, holdable).
narrative_ontology:cs_axiom_grounding('1d6b0fb7-5431-48fb-aaf8-e8439f9960d0', intervention_warrant_independent_of_abstinence_goal, instrumental).
narrative_ontology:cs_reference_frame('1d6b0fb7-5431-48fb-aaf8-e8439f9960d0', medicalized_public_health_stewardship).
narrative_ontology:cs_drift_state('1d6b0fb7-5431-48fb-aaf8-e8439f9960d0', contemporary_fentanyl_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1d6b0fb7-5431-48fb-aaf8-e8439f9960d0', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__harm_reduction_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, treatment_service_providers).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, organized_supply_networks).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, supply_side_enforcement_agencies).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, active_substance_users).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, low_level_suppliers).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, general_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, active_substance_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer the framework: license supervised consumption sites, set eligibility and compliance rules for treatment and housing referrals, commission surveillance, and report on mortality and transmission. Receive appropriations and the epidemiological mandate. Stepping back from the framework would mean ceding the population to criminal justice agencies, which their professional commitments and budgets oppose; stepping forward past their statutory authority requires legislative coalitions they do not control.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__harm_reduction_reading, public_health_authorities, beneficiary).

% Use opioids or stimulants, often daily. Receive naloxone, sterile supplies, and medication-assisted treatment through the framework's services. In exchange they submit to intake assessments, urine screening, and appointment schedules; missing them can suspend dosing, housing referrals, or benefit recommendations. Service records follow them across agencies. Stopping use means withdrawal and lost tolerance, which raises overdose risk; continuing means the conditioning regime. Prior arrests, even where possession is no longer charged, still bar employment and housing.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, active_substance_users, payer,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__harm_reduction_reading, active_substance_users, beneficiary).

% Sell small quantities, frequently to finance their own use. They remain the working object of the retained supply-side criminalization: street-level arrest, booking, short incarceration, and asset seizure land on them while the organizational layers above are rarely reached. Exiting means finding income with a record, repaying debts to upstream networks, and losing the social ties that structure their days.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, low_level_suppliers, payer,
    powerless, immediate, trapped, local).

% Nonprofit and for-profit clinics deliver counseling, agonist treatment, and housing referrals under contract. Revenue scales with enrolled clients and documented contacts, so program survival tracks client flow and funder reporting cycles. They can pivot staff and facilities toward adjacent funding streams — homelessness, mental health — if substance-use contracts dry up, which gives them more mobility than the population they serve.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, treatment_service_providers, beneficiary,
    organized, biographical, mobile, national).

% Transnational trafficking organizations earn margins that the ban on legal competition protects: enforcement raises retail prices faster than it seizes product, and the retail tier absorbs nearly all arrest risk. They reformulate products faster than scheduling regimes update, shifting between analogues and jurisdictions. Nothing in the health-framed settlement touches their revenue; the retained supply criminalization is the load-bearing wall of their market position.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, organized_supply_networks, beneficiary,
    powerful, generational, arbitrage, global).

% Police and customs units retain interdiction and street-level supply enforcement: budgets, headcount, seizure-derived forfeiture, and a mission justification. User-facing enforcement receded under the health frame, so the supply focus is what preserves the agency's remit. Further contraction would mean absorbing cuts and redeploying staff; they bargain to keep supply offenses on the books and resourced.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, supply_side_enforcement_agencies, beneficiary,
    institutional, generational, constrained, national).

% Fund the arrangement through appropriations — both the service apparatus and the residual enforcement apparatus. The per-household stake is small and diffuse, attention is episodic, and exit runs only through electoral voice filtered by representatives who face organized constituencies on every side of the issue.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, general_taxpayers, payer,
    moderate, biographical, constrained, national).

% User unions and formerly incarcerated advocates organize around the principle that nothing about them should be decided without them. They document how conditioned access punishes the sickest clients and push for unconditional services and an end to supply criminalization. Framework consultations invite them episodically; agenda authority stays with agencies and providers. Their objections are minuted more often than acted on.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, drug_user_activists, excluded,
    organized, biographical, constrained, national).

% Epidemiologists and policy scholars measure overdose mortality, HIV and hepatitis C incidence, and program effectiveness across jurisdictions. Both defenders and opponents of the framework cite their output. Their standing depends on methodological reputation rather than program survival, which lets them report results that embarrass any seat.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, public_health_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__harm_reduction_reading, treatment_service_providers).
narrative_ontology:fixing_cost_class(substance_control_kernel__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools scarce clinical and outreach capacity to reach a population that avoids mainstream institutions, creating a single trusted front door; standardizes the response across police, health, and housing agencies so encounters converge on services instead of cells; internalizes communicable-disease and overdose externalities that spill across community boundaries.
% TRANSFER_FUNCTION: Moves public revenue from general taxpayers to health agencies and contracted service providers; moves decision authority over use, treatment compliance, and program access from users to clinical-administrative gatekeepers; and, through the retained supply criminalization, preserves the flow of illicit-market revenue from users up the supply chain to trafficking organizations.
% ABSENT_VOICES: Drug user activists hold an organized objection — no decisions about us without us, an end to conditioned access — but hold no agenda authority. Low-level suppliers have no legitimate voice at all, since their conduct is defined as criminal by the same arrangement that claims to have replaced punishment with care. Libertarian critics of paternalism are heard episodically in hearings but are not seated in program design. The consensus that the framework 'works' is formed largely among the parties that deliver and fund it.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, service contact with hundreds of thousands of people would sever immediately: sterile supplies and naloxone distribution would stop, agonist treatment patients would face abrupt discontinuation, and overdose and infection rates would climb within months. Providers would collapse or convert, enforcement agencies would reabsorb the population under the still-existing supply offenses, and supply networks would gain a larger price umbrella. Housing, emergency medicine, and criminal justice caseloads would all reorganize around the gap.
% FOUNDING_PROBLEM: In the late 1980s, injection-driven HIV transmission was exploding through a population that avoided hospitals and feared police contact, while punishment-centered policy had produced decades of incarceration with no measurable reduction in use. The framework was built to establish pragmatic, non-judgmental contact with people who use drugs and to reduce death and disease transmission directly, without requiring cessation as the price of care.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: national vital-statistics and coroner mortality data, CDC and WHO surveillance of HIV and hepatitis C incidence, and peer-reviewed epidemiology independent of service contracts all attest that the founding problem — mass overdose death and blood-borne transmission — persists at record or near-record levels. Drug user unions, an excluded seat with no funding stake, independently attest the problem is live while disputing the arrangement's conditioned design. No corroborating source attests the problem is solved.
narrative_ontology:disappearance_verdict(substance_control_kernel__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__harm_reduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_kernel__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__harm_reduction_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.58 (moderate-substantial): the arrangement delivers real transfers to users, but it also conditions care on compliance, generates service-linked records that follow people, and preserves a supply chain whose criminalization converts directly into overdose risk and retail-tier incarceration. Suppression is authored at 0.48 as a RAW structural property — criminalized supply, conditioned benefits, drug-court mandates — and is deliberately not scaled by power or scope; only extractiveness is scaled, by the engine, through directionality and scope. Theater ratio 0.30: the core functions (naloxone, sterile supplies, agonist treatment) are real, but a growing share of activity is enrollment-metric performance — waitlists recorded as treatment capacity, outcome-funded contracts rewarding documented contact over documented improvement. Accessibility_collapse 0.40: alternatives remain live once the frame is understood — legalization politics, the abstinence sector, and the unregulated market all persist — so understanding the arrangement does not close the option space. Resistance 0.55: moral opposition ('enabling'), libertarian objection to paternalism, user activism against conditioning, and neighborhood opposition to supervised sites are sustained and organized. Claim/metric independence: claimed_type tangled_rope is asserted from structure alone — a genuine coordination function (pooled clinical capacity reaching a population that avoids institutions) PLUS named payers PLUS active enforcement of both the compliance conditions and the supply ban; the metrics are authored independently as descriptive judgments, and any divergence between the claim and per-seat computed types is the datum the corpus exists to take. Temporal series run on ONE shared grid (1988, 1996, 2004, 2012, 2019, 2024) with every tracked metric authored at every point; the suppression_requirement series is authored because the story specifically tracks enforcement-capacity change (decay of user-facing enforcement, retention of supply-side enforcement), not merely extraction drift.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter/beneficiary seats should compute very differently. From the service-delivery seats, the arrangement is care: lives saved, infections averted, a door open that prison was not. From the user seat, the same structure is conditional custody — help that can be withdrawn for missed appointments, records that outlast the episode of care, and a supply channel whose illegality is the reason a bag's contents are unknown. From the low-level supplier seat, the arrangement's much-advertised compassion stops exactly where their conduct begins. Two same-power seats diverge sharply: users and low-level suppliers both hold powerless/trapped positions, but users face graduated administrative pressure while suppliers face custodial pressure — different mechanisms, different exit impossibilities. Coalition potential exists (users, activists, and parts of the provider sector could press for unconditional access and supply reform), but activation costs are high: users' trapped position suppresses collective action, and providers' funding depends on the conditioned-access model they would be reforming. The researcher seat sees the full structure and no stake in any branch of it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit at the low-d end: public_health_authorities (agenda-setting plus mandate), treatment_service_providers (funding receipt), supply_side_enforcement_agencies (retained mission), and organized_supply_networks (arbitrage-grade adaptation to enforcement-created margins) all derive low d and correspondingly low or negative effective extraction. Targets sit high: active_substance_users and low_level_suppliers bear the arrangement's costs with trapped exit. General_taxpayers bear diffuse costs at moderate d. One override is declared: for the powerless atom, d is set to 0.72 because the automatic derivation would read the users' dual declaration (victims with a secondary beneficiary role) and pull them toward the midpoint, whereas their true structural position is near the target end — trapped exit, paternalistic exposure, and the unsafe-supply consequences of the retained criminalization dominate the incidental service benefit. The override is accurate for low_level_suppliers on the same atom, who are likewise near-target. National-scale scope modestly amplifies effective extraction for the conditioned-compliance mechanisms, since verification of 'voluntary' engagement across a continental population is weak.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — mass overdose death and blood-borne disease transmission among a criminalized, institution-avoiding population — is still live; fatal overdoses reached record levels during the fentanyl era and infectious-disease clusters persist. The arrangement has therefore not outlived its function, no sunset clause exists or should exist yet, and mandatrophy_resolved is deliberately NOT declared. The R5 mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges: no zombie flag fires. The warning sign worth tracking is the theater_ratio slope (0.12 to 0.30): outcome-funded contracting rewards documented enrollment over documented improvement, which is classic Goodhart substrate. If the mortality crisis subsides while enrollment metrics persist, theater_ratio crossing 0.5 would mark the beginning of a piton trajectory — a health apparatus performing care. The classification prevents mislabeling in both directions: reading the arrangement as pure coordination ignores the named payers and the actively enforced supply ban; reading it as pure extraction erases the demonstrated mortality and transmission reductions that give the coordination function its content.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates one reading (harm_reduction_reading) of the substance_control_kernel; how would the sibling readings (prohibition_reading, legalization_reading) restructure the same subject matter?',
    'Generate the sibling stories as separate constraints and compare victim sets, observables, and epsilon over the fixed referent of the standing arrangement.',
    'Under prohibition_reading, users enter the punished class and enforcement becomes the primary observable; under legalization_reading, paternalistic conditioning disappears and services are replaced by externality pricing, dropping extraction toward the coordination floor. The disagreement is located in the normative classification of use itself and the state''s warrant over self-regarding conduct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three mutually exclusive readings of the substance-control kernel.').

omega_variable(
    paternalism_care_boundary,
    'Is the paternalistic component of this arrangement intrinsic to governing use as a health condition, or an artifact of conditioned-access program design (urine screening, appointment compliance, housing and benefit contingencies)?',
    'Compare jurisdictions offering substantially unconditional services (unconditional housing-first, prescribed-supply trials) with conditioned-access regimes on autonomy measures and health outcomes.',
    'If artifact, extraction falls substantially within this reading without altering the reading itself; if intrinsic, the health-framing carries irreducible extraction and the computed classification shifts toward the heavier-extraction end for every seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paternalism_care_boundary, conceptual, 'Whether measured paternalism is separable from the health-condition framing.').

omega_variable(
    residual_supply_criminalization_attribution,
    'Does the retained supply-chain criminalization belong to THIS reading''s arrangement, or is it prohibition_reading machinery persisting underneath a health-framed surface?',
    'Decompose extraction contributions in jurisdictions that removed supply-adjacent penalties while keeping health services; attribute the unsafe-supply and incarceration costs to whichever arrangement maintains them.',
    'If attributed to prohibition residue, this reading''s epsilon drops materially and the extraction travels through the family network edge instead; if intrinsic to the hybrid settlement this reading actually operates, it remains counted here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_supply_criminalization_attribution, conceptual, 'Attribution of retained supply criminalization between this reading and its prohibition sibling.').

omega_variable(
    disease_model_empirical_status,
    'This reading''s foundational axiom rests on dependence as a chronic brain condition; do choice-model accounts and documented remission-without-treatment trajectories erode the patient classification?',
    'Longitudinal natural-history studies of untreated remission rates and replication of proposed neurobiological dependence markers.',
    'Erosion of the disease model weakens the patient-not-offender classification that distinguishes this reading from prohibition_reading, and the engine may compute axiom_overriding drift against this reading''s own foundation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disease_model_empirical_status, empirical, 'Empirical standing of the disease model underwriting the patient classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__harm_reduction_reading, 1988, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sck_harm_red_tr_t1988, substance_control_kernel__harm_reduction_reading, theater_ratio, 1988, 0.12).
narrative_ontology:measurement(sck_harm_red_tr_t1996, substance_control_kernel__harm_reduction_reading, theater_ratio, 1996, 0.16).
narrative_ontology:measurement(sck_harm_red_tr_t2004, substance_control_kernel__harm_reduction_reading, theater_ratio, 2004, 0.21).
narrative_ontology:measurement(sck_harm_red_tr_t2012, substance_control_kernel__harm_reduction_reading, theater_ratio, 2012, 0.25).
narrative_ontology:measurement(sck_harm_red_tr_t2019, substance_control_kernel__harm_reduction_reading, theater_ratio, 2019, 0.28).
narrative_ontology:measurement(sck_harm_red_tr_t2024, substance_control_kernel__harm_reduction_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(sck_harm_red_be_t1988, substance_control_kernel__harm_reduction_reading, base_extractiveness, 1988, 0.34).
narrative_ontology:measurement(sck_harm_red_be_t1996, substance_control_kernel__harm_reduction_reading, base_extractiveness, 1996, 0.41).
narrative_ontology:measurement(sck_harm_red_be_t2004, substance_control_kernel__harm_reduction_reading, base_extractiveness, 2004, 0.47).
narrative_ontology:measurement(sck_harm_red_be_t2012, substance_control_kernel__harm_reduction_reading, base_extractiveness, 2012, 0.52).
narrative_ontology:measurement(sck_harm_red_be_t2019, substance_control_kernel__harm_reduction_reading, base_extractiveness, 2019, 0.56).
narrative_ontology:measurement(sck_harm_red_be_t2024, substance_control_kernel__harm_reduction_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sck_harm_red_su_t1988, substance_control_kernel__harm_reduction_reading, suppression_requirement, 1988, 0.66).
narrative_ontology:measurement(sck_harm_red_su_t1996, substance_control_kernel__harm_reduction_reading, suppression_requirement, 1996, 0.62).
narrative_ontology:measurement(sck_harm_red_su_t2004, substance_control_kernel__harm_reduction_reading, suppression_requirement, 2004, 0.57).
narrative_ontology:measurement(sck_harm_red_su_t2012, substance_control_kernel__harm_reduction_reading, suppression_requirement, 2012, 0.53).
narrative_ontology:measurement(sck_harm_red_su_t2019, substance_control_kernel__harm_reduction_reading, suppression_requirement, 2019, 0.5).
narrative_ontology:measurement(sck_harm_red_su_t2024, substance_control_kernel__harm_reduction_reading, suppression_requirement, 2024, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__harm_reduction_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__legalization_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'drug policy' conflates three structurally distinct claims about the same subject matter, decomposed per the epsilon-invariance principle. prohibition_reading (users as punishable offenders; enforcement observables; high extraction concentrated on users), harm_reduction_reading (this file: users as patients under paternalistic stewardship; overdose and transmission observables; moderate extraction with retained supply criminalization), and legalization_reading (users as rights-holders; externality-pricing observables; extraction near the coordination floor). The readings share a referent domain but assign different victim sets, different state roles, and different epsilons; each is authored as its own story and linked here. Upstream/downstream: prohibition machinery historically precedes and constrains this reading (treaty obligations, inherited enforcement infrastructure), which is why the residual-criminalization attribution question is carried as an omega rather than silently absorbed into this story's epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_kernel__harm_reduction_reading, powerless, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
