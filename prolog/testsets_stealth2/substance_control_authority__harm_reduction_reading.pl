% ============================================================================
% CONSTRAINT STORY: substance_control_authority__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: substance_control_authority__harm_reduction_reading
 *   human_readable: Substance Control Authority - Harm Reduction Reading
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   A jurisdiction withdraws criminal sanction from personal possession of
 *   psychoactive drugs and reorganizes its response around public health
 *   services: sterile syringe distribution, supervised consumption rooms,
 *   widespread naloxone provision, low-threshold medication-assisted
 *   treatment, and outbreak surveillance. The supply side remains criminal:
 *   trafficking and wholesale import stay felonies, so users buy from an
 *   unregulated market whose product no authority tests. The arrangement
 *   therefore splits what the older punitive regime treated as a single
 *   target population: users exit the criminal-justice victim set while
 *   remaining in a health-harm victim set they partly owe to the criminal
 *   market the reading chose to keep; third parties such as site
 *   neighborhoods and victims of acquisition crime absorb residuals the state
 *   now prices as manageable. KEY AGENTS (by structural relationship): users
 *   as dual-positioned principals, legislatures as threshold-writers, health
 *   authorities and police as administering seats, providers and
 *   manufacturers and illicit sellers as collecting seats, taxpayers as
 *   near-symmetric funders, residents and crime victims as residual-cost
 *   bearers, unserved rural users as excluded, analysts as observers. Sibling
 *   readings of the same kernel (prohibition, legalization) are authored as
 *   separate constraint stories; see kernel_context and the network links.
 *
 * KEY AGENTS:
 *   - people_who_use_drugs: dual-positioned principal - primary beneficiary seat and residual target seat (powerless/trapped); exits the criminal victim set while remaining in the health-harm victim set
 *   - legislatures_and_executive_branches: agenda-setter (institutional/mobile) - writes and can rewrite the tolerated-possession threshold and the supply-side felony line
 *   - public_health_authorities: agenda-setter with beneficiary secondary (institutional/constrained) - designs and administers the service portfolio, collects mandate and appropriation
 *   - street_level_police: agenda-setter with beneficiary secondary (institutional/constrained) - enforces the user/supplier boundary daily and retains interdiction budgets
 *   - treatment_and_syringe_providers: beneficiary (organized/constrained) - delivers funded services, depends on annual renewals
 *   - pharmaceutical_manufacturers_of_treatment_agents: beneficiary (powerful/arbitrage) - sells into a deliberately enlarged treatment market
 *   - illicit_market_operators: beneficiary (organized/arbitrage) - stabilized clientele, preserved margin under kept-illegal supply
 *   - taxpayers: near-symmetric contributor (moderate/constrained) - funds the apparatus, recoups part as avoided emergency and carceral cost
 *   - residents_near_service_sites: concentrated-cost bearer (organized/mobile) - lives with the policy's physical footprint
 *   - victims_of_acquisition_property_crime: diffuse-cost bearer (powerless/constrained) - absorbs crime committed to fund purchases
 *   - rural_users_without_service_access: excluded seat (powerless/constrained) - nominally covered by the tolerant regime, practically governed by distance
 *   - epidemiologists_and_policy_analysts: analytical observer - measures the arrangement, holds no vote
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__harm_reduction_reading, 0.47).
domain_priors:suppression_score(substance_control_authority__harm_reduction_reading, 0.41).
domain_priors:theater_ratio(substance_control_authority__harm_reduction_reading, 0.23).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, extractiveness, 0.47).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, theater_ratio, 0.23).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__harm_reduction_reading, "Substance Control Authority - Harm Reduction Reading").
narrative_ontology:topic_domain(substance_control_authority__harm_reduction_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__harm_reduction_reading, 'e69381f7-7ee5-4a09-b750-2c420f79ff0f').
narrative_ontology:cs_kernel_codification('e69381f7-7ee5-4a09-b750-2c420f79ff0f', formalized).
narrative_ontology:cs_authority_grounding('e69381f7-7ee5-4a09-b750-2c420f79ff0f', expertise).
narrative_ontology:cs_interpretation_layer_present('e69381f7-7ee5-4a09-b750-2c420f79ff0f').
narrative_ontology:cs_reading_relation('e69381f7-7ee5-4a09-b750-2c420f79ff0f', substance_control_authority__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('e69381f7-7ee5-4a09-b750-2c420f79ff0f', substance_control_authority__legalization_reading, influences).
narrative_ontology:cs_axiom('e69381f7-7ee5-4a09-b750-2c420f79ff0f', foundational, user_acceptance_for_health_engagement).
narrative_ontology:cs_axiom_status(user_acceptance_for_health_engagement, holdable).
narrative_ontology:cs_axiom_grounding('e69381f7-7ee5-4a09-b750-2c420f79ff0f', user_acceptance_for_health_engagement, instrumental).
narrative_ontology:cs_axiom('e69381f7-7ee5-4a09-b750-2c420f79ff0f', foundational, decriminalize_users_not_markets).
narrative_ontology:cs_axiom_status(decriminalize_users_not_markets, holdable).
narrative_ontology:cs_axiom_grounding('e69381f7-7ee5-4a09-b750-2c420f79ff0f', decriminalize_users_not_markets, instrumental).
narrative_ontology:cs_reference_frame('e69381f7-7ee5-4a09-b750-2c420f79ff0f', public_health_stewardship).
narrative_ontology:cs_drift_state('e69381f7-7ee5-4a09-b750-2c420f79ff0f', synthetic_opioid_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e69381f7-7ee5-4a09-b750-2c420f79ff0f', '').
narrative_ontology:cs_kernel_id(substance_control_authority__harm_reduction_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, treatment_and_syringe_providers).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, pharmaceutical_manufacturers_of_treatment_agents).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, illicit_market_operators).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, residents_near_service_sites).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, victims_of_acquisition_property_crime).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, street_level_police).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, taxpayers).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, residents_near_service_sites).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Buys psychoactive substances from an unregulated market and consumes them without facing arrest or prosecution for personal possession. Can carry naloxone, exchange syringes, and enter treatment without a criminal record as a barrier. Still absorbs the health costs of use itself: dependence, infection risk from inconsistent product content, and overdose exposure that no service fully removes. Because selling remains a felony upstream, price, purity, and availability depend on sellers who answer to no regulator. Exit looks like treatment remission, which is achievable but relapse-prone; the label follows many through housing and employment searches.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, people_who_use_drugs, beneficiary,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, people_who_use_drugs, payer).

% Writes the threshold separating tolerated personal possession from prosecuted trafficking, appropriates the annual service budget, and can widen or narrow the decriminalized zone at any session. Responds to election cycles, media episodes, and coroner reports; retains full authority to restore prosecution of users, as one subnational reversal recently demonstrated.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, legislatures_and_executive_branches, agenda_setter,
    institutional, biographical, mobile, national).

% Designs and administers the intervention portfolio: syringe distribution, supervised consumption rooms, bulk naloxone purchase, treatment contracting, outbreak surveillance. Receives appropriations and reporting duties; publishes the outcome data that justify renewal. Cannot legalize supply, and must request enforcement cooperation from police forces whose priorities differ.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, public_health_authorities, beneficiary).

% Decides daily, at the curb, whether the possession threshold holds: seizure discretion, referral to services versus citation, tolerance of known gathering spots. Retains the trafficking interdiction mission and its budgets. Carries the workload ambiguity of guarding a line the health system treats as porous.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, street_level_police, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, street_level_police, beneficiary).

% Delivers the funded services: exchanges, supervised rooms, counseling, prescribing. Depends on annual grant renewals and performance metrics; expands or contracts with appropriations. Gains organizational permanence from the arrangement continuing and loses mandate if use declines or politics turns.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, treatment_and_syringe_providers, beneficiary,
    organized, biographical, constrained, regional).

% Sells naloxone, buprenorphine, methadone, and related products into a market the arrangement deliberately enlarges. Negotiates prices with public purchasers; patent positions on formulations extend revenue duration. Operates across many jurisdictions, so no single policy reversal threatens the firm.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, pharmaceutical_manufacturers_of_treatment_agents, beneficiary,
    powerful, biographical, arbitrage, global).

% Supplies a customer base the state no longer prosecutes for buying, while the state still seizes shipments and imprisons wholesalers. Demand-side tolerance stabilizes their clientele; supply-side illegality preserves the margin that lawful competition would erode. Moves product through shifting corridors; exit means laundering proceeds into other trades, which the cash flow makes straightforward.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, illicit_market_operators, beneficiary,
    organized, immediate, arbitrage, global).

% Funds the service apparatus through general revenue and receives part of the return as avoided emergency-room, court, and incarceration costs. Has little visibility into unit costs per service contact; encounters the arrangement mainly through budget headlines and street-level anecdotes.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, taxpayers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, taxpayers, payer).

% Lives with the concentrated footprint of the policy: queues outside consumption rooms, discarded paraphernalia when disposal fails, occasional incidents - alongside reductions in public injecting and sharps litter once services run well. Can attend zoning hearings, petition councils, and force relocation of sites; can move away at the cost of a housing search.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, residents_near_service_sites, payer,
    organized, immediate, mobile, local).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, residents_near_service_sites, beneficiary).

% Absorbs burglaries, thefts, and shoplifting committed to fund purchases wherever dependence outruns income and treatment slots run short. Individual victims rarely organize; recourse is insurance claims, avoidance behavior, or relocation. Their aggregate experience is cited by both sides of the policy argument, but they hold no seat in program governance.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, victims_of_acquisition_property_crime, payer,
    powerless, immediate, constrained, national).

% Lives in counties where no syringe program, supervised site, or accessible prescriber operates; nominally covered by the tolerant regime but practically governed by distance, with the nearest service hours away. Would press for mobile distribution and telehealth prescribing if seated in program planning.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, rural_users_without_service_access, excluded,
    powerless, immediate, constrained, regional).

% Measures the arrangement: incidence curves, overdose counts, service reach, cost offsets. Publishes findings that both camps cite selectively. Holds no vote; credibility depends on methodological distance from advocacy.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, epidemiologists_and_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__harm_reduction_reading, diffuse).
narrative_ontology:fixing_cost_class(substance_control_authority__harm_reduction_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates public health capacity on a population that will use drugs regardless of sanction: sterile supplies interrupt bloodborne-disease transmission chains, naloxone distribution and supervised consumption reduce overdose deaths, and low-threshold service contact funnels users into treatment - collective-action problems that punitive processing failed to solve and that no single user or neighborhood can solve privately.
% TRANSFER_FUNCTION: Moves general revenue into service delivery for people who use drugs, and moves users from court dockets into clinic intake. Leaves the drug market's own financial flows untouched: purchases keep flowing from users to illicit sellers at prohibited-supply premiums. Leaves residual costs - acquisition crime, localized disorder, adulterant injury - with users, site neighborhoods, and crime victims.
% ABSENT_VOICES: Victims of acquisition crime and site-adjacent residents rarely hold seats in program governance, which is staffed by health agencies and user-advocacy organizations; recovery-community members who reject the acceptance premise are likewise outside the room; rural users in unserved counties are governed by the arrangement's name without its benefits.
% DISAPPEARANCE_RATIONALE: Overnight repeal would push users back before magistrates, strand service clients without syringes or naloxone, reopen bloodborne-disease transmission chains, and discard the surveillance data stream - while the illicit supply side, already illegal, would simply regain its arrested-customer flow. The world rearranges around the loss of the service layer, not around the market.
% FOUNDING_PROBLEM: Injection-driven HIV and hepatitis C epidemics among people who use drugs, mounting overdose deaths, and the demonstrated failure of mass criminalization to reduce either use or harm - the problem constellation that produced the first decriminalization-plus-services regimes of the 1990s and 2000s.
% FOUNDING_PROBLEM_CORROBORATION: WHO, UNAIDS, and national health agencies attest from outside the benefiting parties that injection-driven HIV and hepatitis C transmission remains an open problem; official vital-statistics systems record overdose mortality at or above historical peaks in the synthetic-opioid era; peer-reviewed evaluations in the general medical journals corroborate both the persistence of the founding problems and the partial effectiveness of the service response. No attestation relies solely on the service-delivery sector's own reporting.
narrative_ontology:disappearance_verdict(substance_control_authority__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__harm_reduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_authority__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__harm_reduction_reading, 0.47, 'stealth/ox-alpha', 'none', direct).

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
 *   Claimed type tangled_rope from structure, authored independently of the metric values: the arrangement carries a genuine coordination function (a service network solving outbreak and overdose collective-action problems that private actors and criminal courts demonstrably could not) AND asymmetric residual burden (users remain exposed to adulterated supply because the market stays criminal; site neighborhoods and acquisition-crime victims bear concentrated residuals; provider, manufacturer, and illicit-seller seats collect durable revenue). Metrics describe operation: extractiveness 0.47 - moderate, far below a carceral baseline because user-facing punishment is withdrawn, but non-trivial because the reading leaves the market criminal and thus leaves users buying unregulated product with their own bodies as the quality-control instrument; suppression 0.41 - user-facing coercion largely dismantled while supply-side interdiction, paraphernalia remnants, and federal-local siting conflicts persist; theater_ratio 0.23 - services are functionally real, with slow bureaucratic accretion around pilots, evaluation frameworks, and advisory machinery; accessibility_collapse 0.30 - the rival arrangements (full legalization, restored prohibition) remain politically alive and operating in other jurisdictions, so understanding this constraint does not foreclose its alternatives; resistance 0.55 - sustained neighborhood siting campaigns, recriminalization movements after visible disorder, and intergovernmental conflict. The three measurement series share one grid (t=0 to 24, step 4), every metric authored at every point. Receipt surface: gains disperse across several partial recipients (provider renewals, the enlarged treatment market, the preserved seller margin) with no single capturing seat, so gain_flow is authored as the checked universal diffuse rather than defaulted; fixing_cost is cheap - the authoring legislature can reverse the arrangement by ordinary bill at modest procedural cost, as a recent subnational reversal demonstrated.
 *
 * PERSPECTIVAL GAP:
 *   From the health-authority seat the arrangement computes as stewardship it designed and defends with its own outcome data; from the user seat it is ambivalent shelter - tolerance purchased at the price of unregulated supply and self-borne harm; from the site-neighborhood and crime-victim seats it is cost-shifting onto people who never consented; from the provider and manufacturer seats it is durable revenue. The engine derives these divergent per-seat classifications from the declared roles, power atoms, and exit options; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low-directionality seats: providers, manufacturers, and illicit operators collect directly, with the arbitrage-exited operators sitting nearest the beneficiary pole. Victim declarations map to high-directionality seats: site-adjacent residents and acquisition-crime victims, whose exit is mobile-at-a-price or constrained. people_who_use_drugs appears in both arrays - the expected structural delta made explicit - so its derived directionality should sit mid-scale, pulled target-ward by the trapped exit option and self-borne health harms and beneficiary-ward by free services and decriminalization. No directionality override is authored: overrides key to power atoms, and a second powerless seat (acquisition-crime victims) occupies a genuinely different structural position, so the role-declaration derivation distinguishes the two more safely than a shared-atom override could. Taxpayers derive near-symmetric: payment is broad-based and partly returned as avoided emergency and carceral cost. Suppression is authored as a raw structural property and is not scaled by scope; only extractiveness is scaled, and the national scope of the service network modestly amplifies verification difficulty.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - injection-driven epidemics combined with mass criminalization's demonstrated failure to reduce them - remains live in the synthetic-opioid era, so no mandatrophy is declared. The classification discipline cuts both ways here: reading the arrangement as pure coordination would erase the named residual victim sets and the preserved illicit-market margin; reading it as pure extraction would erase the documented service function and the withdrawal of user-facing punishment; tangled_rope is the type that holds the working service network and the asymmetric residue in one structure. The U-shaped suppression series argues against a degraded-inertial reading: enforcement capacity was actively wound down and then actively rebuilt, not merely performed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_binding,
    'This classification binds to one reading of the substance_control_authority kernel; what would the prohibition or legalization sibling readings change in the computed structure?',
    'Author and compile the sibling stories (substance_control_authority__prohibition_reading, substance_control_authority__legalization_reading) and compare per-seat classifications and epsilon across the kernel family.',
    'Under the prohibition sibling, users move fully into the victim set with criminal-justice burden added on top of health harm; under the legalization sibling the illicit-operator seat dissolves into licensed commerce and the preserved-margin burden disappears. Values authored here are valid only for the harm-reduction structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_binding, conceptual, 'Kernel-index binding: these numbers hold only for the harm-reduction reading of substance control authority.').

omega_variable(
    user_harm_attribution,
    'How much of the health harm users bear is attributable to this arrangement (adulterated criminal supply, uneven service coverage) versus to pharmacology and dependence themselves?',
    'Cohort and cross-jurisdiction comparison of health trajectories under user-depenalization regimes that differ in supply-side legal status.',
    'If most harm is arrangement-attributable, the user seat sits nearer the target pole and measured excess burden rises; if mostly intrinsic to dependence, the user seat nets toward the beneficiary pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_harm_attribution, empirical, 'Splitting the user''s residual victim status between the arrangement and the underlying condition.').

omega_variable(
    preserved_illicit_margin,
    'How large is the seller margin and associated violence that retaining supply-side prohibition preserves, given a buyer base the state no longer prosecutes?',
    'Compare jurisdictions combining user depenalization with legal supply against jurisdictions keeping supply criminal while depenalizing users.',
    'A large preserved margin means this reading externalizes a major burden onto communities and onto users buying unregulated product, raising effective extraction; a small margin supports the coordination-dominant account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preserved_illicit_margin, empirical, 'Size of the burden the kept-illegal supply side imposes under tolerated demand.').

omega_variable(
    third_party_risk_tradeoff,
    'Are the residual third-party costs (acquisition crime, localized disorder around services) an acceptable price of accepting users, or a failure of service adequacy?',
    'Not resolvable by data alone: depends on how much unconsented third-party risk a polity will trade for user autonomy and reduced carceral harm.',
    'Resolving toward acceptable-price keeps the third-party victim declarations as tradeoff bookkeeping; resolving toward failure converts them into indictment and pushes governing-seat classifications toward extraction-dominant readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(third_party_risk_tradeoff, preference, 'Value question about imposing residual risks on non-consenting third parties.').

omega_variable(
    service_rent_capture,
    'Do the provider and manufacturer seats convert the mandate into persistent over-collection beyond the cost of service delivered?',
    'Unit-cost benchmarking of syringe exchange, naloxone procurement, and prescribing against comparable public health interventions.',
    'Confirmed capture drives theater_ratio upward over the interval and pushes funder-seat classifications toward extraction-dominant readings; its absence supports the coordination account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(service_rent_capture, empirical, 'Whether service-sector and pharmaceutical seats collect more than coordination cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__harm_reduction_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(harm_reduction_reading_tr_t0, substance_control_authority__harm_reduction_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(harm_reduction_reading_tr_t4, substance_control_authority__harm_reduction_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(harm_reduction_reading_tr_t8, substance_control_authority__harm_reduction_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement(harm_reduction_reading_tr_t12, substance_control_authority__harm_reduction_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(harm_reduction_reading_tr_t16, substance_control_authority__harm_reduction_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(harm_reduction_reading_tr_t20, substance_control_authority__harm_reduction_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(harm_reduction_reading_tr_t24, substance_control_authority__harm_reduction_reading, theater_ratio, 24, 0.23).

% Extraction over time
narrative_ontology:measurement(harm_reduction_reading_be_t0, substance_control_authority__harm_reduction_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(harm_reduction_reading_be_t4, substance_control_authority__harm_reduction_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(harm_reduction_reading_be_t8, substance_control_authority__harm_reduction_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(harm_reduction_reading_be_t12, substance_control_authority__harm_reduction_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement(harm_reduction_reading_be_t16, substance_control_authority__harm_reduction_reading, base_extractiveness, 16, 0.46).
narrative_ontology:measurement(harm_reduction_reading_be_t20, substance_control_authority__harm_reduction_reading, base_extractiveness, 20, 0.46).
narrative_ontology:measurement(harm_reduction_reading_be_t24, substance_control_authority__harm_reduction_reading, base_extractiveness, 24, 0.47).

% Suppression requirement over time
narrative_ontology:measurement(harm_reduction_reading_su_t0, substance_control_authority__harm_reduction_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(harm_reduction_reading_su_t4, substance_control_authority__harm_reduction_reading, suppression_requirement, 4, 0.41).
narrative_ontology:measurement(harm_reduction_reading_su_t8, substance_control_authority__harm_reduction_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(harm_reduction_reading_su_t12, substance_control_authority__harm_reduction_reading, suppression_requirement, 12, 0.36).
narrative_ontology:measurement(harm_reduction_reading_su_t16, substance_control_authority__harm_reduction_reading, suppression_requirement, 16, 0.37).
narrative_ontology:measurement(harm_reduction_reading_su_t20, substance_control_authority__harm_reduction_reading, suppression_requirement, 20, 0.39).
narrative_ontology:measurement(harm_reduction_reading_su_t24, substance_control_authority__harm_reduction_reading, suppression_requirement, 24, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__harm_reduction_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, substance_control_authority__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, substance_control_authority__legalization_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label drug policy decomposes, per the epsilon-invariance principle, into three structurally distinct arrangements of the substance_control_authority kernel. They differ on the user-facing sanction axis and the market-status axis, so their epsilon values and victim sets differ by construction: the prohibition sibling concentrates extraction on users through the criminal process; this harm-reduction reading splits the victim set (users partially, third parties residually) while preserving an illicit-market extraction channel; the legalization sibling removes the illicit channel and replaces it with regulated-commerce frictions. The stories are linked pairwise through affects_constraints; the prohibition reading is historically upstream (the status quo ante that reforms respond to), and this reading exerts downstream structural pressure on the legalization debate by producing the evidence base and infrastructure that market-regulation proposals inherit.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
