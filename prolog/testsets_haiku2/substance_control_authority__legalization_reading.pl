% ============================================================================
% CONSTRAINT STORY: substance_control_authority__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__legalization_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: substance_control_authority__legalization_reading
 *   human_readable: Legalized Drug Market Regulation Authority
 *   domain: public_health/economic_policy/criminal_justice
 *
 * SUMMARY:
 *   This constraint instantiates the LEGALIZATION READING of the contested
 *   substance_control_authority kernel. The state claims authority to
 *   regulate drug markets as legal commerce with quality and access controls,
 *   displacing prohibition (criminalization) and harm-reduction (tolerance
 *   without legality) as the governing frame. Under legalization, users gain
 *   exit from criminal markets and liability, while regulatory authority
 *   substitutes for prohibition's criminal apparatus. The reading's core
 *   claim: market regulation under state oversight is the legitimate
 *   mechanism for substance control, producing user protection via quality
 *   assurance and public health data integration. This stands against the
 *   prohibition reading (criminal liability as deterrent) and the
 *   harm-reduction reading (state tolerance of use with minimal legal
 *   interference). The authorially independent metrics (0.62 extractiveness,
 *   0.41 suppression, 0.28 theater) describe the constraint's actual
 *   operation: it extracts substantially (producers capture rents, users pay
 *   higher prices, tax transfer), requires moderate suppression (enforcement
 *   of retail boundaries and unlicensed-supplier prohibition), and carries
 *   some theater (regulatory legitimacy narrative exceeds the
 *   enforcement/coordination machinery's explanatory power). The constraint
 *   is CLAIMED as tangled_rope (coordination + asymmetric extraction) and the
 *   metrics are consistent with that claim — the gap between what the reading
 *   justifies (market coordination for user safety) and what is measured
 *   (substantial extraction with moderate suppression) is exactly the
 *   diagnostic terrain.
 *
 * KEY AGENTS:
 *   - regulatory_authority: Sets all market boundaries, licenses producers, enforces retail/user compliance — d near 0.0 (beneficiary)
 *   - regulated_producers: Gain monopoly rents from legal exclusivity, pay compliance/tax costs — d near 0.2 (light beneficiary)
 *   - users_compliant: Exit criminal liability and gain product safety; pay higher prices and comply with consumption restrictions — d near 0.5 (symmetric)
 *   - prior_black_market_suppliers: Lose market access entirely, face criminal prosecution, trapped — d near 1.0 (full target)
 *   - users_noncompliant: Remain subject to criminal liability under heightened enforcement (residual black market is now explicitly illegal); cannot afford or access legal supply — d near 1.0 (full target)
 *   - law_enforcement: Shift from mass interdiction to targeted boundary enforcement; budget and resource profile improve; incarceration rates for possession drop — d near 0.1 (light beneficiary, via institutional benefit)
 *   - third_parties_public_health: Gain surveillance/intervention capacity via regulated supply data; bear cost risk if usage volumes rise — d near 0.3 (moderate beneficiary with contingent cost)
 *   - communities_bearing_transition_costs: Absorb retail-density externalities (congregation, robbery, zoning conflict) without compensation — d near 0.85 (substantial target, via localized unpaid harm)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__legalization_reading, 0.62).
domain_priors:suppression_score(substance_control_authority__legalization_reading, 0.41).
domain_priors:theater_ratio(substance_control_authority__legalization_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__legalization_reading, "Legalized Drug Market Regulation Authority").
narrative_ontology:topic_domain(substance_control_authority__legalization_reading, "public_health/economic_policy/criminal_justice").

domain_priors:requires_active_enforcement(substance_control_authority__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__legalization_reading, '32a1f86c-70f6-47f7-b679-534459d59cf8').
narrative_ontology:cs_kernel_codification('32a1f86c-70f6-47f7-b679-534459d59cf8', distributed).
narrative_ontology:cs_authority_grounding('32a1f86c-70f6-47f7-b679-534459d59cf8', expertise).
narrative_ontology:cs_interpretation_layer_present('32a1f86c-70f6-47f7-b679-534459d59cf8').
narrative_ontology:cs_reading_relation('32a1f86c-70f6-47f7-b679-534459d59cf8', substance_control_authority__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('32a1f86c-70f6-47f7-b679-534459d59cf8', substance_control_authority__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('32a1f86c-70f6-47f7-b679-534459d59cf8', foundational, legal_market_regulation_superior_to_prohibition).
narrative_ontology:cs_axiom_status(legal_market_regulation_superior_to_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('32a1f86c-70f6-47f7-b679-534459d59cf8', legal_market_regulation_superior_to_prohibition, empirically_contingent).
narrative_ontology:cs_axiom('32a1f86c-70f6-47f7-b679-534459d59cf8', foundational, market_coordination_primary_harm_reduction_mechanism).
narrative_ontology:cs_axiom_status(market_coordination_primary_harm_reduction_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('32a1f86c-70f6-47f7-b679-534459d59cf8', market_coordination_primary_harm_reduction_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('32a1f86c-70f6-47f7-b679-534459d59cf8', evidence_based_substance_policy).
narrative_ontology:cs_drift_state('32a1f86c-70f6-47f7-b679-534459d59cf8', contemporary_post_opioid_crisis, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('32a1f86c-70f6-47f7-b679-534459d59cf8', '').
narrative_ontology:cs_kernel_id(substance_control_authority__legalization_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, regulated_producers).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, public_health_systems).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, law_enforcement_agencies).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, prior_black_market_suppliers).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, users_subject_to_compliance_requirements).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, communities_bearing_transition_costs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, users_compliant).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, law_enforcement).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, third_parties_public_health).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, users_compliant).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, users_noncompliant).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces all rules governing legal drug markets: licensing criteria for producers, allowable potency/dosage ranges, retail density and location restrictions, age gates, purchase quantity limits, tax rates, and tracking/compliance monitoring. Operates the boundary between legal and illegal markets through inspection, licensing denial, and criminal prosecution of unlicensed suppliers and retailers. Collects tax revenue from legal transactions. Claims legitimacy from the public health mission (user safety via quality assurance and data integration).
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, regulatory_authority, agenda_setter,
    institutional, generational, analytical, national).

% Obtain exclusive legal market access (black market suppliers are criminally excluded by regulatory enforcement). Sell at prices substantially higher than pre-legalization black market (product quality markup, tax pass-through, compliance costs). Operate openly without criminal liability for supply. Bear licensing fees, compliance and testing costs, and tax liabilities. Entry to production is controlled by regulatory standards (capital requirements, track record, facility standards); most prior black market suppliers cannot meet these. High profit margins during the first 5–10 years of market establishment, then competitive pressure from new entrants holding licenses.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, regulated_producers, beneficiary,
    organized, biographical, constrained, national).

% Exit the criminal market entirely; possession is legal (in full legalization) or decriminalized (in partial regimes). Gain access to tested product at known purity and potency, reducing overdose risk from unknown fentanyl or adulterants. Can seek regulatory remedy if product is mislabeled or defective. Subject to retail restrictions (licensed locations only, age verification, quantity limits per transaction, hours of operation). Pay substantially higher prices than pre-legalization black market (estimated 30–60% markup from tax + quality + compliance). Tracked via licensing and sales databases, creating surveillance infrastructure (potential barrier to truly private use). Most users in a legalized jurisdiction transition to the legal market within 1–3 years.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, users_compliant, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__legalization_reading, users_compliant, payer).

% Lose market access entirely. Legal competitors undersell them because legal producers can operate openly (no risk premium), benefit from economies of scale, and have superior supply chains; black market prices cannot compete. Face increased criminal prosecution for operating outside the now-legal market (whereas prohibition regimes tolerate varying levels of black market activity, legalization makes unlicensed supply explicitly illegal and enforcement-targeted). Those with capital, technical knowledge, and clean records can attempt transition into licensed production if they can meet regulatory standards and obtain licensing approval (most cannot due to capital or record barriers). Those without a transition path face exit: quit the industry, relocate to unlegalized jurisdictions, or attempt to operate in the residual black market (high risk, low profit).
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, prior_black_market_suppliers, payer,
    powerful, immediate, trapped, national).

% Reject the regulatory frame (e.g., seek unmarked product, higher potency than permitted, untracked purchase, consumption outside regulated locations). May be unable to afford legal prices (particularly relevant for frequent users or those with high-potency preferences). May refuse the surveillance infrastructure (licensing databases). Remain subject to criminal penalties for possession in most legalization regimes (though penalties are typically lighter than prohibition regimes — e.g., decriminalized fines rather than incarceration). Face increased enforcement pressure compared to prohibition regimes because legalization makes the legal/illegal boundary stark and enforcement can target it directly (residual black market prosecution); in prohibition regimes, tolerance and enforcement inconsistency blur the boundary. Are trapped: cannot access legal supply on their own terms, and cannot exit to the prior black market (it has been dismantled by legal-market competition and enforcement).
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, users_noncompliant, payer,
    powerless, biographical, identity_locked, national).

% Shift enforcement mission from mass interdiction and user incarceration (expensive, generates low conviction rates, high incarceration population burden) to targeted prosecution of unlicensed suppliers and enforcement of retail boundaries. Incarceration for drug possession declines sharply (major budget relief and institutional legitimacy gain). Regulatory enforcement employment increases (inspection, licensing denial, prosecution of unlicensed retailers). Cooperation with regulatory authority (joint enforcement of producer licensing, supply-chain tracking) becomes primary mechanism. Overall enforcement resource demand may drop 20–40% compared to prohibition regimes, freeing capacity for other priority crimes.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, law_enforcement, beneficiary,
    institutional, generational, analytical, national).

% Gain epidemiological data from the regulated market (dosage/purity tracking, user licensing databases, retail transaction logs) enabling public health surveillance at scale. Can target interventions on known populations (e.g., high-potency users, youth users) and track outcomes. Reduce acute health harms from adulterated product (overdose deaths from unknown fentanyl drop). Bear contingent cost if overall usage volume rises post-legalization: increased prevalence means higher absolute numbers of dependent users, overdose risk despite product safety, and public health resource demand for treatment/prevention. Benefit is achieved if regulatory authority acts on surveillance data; threat is data without capacity to intervene or a growing user base without corresponding treatment infrastructure.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, third_parties_public_health, beneficiary,
    institutional, generational, analytical, national).

% Experience concentrated retail presence of licensed drug outlets (dispensaries, licensed bars, vape lounges, etc.). Neighborhoods chosen for retail-outlet clustering face increased user congregation, potential property crime and violence near outlets, zoning conflicts, and externalities (litter, odors, user congregation on streets). These harms are not uniformly distributed: lower-income neighborhoods and communities of color are disproportionately targeted for retail-density zoning (regulatory authorities seek high-population-density areas for tax efficiency). Compensation for retail-adjacent harms is not standard in most legalization regimes; mitigation funds and community benefit agreements are secondary or absent. Are not seated in the regulatory decision-making process that determines their neighborhood's retail density.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, communities_bearing_transition_costs, payer,
    powerless, biographical, constrained, local).

% Under the legalization reading, harm reduction (supervised consumption facilities, syringe services, medication-assisted treatment) is subordinated to legal-market operation as a secondary public health mechanism. Practitioners would argue that legalization alone does not address demand-side harms (addiction, overdose, mental health integration, trauma-informed care) and that harm reduction should be co-primary, not secondary. They are not parties to the regulatory authority's market-design decisions and their perspective is marginalized in policy discourse dominated by the legalization vs. prohibition binary.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, medical_harm_reduction_practitioners, excluded,
    organized, biographical, constrained, national).

% Suppliers from prohibition-regime jurisdictions without legalization export into legalized markets to undercut regulated prices (tax and compliance cost arbitrage). Also, residual black markets within legalization jurisdictions persist for noncompliant users (seeking higher potency, untracked product, or lower price). These markets are structurally defined as the regulatory authority's enforcement target but are not seated in the regulatory conversation. They compete with legal producers and are the mechanism by which noncompliant users and those in prohibition jurisdictions continue to access product outside the legal system.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, adjacent_black_markets, excluded,
    powerful, immediate, trapped, regional).

% Neighboring prohibition-regime enforcement faces conflicting incentives. They benefit from cross-border drug tourism and smuggling (institutional justification for budgets, career advancement through drug-interdiction arrests). But they lose cooperation from legalization-jurisdiction authorities (who no longer treat drug supply as a priority crime and may actively work against their enforcement). This creates enforcement divergence: the prohibition regime wants strict border control and prosecution of export to legalization jurisdictions; the legalization regime does not prioritize border enforcement and may view it as undermining their market-stabilization goals. Are not seated at the decision table and operate at cross-purposes.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, enforcement_agencies_adjacent_jurisdictions, observer,
    institutional, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__legalization_reading, regulated_producers).
narrative_ontology:fixing_cost_class(substance_control_authority__legalization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a unified supply chain (cultivation → processing → distribution → retail) under regulatory oversight, replacing fragmented black markets with known purity/dosage, consistent retail availability, and public health data integration. Solves the black-market problem of adulteration, violent supplier competition, and supply-side unpredictability.
% TRANSFER_FUNCTION: Moves a tax share of every transaction to the state (and via licensing fees to the regulatory authority); moves monopoly rents to licensed producers; moves compliance costs to users (higher prices, retail restrictions); moves enforcement costs from mass interdiction to boundary enforcement (prosecution of unlicensed suppliers and users noncompliant with retail restrictions). The transfer is primarily vertical (users and noncompliant users to producers and state) and horizontal (black market suppliers to licensed producers).
% ABSENT_VOICES: Prior black market suppliers are excluded by enforcement (not seated as parties); users with non-compliant preferences are excluded from the conversation that designs retail boundaries; practitioners of harm-reduction approaches that do not rely on legal supply (supervised consumption, medication-assisted treatment outside pharmaceutical channels) are marginalized beneath the legalization frame's emphasis on legal-market operation; adjacent jurisdictions remain prohibition-regime enforcement, creating regional contraband flows but no seat at the design table.
% DISAPPEARANCE_RATIONALE: If the legal regulatory authority and its enforcement vanished, black markets would rapidly reconstitute, prices would fall toward pre-legalization black-market levels, adulteration would return as a dominant supply problem, and enforcement agencies would face renewed pressure to criminalize possession and supply. The entire user population would revert to illegal supply or abstention. The constraint's persistence is essential to the legal market's existence.
% FOUNDING_PROBLEM: Black markets produce adulterated/unknown-potency product causing user overdose deaths, violent supplier competition causing collateral homicides, law enforcement corruption from drug-trade bribery, and mass incarceration of users and low-level suppliers without reducing supply. Users cannot verify product safety and face criminal liability for seeking it.
% FOUNDING_PROBLEM_CORROBORATION: Public health researchers attesting overdose deaths from unknown fentanyl concentrations in illicit supply; enforcement agencies attesting costs of mass incarceration and corruption; user advocacy groups attesting to adulteration harms and incarceration barriers to treatment; economic analyses documenting that enforcement spending does not reduce supply (outside the benefiting parties). The corroboration is robust across seat boundaries.
narrative_ontology:disappearance_verdict(substance_control_authority__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__legalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__legalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_authority__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__legalization_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__legalization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_authority__legalization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_authority__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) tracks the constraint's core transfer: users pay higher prices (tax + quality/compliance markup), black market suppliers lose market entirely, producers capture rents from legalization, and the state/regulatory authority collect tax and control. The reading legitimates this as coordination value (users exit crime, gain safety), but measurement shows extraction exceeds plausible coordination cost — the tax rate, producer markup, and retail-boundary compliance costs are substantially higher than the marginal cost of quality assurance and public health data. Suppression (0.41, mid-range) reflects that the constraint's persistence requires active enforcement of the legal/illegal boundary: unlicensed suppliers are criminally prosecuted, users in noncompliant markets face increased enforcement intensity (legalization makes the boundary stark, raising prosecution pressure on residual black markets). Suppression is lower than in prohibition regimes because compliant users face no criminal pressure; suppression concentrates on boundary enforcement and residual-market prosecution. Theater (0.28, low-moderate) marks a constraint where the coordination narrative (quality assurance, public health) is real but does not account for the full extraction machine (tax, producer monopoly, retail boundaries). The measurements track the interval post-legalization: extractiveness rises slightly in years 0–18 (producers consolidate market, usage volumes stabilize), then plateaus as the market matures. Suppression and theater are stable — no evidence of escalation or attenuation over the interval. This stability suggests the constraint has reached an equilibrium: the regulatory frame is accepted, enforcement is routinized, and extraction has scaled to whatever level the market will bear. The shared measurement grid ensures every metric is authored at every time point (0, 3, 6, 12, 18, 25), preventing temporal misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The regulatory authority and compliant-user seats should compute differently from the black-market-supplier and noncompliant-user seats. From the authority's position: legalization is coordination (safety, tax revenue, enforcement efficiency). From the black-market-supplier position: legalization is predation (market exclusion, criminal prosecution, exit denial). From the noncompliant-user position: legalization increases enforcement pressure relative to prohibition (the boundary is now stark and actively enforced, whereas prohibition regimes often tolerate some use in practice). The engine derives this per-seat divergence from the structural data: authority gets low d (beneficiary), black market suppliers get high d (target, trapped exit). The authored claim (tangled_rope) is consistent with this divergence — tangled rope requires both coordination and extraction, and it requires that the coordinated and extracted seats experience different types. The claim and metrics are independent facts; the divergence is the measurement the system exists to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality divides sharply by stakeholder structure. The regulatory authority (d ≈ 0.0, full beneficiary) sets rules that benefit itself (tax revenue, institutional control, enforcement budget); exit is not salient because the authority IS the rule. Licensed producers (d ≈ 0.2) are moderate beneficiaries: they gain legal monopoly rents but pay compliance/tax costs and are subject to regulatory control. Compliant users (d ≈ 0.5) are near-symmetric: they gain exit from criminality and product safety (benefits), pay higher prices and comply with retail restrictions (costs). Non-compliant users and black market suppliers (d ≈ 1.0) are full targets: they are explicitly criminalized and excluded. Law enforcement (d ≈ 0.1) is a light beneficiary via institutional improvement (lower mass-incarceration burden, clearer targeting). Communities bearing transition costs (d ≈ 0.85) are substantial targets: they absorb retail externalities without compensation or a seat at the design table. The gap between the regulatory authority's seat and the powerless/noncompliant user seat is the largest: the authority experiences the constraint as a beneficial coordination mechanism it administers; the noncompliant user experiences it as criminal prohibition with higher suppression intensity. This divergence is a feature of the tangled_rope classification: the same constraint provides coordination for compliant users and extraction from noncompliant users.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (the founding problem has become obsolete) is CONTESTED here. The founding problem — black-market adulteration, supply-chain violence, mass incarceration for possession — is LIVE from user and public-health advocacy seats: the problem it was built to solve (user safety, reducing overdose deaths from unknown-potency product) persists and is substantially addressed by the legal-supply mechanism. However, a secondary reading argues mandatrophy: if usage volumes rise post-legalization, the public-health benefit becomes contingent on regulatory capacity to manage higher prevalence. If prevalence rises faster than treatment infrastructure and prevention capacity, the founding-problem answer (safe legal supply) becomes insufficient and the constraint shifts from solving the founding problem to managing its escalation. This is a live disagreement between regulatory advocates (founding problem is live, legalization is solving it) and critics (founding problem was volume-bounded, legalization has delinked volume from supply-side risk, creating new public-health mandates not solved by legalization alone). The mandatrophy verdict is CONTESTED rather than resolved; the engine's mandatrophy flag would trigger (founding_problem_status=live + high extraction + rising theater_ratio IF theater_ratio were rising, which it is not) only if usage-driven costs begin to exceed the founding-problem solution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    usage_volume_response,
    'How much will usage volume increase post-legalization, and at what rate? Will it stabilize or continue rising?',
    'Longitudinal epidemiological data from legalized jurisdictions (Canada, Uruguay, various US states) tracking usage prevalence, frequency, and intensity post-legalization versus pre-legalization baselines and prohibition-regime neighbors.',
    'If volume rise is modest and stable (10–20%), the founding problem is largely solved by the legal-supply mechanism. If volume rises sharply (50%+) and continues, the founding problem becomes volume-management (treatment infrastructure, prevention capacity) rather than supply-chain safety, and the constraint''s efficacy at solving the founding problem decays. High volume rise combined with stagnant treatment capacity would trigger mandatrophy signals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(usage_volume_response, empirical, 'The post-legalization usage trajectory and its relationship to public health capacity.').

omega_variable(
    regulatory_capture_by_producers,
    'Will regulated producers capture the regulatory authority and use it to maximize extraction (e.g., blocking potency limits, raising tax favorability, blocking new entrants) rather than achieving the legalization reading''s stated goal of user protection?',
    'Regulatory-decision analysis: do licensing, tax policy, and potency standards track user-safety goals or producer-profit maximization? Testimony from regulatory authority staff about industry lobbying and agency independence.',
    'If capture occurs, the constraint shifts from tangled_rope (coordination + asymmetric extraction) toward snare (pure extraction cover story of coordination). The theater ratio would rise (enforcement becomes increasingly about protecting producer interests rather than user safety). Extraction would plateau or rise as the regulatory boundary becomes a cartel-protection mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_by_producers, empirical, 'Whether the regulatory authority remains independent or is captured by the regulated industry.').

omega_variable(
    residual_black_market_persistence,
    'Does the residual black market for noncompliant (higher-potency, unregulated) product persist at significant scale, and does it undermine the legalization reading''s user-safety claim?',
    'Supply-side data: drug-seizure analysis, user-survey data on sources (what share of users buy from legal vs. black market), price/potency comparison between legal and black markets.',
    'If the residual black market persists at 20%+ of pre-legalization volume, users seeking higher potency or lower cost continue to face adulteration risks, and the founding-problem solution is incomplete. The constraint''s claim to safety superiority is weakened and the suppression measurement should rise (enforcement must intensify to defend the legal market''s share against black-market undercutting).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_black_market_persistence, empirical, 'Whether legalization eliminates the black market or creates a tiered market with persistent residual supply.').

omega_variable(
    committer_frame_reading_stability,
    'Will the legalization reading remain the governing frame, or will events force a reversion to prohibition (due to usage surge, public health crisis, political shift) or a collapse into harm reduction (due to regulatory failure, captured regulatory authority)?',
    'Political-economy analysis: is the legalization reading''s coalition stable? Do users continue to support legal regulation, or do usage harms (overdose, psychosis, driving impairment) trigger demand for stricter controls? Do producers continue to lobby for market expansion, or do public health crises trigger closure pressures?',
    'If the legalization reading destabilizes and the prohibition or harm-reduction readings take over, this constraint becomes superseded and a new sibling constraint becomes the operative one. This is not foreclosure (the readings remain live in other jurisdictions or among other parties) but reading-switching within a single jurisdiction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_reading_stability, conceptual, 'Whether the legalization reading''s authority framework remains stable or is displaced by a sibling reading.').

omega_variable(
    identity_lock_mechanism_noncompliant_users,
    'Is the identity_locked exit categorization for noncompliant users accurate? Or do noncompliant users face constrained but not identity-locked exit?',
    'Qualitative research: interviews with users who remain in the black market post-legalization. Do they continue because they cannot afford legal supply (constrained exit), prefer unmarked product (preference lock), have built identity around illicit supply or refusal of state authority (identity lock), or face active suppression/surveillance (trapped exit)?',
    'If exit is constrained (financial barriers) rather than identity-locked, the suppression measurement is lower than authored (suppression does not bind those with adequate resources), and the constraint is less totalizing than described. If identity-locked (refusal of the regulatory frame), suppression is higher because the constraint depends on psychological/ideological conformity, not just enforcement. This affects the directionality of noncompliant users: constrained exit → d near 0.8; identity-locked → d closer to 1.0.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_noncompliant_users, empirical, 'The nature of exit constraints for users who reject legal-market compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__legalization_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__legalization_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(subs_tr_t0, projected).
narrative_ontology:measurement(subs_tr_t3, substance_control_authority__legalization_reading, theater_ratio, 3, 0.22).
narrative_ontology:measurement_basis(subs_tr_t3, observed).
narrative_ontology:measurement(subs_tr_t6, substance_control_authority__legalization_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement_basis(subs_tr_t6, observed).
narrative_ontology:measurement(subs_tr_t12, substance_control_authority__legalization_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement_basis(subs_tr_t12, observed).
narrative_ontology:measurement(subs_tr_t18, substance_control_authority__legalization_reading, theater_ratio, 18, 0.29).
narrative_ontology:measurement_basis(subs_tr_t18, observed).
narrative_ontology:measurement(subs_tr_t25, substance_control_authority__legalization_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(subs_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__legalization_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(subs_be_t0, projected).
narrative_ontology:measurement(subs_be_t3, substance_control_authority__legalization_reading, base_extractiveness, 3, 0.52).
narrative_ontology:measurement_basis(subs_be_t3, observed).
narrative_ontology:measurement(subs_be_t6, substance_control_authority__legalization_reading, base_extractiveness, 6, 0.56).
narrative_ontology:measurement_basis(subs_be_t6, observed).
narrative_ontology:measurement(subs_be_t12, substance_control_authority__legalization_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement_basis(subs_be_t12, observed).
narrative_ontology:measurement(subs_be_t18, substance_control_authority__legalization_reading, base_extractiveness, 18, 0.62).
narrative_ontology:measurement_basis(subs_be_t18, observed).
narrative_ontology:measurement(subs_be_t25, substance_control_authority__legalization_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(subs_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__legalization_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(subs_su_t0, projected).
narrative_ontology:measurement(subs_su_t3, substance_control_authority__legalization_reading, suppression_requirement, 3, 0.38).
narrative_ontology:measurement_basis(subs_su_t3, observed).
narrative_ontology:measurement(subs_su_t6, substance_control_authority__legalization_reading, suppression_requirement, 6, 0.4).
narrative_ontology:measurement_basis(subs_su_t6, observed).
narrative_ontology:measurement(subs_su_t12, substance_control_authority__legalization_reading, suppression_requirement, 12, 0.41).
narrative_ontology:measurement_basis(subs_su_t12, observed).
narrative_ontology:measurement(subs_su_t18, substance_control_authority__legalization_reading, suppression_requirement, 18, 0.42).
narrative_ontology:measurement_basis(subs_su_t18, observed).
narrative_ontology:measurement(subs_su_t25, substance_control_authority__legalization_reading, suppression_requirement, 25, 0.41).
narrative_ontology:measurement_basis(subs_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__legalization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(substance_control_authority__legalization_reading, 0.18).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, substance_control_authority__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, substance_control_authority__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the substance_control_authority kernel. All three readings (prohibition, legalization, harm_reduction) dispute the state's legitimate authority over substance markets and the mechanism for exercising it. The legalization reading instantiates the constraint when the state chooses market regulation over criminalization (prohibition) or tolerance (harm_reduction). The network links capture that these are readings of the same kernel, not separate constraints; a jurisdiction's choice of one reading displaces the others. Each reading has distinct metrics, beneficiaries, and enforcement apparatus despite sharing the same real-world subject matter (state authority over drug markets). The ε values differ substantially: prohibition is similarly extractive (0.60–0.65) but relies on criminal suppression (0.80+), while legalization relies on regulatory suppression (0.41); harm reduction has lower extractiveness (0.35–0.45) and suppression (0.25–0.35) but requires different beneficiary alignment (public health over producers/authority). The three readings compete in the same policy spaces; the network relationships preserve the fact that a choice of one reading has downstream effects on the others (e.g., if legalization succeeds in a jurisdiction, it reduces political pressure for harm reduction and increases evidence against prohibition).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_authority__legalization_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
