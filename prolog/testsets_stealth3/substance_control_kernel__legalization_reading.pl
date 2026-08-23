% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   human_readable: Legalized-Regulated Substance Market Regime (Liberty Reading)
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   A jurisdiction treats adult substance use as a protected liberty:
 *   possession and sale are lawful through licensed channels, the state
 *   levies excise taxes justified as externality-cost capture, enforces
 *   third-party protections (impaired-driving enforcement, age gates,
 *   place-of-use limits), and routes revenue to general funds and public
 *   health programs. Criminal sanctions on users are gone; in their place
 *   stands a taxed, licensed market with an enforcement apparatus aimed at
 *   third-party harms and at suppliers operating outside the license system.
 *   Assumptions stated plainly: the story models a mature post-legalization
 *   regime of the cannabis-style kind, generalized across substances;
 *   interval years 0-14 approximate 2010-2024; tax and licensing figures
 *   reflect the typical trajectory of such regimes rather than any single
 *   statute. The claim/metrics split is deliberate: the arrangement is
 *   CLAIMED as tangled_rope from its structure (genuine coordination function
 *   plus identifiable uncompensated payers), while each metric is authored
 *   from what descriptively holds — the engine computes per-seat
 *   classifications from the structural data and owns any divergence.
 *
 * KEY AGENTS:
 *   - adult_substance_users: Primary beneficiary (moderate/constrained) — hold restored legal access; pay excise taxes and comply with place-of-use limits
 *   - licensed_producers_retailers: Secondary beneficiary (powerful/arbitrage) — run the legal market; absorb licensing and compliance costs; lobby on tax and license policy
 *   - state_tax_and_regulatory_agencies: Agenda setter and revenue collector (institutional/constrained) — writes and enforces the rules and receives the revenue
 *   - public_health_agencies: Funded monitor (institutional/constrained) — receives earmarked revenue, tracks harms, advises on limits; funding tied to continued sales
 *   - road_traffic_participants: Primary third-party payer (powerless/trapped) — bears impaired-driving risk never consented to and cannot exit shared roads
 *   - involuntary_secondhand_exposure_populations: Third-party payer (powerless/trapped) — breathes shared air in housing, workplaces, and public space
 *   - gray_market_operators: Residual supplier (organized/mobile) — persists where taxes or license scarcity leave price room; bears enforcement costs
 *   - recovery_community_members: Excluded voice (organized/constrained) — objects to normalization and advertising but holds no seat in licensing decisions
 *   - civil_liberties_organizations: Analytical observer (organized/analytical) — polices the boundary between warranted third-party protection and liberty infringement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__legalization_reading, 0.52).
domain_priors:suppression_score(substance_control_kernel__legalization_reading, 0.38).
domain_priors:theater_ratio(substance_control_kernel__legalization_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(substance_control_kernel__legalization_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__legalization_reading, "Legalized-Regulated Substance Market Regime (Liberty Reading)").
narrative_ontology:topic_domain(substance_control_kernel__legalization_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__legalization_reading, '464a11f9-fe57-4a06-84f4-75294c3aedfa').
narrative_ontology:cs_kernel_codification('464a11f9-fe57-4a06-84f4-75294c3aedfa', distributed).
narrative_ontology:cs_authority_grounding('464a11f9-fe57-4a06-84f4-75294c3aedfa', distributed).
narrative_ontology:cs_reading_relation('464a11f9-fe57-4a06-84f4-75294c3aedfa', substance_control_kernel__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('464a11f9-fe57-4a06-84f4-75294c3aedfa', substance_control_kernel__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('464a11f9-fe57-4a06-84f4-75294c3aedfa', foundational, adult_use_is_protected_liberty).
narrative_ontology:cs_axiom_status(adult_use_is_protected_liberty, holdable).
narrative_ontology:cs_axiom_grounding('464a11f9-fe57-4a06-84f4-75294c3aedfa', adult_use_is_protected_liberty, deontological).
narrative_ontology:cs_axiom('464a11f9-fe57-4a06-84f4-75294c3aedfa', foundational, externality_capture_over_punishment).
narrative_ontology:cs_axiom_status(externality_capture_over_punishment, holdable).
narrative_ontology:cs_axiom_grounding('464a11f9-fe57-4a06-84f4-75294c3aedfa', externality_capture_over_punishment, instrumental).
narrative_ontology:cs_reference_frame('464a11f9-fe57-4a06-84f4-75294c3aedfa', liberty_default_externality_pricing_state).
narrative_ontology:cs_drift_state('464a11f9-fe57-4a06-84f4-75294c3aedfa', contemporary_regulated_market_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('464a11f9-fe57-4a06-84f4-75294c3aedfa', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__legalization_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, adult_substance_users).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, licensed_producers_retailers).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, state_tax_and_regulatory_agencies).
narrative_ontology:constraint_beneficiary(substance_control_kernel__legalization_reading, public_health_agencies).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, road_traffic_participants).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, involuntary_secondhand_exposure_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, adult_substance_users).
narrative_ontology:constraint_victim(substance_control_kernel__legalization_reading, gray_market_operators).
narrative_ontology:constraint_vindicates(substance_control_kernel__legalization_reading, mill_harm_principle).
narrative_ontology:constraint_vindicates(substance_control_kernel__legalization_reading, individual_liberty_doctrine).
narrative_ontology:constraint_vindicates(substance_control_kernel__legalization_reading, externality_internalization_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Buy and consume lawful substances through licensed retailers. They gained legal access, tested product, and freedom from possession prosecution; they pay excise taxes built into prices, observe place-of-use rules, and face impaired-driving liability if they drive after using. Leaving the arrangement means abstaining, relocating to a stricter jurisdiction, or buying untaxed product at legal risk.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, adult_substance_users, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__legalization_reading, adult_substance_users, payer).

% Grow, process, and sell under licenses the state grants and caps. Revenue scales with the legal market; costs include licensing fees, testing, seed-to-sale tracking, and excise taxes. They lobby legislatures on tax rates and license counts and can shift capital between product lines or across state lines where rules differ.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, licensed_producers_retailers, beneficiary,
    powerful, biographical, arbitrage, national).

% Write and enforce licensing, testing, advertising, and tax rules; collect excise revenue into general funds and earmarked accounts. Their budgets and the programs they finance now depend partly on continued taxable sales, and statutes bind them to revenue forecasts they helped write.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, state_tax_and_regulatory_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Receive earmarked shares of substance-tax revenue for treatment, prevention, and surveillance. They publish harm data and recommend limits on potency, outlet density, and advertising; their program budgets rise and fall with taxable consumption, which ties their operating scale to the behavior they are charged with reducing.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, public_health_agencies, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__legalization_reading, public_health_agencies, observer).

% Share roads with drivers who may be impaired. They consented to no such exposure; their protection consists of enforcement they do not control, and compensation reaches them only partially, through general funds their own taxes also fill. In car-dependent areas they cannot opt out of road use.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, road_traffic_participants, payer,
    powerless, biographical, trapped, national).

% Live and work adjacent to consumption they did not choose: multi-unit housing, sidewalks, patios, shared ventilation. Place-of-use rules reduce but do not eliminate exposure, and harm is diffuse, making attribution and compensation difficult for any single exposed person.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, involuntary_secondhand_exposure_populations, payer,
    powerless, biographical, trapped, regional).

% Sell without licenses where taxes, license caps, or municipal bans leave a price gap. They undercut taxed prices, bear seizure and penalty risk scaled well below the old criminal regime, and migrate across product lines and borders as enforcement attention shifts.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, gray_market_operators, payer,
    organized, immediate, mobile, national).

% Maintain recovery from dependence and object to retail density, advertising, and normalization messages they argue recruit new dependent users. Licensing hearings hear industry and revenue testimony; their testimony carries no vote and they hold few seats in the bodies setting retail policy.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, recovery_community_members, excluded,
    organized, biographical, constrained, national).

% Litigate and comment on where regulation crosses from protecting third parties into restricting consenting adults: advertising bans, home-cultivation limits, consumption-place rules. They hold no administrative power and work through courts, comment periods, and public argument.
narrative_ontology:constraint_stakeholder(substance_control_kernel__legalization_reading, civil_liberties_organizations, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__legalization_reading, state_tax_and_regulatory_agencies).
narrative_ontology:fixing_cost_class(substance_control_kernel__legalization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Operates a licensed, tested, taxed supply chain in place of an unlicensed one: product-safety standards are solved once and centrally, age verification is enforced at retail, response to impaired driving concentrates on the dangerous act rather than possession, and a dedicated revenue stream is assigned to costs the trade imposes on others.
% TRANSFER_FUNCTION: Moves excise revenue from producers and consumers to state treasuries and earmarked programs; leaves residual harm risk on third parties wherever pricing under-recovers externality costs; moves market share from unlicensed suppliers to license holders.
% ABSENT_VOICES: Recovery community members object to normalization and advertising but hold no seat in licensing decisions; residents of neighborhoods with concentrated retail bear siting burdens decided in other rooms; minors are excluded by design and cannot contest the advertising environment surrounding them; citizens holding the punitive stance have no seat inside this regime's justification at all — their position lives in the sibling reading, not here.
% DISAPPEARANCE_RATIONALE: If the licensed-and-taxed regime vanished overnight, supply would reorganize around unlicensed channels within weeks, excise streams funding treatment programs and general budgets would evaporate, impaired-driving response would lose its regulatory anchor, and product testing and age verification would lapse — the market would rearrange around whichever networks seized distribution first.
% FOUNDING_PROBLEM: Under prior prohibition-style controls the standing problems were violent unregulated supply chains, product of unknown composition, mass criminal processing of possessors, and third-party harms with no funding attached. This arrangement was built to permit adult use while containing third-party harm and assigning its costs to the trade that generates them.
% FOUNDING_PROBLEM_CORROBORATION: Traffic-safety researchers and crash-statistics agencies attest that impaired-driving harms persist; public-health surveillance systems attest that dependence and youth-initiation concerns remain; healthcare cost accounting attests to unpriced residuals. None of these sources sits inside the benefiting industry. Industry associations, by contrast, attest the founding problem is substantially solved — the disagreement between those testimonies is itself the record.
narrative_ontology:disappearance_verdict(substance_control_kernel__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__legalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__legalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_kernel__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__legalization_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__legalization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_kernel__legalization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_kernel__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.52: the referent is the standing legalized-regulated arrangement assessed by this reading's own lights — the reading warrants intervention only for third-party harm prevention and externality-cost capture, so excise rates that exceed demonstrated externality costs, license caps that concentrate rents, and advertising restrictions that outrun the third-party rationale all register as taking beyond warrant, while the intact core of legal access keeps the total moderate rather than high. Suppression 0.38 is authored as a raw structural property, unscaled by power or scope: enforcement machinery is real (age-gate compliance sweeps, drug-recognition enforcement, advertising limits, tax collection) but nothing criminalizes the user as such. Theater_ratio 0.32: product testing and age verification are functional, but a growing share of 'public health' framing covers general-fund revenue use that no health program sees. Accessibility_collapse 0.35: alternatives persist — home cultivation where permitted, gray-market supply, cross-border purchase, abstinence — so understanding the regime does not foreclose exits. Resistance 0.42: taxpayer pushback on rate hikes, industry litigation and lobbying, recovery-community campaigns against density, municipal bans. Measurements run on ONE shared grid (t=0,2,4,...,14) with all three metrics authored at every point: extractiveness ratchets upward as taxes escalate and license caps bite, theater climbs as health framing stretches over general-fund use, and suppression_requirement rises while enforcement machinery matures then plateaus — a static-enforcement tail, not decay. No cyclical oscillation is asserted; the trajectories are monotonic drift.
 *
 * PERSPECTIVAL GAP:
 *   The engine computes divergent per-seat classifications from this structural data, and the divergence is the finding. From the agenda-setter seat the arrangement is a functioning mandate plus a revenue stream it administers; from the trapped third-party seats it is under-compensated risk-bearing — protection promised, delivery partial, compensation diffuse; from the user seat it is liberty restored at the price of a tax; from the gray-operator seat it is a squeeze that criminalizes lightly but excludes commercially. Same-level lateral divergence matters too: road_traffic_participants and adult_substance_users are both diffuse publics of comparable nominal power, differentiated by consent and exit — users opted in and collect the benefit, road participants never consented and cannot leave, which is why their exit atoms (constrained vs trapped) and directionalities must differ despite equal global standing. Coalition check: the third-party victims are numerous but radically diffuse, so coalition power is weak; their protection depends on agenda-setter diligence rather than their own leverage, which is precisely the structural condition that lets under-pricing persist.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. adult_substance_users sit near the beneficiary pole (declared beneficiary, secondary payer — the liberty gain dominates the excise paid). licensed_producers_retailers sit near the beneficiary pole with arbitrage-grade exit damping their effective burden further. state_tax_and_regulatory_agencies are the receipt seat: they collect the transfer and write the rules, so their derived directionality is deep in beneficiary territory. road_traffic_participants and involuntary_secondhand_exposure_populations are declared victims with trapped exit, placing them near the full-target pole — the constraint's costs land on them and no exit modulates it. gray_market_operators bear enforcement costs (payer role) but retain mobility, moderating their effective position. One ambivalence is documented rather than overridden: public_health_agencies are declared beneficiaries (earmark recipients) and the derivation will read them as such, but their mission prefers reduced consumption while their funding requires sustained sales — a mixed position nearer symmetric than pure beneficiary. Authoring a directionality override was considered and rejected because the override surface keys on the power atom, and the only institutional atoms in this story include the agenda-setter seat, which a blanket institutional override would distort. The ambivalence is carried instead by the consumption_funded_public_health_incentive omega.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem had two components: the punitive-disorder component (violent supply, poison product, mass possessor processing) and the harm-containment component (third-party injury with no funding attached). The first is substantially dead — this arrangement exists because it killed it. The second is live: impaired driving, secondhand exposure, and youth initiation persist, and the pricing of their costs remains contested. Founding_problem_status is therefore live, matching disappearance_verdict world_rearranges — no mismatch flag arises, correctly, because the arrangement still does what part of its mandate requires. The classification prevents mislabeling in both directions: calling this a snare would erase the genuine coordination function (tested supply, centralized age verification, harm-targeted enforcement) and the fact that users exited the victim set entirely; calling it a rope would erase the identifiable uncompensated payers and the revenue ratchet visible in the measurement series. If externality pricing converged on true costs and residuals were compensated, the structure would degrade toward pure coordination; if the tax ratchet continues unchecked and gray-zone repression hardens, it drifts toward extraction-dominance. Mandatrophy is not resolved; the arrangement retains live function with a growing extraction layer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is the legalization_reading of the substance_control_kernel; how would the prohibition_reading restructure the same subject matter?',
    'Compile and classify the sibling stories (substance_control_kernel__prohibition_reading, substance_control_kernel__harm_reduction_reading) and compare victim sets, epsilon, and computed types across the kernel family.',
    'Under the prohibition_reading, users re-enter the victim set, epsilon rises sharply, and the computed type shifts toward enforced extraction with the state as punisher rather than revenue collector; cross-reading comparison isolates what each premise contributes to the structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Kernel membership: this story is one reading of substance_control_kernel, not the topic whole.').

omega_variable(
    harm_reduction_sibling_delta,
    'What specifically would the harm_reduction_reading change in the structural surface this story authors?',
    'Author the sibling story and diff the beneficiary/victim sets, enforcement posture, and coordination type against this file.',
    'Users would convert from liberty-holders to patients; the victim set would shift toward untreated dependence and service gaps; taxation would be reframed as treatment funding rather than externality pricing; enforcement_mechanism coordination would give way to clinical infrastructure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_reduction_sibling_delta, conceptual, 'Sibling structural delta: harm_reduction_reading''s expected differences from this reading.').

omega_variable(
    externality_pricing_adequacy,
    'Do current excise rates and compliance costs track the actual social cost of residual third-party harms (impaired driving, secondhand exposure, youth uptake), or do they exceed it?',
    'Actuarial and epidemiological costing studies comparing tax yield per unit consumed against measured externality costs, audited independently of the receiving treasuries.',
    'If taxes systematically exceed externality cost, the excess is revenue-seeking layered onto coordination and the extraction ratchet in the measurement series is confirmed as structural; if below, third parties silently subsidize consumption and the payer seats are worse off than the modeled directionality suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_pricing_adequacy, empirical, 'Whether the tax layer prices externalities or harvests them.').

omega_variable(
    gray_market_persistence_cause,
    'Does the unlicensed market collapse or persist in gray areas, and where it persists, is the driver over-taxation, license scarcity, or enforcement gaps?',
    'Price-gap and seizure data across jurisdictions with differing tax and licensing regimes; natural experiments from tax reductions and license expansions.',
    'Persistence driven by over-taxation confirms the extraction layer is pushing supply outside the licensed channel; persistence despite low taxes indicates enforcement failure rather than price effects, changing which remedy follows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gray_market_persistence_cause, empirical, 'Whether black-market residue indicts the tax layer or the enforcement layer.').

omega_variable(
    consumption_funded_public_health_incentive,
    'Does earmarking substance-tax revenue to public health programs create an institutional incentive to sustain consumption levels the programs nominally aim to reduce?',
    'Budget-dependency analysis: program and agency behavior when consumption falls (market saturation, substitution events) and legislative behavior when revenue declines.',
    'If advocacy for demand reduction softens as revenue falls, the public_health_agencies seat is partly captured by the revenue stream and its true directionality is materially higher than its beneficiary declaration implies — a case where a future per-agent override surface would be warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumption_funded_public_health_incentive, empirical, 'Whether the funded monitor''s incentives align with its monitoring mandate.').

omega_variable(
    youth_uptake_boundary_location,
    'Is youth initiation into substance use a third-party harm inside this reading''s warrant for state intervention, or self-regarding conduct outside it?',
    'Conceptual analysis of the harm principle applied to developmental consent, combined with comparative statutory treatment of age gates, advertising limits, and potency caps.',
    'If youth uptake counts as third-party harm, advertising bans and potency caps are warranted coordination and their suppression cost drops out of the extraction ledger; if not, those measures exceed the reading''s own warrant and raise measured extractiveness accordingly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(youth_uptake_boundary_location, conceptual, 'Where the third-party boundary falls for youth initiation — the reading''s most contested edge.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__legalization_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__legalization_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(subs_tr_t0, observed).
narrative_ontology:measurement(subs_tr_t2, substance_control_kernel__legalization_reading, theater_ratio, 2, 0.22).
narrative_ontology:measurement_basis(subs_tr_t2, observed).
narrative_ontology:measurement(subs_tr_t4, substance_control_kernel__legalization_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement_basis(subs_tr_t4, observed).
narrative_ontology:measurement(subs_tr_t6, substance_control_kernel__legalization_reading, theater_ratio, 6, 0.27).
narrative_ontology:measurement_basis(subs_tr_t6, observed).
narrative_ontology:measurement(subs_tr_t8, substance_control_kernel__legalization_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement_basis(subs_tr_t8, observed).
narrative_ontology:measurement(subs_tr_t10, substance_control_kernel__legalization_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(subs_tr_t10, observed).
narrative_ontology:measurement(subs_tr_t12, substance_control_kernel__legalization_reading, theater_ratio, 12, 0.31).
narrative_ontology:measurement_basis(subs_tr_t12, observed).
narrative_ontology:measurement(subs_tr_t14, substance_control_kernel__legalization_reading, theater_ratio, 14, 0.32).
narrative_ontology:measurement_basis(subs_tr_t14, observed).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__legalization_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(subs_be_t0, observed).
narrative_ontology:measurement(subs_be_t2, substance_control_kernel__legalization_reading, base_extractiveness, 2, 0.43).
narrative_ontology:measurement_basis(subs_be_t2, observed).
narrative_ontology:measurement(subs_be_t4, substance_control_kernel__legalization_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement_basis(subs_be_t4, observed).
narrative_ontology:measurement(subs_be_t6, substance_control_kernel__legalization_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement_basis(subs_be_t6, observed).
narrative_ontology:measurement(subs_be_t8, substance_control_kernel__legalization_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement_basis(subs_be_t8, observed).
narrative_ontology:measurement(subs_be_t10, substance_control_kernel__legalization_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement_basis(subs_be_t10, observed).
narrative_ontology:measurement(subs_be_t12, substance_control_kernel__legalization_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement_basis(subs_be_t12, observed).
narrative_ontology:measurement(subs_be_t14, substance_control_kernel__legalization_reading, base_extractiveness, 14, 0.52).
narrative_ontology:measurement_basis(subs_be_t14, observed).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__legalization_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(subs_su_t0, observed).
narrative_ontology:measurement(subs_su_t2, substance_control_kernel__legalization_reading, suppression_requirement, 2, 0.32).
narrative_ontology:measurement_basis(subs_su_t2, observed).
narrative_ontology:measurement(subs_su_t4, substance_control_kernel__legalization_reading, suppression_requirement, 4, 0.36).
narrative_ontology:measurement_basis(subs_su_t4, observed).
narrative_ontology:measurement(subs_su_t6, substance_control_kernel__legalization_reading, suppression_requirement, 6, 0.38).
narrative_ontology:measurement_basis(subs_su_t6, observed).
narrative_ontology:measurement(subs_su_t8, substance_control_kernel__legalization_reading, suppression_requirement, 8, 0.39).
narrative_ontology:measurement_basis(subs_su_t8, observed).
narrative_ontology:measurement(subs_su_t10, substance_control_kernel__legalization_reading, suppression_requirement, 10, 0.39).
narrative_ontology:measurement_basis(subs_su_t10, observed).
narrative_ontology:measurement(subs_su_t12, substance_control_kernel__legalization_reading, suppression_requirement, 12, 0.38).
narrative_ontology:measurement_basis(subs_su_t12, observed).
narrative_ontology:measurement(subs_su_t14, substance_control_kernel__legalization_reading, suppression_requirement, 14, 0.38).
narrative_ontology:measurement_basis(subs_su_t14, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__legalization_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__legalization_reading, substance_control_kernel__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the substance_control_kernel decomposes into three readings with distinct victim sets and epsilon values — prohibition_reading (users as victims, high epsilon, state as punisher), legalization_reading (this file: users exit the victim set entirely, third parties enter via externalities, moderate epsilon, state as revenue collector), harm_reduction_reading (users as patients, victim set shifts to untreated harm, state as clinician-funder). Each is a separate epsilon-invariant constraint authored in its own file; the edges here let legitimacy pressure and contamination propagate across the family — e.g., a scandal in the licensed market feeds the prohibition_reading's premises, while treatment-outcome evidence feeds the harm_reduction_reading's.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
