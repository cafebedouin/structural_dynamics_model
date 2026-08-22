% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__expected_value_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__expected_value_dominant, []).

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
 *   constraint_id: acceptable_risk_energy__expected_value_dominant
 *   human_readable: Expected-Value-Dominant Acceptable-Risk Standard for Energy Pathways
 *   domain: economic/political/decision-theoretic
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the acceptable_risk_energy kernel:
 *   the expected_value_dominant reading, under which a generation pathway is
 *   acceptable when it minimizes aggregate expected harm, measured in deaths
 *   per terawatt-hour. Under this reading the fossil pathway's toll —
 *   air-pollution mortality and mining deaths — enters the ledger at full
 *   weight, while low-probability catastrophic outcomes such as reactor
 *   accidents are discounted by their estimated probabilities; the
 *   operational consequence is heavy suppression of the fossil pathway and
 *   strong comparative legitimation of nuclear and renewable pathways. The
 *   kernel itself is contested: catastrophic_tail_dominant and
 *   option_value_preserving are sibling readings that instantiate DIFFERENT
 *   constraints with different victim sets and different suppression
 *   directions, and they are authored as separate stories linked through the
 *   network. The claim/metric gap here is deliberate and independent: the
 *   constraint is CLAIMED as tangled_rope because it possesses both a genuine
 *   coordination function (commensuration of incommensurable risks) and
 *   asymmetric extraction (concentrated uncompensated burdens booked against
 *   diffuse gains), while the metrics are authored from the constraint's
 *   observed operation over five decades — the engine computes per-seat
 *   classifications from the structural data and measures any divergence.
 *
 * KEY AGENTS:
 *   - risk_regulatory_agencies: agenda-setter (institutional/constrained) — administers the metric, sets thresholds, defends methodology in court
 *   - cost_benefit_analysis_professionals: primary beneficiary (organized/identity_locked) — careers and authority constituted by the framework
 *   - low_carbon_energy_producers: primary beneficiary (powerful/mobile) — receives redirected permission and capital as fossil scores worst
 *   - downwind_urban_populations: diffuse beneficiary (moderate/mobile) — health gains booked broadly, unorganized
 *   - coal_mining_communities: primary target (powerless/trapped) — deaths counted at full weight, compensation absent, exit locked
 *   - fossil_fuel_workforce: secondary target (organized/constrained) — livelihoods phased out by metric-driven retirement
 *   - nuclear_siting_neighbors: secondary target (moderate/constrained) — carries tail exposure the arithmetic discounts to near zero
 *   - precautionary_principle_advocates: excluded (organized/trapped) — holds the rival decision rule, no procedural seat
 *   - independent_risk_ethicists: analytical observer (analytical/analytical) — sees the full structure, collects nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__expected_value_dominant, 0.52).
domain_priors:suppression_score(acceptable_risk_energy__expected_value_dominant, 0.73).
domain_priors:theater_ratio(acceptable_risk_energy__expected_value_dominant, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, extractiveness, 0.52).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0.73).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__expected_value_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__expected_value_dominant, "Expected-Value-Dominant Acceptable-Risk Standard for Energy Pathways").
narrative_ontology:topic_domain(acceptable_risk_energy__expected_value_dominant, "economic/political/decision-theoretic").

domain_priors:requires_active_enforcement(acceptable_risk_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__expected_value_dominant, 'ab1cc76b-eb7d-4625-88ed-8680ada6f3be').
narrative_ontology:cs_kernel_codification('ab1cc76b-eb7d-4625-88ed-8680ada6f3be', formalized).
narrative_ontology:cs_authority_grounding('ab1cc76b-eb7d-4625-88ed-8680ada6f3be', expertise).
narrative_ontology:cs_interpretation_layer_present('ab1cc76b-eb7d-4625-88ed-8680ada6f3be').
narrative_ontology:cs_reading_relation('ab1cc76b-eb7d-4625-88ed-8680ada6f3be', acceptable_risk_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('ab1cc76b-eb7d-4625-88ed-8680ada6f3be', acceptable_risk_energy__option_value_preserving, influences).
narrative_ontology:cs_axiom('ab1cc76b-eb7d-4625-88ed-8680ada6f3be', foundational, aggregate_expected_harm_is_decisive_criterion).
narrative_ontology:cs_axiom_status(aggregate_expected_harm_is_decisive_criterion, holdable).
narrative_ontology:cs_axiom_grounding('ab1cc76b-eb7d-4625-88ed-8680ada6f3be', aggregate_expected_harm_is_decisive_criterion, instrumental).
narrative_ontology:cs_axiom('ab1cc76b-eb7d-4625-88ed-8680ada6f3be', secondary, mortality_per_twh_commensurability).
narrative_ontology:cs_axiom_status(mortality_per_twh_commensurability, holdable).
narrative_ontology:cs_axiom_grounding('ab1cc76b-eb7d-4625-88ed-8680ada6f3be', mortality_per_twh_commensurability, empirically_contingent).
narrative_ontology:cs_reference_frame('ab1cc76b-eb7d-4625-88ed-8680ada6f3be', aggregate_expected_harm_minimization).
narrative_ontology:cs_drift_state('ab1cc76b-eb7d-4625-88ed-8680ada6f3be', contemporary_deep_uncertainty_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ab1cc76b-eb7d-4625-88ed-8680ada6f3be', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, downwind_urban_populations).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, low_carbon_energy_producers).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, cost_benefit_analysis_professionals).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, energy_consumers).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, coal_mining_communities).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, fossil_fuel_workforce).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, nuclear_siting_neighbors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, energy_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publish the mortality-per-unit-energy comparisons, set the numerical thresholds that determine which generation pathways may receive permits and finance, and defend the methodology in court when challenged. Their statutory mandates, guidance documents, and judicial deference are all built around the expected-harm accounting; adopting a different decision rule would require legislative redefinition of their authority.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, risk_regulatory_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Economists, risk analysts, and consultants whose training, career ladders, publication venues, and consulting markets are constituted by the expected-harm framework. They staff the agencies, peer-review the analyses, and testify in proceedings. Leaving the framework means leaving the profession they were formed in.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, cost_benefit_analysis_professionals, beneficiary,
    organized, biographical, identity_locked, global).

% Nuclear operators and renewable developers whose pathways score lowest on the mortality metric. As fossil generation is ruled out or priced out, permitting priority, investment flows, and subsidy justification move to them. They operate across jurisdictions and can shift capital to wherever the standard is applied most favorably.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, low_carbon_energy_producers, beneficiary,
    powerful, generational, mobile, global).

% Populations whose air-pollution mortality risk falls as high-emission generation retires. Their gains are large in aggregate but diffuse and unorganized — no seat represents them as such, and they experience the benefit simply as ordinary public health rather than as the output of a decision rule.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, downwind_urban_populations, beneficiary,
    moderate, biographical, mobile, regional).

% Households and industry receiving electricity from the optimized generation mix. They gain from reliability and long-run health improvements but absorb transition costs through rates, taxes, and stranded-asset recovery charges. Exiting the grid is not a practical option for any of them.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, energy_consumers, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__expected_value_dominant, energy_consumers, payer).

% Communities whose occupational and environmental mortality is entered into the aggregate calculation at full weight, yet who receive no individual compensation for the deaths the number records. As the pathway they depend on scores worst and is retired, local economies collapse around them; company-town geography, single-industry skills, and depressed property values make relocation largely theoretical.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, coal_mining_communities, payer,
    powerless, generational, trapped, local).

% Miners, plant operators, and oil and gas workers whose livelihoods are phased out as the metric-driven retirement of their sector proceeds. They retain unions and political voice, and have won partial retraining funds, but the organizing principle of the decision rule offers them no seat — they appear in it only as a mortality numerator.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, fossil_fuel_workforce, payer,
    organized, biographical, constrained, continental).

% People living near reactors who carry a low-probability, high-consequence accident exposure that the expected-harm arithmetic discounts to near zero. They cannot insure against the tail outcome at actuarial terms, evacuation is physically difficult, and property values bind them to the location. The decision rule books their exposure as negligible; they experience it as an uncompensated bet placed on their behalf.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, nuclear_siting_neighbors, payer,
    moderate, biographical, constrained, local).

% Public-interest groups, some academics, and affected-community organizers who argue that low-probability catastrophic outcomes and irreversible harms should dominate the ranking regardless of expected value. They have no formal standing inside the analytical dockets where pathway acceptability is decided; their objections enter only as comments appended to completed analyses.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, precautionary_principle_advocates, excluded,
    organized, biographical, trapped, national).

% Philosophers and decision theorists outside the regulatory process who examine whether aggregating harms without distributional weights is a defensible way to impose risk on identifiable people. They publish critiques and alternative frameworks but hold no enforcement seat and collect nothing from the arrangement's operation.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, independent_risk_ethicists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__expected_value_dominant, low_carbon_energy_producers).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__expected_value_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single commensurable currency — expected mortality per unit of energy delivered — for comparing risks that are otherwise incommensurable (particulate disease, mining accidents, radiation exposure), enabling regulators, courts, and investors to rank pathways and allocate finite safety resources without ad hoc judgment in each case.
% TRANSFER_FUNCTION: Moves regulatory permission, investment capital, and market share away from high-mortality-per-TWh pathways (coal, then oil and gas) toward low-mortality pathways (nuclear, renewables); simultaneously concentrates the residual burdens — occupational death, community collapse, discounted accident exposure — on mining communities, fossil workers, and reactor neighbors, while the corresponding health gains are booked diffusely across whole populations.
% ABSENT_VOICES: Precautionary and tail-risk advocates have no procedural seat in the analytical dockets; affected mining communities appear only as aggregated data points, never as parties; reactor neighbors are represented by a probability estimate rather than by themselves. Unanimity inside the framework arises partly because the seats that would object were never admitted to the room.
% DISAPPEARANCE_RATIONALE: If the expected-harm standard vanished overnight, every pending permit, subsidy justification, and comparative risk assessment would lose its deciding criterion; courts would have no deference doctrine to apply; capital allocated by mortality rankings would reprice; and the contest between tail-priority and flexibility-preserving decision rules would move from the margins into the empty center of energy regulation.
% FOUNDING_PROBLEM: Post-war energy expansion confronted regulators with risks that could not be compared on any common scale — how many coal-miner deaths equal one reactor accident? how should a hypothetical meltdown be weighed against documented respiratory mortality? The framework was built to answer this: render all pathway harms commensurable in expected-mortality terms so that acceptable risk could be computed rather than negotiated.
% FOUNDING_PROBLEM_CORROBORATION: National Academies reviews of regulatory risk assessment, appellate court opinions articulating deference to quantified risk analysis, and decision theorists writing from outside the benefiting professions all attest that the comparison problem is real and unresolved. Fossil-sector economists likewise attest the problem is live while disputing the metric — corroboration of the problem does not depend on the framework's own beneficiaries.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__expected_value_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__expected_value_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__expected_value_dominant, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__expected_value_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_energy__expected_value_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_energy__expected_value_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 at interval end): the framework delivers real coordination value, but its operation transfers concentrated, uncompensated burdens — recorded deaths without redress, collapsed local economies, discounted tail exposure — onto identifiable populations while the gains book diffusely. Suppression is high (0.73) and is authored as a raw structural property, unscaled by power or scope: once the metric ranks a pathway unacceptable, permits, finance, and insurance withdraw together, and the fossil pathway's persistence requires active defense against the standard itself. Theater ratio (0.34) tracks Goodhart drift — analyses increasingly produced to survive judicial review rather than to decide, with conclusions sometimes selected before the computation. Accessibility collapse is moderate (0.60): inside institutional arenas the expected-harm frame has largely foreclosed rivals (precautionary analysis appears only as appended comment), but the sibling readings remain live in legislation, scholarship, and public discourse, so alternatives are suppressed rather than extinguished. Resistance is substantial (0.65): fossil-sector lobbying, community litigation, and precautionary counter-scholarship contest the standard continuously. The temporal series run on one shared grid (t=0..50, six points, all three metrics authored at every point) so the engine samples a complete matrix; the trajectories show extraction accumulating and enforcement machinery hardening as the framework matured from a nuclear-debate instrument into the general grammar of energy risk.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (regulatory agencies) the arrangement presents as rational allocation — a hard-won discipline replacing ad hoc fear with arithmetic. From the payer seats the same structure presents as imposed sacrifice: mining communities see their dead entered as data and their towns written off; reactor neighbors see their exposure priced at someone else's probability estimate. The professional seat experiences the framework as identity — competence, method, belonging. These are not disagreements about facts; they are structurally different positions in the same arrangement, and the engine computes the per-seat divergence from the declared power, exit, and role data rather than from this claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: low_carbon_energy_producers (mobile exit, direct gain) sit nearest the subsidized end; cost_benefit_analysis_professionals collect professional authority and are identity_locked into the frame; downwind populations gain diffusely with real but partial mobility. Victim declarations drive high directionality: coal_mining_communities combine full-weight harm with trapped exit, placing them nearest the full-target end; fossil_fuel_workforce are constrained but organized; nuclear_siting_neighbors bear a harm the metric itself discounts, so their structural position is worse than their ledger entry suggests — trapped exit pushes their effective extraction up despite the nominal near-zero pricing. Energy_consumers sit near symmetric: genuine service received, transition costs paid through rates. No directionality overrides were needed; the beneficiary/victim declarations plus exit options reproduce the structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — rendering incommensurable risks commensurable — remains live: every new pathway (SMRs, hydrogen, storage fires) reintroduces the comparison problem, so status=live combined with disappearance_verdict=world_rearranges produces no zombie flag under the mismatch consumer. Mandatrophy protection runs in both directions here: reading the arrangement as pure coordination (rope) would erase the uncompensated concentration of burden that its aggregation licenses; reading it as pure extraction (snare) would erase the genuine commensuration function that makes any rational risk allocation possible at all. The tangled_rope claim preserves both halves. The piton test fails on the cost side: fixing is prohibitive (thousands of rules, precedents, and treaties rest on the metric), but unlike a piton there IS a concentrated capturer — the low-carbon production complex and the professional class both collect concretely — which places the arrangement firmly in hybrid territory rather than inertial decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'Which reading of the acceptable-risk kernel should govern energy risk acceptance — aggregate expected-value minimization (this reading), catastrophic-tail priority, or option-value preservation?',
    'Statutory amendment of agency mandates, shifts in judicial deference doctrine, or treaty-level adoption of an alternative decision rule; each is an institutional selection point at which the kernel''s governing reading is chosen.',
    'Under catastrophic_tail_dominant, nuclear siting neighbors move from discounted to full-weight victims and fossil suppression relaxes; under option_value_preserving, the single-metric structure dissolves entirely and pathway diversity becomes the objective. This story''s victim set, suppression direction, and classification are all conditional on the expected_value_dominant reading holding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Committer structure: this constraint is one reading of the acceptable_risk_energy kernel; sibling readings instantiate different constraints with different victim sets.').

omega_variable(
    distributional_weight_omission,
    'Does aggregating harms without distributional weights systematically misallocate acceptable risk onto geographically concentrated, low-power populations?',
    'Distributional cost-benefit pilots and compensation experiments: run the same pathway decisions with explicit equity weights and compare which communities bear residual burden.',
    'If weighted analysis reverses pathway rankings or reveals uncompensated concentration, measured extraction rises materially and the arrangement drifts toward the snare end of the hybrid range; if rankings are robust to weighting, the coordination reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_weight_omission, empirical, 'Whether the metric''s neutrality conceals regressive risk imposition.').

omega_variable(
    tail_probability_underestimation,
    'Are the accident probabilities used to discount catastrophic nuclear outcomes accurate, or systematically underestimated by fat-tailed failure modes?',
    'Validation of probabilistic risk assessments against operational history — Fukushima demonstrated that station-blackout scenarios rated negligible occurred; systematic comparison of PRA predictions with accumulated reactor-years.',
    'If tails are fatter than modeled, the discounting that keeps nuclear siting neighbors near zero in the ledger is an artifact, their true exposure rises, and the reading''s central trade-off (full-weight fossil deaths versus discounted nuclear deaths) is mispriced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tail_probability_underestimation, empirical, 'Validity of the probability discount applied to catastrophic outcomes.').

omega_variable(
    energy_poverty_hidden_victim_class,
    'Does suppressing the fossil pathway raise energy prices enough to increase mortality among energy-poor households, creating a victim class the mortality-per-TWh ledger does not record?',
    'Ex-post studies of coal-retirement regions combining price trajectories, household energy expenditure shares, and excess winter mortality; natural experiments from jurisdictions that retired fossil capacity at different speeds.',
    'A confirmed energy-poverty mortality channel adds a diffuse victim class invisible to the metric itself, raising effective extraction and complicating the claim that the reading minimizes aggregate harm on its own terms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(energy_poverty_hidden_victim_class, empirical, 'Whether the metric''s own blind spot generates victims it cannot count.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__expected_value_dominant, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(evd_energy_risk_tr_t0, acceptable_risk_energy__expected_value_dominant, theater_ratio, 0, 0.15).
narrative_ontology:measurement(evd_energy_risk_tr_t10, acceptable_risk_energy__expected_value_dominant, theater_ratio, 10, 0.19).
narrative_ontology:measurement(evd_energy_risk_tr_t20, acceptable_risk_energy__expected_value_dominant, theater_ratio, 20, 0.24).
narrative_ontology:measurement(evd_energy_risk_tr_t30, acceptable_risk_energy__expected_value_dominant, theater_ratio, 30, 0.28).
narrative_ontology:measurement(evd_energy_risk_tr_t40, acceptable_risk_energy__expected_value_dominant, theater_ratio, 40, 0.31).
narrative_ontology:measurement(evd_energy_risk_tr_t50, acceptable_risk_energy__expected_value_dominant, theater_ratio, 50, 0.34).

% Extraction over time
narrative_ontology:measurement(evd_energy_risk_be_t0, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(evd_energy_risk_be_t10, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(evd_energy_risk_be_t20, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(evd_energy_risk_be_t30, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 30, 0.46).
narrative_ontology:measurement(evd_energy_risk_be_t40, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 40, 0.49).
narrative_ontology:measurement(evd_energy_risk_be_t50, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 50, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(evd_energy_risk_su_t0, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(evd_energy_risk_su_t10, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(evd_energy_risk_su_t20, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(evd_energy_risk_su_t30, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 30, 0.64).
narrative_ontology:measurement(evd_energy_risk_su_t40, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(evd_energy_risk_su_t50, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 50, 0.73).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__expected_value_dominant, resource_allocation).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy__option_value_preserving).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'acceptable risk in energy policy.' The label conflates three structurally distinct decision rules that share a kernel but differ in aggregation principle, victim sets, and suppression direction: expected_value_dominant (this story — full-weight fossil deaths, probability-discounted catastrophes, fossil pathway suppressed), catastrophic_tail_dominant (tail severity ordering — reactor neighbors elevated, fossil suppression relaxed), and option_value_preserving (pathway diversity under deep uncertainty — no single metric governs). Per the epsilon-invariance principle these are separate constraints with separate epsilons, not one constraint viewed from angles. This reading is the upstream member: its institutional entrenchment shapes the operating environment (docket standing, funding, judicial deference) in which the sibling readings are argued, which is why the edges run from this story to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
