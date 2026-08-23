% ============================================================================
% CONSTRAINT STORY: climate_response_action__degrowth_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__degrowth_transformation, []).

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
 *   constraint_id: climate_response_action__degrowth_transformation
 *   human_readable: Growth-Organized Climate-Economic Arrangement (Degrowth Reading)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This file instantiates the degrowth_transformation reading of the
 *   climate_response_action kernel (see commentary.kernel_context). The
 *   standing arrangement under contest — and the fixed ε referent for this
 *   story — is the global growth-organized climate-economic regime: an
 *   economy whose productive, financial, and political machinery is keyed to
 *   GDP growth, paired with a climate-response posture that relies on
 *   technological substitution (renewables deployment, efficiency, offset
 *   markets, prospective carbon removal) while leaving the growth organizing
 *   principle intact. The degrowth reading assesses that arrangement as
 *   deeply and constitutively extractive: it transfers ecological space and
 *   material surplus from Global South populations and from common sinks to
 *   Northern high-consumption classes and capital returns, and it transfers
 *   the costs of climate destabilization to future generations and to the
 *   populations least responsible for cumulative emissions. Its climate
 *   governance layer performs decarbonization — pledges, offsets, net-zero
 *   targets dated beyond incumbent horizons — while aggregate throughput
 *   continues to rise. The reading's demanded remedy (universal basic
 *   services, working-time reduction, democratic firm ownership,
 *   North-to-South redistribution, minimized reliance on speculative removal)
 *   is the reading's endorsed alternative and is deliberately NOT the
 *   referent of ε; it enters this story as the remedy whose necessity the
 *   measured transfer structure grounds, and as committer content routed to
 *   omega variables per the kernel-reading rules. KEY AGENTS (by structural
 *   relationship): - wealthy_north_consumer_classes: principal beneficiary
 *   (powerful/arbitrage) — consumption sustained by global throughput; can
 *   purchase insulation from impacts - fossil_industrial_capital:
 *   concentrated beneficiary and enforcement financier
 *   (institutional/arbitrage) — owns the arrangement's material base;
 *   captures the largest rents - growth_conditioned_political_establishments:
 *   agenda setter (institutional/identity_locked) — administers legitimacy
 *   tied to growth delivery; cannot exit the growth frame without dissolving
 *   their own mandate - global_south_development_populations: principal
 *   target (organized/constrained) — bear ecological debt, unequal exchange,
 *   and development-space foreclosure - future_generations: silent target
 *   (powerless/trapped) — inherit destabilized climate; present only through
 *   proxies - climate_vulnerable_low_income_communities: target
 *   (powerless/trapped) — bear impacts with least recourse -
 *   degrowth_climate_justice_movements: excluded challenger
 *   (organized/constrained) — articulate the alternative outside official
 *   channels - ipcc_and_independent_researchers: analytical observer
 *   (institutional/analytical) — document the physical record the
 *   arrangement's governance narrates selectively Family note: this story
 *   shares its ε referent with the sibling readings' stories
 *   (mitigation_priority, adaptation_priority); the three files differ in
 *   reading-indexed ε, victim identification, and demanded remedy. See
 *   network.dual_formulation_note.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__degrowth_transformation, 0.86).
domain_priors:suppression_score(climate_response_action__degrowth_transformation, 0.58).
domain_priors:theater_ratio(climate_response_action__degrowth_transformation, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, extractiveness, 0.86).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__degrowth_transformation, tangled_rope).
narrative_ontology:human_readable(climate_response_action__degrowth_transformation, "Growth-Organized Climate-Economic Arrangement (Degrowth Reading)").
narrative_ontology:topic_domain(climate_response_action__degrowth_transformation, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_action__degrowth_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__degrowth_transformation, '92389a17-5cde-4016-a91c-acd2ea730b97').
narrative_ontology:cs_kernel_codification('92389a17-5cde-4016-a91c-acd2ea730b97', formalized).
narrative_ontology:cs_authority_grounding('92389a17-5cde-4016-a91c-acd2ea730b97', distributed).
narrative_ontology:cs_reading_relation('92389a17-5cde-4016-a91c-acd2ea730b97', climate_response_action__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('92389a17-5cde-4016-a91c-acd2ea730b97', climate_response_action__adaptation_priority, coexists_with).
narrative_ontology:cs_axiom('92389a17-5cde-4016-a91c-acd2ea730b97', foundational, growth_organizing_principle_rejection).
narrative_ontology:cs_axiom_status(growth_organizing_principle_rejection, holdable).
narrative_ontology:cs_axiom_grounding('92389a17-5cde-4016-a91c-acd2ea730b97', growth_organizing_principle_rejection, empirically_contingent).
narrative_ontology:cs_axiom('92389a17-5cde-4016-a91c-acd2ea730b97', foundational, sufficiency_equity_primacy).
narrative_ontology:cs_axiom_status(sufficiency_equity_primacy, holdable).
narrative_ontology:cs_axiom_grounding('92389a17-5cde-4016-a91c-acd2ea730b97', sufficiency_equity_primacy, deontological).
narrative_ontology:cs_reference_frame('92389a17-5cde-4016-a91c-acd2ea730b97', sufficiency_organized_post_growth_economy).
narrative_ontology:cs_drift_state('92389a17-5cde-4016-a91c-acd2ea730b97', contemporary_growth_locked_policy_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('92389a17-5cde-4016-a91c-acd2ea730b97', '').
narrative_ontology:cs_kernel_id(climate_response_action__degrowth_transformation, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, wealthy_north_consumer_classes).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, fossil_industrial_capital).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, growth_conditioned_political_establishments).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, global_south_development_populations).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, future_generations).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, climate_vulnerable_low_income_communities).
narrative_ontology:constraint_vindicates(climate_response_action__degrowth_transformation, gdp_growth_imperative_doctrine).
narrative_ontology:constraint_vindicates(climate_response_action__degrowth_transformation, green_growth_substitution_hypothesis).
narrative_ontology:constraint_vindicates(climate_response_action__degrowth_transformation, absolute_decoupling_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the world's highest per-capita consumption, sustained by global supply chains and by access to the atmosphere's remaining sink capacity. Their living standards are the arrangement's headline product, and their savings are invested in the growth process itself. When climate impacts arrive, they can relocate, insure, cool, and import their way around the worst of it — options unavailable to most of the world.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, wealthy_north_consumer_classes, beneficiary,
    powerful, biographical, arbitrage, global).

% Owns and finances the material base of the energy and industrial system: reserves, pipelines, plants, and the lobbying and research apparatus that defends them. Receives the largest concentrated returns from continued throughput and funds the political defense of business-as-usual, while hedging by diversifying into renewables where returns allow.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, fossil_industrial_capital, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__degrowth_transformation, fossil_industrial_capital, agenda_setter).

% Govern states whose creditworthiness, employment figures, and electoral legitimacy are all indexed to GDP growth. Their policy imagination runs through growth-compatible instruments — carbon prices, subsidies, distant targets — because promising anything else reads as economic surrender. Leaving the growth frame would dissolve the mandate they were elected and funded to deliver.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, growth_conditioned_political_establishments, agenda_setter,
    institutional, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(climate_response_action__degrowth_transformation, growth_conditioned_political_establishments, beneficiary).

% Majority-world populations whose development aspirations run up against atmospheric and ecological limits already consumed elsewhere. They export raw materials at declining terms of trade, host resource-frontier regions, and face climate impacts they did little to cause, while negotiation agendas are set by the large emitters and creditors. Collective diplomatic organization — bargaining blocs, loss-and-damage coalitions — is their main lever; exiting the world economy is not an option.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, global_south_development_populations, payer,
    organized, generational, constrained, global).

% People not yet born who will inherit the accumulated carbon stock, depleted sinks, and locked-in warming that the arrangement's present beneficiaries decline to price. They act in the world only through proxies — litigants, ombudspersons, constitutional clauses — and cannot exit, consent, or renegotiate. Every year of deferred reduction is a cost transferred to them.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, future_generations, payer,
    powerless, civilizational, trapped, global).

% Low-income communities in flood plains, drought belts, deltas, and small island states who bear the earliest and hardest impacts with the fewest buffers. Their homes, crops, and water security are priced as externalities elsewhere. Migration is constrained by borders and poverty; adaptation finance arrives late and small relative to need.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, climate_vulnerable_low_income_communities, payer,
    powerless, biographical, trapped, regional).

% Movements and scholars articulating sufficiency, redistribution, and post-growth economics. They operate outside official climate-policy channels — which are structured around growth-compatible instruments — through publications, protests, municipal experiments, and litigation support. Their proposals are heard as radicalism rather than policy; their leverage is agenda-disruption, not administration.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, degrowth_climate_justice_movements, excluded,
    organized, generational, constrained, global).

% Assessment bodies and independent researchers who compile the physical record — emissions, concentrations, impacts, decoupling rates — that the arrangement's governance narrates selectively. They hold no administrative power; their influence runs through credibility, and their findings are routinely acknowledged and then set aside in policy design.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, ipcc_and_independent_researchers, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_action__degrowth_transformation, fossil_industrial_capital).
narrative_ontology:fixing_cost_class(climate_response_action__degrowth_transformation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global production, exchange, and welfare provision through growth-conditioned markets, monetary systems, and states: investment is disciplined by growth expectations, public goods are financed from growth-derived revenues, and employment is provisioned through expanding output. Stated without evaluation: this is the coordination the arrangement actually performs.
% TRANSFER_FUNCTION: Moves material surplus and ecological space (carbon sinks, land, freshwater, mineral throughput) from Global South producers and from common pools to Northern high-consumption classes and capital returns; moves the costs of climate destabilization forward onto future generations and outward onto the populations least responsible for cumulative emissions.
% ABSENT_VOICES: Future generations would object loudest and are present only through proxy advocates. Global South delegations hold formal seats in climate negotiations but face agenda control by major emitters and creditor institutions. Degrowth and post-growth economists sit outside official policy channels, which are pre-structured around growth-compatible instruments (carbon pricing, offset markets, technology subsidies); their exclusion is the operational content of the excluded stakeholder seat.
% DISAPPEARANCE_RATIONALE: If the growth-organized arrangement vanished overnight, global production, finance, employment, food distribution, and state legitimacy would collapse together — the arrangement is the load-bearing structure of the contemporary world economy, whatever one concludes about its transfer structure. The degrowth reading's own program presupposes a managed, decades-long exit precisely because unmanaged disappearance is catastrophic.
% FOUNDING_PROBLEM: Post-war reconstruction, mass unemployment, and the coordination of industrial economies: GDP growth was adopted as the measurable proof of success, and the distributive bargain — rising aggregate output legitimating inequality while lifting incomes — became the arrangement's founding settlement.
% FOUNDING_PROBLEM_CORROBORATION: Split attestation from outside the beneficiary set: development economics and historical income statistics corroborate that the founding prosperity problem was real and partially solved; IPCC physical-science assessments and Global South trade-and-environment scholarship corroborate that the solution's mechanism now generates ecological destabilization and development-space foreclosure. No source outside the arrangement's beneficiaries and managers attests that growth remains necessary for the prosperity function — that necessity claim is self-attested, which is itself signal.
narrative_ontology:disappearance_verdict(climate_response_action__degrowth_transformation, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_action__degrowth_transformation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__degrowth_transformation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_action__degrowth_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_action__degrowth_transformation, 0.86, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__degrowth_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_action__degrowth_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_action__degrowth_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.86) because the degrowth reading holds the standing arrangement's transfer structure to be constitutive rather than incidental: growth is the mechanism through which ecological space moves from South to North and costs move from present to future, and the carbon stock compounds annually, so the reading's index of the arrangement drifts upward across the interval (0.78 to 0.86). Suppression (0.58) is structural discipline rather than overt coercion: growth-conditioned finance, electoral incentive structures, and narrative management marginalize post-growth alternatives without eradicating them — hence accessibility_collapse stays low (0.32): the alternatives remain visible and articulated, unlike a natural law whose alternatives vanish on understanding. Theater_ratio (0.55) reflects the pledge-and-offset architecture: net-zero targets dated beyond incumbents' horizons, offset markets performing reduction, throughput rising underneath — Goodhart drift in which targets substitute for reduction. Resistance (0.60) is real and growing — climate justice mobilization, Global South bargaining blocs, strategic litigation — and is met mainly with co-optation rather than concession. The claimed type, tangled_rope, is stated independently of these metrics: the arrangement does perform genuine coordination (complex production, exchange, welfare provision), and the honest degrowth-seat description is hybrid, sitting near the snare boundary. Where the engine's per-seat computations diverge from the claim, that divergence is the measurement. Victim-coalition potential is acknowledged: organized Global South blocs and climate-justice coalitions are the arrangement's most credible internal challenge, which is why its suppression machinery concentrates on agenda control rather than street control. The measurement series run on one shared time grid (1992, 2000, 2009, 2015, 2020, 2025) so every tracked metric is authored at every examined point; suppression_requirement is tracked because this story specifically traces enforcement hardening (creditor conditionality, protest policing, narrative management) across the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the wealthy_north_consumer_classes seat the arrangement computes as a prosperity machine with a greening option — coordination first, costs hypothetical and deferrable. From the global_south_development_populations seat the same structure computes as enforced dispossession — a system that consumed the sink space first and now lectures on responsibility. From the growth_conditioned_political_establishments seat it is simply reality: identity-locked administrators cannot conceive the frame's replacement without dissolving themselves. Future generations hold no seat at all; their interests appear only through proxy advocates, which is itself a structural asymmetry no metric fully registers. The engine computes these divergent per-seat classifications from the power, exit, and role data; the authored tangled_rope claim does not adjudicate among them — the divergence between the beneficiary-side rope-flavored computation and the victim-side snare-flavored computation is precisely the datum this story exists to register.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarations map to d as follows. The three beneficiary seats (wealthy_north_consumer_classes, fossil_industrial_capital, and growth_conditioned_political_establishments via its secondary beneficiary position) derive low d — the arrangement subsidizes them, and arbitrage-grade exit (relocation, portfolio diversification, impact hedging) pushes them toward the beneficiary pole. The three victim seats derive high d: global_south_development_populations (constrained exit amplifies), climate_vulnerable_low_income_communities (trapped), and future_generations (trapped with no seat at all — the maximal-target end, weighted per the future_generations_seat_warrant omega). The arrangement's spatial scope is global, which raises verification difficulty and modestly amplifies effective extraction for target seats. Suppression is authored as a raw structural property and is deliberately NOT scaled — only extractiveness scales with directionality and scope in the engine's computation. No directionality overrides were needed: the beneficiary/victim declarations plus exit options already place every seat correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical errors. Reading the standing arrangement as pure predation erases its real coordination achievements — complex production, exchange, and welfare provision that billions depend on daily — and licenses remedies that shatter the provisioning system along with the transfer structure. Reading it as mere coordination needing optimization erases the constitutive transfer the degrowth reading documents. Tangled_rope holds both truths and relocates the analytic question from presence-or-absence of function to separability of function from the growth imperative (omega: coordination_extraction_separability). On mandatrophy proper: the arrangement's founding problem (post-war reconstruction and mass prosperity) is contested rather than dead — prosperity delivery continues unevenly while the growth mechanism now generates the problem it was built to solve — so no mandatrophy resolution is declared. The R5 mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges and finds no zombie signature: the arrangement is overrunning its mandate, not theatrically performing a dead one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is the degrowth_transformation reading of the climate_response_action kernel; what structural differences would instantiate if a sibling reading (mitigation_priority, adaptation_priority) governed instead?',
    'Cross-read the three sibling stories'' victim identifications, burden incidence, and demanded remedies; the disagreement is located in the growth-compatibility premise and in the locus of response (prevention versus protection versus transformation).',
    'Under mitigation_priority the growth organizing principle stands, burden shifts toward future generations and speculative carbon removal, and this story''s victim set narrows to under-mitigation losses; under adaptation_priority temperature rise is accepted as inevitable and burden falls on vulnerable populations, dissolving this reading''s prevention claim entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one kernel, three readings; this file authors the degrowth seat.').

omega_variable(
    absolute_decoupling_sufficiency,
    'Can GDP growth be absolutely decoupled from resource throughput and emissions fast enough to stabilize the climate without rejecting growth as the organizing principle?',
    'Consumption-based material-flow and emissions accounting at the required rates (sustained multi-percent annual absolute decoupling, globally), checked against the historical record compiled in environmental-accounting assessments.',
    'Demonstrated sufficient decoupling would weaken this reading''s necessity axiom and strengthen mitigation_priority; continued failure would convert the degrowth claim from contested to structurally forced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolute_decoupling_sufficiency, empirical, 'The empirical hinge beneath the reading''s rejection of growth-compatibility.').

omega_variable(
    political_feasibility_lockin,
    'Is the degrowth program''s political infeasibility a contingent barrier that mobilization could overcome, or a structural property of the standing arrangement such that the arrangement forecloses its own remedy?',
    'Comparative tracking of jurisdictions attempting post-growth policies (working-time reduction, universal basic services pilots) under growth-conditioned finance and electoral competition; historical analysis of transformative policy windows.',
    'If lock-in is structural, the standing arrangement drifts toward the snare pole (self-perpetuating transfer with suppressed exits); if contingent, the tangled_rope claim stands with live reform pathways.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_feasibility_lockin, empirical, 'Whether feasibility barriers are contingent or constitutive.').

omega_variable(
    coordination_extraction_separability,
    'Are the standing arrangement''s genuine coordination functions (complex production, exchange, welfare provision) separable from its growth imperative, or does coordination degrade without growth?',
    'Sectoral and historical analysis: steady-state modeling, no-growth episodes, and provisioning-system redesign studies (universal basic services, working-time reduction) tested for functional retention.',
    'Separable functions confirm the tangled_rope reading and locate the fix in reorganization; inseparability would indicate the transfer structure is constitutive of the coordination itself, pushing toward the snare pole.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether function and extraction can be pried apart.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the marginalization of post-growth positions structural (funding, electoral incentives, finance discipline, agenda control) or internalized (policymakers'' and economists'' growth-realism as professional identity)?',
    'Post-exit trajectory analysis: track officials and economists who leave growth-conditioned institutions; if growth-realism persists after the incentive structure is removed, the suppression is partly internalized.',
    'If substantially internalized, removing formal barriers would not open policy space and the scalar suppression understates effective suppression; the arrangement''s suppressive force travels inside its targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Structural versus internalized suppression of the alternative.').

omega_variable(
    future_generations_seat_warrant,
    'Future generations are declared victims but hold no seat and no agency; with what warrant does any proxy speak their directionality, and does their victim declaration carry classification weight?',
    'Institutional-design analysis of representation devices (ombudspersons for future generations, constitutional climate clauses, litigation standing in Urgenda-class cases) and their uptake.',
    'Strong warranted proxies raise the effective d of the intergenerational victim declaration; weak proxies discount it, shifting computed extraction toward the presently-living victim seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_generations_seat_warrant, conceptual, 'Warrant for the intergenerational victim seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__degrowth_transformation, 1992, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1992, climate_response_action__degrowth_transformation, theater_ratio, 1992, 0.3).
narrative_ontology:measurement_basis(clim_tr_t1992, observed).
narrative_ontology:measurement(clim_tr_t2000, climate_response_action__degrowth_transformation, theater_ratio, 2000, 0.34).
narrative_ontology:measurement_basis(clim_tr_t2000, observed).
narrative_ontology:measurement(clim_tr_t2009, climate_response_action__degrowth_transformation, theater_ratio, 2009, 0.42).
narrative_ontology:measurement_basis(clim_tr_t2009, observed).
narrative_ontology:measurement(clim_tr_t2015, climate_response_action__degrowth_transformation, theater_ratio, 2015, 0.48).
narrative_ontology:measurement_basis(clim_tr_t2015, observed).
narrative_ontology:measurement(clim_tr_t2020, climate_response_action__degrowth_transformation, theater_ratio, 2020, 0.52).
narrative_ontology:measurement_basis(clim_tr_t2020, observed).
narrative_ontology:measurement(clim_tr_t2025, climate_response_action__degrowth_transformation, theater_ratio, 2025, 0.55).
narrative_ontology:measurement_basis(clim_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(clim_be_t1992, climate_response_action__degrowth_transformation, base_extractiveness, 1992, 0.78).
narrative_ontology:measurement_basis(clim_be_t1992, observed).
narrative_ontology:measurement(clim_be_t2000, climate_response_action__degrowth_transformation, base_extractiveness, 2000, 0.8).
narrative_ontology:measurement_basis(clim_be_t2000, observed).
narrative_ontology:measurement(clim_be_t2009, climate_response_action__degrowth_transformation, base_extractiveness, 2009, 0.82).
narrative_ontology:measurement_basis(clim_be_t2009, observed).
narrative_ontology:measurement(clim_be_t2015, climate_response_action__degrowth_transformation, base_extractiveness, 2015, 0.84).
narrative_ontology:measurement_basis(clim_be_t2015, observed).
narrative_ontology:measurement(clim_be_t2020, climate_response_action__degrowth_transformation, base_extractiveness, 2020, 0.85).
narrative_ontology:measurement_basis(clim_be_t2020, observed).
narrative_ontology:measurement(clim_be_t2025, climate_response_action__degrowth_transformation, base_extractiveness, 2025, 0.86).
narrative_ontology:measurement_basis(clim_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1992, climate_response_action__degrowth_transformation, suppression_requirement, 1992, 0.4).
narrative_ontology:measurement_basis(clim_su_t1992, observed).
narrative_ontology:measurement(clim_su_t2000, climate_response_action__degrowth_transformation, suppression_requirement, 2000, 0.44).
narrative_ontology:measurement_basis(clim_su_t2000, observed).
narrative_ontology:measurement(clim_su_t2009, climate_response_action__degrowth_transformation, suppression_requirement, 2009, 0.5).
narrative_ontology:measurement_basis(clim_su_t2009, observed).
narrative_ontology:measurement(clim_su_t2015, climate_response_action__degrowth_transformation, suppression_requirement, 2015, 0.54).
narrative_ontology:measurement_basis(clim_su_t2015, observed).
narrative_ontology:measurement(clim_su_t2020, climate_response_action__degrowth_transformation, suppression_requirement, 2020, 0.56).
narrative_ontology:measurement_basis(clim_su_t2020, observed).
narrative_ontology:measurement(clim_su_t2025, climate_response_action__degrowth_transformation, suppression_requirement, 2025, 0.58).
narrative_ontology:measurement_basis(clim_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__degrowth_transformation, resource_allocation).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, climate_response_action__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, climate_response_action__adaptation_priority).

% DUAL FORMULATION NOTE:
% The colloquial label 'climate response' decomposes into three structurally distinct constraints sharing one ε referent (the standing growth-organized climate-economic arrangement) and differing in reading-indexed ε, victim identification, and demanded remedy: mitigation_priority (upstream, mainstream, highest empirical confidence — its growth-compatibility premise is what this reading contests), degrowth_transformation (this file — rejects the growth premise; its foundational axiom contradicts mitigation's within any single framework, hence the forecloses edge), and adaptation_priority (an orthogonal protection locus that coexists with this reading in combined movement frameworks). Upstream influences downstream: mitigation_priority's institutional dominance shapes the resource and legitimacy conditions under which this reading is marginalized.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
