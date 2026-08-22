% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__degrowth_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: climate_response_imperative__degrowth_reading
 *   human_readable: Degrowth Requirement on Global North Economies (Climate Response Imperative, Degrowth Reading)
 *   domain: climate policy/political economy/intergenerational justice
 *
 * SUMMARY:
 *   This file instantiates the degrowth_reading of the
 *   climate_response_imperative kernel as a single epsilon-invariant
 *   constraint: the requirement that climate response run through structural
 *   transformation of Global North economies — reduced consumption,
 *   redistribution, post-growth institutions — enabling both mitigation and
 *   adaptation. Per the kernel-reading rules, the sibling readings
 *   (mitigation_priority_reading, adaptation_priority_reading) are other
 *   constraints in other files; the contest is not adjudicated here and
 *   epsilon is not hedged across readings. The standing arrangement under
 *   contest, assessed by this reading's own lights, is the growth-compatible
 *   climate response regime (green-growth mitigation, market mechanisms,
 *   CDR-reliant net-zero targets) that the reading holds physically
 *   insufficient; the epsilon authored here is for the transformation
 *   requirement's own operation on those it governs — the real, concentrated,
 *   near-term costs it imposes on present-day Northern populations — not for
 *   the post-growth steady state the reading endorses. The claim and the
 *   metrics are independent authored facts: the constraint is claimed as
 *   tangled_rope because it genuinely coordinates (demand reduction is the
 *   only physically available substitute for unproven carbon removal;
 *   redistribution is the operative adaptation-finance mechanism) while
 *   asymmetrically extracting (present-day Northern populations bear costs
 *   whose benefit stream accrues to future generations and the Global South,
 *   who bear none), under enforcement that must run against revealed Northern
 *   preference. KEY AGENTS (by structural relationship): -
 *   affluent_northern_households: primary target (powerful/arbitrage) — bears
 *   consumption ceilings, wealth levies, and asset stranding; partially
 *   evades via capital flight and exemption purchasing -
 *   precarious_northern_households: dual-positioned target and beneficiary
 *   (organized/trapped) — bears working-time and consumption adjustments,
 *   receives domestic redistribution; holds the electoral veto -
 *   future_generations: principal beneficiary (powerless/trapped) — receives
 *   the benefit stream at zero cost; structurally unable to consent or object
 *   - global_south_populations: beneficiary (moderate/trapped) — receives
 *   adaptation finance and atmospheric space; spared damages the
 *   transformation avoids - degrowth_policy_community: agenda-setter
 *   (moderate/identity_locked) — authors the transformation blueprints;
 *   professional identity fused with the project -
 *   carbon_intensive_industries: institutional payer
 *   (institutional/arbitrage) — faces managed wind-down; responds with
 *   capture, relocation, rebranding - climate_science_community: analytical
 *   observer (institutional/analytical) — assesses pathway sufficiency;
 *   underwrites the necessity claim without enforcement power -
 *   climate_vulnerable_nation_governments: excluded voice
 *   (moderate/constrained) — formal seats, no agenda power over Northern
 *   domestic transformation
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, 0.65).
domain_priors:suppression_score(climate_response_imperative__degrowth_reading, 0.7).
domain_priors:theater_ratio(climate_response_imperative__degrowth_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__degrowth_reading, "Degrowth Requirement on Global North Economies (Climate Response Imperative, Degrowth Reading)").
narrative_ontology:topic_domain(climate_response_imperative__degrowth_reading, "climate policy/political economy/intergenerational justice").

domain_priors:requires_active_enforcement(climate_response_imperative__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__degrowth_reading, '34f57496-2ab9-45ba-8f80-639f661b9ac1').
narrative_ontology:cs_kernel_codification('34f57496-2ab9-45ba-8f80-639f661b9ac1', distributed).
narrative_ontology:cs_authority_grounding('34f57496-2ab9-45ba-8f80-639f661b9ac1', distributed).
narrative_ontology:cs_reading_relation('34f57496-2ab9-45ba-8f80-639f661b9ac1', climate_response_imperative__mitigation_priority_reading, forecloses).
narrative_ontology:cs_reading_relation('34f57496-2ab9-45ba-8f80-639f661b9ac1', climate_response_imperative__adaptation_priority_reading, influences).
narrative_ontology:cs_axiom('34f57496-2ab9-45ba-8f80-639f661b9ac1', foundational, growth_compatible_mitigation_insufficient).
narrative_ontology:cs_axiom_status(growth_compatible_mitigation_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('34f57496-2ab9-45ba-8f80-639f661b9ac1', growth_compatible_mitigation_insufficient, empirically_contingent).
narrative_ontology:cs_axiom('34f57496-2ab9-45ba-8f80-639f661b9ac1', foundational, burden_allocation_tracks_historical_responsibility).
narrative_ontology:cs_axiom_status(burden_allocation_tracks_historical_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('34f57496-2ab9-45ba-8f80-639f661b9ac1', burden_allocation_tracks_historical_responsibility, deontological).
narrative_ontology:cs_reference_frame('34f57496-2ab9-45ba-8f80-639f661b9ac1', post_growth_sufficiency_within_planetary_boundaries).
narrative_ontology:cs_drift_state('34f57496-2ab9-45ba-8f80-639f661b9ac1', post_sr15_scenario_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('34f57496-2ab9-45ba-8f80-639f661b9ac1', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__degrowth_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, global_south_populations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, precarious_northern_households).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, affluent_northern_households).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, precarious_northern_households).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, carbon_intensive_industries).
narrative_ontology:constraint_vindicates(climate_response_imperative__degrowth_reading, decoupling_insufficiency_thesis).
narrative_ontology:constraint_vindicates(climate_response_imperative__degrowth_reading, sufficiency_principle).
narrative_ontology:constraint_vindicates(climate_response_imperative__degrowth_reading, common_but_differentiated_responsibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Top income and wealth deciles across North America, Europe, and East Asia. Their consumption patterns and asset portfolios carry the largest per-capita footprints in the world economy. Under the transformation they face consumption ceilings, wealth levies, and the stranding of carbon-intensive holdings. From where they stand, exit means relocating residence or assets to permissive jurisdictions, purchasing exemptions where enforcement is porous, and funding political opposition to the requirement.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, affluent_northern_households, payer,
    powerful, biographical, arbitrage, continental).

% Working- and middle-class households whose employment, pensions, and public services are tied to economic growth. They bear working-time restructuring, consumption adjustments, and transition risk, and they receive domestic redistribution, energy-cost protection, and expanded public provision in return. They cannot leave their national economies, and their main lever is the electoral veto they hold over any government proposing the requirement.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, precarious_northern_households, payer,
    organized, biographical, trapped, continental).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__degrowth_reading, precarious_northern_households, beneficiary).

% People not yet born who will inherit whatever climate and institutional legacy the present creates. They receive the mitigation and adaptation benefit stream at no cost to themselves and cannot consent, object, negotiate, or exit. Every design choice in the requirement — how fast consumption falls, how much is redistributed, whether carbon removal is relied upon — lands on them without their voice.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% The majority of humanity, concentrated in regions bearing the largest climate damages under current trajectories. Under the requirement they receive adaptation finance, technology transfer, and a larger share of atmospheric space, and they are spared damages the transformation avoids. Their leverage over Northern domestic policy is limited; migration pressure under climate stress is the constrained exit available to their worst-affected members.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_south_populations, beneficiary,
    moderate, generational, trapped, global).

% Post-growth economists, sufficiency researchers, transition designers, and movement organizers who author the transformation blueprints: working-time schemes, rationing architectures, redistribution mechanisms, citizen-assembly designs. They set the intellectual agenda of the requirement but hold no state power. Their careers, citation networks, and worldview are fused with the project; abandoning it would mean re-founding their professional and ideological lives.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, degrowth_policy_community, agenda_setter,
    moderate, generational, identity_locked, global).

% Fossil fuel producers, airlines, automotive manufacturers, and cement and steel incumbents whose business models the transformation winds down on a schedule they do not set. They face stranded assets and managed contraction. Their responses are capital relocation, regulatory capture, demand-side lobbying, and rebranding; their participation in designing the requirement is deliberately excluded by its premise.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, carbon_intensive_industries, payer,
    institutional, biographical, arbitrage, global).

% IPCC assessment authors and Earth-system scientists who quantify emission budgets, decoupling rates, and carbon-removal scalability. Their findings underwrite the necessity claim the requirement rests on, but they hold no enforcement power and take no side in the distributive design. Their assessments are the reference against which the requirement's premises are checked.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, climate_science_community, observer,
    institutional, civilizational, analytical, global).

% Governments of small island states and Sahelian and deltaic nations on the front line of climate damage. They hold formal seats in climate negotiations and would press for faster, larger redistribution and harder Northern timelines, but they hold no agenda power over Northern domestic economic policy. Their consent is rhetorically invoked in the requirement's design literature while their voice is structurally absent from the forums where Northern transformation would actually be decided.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, climate_vulnerable_nation_governments, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__degrowth_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_response_imperative__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces aggregate material throughput and consumption in the region with the highest per-capita emissions, where demand reduction substitutes for unproven carbon removal, and pools redistributed resources to finance adaptation and domestic transition cohesion — solving the mitigation collective-action problem through sufficiency rather than speculative supply-side technology.
% TRANSFER_FUNCTION: Moves consumption capacity, working time, and wealth from present-day Global North households (weighted toward affluent deciles and carbon-intensive asset holders) toward Global South adaptation finance, domestic public provision, and atmospheric stabilization; the benefit stream accrues to future generations and the Global South, who bear none of the transfer's cost.
% ABSENT_VOICES: Future generations are structurally absent — they cannot object or consent. Climate-vulnerable nation governments hold formal seats but no agenda power over Northern domestic policy. Growth-dependent workers and carbon-intensive industries would object but are excluded by the framework's premise, which plans their wind-down without their participation. Present-day Northern electorates retain veto power, which the constraint's institutional design (insulated planning bodies, citizen assemblies) deliberately routes around.
% DISAPPEARANCE_RATIONALE: If the degrowth requirement vanished, Northern climate policy reverts to growth-compatible mitigation: consumption and emissions trajectories continue, CDR reliance expands to cover the gap, adaptation finance stays at current inadequate levels, and the deferred damages land on the Global South and future generations — the arrangement each party's position depends on would be replaced by the mitigation-priority status quo.
% FOUNDING_PROBLEM: The mitigation gap recognized after IPCC SR15 (2018): Paris temperature targets cannot be met by growth-compatible pathways, because observed absolute decoupling is too slow and the dominant scenarios close the gap only by assuming carbon dioxide removal at scales never demonstrated — leaving demand reduction in the North as the only physically available lever.
% FOUNDING_PROBLEM_CORROBORATION: IPCC AR6 WGIII scenario documentation attests the CDR reliance and the decoupling shortfall from outside the degrowth beneficiary set, as do UNEP emissions gap reports and energy-system analysts. The corroboration is partial and stated as such: the same assessment literature produces the CDR-reliant scenarios this reading rejects, and technology-optimist analysts outside the beneficiary set dispute the insufficiency premise. No corroboration is fully disinterested, but the gap itself is attested by parties who do not benefit from the degrowth program.
narrative_ontology:disappearance_verdict(climate_response_imperative__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__degrowth_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_imperative__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__degrowth_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.65: the requirement's costs are concentrated, near-term, and borne by a defined population (consumption ceilings, working-time restructuring, wealth levies, stranded assets), while the benefit stream is diffuse, deferred, and partly extra-generational — the classic asymmetry — tempered, by this reading's own lights, by burden allocation that tracks historical responsibility and capacity, and by the fact that much of the reduced consumption is surplus rather than necessity. Suppression 0.70 is structural, not internalized: Northern opposition to the requirement is overt and organized (high resistance), not a cognitive pattern that would persist after barrier removal; no voluntary path to the transformation has been observed in any Northern jurisdiction, so enforcement machinery (rationing, caps, working-time regulation, redistribution mandates) must operate against revealed preference. Theater 0.20: the advocacy is largely operational — policy blueprints, citizen-assembly designs, pilot programs — with a declining performative share as proposals concretize (see series). Accessibility_collapse 0.55: within the frame, once the no-CDR and slow-decoupling premises are accepted, alternatives collapse strongly; the frame's entry itself remains contested, leaving real exits (technological optimism) open to those who reject the premises. Resistance 0.80: electoral backlash dynamics, industry opposition, and growth-dependent institutions (pensions, employment, fiscal models) all contest the constraint. The temporal series run on one shared grid (2018–2026, five points) so every tracked metric is authored at every examined time point; suppression_requirement is deliberately not series-tracked because the constraint is not yet enforced — its enforcement picture is static and carried by the scalar.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is the point of this story. From the degrowth_policy_community seat the arrangement computes as necessary, physically honest coordination — the only response that does not mortgage the future to unproven technology. From the affluent_northern_households seat the same structure operates as expropriation of lifestyle and wealth without consent. From the precarious_northern_households seat it is genuinely mixed: working-time reduction and public provision against income and pension risk. From future_generations the difference is a livable versus unlivable world — but that seat cannot speak, which is precisely why its interests must be carried by declaration rather than voice. The engine computes per-seat classifications from the structural data; the authored claim does not adjudicate among these seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: future_generations and global_south_populations receive the benefit stream at zero cost-bearing (d near the beneficiary end); precarious_northern_households are dual-positioned — they pay working-time and consumption adjustments and receive domestic redistribution — landing mid-low. Victim declarations: affluent_northern_households bear the largest per-capita burden but hold arbitrage-grade exit (capital flight, exemption purchasing, jurisdictional arbitrage), which damps their effective extraction; carbon_intensive_industries bear stranded-asset costs with the same arbitrage damping. The degrowth_policy_community, as agenda-setter, is subsidized by the constraint — it is their project — giving low d with identity-locked exit; the lock is professional-ideological (careers, citation networks, and a sufficiency worldview fused with the project), and if the frame broke — CDR scaling, decoupling outperformance — the seat would need to re-found itself, which is the advocacy_identity_independence omega's question. One directionality override is authored: for the powerless class (occupied here only by future_generations), d is pinned to 0.05 because the structural derivation weights trapped exit toward the target end, but future generations' trapped-ness is about inheriting outcomes, not bearing constraint costs — their declared zero cost-bearing and total benefit receipt place them at the beneficiary end.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification blocks two mislabels. As pure extraction it would erase the genuine coordination function: absent carbon removal at scale, demand reduction in the North is the only physically available mitigation lever, and redistribution is the operative adaptation-finance mechanism — the constraint solves a real collective-action problem. As pure coordination it would erase the real victim set: costs fall on a defined present-day population, enforcement runs against revealed preference, and the benefit stream accrues largely to parties who bear no commensurate cost and cannot reciprocate. Mandatrophy runs in the non-standard direction here: the founding problem (the mitigation gap) is live and widening, so the mandate has not outlived its function — the corruption risk is not atrophied persistence but premature mandate declaration by growth interests, and the mismatch consumer should find status=live consistent with verdict=world_rearranges. The receipt surface carries the piton cell signature honestly (diffuse gains, prohibitive fix): the transformation's costs cannot be offloaded or deferred without dissolving the constraint's function — that is the reading's core claim, the elimination of the CDR escape hatch — and no named seat captures the gains, which split across Southern adaptation finance, domestic redistribution, and the atmospheric public good. That cell flags cost-asymmetry; it does not make the constraint a piton, which would require an atrophied function and theatrical maintenance, and the theater series runs the other way.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (degrowth_reading) of the climate_response_imperative kernel: what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Corpus-level comparison of the three reading files over the shared standing arrangement; no within-story resolution exists by design (one reading, one constraint, one epsilon).',
    'The mitigation_priority_reading moves present-day Northern consumption off the victim ledger, shifts victims toward carbon-intensive industries and beneficiaries toward technology sectors, and restores CDR reliance (raising its own deferred-cost extraction). The adaptation_priority_reading shifts victims to climate-exposed populations and beneficiaries to resilience infrastructure, demoting mitigation to the aspirational. The disagreement is located in two structural elements: the sufficiency premise (whether growth-compatible pathways can deliver the response at all) and the burden-allocation rule (who pays, on what principle).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed structure of the climate response kernel: sibling deltas and the location of the contest.').

omega_variable(
    cdr_scalability_contest,
    'Can carbon dioxide removal scale to the gigatonne-per-year levels the dominant net-zero scenarios assume, relaxing or dissolving the degrowth requirement?',
    'Monitored deployment and cost curves against scenario requirement levels (UNEP emissions gap reporting, CDR market data, IPCC assessment cycles).',
    'If CDR scales, the reading''s foundational axiom (growth_compatible_mitigation_insufficient, empirically contingent) weakens, the victim set shrinks, and this reading converges toward the mitigation-priority sibling; if it does not, the requirement hardens and the resistance it meets must break politically rather than technologically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cdr_scalability_contest, empirical, 'CDR scalability as the primary empirical hinge of the degrowth axiom.').

omega_variable(
    absolute_decoupling_sufficiency,
    'Can Global North economies absolutely decouple GDP from emissions and material throughput fast enough to meet temperature targets without deliberate demand reduction?',
    'Consumption-based emissions and material footprint accounting against required decarbonization and dematerialization rates.',
    'If decoupling suffices, the reduced-consumption and working-time demands are unnecessary and the constraint''s victim set is spurious; if not, the coordination function of the requirement is confirmed and the extraction component is the price of physics rather than of ideology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolute_decoupling_sufficiency, empirical, 'Decoupling sufficiency as the second empirical escape hatch from the requirement.').

omega_variable(
    present_generation_burden_legitimacy,
    'Are the costs imposed on present-day Global North populations legitimate responsibility-weighted burden-sharing (this reading''s claim) or extraction without consent (the payer seats'' claim)?',
    'Democratic legitimacy mechanisms: citizen assemblies, referenda, negotiated transition compacts that would convert imposed burden into consented burden.',
    'If legitimated, the constraint''s asymmetric cost-bearing reads as justified coordination and enforcement costs fall; if refused, the requirement either fails politically or persists only through coercion, drifting the arrangement toward pure extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(present_generation_burden_legitimacy, preference, 'Consent status of the present-generation burden: the central seat-divergence question.').

omega_variable(
    advocacy_identity_independence,
    'Does the degrowth policy community update on disconfirming evidence (CDR cost declines, decoupling outperformance) or defend the frame identity-protectively?',
    'Track the community''s published response to disconfirming evidence across successive assessment cycles; compare internal critiques against external replication.',
    'If identity-protective, the reading''s sufficiency and burden assessments drift optimistic and the agenda-setter seat hardens toward captured advocacy; if truth-tracking, the reading''s claims retain evidential standing independent of the community''s fused identity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(advocacy_identity_independence, empirical, 'Identity-fusion risk in the agenda-setting advocacy community.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__degrowth_reading, 2018, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(degrowth_reading_tr_t2018, climate_response_imperative__degrowth_reading, theater_ratio, 2018, 0.3).
narrative_ontology:measurement_basis(degrowth_reading_tr_t2018, observed).
narrative_ontology:measurement(degrowth_reading_tr_t2020, climate_response_imperative__degrowth_reading, theater_ratio, 2020, 0.27).
narrative_ontology:measurement_basis(degrowth_reading_tr_t2020, observed).
narrative_ontology:measurement(degrowth_reading_tr_t2022, climate_response_imperative__degrowth_reading, theater_ratio, 2022, 0.25).
narrative_ontology:measurement_basis(degrowth_reading_tr_t2022, observed).
narrative_ontology:measurement(degrowth_reading_tr_t2024, climate_response_imperative__degrowth_reading, theater_ratio, 2024, 0.22).
narrative_ontology:measurement_basis(degrowth_reading_tr_t2024, observed).
narrative_ontology:measurement(degrowth_reading_tr_t2026, climate_response_imperative__degrowth_reading, theater_ratio, 2026, 0.2).
narrative_ontology:measurement_basis(degrowth_reading_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(degrowth_reading_be_t2018, climate_response_imperative__degrowth_reading, base_extractiveness, 2018, 0.55).
narrative_ontology:measurement_basis(degrowth_reading_be_t2018, observed).
narrative_ontology:measurement(degrowth_reading_be_t2020, climate_response_imperative__degrowth_reading, base_extractiveness, 2020, 0.58).
narrative_ontology:measurement_basis(degrowth_reading_be_t2020, observed).
narrative_ontology:measurement(degrowth_reading_be_t2022, climate_response_imperative__degrowth_reading, base_extractiveness, 2022, 0.61).
narrative_ontology:measurement_basis(degrowth_reading_be_t2022, observed).
narrative_ontology:measurement(degrowth_reading_be_t2024, climate_response_imperative__degrowth_reading, base_extractiveness, 2024, 0.63).
narrative_ontology:measurement_basis(degrowth_reading_be_t2024, observed).
narrative_ontology:measurement(degrowth_reading_be_t2026, climate_response_imperative__degrowth_reading, base_extractiveness, 2026, 0.65).
narrative_ontology:measurement_basis(degrowth_reading_be_t2026, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(climate_response_imperative__degrowth_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__degrowth_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, mitigation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, adaptation_priority_reading).

% DUAL FORMULATION NOTE:
% The climate_response_imperative kernel decomposes into three reading-constraints over one shared standing arrangement: this degrowth reading; the mitigation_priority_reading (technology-led, growth-compatible; victims among carbon-intensive industries, beneficiaries among technology sectors; CDR reliance raises its deferred-cost extraction); and the adaptation_priority_reading (resilience-first; victims among climate-exposed populations, beneficiaries among resilience infrastructure). The readings differ in victim/benefit sets and in authored epsilon, not in the arrangement they assess; family links are declared so legitimacy shifts propagate across the set. The mitigation-priority reading currently dominates institutional configuration, which is why this reading registers structural pressure on both siblings rather than merely coexisting with them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_imperative__degrowth_reading, powerless, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
