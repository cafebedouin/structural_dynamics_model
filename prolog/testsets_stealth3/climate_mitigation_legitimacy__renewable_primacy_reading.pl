% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__renewable_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__renewable_primacy_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__renewable_primacy_reading
 *   human_readable: Renewable Primacy Doctrine — Renewables-Plus-Storage Sufficiency Reading
 *   domain: energy policy/climate mitigation/technology governance
 *
 * SUMMARY:
 *   The claim that renewables plus storage can decarbonize faster and cheaper
 *   than nuclear functions, once institutionalized, as a constraint on the
 *   legitimate solution space of climate mitigation: it steers subsidy
 *   design, green-financing eligibility, grid-planning assumptions, and
 *   activist coalition identity toward a renewables-centric pathway while
 *   recasting nuclear investment as a capital diversion. This file
 *   instantiates the renewable_primacy_reading of the kernel
 *   climate_mitigation_legitimacy — one reading, one epsilon, per the
 *   decomposition discipline. The epsilon referent is the standing
 *   arrangement the story is about: the policy, finance, and discourse regime
 *   shaped by this reading since roughly 2000 (interval t=0..25 approximates
 *   2000-2025). Sibling readings — baseload_necessity_reading,
 *   portfolio_pragmatism_reading, degrowth_sufficiency_reading — instantiate
 *   different constraints with different victim and beneficiary sets and
 *   different epsilon over the same referent; all are linked via
 *   network.affects_constraints. The claim/metric gap is deliberate:
 *   claimed_type records my structural judgment (tangled_rope — genuine
 *   mobilization coordination plus asymmetric extraction borne by the nuclear
 *   complex); the metrics record descriptive operation as this reading's own
 *   lights assess it; the engine computes per-seat classifications from the
 *   structural data, and any divergence between claim and computed type is
 *   the measurement the corpus exists to take. KEY AGENTS (by structural
 *   relationship): - climate_ministries_and_regulators: Agenda-setter
 *   (institutional/constrained) — adopts and enforces the doctrine in policy
 *   - renewable_development_industry: Primary beneficiary (powerful/mobile) —
 *   receives steered capital and procurement priority -
 *   green_finance_institutions: Beneficiary (institutional/mobile) — controls
 *   capital eligibility - battery_storage_manufacturers: Beneficiary
 *   (organized/arbitrage) - climate_advocacy_networks: Beneficiary and
 *   discourse agenda-setter (organized/identity_locked) - nuclear_operators:
 *   Primary target (powerful/trapped) — bears retirement pressure and
 *   financing exclusion - nuclear_skilled_workforce: Target
 *   (moderate/identity_locked) - host_communities_of_retiring_plants: Target
 *   (powerless/trapped) - advanced_nuclear_developers: Target, structurally
 *   excluded (moderate/trapped) - natural_gas_generators: Anomalous
 *   beneficiary (institutional/arbitrage) - electricity_ratepayers:
 *   Near-symmetric dual seat (moderate/constrained) -
 *   integrated_assessment_modeling_community: Analytical observer
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__renewable_primacy_reading, 0.48).
domain_priors:suppression_score(climate_mitigation_legitimacy__renewable_primacy_reading, 0.55).
domain_priors:theater_ratio(climate_mitigation_legitimacy__renewable_primacy_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__renewable_primacy_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__renewable_primacy_reading, "Renewable Primacy Doctrine — Renewables-Plus-Storage Sufficiency Reading").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__renewable_primacy_reading, "energy policy/climate mitigation/technology governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__renewable_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__renewable_primacy_reading, '3d2339ae-2851-4b74-9c4c-f046a34b5a25').
narrative_ontology:cs_kernel_codification('3d2339ae-2851-4b74-9c4c-f046a34b5a25', distributed).
narrative_ontology:cs_authority_grounding('3d2339ae-2851-4b74-9c4c-f046a34b5a25', distributed).
narrative_ontology:cs_reading_relation('3d2339ae-2851-4b74-9c4c-f046a34b5a25', climate_mitigation_legitimacy__baseload_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('3d2339ae-2851-4b74-9c4c-f046a34b5a25', climate_mitigation_legitimacy__portfolio_pragmatism_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d2339ae-2851-4b74-9c4c-f046a34b5a25', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('3d2339ae-2851-4b74-9c4c-f046a34b5a25', foundational, renewables_storage_sufficiency_for_full_decarbonization).
narrative_ontology:cs_axiom_status(renewables_storage_sufficiency_for_full_decarbonization, holdable).
narrative_ontology:cs_axiom_grounding('3d2339ae-2851-4b74-9c4c-f046a34b5a25', renewables_storage_sufficiency_for_full_decarbonization, empirically_contingent).
narrative_ontology:cs_axiom('3d2339ae-2851-4b74-9c4c-f046a34b5a25', foundational, nuclear_expansion_as_mitigation_capital_sink).
narrative_ontology:cs_axiom_status(nuclear_expansion_as_mitigation_capital_sink, holdable).
narrative_ontology:cs_axiom_grounding('3d2339ae-2851-4b74-9c4c-f046a34b5a25', nuclear_expansion_as_mitigation_capital_sink, empirically_contingent).
narrative_ontology:cs_reference_frame('3d2339ae-2851-4b74-9c4c-f046a34b5a25', manufactured_generation_sufficiency_norm).
narrative_ontology:cs_drift_state('3d2339ae-2851-4b74-9c4c-f046a34b5a25', post_energy_security_reassessment, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3d2339ae-2851-4b74-9c4c-f046a34b5a25', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_development_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, battery_storage_manufacturers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, green_finance_institutions).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, climate_advocacy_networks).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, electricity_ratepayers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, natural_gas_generators).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_operators).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_skilled_workforce).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, host_communities_of_retiring_plants).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, advanced_nuclear_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, natural_gas_generators).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, electricity_ratepayers).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__renewable_primacy_reading, lcoe_competitiveness_thesis).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__renewable_primacy_reading, manufacturing_learning_curve_scaling).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__renewable_primacy_reading, storage_cost_trajectory_optimism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopts the renewables-first reading into subsidy design, auction rules, grid-planning assumptions, and phase-out schedules. Collects electoral credit for visible deployment and carries blame for adequacy shortfalls and price spikes. Once coalitions and committed capital organize around the framework, credible reversal becomes politically expensive.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, climate_ministries_and_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Develops, manufactures, and finances solar, wind, and storage assets. Receives preferential procurement priority, subsidized finance, and unconditional eligibility under green frameworks. Lobbies to keep technology comparison centered on per-unit cost trajectories favorable to its products. Capital can redeploy across markets and adjacent technologies if returns shift.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_development_industry, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_development_industry, agenda_setter).

% Manufactures batteries for stationary and vehicle markets. Order books depend on mandates and capacity mechanisms premised on storage-heavy system designs; product lines are fungible between segments, giving the sector unusual flexibility about where growth materializes.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, battery_storage_manufacturers, beneficiary,
    organized, biographical, arbitrage, global).

% Runs ESG funds and eligibility screens that treat renewables as unconditionally eligible and nuclear as conditional or excluded. Collects management fees on the resulting flows and holds no physical-plant exposure if system outcomes disappoint expectations.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, green_finance_institutions, beneficiary,
    institutional, biographical, mobile, global).

% Campaigns, litigates, and shapes media narratives around a renewables-first framing; membership identity and donor bases are bound to it. Participates in taxonomy consultations and divestment campaigns. Revisiting the sufficiency premise would unsettle coalition partners and funding narratives, so the frame is maintained even as contrary evidence accumulates.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, climate_advocacy_networks, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__renewable_primacy_reading, climate_advocacy_networks, agenda_setter).

% Operates existing reactor fleets under retirement pressure, hesitant reinvestment, and lending screens applied by institutions using green-eligibility criteria. Plants are fixed to their sites; shutdown converts producing assets into decommissioning liabilities, so exit from the arrangement is only available as liquidation.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_operators, payer,
    powerful, biographical, trapped, national).

% Reactor operators, engineers, and fuel-cycle specialists with decades-deep specialized training. Closures eliminate career ladders; comparable work exists almost exclusively at other plants, so professional identity and livelihood move together and cannot be relocated into the growing sectors.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_skilled_workforce, payer,
    moderate, biographical, identity_locked, regional).

% Towns hosting reactor sites rely on plant wages, tax base, and civic institutions. Retirement decisions are made in distant capitals on asset-economics grounds; replacement employment rarely arrives at comparable scale, and the community cannot move to follow the work.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, host_communities_of_retiring_plants, payer,
    powerless, generational, trapped, local).

% Pursues new reactor designs under licensing timelines of a decade or more with capital committed upfront. Largely ineligible for green-labelled capital; depends on state customers, venture funding, and philanthropy outside ESG channels. Would object to eligibility rulings but lacks seats in most climate-finance forums where the rulings are drafted.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, advanced_nuclear_developers, payer,
    moderate, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__renewable_primacy_reading, advanced_nuclear_developers, excluded).

% Operates gas-fired fleets supplying backup and capacity. Earns dispatch and capacity revenue in hours and seasons when variable output and stored energy fall short, particularly after nearby nuclear retirements. Nominally a displacement target of decarbonization, currently an operational beneficiary of its gaps; long-term revenues face carbon pricing and electrification displacement.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, natural_gas_generators, beneficiary,
    institutional, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__renewable_primacy_reading, natural_gas_generators, payer).

% Households and businesses buying from the grid. See falling bulk-energy prices where renewable build-out is strong, and rising network, backup-capacity, and stranded-cost charges where firm supply ends up thinner than planned. Monthly bills are the arena where the doctrine's promises meet system-integration costs; only affluent customers can partially exit via self-supply.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, electricity_ratepayers, beneficiary,
    moderate, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__renewable_primacy_reading, electricity_ratepayers, payer).

% Produces the integrated scenario literature — shared socioeconomic pathways, net-zero scenarios — featuring renewables-led expansions alongside substantial nuclear retention. Cited selectively by every faction; runs the models that all sides claim vindicate them, and collects nothing from the outcome either way.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, integrated_assessment_modeling_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_development_industry).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__renewable_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Decarbonization requires deploying manufactured clean generation at unprecedented scale and speed, which requires aligning investor expectations, manufacturing build-out, permitting regimes, and political coalitions on a common pathway. The doctrine supplies that common signal, collapsing perceived technology risk for capital providers and letting supply chains, regulators, and activists pull in the same direction.
% TRANSFER_FUNCTION: Moves public subsidy, private capital, and regulatory attention toward solar, wind, and storage supply chains; moves financing eligibility and legitimacy away from nuclear projects; moves interim dispatch and capacity revenue to gas-fired generation where firm supply falls short; moves closure costs to plant-host regions and their workforces.
% ABSENT_VOICES: Nuclear-experienced engineers and system operators who argue firm-supply requirements are systematically underestimated are present in technical literature but marginalized in policy fora; developing-country planners facing capital ceilings are outside green-taxonomy consultations; future generations exposed to residual emissions if the pathway underdelivers have no seat anywhere. Dissenting capacity-auction participants and grid planners hear the doctrine's assumptions but do not draft them.
% DISAPPEARANCE_RATIONALE: Capital allocated under doctrine-shaped eligibility reprices overnight: nuclear projects regain financing access, storage mandates lose their sufficiency rationale, advocacy coalitions reorganize around portfolio or sufficiency framings, and grid plans rebalance toward firm capacity. Banked deployment persists, but the marginal-allocation machine — auctions, taxonomy screens, ESG mandates, campaign infrastructure — is the surface that rearranges.
% FOUNDING_PROBLEM: In the early 2000s the binding constraint on climate action was deployable supply: nuclear was capital-intensive, slow to license, politically stigmatized after Chernobyl and later Fukushima, and entangled with proliferation concerns, while coal remained the marginal fuel. The doctrine was built to answer 'how do we mobilize mitigation capital now, at scale' with 'manufacture it' — solar, wind, batteries — where costs fall with cumulative production rather than with mega-project execution.
% FOUNDING_PROBLEM_CORROBORATION: The mobilization problem and renewables' central role are corroborated from outside the beneficiary set by IEA scenario work and IPCC pathway literature — both of which, notably, retain large nuclear build-out and thereby dispute this reading's exclusivity claim. Transmission-operator adequacy assessments independently attest the firm-supply problem the sufficiency premise discounts. No corroborating source outside the doctrine's own coalition attests the exclusivity claim itself; that statement is itself signal.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__renewable_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__renewable_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__renewable_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__renewable_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__renewable_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__renewable_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.48: substantial but bounded, and deliberately authored from this reading's own seat over the fixed referent — the reading credits the regime's demonstrated coordination achievement (which damps epsilon below the near-maximal value a rival reading would assign to the same arrangement) while its reflective margin concedes the documented costs: retirement-driven gas backfill, financing exclusion, pledge theater. A triumphalist variant of this reading would author near-zero; the divergence across readings over one referent is exactly what the reading-indexed epsilon apparatus exists to measure. Suppression 0.55: the doctrine's persistence depends on actively maintained exclusion — green-taxonomy eligibility contests, lender screening, asymmetric licensing burdens, retirement advocacy — but nuclear pathways remain legally available in most jurisdictions, capping suppression below snare levels. Theater 0.30: a growing share of doctrine activity is performative (net-zero-aligned 100-percent-renewable pledges, corporate clean-energy claims untethered from firm-supply accounting) layered on genuinely functional deployment. Accessibility_collapse 0.38: alternatives remain visibly arguable — this is a contested doctrine, not an accepted law. Resistance 0.58: continuous counter-mobilization from nuclear states, operators, portions of the modeling community, and energy-security institutions. All three temporal series share one grid (t = 0,5,10,15,20,25); dynamics are monotonic rather than cyclical across the interval — the Fukushima trough and the 2022 energy-security rebound register as perturbations on the trend, not a complete oscillation cycle, and their mechanism ambiguity is carried in the suppression-mechanism omega rather than modeled as a cycle.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and beneficiary seats the doctrine computes as coordination: a shared deployment program that solved a genuine collective-action problem by aligning trillions in capital against carbon budgets. From the payer seats — trapped nuclear operators, identity-locked specialists, place-bound host communities — the same structure computes as extraction with suppression: their alternatives collapse (sites are immobile, skills non-transferable, eligibility screens closed) precisely because the doctrine's legitimacy claims steer capital elsewhere. The analytical seats see both faces at once: coordination whose surplus is real but whose incidence is asymmetric. The engine derives this per-seat divergence from the declared structure; the divergence, not the label, is the datum.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries derive low directionality: the development industry receives the steered capital directly; storage manufacturers ride the mandated order book; green finance collects fee flows on doctrine-shaped eligibility; advocacy networks collect identity and funding rents. Electricity ratepayers sit nearer symmetric — bulk-energy savings against integration-cost pass-throughs — and their dual exposure is documented in their situation and in the hidden-gas omega rather than averaged away. Natural-gas generators are the anomalous case: nominally targeted by decarbonization yet collecting dispatch and capacity revenue through the doctrine's firm-supply gaps; their declared beneficiary position is real but parasitic on the doctrine's shortfalls, and is routed to an omega instead of being silently absorbed into the coordination account. Declared victims derive high directionality, amplified by exit structure: the trapped operator (immobile assets), the identity-locked specialist, the place-bound community, and the capital-committed advanced developer sit progressively nearer the full-target end — the least mobile targets bear the amplified effective burden. Host communities' theoretical coalition power is checked by place-boundedness and by timing: closure decisions are taken before organizing windows open. Ministries carry partial-beneficiary position (electoral credit) tempered by accountability for adequacy failures and price spikes; the engine weighs these from the declared roles.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to mobilize mitigation-scale capital quickly given nuclear's 2000s-era cost, speed, licensing, and stigma constraints — was substantially addressed: manufactured clean technology scaled far beyond what 2000-era planning anticipated. But the mandate metastasized from acceleration to substitution: the doctrine now claims sufficiency, not merely priority. The tangled-rope classification prevents two symmetrical errors. Reading the structure as pure rope erases the identifiable seats bearing the burden — premature fleet losses, dissolved career ladders, stranded host regions are not coordination overhead. Reading it as pure snare erases the demonstrated mobilization the coordination function delivered, and would misread a live empirical contest as mere cover. Whether the mandate has outlived its function turns on the unresolved system-cost dispute (omega system_cost_parity_dispute); hence founding_problem_status is contested, not dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates one reading (renewable_primacy_reading) of the kernel climate_mitigation_legitimacy; how would classification change under each sibling reading?',
    'Author the sibling files (baseload_necessity_reading, portfolio_pragmatism_reading, degrowth_sufficiency_reading) over the same referent and compare per-seat classifications; divergence localizes which structural element carries the disagreement.',
    'Under portfolio_pragmatism, nuclear leaves the victim set and becomes a coordinated participant, dropping measured extraction on its seats; under baseload_necessity, the integration burden shifts onto renewables themselves as cost-carriers and the beneficiary structure inverts; under degrowth_sufficiency, the entire supply-expansion contest is bypassed and this reading''s transfer surface dissolves.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame routing: one reading of a four-reading kernel; disagreement located in the firm-supply sufficiency premise.').

omega_variable(
    system_cost_parity_dispute,
    'At high penetration with enforced reliability, does a renewables-plus-storage-dominant system actually deliver full decarbonization faster and at lower total system cost than a portfolio retaining nuclear?',
    'Open capacity-expansion model ensembles with explicit adequacy constraints and harmonized assumptions, cross-checked against observed outcomes from high-penetration grids versus nuclear-retaining systems.',
    'If diversified portfolios win on total system cost, the doctrine''s steering of capital away from nuclear loses its coordination justification and the constraint trends snare; if renewables-plus-storage wins robustly, measured extraction is transitional coordination overhead and the constraint trends rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(system_cost_parity_dispute, empirical, 'Whether the reading''s core empirical premise survives whole-system accounting with adequacy enforced.').

omega_variable(
    nuclear_exclusion_suppression_mechanism,
    'Is the suppression of nuclear financing structural (eligibility rules, taxonomy exclusion, regulatory asymmetry) or internalized (investor risk priors formed at Three Mile Island, Chernobyl, and Fukushima that persist independently of policy)?',
    'Post-neutralization trajectory: observe whether private capital returns to nuclear when eligibility rules equalize (partially observed following the 2023 COP28 tripling declaration); persistent flow depression after policy equalization indicates internalized aversion.',
    'Internalized suppression persists even after the doctrine''s policy instruments are removed, raising effective suppression and stabilizing the extraction pattern; purely structural suppression unwinds quickly under technology-neutral rules.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_exclusion_suppression_mechanism, empirical, 'Structural versus internalized suppression mechanism behind nuclear financing exclusion.').

omega_variable(
    storage_learning_curve_persistence,
    'Will storage costs continue declining fast enough to firm a renewables-dominant system at acceptable cost, or do integration costs (curtailment, transmission, seasonal storage) inflect upward before full decarbonization?',
    'Track delivered storage system costs, value deflation at rising penetration, and seasonal-gap coverage economics through 2035 against doctrine-era projections.',
    'Continued decline supports the sufficiency axiom; plateau or inflection empirically undermines it and forces migration toward portfolio or baseload readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(storage_learning_curve_persistence, empirical, 'Trajectory risk underlying the sufficiency axiom.').

omega_variable(
    hidden_gas_beneficiary_ambiguity,
    'Does the doctrine''s operation systematically route interim dispatch and capacity revenue to natural-gas generation where firm nuclear capacity retires without equivalent firm replacement, making fossil incumbents undeclared beneficiaries?',
    'Dispatch and capacity-market data correlating nuclear retirement events with subsequent gas generation and capacity payments in the same balancing areas.',
    'If confirmed, the beneficiary structure is contaminated: a nominally targeted actor collects through the constraint''s operation, strengthening the extraction reading and extending the victim set toward the climate outcome itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hidden_gas_beneficiary_ambiguity, empirical, 'Whether gas captures value through the doctrine''s firm-supply gaps.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__renewable_primacy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 5, 0.13).
narrative_ontology:measurement_basis(clim_tr_t5, observed).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement_basis(clim_tr_t10, observed).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement_basis(clim_tr_t15, observed).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(clim_tr_t20, observed).
narrative_ontology:measurement(clim_tr_t25, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement_basis(clim_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 0, 0.26).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 5, 0.31).
narrative_ontology:measurement_basis(clim_be_t5, observed).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement_basis(clim_be_t10, observed).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 15, 0.41).
narrative_ontology:measurement_basis(clim_be_t15, observed).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement_basis(clim_be_t20, observed).
narrative_ontology:measurement(clim_be_t25, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 25, 0.48).
narrative_ontology:measurement_basis(clim_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 5, 0.37).
narrative_ontology:measurement_basis(clim_su_t5, observed).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement_basis(clim_su_t10, observed).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 15, 0.49).
narrative_ontology:measurement_basis(clim_su_t15, observed).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement_basis(clim_su_t20, observed).
narrative_ontology:measurement(clim_su_t25, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 25, 0.55).
narrative_ontology:measurement_basis(clim_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__renewable_primacy_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'how should we decarbonize.' The kernel climate_mitigation_legitimacy decomposes into four structurally distinct constraints (readings), each with its own stable epsilon over the shared referent (the standing mitigation-legitimacy regime): this renewable_primacy file (nuclear complex in victim set, distributed generation privileged, shorter capital cycles); baseload_necessity (variable generation itself becomes the cost-carrier); portfolio_pragmatism (victim set empties, both industries coordinate); degrowth_sufficiency (supply-side contest bypassed, demand-side seats become the contested surface). The empirical cost-and-scalability literature sits upstream of this reading and is cited as evidence by it; the readings are mutually linked via affects_constraints so contamination and foreclosure propagate through the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
