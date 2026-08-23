% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__mitigation_priority, []).

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
 *   constraint_id: climate_response_legitimacy__mitigation_priority
 *   human_readable: Mitigation-Priority Climate Legitimacy Boundary (Green-Growth Reading)
 *   domain: political economy/environmental governance/intergenerational ethics
 *
 * SUMMARY:
 *   The colloquial label 'the legitimate climate response' names a contested
 *   kernel: different coalitions read climate legitimacy differently, and
 *   each reading instantiates a different constraint. This story authors ONE
 *   reading — mitigation_priority: legitimate response reduces emissions
 *   through technological innovation and carbon pricing while preserving
 *   economic growth, decoupling GDP from emissions. The standing arrangement
 *   under contest is the actual regime this reading has built: the
 *   pledge-and-review architecture, carbon markets, innovation subsidy at
 *   trillion-dollar scale, net-zero accounting that books future removals,
 *   and the credibility boundary that admits growth-compatible proposals and
 *   shelves the rest. Per the epsilon-referent rule, epsilon is authored for
 *   THAT arrangement, assessed by this reading's own lights — not for the
 *   arrangements the sibling readings (adaptation_priority,
 *   degrowth_transformation) would build; those are separate files linked
 *   through the network section. Structurally the arrangement couples a
 *   genuine coordination function — it assembled growth-dependent governments
 *   into a common decarbonization framework that pure-sacrifice framings
 *   never could — with asymmetric burden allocation: present wealthy-nation
 *   consumption is shielded, transition rents flow to identifiable capitals,
 *   and the residual-warming bill is written, contingently, to parties with
 *   no seat. KEY AGENTS (by structural relationship):
 *   climate_governance_establishment — agenda setter
 *   (institutional/constrained) — administers the credibility boundary;
 *   incumbent_fossil_asset_holders — primary beneficiary
 *   (institutional/arbitrage) — asset values protected by gradual timelines;
 *   green_technology_industries — secondary beneficiary
 *   (institutional/mobile) — receives subsidized demand;
 *   carbon_market_service_sector — tertiary beneficiary (organized/mobile) —
 *   fee income from pricing and offset architecture;
 *   high_consumption_households — beneficiary with payer secondary role
 *   (moderate/constrained) — consumption legitimated, visible carbon costs
 *   paid; future_generations — primary target (powerless/trapped) — bears
 *   residual-warming costs contingent on decoupling performance;
 *   climate_vulnerable_populations — primary target (powerless/trapped) —
 *   absorbs impacts while adaptation is subordinated; fossil_region_workers —
 *   payer (organized/identity_locked) — transition dislocation with fused
 *   community and occupational identity; degrowth_advocates and
 *   climate_justice_movements — excluded (moderate/constrained) — locked out
 *   of the venues where legitimacy is assigned;
 *   independent_climate_economists — analytical observer
 *   (analytical/analytical) — audits decoupling and offset claims from
 *   outside the benefiting coalition.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__mitigation_priority, 0.58).
domain_priors:suppression_score(climate_response_legitimacy__mitigation_priority, 0.62).
domain_priors:theater_ratio(climate_response_legitimacy__mitigation_priority, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__mitigation_priority, "Mitigation-Priority Climate Legitimacy Boundary (Green-Growth Reading)").
narrative_ontology:topic_domain(climate_response_legitimacy__mitigation_priority, "political economy/environmental governance/intergenerational ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__mitigation_priority, '8fbbdc80-0050-42af-84c1-401b320be13b').
narrative_ontology:cs_kernel_codification('8fbbdc80-0050-42af-84c1-401b320be13b', distributed).
narrative_ontology:cs_authority_grounding('8fbbdc80-0050-42af-84c1-401b320be13b', expertise).
narrative_ontology:cs_interpretation_layer_present('8fbbdc80-0050-42af-84c1-401b320be13b').
narrative_ontology:cs_reading_relation('8fbbdc80-0050-42af-84c1-401b320be13b', climate_response_legitimacy__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('8fbbdc80-0050-42af-84c1-401b320be13b', climate_response_legitimacy__degrowth_transformation, forecloses).
narrative_ontology:cs_axiom('8fbbdc80-0050-42af-84c1-401b320be13b', foundational, growth_preservation_constitutive_of_legitimate_response).
narrative_ontology:cs_axiom_status(growth_preservation_constitutive_of_legitimate_response, holdable).
narrative_ontology:cs_axiom_grounding('8fbbdc80-0050-42af-84c1-401b320be13b', growth_preservation_constitutive_of_legitimate_response, instrumental).
narrative_ontology:cs_axiom('8fbbdc80-0050-42af-84c1-401b320be13b', foundational, emissions_reduction_precedence_over_impacts_management).
narrative_ontology:cs_axiom_status(emissions_reduction_precedence_over_impacts_management, holdable).
narrative_ontology:cs_axiom_grounding('8fbbdc80-0050-42af-84c1-401b320be13b', emissions_reduction_precedence_over_impacts_management, empirically_contingent).
narrative_ontology:cs_axiom('8fbbdc80-0050-42af-84c1-401b320be13b', secondary, market_innovation_sufficiency_for_decarbonization).
narrative_ontology:cs_axiom_status(market_innovation_sufficiency_for_decarbonization, holdable).
narrative_ontology:cs_axiom_grounding('8fbbdc80-0050-42af-84c1-401b320be13b', market_innovation_sufficiency_for_decarbonization, empirically_contingent).
narrative_ontology:cs_reference_frame('8fbbdc80-0050-42af-84c1-401b320be13b', green_growth_decoupling_compact).
narrative_ontology:cs_drift_state('8fbbdc80-0050-42af-84c1-401b320be13b', contemporary_emissions_gap_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8fbbdc80-0050-42af-84c1-401b320be13b', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, incumbent_fossil_asset_holders).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, green_technology_industries).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, carbon_market_service_sector).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, high_consumption_households).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, climate_vulnerable_populations).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, fossil_region_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, incumbent_fossil_asset_holders).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, high_consumption_households).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__mitigation_priority, green_growth_absolute_decoupling_hypothesis).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__mitigation_priority, carbon_pricing_cost_effectiveness_doctrine).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__mitigation_priority, integrated_assessment_optimal_pathway_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the machinery that decides which climate proposals count as credible: assessment-report scoping, integrated-assessment scenario library curation, central-bank stress-test design, and the pledge-and-review cycle. Staffed by economists and scientists whose professional standing rests on the growth-compatible framing. They could widen the boundary to admit rival framings, but doing so would unsettle the models, curricula, and career ladders built on the current one; abandoning climate governance altogether is not available to them.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, climate_governance_establishment, agenda_setter,
    institutional, generational, constrained, global).

% Owners of reserves, pipelines, refineries, and combustion infrastructure. Gradual phase-out schedules, grandfathered allowances, offset eligibility, and continued permitting protect the book value of these assets. They pay allowance and compliance costs and absorb some stranded-asset risk, but recapture much of it through pass-through pricing and allocation rules. Their exit options are strong: portfolio shifts, jurisdictional arbitrage, lobbying over design details, and diversification into the subsidized green sectors themselves.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, incumbent_fossil_asset_holders, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__mitigation_priority, incumbent_fossil_asset_holders, payer).

% Manufacturers and developers of renewables, storage, electric vehicles, hydrogen, and removal technologies. The innovation-centered framing channels public capital to them through tax credits, mandates, and procurement, creating demand they did not have to win in open competition. They can serve multiple jurisdictions and switch product lines; their dependence on the arrangement runs through subsidy continuity rather than physical lock-in.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, green_technology_industries, beneficiary,
    institutional, biographical, mobile, global).

% Exchanges, registries, verification firms, offset project developers, and trading desks. Every tonne priced, traded, or offset generates fee income for this sector. The income exists only as long as the pricing and offsetting architecture persists, so the sector's fortunes rise and fall with the arrangement's credibility. Exit is easy in principle because the skills transfer to other financial services, but the client base is specific to this architecture.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, carbon_market_service_sector, beneficiary,
    organized, biographical, mobile, global).

% Households in wealthy economies whose consumption patterns — frequent flying, meat-rich diets, large homes, car ownership — remain socially normal under this framing. They pay visible carbon charges on fuel and power and fund subsidies through taxation, while avoiding the deeper lifestyle restructuring that rival framings would ask of them. Individual exit means costly voluntary deprivation in a social environment that rewards the opposite.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, high_consumption_households, beneficiary,
    moderate, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__mitigation_priority, high_consumption_households, payer).

% Will inhabit the climate stock today's choices create. They bear whatever warming, sea-level rise, and infrastructure lock-in result from the pace of cuts and from net-zero accounting that counts on removals which may never arrive at scale. They appear in the arrangement only through proxy advocates and through the discount rates embedded in assessment models. There is no exit: they cannot leave the planet, the accumulated stock, or the institutional commitments made before they could speak.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Communities in low-lying coastal zones, arid interiors, and tropical regions already experiencing intensifying heat, drought, flooding, and storm damage. Adaptation finance reaches them late and small relative to mitigation finance; loss-and-damage mechanisms remain marginal. Moving requires resources and receiving-country permission most lack, so exposure is effectively fixed. They bear the difference between the temperature outcomes the arrangement promises and the ones it delivers.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, climate_vulnerable_populations, payer,
    powerless, biographical, trapped, global).

% Live in coal, oil, and gas dependent regions. The gradual transition timeline protects their jobs in the short run and erodes them in the medium run; promised retraining and regional funds arrive slower than closures. Their skills, pensions, towns, and family histories are tied to the industry, so leaving means losing community and occupational identity together, not just changing employers.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, fossil_region_workers, payer,
    organized, biographical, identity_locked, regional).

% Propose reducing material throughput in wealthy economies through working-time reduction, universal basic services, and democratic firm ownership. They publish in ecological-economics venues, organize outside official summits, and appear in media coverage of protests. They are kept out of the integrated-assessment scenario libraries and ministerial agendas where policy credibility is assigned — not by formal ban but by credibility gatekeeping that treats their founding premise as outside legitimate debate.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, degrowth_advocates, excluded,
    moderate, generational, constrained, global).

% Organize frontline communities and youth demanding loss-and-damage finance, reparations for historical emissions, and faster cuts than the pledge architecture delivers. They hold side-event space and protest permits but not seats where the framework's core logic is written. Their objection: the arrangement preserves the consumption of those least exposed while those most exposed wait for benefits that arrive late or never.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, climate_justice_movements, excluded,
    moderate, biographical, constrained, global).

% Audit the arrangement's empirical claims from outside the benefiting coalition: decoupling rates against pathway requirements, offset integrity, model sensitivity to discount rates and technology assumptions. They hold no administrative role and collect no fees; their findings feed the open questions this story registers.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, independent_climate_economists, observer,
    analytical, generational, analytical, global).

narrative_ontology:fixing_cost_class(climate_response_legitimacy__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the global collective-action problem of decarbonization under growth-dependent politics: a common carbon metric lets jurisdictions compare efforts, predictable price signals steer private capital into clean technology at scale, and the growth-compatibility promise keeps growth-dependent governments at the table — a level of participation that austerity-framed climate policy never assembled.
% TRANSFER_FUNCTION: Moves compliance payments from covered firms and households to treasuries and allowance holders; moves public capital toward green technology sectors via subsidies and mandated demand; moves fee income to carbon-market intermediaries; and — contingently on decoupling performance — moves residual warming costs onto future generations and currently-exposed populations, while shielding present wealthy-nation consumption from the deeper restructuring rival framings would require.
% ABSENT_VOICES: Degrowth and sufficiency theorists sit outside the scenario libraries and ministerial agendas that define credibility; climate-vulnerable populations hold thin formal representation relative to their exposure; future generations are present only through proxy advocates and the discount rates embedded in assessment models. Unanimity inside the framework arises partly because these seats were never in the room.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority legitimacy boundary vanished overnight, the climate-policy field would reorganize around one of the rival readings: adaptation-first triage would redirect finance from abatement to resilience, or degrowth transformation would target wealthy-nation throughput directly. Trillions in net-zero-aligned capital, carbon markets, and pledge architectures would lose their legitimating frame and reprice.
% FOUNDING_PROBLEM: After a decade of failed attempts to negotiate binding cuts (Rio through Kyoto to Copenhagen), the binding obstacle to climate action was political feasibility: governments dependent on growth and voters unwilling to accept austerity-style sacrifice could not ratify deep mandatory reductions. The mitigation-through-innovation-and-pricing framing was built to dissolve that deadlock by making decarbonization compatible with continued growth.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the diplomatic record of Copenhagen's 2009 collapse and the subsequent pivot to pledge-and-review, documented in negotiation histories and participant accounts independent of green-industry interests; IPCC Working Group III feasibility analyses treating growth-compatibility as a design requirement; and political-science scholarship on the Paris architecture. Degrowth scholars corroborate that the founding problem was real while disputing that this solution resolves it.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_legitimacy__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__mitigation_priority, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: the arrangement delivers real abatement while channeling transition rents to subsidy recipients and allowance holders and writing contingent residual-warming costs onto unseated parties; the value sits well above a pure-coordination baseline but below cover-story territory because the abatement is genuine. Suppression 0.62: the boundary is maintained by credibility gatekeeping — scenario-library curation, funding lines, editorial norms — rather than formal prohibition; rivals publish, but outside the venues where legitimacy is assigned. Theater 0.42: pledge inflation, voluntary-offset integrity failures, and net-zero accounting that books speculative removals are performative, while carbon prices and deployed renewables are functional. Accessibility_collapse 0.45: rival framings remain articulable and visible — they have not collapsed — but accessing institutional resources through them is blocked. Resistance 0.55: sustained pushback from justice movements, degrowth scholarship, and exposed-state diplomacy. All three tracked metrics share one grid (t=0..30, mapped to roughly 1995-2025): extractiveness and theater rise monotonically as the pledge-delivery gap widens; suppression_requirement is U-shaped — high while the Kyoto-era frame was contested, dipping during the mid-2000s consensus, rising again after 2018 as degrowth and climate-justice challenges forced active re-marginalization. The oscillation is not intermittent reinforcement; it tracks the visibility of rival readings, and the current rising phase reflects defensive enforcement, not extraction mechanics. Suppression is authored as a raw structural property; only extractiveness gets scaled downstream by directionality and scope. Claim and metrics are independent: the tangled_rope claim comes from the structure (real coordination plus asymmetric burden plus active enforcement), not from tuning to predicted outputs.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently. From the agenda-setter seat the arrangement is the framework its holders built and staffed — coordination they administer, with revision costs that look prohibitive from inside. From the beneficiary seats it is a revenue and asset-protection surface. From the payer seats it inverts: fossil-region workers experience promised-then-delayed compensation; exposed populations experience subordinated adaptation; future generations experience the entire structure as decisions taken on their behalf at a discount rate they never chose. The future-generations seat is the sharpest divergence: powerless and fully trapped, it computes at the full-target end of directionality despite having no voice in the discourse — the engine sees a party the legitimacy conversation does not seat. Coalition potential among payers is real but structurally blocked: workers, exposed populations, and future-generation proxies share targets but not timescales, venues, or identities, which is precisely why the asymmetry holds. Same-power divergence is visible between the two institutional beneficiary seats: fossil asset holders hold arbitrage-grade exit (portfolio shifts, jurisdictional shopping) while green-technology capital holds mere mobility (subsidy continuity dependence), so identical nominal standing produces different effective positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: fossil asset holders sit near the beneficiary end despite paying compliance costs — their arbitrage-grade exit and recapture channels dominate, which is why they carry the beneficiary role with a payer secondary role rather than needing an override entry; green technology industries and the carbon-market service sector are pure collectors; high-consumption households derive near-symmetric (visible carbon costs paid, larger shielded benefit received). Victim declarations drive high d: future generations (powerless, trapped — nearest the full-target end the derivation can assign), climate-vulnerable populations (powerless, trapped by geography and borders), fossil-region workers (organized but identity-locked, so exit modulation amplifies rather than damps their target position). The agenda-setter derives from administration rather than collection; its d sits low-mid because its benefit is standing and career continuity, not receipts. No directionality overrides were needed: the beneficiary/victim plus exit-option data yields the correct ordering, and the dual-positioned household seat is handled by its declared roles.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both mislabelings. Calling this a snare would erase the real coordination achievement — the framework did assemble global decarbonization cooperation that austerity framings could not, and abatement is genuinely delivered; the founding problem (growth-dependent political feasibility) is still live, so nothing here is vestigial. Calling it a rope would erase the asymmetry — burden allocation systematically shields present wealthy consumption, channels rents to identifiable capitals, and writes contingent costs onto unseated parties, held in place by active credibility enforcement. Mandatrophy is not declared: the mandate has not outlived its function, though the theater series (0.20 to 0.42) tracks a growing share of performative compliance that would, if it kept rising past functional activity, begin a drift toward theatrically maintained inertia. The decoupling-feasibility omega is the tripwire: if absolute decoupling fails at the required rate, the contingent victim set hardens and the balance shifts decisively toward the extractive pole.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates only the mitigation_priority reading of the climate_response_legitimacy kernel; which structural elements would change under the sibling readings?',
    'Comparative read of the sibling stories (adaptation_priority, degrowth_transformation): track how the victim set, beneficiary set, and epsilon referent shift when the growth-preservation premise or the mitigation-precedence premise is replaced.',
    'Under adaptation_priority the victim set moves to presently-exposed populations as definite rather than residual payers and resilience infrastructure enters the beneficiary set; under degrowth_transformation current wealthy-nation consumers move from beneficiary to payer and the growth-preservation axiom is denied. Classification of THIS arrangement is not stable across readings and must not be averaged across them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one-of-three readings; the disagreement is located in the growth-preservation premise and the mitigation-versus-adaptation precedence ordering.').

omega_variable(
    absolute_decoupling_feasibility,
    'Can absolute decoupling of GDP from territorial and consumption-based emissions proceed fast enough, at global scale, to hold warming near the levels the arrangement promises?',
    'Observed decoupling rates versus pathway requirements in successive IPCC assessments; consumption-based accounting in major economies; material throughput and rebound data.',
    'If infeasible at the required rate, future_generations shifts from contingent to definite victim, epsilon rises sharply, and the arrangement drifts from its current hybrid shape toward the purely extractive pole; if feasible, the coordination reading strengthens and the burden-allocation critique weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolute_decoupling_feasibility, empirical, 'Whether the arrangement''s core empirical wager — growth-compatible decarbonization at the required pace — holds.').

omega_variable(
    cdr_scale_up_moral_hazard,
    'Does the arrangement''s reliance on future carbon dioxide removal at gigatonne scale function as a hedge or as a license to defer cuts, and will removal technologies actually scale?',
    'Track realized removal deployment against net-zero accounting assumptions; compare cut ambition in portfolios with heavy removal reliance versus light reliance.',
    'If moral hazard dominates, the costs deferred onto future generations are larger than headline ambition suggests and the theater ratio understates performative compliance; if removal scales, part of the measured burden is the price of a real hedge rather than deferred cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cdr_scale_up_moral_hazard, empirical, 'Technological dependency risk: whether counted future removals materialize or merely license delay.').

omega_variable(
    suppression_mechanism_structure,
    'Is the suppression of rival readings structural (funding, venue, and credibility gatekeeping) or internalized (researchers and officials self-censor deviant framings as career-risking)?',
    'Post-exit trajectory: track whether researchers who leave the mainstream assessment circuit resume publishing heterodox framings at prior rates, and whether venue access alone explains output differences.',
    'If substantially internalized, effective suppression exceeds the observable gatekeeping measure and rival readings stay collapsed even where formal access opens; if structural, opening venues and funding lines would rapidly revive them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structure, empirical, 'Structural versus internalized suppression of degrowth and adaptation-first framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__mitigation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cr_mitigation_priority_tr_t0, climate_response_legitimacy__mitigation_priority, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(cr_mitigation_priority_tr_t0, observed).
narrative_ontology:measurement(cr_mitigation_priority_tr_t5, climate_response_legitimacy__mitigation_priority, theater_ratio, 5, 0.24).
narrative_ontology:measurement_basis(cr_mitigation_priority_tr_t5, observed).
narrative_ontology:measurement(cr_mitigation_priority_tr_t10, climate_response_legitimacy__mitigation_priority, theater_ratio, 10, 0.27).
narrative_ontology:measurement_basis(cr_mitigation_priority_tr_t10, observed).
narrative_ontology:measurement(cr_mitigation_priority_tr_t15, climate_response_legitimacy__mitigation_priority, theater_ratio, 15, 0.31).
narrative_ontology:measurement_basis(cr_mitigation_priority_tr_t15, observed).
narrative_ontology:measurement(cr_mitigation_priority_tr_t20, climate_response_legitimacy__mitigation_priority, theater_ratio, 20, 0.35).
narrative_ontology:measurement_basis(cr_mitigation_priority_tr_t20, observed).
narrative_ontology:measurement(cr_mitigation_priority_tr_t25, climate_response_legitimacy__mitigation_priority, theater_ratio, 25, 0.39).
narrative_ontology:measurement_basis(cr_mitigation_priority_tr_t25, observed).
narrative_ontology:measurement(cr_mitigation_priority_tr_t30, climate_response_legitimacy__mitigation_priority, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(cr_mitigation_priority_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(cr_mitigation_priority_be_t0, climate_response_legitimacy__mitigation_priority, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(cr_mitigation_priority_be_t0, observed).
narrative_ontology:measurement(cr_mitigation_priority_be_t5, climate_response_legitimacy__mitigation_priority, base_extractiveness, 5, 0.44).
narrative_ontology:measurement_basis(cr_mitigation_priority_be_t5, observed).
narrative_ontology:measurement(cr_mitigation_priority_be_t10, climate_response_legitimacy__mitigation_priority, base_extractiveness, 10, 0.47).
narrative_ontology:measurement_basis(cr_mitigation_priority_be_t10, observed).
narrative_ontology:measurement(cr_mitigation_priority_be_t15, climate_response_legitimacy__mitigation_priority, base_extractiveness, 15, 0.5).
narrative_ontology:measurement_basis(cr_mitigation_priority_be_t15, observed).
narrative_ontology:measurement(cr_mitigation_priority_be_t20, climate_response_legitimacy__mitigation_priority, base_extractiveness, 20, 0.53).
narrative_ontology:measurement_basis(cr_mitigation_priority_be_t20, observed).
narrative_ontology:measurement(cr_mitigation_priority_be_t25, climate_response_legitimacy__mitigation_priority, base_extractiveness, 25, 0.56).
narrative_ontology:measurement_basis(cr_mitigation_priority_be_t25, observed).
narrative_ontology:measurement(cr_mitigation_priority_be_t30, climate_response_legitimacy__mitigation_priority, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(cr_mitigation_priority_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(cr_mitigation_priority_su_t0, climate_response_legitimacy__mitigation_priority, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(cr_mitigation_priority_su_t0, observed).
narrative_ontology:measurement(cr_mitigation_priority_su_t5, climate_response_legitimacy__mitigation_priority, suppression_requirement, 5, 0.52).
narrative_ontology:measurement_basis(cr_mitigation_priority_su_t5, observed).
narrative_ontology:measurement(cr_mitigation_priority_su_t10, climate_response_legitimacy__mitigation_priority, suppression_requirement, 10, 0.46).
narrative_ontology:measurement_basis(cr_mitigation_priority_su_t10, observed).
narrative_ontology:measurement(cr_mitigation_priority_su_t15, climate_response_legitimacy__mitigation_priority, suppression_requirement, 15, 0.5).
narrative_ontology:measurement_basis(cr_mitigation_priority_su_t15, observed).
narrative_ontology:measurement(cr_mitigation_priority_su_t20, climate_response_legitimacy__mitigation_priority, suppression_requirement, 20, 0.54).
narrative_ontology:measurement_basis(cr_mitigation_priority_su_t20, observed).
narrative_ontology:measurement(cr_mitigation_priority_su_t25, climate_response_legitimacy__mitigation_priority, suppression_requirement, 25, 0.59).
narrative_ontology:measurement_basis(cr_mitigation_priority_su_t25, observed).
narrative_ontology:measurement(cr_mitigation_priority_su_t30, climate_response_legitimacy__mitigation_priority, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(cr_mitigation_priority_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__mitigation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy__degrowth_transformation).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: 'the legitimate climate response' is a colloquial label covering three structurally distinct arrangements. This file authors the mitigation_priority reading only. adaptation_priority carries a different victim set (presently-exposed populations as definite rather than residual payers) and different beneficiaries (resilience-infrastructure builders); degrowth_transformation denies the growth-preservation premise outright, moving current wealthy-nation consumers from beneficiary to payer. The three epsilon values differ because the arrangements differ — measuring 'climate legitimacy' with a single observable would average over distinct constraints. Edges: this reading's under-delivery structurally feeds adaptation demand, and its discursive dominance provoked the degrowth critique; at the axiom level this reading and degrowth_transformation are mutually foreclosing within any single framework, which the cs_structure block records.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
