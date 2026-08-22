% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__qualitative_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__qualitative_development_reading, []).

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
 *   constraint_id: performance_legitimacy__qualitative_development_reading
 *   human_readable: High-Quality Development Legitimacy Criterion (Qualitative Reading of Performance Legitimacy)
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   A party-state re-grounds its performance legitimacy from aggregate output
 *   growth to 'high-quality development': innovation intensity, efficiency
 *   gains, and sustainability replace raw GDP as the scoreboard on which
 *   cadres, banks, and firms are graded. The re-grading is enforced through
 *   cadre evaluation reform, directed credit, guidance funds, and regulatory
 *   campaigns against property leverage and 'backward capacity'. KEY AGENTS
 *   (by structural relationship): central_party_planning_authority — agenda
 *   setter (institutional/arbitrage), defines and enforces the quality
 *   scoreboard; high_tech_sector_firms — primary beneficiary
 *   (powerful/constrained), receives directed capital and protected demand;
 *   state_guidance_fund_operators — secondary beneficiary
 *   (institutional/arbitrage), administers the allocation channel;
 *   traditional_manufacturing_firms — primary target (moderate/trapped),
 *   bears credit starvation and consolidation;
 *   property_dependent_local_governments — target
 *   (institutional/constrained), lost land finance, kept the mandates;
 *   property_developers — target (powerful/trapped), deleveraged under
 *   leverage caps; urban_households — dual-positioned
 *   (organized/constrained), promised upside, carrying property-wealth
 *   losses; foreign_trade_partners — excluded voice (institutional/mobile),
 *   absorbs exported overcapacity; independent_development_economists —
 *   analytical observer. This file instantiates ONE reading of the
 *   performance_legitimacy kernel; the epsilon referent is the standing
 *   quality-development arrangement itself, assessed by this reading's own
 *   lights — not the growth-first arrangement a sibling reading would defend.
 *   Claim and metrics are authored independently: the reading claims a
 *   coordination-forward transformation mandate; the metrics describe
 *   substantially extractive, actively enforced operation with growing metric
 *   theater.
 *
 * KEY AGENTS:
 *   - central_party_planning_authority: agenda setter (institutional/arbitrage) — owns the scoreboard, grades the graders
 *   - high_tech_sector_firms: primary beneficiary (powerful/constrained) — collects directed credit, subsidies, procurement
 *   - state_guidance_fund_operators: secondary beneficiary (institutional/arbitrage) — intermediates strategic capital, scales with flow
 *   - traditional_manufacturing_firms: primary target (moderate/trapped) — bears credit starvation, energy caps, consolidation
 *   - property_dependent_local_governments: target (institutional/constrained) — land finance collapsed, mandates retained
 *   - property_developers: target (powerful/trapped) — leverage caps forced distressed deleveraging
 *   - urban_households: dual-positioned beneficiary/payer (organized/constrained) — aspirational gains, property-wealth losses
 *   - foreign_trade_partners: excluded (institutional/mobile) — object to subsidy-driven overcapacity from outside
 *   - independent_development_economists: analytical observer (analytical/analytical) — audit metrics the system does not grade
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, 0.58).
domain_priors:suppression_score(performance_legitimacy__qualitative_development_reading, 0.62).
domain_priors:theater_ratio(performance_legitimacy__qualitative_development_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__qualitative_development_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__qualitative_development_reading, "High-Quality Development Legitimacy Criterion (Qualitative Reading of Performance Legitimacy)").
narrative_ontology:topic_domain(performance_legitimacy__qualitative_development_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__qualitative_development_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__qualitative_development_reading, '9f6ea9d3-e818-48d1-84d3-73b718fc6b1f').
narrative_ontology:cs_kernel_codification('9f6ea9d3-e818-48d1-84d3-73b718fc6b1f', formalized).
narrative_ontology:cs_authority_grounding('9f6ea9d3-e818-48d1-84d3-73b718fc6b1f', lineage).
narrative_ontology:cs_interpretation_layer_present('9f6ea9d3-e818-48d1-84d3-73b718fc6b1f').
narrative_ontology:cs_reading_relation('9f6ea9d3-e818-48d1-84d3-73b718fc6b1f', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('9f6ea9d3-e818-48d1-84d3-73b718fc6b1f', performance_legitimacy__techno_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('9f6ea9d3-e818-48d1-84d3-73b718fc6b1f', performance_legitimacy__livelihood_security_reading, coexists_with).
narrative_ontology:cs_axiom('9f6ea9d3-e818-48d1-84d3-73b718fc6b1f', foundational, structural_transformation_outweighs_output_volume).
narrative_ontology:cs_axiom_status(structural_transformation_outweighs_output_volume, holdable).
narrative_ontology:cs_axiom_grounding('9f6ea9d3-e818-48d1-84d3-73b718fc6b1f', structural_transformation_outweighs_output_volume, empirically_contingent).
narrative_ontology:cs_axiom('9f6ea9d3-e818-48d1-84d3-73b718fc6b1f', secondary, directed_innovation_capital_is_public_investment).
narrative_ontology:cs_axiom_status(directed_innovation_capital_is_public_investment, holdable).
narrative_ontology:cs_axiom_grounding('9f6ea9d3-e818-48d1-84d3-73b718fc6b1f', directed_innovation_capital_is_public_investment, instrumental).
narrative_ontology:cs_reference_frame('9f6ea9d3-e818-48d1-84d3-73b718fc6b1f', innovation_led_structural_transformation).
narrative_ontology:cs_drift_state('9f6ea9d3-e818-48d1-84d3-73b718fc6b1f', contemporary_property_downturn, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9f6ea9d3-e818-48d1-84d3-73b718fc6b1f', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__qualitative_development_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, high_tech_sector_firms).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, state_guidance_fund_operators).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, traditional_manufacturing_firms).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, property_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, urban_households).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, urban_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines the scoreboard by which provinces, ministries, and SOE executives are graded: R&D intensity, patent and self-sufficiency ratios, energy-intensity and carbon targets, total-factor-productivity language in five-year plans. Rewrites cadre evaluation formulas, directs bank credit toward strategic sectors, and launches regulatory campaigns against activity it classifies as low-quality. Its own authority is staked on the claim that it can identify and deliver structural transformation.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, central_party_planning_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Semiconductor, AI, battery, electric-vehicle, and solar firms receive subsidized credit below market rates, tax breaks, government procurement, protected home markets, and priority in listing and M&A approval. Their business models are entangled with the state support channel: exiting the favored circle means forfeiting the subsidy stream and facing the same credit conditions as everyone else.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, high_tech_sector_firms, beneficiary,
    powerful, biographical, constrained, global).

% Operate the government guidance funds and 'big funds' that intermediate directed capital into strategic industries. Fund scale, fee income, and career advancement grow with each new allocation round; they help decide which technologies count as strategic, and face no market test on capital preservation comparable to private LPs.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, state_guidance_fund_operators, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__qualitative_development_reading, state_guidance_fund_operators, agenda_setter).

% Low-end export manufacturers face tightening credit, energy-consumption caps, environmental enforcement, and consolidation pressure framed as clearing out 'backward capacity'. Moving up the value chain requires capital and technology they cannot access under the new allocation rules; their existing margins depend on the cheap credit and land that the arrangement now routes elsewhere.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, traditional_manufacturing_firms, payer,
    moderate, biographical, trapped, global).

% Built budgets and off-balance-sheet financing vehicles on land-sale revenue; the developer deleveraging campaign collapsed that revenue while service mandates, payroll, and debt service remained. They cannot refuse the KPI system, cannot repudiate debt, and now compete for central transfers and approved project lists. Careers of officials promoted under the old GDP-tournament formula lose their currency when the scoreboard changes.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments, payer,
    institutional, biographical, constrained, regional).

% Subject to leverage caps ('three red lines') that cut refinancing access; assets must be sold into a falling market to meet obligations, and defaults cascade through contractors, wealth-management products, and presold housing. Their prior political weight bought no exemption once the arrangement reclassified their sector as the obstacle to quality development.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, property_developers, payer,
    powerful, immediate, trapped, national).

% Are promised the upside of the transformation: cleaner air, higher-value jobs, better public services funded by a broader tax base. In the interim they hold most household wealth in property whose price the deleveraging deliberately suppresses, and their employment sits in both the contracting old sectors and the still-small new ones. Capital controls limit their exit to offshore assets.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, urban_households, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__qualitative_development_reading, urban_households, payer).

% Import economies absorbing subsidized output in EVs, solar, and steel argue the arrangement exports overcapacity and de-industrializes their manufacturing bases. They are not party to the domestic conversation that sets the allocation rules; their available responses are tariffs, anti-subsidy investigations, and supply-chain diversification.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, foreign_trade_partners, excluded,
    institutional, generational, mobile, global).

% Assess from outside whether measured innovation indicators reflect real productivity gains or metric production: citation-quality analysis of patents, TFP accounting, fund-return audits, and comparison of announced self-sufficiency against import data. Publish findings the domestic scoreboard does not have to grade.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, independent_development_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__qualitative_development_reading, high_tech_sector_firms).
narrative_ontology:fixing_cost_class(performance_legitimacy__qualitative_development_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a collective-action problem the old scoreboard created: when every locality was graded on GDP, none could unilaterally stop courting smokestack projects, land sales, and credit-fueled construction without losing the tournament. Changing the shared scoreboard to innovation, efficiency, and sustainability metrics lets the center move capital, talent, and official attention toward activities with long payoffs that no individual locality would rationally fund alone.
% TRANSFER_FUNCTION: Moves subsidized credit, fiscal transfers, procurement, listing approvals, and regulatory forbearance from traditional manufacturing and property-linked activity toward strategic high-tech sectors and the state innovation ecosystem; moves land-finance discretion from local governments to central control; moves adjustment costs onto property developers, their creditors, and households holding property wealth.
% ABSENT_VOICES: Foreign trade partners absorb the overcapacity the arrangement generates but sit outside the conversation that sets it. Urban households bear suppressed property wealth and transition unemployment without a direct seat. Growth-first officials inside the system retain formal channels but lost agenda-setting power after the scoreboard changed; their objections surface mainly as quiet non-compliance rather than argument.
% DISAPPEARANCE_RATIONALE: If the quality-development criterion vanished overnight, cadre evaluation would revert to GDP tournament competition, bank credit would reflow to property and low-end manufacturing within quarters, the guidance-fund apparatus would lose its allocation mandate, and the innovation buildout would shrink to commercially viable islands. The underlying debt, demographic, and chokepoint pressures would then reassert themselves against a restored volume-growth machine.
% FOUNDING_PROBLEM: The input-driven growth model was exhausting itself: debt accumulation outran output gains, the working-age population peaked, environmental ceilings bound, export volume growth slowed, and chokepoint dependencies in semiconductors and core technologies became strategic liabilities. The arrangement was built to force a transition to factor-productivity-led growth before stagnation arrived.
% FOUNDING_PROBLEM_CORROBORATION: The underlying problem is corroborated from outside the benefiting parties: IMF Article IV consultations, World Bank growth and aging analyses, published demographic statistics, and independent TFP accounting all document the exhaustion dynamics. Fiscal distress reported by property-dependent local governments themselves attests the cost side. What is NOT independently corroborated is the specific claim that the center's chosen instrument mix delivers the transformation — that framing is authored by the center grading itself, and the observer seat exists precisely because no external body certifies the remedy.
narrative_ontology:disappearance_verdict(performance_legitimacy__qualitative_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__qualitative_development_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__qualitative_development_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(performance_legitimacy__qualitative_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__qualitative_development_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__qualitative_development_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__qualitative_development_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58: the arrangement moves large, asymmetric flows (below-market credit, procurement, listing priority) to favored sectors while imposing credit starvation, energy caps, and forced deleveraging on identifiable payers — but genuine industrial upgrading does occur alongside the transfer, so epsilon sits well below pure-extraction levels. Suppression is 0.62 and structural, not interpersonal: political hierarchy, credit control, and campaign-style regulation close alternatives; there is no internalized-suppression ambiguity of the interpersonal kind. Theater_ratio is 0.40 and rising: patent counts, R&D ratios, and showcase 'little giant' designations are partially gamed (quantity-over-quality patents, subsidy-driven shell innovation), while real capability gains in EV, battery, and solar supply chains keep the ratio below majority-theater. Accessibility_collapse is 0.50: market allocation and growth-first politics persist as live alternatives but are penalized rather than impossible. Resistance is 0.45: local governments manipulate data and accumulate hidden debt, traditional manufacturers lobby through associations, developers litigate restructuring terms — but open defiance of the scoreboard is rare given hierarchical discipline. Coalition potential among the three payer seats is limited: manufacturers want cheaper land and labor, local governments want land-finance restoration, developers want refinancing — their remedies conflict, and vertical discipline raises coordination costs. All three temporal series share one six-point grid; the final values equal the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat the arrangement is a deliberate, survivable transformation program it designed and can revise. From the beneficiary seats it is a subsidy landscape to be navigated. From the payer seats the same structure operates as extraction: traditional manufacturers see credit they priced into their business models withdrawn by rule change; property-dependent local governments see the revenue engine confiscated while the obligations stayed. Identity-lock sharpens the divergence for mid-career officials whose promotions were earned under the GDP formula — the scoreboard change strands their accumulated signaling capital, so they experience the new criterion as illegitimate goalpost-moving rather than correction. The engine computes these per-seat classifications from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place high_tech_sector_firms and state_guidance_fund_operators near the beneficiary end (low d, damped or negative effective extraction): they receive the flows the arrangement directs. Victim declarations place traditional_manufacturing_firms, property_dependent_local_governments, and property_developers near the target end (high d, amplified extraction), with exit modulation doing real work: manufacturers are trapped (asset specificity, thin margins), developers trapped (unsellable inventory), while local governments are institutionally powerful yet constrained — their power does not buy exit because the counterparty is the same hierarchy that grants it. The center sits nearest the beneficiary pole via agenda-setting and arbitrage-grade exit (it writes the rules it is graded on). No directionality_overrides are authored: the two institutional seats (center vs. local governments) differ in role and exit options, so the structural derivation distinguishes them without override; adding a power-atom-keyed override would collide across those seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents mislabeling in both directions. Reading the arrangement as pure coordination (the official framing: necessary transition, everyone sacrifices for the future) erases the identifiable payers whose losses are permanent, not transitional — credit starved from traditional manufacturing does not return if the sector never recovers. Reading it as pure extraction (crony capitalism picking winners) erases the real collective-action achievement: no locality would have exited the GDP tournament unilaterally, and the scoreboard change did redirect measurable capital and talent toward activities with long payoffs. Mandatrophy is NOT resolved: the founding problem (model exhaustion) is live, so the arrangement has not outlived its function — the R5 mismatch consumer should find status=live paired with verdict=world_rearranges, no zombie flag. The open question the omegas carry is whether the extraction component decays as the transition completes (rope-like) or hardens as the favored sectors acquire defenders (snare-drift).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_selection_underdetermination,
    'Is the operative legitimacy criterion actually the qualitative reading, or a hybrid in which a growth floor silently dominates whenever employment or fiscal stress spikes?',
    'Observe crisis-period behavior: which metric triggers stimulus rounds, which officials are promoted after downturns, whether property easing recurs despite contradicting the quality frame. Revealed preference under stress discriminates the binding criterion from the proclaimed one.',
    'If growth floors dominate in stress periods, this story describes a fair-weather constraint and the effective arrangement is closer to the quantitative_growth_reading''s structure — epsilon, beneficiaries, and victims all shift toward that sibling''s profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_underdetermination, empirical, 'Whether the qualitative reading or a latent growth floor is the binding legitimacy test.').

omega_variable(
    metric_gaming_vs_real_capability,
    'How much of the measured innovation record (patent counts, R&D intensity, self-sufficiency announcements) reflects real productive capability versus metric production aimed at the scoreboard?',
    'Citation-quality and triadic-patent analysis, TFP accounting independent of official statistics, audit of guidance-fund portfolio returns, and performance testing of claimed strategic capabilities under operational conditions.',
    'High gaming drives theater_ratio past 0.5 and pushes the computed classification toward piton-flavored maintenance of appearances; verified capability gains confirm the coordination half and stabilize the tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_gaming_vs_real_capability, empirical, 'Real-upgrading versus metric-theater composition of the transformation record.').

omega_variable(
    transition_cost_permanence,
    'Are the costs imposed on traditional manufacturing and property-dependent local governments temporary transition costs that decay, or a permanent structural transfer that hardens?',
    'Longitudinal tracking: do displaced sectors recover or consolidate away; does fiscal recentralization compensate localities durably or leave structural holes; do former payers gain access to the favored channel or stay excluded?',
    'Decaying costs support a rope-dominant trajectory for the arrangement; permanent exclusion with entrenched favored-sector defenders marks drift toward snare structure with the same nominal frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_cost_permanence, empirical, 'Whether extraction component decays with transition completion or persists as structural transfer.').

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the performance_legitimacy kernel; the sibling readings (quantitative_growth_reading, techno_nationalist_reading, livelihood_security_reading) would each relocate the entire beneficiary/victim structure — where exactly is the disagreement located, and which structural element does each sibling modify?',
    'Comparative analysis across the four reading files: the disagreement locates in which performance dimension constitutes the binding test of legitimacy, which propagates to different victim sets, different enforcement machinery, and different tolerance for short-term pain. No dataset resolves it; it resolves politically, when a faction holding one reading captures the scoreboard.',
    'If a sibling reading captures the scoreboard, this constraint''s epsilon referent dissolves and its classification retires with it; the corpus should track all four readings as parallel files linked by network edges, never merged into one averaged constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: which performance dimension binds, and what each sibling reading would change structurally.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__qualitative_development_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__qualitative_development_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(perf_tr_t6, performance_legitimacy__qualitative_development_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement(perf_tr_t12, performance_legitimacy__qualitative_development_reading, theater_ratio, 12, 0.31).
narrative_ontology:measurement(perf_tr_t18, performance_legitimacy__qualitative_development_reading, theater_ratio, 18, 0.35).
narrative_ontology:measurement(perf_tr_t24, performance_legitimacy__qualitative_development_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(perf_tr_t30, performance_legitimacy__qualitative_development_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__qualitative_development_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(perf_be_t6, performance_legitimacy__qualitative_development_reading, base_extractiveness, 6, 0.44).
narrative_ontology:measurement(perf_be_t12, performance_legitimacy__qualitative_development_reading, base_extractiveness, 12, 0.49).
narrative_ontology:measurement(perf_be_t18, performance_legitimacy__qualitative_development_reading, base_extractiveness, 18, 0.52).
narrative_ontology:measurement(perf_be_t24, performance_legitimacy__qualitative_development_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(perf_be_t30, performance_legitimacy__qualitative_development_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__qualitative_development_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(perf_su_t6, performance_legitimacy__qualitative_development_reading, suppression_requirement, 6, 0.54).
narrative_ontology:measurement(perf_su_t12, performance_legitimacy__qualitative_development_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(perf_su_t18, performance_legitimacy__qualitative_development_reading, suppression_requirement, 18, 0.6).
narrative_ontology:measurement(perf_su_t24, performance_legitimacy__qualitative_development_reading, suppression_requirement, 24, 0.61).
narrative_ontology:measurement(perf_su_t30, performance_legitimacy__qualitative_development_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__qualitative_development_reading, resource_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__techno_nationalist_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__livelihood_security_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the performance_legitimacy kernel. The colloquial label 'performance legitimacy' conflates four structurally distinct legitimacy tests — aggregate growth, structural quality, technological self-sufficiency, daily-life delivery — each with its own epsilon, beneficiary/victim structure, and enforcement machinery. This file is the qualitative_development_reading only. Family edges run from this reading to all three siblings: the quality scoreboard's resource commitments reshape the operating environment of each sibling (starving the growth reading of KPI primacy, feeding the techno-nationalist reading's infrastructure, squeezing the livelihood reading's fiscal base) without logically eliminating any of them. Sibling files should carry reciprocal edges and their own epsilons; no story in this family may average across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
