% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__techno_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__techno_nationalist_reading, []).

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
 *   constraint_id: performance_legitimacy__techno_nationalist_reading
 *   human_readable: Techno-Nationalist Performance Legitimacy Compact
 *   domain: political economy/development planning/state capitalism
 *
 * SUMMARY:
 *   A governing party-state stakes its claim to rule on delivering
 *   technological self-sufficiency and leadership in designated strategic
 *   industries — semiconductors, artificial intelligence, aerospace, advanced
 *   manufacturing — as guarantees of national security and great-power
 *   standing. The arrangement runs through five-year plans and dedicated
 *   state funds that steer credit, land, and talent toward national
 *   champions; procurement preferences and retaliatory export controls wall
 *   off strategic markets; and milestone rhetoric converts engineering
 *   outcomes into political currency. The compact delivers real capability —
 *   electric vehicles, solar manufacturing, launch systems — while starving
 *   consumer sectors of capital, taxing households through administered-rate
 *   financial repression, and generating recurring subsidy-fraud scandals.
 *   Claimed here as tangled_rope: a genuine coordination function
 *   (capability-building under embargo threat that markets will not fund at
 *   security scale) operating through a structure that asymmetrically
 *   extracts from everyone outside the strategic perimeter. Time points 0-10
 *   map to 2015-2025, spanning the made-for-export industrial-plan era
 *   through the export-control escalation cycle.
 *
 * KEY AGENTS:
 *   - party_state_planners: Agenda-setter and legitimacy collector (institutional/identity_locked) — designs and enforces the compact; fused with it
 *   - national_champions: Primary beneficiary (powerful/arbitrage) — captures directed credit and protected procurement
 *   - defense_adjacent_tech_sectors: Secondary beneficiary (institutional/constrained) — guaranteed order books tied to milestones
 *   - urban_households: Primary target (moderate/trapped) — savings channeled into strategic lending, consumption suppressed
 *   - consumer_goods_sectors: Target (moderate/constrained) — capital starvation against prioritized borrowers
 *   - non_strategic_private_firms: Target (moderate/constrained) — credit discrimination off the strategic catalog
 *   - foreign_tech_suppliers: Excluded party (powerful/mobile) — progressively barred by substitution policy
 *   - development_economists: Analytical observer (analytical/analytical) — audits capability claims from outside the bargain
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__techno_nationalist_reading, 0.68).
domain_priors:suppression_score(performance_legitimacy__techno_nationalist_reading, 0.7).
domain_priors:theater_ratio(performance_legitimacy__techno_nationalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(performance_legitimacy__techno_nationalist_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__techno_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__techno_nationalist_reading, "Techno-Nationalist Performance Legitimacy Compact").
narrative_ontology:topic_domain(performance_legitimacy__techno_nationalist_reading, "political economy/development planning/state capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__techno_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__techno_nationalist_reading, 'fdf94d71-575a-40ed-a654-66c600a4f1ec').
narrative_ontology:cs_kernel_codification('fdf94d71-575a-40ed-a654-66c600a4f1ec', formalized).
narrative_ontology:cs_authority_grounding('fdf94d71-575a-40ed-a654-66c600a4f1ec', lineage).
narrative_ontology:cs_interpretation_layer_present('fdf94d71-575a-40ed-a654-66c600a4f1ec').
narrative_ontology:cs_reading_relation('fdf94d71-575a-40ed-a654-66c600a4f1ec', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('fdf94d71-575a-40ed-a654-66c600a4f1ec', performance_legitimacy__qualitative_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('fdf94d71-575a-40ed-a654-66c600a4f1ec', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_axiom('fdf94d71-575a-40ed-a654-66c600a4f1ec', foundational, technological_self_sufficiency_is_existential).
narrative_ontology:cs_axiom_status(technological_self_sufficiency_is_existential, holdable).
narrative_ontology:cs_axiom_grounding('fdf94d71-575a-40ed-a654-66c600a4f1ec', technological_self_sufficiency_is_existential, empirically_contingent).
narrative_ontology:cs_axiom('fdf94d71-575a-40ed-a654-66c600a4f1ec', foundational, market_signals_must_yield_to_strategic_priority).
narrative_ontology:cs_axiom_status(market_signals_must_yield_to_strategic_priority, holdable).
narrative_ontology:cs_axiom_grounding('fdf94d71-575a-40ed-a654-66c600a4f1ec', market_signals_must_yield_to_strategic_priority, instrumental).
narrative_ontology:cs_reference_frame('fdf94d71-575a-40ed-a654-66c600a4f1ec', strategic_autonomy_legitimacy_baseline).
narrative_ontology:cs_drift_state('fdf94d71-575a-40ed-a654-66c600a4f1ec', post_export_control_escalation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fdf94d71-575a-40ed-a654-66c600a4f1ec', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__techno_nationalist_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, national_champions).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, defense_adjacent_tech_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, party_state_planners).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, consumer_goods_sectors).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, urban_households).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, non_strategic_private_firms).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, foreign_tech_suppliers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, urban_households).
narrative_ontology:constraint_beneficiary(performance_legitimacy__techno_nationalist_reading, strategic_sector_engineers).
narrative_ontology:constraint_victim(performance_legitimacy__techno_nationalist_reading, strategic_sector_engineers).
narrative_ontology:constraint_vindicates(performance_legitimacy__techno_nationalist_reading, strategic_trade_theory).
narrative_ontology:constraint_vindicates(performance_legitimacy__techno_nationalist_reading, military_civil_fusion_doctrine).
narrative_ontology:constraint_vindicates(performance_legitimacy__techno_nationalist_reading, security_externalities_justify_market_override).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets five-year plans, designates the strategic-industry catalog, routes directed credit through state banks, and enforces procurement preferences and export-control countermeasures. Collects legitimacy persistence when milestones are met and suffers credibility loss when they fail. Its self-concept as guarantor of national rejuvenation is fused with the strategy, so retreating from it reads internally as betrayal of the national mission rather than as a policy correction.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, party_state_planners, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__techno_nationalist_reading, party_state_planners, beneficiary).

% Designated flagships in semiconductors, AI, aerospace, and electric vehicles receive below-market credit, subsidized land, and protected procurement. Losses are socialized through bailouts while gains accrue privately; they shop between ministries and provinces for the richest support packages and have no incentive to leave the designation system.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, national_champions, beneficiary,
    powerful, generational, arbitrage, global).

% Semiconductor equipment makers, aerospace primes, and dual-use suppliers hold guaranteed order books and R&D grants tied to self-sufficiency milestones. Embedded in military-civil fusion pipelines and clearance regimes, exiting means forfeiting the protected procurement relationship that constitutes their core business.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, defense_adjacent_tech_sectors, beneficiary,
    institutional, generational, constrained, global).

% Light manufacturing, retail, and services compete for capital against prioritized strategic borrowers, paying higher financing costs and absorbing policy neglect. Their demand base is domestic, so they cannot relocate away from the allocation squeeze; their recourse is quiet lobbying through industry associations.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, consumer_goods_sectors, payer,
    moderate, biographical, constrained, national).

% Household savings deposited in state banks fund strategic lending at administered rates; the investment-heavy allocation taxes consumption indirectly through property costs and thin safety nets. They receive status goods of national achievement and construction-cycle employment. Capital controls and household registration rules close the exit of moving savings or residence abroad.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, urban_households, payer,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__techno_nationalist_reading, urban_households, beneficiary).

% Private firms off the strategic catalog face credit discrimination, slower license approvals, and periodic regulatory attention. Some offshore production or listings, but scale, supplier networks, and domestic market access tie most of them to the system they are disadvantaged within.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, non_strategic_private_firms, payer,
    moderate, biographical, constrained, national).

% Multinationals supplying chips, machine tools, and industrial software face deliberate substitution and procurement localization that progressively bars them from the strategic market they helped build. They retain global customers elsewhere, so exit is costly but feasible; their countermove is export controls of their own.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, foreign_tech_suppliers, excluded,
    powerful, biographical, mobile, global).

% Engineers and researchers draw wage premiums, housing subsidies, and mission prestige inside strategic programs. They pay with extreme working hours, loyalty screening, and careers locked to whichever projects funding follows; taking a foreign offer invites suspicion of disloyalty.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, strategic_sector_engineers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__techno_nationalist_reading, strategic_sector_engineers, payer).

% Economists advocating consumption-led rebalancing and market-based allocation publish at the margins of official discourse and are sidelined from planning bodies. Their misallocation diagnoses circulate in internal briefings but never reach the agenda that sets the strategic catalog.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, liberal_market_economists, excluded,
    moderate, generational, constrained, national).

% External analysts track subsidy flows, patent quality, and total-factor productivity to judge whether directed investment builds durable capability or relocates rents. They hold no position in the domestic bargain and can compare outcomes across competing industrial-policy regimes.
narrative_ontology:constraint_stakeholder(performance_legitimacy__techno_nationalist_reading, development_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__techno_nationalist_reading, national_champions).
narrative_ontology:fixing_cost_class(performance_legitimacy__techno_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of building capabilities that markets will not fund at security-relevant scale: decades-long R&D horizons, dual-use infrastructure, and supply-chain redundancy against embargo are coordinated centrally because no individual firm can internalize the national-security externality.
% TRANSFER_FUNCTION: Moves capital (household savings via administered-rate state banking, fiscal funds, cheap land, and talent quotas) from consumer sectors and households to designated strategic industries; moves present consumption opportunities from households into future national capability and the balance sheets of national champions.
% ABSENT_VOICES: Consumer-welfare advocates and market-allocation economists have no seat in planning bodies; households affected by suppressed consumption are represented only statistically; foreign suppliers being substituted away have no voice in the councils deciding their exclusion.
% DISAPPEARANCE_RATIONALE: If the compact vanished overnight, capital would reprice toward consumption and services, national champions would lose directed credit and protected procurement, export-control postures would relax, and the regime would need a different basis for its claim to rule — the fiscal and institutional architecture built around strategic buildout would reorganize around whatever replaced it.
% FOUNDING_PROBLEM: Technological dependence on foreign suppliers was experienced as an existential vulnerability — chokepoints adversaries had demonstrated willingness to exploit — compounded by a great-power status deficit the regime had staked its standing on closing.
% FOUNDING_PROBLEM_CORROBORATION: The vulnerability is corroborated from outside the benefiting parties: foreign export-control actions themselves demonstrate that the dependence is exploitable, and multinational customers' diversification behavior attests the chokepoint reality. Whether the prescribed response — unlimited directed investment overriding market signals — matches the problem is disputed by development economists and market-allocation advocates outside the beneficiary set.
narrative_ontology:disappearance_verdict(performance_legitimacy__techno_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__techno_nationalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__techno_nationalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(performance_legitimacy__techno_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__techno_nationalist_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__techno_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__techno_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__techno_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.68: the transfer volume is enormous and decoupled from consumer welfare, but real capability is delivered, so this is not pure rent. Suppression is 0.70 as a raw structural property (unscaled by power or scope): capital controls, credit-allocation gates, licensing discretion, and loyalty screening do the coercive work, with a minority internalized component — mission belief and career fear among engineers and planners persists independently of any barrier. Theater ratio is 0.40 and rising: showcase labs, inflated self-sufficiency statistics, and repeated subsidy-fraud collapses coexist with genuine output. Accessibility collapse is 0.55: within official discourse the security frame forecloses market-allocation alternatives almost completely, but elite debate retains rival framings and firms retain partial arbitrage, so alternatives are suppressed rather than annihilated. Resistance is 0.45: quiet lobbying, defensive over-saving by households, economist dissent, and local gaming of targets — persistent friction without open defiance. The measurement series run on one shared grid (t=0,2,4,6,8,10) with all three metrics authored at every point; the suppression_requirement series is authored deliberately because enforcement machinery visibly hardened over the interval (retaliatory export controls, tightened capital controls, discipline campaigns) rather than holding static.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the planner seat the compact is civilizational necessity — the security frame admits no trade-off, and its identity lock makes the arrangement feel like reality rather than policy. From the champion seat it is a windfall machine. From the household and consumer-firm seats the identical structure operates as capital starvation and suppressed consumption levied for someone else's security. Same-level dynamics: national champions and non-strategic private firms are both large domestic firms, yet strategic-catalog designation alone splits them into beneficiary and payer — the differentiator is the listing, not size or competence. Coalition potential among the payer seats is weak: households are diffuse and unorganized, consumer firms compete with each other for the credit that remains, and the excluded economists hold no agenda seat, so the extraction meets friction but not combined opposition.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: national champions (arbitrage exit, subsidy capture) sit nearest the beneficiary pole; defense-adjacent sectors (constrained by procurement embedding) slightly less so; the planner seat collects legitimacy rather than cash but is structurally subsidized by the arrangement it administers. Targets derive high directionality: urban_households sit near full-target (trapped by capital controls and registration rules, no arbitrage path for their savings); consumer_goods_sectors and non_strategic_private_firms are damped slightly by constrained exit; foreign_tech_suppliers carry high nominal directionality but mobile exit damps their effective extraction. Global spatial scope on the planner, champion, and foreign-supplier seats raises verification difficulty, modestly amplifying effective extraction on those seats. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already reproduce the structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate is live, not atrophied: the founding problem (chokepoint exposure, status deficit) remains real and externally corroborated, there is no sunset clause, and enforcement is intensifying rather than decaying. The tangled_rope classification guards against both misreadings: calling this a pure snare erases the genuine coordination function — markets demonstrably underfund security-scale capability with decade-long horizons — while calling it a rope launders the asymmetric transfer, since consumer sacrifice is not a coordination cost anyone consented to. Mandatrophy becomes relevant only if the security premise collapses while the investment continues; the threat-proportionality omega tracks that boundary, and the rising theater_ratio series is the early-warning indicator.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_domination_under_resource_stress,
    'When growth or welfare shocks arrive, which reading of the performance_legitimacy kernel commands the budget — does the regime double down on strategic buildout or pivot fiscal space toward livelihood delivery?',
    'Compare crisis-period fiscal allocations and plan revisions against baseline trajectories: sustained strategic-share increases confirm this reading binds; pivots toward welfare and consumption indicate a sibling reading has taken the load.',
    'Determines whether this constraint or a sibling is the operative extraction structure; a livelihood pivot would loosen the consumer-sector extraction measured here and reclassify the binding constraint family member.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_domination_under_resource_stress, empirical, 'Which sibling reading of the performance-legitimacy kernel binds under budget stress.').

omega_variable(
    threat_proportionality,
    'Is the chokepoint vulnerability proportionate to the extraction it justifies, or is the security framing inflated beyond the demonstrable exposure?',
    'Independent assessment of actual import dependence per strategic sector against realized substitution outcomes and adversary exploitation history, audited outside the planning apparatus.',
    'If the threat is inflated, the genuine coordination component shrinks and the arrangement drifts toward snare; if proportionate, the coordination floor stands and tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_proportionality, empirical, 'Whether the security justification scales with real exposure or serves as extraction cover.').

omega_variable(
    capability_vs_theater_accounting,
    'What fraction of reported self-sufficiency progress is durable capability versus statistical inflation and subsidy-chasing theater?',
    'Audit-grade output measures — fab yield rates, export market shares, third-party certification — contrasted with announced milestones and disbursed subsidies, including forensic accounting of collapsed showcase projects.',
    'Sustained fraud discovery would push theater_ratio past the 0.5 threshold and date a Goodhart-drift transition; verified capability would cap the theater trajectory and stabilize the tangled_rope reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capability_vs_theater_accounting, empirical, 'Real-versus-performed share of measured self-sufficiency progress.').

omega_variable(
    planner_identity_lock_reversibility,
    'Is the planner seat''s commitment to the compact instrumental (revisable if its costs exceed its legitimacy yield) or identity-fused (exit unthinkable regardless of outcome)?',
    'Observe the response to costly public failures: policy revision and personnel rotation indicate instrumentality; scapegoating, redoubled investment, and narrative hardening indicate identity fusion.',
    'If fused, the constraint persists past its usefulness and the piton trajectory becomes live even while extraction continues; if instrumental, an elite bargain could unwind the compact without regime-level rupture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(planner_identity_lock_reversibility, conceptual, 'Instrumental versus identity-fused character of the planner seat''s commitment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__techno_nationalist_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pl_techno_nat_tr_t0, performance_legitimacy__techno_nationalist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(pl_techno_nat_tr_t2, performance_legitimacy__techno_nationalist_reading, theater_ratio, 2, 0.28).
narrative_ontology:measurement(pl_techno_nat_tr_t4, performance_legitimacy__techno_nationalist_reading, theater_ratio, 4, 0.32).
narrative_ontology:measurement(pl_techno_nat_tr_t6, performance_legitimacy__techno_nationalist_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement(pl_techno_nat_tr_t8, performance_legitimacy__techno_nationalist_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(pl_techno_nat_tr_t10, performance_legitimacy__techno_nationalist_reading, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(pl_techno_nat_be_t0, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(pl_techno_nat_be_t2, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 2, 0.56).
narrative_ontology:measurement(pl_techno_nat_be_t4, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 4, 0.6).
narrative_ontology:measurement(pl_techno_nat_be_t6, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 6, 0.63).
narrative_ontology:measurement(pl_techno_nat_be_t8, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 8, 0.66).
narrative_ontology:measurement(pl_techno_nat_be_t10, performance_legitimacy__techno_nationalist_reading, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(pl_techno_nat_su_t0, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(pl_techno_nat_su_t2, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 2, 0.62).
narrative_ontology:measurement(pl_techno_nat_su_t4, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 4, 0.66).
narrative_ontology:measurement(pl_techno_nat_su_t6, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 6, 0.69).
narrative_ontology:measurement(pl_techno_nat_su_t8, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 8, 0.72).
narrative_ontology:measurement(pl_techno_nat_su_t10, performance_legitimacy__techno_nationalist_reading, suppression_requirement, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__techno_nationalist_reading, resource_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__techno_nationalist_reading, livelihood_security_reading).

% DUAL FORMULATION NOTE:
% Performance legitimacy is a single colloquial label covering four structurally distinct legitimacy arrangements (epsilon-invariance decomposition). Each reading names a different performance metric, a different beneficiary set, and a different victim set: this techno-nationalist reading extracts from consumer sectors and households to subsidize strategic industries; the livelihood reading would distribute the same fiscal space as daily-life goods; the growth reading defends headline expansion; the qualitative-development reading redirects toward efficiency and sustainability. The stories form a constraint family linked through affects_constraints; upstream-downstream pressure runs from this reading into the growth and livelihood readings via budget competition, while the qualitative-development reading overlaps enough with this one that the two coexist as competing emphases rather than resource rivals.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
