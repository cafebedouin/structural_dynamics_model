% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__mitigation_priority, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: climate_harm_prevention__mitigation_priority
 *   human_readable: Mitigation-Priority Climate Response Within Growth Framework
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint story represents the 'mitigation_priority' reading of the
 *   contested kernel 'climate_harm_prevention'. It asserts that legitimate
 *   climate response consists of prioritizing emissions reduction through
 *   technological transition (renewables, electrification, efficiency, carbon
 *   removal) within a continued economic growth framework. The constraint
 *   coordinates global action around carbon budgets and net-zero targets
 *   while extracting transition costs from the present generation —
 *   especially carbon-intensive sectors and their workers — for the benefit
 *   of future generations and climate-vulnerable populations. The
 *   claimed_type is tangled_rope: genuine coordination function (preventing
 *   catastrophic warming) combined with asymmetric extraction
 *   (intergenerational cost transfer) requiring active enforcement (carbon
 *   pricing, regulation, subsidy reform, trade measures).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__mitigation_priority, 0.68).
domain_priors:suppression_score(climate_harm_prevention__mitigation_priority, 0.62).
domain_priors:theater_ratio(climate_harm_prevention__mitigation_priority, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(climate_harm_prevention__mitigation_priority, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__mitigation_priority, "Mitigation-Priority Climate Response Within Growth Framework").
narrative_ontology:topic_domain(climate_harm_prevention__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__mitigation_priority, 'ac588646-3018-4545-8770-26c52ed77907').
narrative_ontology:cs_kernel_codification('ac588646-3018-4545-8770-26c52ed77907', formalized).
narrative_ontology:cs_authority_grounding('ac588646-3018-4545-8770-26c52ed77907', lineage).
narrative_ontology:cs_interpretation_layer_present('ac588646-3018-4545-8770-26c52ed77907').
narrative_ontology:cs_reading_relation('ac588646-3018-4545-8770-26c52ed77907', climate_harm_prevention__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('ac588646-3018-4545-8770-26c52ed77907', climate_harm_prevention__degrowth_reading, forecloses).
narrative_ontology:cs_axiom('ac588646-3018-4545-8770-26c52ed77907', foundational, growth_compatible_decarbonization_feasible).
narrative_ontology:cs_axiom_status(growth_compatible_decarbonization_feasible, holdable).
narrative_ontology:cs_axiom_grounding('ac588646-3018-4545-8770-26c52ed77907', growth_compatible_decarbonization_feasible, empirically_contingent).
narrative_ontology:cs_axiom('ac588646-3018-4545-8770-26c52ed77907', foundational, intergenerational_justice_requires_mitigation_priority).
narrative_ontology:cs_axiom_status(intergenerational_justice_requires_mitigation_priority, holdable).
narrative_ontology:cs_axiom_grounding('ac588646-3018-4545-8770-26c52ed77907', intergenerational_justice_requires_mitigation_priority, deontological).
narrative_ontology:cs_axiom('ac588646-3018-4545-8770-26c52ed77907', secondary, technological_transition_sufficient_for_harm_prevention).
narrative_ontology:cs_axiom_status(technological_transition_sufficient_for_harm_prevention, holdable).
narrative_ontology:cs_axiom_grounding('ac588646-3018-4545-8770-26c52ed77907', technological_transition_sufficient_for_harm_prevention, empirically_contingent).
narrative_ontology:cs_reference_frame('ac588646-3018-4545-8770-26c52ed77907', technological_mitigation_framework).
narrative_ontology:cs_drift_state('ac588646-3018-4545-8770-26c52ed77907', post_paris_implementation_gap_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ac588646-3018-4545-8770-26c52ed77907', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__mitigation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, climate_vulnerable_populations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__mitigation_priority, green_technology_sectors).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, carbon_intensive_sectors).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, fossil_fuel_workers).
narrative_ontology:constraint_victim(climate_harm_prevention__mitigation_priority, energy_intensive_industries).
narrative_ontology:constraint_vindicates(climate_harm_prevention__mitigation_priority, growth_compatible_decarbonization).
narrative_ontology:constraint_vindicates(climate_harm_prevention__mitigation_priority, technological_optimism).
narrative_ontology:constraint_vindicates(climate_harm_prevention__mitigation_priority, intergenerational_justice).
narrative_ontology:constraint_vindicates(climate_harm_prevention__mitigation_priority, carbon_budget_approach).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the consequences of climate harm if mitigation fails; benefit from avoided catastrophic warming. Cannot act, negotiate, or exit — their interests are represented only through present-day proxies. The constraint's legitimacy rests on preventing harm to this non-present party.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(climate_harm_prevention__mitigation_priority, future_generations).

% Current populations in low-lying, arid, or climate-exposed regions who benefit from avoided near-term warming. Have limited political voice internationally; exit options constrained by borders and poverty. Gain directly from mitigation that reduces extreme weather, sea-level rise, and agricultural disruption.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, climate_vulnerable_populations, beneficiary,
    moderate, biographical, constrained, global).

% Renewable energy, efficiency, electrification, and carbon removal industries that gain market share, policy support, and investment from mitigation prioritization. Actively lobby for stronger mitigation policy. Their profitability depends on the constraint's enforcement continuing and expanding.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, green_technology_sectors, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__mitigation_priority, green_technology_sectors, agenda_setter).

% Fossil fuel extraction, refining, and primary consumption industries (steel, cement, chemicals, aviation, shipping) that face stranded asset risk, carbon pricing, and regulatory phase-out. Deploy political influence to weaken enforcement, delay timelines, and secure transition subsidies. Exit means restructuring or dissolution; constrained by capital intensity and path dependence.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, carbon_intensive_sectors, payer,
    powerful, biographical, constrained, global).

% Workers and communities dependent on coal, oil, and gas employment. Bear concentrated transition costs (job loss, community degradation, identity loss) while mitigation benefits are diffuse. Exit options limited by geography, skills mismatch, and age; often politically mobilized against rapid phase-out. 'Just transition' rhetoric acknowledges their position but delivery is inconsistent.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, fossil_fuel_workers, payer,
    moderate, biographical, constrained, regional).

% Manufacturing sectors (steel, cement, chemicals, aluminum) facing high decarbonization costs and carbon leakage risk. Lobby for free allocations, border adjustments, and technology-neutral policies. Can partially pass costs to consumers but face competitiveness pressure. Exit means relocation or closure; constrained by supply chains and capital stock.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, energy_intensive_industries, payer,
    organized, biographical, constrained, global).

% Government officials, negotiators, and regulators who design and enforce mitigation policy (NDCs, carbon prices, standards, subsidies). Accountable to electorates and international processes. Their enforcement capacity depends on political coalition durability. They set the agenda but are constrained by the very sectors they regulate.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, climate_policy_makers, agenda_setter,
    institutional, generational, analytical, national).

% Nations with low historical emissions but high climate vulnerability and development needs. The mitigation-priority framework demands they forego fossil-fueled development while finance and technology transfer promises remain unmet. Their structural position — neither primary beneficiaries of current mitigation (which protects future generations) nor able to exit the climate regime — makes them systematically excluded from the constraint's benefit calculus.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, global_south_developing_nations, excluded,
    moderate, generational, trapped, global).

% Provide the physical basis for the harm-prevention claim (carbon budgets, warming trajectories, impact assessments). Their authority underpins the constraint's coordination function but they do not set policy. Structural position is outside the transfer — they neither pay nor collect, but their epistemic output defines the constraint's legitimacy.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__mitigation_priority, climate_scientists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the global collective-action problem of greenhouse gas accumulation by aligning national policies around a shared mitigation trajectory (net-zero by mid-century) that aims to limit warming to well-below 2°C, preventing catastrophic and irreversible harm to future generations.
% TRANSFER_FUNCTION: Moves transition costs (capital stranding, workforce displacement, higher energy prices during transition, green premiums) from future generations to the present generation — concentrated on carbon-intensive sectors and their workers — while moving benefits (avoided damages, stable climate, new industrial opportunities) to future generations and green technology sectors.
% ABSENT_VOICES: Global South developing nations (excluded stakeholder) would object to a framework that locks in mitigation priority without guaranteed finance, technology transfer, and carbon space for development. Their absence from effective decision-making in the UNFCCC consensus process means the constraint's coordination function reflects Global North priorities. Future generations are structurally absent — their interests are represented only by present-day advocates.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority constraint vanished overnight, nations would revert to uncoordinated energy security and economic growth strategies; fossil fuel expansion would resume; carbon pricing would collapse; green technology investment would lose policy certainty. The global energy system would reorganize around least-cost fossil resources, locking in higher warming. The Paris Agreement architecture would dissolve.
% FOUNDING_PROBLEM: Uncoordinated national emissions trajectories leading to catastrophic warming (>3-4°C) that would cause irreversible harm to future generations, ecosystem collapse, and civilizational instability — a classic intergenerational collective-action failure where each generation's rational short-term choice produces collective long-term ruin.
% FOUNDING_PROBLEM_CORROBORATION: IPCC assessments (outside the benefiting parties) corroborate the physical reality of the founding problem — cumulative emissions drive warming, and current trajectories exceed safe limits. However, the claim that growth-compatible technological transition can solve it at required speed is contested by degrowth advocates and some energy systems analysts. The operator (climate policy makers) attests the problem is live and the framework is working; fossil fuel interests attest the problem is exaggerated; vulnerable nations attest the problem is live but the solution is unjust.
narrative_ontology:disappearance_verdict(climate_harm_prevention__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__mitigation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_harm_prevention__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__mitigation_priority, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial: the present generation bears concentrated transition costs (stranded assets, workforce displacement, green premiums) while benefits accrue primarily to non-present future generations. The growth-compatible framing limits the extraction ceiling — if degrowth were required, extractiveness would be higher. Suppression (0.62) reflects the active enforcement needed: carbon pricing, regulatory phase-outs, subsidy removal, border carbon adjustments, and the political suppression of fossil fuel expansion. Theater ratio (0.42) is significant: net-zero pledges, distant targets, and technology optimism (CCS, hydrogen) perform coordination while near-term emissions continue rising. Accessibility collapse (0.55) is moderate: adaptation and degrowth alternatives remain discursively available but are structurally marginalized in official policy. Resistance (0.71) is high: from fossil fuel incumbents, worker communities, developing nations demanding carbon space, and political movements opposing carbon pricing.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (carbon-intensive sectors, fossil fuel workers, energy-intensive industries) experience this constraint as snare-like extraction — coordinated global policy dismantling their business models and communities. The beneficiary seats (future generations, vulnerable populations, green tech) experience it as rope-like coordination — solving a genuine collective-action problem. The agenda-setter seat (policy makers) experiences it as scaffold-like — a transitional arrangement meant to bridge to a decarbonized steady state, but without a declared sunset. The engine computes this divergence from the structural data; the authored claim (tangled_rope) acknowledges the hybrid nature without resolving the seat-level disagreement.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations are full beneficiaries (d ≈ 0.0) — they receive the subsidy of avoided catastrophe without paying transition costs. Climate-vulnerable populations are partial beneficiaries (d ≈ 0.2) — they gain avoided near-term harm but lack power to enforce the constraint. Green technology sectors are beneficiaries with agenda-setter power (d ≈ 0.15) — they collect rents from the transition while helping enforce it. Carbon-intensive sectors are full targets (d ≈ 0.9) — they pay concentrated costs with constrained exit. Fossil fuel workers are targets with identity_locked exit (d ≈ 0.85) — their community identity fuses with the industry, making exit psychologically and structurally difficult. Energy-intensive industries are targets with constrained exit (d ≈ 0.75). Climate policy makers sit near symmetric (d ≈ 0.5) — they administer the constraint and bear political costs but gain legitimacy. Global South nations are excluded (d ≈ 0.6) — they pay opportunity costs without full benefit access. Climate scientists are analytical observers (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate (prevent future harm via mitigation) remains live — the founding problem is uncontested physically. But the growth-compatible decarbonization pathway is increasingly contested as emissions accumulate and the carbon budget shrinks. If the technological transition fails to deliver at speed, the constraint's coordination function degrades while its extraction function persists — a classic mandatrophy trajectory toward snare or piton. The theater ratio rise (0.15→0.42) tracks this: enforcement increasingly performs ambition (net-zero pledges) while delivery lags. The constraint has not resolved its mandatrophy; it is in the contested zone where the founding problem persists but the solution's viability is disputed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the mitigation_priority reading a distinct constraint from its sibling readings, or do they form a single constraint with variable emphasis?',
    'Test ε-invariance: if measuring extractiveness under mitigation_priority (growth-compatible tech transition) yields a different ε than under degrowth_reading (planned contraction), they are distinct constraints. Decompose per ε-invariance principle.',
    'If distinct, each reading gets its own constraint story with independent classification. If unified, the constraint''s type becomes observer-relative — violating ε-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s readings instantiate structurally distinct constraints with different ε values.').

omega_variable(
    future_generations_beneficiary_status,
    'Can non-existent future generations be structural beneficiaries in the directionality derivation, or does their absence from the enforcement coalition make the constraint''s coordination function fictitious?',
    'Analyze whether present-day proxies (youth movements, legal guardianship doctrines, long-term institutional mandates) functionally represent future interests in the constraint''s enforcement, or whether the beneficiary claim is purely rhetorical.',
    'If future generations are not structurally represented, the constraint''s coordination function collapses — it becomes extraction from present sectors without a genuine beneficiary coalition, shifting classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_generations_beneficiary_status, conceptual, 'Whether future generations function as real beneficiaries in the constraint''s enforcement coalition.').

omega_variable(
    growth_compatibility_empirical_status,
    'Is growth-compatible decarbonization at the speed required by carbon budgets empirically feasible, or is the technological optimism axiom empirically contested?',
    'Track deployment rates of renewables, storage, electrification, and carbon removal against IPCC 1.5°C pathways. If deployment consistently lags required rates without policy correction, the axiom''s empirical grounding weakens.',
    'If growth-compatible decarbonization is empirically falsified, the mitigation_priority reading''s foundational axiom is overridden, and the constraint''s coordination function degrades — extraction persists without the coordination justification, shifting toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(growth_compatibility_empirical_status, empirical, 'Whether the technological transition assumption holds under empirical scrutiny.').

omega_variable(
    suppression_mechanism_mixture,
    'Is the constraint''s suppression structural (policy enforcement, carbon pricing, regulation) or does it include internalized suppression (climate guilt, greenwashing compliance, epistemic capture of climate discourse)?',
    'Post-policy-change observation: if carbon pricing is removed but corporations maintain net-zero commitments and individuals maintain low-carbon behaviors, internalized suppression is significant.',
    'If internalized suppression is substantial, the constraint''s effective suppression exceeds the structural measure — the constraint persists partly through internalized norms even if formal enforcement weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_mixture, empirical, 'Structural vs. internalized suppression mechanisms in climate policy adherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__mitigation_priority, 1990, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1990, climate_harm_prevention__mitigation_priority, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(clim_tr_t1997, climate_harm_prevention__mitigation_priority, theater_ratio, 1997, 0.22).
narrative_ontology:measurement(clim_tr_t2005, climate_harm_prevention__mitigation_priority, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(clim_tr_t2009, climate_harm_prevention__mitigation_priority, theater_ratio, 2009, 0.32).
narrative_ontology:measurement(clim_tr_t2015, climate_harm_prevention__mitigation_priority, theater_ratio, 2015, 0.35).
narrative_ontology:measurement(clim_tr_t2020, climate_harm_prevention__mitigation_priority, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(clim_tr_t2025, climate_harm_prevention__mitigation_priority, theater_ratio, 2025, 0.4).
narrative_ontology:measurement(clim_tr_t2030, climate_harm_prevention__mitigation_priority, theater_ratio, 2030, 0.42).

% Extraction over time
narrative_ontology:measurement(clim_be_t1990, climate_harm_prevention__mitigation_priority, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(clim_be_t1997, climate_harm_prevention__mitigation_priority, base_extractiveness, 1997, 0.38).
narrative_ontology:measurement(clim_be_t2005, climate_harm_prevention__mitigation_priority, base_extractiveness, 2005, 0.42).
narrative_ontology:measurement(clim_be_t2009, climate_harm_prevention__mitigation_priority, base_extractiveness, 2009, 0.48).
narrative_ontology:measurement(clim_be_t2015, climate_harm_prevention__mitigation_priority, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(clim_be_t2020, climate_harm_prevention__mitigation_priority, base_extractiveness, 2020, 0.61).
narrative_ontology:measurement(clim_be_t2025, climate_harm_prevention__mitigation_priority, base_extractiveness, 2025, 0.65).
narrative_ontology:measurement(clim_be_t2030, climate_harm_prevention__mitigation_priority, base_extractiveness, 2030, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1990, climate_harm_prevention__mitigation_priority, suppression_requirement, 1990, 0.25).
narrative_ontology:measurement(clim_su_t1997, climate_harm_prevention__mitigation_priority, suppression_requirement, 1997, 0.35).
narrative_ontology:measurement(clim_su_t2005, climate_harm_prevention__mitigation_priority, suppression_requirement, 2005, 0.42).
narrative_ontology:measurement(clim_su_t2009, climate_harm_prevention__mitigation_priority, suppression_requirement, 2009, 0.48).
narrative_ontology:measurement(clim_su_t2015, climate_harm_prevention__mitigation_priority, suppression_requirement, 2015, 0.52).
narrative_ontology:measurement(clim_su_t2020, climate_harm_prevention__mitigation_priority, suppression_requirement, 2020, 0.56).
narrative_ontology:measurement(clim_su_t2025, climate_harm_prevention__mitigation_priority, suppression_requirement, 2025, 0.59).
narrative_ontology:measurement(clim_su_t2030, climate_harm_prevention__mitigation_priority, suppression_requirement, 2030, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__mitigation_priority, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(climate_harm_prevention__mitigation_priority, 0.12).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, carbon_pricing_mechanisms).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, renewable_energy_deployment_mandates).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, fossil_fuel_subsidy_reform).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, just_transition_policy_framework).
narrative_ontology:affects_constraint(climate_harm_prevention__mitigation_priority, climate_finance_obligations).

% DUAL FORMULATION NOTE:
% This constraint (mitigation_priority) and its siblings (adaptation_priority, degrowth_reading) form the climate_harm_prevention constraint family. They share the kernel 'prevent climate harm' but instantiate different ε values: mitigation_priority has moderate-high ε (intergenerational transfer); adaptation_priority has lower ε (present-generation cost-sharing) but higher residual harm; degrowth_reading has high ε (present-generation contraction) but different beneficiary structure (present vulnerable populations over future generations). The mitigation_priority reading influences the others by consuming political capital and finance that could flow to adaptation or structural reform.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_harm_prevention__mitigation_priority, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
