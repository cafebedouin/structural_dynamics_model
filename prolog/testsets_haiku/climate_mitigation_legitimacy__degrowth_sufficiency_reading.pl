% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__degrowth_sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__degrowth_sufficiency_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__degrowth_sufficiency_reading
 *   human_readable: Decarbonization via Demand Reduction (Degrowth Sufficiency Reading)
 *   domain: energy/climate/economic-systems
 *
 * SUMMARY:
 *   This constraint is ONE reading of the contested kernel
 *   'climate_mitigation_legitimacy': the degrowth_sufficiency_reading. The
 *   reading instantiates a specific, structurally-distinct constraint story
 *   whose referent is the standing arrangement of growth-compatible
 *   decarbonization policy (the framework the reading contests). Under this
 *   reading, decarbonization requires absolute demand reduction, making
 *   large-scale generation expansion (nuclear or renewable) unnecessary. The
 *   reading privileges energy system downsizing and minimal new capital
 *   deployment. This is a tangled rope: it provides genuine coordination (a
 *   coherent interpretive frame for climate science, a coalition identity, a
 *   policy axis) AND extracts from those it subordinates (energy-intensive
 *   industries, nuclear developers, developing-economy growth aspirants). The
 *   reading's operation depends on active enforcement: suppressing or
 *   delegitimizing the competing framings (baseload_necessity,
 *   renewable_primacy, portfolio_pragmatism) and imposing demand-reduction
 *   metrics and carbon budgets that constrain growth-based infrastructure
 *   planning.
 *
 * KEY AGENTS:
 *   - degrowth_advocacy_coalitions: Agenda-setter; articulates and advances the reading; derives authority from it.
 *   - energy_intensive_industries: Payer; faces obsolescence or radical restructuring under the reading's sufficiency principle.
 *   - nuclear_power_sector: Payer; loses legitimacy for baseload generation expansion; becomes categorically disfavored.
 *   - incumbent_renewable_developers: Payer (constrained deployment) + Beneficiary (framing favoring renewables over nuclear); occupy an ambiguous seat.
 *   - electrical_grid_infrastructure_operators: Payer; forced to operate legacy systems at lower utilization or invest in demand-response infrastructure.
 *   - developing_economy_growth_aspirants: Payer (trapped); face binding constraint of energy-sufficiency principle globally; energy poverty becomes normalized.
 *   - conservation_technology_sectors: Beneficiary; market expands as conservation becomes primary policy objective.
 *   - renewable_equipment_manufacturers: Beneficiary (legitimacy) + Payer (constrained volume); gain framing but lose deployment scale.
 *   - climate_policy_authorities: Agenda-setter + Observer; their adoption of the reading determines its operationality.
 *   - baseload_necessity_advocates: Excluded; their core premise is pre-judged as growth-predicated; systematic exclusion from legitimate discourse.
 *   - growth_economy_analysts: Observer seat; measure whether the reading's claims hold empirically.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.68).
domain_priors:suppression_score(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.72).
domain_priors:theater_ratio(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "Decarbonization via Demand Reduction (Degrowth Sufficiency Reading)").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "energy/climate/economic-systems").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'b39efc92-1183-4792-8139-e2548b9bf009').
narrative_ontology:cs_kernel_codification('b39efc92-1183-4792-8139-e2548b9bf009', distributed).
narrative_ontology:cs_authority_grounding('b39efc92-1183-4792-8139-e2548b9bf009', extraction).
narrative_ontology:cs_reading_relation('b39efc92-1183-4792-8139-e2548b9bf009', climate_mitigation_legitimacy__baseload_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('b39efc92-1183-4792-8139-e2548b9bf009', climate_mitigation_legitimacy__renewable_primacy_reading, influences).
narrative_ontology:cs_reading_relation('b39efc92-1183-4792-8139-e2548b9bf009', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_axiom('b39efc92-1183-4792-8139-e2548b9bf009', foundational, growth_incompatible_with_decarbonization).
narrative_ontology:cs_axiom_status(growth_incompatible_with_decarbonization, holdable).
narrative_ontology:cs_axiom_grounding('b39efc92-1183-4792-8139-e2548b9bf009', growth_incompatible_with_decarbonization, empirically_contingent).
narrative_ontology:cs_axiom('b39efc92-1183-4792-8139-e2548b9bf009', foundational, sufficiency_principle_supersedes_expansion).
narrative_ontology:cs_axiom_status(sufficiency_principle_supersedes_expansion, holdable).
narrative_ontology:cs_axiom_grounding('b39efc92-1183-4792-8139-e2548b9bf009', sufficiency_principle_supersedes_expansion, deontological).
narrative_ontology:cs_reference_frame('b39efc92-1183-4792-8139-e2548b9bf009', growth_compatible_decarbonization_framework).
narrative_ontology:cs_drift_state('b39efc92-1183-4792-8139-e2548b9bf009', contemporary_climate_policy_crisis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b39efc92-1183-4792-8139-e2548b9bf009', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, degrowth_advocacy_coalitions).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_equipment_manufacturers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, conservation_technology_sectors).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_intensive_industries).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_power_sector).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, incumbent_renewable_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, electrical_grid_infrastructure_operators).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, developing_economy_growth_aspirants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, incumbent_renewable_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_equipment_manufacturers).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_demand_is_culturally_contingent).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__degrowth_sufficiency_reading, continuous_economic_growth_is_incompatible_with_decarbonization).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__degrowth_sufficiency_reading, sufficiency_principle_supersedes_efficiency_gains).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Articulate and advance the reading that decarbonization requires demand reduction and energy system downsizing. Frame growth as incompatible with climate targets. Set policy agendas in advocacy spaces, academic publications, and climate forums. Derive authority and influence from advancing this reading. Can exit from policy disappointments by shifting focus or strategy.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, degrowth_advocacy_coalitions, agenda_setter,
    organized, generational, mobile, global).

% Steel, cement, fertilizer, aluminum, petrochemical production sectors. Currently operate at energy intensity levels incompatible with the reading's sufficiency principle. Face regulatory pressure to reduce demand for their energy inputs and/or restructure production. Cannot exit their core industries without total business obsolescence. Hold political power but are constrained by climate policy adoption.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_intensive_industries, payer,
    powerful, biographical, constrained, national).

% Utilities, vendors, operators of nuclear generation. Under this reading, the value case for baseload generation—large-scale reliable power for growth—becomes incoherent. Face deprioritization in climate funding, policy hostility, and capital diversion to conservation. Their core business model (reliable baseload for expansion) is delegitimized. Constrained exit (they are locked into nuclear infrastructure).
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_power_sector, payer,
    institutional, generational, constrained, global).

% Wind and solar developers, renewable utilities. Benefit from the reading's favorable framing of renewables as the decarbonization pathway. Pay through constrained deployment mandates: the reading implies minimal new generation expansion, not explosive renewable build-out. Their capital deployment models assume rapid growth; the reading undermines that assumption. Constrained exit (they depend on policy favor for revenue).
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, incumbent_renewable_developers, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__degrowth_sufficiency_reading, incumbent_renewable_developers, payer).

% Transmission and distribution utilities whose revenue and planning models depend on demand growth and capacity expansion forecasts. Under this reading, demand reduction becomes the policy objective; new transmission lines and distribution capacity become illegitimate. Face pressure to maintain existing infrastructure at lower utilization or pivot to demand-response investment. Constrained exit (they are locked into grid operation).
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, electrical_grid_infrastructure_operators, payer,
    institutional, generational, constrained, national).

% Nations and populations seeking electrification, infrastructure expansion, and rising living standards. The reading's principle that decarbonization requires demand reduction (globally) subordinates their growth aspirations. They face pressure to accept energy poverty as permanent or to violate climate commitments. Cannot exit development needs or violate physical carbon budgets. Trapped and powerless under the reading's framework.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, developing_economy_growth_aspirants, payer,
    powerless, biographical, trapped, global).

% Efficiency retrofit, smart building, behavioral monitoring, energy management system vendors. Benefit from the reading's prioritization of conservation and demand management. Market expands as conservation becomes the primary policy lever. Can exit or pivot strategy if policy winds shift. Mobile and organized.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, conservation_technology_sectors, beneficiary,
    organized, biographical, mobile, global).

% Solar panel, wind turbine, battery, and storage manufacturers. Benefit from the reading's delegitimization of nuclear and framing of renewables as the decarbonization pathway. Pay through constrained deployment volume: the reading privileges minimal new generation expansion, not aggressive renewable deployment. Gain legitimacy but lose scale. Constrained exit (dependent on policy favor and energy markets).
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_equipment_manufacturers, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_equipment_manufacturers, payer).

% National governments, regional climate policy bodies, international climate bodies. Adopt (or resist) this reading as the operative policy frame. When adopted, they set carbon budgets, demand-reduction mandates, and energy rationing frameworks. Constrained exit (they operate within political constituencies and international agreements).
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_policy_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Energy modelers, systems analysts, utility planners, and some engineers who argue that reliable baseload generation at scale is necessary for decarbonization at current demand levels. Under this reading, their core argument is pre-judged as resting on unjustified growth assumptions. Systematically excluded from legitimate policy discourse. Trapped in their expertise but without policy voice.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, baseload_necessity_advocates, excluded,
    organized, generational, trapped, global).

% Economists, energy researchers, and policy analysts measuring whether the reading's claims hold empirically. Observe whether demand reduction is feasible at the scale required, whether technological decoupling is possible, and whether the reading's framing is durable or contested. Analytical seat, neither collecting nor paying.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, growth_economy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__degrowth_sufficiency_reading, degrowth_advocacy_coalitions).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__degrowth_sufficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes the interpretive frame that decarbonization's core requirement is absolute demand reduction, not generation technology selection. Coordinates a coalition around the claim that energy intensity is culturally contingent and that sufficiency (meeting needs at minimum energy intensity) can replace growth as the organizing principle. Solves the conceptual problem: 'How do we make sense of climate targets in a growth economy?' by answering 'We can't—decarbonization requires degrowth.'
% TRANSFER_FUNCTION: Moves legitimacy away from growth-dependent energy infrastructure (nuclear baseload, large renewable deployments) toward conservation and demand management. Transfers regulatory attention and capital allocation from generation capacity to efficiency retrofits, behavioral interventions, and energy rationing frameworks. Transfers authority to advocate coalitions and conservation sectors; away from energy companies and industrial sectors dependent on growth.
% ABSENT_VOICES: Baseload necessity advocates, development economists, and energy-poverty populations in developing economies are systematically excluded: the reading frames their arguments as resting on unjustified growth assumptions. Incumbent renewable developers and nuclear operators face de facto exclusion from legitimate policy discourse under this reading, though they formally remain present. Their objections are pre-judged as growth-motivated rather than substantively engaged.
% DISAPPEARANCE_RATIONALE: If this reading disappeared and the alternative readings (baseload_necessity, renewable_primacy, portfolio_pragmatism) returned to equal legitimacy, energy policy would revert to growth-compatible decarbonization targets, massive generation capacity expansion would become legitimate again, and capital would flow to nuclear and renewable build-outs rather than conservation and demand reduction. Industrial energy intensity would stabilize rather than reduce. The policy apparatus would restructure around capacity planning rather than demand management.
% FOUNDING_PROBLEM: Climate science established that carbon budgets are finite and non-negotiable; simultaneous economic growth and decarbonization appeared to be in tension. The reading answers: decarbonization and growth ARE incompatible; the founding problem is choosing: growth or decarbonization?
% FOUNDING_PROBLEM_CORROBORATION: Energy modeling by degrowth researchers and some climate scientists attests that absolute demand reduction is mathematically necessary given current carbon-intensity trajectories and available technology timelines (corroboration from outside the advocacy coalition exists in peer-reviewed modeling). Growth economists and energy analysts (outside the advocacy coalition) contest whether demand reduction at the scale required is politically feasible or whether technological decoupling of growth and emissions remains possible, particularly for developing economies. Development economists and energy-access advocates contest whether the reading's universal applicability can be justified given equity constraints.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__degrowth_sufficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__degrowth_sufficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68 at interval end) captures the reading's operation: it redistriutes legitimacy and capital away from growth-dependent energy infrastructure and toward conservation and demand management. The reading is not a simple coordination—it actively imposes costs on energy-intensive industries and constrains developing-economy options. Suppression is high (0.72) because the reading's persistence requires delegitimizing competing framings and suppressing growth-based energy forecasting within policy discourse. The constraint must actively maintain itself by excluding or marginalizing the baseload_necessity and portfolio_pragmatism readings. Theater_ratio rises over the measurement interval (0.28 → 0.41) as the reading matures: early (observed) measurements show the reading competing for legitimacy against incumbent framings; later (projected) measurements model a scenario where the reading achieves policy uptake and much of its enforcement becomes maintenance of the new interpretive frame—a growing ratio of ritual/symbolic operation ('we are a degrowth society') relative to material enforcement of demand reduction. The suppression curve flattens after t=15, suggesting the reading reaches a steady-state enforcement profile once adopted into policy. Accessibility_collapse (0.62) reflects that once the reading becomes the policy frame, alternatives (growth-compatible decarbonization) collapse as legitimate options within formal climate policy discourse—though they persist in competing framings held by excluded stakeholders. Resistance (0.78) is high: energy-intensive industries, nuclear sectors, development economies, and growth economists all mount substantial resistance; the reading does not emerge from consensual coordination.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (degrowth coalitions) and the payer seats (energy-intensive industries, nuclear, developing economies) compute radically different types from the same constraint. From the agenda-setter's seat, the reading is a rope—they are coordinating around a shared interpretive frame, solving the problem of how to make sense of climate science within a growth economy. From the payer seats, the reading is a snare or worse: it redistributes costs and legitimacy without their consent, constrains their options, and operates through suppression of alternatives. From the incumbent renewable developer's dual seat, the reading produces asymmetric benefits: legitimacy gain but volume loss. The engine computes per-seat type from the structural data; the authored claim (tangled_rope) reflects the aggregate structure, not any single seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Degrowth coalitions operate at d ≈ 0.1–0.2 (beneficiaries): the reading advances their legitimacy and policy influence; they have high exit options (mobile)—they can promote or abandon the reading. Energy-intensive industries sit near d ≈ 0.85–0.95 (full targets): the reading subordinates them; their exit is trapped (energy-intensive production is their core function; abandoning it is obsolescence). Nuclear sits near d ≈ 0.8 (high target): the reading delegitimizes baseload expansion; their constrained exit (they are locked into nuclear generation) makes them vulnerable. Incumbent renewable developers sit near d ≈ 0.4–0.6 (mixed): they benefit from the reading's favor toward renewables but lose from constrained deployment; their power level and modest exit options keep them near symmetric. Developing economies sit near d ≈ 0.85–0.95 (full targets): the reading's universal demand-reduction principle constrains their growth options; their trapped exit (they cannot exit development needs) and powerless position lock them as targets. Conservation technology sectors sit near d ≈ 0.15–0.25 (beneficiaries): the reading expands their market. The directionality structure creates a radically asymmetric constraint: powerful, organized beneficiaries (coalitions, conservation sectors) extract from powerful but constrained payers (energy-intensive industries, nuclear, renewable developers) and from powerless, trapped payers (developing economies).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is (reading-framed as): 'How do we achieve decarbonization in a growth economy?' The reading answers: 'We can't—decarbonization requires degrowth.' The founding_problem_status is 'contested' because growth economists and energy modelers outside the advocacy coalition argue that technological decoupling remains possible. The disappearance_verdict is 'world_rearranges' because the reading's adoption reshapes energy policy infrastructure, capital allocation, and industrial legitimacy. No mandatrophy is present—the founding problem (reconciling climate targets with growth) remains live and contested. The reading does not operate as a zombified constraint sustained by inertia; it is actively maintained through advocacy, policy adoption, and suppression of alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demand_reduction_feasibility,
    'Is the scale of demand reduction required by this reading—sufficient to eliminate growth-based generation expansion—politically and technologically feasible in practice?',
    'Real-world adoption in policy: observe whether jurisdictions that adopt the reading achieve the required demand reduction rates, or whether political resistance and behavioral lock-in limit reductions to levels that still require significant generation expansion.',
    'If feasible, the reading''s constraint strengthens and its extraction persists; if infeasible, the reading becomes theatrical (high theater_ratio) and pressure mounts to revert to competing framings (baseload_necessity, portfolio_pragmatism). Infeasibility would reclassify the constraint toward piton (degraded, inertially maintained).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(demand_reduction_feasibility, empirical, 'Whether the reading''s core claim about demand reduction feasibility holds under real implementation.').

omega_variable(
    technological_decoupling_possibility,
    'Is technological decoupling of economic growth and carbon emissions possible, even at 1.5°C targets? Or is decarbonization genuinely incompatible with growth, as the reading asserts?',
    'Empirical modeling and energy analysis from outside the advocacy coalition: can renewable energy plus efficiency gains plus electrification support continued economic growth while meeting carbon budgets?',
    'If decoupling is possible, the reading''s core axiom (growth is incompatible with decarbonization) is overridden, and the constraint weakens. If decoupling is ruled out, the axiom is vindicated and the reading''s legitimacy strengthens. The answer determines whether the reading forecloses or coexists with the baseload_necessity and portfolio_pragmatism readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_decoupling_possibility, empirical, 'Whether the reading''s foundational premise about growth-decarbonization incompatibility is empirically sound.').

omega_variable(
    sufficiency_principle_legitimacy,
    'Is the sufficiency principle (meeting needs at minimum energy intensity, rejecting growth as an organizing goal) a legitimate universal principle for climate policy, or does it rest on a particular moral framework that developing economies and growth-aspiring populations may rationally reject?',
    'Political economy analysis of developing-economy adoption and resistance to the reading; observation of whether the reading''s global applicability becomes contested along north-south equity lines.',
    'If the sufficiency principle is contested as culturally or economically contingent (rather than universal), the reading''s authority erodes. Developing economies that reject the principle would adopt competing readings (portfolio_pragmatism, renewable_primacy). The constraint''s extraction of legitimate growth aspirations becomes more visible and politically unsustainable. The reading''s type might shift from tangled_rope toward snare (pure extraction without coordination justification) if the sufficiency principle is seen as arbitrary imposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sufficiency_principle_legitimacy, preference, 'Whether the sufficiency principle is universal or culturally contingent; whether its imposition on developing economies is justified.').

omega_variable(
    committer_structure_degrowth_framing,
    'Is the degrowth reading a genuine structural insight into decarbonization requirements, or a reading that benefits advocacy coalitions by closing policy discourse and delegitimizing alternatives?',
    'Meta-analysis of the reading''s adoption patterns and its effects on policy diversity. Does adoption of the degrowth reading expand or contract the set of legitimate policy framings? Does it amplify or suppress dissent from baseload_necessity and portfolio_pragmatism advocates?',
    'If the reading is seen as advantageous primarily to advocacy coalitions and conservation sectors (not to those paying the costs), its classification may shift from tangled_rope (genuine coordination plus extraction) toward snare (pure extraction disguised as coordination). If the suppression of alternatives is revealed as primary to the reading''s operation, mandatrophy risks (the reading persists as zombie after its founding problem is solved) increase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_degrowth_framing, conceptual, 'Whether this reading is a genuine structural claim or an agenda-driven framing that benefits specific coalitions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 5, 0.33).
narrative_ontology:measurement_basis(clim_tr_t5, observed).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(clim_tr_t10, observed).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(clim_tr_t15, projected).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(clim_tr_t20, projected).
narrative_ontology:measurement(clim_tr_t25, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(clim_tr_t25, projected).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(clim_tr_t30, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(clim_be_t5, observed).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement_basis(clim_be_t10, observed).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(clim_be_t15, projected).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(clim_be_t20, projected).
narrative_ontology:measurement(clim_be_t25, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(clim_be_t25, projected).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(clim_be_t30, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(clim_su_t5, observed).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement_basis(clim_su_t10, observed).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement_basis(clim_su_t15, projected).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(clim_su_t20, projected).
narrative_ontology:measurement(clim_su_t25, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(clim_su_t25, projected).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(clim_su_t30, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__degrowth_sufficiency_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.22).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).

% DUAL FORMULATION NOTE:
% The climate_mitigation_legitimacy kernel contains four structurally distinct readings, each instantiating a different constraint story with different ε values, beneficiary/victim structures, and types. The degrowth_sufficiency_reading (this story) constrains growth-compatible energy expansion and privileges demand reduction. It forecloses or influences competing readings via its core axiom (growth is incompatible with decarbonization). The baseload_necessity_reading privileges dispatchable generation and influences the degrowth reading by asserting that demand reduction alone is insufficient. The portfolio_pragmatism reading influences both by claiming technology-neutral optimization can reconcile competing objectives. The renewable_primacy reading influences the degrowth reading by claiming renewables + storage can achieve decarbonization without demand reduction. All four stories share the same kernel but instantiate different constraints; they are linked via network.affects_constraints to enable contamination and genealogy analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_legitimacy__degrowth_sufficiency_reading, powerless, 0.92).
constraint_indexing:directionality_override(climate_mitigation_legitimacy__degrowth_sufficiency_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
