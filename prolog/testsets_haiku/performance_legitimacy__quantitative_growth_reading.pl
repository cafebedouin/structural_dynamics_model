% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__quantitative_growth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__quantitative_growth_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: performance_legitimacy__quantitative_growth_reading
 *   human_readable: Performance Legitimacy via Quantitative GDP Growth
 *   domain: political_economy/state_capitalism/development_planning
 *
 * SUMMARY:
 *   A state apparatus grounds its political legitimacy in demonstrating
 *   sustained GDP growth rates (historically 6-8%+), operationalized as the
 *   primary metric for judging official performance and allocating
 *   investment. Provinces and local governments are measured on growth
 *   achievement; promotion is contingent on hitting targets. The constraint
 *   coordinates economic actors (investors, manufacturers, workers,
 *   officials) around expansionary objectives and transfers environmental
 *   costs, labor suppression, and agricultural divestment to rural
 *   populations, workers, and the global commons while concentrating credit
 *   access and promotional opportunity among industrial-export beneficiaries
 *   and growth-measured officials. This story instantiates ONE READING of a
 *   contested legitimacy kernel: the quantitative-growth reading. Alternative
 *   readings (livelihood-security, high-quality-development,
 *   techno-nationalist) are separate constraints with different
 *   beneficiaries, different measurement metrics, and different institutional
 *   implementations. This constraint's ε reflects the growth-reading's
 *   structural extraction: the beneficiary seat (state planning apparatus and
 *   industrial-export complex) collects legitimacy and investment rent; the
 *   victim seats (rural, manufacturing workers, environmental commons) bear
 *   suppressed wages, resource degradation, and futures claims. Theater rises
 *   over the interval as growth targets become harder to meet naturally and
 *   data manipulation intensifies (Goodhart drift).
 *
 * KEY AGENTS:
 *   - state_planning_apparatus: Agenda-setter; designs and enforces growth targets; career advancement of officials is contingent on target achievement; institutional power with trapped exit (cannot exit the legitimacy game without abdicating authority).
 *   - industrial_export_complex: Primary beneficiary; receives preferential credit, infrastructure investment, regulatory flexibility; has arbitrage exit (can relocate to rival countries); powerful but exit option means suppression is lower than for trapped seats.
 *   - local_government_officials: Dual-positioned beneficiary/payer; benefit from growth-driven revenue and promotion but are trapped by identity (a cadre's legitimacy is inseparable from growth achievement); their exit is identity_locked, not merely constrained.
 *   - rural_agricultural_populations: Victim; lose land to industrial zones and export infrastructure; underfunded relative to industrial investment; powerless with constrained exit (rural-to-urban migration is structural, not choice).
 *   - low_wage_manufacturing_workers: Victim; generate growth statistics but wages lag productivity; labor mobility is controlled (hukou system); moderate power but constrained exit makes suppression high.
 *   - environmental_commons_users: Victim class; bear pollution, water degradation, carbon emissions from export-oriented manufacturing; powerless and trapped (cannot exit from atmospheric/hydrological systems).
 *   - international_development_institutions: Observer seat; monitoring and reporting practices embed the growth-as-legitimacy equation globally, reinforcing this reading's institutional authority.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, 0.68).
domain_priors:suppression_score(performance_legitimacy__quantitative_growth_reading, 0.71).
domain_priors:theater_ratio(performance_legitimacy__quantitative_growth_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__quantitative_growth_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__quantitative_growth_reading, "Performance Legitimacy via Quantitative GDP Growth").
narrative_ontology:topic_domain(performance_legitimacy__quantitative_growth_reading, "political_economy/state_capitalism/development_planning").

domain_priors:requires_active_enforcement(performance_legitimacy__quantitative_growth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__quantitative_growth_reading, '7ab61d46-25ea-4e8e-a7ec-fac8a1d1044a').
narrative_ontology:cs_kernel_codification('7ab61d46-25ea-4e8e-a7ec-fac8a1d1044a', formalized).
narrative_ontology:cs_authority_grounding('7ab61d46-25ea-4e8e-a7ec-fac8a1d1044a', extraction).
narrative_ontology:cs_interpretation_layer_present('7ab61d46-25ea-4e8e-a7ec-fac8a1d1044a').
narrative_ontology:cs_reading_relation('7ab61d46-25ea-4e8e-a7ec-fac8a1d1044a', performance_legitimacy__livelihood_security_reading, coexists_with).
narrative_ontology:cs_reading_relation('7ab61d46-25ea-4e8e-a7ec-fac8a1d1044a', performance_legitimacy__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('7ab61d46-25ea-4e8e-a7ec-fac8a1d1044a', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_axiom('7ab61d46-25ea-4e8e-a7ec-fac8a1d1044a', foundational, economic_expansion_is_legitimacy).
narrative_ontology:cs_axiom_status(economic_expansion_is_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('7ab61d46-25ea-4e8e-a7ec-fac8a1d1044a', economic_expansion_is_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('7ab61d46-25ea-4e8e-a7ec-fac8a1d1044a', foundational, growth_rates_are_objective_development_measure).
narrative_ontology:cs_axiom_status(growth_rates_are_objective_development_measure, overridden).
narrative_ontology:cs_axiom_grounding('7ab61d46-25ea-4e8e-a7ec-fac8a1d1044a', growth_rates_are_objective_development_measure, conventional).
narrative_ontology:cs_reference_frame('7ab61d46-25ea-4e8e-a7ec-fac8a1d1044a', growth_as_legitimacy_metric).
narrative_ontology:cs_drift_state('7ab61d46-25ea-4e8e-a7ec-fac8a1d1044a', contemporary_mature_economy, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7ab61d46-25ea-4e8e-a7ec-fac8a1d1044a', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__quantitative_growth_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, industrial_export_complex).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, local_government_officials).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, state_planning_apparatus).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, rural_agricultural_populations).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, low_wage_manufacturing_workers).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, environmental_commons_users).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, local_government_officials).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs five-year plans and macroeconomic policy targeting sustained GDP growth rates (historically 6-8%+). Measures provincial and local officials on growth achievement; promotion and career advancement are contingent on hitting or exceeding targets. Controls credit allocation, investment priorities, and state-owned enterprise directives to maintain growth. Views growth as the foundational legitimacy metric for the political system itself.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, state_planning_apparatus, agenda_setter,
    institutional, generational, trapped, national).

% Large manufacturers, exporters, and their supply chains receive preferential credit, infrastructure investment, land access, and regulatory flexibility. Benefit from heavy investment in export processing zones, port infrastructure, and special economic zones. Can relocate production to rival countries if domestic costs rise; exit option mitigates their suppression relative to other payers.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, industrial_export_complex, beneficiary,
    powerful, biographical, arbitrage, global).

% Measured and promoted by provincial and central targets on GDP growth within their jurisdictions. Respond by driving local investment projects, industrial zones, and construction booms. Benefit from revenue streams tied to growth (land sales, tax revenue from new enterprises, promotional opportunities). Trapped by career identity: a government official's legitimacy is inseparable from hitting growth targets; failing to grow is read as administrative failure, not a conscious policy choice.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, local_government_officials, beneficiary,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__quantitative_growth_reading, local_government_officials, payer).

% Land is requisitioned or degraded for industrial zones, export infrastructure, and urban expansion to service growth targets. Agricultural investment and rural services are chronically underfunded relative to industrial and export-oriented spending. Rural-to-urban migration is driven by growth-optimized investment patterns rather than choice. Limited political voice in planning processes; their interests are aggregated only as 'labor supply' in growth accounting.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, rural_agricultural_populations, payer,
    powerless, biographical, constrained, regional).

% Work in the expanding manufacturing and export sectors that generate growth statistics. Wages remain suppressed relative to productivity gains because labor mobility is controlled (hukou system, spatial barriers) and growth targets require cost minimization. Labor regulations are selectively enforced to prevent wage pressure that would reduce export competitiveness. Nominally employed in 'job creation' that counts toward legitimacy, but real wages lag growth rates.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, low_wage_manufacturing_workers, payer,
    moderate, biographical, constrained, regional).

% Bear the costs of heavy industrial pollution, water degradation, and resource extraction required to sustain growth rates. Environmental protection regulations are subordinated to growth targets when they conflict. Carbon emissions embedded in export-oriented manufacturing are not allocated to domestic growth accounting but are borne as planetary externality. No seat at the planning table; interests represented only in aggregate environmental impact data that does not feedback into growth-target setting.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, environmental_commons_users, payer,
    powerless, civilizational, trapped, global).

% Inherit accumulated environmental debt, depleted resource stocks, and climate instability from growth-maximizing present-day extraction. Not present in planning processes; their interests are theoretically carried by sustainability clauses that are subordinated in practice to growth targets.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, future_generations, excluded,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(performance_legitimacy__quantitative_growth_reading, future_generations).

% Policy advocates and economists arguing for livelihood-security or high-quality-development framings are sidelined from central planning authority. Academic and policy research funding flows disproportionately to growth-accounting research. Alternative paradigms influence discourse but lack enforcement machinery within the state planning apparatus.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, competing_development_paradigms, excluded,
    moderate, generational, constrained, national).

% Monitor and report on growth rates, structural transformation, and development progress. GDP growth remains the primary metric in development rankings and conditionality instruments, reinforcing the quantitative reading as the globally legitimate framework. Their reporting practices embed and perpetuate the growth-as-legitimacy equation.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, international_development_institutions, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of translating abstract state legitimacy into a quantifiable, measurable target; coordinates millions of distributed economic actors (provincial governments, enterprises, workers, investors) toward a common expansionary objective. Provides a single lingua franca for resource allocation and official performance evaluation: if an activity drives GDP growth, it receives credit and funding; if it does not, it is subordinated. Eliminates the need to debate what 'development' means — the metric is objective and comparable across jurisdictions.
% TRANSFER_FUNCTION: Moves environmental costs (pollution, water degradation, carbon emissions), agricultural divestment (land requisition, underfunded rural services), labor suppression (wages lagging productivity, controlled mobility), and deferred futures claims (depleted resource stocks, accumulated climate debt) to rural populations, manufacturing workers, and environmental commons. Simultaneously moves credit access, infrastructure investment, regulatory flexibility, and promotional opportunity to the industrial-export complex and growth-measured local officials. The constraint transfers growth achievement from those who realize it (workers, manufacturers) to those measured by it (officials, planners).
% ABSENT_VOICES: Rural populations are excluded from land-use and resource-allocation decisions affecting them. Manufacturing workers have no voice in wage-setting or labor-standards enforcement — their interests are represented only as 'job creation' in growth accounting. Environmental commons users have no seat in growth-target negotiations. Advocates of alternative development readings (livelihood-security, quality development) are marginalized from central planning authority. Competing paradigms influence discourse but lack enforcement machinery.
% DISAPPEARANCE_RATIONALE: If the GDP-growth legitimacy constraint vanished, the state would be forced to adopt alternative legitimacy metrics; provincial officials would be measured on different targets; investment patterns would rebalance from export-oriented manufacturing toward rural services, healthcare, education, and environmental restoration; the industrial-export complex would lose preferential credit access and regulatory privilege; workers would gain bargaining power as labor suppression mechanisms no longer serve growth optimization; environmental regulation would be elevated from subordinate to primary; the entire architecture of resource allocation and state performance measurement would reorganize around a different metric.
% FOUNDING_PROBLEM: In the 1980s–1990s, a state transitioning from central planning and communist ideological claims to market mechanisms needed a new legitimacy basis to justify the transition and prove the system was delivering material improvement. Demonstrable economic expansion and job creation provided visible, measurable proof that reform was working and justified the political system's continued rule. GDP growth became the scoreboard on which to judge whether the transition was succeeding.
% FOUNDING_PROBLEM_CORROBORATION: State planning apparatus attests the founding problem remains live, citing the need for continuous growth to absorb rural migration and provide employment. Economists and livelihood-security advocates attest the founding problem is substantially solved — material living standards have risen ~40-fold over four decades, the economy is now among the world's largest, and poverty has declined dramatically; growth targets now persist as institutional inertia and as the enforcement mechanism by which officials maintain control over resource allocation, not as the solution to the founding legitimacy crisis. Independent development research from World Bank, UNDP, and academic economists documents the decoupling of GDP growth from livelihood improvement and environmental sustainability, supporting the contested framing that the original problem is solved but the constraint persists.
narrative_ontology:disappearance_verdict(performance_legitimacy__quantitative_growth_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__quantitative_growth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__quantitative_growth_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(performance_legitimacy__quantitative_growth_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__quantitative_growth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__quantitative_growth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts at 0.38 (early-stage transition period when growth delivered genuine material improvement) and rises to 0.68 (plateau at 0.25-0.40 interval when growth targets become harder to meet naturally and data falsification increases). The trajectory models rent-seeking layering onto an initially genuine coordination function. Suppression requirement rises from 0.48 to 0.71 because maintaining growth targets against natural economic maturation requires active enforcement: control of labor mobility, environmental regulation subordination, land requisition, and suppression of competing development narratives. Theater ratio rises from 0.18 to 0.42, indicating Goodhart drift — as actual growth slows, more official activity goes into statistical manipulation, local project theater (building and abandoning infrastructure), and public relations rather than real value creation. The measurements are authored on one shared grid with all three metrics at all nine time points. The coercion_grid models leveled pressure: structural-level suppression rises most sharply (growth targets are enforced through administrative hierarchy); class-level suppression (on agricultural and manufacturing working classes) also rises sharply; individual resistance declines as identity-lock and economic dependence tighten. Organizational-level suppression of competing paradigms also rises as the planning apparatus consolidates authority.
 *
 * PERSPECTIVAL GAP:
 *   The state-planning-apparatus seat (agenda-setter) experiences this constraint as legitimate coordination: a proven mechanism for achieving material expansion and organizing economic actors. The victim seats (rural populations, workers, environmental commons) experience it as coercive extraction: targets achieved by shifting costs to the powerless, not by genuine productivity. The industrial-export-complex seat experiences it as beneficial but recognizes the arbitrage exit, so suppression-felt is lower than for identity-locked officials. The measurement divergence is the signal: a rope solving a genuine coordination problem would show rising theater_ratio only if corruption increased post-hoc; this constraint shows theater rising IN SYNC with extractiveness, suggesting the performance metrics themselves are becoming the primary object rather than measures of real progress. The per-seat perception should diverge: the planning apparatus computes as rope (coordination with minor rents); the victim seats compute as snare (pure extraction defended by data control); the industrial-export complex computes as tangled_rope (genuine coordination benefit, but extraction ride-along on labor and environmental suppression).
 *
 * DIRECTIONALITY LOGIC:
 *   State-planning apparatus: d near 0.1-0.15 (full beneficiary — sets the targets, controls the measurement, gains legitimacy from the system itself; no exit option, so trapped beneficiary is unusual but not impossible). Industrial-export complex: d near 0.2-0.3 (substantial beneficiary, receives preferential allocation, but arbitrage exit option moderates their full-target status). Local government officials: d near 0.45-0.55 (symmetric or slightly extracting: they benefit from promotion and growth-driven revenue, but are simultaneously trapped and measured, so they absorb pressure from both state planning and victim seats). Rural populations: d near 0.75-0.85 (full target: bear land loss, environmental costs, labor suppression, with constrained exit). Manufacturing workers: d near 0.70-0.80 (full target: wages suppressed, mobility controlled, labor absorbed into growth statistics; identity_locked exit means suppression persists even after exit). Environmental commons: d near 0.9 (full target: no exit, no compensation, cumulative extraction). No directionality overrides are needed; the structural derivation chain (beneficiary/victim + power + exit) produces the correct per-seat d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimacy crisis of a transitioning state in the 1980s-1990s) is substantially solved — GDP has expanded 30-fold, material living standards have risen dramatically, the political system's capacity for resource mobilization is proven. Yet the constraint persists, with extraction rising as growth targets become harder to meet naturally. The measured theater_ratio rise (0.18 → 0.42) indicates Goodhart drift: the measurement (growth) has become decoupled from the measured-phenomenon (genuine material improvement). This is the signature of mandatrophy: the constraint's original coordination function (translate legitimacy claims into objective achievements) is increasingly replaced by theatrical maintenance of the metric itself. The measured mismatch (rising extractiveness + rising theater while actual growth slows) is diagnostic of a constraint at the rope/tangled-rope boundary moving toward snare. The constraint requires active enforcement to maintain high growth targets that are no longer naturally achievable — this enforcement is what holds extractiveness high. Without active suppression (labor control, environmental subordination, data manipulation), extractiveness would collapse. Mandatrophy is LIVE: the founding problem is dead, the constraint persists as institutional inertia, and the apparatus has begun extracting via measurement control rather than genuine coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_coordination_vs_extraction_boundary,
    'What share of the measured extractiveness is the necessary coordination cost of organizing millions of distributed economic actors versus what share is monopoly rent collected by the planning apparatus and industrial-export complex?',
    'Comparative analysis of institutional coordination costs in economies using alternative legitimacy metrics (livelihood, quality development); counterfactual modeling of growth targets at lower extraction levels; analysis of how much enforcement intensity and data manipulation would be unnecessary if targets were calibrated to actual productive capacity.',
    'A tight cost-to-coordination ratio would support the rope classification (necessary overhead); a loose ratio would confirm tangled_rope or snare classification (extraction ride-along). This determines whether the constraint can be reformed by recalibrating targets or requires replacement of the entire legitimacy metric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_coordination_vs_extraction_boundary, empirical, 'Whether extractiveness tracks genuine coordination cost or accumulating monopoly rent.').

omega_variable(
    identity_lock_mechanism_local_officials,
    'Is the measured exit_options=identity_locked status for local government officials a structural feature or an internalized belief pattern that could shift if the central planning apparatus changed signals?',
    'Natural experiments from instances where central planning apparatus has reduced growth-target emphasis (COVID periods, environmental crackdowns) and observed whether local official behavior and self-concept shift; post-career interview data from retired officials on whether identity remained growth-focused after exit from role.',
    'If identity-lock is structural (career path dependence so complete that it persists post-role), then officials are true victims even as beneficiaries. If identity-lock is internalized but reversible, then the suppression experienced is partly self-perpetuating (omega_suppression_internalization case). This affects whether reclassifying officials as payer-side (rather than beneficiary-side) is warranted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_local_officials, empirical, 'Whether local-official identity-lock is structural or internalized/reversible.').

omega_variable(
    quantitative_growth_foreclosure_of_alternatives,
    'Does the quantitative-growth reading logically foreclose the livelihood-security reading, or do they coexist as genuinely alternative frames that different institutional actors can hold simultaneously?',
    'Analysis of whether the two readings'' core premises directly contradict each other (growth might be achieved without livelihood improvement; livelihood improvement might be achieved without GDP growth). If both are possible, readings coexist; if one necessarily excludes the other, one forecloses the other.',
    'If quantitative_growth FORECLOSES livelihood_security within a single institutional framework, then adopting the livelihood metric would require rejecting growth-measured legitimacy entirely. If they coexist, alternative readings can be held by competing political factions without logical contradiction. This determines whether the kernel contest is a fundamental either-or or a distribution-of-emphasis question.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(quantitative_growth_foreclosure_of_alternatives, conceptual, 'Logical relationship between growth and livelihood legitimacy readings.').

omega_variable(
    theater_ratio_measurement_artifact,
    'Does the rising theater_ratio (0.18 → 0.42) reflect genuine Goodhart drift (the measurement is becoming decoupled from reality and officials are adapting by focusing on the metric rather than the measured phenomenon) or is it an artifact of increased measurement transparency (better data availability makes theatrical elements more visible)?',
    'Longitudinal comparison of growth-statistics quality, audit reports on data falsification, and official task-allocation over the interval. If theater rises while underlying data quality remains stable, measurement-artifact hypothesis; if theater rises with statistical anomalies and audit findings, Goodhart drift is confirmed.',
    'If Goodhart drift is confirmed, the theater rise is diagnostic of mandatrophy: the constraint is evolving from coordination (genuine growth) toward extraction (metric performance). If measurement-artifact, the rising theater ratio is a reporting artifact and does not indicate functional degradation of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_measurement_artifact, empirical, 'Whether theater-ratio rise reflects Goodhart drift or measurement transparency.').

omega_variable(
    environmental_commons_internalization_of_suppression,
    'The environmental commons is suppressed structurally (no seat, no exit, no voice). Is there a mechanism by which environmental-destruction patterns become internalized — citizens cease to perceive resource degradation as suppression and accept it as natural or inevitable?',
    'Survey data on citizen perception of environmental quality over time; analysis of whether environmental-concern activism rises or falls despite (or because of) degradation; observation of whether environmental-protection framing shifts from rights-claiming to adaptation rhetoric.',
    'If internalization is significant, the suppression metric understates the constraint''s effective extraction because the suppression persists even after awareness. The constraint is more pernicious than the scalar suppression score indicates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(environmental_commons_internalization_of_suppression, empirical, 'Whether environmental suppression is structural only or partly internalized/normalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__quantitative_growth_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__quantitative_growth_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(perf_tr_t5, performance_legitimacy__quantitative_growth_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(perf_tr_t10, performance_legitimacy__quantitative_growth_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(perf_tr_t15, performance_legitimacy__quantitative_growth_reading, theater_ratio, 15, 0.31).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__quantitative_growth_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(perf_tr_t25, performance_legitimacy__quantitative_growth_reading, theater_ratio, 25, 0.39).
narrative_ontology:measurement(perf_tr_t30, performance_legitimacy__quantitative_growth_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(perf_tr_t35, performance_legitimacy__quantitative_growth_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement(perf_tr_t40, performance_legitimacy__quantitative_growth_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(perf_be_t5, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(perf_be_t10, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(perf_be_t15, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(perf_be_t25, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 25, 0.65).
narrative_ontology:measurement(perf_be_t30, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement(perf_be_t35, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(perf_be_t40, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(perf_su_t5, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(perf_su_t10, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(perf_su_t15, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 15, 0.61).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(perf_su_t25, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 25, 0.68).
narrative_ontology:measurement(perf_su_t30, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(perf_su_t35, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement(perf_su_t40, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 40, 0.71).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(perf_grid_01, performance_legitimacy__quantitative_growth_reading, accessibility_collapse(class), 0, 0.55).
narrative_ontology:measurement(perf_grid_02, performance_legitimacy__quantitative_growth_reading, accessibility_collapse(class), 40, 0.62).
narrative_ontology:measurement(perf_grid_03, performance_legitimacy__quantitative_growth_reading, accessibility_collapse(individual), 0, 0.42).
narrative_ontology:measurement(perf_grid_04, performance_legitimacy__quantitative_growth_reading, accessibility_collapse(individual), 40, 0.48).
narrative_ontology:measurement(perf_grid_05, performance_legitimacy__quantitative_growth_reading, accessibility_collapse(organizational), 0, 0.45).
narrative_ontology:measurement(perf_grid_06, performance_legitimacy__quantitative_growth_reading, accessibility_collapse(organizational), 40, 0.58).
narrative_ontology:measurement(perf_grid_07, performance_legitimacy__quantitative_growth_reading, accessibility_collapse(structural), 0, 0.38).
narrative_ontology:measurement(perf_grid_08, performance_legitimacy__quantitative_growth_reading, accessibility_collapse(structural), 40, 0.52).
narrative_ontology:measurement(perf_grid_09, performance_legitimacy__quantitative_growth_reading, resistance(class), 0, 0.42).
narrative_ontology:measurement(perf_grid_10, performance_legitimacy__quantitative_growth_reading, resistance(class), 40, 0.38).
narrative_ontology:measurement(perf_grid_11, performance_legitimacy__quantitative_growth_reading, resistance(individual), 0, 0.35).
narrative_ontology:measurement(perf_grid_12, performance_legitimacy__quantitative_growth_reading, resistance(individual), 40, 0.32).
narrative_ontology:measurement(perf_grid_13, performance_legitimacy__quantitative_growth_reading, resistance(organizational), 0, 0.58).
narrative_ontology:measurement(perf_grid_14, performance_legitimacy__quantitative_growth_reading, resistance(organizational), 40, 0.48).
narrative_ontology:measurement(perf_grid_15, performance_legitimacy__quantitative_growth_reading, resistance(structural), 0, 0.52).
narrative_ontology:measurement(perf_grid_16, performance_legitimacy__quantitative_growth_reading, resistance(structural), 40, 0.45).
narrative_ontology:measurement(perf_grid_17, performance_legitimacy__quantitative_growth_reading, stakes_inflation(class), 0, 0.48).
narrative_ontology:measurement(perf_grid_18, performance_legitimacy__quantitative_growth_reading, stakes_inflation(class), 40, 0.65).
narrative_ontology:measurement(perf_grid_19, performance_legitimacy__quantitative_growth_reading, stakes_inflation(individual), 0, 0.32).
narrative_ontology:measurement(perf_grid_20, performance_legitimacy__quantitative_growth_reading, stakes_inflation(individual), 40, 0.41).
narrative_ontology:measurement(perf_grid_21, performance_legitimacy__quantitative_growth_reading, stakes_inflation(organizational), 0, 0.52).
narrative_ontology:measurement(perf_grid_22, performance_legitimacy__quantitative_growth_reading, stakes_inflation(organizational), 40, 0.72).
narrative_ontology:measurement(perf_grid_23, performance_legitimacy__quantitative_growth_reading, stakes_inflation(structural), 0, 0.35).
narrative_ontology:measurement(perf_grid_24, performance_legitimacy__quantitative_growth_reading, stakes_inflation(structural), 40, 0.58).
narrative_ontology:measurement(perf_grid_25, performance_legitimacy__quantitative_growth_reading, suppression(class), 0, 0.58).
narrative_ontology:measurement(perf_grid_26, performance_legitimacy__quantitative_growth_reading, suppression(class), 40, 0.78).
narrative_ontology:measurement(perf_grid_27, performance_legitimacy__quantitative_growth_reading, suppression(individual), 0, 0.48).
narrative_ontology:measurement(perf_grid_28, performance_legitimacy__quantitative_growth_reading, suppression(individual), 40, 0.62).
narrative_ontology:measurement(perf_grid_29, performance_legitimacy__quantitative_growth_reading, suppression(organizational), 0, 0.35).
narrative_ontology:measurement(perf_grid_30, performance_legitimacy__quantitative_growth_reading, suppression(organizational), 40, 0.52).
narrative_ontology:measurement(perf_grid_31, performance_legitimacy__quantitative_growth_reading, suppression(structural), 0, 0.42).
narrative_ontology:measurement(perf_grid_32, performance_legitimacy__quantitative_growth_reading, suppression(structural), 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__quantitative_growth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__quantitative_growth_reading, 0.18).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__livelihood_security_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__techno_nationalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested legitimacy kernel shared with three sibling readings. The quantitative-growth reading grounds state legitimacy in GDP growth rates and economic expansion; the livelihood-security reading grounds it in tangible improvements in citizens' daily life; the qualitative-development reading grounds it in structural innovation and sustainability; the techno-nationalist reading grounds it in technological self-sufficiency and great-power status. These are not different measurements of one phenomenon — they are structurally distinct constraints with different ε values, different beneficiary/victim structures, and different institutional implementations. All four readings are live in contemporary policy discourse. This story models ONLY the quantitative-growth reading. Each sibling reading should be authored in a separate story with its own stakeholders, measurements, and commentary. The network edges link all four stories so the corpus documents the kernel contest directly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performance_legitimacy__quantitative_growth_reading, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
