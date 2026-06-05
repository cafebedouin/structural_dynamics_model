% ============================================================================
% CONSTRAINT STORY: taiwan_ids_program
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_taiwan_ids_program, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: taiwan_ids_program
 *   human_readable: Taiwan's Indigenous Defense Submarine (IDS) Program
 *   domain: geopolitical/technological
 *
 * SUMMARY:
 *   Taiwan's Indigenous Defense Submarine (IDS) program represents a
 *   constrained strategic response to the de facto international blockade on
 *   military submarine sales to Taiwan imposed by the People's Republic of
 *   China. Facing a credible military threat across the Taiwan Strait with no
 *   legal avenue for purchasing submarines from traditional allies (the US,
 *   traditionally bound by restrictions on direct military technology
 *   transfer; European suppliers constrained by PRC pressure; other sources
 *   politically untenable), Taiwan initiated the IDS program in 2016 as a
 *   forced entry into domestic submarine design and construction. The program
 *   embodies a fundamental tension: it is simultaneously a necessary
 *   coordination response to an external blockade AND a mechanism through
 *   which domestic contractors extract monopolistic rents from a fiscally
 *   constrained state. The constraint exhibits tangled rope structure from
 *   multiple perspectives — the genuine coordination problem (no submarines
 *   available through markets) combines with asymmetric extraction (taxpayers
 *   bear full cost; contractors capture monopoly profits; the Taiwanese
 *   population sacrifices alternative investments in healthcare, education,
 *   and social infrastructure). Theater ratio of 0.48 indicates moderate
 *   performative content: public legitimation emphasizing 'indigenous
 *   capability' and 'autonomy' performs strategic reassurance, but the actual
 *   technical content is substantially sourced from foreign design
 *   partnerships and critical component imports, limiting the claimed
 *   autonomy. The program's extractiveness has risen over 14 years as cost
 *   overruns accumulated (initial budget NT$300B; current estimates exceed
 *   NT$500B) and schedule delays shifted costs to later fiscal years,
 *   increasing opportunity costs.
 *
 * KEY AGENTS:
 *   - Taiwan's State (Institutional/Constrained) — Primary decision-maker; faces geopolitical constraint (blockade) but also reaps coordination and strategic benefits from domestic capability
 *   - Taiwanese Civilian Population (Powerless/Trapped) — Primary victims; bear full fiscal cost through opportunity costs in non-defense spending; cannot exit Taiwan Strait geography
 *   - Domestic Shipbuilding Contractors (Organized/Constrained) — Primary beneficiaries; capture monopoly rents through sole-source procurement; also coordinate genuine technical problem of submarine development
 *   - US Strategic Partnership (Institutional/Mobile) — Provides technology transfer scaffolding; can adjust support posture (mobile exit); intended to sunset once Taiwan capability matures
 *   - People's Republic of China (Powerful/Arbitrage) — Structural constrainer (imposes blockade); benefits from forcing Taiwan into expensive indigenous development path; exit available (could lift blockade) but chooses not to
 *   - Alternative Defense Sectors (Moderate/Trapped) — Structural victims; other military modernization needs (air defense, cyber capability, missile systems) underfunded due to IDS resource monopolization
 *   - Analytical Observer (Analytical/Analytical) — Sees full structure: genuine coordination necessity combined with real extraction mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(taiwan_ids_program, 0.58).
domain_priors:suppression_score(taiwan_ids_program, 0.72).
domain_priors:theater_ratio(taiwan_ids_program, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(taiwan_ids_program, extractiveness, 0.58).
narrative_ontology:constraint_metric(taiwan_ids_program, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(taiwan_ids_program, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(taiwan_ids_program, tangled_rope).
narrative_ontology:human_readable(taiwan_ids_program, "Taiwan's Indigenous Defense Submarine (IDS) Program").
narrative_ontology:topic_domain(taiwan_ids_program, "geopolitical/technological").

domain_priors:requires_active_enforcement(taiwan_ids_program).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(taiwan_ids_program, taiwanese_state_security).
narrative_ontology:constraint_beneficiary(taiwan_ids_program, domestic_shipbuilding_industry).
narrative_ontology:constraint_beneficiary(taiwan_ids_program, technological_autonomy_coalition).
narrative_ontology:constraint_victim(taiwan_ids_program, cross_strait_stability).
narrative_ontology:constraint_victim(taiwan_ids_program, taiwanese_public_fiscal_resources).
narrative_ontology:constraint_victim(taiwan_ids_program, alternative_defense_investments).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TAIWANESE CIVILIAN POPULATION (SNARE) — Trapped by geopolitical facts; bears full extraction cost through opportunity costs in healthcare, education, and social investment. Program diverts fiscal resources (estimated NT$300B+) with no alternative path for defense. Cannot exit the Taiwan Strait geography. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.73.
constraint_indexing:constraint_classification(taiwan_ids_program, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: TAIWAN NAVAL COMMAND & DEFENSE PLANNERS (TANGLED ROPE) — Both coordinate genuine defense necessity (no international submarine supply available) AND extract rents through vendor consolidation and technology monopolization within domestic suppliers. Constrained by geopolitical reality but benefit from monopolistic procurement. d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.61.
constraint_indexing:constraint_classification(taiwan_ids_program, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMESTIC SHIPBUILDING CONTRACTORS (ROPE) — Experience the program as pure coordination: solve collective action problem of building submarines without foreign suppliers. Benefit through guaranteed procurement contracts and technological capability-building. Constrained by technical complexity but genuinely solve coordination problem. d≈0.35, f(d)≈0.32, σ=1.0 → χ≈0.19.
constraint_indexing:constraint_classification(taiwan_ids_program, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: US STRATEGIC PARTNERSHIP (SCAFFOLD) — Provides temporary coordination scaffolding through technology transfer agreements, engineering support, and strategic deterrence partnership. Support is explicitly designed to sunset once Taiwan achieves indigenous capability. Effective extraction low (mobile exit available via shifting US strategic posture). d≈0.42, f(d)≈0.42, σ=1.1 → χ≈0.25. Sunset clause: intended to expire once IDS operational and Taiwan gains technological autonomy.
constraint_indexing:constraint_classification(taiwan_ids_program, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: PEOPLE'S REPUBLIC OF CHINA (SNARE) — The PRC's blockade on submarine sales to Taiwan IS the constraining force that drives the entire IDS program. From PRC perspective, the program is a response to their superior military leverage and de facto veto over Taiwan's defense procurement. PRC extracts strategic concessions and deterrence value from Taiwan's forced resource reallocation. Arbitrage exit (can adjust blockade policies but chooses not to). d≈0.15, f(d)≈0.02, σ=1.1 → χ≈0.01. But note: PRC's role is as the constraining actor, not the constrained; classification from PRC perspective differs fundamentally from classification relative to Taiwan.
constraint_indexing:constraint_classification(taiwan_ids_program, snare,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From global analytical perspective, the IDS program both (a) coordinates genuine defense necessity given blockade reality AND (b) extracts technological rents through monopolistic domestic procurement architecture. The program is structurally justified (no alternative) but also structurally extractive (monopoly costs borne by taxpayer). Theater ratio 0.48 reflects functional defense design; constraint is neither natural law nor pure illusion. d≈0.60, f(d)≈0.78, σ=1.0 → χ≈0.45.
constraint_indexing:constraint_classification(taiwan_ids_program, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taiwan_ids_program_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(taiwan_ids_program, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taiwan_ids_program, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(taiwan_ids_program, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(taiwan_ids_program_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The program extracts significant fiscal resources (NT$500B+ vs healthcare budget NT$200B annually) with no alternative path available. However, extraction is not maximal (0.70+) because the program solves a genuine coordination problem — submarines must be procured somehow, and blockade eliminates market solutions. The extraction component reflects monopolistic contracting (lack of competition drives costs higher) rather than pure coercive fiction. Suppression (0.72): High. Multiple barriers prevent exit or alternatives: (1) PRC blockade eliminates international market, (2) technical complexity requires specialized workforce, (3) fiscal costs lock Taiwan into path dependence, (4) geopolitical urgency suppresses cost-benefit analysis and alternative evaluation, (5) domestic industrial capacity is concentrated in single contractor. Suppression is not maximal (0.85+) because some public debate occurs and alternative proposals (seeking negotiated blockade lifting, investing in defensive asymmetric capabilities instead of submarines) receive airing, even if politically overridden. Theater ratio (0.48): Moderate. The program has significant performative content — public messaging emphasizing 'indigenous' capability obscures foreign design sourcing and critical component dependencies. However, the submarine itself is functionally designed and technically demanding; theater is not the majority of the program. The theater ratio reflects strategic legitimation needs (reassuring public that defense is improving) more than pure theatrical substitution for function.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiary and victim is acute. The domestic contractors (organized/constrained) see a Rope: they are solving the genuine coordination problem of building submarines without foreign suppliers, capturing efficiency gains and technological capability. The Taiwanese population (powerless/trapped) sees a Snare: they bear full extraction cost with no exit option, facing opportunity costs in healthcare and education. The US partnership sees a Scaffold: providing temporary support with explicit sunset once Taiwan achieves capability. Taiwan's naval command sees Tangled Rope: both coordinating genuine defense necessity and extracting monopoly rents. The PRC sees this as successful extraction through blockade leverage — Taiwan is forced to invest heavily in expensive indigenous capability rather than purchasing cheaper mature designs. The analytical observer sees Tangled Rope: the program is both structurally justified (no alternative) and structurally extractive (monopoly costs are real; alternatives underfunded). This gap reveals that 'coordination necessity' and 'extraction mechanism' are not opposites — the IDS program demonstrates how a genuine coordination problem can be solved via extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Taiwanese population: Victim + trapped → d≈0.92, f(d)≈1.38. Maximal extraction; bears full cost with no alternative path. Domestic contractors: Beneficiary + constrained → d≈0.35, f(d)≈0.32. Net beneficiaries; capture monopoly rents but genuinely solve coordination problem; some extraction risk comes from competition-free procurement. Taiwan naval command: Mixed (both beneficiary of capability and victim of resource constraint) + constrained → d≈0.68, f(d)≈1.05. Effective extraction moderate-high; benefits from submarine capability but also constrained by budget limitations and forced to defend monopolistic contractor arrangements. US partnership: Beneficiary (in extended deterrence stability) + mobile → d≈0.42, f(d)≈0.42. Low effective extraction; can adjust posture, has multiple Taiwan defense options, mobile exit. PRC: Beneficiary (from forcing expensive response) + arbitrage → d≈0.15, f(d)≈0.02. Net beneficiary from blockade leverage; could lift blockade (arbitrage exit) but chooses not to; effective extraction minimal because PRC is external actor setting constraint conditions rather than constrained party. Analytical observer: d≈0.60, f(d)≈0.78. Moderate-high effective extraction; system is both coordinating and extracting.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED AT χ≈0.73. The classification rejects false naturalizing in multiple directions: (1) The program is NOT a mountain — the blockade is geopolitical choice, not physical law. Taiwan could theoretically negotiate submarine access if political conditions shifted. (2) The program is NOT pure rope — genuine coordination benefits exist, but monopolistic extraction is real. Contractors benefit disproportionately. (3) The program IS tangled rope — both genuine coordination (no submarine alternative) AND asymmetric extraction (monopoly rents) are structurally present. The mandatrophy resolution recognizes that 'forced coordination' (coordination necessitated by external constraint) can simultaneously be 'extraction mechanism' (the resolution of the coordination problem benefits some parties disproportionately). The IDS program mandatrophy is resolved by acknowledging that the constraint serves the Taiwanese state's genuine defense need while simultaneously serving domestic contractors' monopolistic profit interests. Neither reading is false; both are structurally real. The perspectival gap (Snare from victim view, Rope from contractor view, Tangled Rope from system view) reflects the actual structure: the same constraint mechanism appears extractive or coordinating depending on structural position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    blockade_permanence,
    'Is the PRC''s submarine sale blockade structurally permanent (reflecting long-term geopolitical reality) or contingent on current Chinese leadership preference?',
    'Long-term monitoring of PRC policy statements, cross-strait diplomatic developments, shifts in Chinese military doctrine; comparison with precedent cases of arms embargo reversal (Taiwan itself, South Africa, Iran)',
    'If permanent: IDS program is justified as only available defense path (Rope/Scaffold dominant). If contingent: program may represent overinvestment in solving a problem that could be solved via negotiation (extraction risk higher).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(blockade_permanence, empirical, 'Whether PRC submarine blockade is structurally permanent or contingent').

omega_variable(
    domestic_cost_vs_import_cost,
    'Is the per-unit cost of indigenous submarine development (accounting for R&D, tooling, workforce retraining) lower or higher than counterfactual cost of imported submarines if blockade were lifted?',
    'Cost accounting analysis: Taiwan IDS program unit costs vs historical submarine procurement costs for comparable democracies; sensitivity analysis on learning curves and production scale',
    'If indigenous < import: program is economically justified coordination. If indigenous > import by >30%: extraction component is significant; fiscal cost to population is rent-seeking overhead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(domestic_cost_vs_import_cost, empirical, 'Per-unit cost of indigenous vs imported submarines').

omega_variable(
    technological_autonomy_benefit,
    'Does indigenous submarine capability actually reduce Taiwan''s dependence on foreign supply chains, or does it substitute one form of dependency (foreign suppliers) for another (critical component imports, foreign design IP)?',
    'Supply chain analysis; identification of critical components and their sourcing; comparison with other Taiwan indigenous defense programs; assessment of actual autonomy vs perceived autonomy',
    'If true autonomy achieved: program justified as breaking dependency trap. If dependency merely relocated: much of extraction cost is wasted on false autonomy theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_autonomy_benefit, empirical, 'Whether IDS achieves true technological autonomy').

omega_variable(
    strategic_deterrence_effectiveness,
    'Does submarine deterrence against PRC naval invasion actually reduce probability of military conflict, or does submarine capability create escalation dynamics that increase conflict risk?',
    'Game-theoretic analysis of cross-strait deterrence balance; historical cases of submarine deployment in regional conflicts; assessment of PRC strategic response to IDS operationalization',
    'If deterrence effective: program is security coordination. If escalatory: extraction mechanism (forcing arms race) is real and extraction cost higher.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(strategic_deterrence_effectiveness, empirical, 'Whether submarines deter or escalate cross-strait conflict').

omega_variable(
    contractor_monopoly_efficiency,
    'Does the consolidated domestic shipbuilding contractor structure (China State Shipbuilding Corporation subunit, Taiwan Industrial Partnership) enable efficiency through specialization, or does it create rent-seeking monopoly through lack of competition?',
    'Comparison of IDS cost overruns, schedule delays, and technical performance vs international submarine programs with competitive bidding; analysis of contractor profit margins; assessment of technological innovation rate',
    'If efficient: moderate extraction component. If monopolistic: extraction component is substantial; could be reduced by procurement competition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contractor_monopoly_efficiency, empirical, 'Whether domestic contractor structure is efficient or monopolistic').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(taiwan_ids_program, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ids_tr_t0, taiwan_ids_program, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ids_tr_t7, taiwan_ids_program, theater_ratio, 7, 0.42).
narrative_ontology:measurement(ids_tr_t14, taiwan_ids_program, theater_ratio, 14, 0.48).

% Extraction over time
narrative_ontology:measurement(ids_be_t0, taiwan_ids_program, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ids_be_t7, taiwan_ids_program, base_extractiveness, 7, 0.5).
narrative_ontology:measurement(ids_be_t14, taiwan_ids_program, base_extractiveness, 14, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(taiwan_ids_program, enforcement_mechanism).
narrative_ontology:affects_constraint(taiwan_ids_program, cross_strait_military_balance).
narrative_ontology:affects_constraint(taiwan_ids_program, taiwan_defense_budget_constraint).
narrative_ontology:affects_constraint(taiwan_ids_program, us_taiwan_strategic_partnership).
narrative_ontology:affects_constraint(taiwan_ids_program, prc_military_coercion_capability).

% DUAL FORMULATION NOTE:
% The IDS program is downstream of the PRC blockade (the upstream constraint that forces indigenization). The blockade has ε≈0.25 (mountain-like in its structural permanence from Taiwan's perspective) while the IDS program has ε≈0.58 (tangled rope in its combination of genuine coordination and extraction). The program also affects cross-strait military balance (higher deterrence) and Taiwan's broader defense budget allocation (opportunity costs). Network links to US strategic partnership (scaffolding provider) and PRC coercion capability (upstream constraint).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(taiwan_ids_program, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
