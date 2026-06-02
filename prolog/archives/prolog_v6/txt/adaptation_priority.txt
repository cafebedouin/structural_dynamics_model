% ============================================================================
% CONSTRAINT STORY: adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_adaptation_priority, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: adaptation_priority
 *   human_readable: Adaptation Priority Over Mitigation: Normalization of 2-3°C Warming
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The normalization of 2-3°C warming as an inevitable target, coupled with
 *   reallocation of climate finance from mitigation to adaptation, represents
 *   a structural constraint that embeds extraction of future generations and
 *   climate-vulnerable populations to benefit current wealthy nations and
 *   fossil fuel capital. This constraint emerges from a contested kernel
 *   (climate_response_obligation) that admits multiple coherent readings —
 *   adaptation_priority is ONE such reading, instantiated here as a clean,
 *   structurally distinct constraint. The adaptation-priority reading
 *   presents itself as pragmatic acceptance of physical inevitability ('we
 *   cannot prevent warming below 2-3°C, so invest in resilience') but
 *   operates structurally as a choice to defer prevention costs while
 *   concentrating adaptation benefits in wealthy regions. The constraint
 *   exhibits high suppression (0.72) because future generations cannot
 *   participate in policy choice, Global South populations lack capital for
 *   adaptation, and scientific uncertainty about tipping points is weaponized
 *   to justify inaction. Theater ratio (0.58) reflects that climate
 *   governance institutions maintain formal commitments to lower targets
 *   (1.5°C, 2°C) while funding mechanisms and policy trajectories normalize
 *   2-3°C outcomes — the performative gap between targets and pathways.
 *   Extractiveness increases over time (0.48 → 0.68) as the policy choice
 *   becomes entrenched, infrastructure locks in, and the gap between
 *   announced targets and funded pathways widens.
 *
 * KEY AGENTS:
 *   - Current Wealthy Nations (institutional/arbitrage): Primary beneficiary — avoids transition costs, captures adaptation investment concentrated in wealthy regions, defers climate burden to others
 *   - Fossil Fuel Capital (institutional/arbitrage): Primary beneficiary — avoids stranded asset write-downs, maintains extraction rates, framing permits continued carbon intensity under adaptation language
 *   - Future Generations (powerless/trapped): Primary victim — inherits warmed baseline without choice; cannot retroactively change emissions trajectory; cannot exit the climate burden
 *   - Global South Vulnerable Populations (powerless/trapped): Primary victim — faces 2-3°C warming baseline with minimal adaptation capital; geographic exposure without decision-making power; economic structures constrain autonomy
 *   - Climate Scientists / Epistemic Community (moderate/mobile): Secondary agent — constrained by funding dependency on adaptation research; mobile but channeled toward adaptation emphasis; benefits from continuous measurement/monitoring demands
 *   - International Climate Governance (institutional/arbitrage): Institutional actor — maintains performative commitment to lower targets; funding mechanisms and policy trajectories embed acceptance of higher outcomes; arbitrage available through claims of progress via adaptation finance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(adaptation_priority, 0.68).
domain_priors:suppression_score(adaptation_priority, 0.72).
domain_priors:theater_ratio(adaptation_priority, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(adaptation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(adaptation_priority, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(adaptation_priority, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(adaptation_priority, snare).
narrative_ontology:human_readable(adaptation_priority, "Adaptation Priority Over Mitigation: Normalization of 2-3°C Warming").
narrative_ontology:topic_domain(adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(adaptation_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(adaptation_priority, current_wealthy_nations).
narrative_ontology:constraint_beneficiary(adaptation_priority, fossil_fuel_capital).
narrative_ontology:constraint_beneficiary(adaptation_priority, high_consumption_sectors).
narrative_ontology:constraint_victim(adaptation_priority, future_generations).
narrative_ontology:constraint_victim(adaptation_priority, global_south_nations).
narrative_ontology:constraint_victim(adaptation_priority, climate_vulnerable_populations).
narrative_ontology:constraint_victim(adaptation_priority, ecological_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE GENERATIONS (SNARE) — Trapped by inability to participate in present policy decisions that determine their climate baseline. Bears full extraction: inherits a warmed planet (2-3°C minimum) without having chosen prevention investments. No exit option — cannot refuse the climate burden imposed today. Suppression operates through temporal asymmetry: powerless to organize across time, unable to enforce claims against current decision-makers.
constraint_indexing:constraint_classification(adaptation_priority, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FOSSIL FUEL CAPITAL (ROPE) — Experiences the constraint as pure coordination: 'adaptation' language permits continued extraction of carbon resources without transition cost. Net beneficiary — avoids stranded asset write-downs, preserves business models, defers transition investments. Arbitrage exit: can maintain production and profitability under adaptation framing. The constraint solves their coordination problem: how to sustain fossil intensity while appearing to address climate risk.
constraint_indexing:constraint_classification(adaptation_priority, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: WEALTHY NATION GOVERNMENTS (TANGLED ROPE) — Constrained by electoral cycles and domestic political economy (fossil fuel lobbying, energy-intensive industries, consumer expectations). Also benefits from adaptation framing: capital investment in resilience infrastructure (sea walls, cooling systems, drought-resistant agriculture) is domestically concentrated and profitable. Mixed experience: genuine coordination problem (how to address climate while maintaining growth) layered with asymmetric extraction (deferring costs to others). Suppression is high but not total — some wealthy nations can pursue mitigation, but face political resistance.
constraint_indexing:constraint_classification(adaptation_priority, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: GLOBAL SOUTH VULNERABLE POPULATIONS (SNARE) — Trapped by geographic exposure and resource constraints. Faces 2-3°C warming baseline without the capital to invest in resilience. Adaptation investment concentrates in wealthy regions (Northern adaptation protects Northern assets); Global South receives minimal adaptation finance while bearing disproportionate climate impacts. Suppression operates through economic dependency (debt, trade structures) that constrains policy autonomy. No exit option — cannot escape climate exposure, cannot access adaptation capital, cannot influence the decision to accept warming.
constraint_indexing:constraint_classification(adaptation_priority, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 5: CLIMATE EPISTEMIC COMMUNITY (TANGLED ROPE) — Mobile agents (can shift research focus, publish alternative framings) but constrained by funding dependencies and institutional pressures. The adaptation-priority framing generates research funding (adaptation requires continuous assessment, monitoring, engineering) while suppressing research into prevention-centered pathways. Mixed experience: genuine coordination problem (scientific integration of climate impacts) layered with extractive constraints on research autonomy. Theater ratio involves selective emphasis on adaptation feasibility studies while de-emphasizing mitigation feasibility.
constraint_indexing:constraint_classification(adaptation_priority, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: CLIMATE GOVERNANCE INSTITUTIONS (PITON) — UNFCCC, IPCC, World Bank climate finance mechanisms maintain formal commitment to mitigation while operationalizing adaptation investment. Performative: treaties declare 1.5°C targets while funding mechanisms normalize 2-3°C outcomes. Institutional inertia preserves the gap between declared goals and funded pathways. Theater ratio high (0.70+): rituals of negotiation, pledges, summits persist while the institutional trajectory embeds acceptance of warming. Arbitrage exit available: institutions can claim progress through adaptation finance counts without achieving emissions reductions.
constraint_indexing:constraint_classification(adaptation_priority, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / PHYSICAL NECESSITY (MOUNTAIN) — From a civilizational/universal perspective, some warming is physically inevitable due to committed emissions and atmospheric lag: CO2 already emitted will warm the planet for centuries. Adaptation to some baseline warming (0.5-1.0°C above current) is structurally necessary. However, the constraint story shifts this from inevitable physics (warming due to committed emissions) to a normative choice (accepting 2-3°C as target and defunding prevention). This perspective risks naturalizing what is actually a political choice. Engine will flag as false summit: beneficiary presence and extraction asymmetry reveal this as constructed normality, not natural law.
constraint_indexing:constraint_classification(adaptation_priority, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(adaptation_priority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(adaptation_priority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(adaptation_priority, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(adaptation_priority, TR),
    TR >= 0.70.

:- end_tests(adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint transfers climate response costs from current wealthy populations (who would bear mitigation transition costs) to future generations and Global South populations (who bear climate impacts). This cost-shifting is the core extraction mechanism. Extractiveness is not extreme (0.68 rather than 0.85+) because some genuine adaptation coordination is required, and some adaptation capacity exists in wealthy nations. But the asymmetry is severe: wealthy nations avoid ~$2-4 trillion in near-term transition costs while externalizing ~$50+ trillion in 21st-century climate impacts. Suppression (0.72): High. Future generations cannot participate in decisions affecting their climate baseline; Global South populations lack capital to adapt; scientific uncertainty about tipping points is used to justify inaction; temporal asymmetry prevents retaliation or enforcement. Suppression operates through economic structures (debt, trade) that constrain Global South policy autonomy, and through institutional inertia that locks in high-carbon pathways. Theater ratio (0.58): Moderate-high. Climate governance institutions maintain public commitment to 1.5-2.0°C targets while funding mechanisms prioritize adaptation. COP negotiations, pledges, and summits perform urgency while policy trajectories embed 2-3°C outcomes. The gap between declared goals and funded pathways is the theater — it permits both beneficiaries and governance institutions to claim climate action while pursuing different outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The primary gap is between beneficiary (Rope/Tangled Rope) and victim (Snare) perspectives. Wealthy nations and fossil capital experience the constraint as rational coordination ('how to manage climate response cost-effectively'), enabled by treating 2-3°C as necessary baseline. Future generations and Global South populations experience pure extraction — they bear costs of a warming baseline without choosing it. The institutional perspective (governance, private adaptation investors) is opportunistic — the constraint solves their problem (how to address climate while preserving capital flows and investment opportunities). The epistemic community perspective shows how knowledge production is channeled toward confirming the adaptation frame (feasibility studies) while suppressing mitigation alternatives (prevention becomes framed as impossible). The analytical observer risks naturalizing this as inevitable ('physics constrains us to 2-3°C'), but the false summit detector flags the beneficiary presence as evidence of construction. The piton perspective shows how governance institutions preserve appearance of mitigation commitment while operationalizing adaptation pathways.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value (d) derives from their structural position: who benefits, who bears costs, what exit options exist. Wealthy nations and fossil capital are beneficiaries with high exit options (arbitrage) — they can maintain production/consumption or shift to resilience investment. Their d ≈ 0.15-0.25, producing low or negative effective extraction (they experience the constraint as enabling, not constraining). Future generations are victims with zero exit options (trapped) — they inherit a warmed baseline without choice. Their d ≈ 0.95, producing maximum experienced extraction (f(d) ≈ 1.42). Global South populations are victims with constrained exit (high adaptation costs, limited capital, economic dependency) — their d ≈ 0.85-0.90, producing very high experienced extraction. Climate scientists are moderate power with mobile exit but constrained by funding — their d ≈ 0.60-0.70. Governance institutions are beneficiaries with arbitrage (can claim progress without achieving emissions cuts) — their d ≈ 0.20. The engine derives these automatically from beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The adaptation-priority constraint avoids classical mandatrophy (mislabeling pure extraction as coordination) through careful perspective decomposition. The beneficiary's Rope classification is defensible: current wealthy nations do solve a genuine coordination problem (how to manage climate impacts within capital accumulation). But the snare classifications for victims reveal the extraction: the same constraint that solves beneficiary coordination transfers costs to powerless agents. The tangled rope classifications (governments, scientists) show mixed experience — they coordinate while being constrained by the adaptation framing. The piton classification (governance institutions) reveals performative commitment masking operational acceptance of higher warming. The false-summit mountain classification forces analytical observers to confront whether they are naturalizing what is actually a policy choice. Mandatrophy is resolved by refusing to collapse these perspectives — the constraint genuinely IS coordination from the beneficiary's position AND pure extraction from the victim's position. The perspectival gap is the answer, not evidence of miscategorization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint ONE reading of the contested climate response kernel, or a description of climate policy generally?',
    'This story instantiates the adaptation_priority READING of climate_response_obligation kernel. Sibling readings (mitigation_priority, degrowth_reading) are separate constraint stories with different ε values, beneficiary/victim structures, and temporal baselines. No reconciliation attempted here — only this reading is modeled.',
    'Identifies this constraint as one perspectival instantiation. The mitigation_priority reading would have lower ε, different beneficiary/victim sets (current generation also victim), and higher suppression of adaptation-as-distraction. The degrowth_reading would model transition costs and welfare losses for wealthy populations. Each reading is a distinct constraint with its own type classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the adaptation_priority reading of the climate_response_obligation kernel').

omega_variable(
    intergenerational_discount_rate,
    'At what implicit discount rate does 2-3°C warming become acceptable for current decision-makers?',
    'Revealed preference analysis: compare mitigation costs (now) vs adaptation costs (distributed over time, exponentiated by compounding impacts); extract implied social discount rate from policy choices; compare to standard economic and ethical discount rates.',
    'High discount rate (> 5% annually) makes future suffering negligible in present-value terms — renders the snare classification correct from the beneficiary''s perspective. Low discount rate (< 2%) makes future costs dominant — snare classification becomes indefensible ethically, forcing reclassification toward victim-centered frames. Discount rate assumption is the key parameter determining whether adaptation priority appears rational or extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_discount_rate, preference, 'Implicit intergenerational discount rate embedded in adaptation-priority policy').

omega_variable(
    adaptation_sufficiency_threshold,
    'What level of adaptation investment is sufficient to offset 2-3°C warming impacts, and for whom?',
    'Cost-benefit analysis: compare estimated adaptation spending (water infrastructure, cooling, agricultural resilience, migration support) to projected climate damages under 2-3°C scenario; identify geographic and sectoral gaps where adaptation is economically infeasible.',
    'If adaptation can be sufficient for all populations at affordable cost: snare classification is weakened (victims can escape via adaptation). If adaptation is insufficient and unevenly distributed: snare classification is reinforced (trapped populations cannot adapt regardless of investment). Current evidence suggests massive adaptation gaps in Global South and for extreme events.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_sufficiency_threshold, empirical, 'Whether projected adaptation investment can adequately offset 2-3°C warming globally').

omega_variable(
    mitigation_cost_displacement,
    'How much of the stated ''high cost of mitigation'' is transferred to adaptation, and to which populations?',
    'Accounting analysis: disaggregate total climate response costs by mitigation vs adaptation pathways; track who bears transition costs (wealthy nations'' carbon workers) vs who bears impact costs (Global South, future generations); calculate temporal distribution of costs.',
    'If transition costs are front-loaded in mitigation but impact costs are back-loaded in adaptation: wealthy current populations experience adaptation framing as cost-shifting to weaker populations. This reveals the extraction asymmetry. If costs are genuinely lower under adaptation: snare classification is questioned. Evidence strongly favors the former — adaptation is cheaper for wealthy nations in the near term but more expensive globally in the long term.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mitigation_cost_displacement, empirical, 'Whether adaptation-priority framing shifts costs from current wealthy populations to future/vulnerable populations').

omega_variable(
    prevention_lock_in,
    'Does accepting 2-3°C as baseline create path dependencies that prevent lower outcomes later?',
    'Historical analysis: examine whether infrastructure, investment patterns, and policy commitments made under adaptation framing entrench high-carbon systems; model counterfactual outcomes under earlier mitigation commitment.',
    'If acceptance of 2-3°C locks in infrastructure and behavior that makes lower warming unachievable: the snare classification gains force — the extraction is latent, manifest only when the lock-in becomes apparent (2050+). If later mitigation is still possible: snare is weakened. Evidence suggests significant lock-in for 2-3°C: fossil fuel infrastructure, urban sprawl, agricultural systems optimized for higher-carbon climate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prevention_lock_in, empirical, 'Whether adaptation-priority commits us to 2-3°C+ outcomes via infrastructure lock-in').

omega_variable(
    false_summit_natural_law_defense,
    'Is 2-3°C warming a natural physical outcome of committed emissions, or a policy choice masquerading as inevitability?',
    'Compare announced climate targets (1.5°C, 2°C) to current policy trajectories (3.0-3.5°C warming). If targets reflect genuine physical constraints: mountain classification. If targets are politically desirable but policies achieve different outcomes: false summit — the ''inevitability'' is constructed, not natural.',
    'If false summit (beneficiary-protected policy choice presented as law): adaptation-priority constraint is correctly classified as snare with false-summit appearance. If genuine physical ceiling (current emissions commit to ≥2°C): mountain classification partially justified, though beneficiary presence (fossil capital avoiding transition) still triggers FSM evaluation. Current policy trajectories suggest false summit: 2-3°C is policy outcome, not physical necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_defense, empirical, 'Whether 2-3°C warming is physical necessity or policy choice presented as inevitable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(adaptation_priority, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(adapt_theater_t0, adaptation_priority, theater_ratio, 0, 0.42).
narrative_ontology:measurement(adapt_theater_t5, adaptation_priority, theater_ratio, 5, 0.54).
narrative_ontology:measurement(adapt_theater_t10, adaptation_priority, theater_ratio, 10, 0.58).
narrative_ontology:measurement(adapt_theater_t15, adaptation_priority, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(adapt_extractiveness_t0, adaptation_priority, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(adapt_extractiveness_t5, adaptation_priority, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(adapt_extractiveness_t10, adaptation_priority, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(adapt_extractiveness_t15, adaptation_priority, base_extractiveness, 15, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(adaptation_priority, fossil_fuel_stranded_assets).
narrative_ontology:affects_constraint(adaptation_priority, climate_finance_distribution).
narrative_ontology:affects_constraint(adaptation_priority, intergenerational_risk_allocation).

% DUAL FORMULATION NOTE:
% The adaptation-priority constraint is one reading of the climate_response_obligation kernel. Sibling readings (mitigation_priority, degrowth_reading) are separate constraint stories in the climate_response family. Each story has its own ε, beneficiary/victim structure, and type classification. Network links track how the adaptation-priority policy choice cascades to stranded asset outcomes, finance distribution, and intergenerational risk allocation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
