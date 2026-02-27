% ============================================================================
% CONSTRAINT STORY: scam_doubt_manufacturing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_scam_doubt_manufacturing, []).

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
 *   constraint_id: scam_doubt_manufacturing
 *   human_readable: The Playbook for Manufacturing Scientific Doubt (SCAMs)
 *   domain: economic/political
 *
 * SUMMARY:
 *   The systematic manufacture of doubt about scientific evidence linking
 *   products or activities to public harm represents one of the most
 *   consequential extraction mechanisms in modern economies. Pioneered by the
 *   tobacco industry in the 1950s and replicated across fossil fuels,
 *   chemicals, and pharmaceuticals, this constraint operates by flooding the
 *   epistemic commons with manufactured uncertainty — funding marginal
 *   research, amplifying dissenting voices, exploiting legitimate scientific
 *   disagreement, and overwhelming regulatory capacity with complexity. The
 *   constraint extracts regulatory delay, market access, and postponement of
 *   liability costs by deliberately raising the evidence threshold required
 *   for action. Unlike pure extraction (Snare), the doubt-manufacturing
 *   playbook mimics coordination and scientific debate. Unlike pure
 *   coordination (Rope), it asymmetrically benefits the incumbent industry
 *   while harming public health commons and regulatory institutions. The
 *   theater ratio reflects the performative nature of the 'scientific debate'
 *   created by doubt manufacturing — the appearance of genuine scientific
 *   uncertainty masking coordinated industry strategy. Over the interval,
 *   both theater and extractiveness have increased as the playbook has become
 *   more sophisticated, regulatory agencies have been captured, and
 *   independent research capacity has declined relative to industry-funded
 *   production.
 *
 * KEY AGENTS:
 *   - Incumbent Industries (tobacco, fossil fuels, chemicals, pharmaceuticals): Primary beneficiary (institutional/arbitrage) — captures regulatory delay, market access, liability postponement. Has full option to exit by reforming products but chooses doubt manufacturing instead.
 *   - Doubt Manufacturing Firms (public relations, law firms, contract research organizations): Secondary beneficiary (institutional/arbitrage) — extracts service fees from industries funding the campaigns. Institutionalized as specialized profession.
 *   - Public Health Commons: Primary victim (powerless/trapped) — cannot opt out of exposure to harmful products. Suffers health costs, shortened lifespans, medical expenses. Bears full extraction with no exit.
 *   - Public Health Institutions: Secondary victim (moderate/constrained) — credibility eroded, policy authority undermined, resources diverted to defending settled science. Constrained by regulatory authority limits and political pressure.
 *   - Independent Researchers: Mixed victim/constrained actor (organized/constrained) — constrained by funding pressures and career risk; also benefits from access to industry data and collaboration. Forced to expend resources defending evidence.
 *   - Transparency and Accountability Advocates: Organized response (organized/constrained) — building alternative epistemic pathways (funding disclosure, study labeling, direct evidence communication) with sunset logic.
 *   - Regulatory Agencies: Institutional actor (institutional/arbitrage) — maintain performative regulatory ritual; see own function (timely protection based on evidence) as degraded. Piton classification.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(scam_doubt_manufacturing, 0.68).
domain_priors:suppression_score(scam_doubt_manufacturing, 0.72).
domain_priors:theater_ratio(scam_doubt_manufacturing, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(scam_doubt_manufacturing, extractiveness, 0.68).
narrative_ontology:constraint_metric(scam_doubt_manufacturing, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(scam_doubt_manufacturing, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(scam_doubt_manufacturing, snare).
narrative_ontology:human_readable(scam_doubt_manufacturing, "The Playbook for Manufacturing Scientific Doubt (SCAMs)").
narrative_ontology:topic_domain(scam_doubt_manufacturing, "economic/political").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(scam_doubt_manufacturing, incumbent_industries).
narrative_ontology:constraint_beneficiary(scam_doubt_manufacturing, doubt_manufacturing_firms).
narrative_ontology:constraint_victim(scam_doubt_manufacturing, public_health_commons).
narrative_ontology:constraint_victim(scam_doubt_manufacturing, epistemic_integrity).
narrative_ontology:constraint_victim(scam_doubt_manufacturing, regulatory_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE EXPOSED PUBLIC (SNARE) — Cannot opt out of exposure to harmful products or substances. Trapped by market concentration, regulatory gaps, and manufactured uncertainty about risk. Bears full extraction: health costs, shortened lifespans, medical expenses. No exit option; powerless to verify scientific claims against coordinated doubt campaigns. Maximum experienced extraction.
constraint_indexing:constraint_classification(scam_doubt_manufacturing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PUBLIC HEALTH INSTITUTIONS (SNARE) — Constrained by regulatory authority limits, political pressure, and funding dependence on industry partnerships. Cannot act on evidence until certainty threshold is reached, which the doubt campaign deliberately raises. Victims of the extraction: credibility is undermined, policy authority is eroded, resources are diverted to defending settled science.
constraint_indexing:constraint_classification(scam_doubt_manufacturing, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT INDUSTRY (ROPE) — Experiences the doubt-manufacturing constraint as a coordination mechanism for preserving market access and delaying costly regulation. Net beneficiary: extracts regulatory delay, market share retention, and postponement of liability. Has full arbitrage option — can exit by reforming products or accepting regulation, but chooses to fund doubt instead. Benefits far exceed costs; sees constraint as solving the 'problem' of inconvenient scientific evidence.
constraint_indexing:constraint_classification(scam_doubt_manufacturing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INDEPENDENT RESEARCHERS (TANGLED ROPE) — Constrained by funding pressures and career risk when their findings contradict industry-sponsored research or attack industry-funded doubt campaigns. Benefits from access to industry research data and collaboration; costs from reputational attacks, career retaliation, and suppression of findings. Asymmetric extraction masked by coordination language ('scientific debate'). Forced to expend resources defending settled science against manufactured challenges.
constraint_indexing:constraint_classification(scam_doubt_manufacturing, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY AGENCIES (PITON) — The regulatory apparatus for evaluating scientific evidence has become largely theatrical. Agencies perform the ritual of 'balanced consideration' of industry-funded doubt campaigns even when the evidence is settled. The procedural apparatus persists (advisory committees, public comment periods, multi-year review cycles) but its actual function — timely protection based on evidence — has atrophied. Theater ratio reflects the performative nature of 'regulatory deliberation' when doubt is manufactured on demand.
constraint_indexing:constraint_classification(scam_doubt_manufacturing, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: TRANSPARENCY AND ACCOUNTABILITY MOVEMENTS (SCAFFOLD) — Organized agents (NGOs, investigative journalists, data transparency advocates) are building alternative epistemic pathways: funding disclosure requirements, industry-funded study labeling, direct public communication of evidence, and historical documentation of doubt campaigns. These are temporary support structures with sunset logic — as transparency norms mature and meta-scientific auditing becomes standard, the doubt-manufacturing playbook loses effectiveness. Low effective extraction because coalition has agency and sees exit path through institutional reform.
constraint_indexing:constraint_classification(scam_doubt_manufacturing, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: THE ANALYTICAL OBSERVER (SNARE) — From a civilizational/universal perspective, the doubt-manufacturing constraint is not a natural law or inevitable feature of science. It is a coordinated extraction mechanism that could be eliminated by transparency requirements, regulatory reform, and institutional change. The trap is structural and reversible, not inherent. The engine classifies this as Snare, not Mountain, correctly identifying that the manufacturing of doubt is a contingent institutional arrangement, not a limit of knowledge itself.
constraint_indexing:constraint_classification(scam_doubt_manufacturing, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(scam_doubt_manufacturing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(scam_doubt_manufacturing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(scam_doubt_manufacturing, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(scam_doubt_manufacturing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(scam_doubt_manufacturing, TR),
    TR >= 0.70.

:- end_tests(scam_doubt_manufacturing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The doubt-manufacturing playbook systematically extracts regulatory delay, market access, and liability postponement from public health institutions and exposed populations. The extraction mechanism is coordinated, sophisticated, and has grown more effective over time as industries have learned to replicate the tobacco playbook. The value reflects that extraction is severe but not absolute — some regulatory decisions still proceed despite manufactured doubt, and transparency movements are beginning to expose the mechanism. Suppression (0.72): High. Multiple barriers prevent exit or resistance: (1) concentrated industry funding overwhelms independent research capacity; (2) regulatory procedures favor false balance over evidence hierarchy; (3) career risk punishes researchers who attack industry-funded campaigns; (4) public media literacy about manufactured doubt is limited; (5) market structure prevents consumer exit (essential products like fuel, medicines). Theater ratio (0.65): Moderate-high. The doubt-manufacturing constraint mimics legitimate scientific debate and regulatory deliberation, but much of this activity is performative. Industry-sponsored doubt campaigns create the appearance of scientific uncertainty where evidence is already settled. Regulatory review processes perform the ritual of balanced consideration even when the evidence clearly demonstrates harm. The performative content has increased over time as the playbook has become more refined and regulatory procedures have formalized false-balance rules.
 *
 * PERSPECTIVAL GAP:
 *   The exposed public and public health institutions see pure extraction (Snare) — they bear costs with no benefits and no exit. The incumbent industry sees coordination (Rope) — they experience the doubt-manufacturing constraint as solving the problem of inconvenient evidence, with arbitrage options fully available. Independent researchers see mixed coordination and extraction (Tangled Rope) — they benefit from access to industry research and funding but are constrained by career risk and funding dependence. The transparency movements see a temporary problem being solved (Scaffold) — funding disclosure, study labeling, and direct evidence communication are building alternative epistemic pathways with sunset logic. Regulatory agencies see their own degraded ritual (Piton) — the procedural apparatus for evaluating evidence persists but its actual function has atrophied. The analytical observer sees a coordinated extraction mechanism that could be eliminated by institutional reform (Snare with agency for change). The perspectival gap reveals that the same structural phenomenon — the gap between evidence and regulatory action — is experienced as inevitable by some (Piton, Mountain frame) but contingent and reversible by others (Scaffold, Snare with reform capacity).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by the agent's position in the extraction flow. The exposed public (powerless/trapped) has maximum d ~ 0.95, maximum experienced extraction. Public health institutions (moderate/constrained) have high d ~ 0.75, high extraction. The incumbent industry (institutional/arbitrage) has low d ~ 0.10, net benefit/negative extraction. Independent researchers (organized/constrained) have moderate d ~ 0.55, mixed experience. Regulatory agencies (institutional/arbitrage) have low d ~ 0.20, net benefit through procedural expansion. Transparency advocates (organized/constrained) have moderate d ~ 0.50, constrained agency but exit pathways visible. The engine derives d from these beneficiary/victim declarations and exit options, then applies the sigmoid f(d) to compute effective extractiveness chi from each perspective. The result is that the same base extractiveness (0.68) produces experienced extraction of ~1.30+ for trapped publics but ~-0.15 for beneficiary industries.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION RESOLVED: This constraint resolves the mandatrophy by correctly identifying doubt manufacturing as asymmetric extraction (Snare) rather than mislabeling it as coordination (Rope or Tangled Rope). The industry perspective would describe the constraint as 'scientific debate' or 'regulatory process,' using coordination language to legitimize extraction. The mandatrophy gate requires evidence that at least one perspective classifies as Snare (pure extraction) to prevent false coordination claims. This story includes multiple Snare perspectives (exposed public, public health institutions) plus the analytical observer's Snare, confirming that the constraint is not a coordination mechanism hiding benefits for all parties. The Rope and Tangled Rope perspectives in earlier versions of this analysis were rejected because they did not reflect the structural asymmetry: the public health commons receives no coordination benefit from the doubt campaign — it only bears costs. The constraint is pure extraction, correctly classified as Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_threshold_manipulation,
    'What evidence threshold legitimately separates ''uncertain'' from ''proven harmful'', and can that threshold be systematically elevated by funding bias toward null or negative findings?',
    'Meta-analysis of funding sources and outcome reporting bias; comparison of industry-funded vs independent research publication rates for null findings; historical trend analysis of regulatory certainty thresholds over time',
    'If thresholds can be manipulated: doubt manufacturing is an asymmetric extraction of regulatory delay. If thresholds are objective and immune: doubt campaigns are merely temporary communication noise (much lower extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epistemic_threshold_manipulation, empirical, 'Whether evidence thresholds can be systematically elevated through funding bias').

omega_variable(
    researcher_independence_capture,
    'Does industry funding of research create systematic bias in study design, publication, and meta-analysis that persists even when individual researchers are acting in good faith?',
    'Structural analysis of industry research funding mechanisms; comparison of publication patterns and finding distributions in industry-funded vs independent cohorts; financial flow tracing through research institutions',
    'If true: doubt manufacturing is embedded in research institutions themselves (high extraction, high suppression). If false or weak: doubt is created through selective citation and rhetoric rather than institutional capture (lower suppression).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(researcher_independence_capture, empirical, 'Whether industry funding creates systematic research bias').

omega_variable(
    regulatory_capacity_exhaustion,
    'Does the volume and complexity of manufactured doubt deliberately exceed the epistemic and administrative capacity of regulatory agencies to process and respond?',
    'Time-series analysis of regulatory decision timelines; comparison of agency workload and timeline to volume of industry submissions; modeling of deliberate complexity escalation strategies',
    'If true: suppression mechanism is active and deliberate (high suppression value). If false: regulatory delay is passive (suppression lower, constraint may degrade to Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capacity_exhaustion, empirical, 'Whether doubt manufacturing deliberately overwhelms regulatory capacity').

omega_variable(
    alternative_epistemic_pathways_sufficiency,
    'Can transparency requirements, funding disclosure, and meta-scientific auditing create a fully independent verification ecosystem that bypasses industry-controlled research channels?',
    'Feasibility analysis of institutional reforms; comparison of independent vs industry-funded research production rates in markets with strong transparency vs weak transparency; historical case studies of successful epistemic independence movements',
    'If sufficiency is high: scaffold perspective is confirmed — transparency sunset is achievable and extractiveness will decline. If sufficiency is low: alternative pathways will be partial and extraction will persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_epistemic_pathways_sufficiency, empirical, 'Whether transparency and auditing can create independent epistemic pathways').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(scam_doubt_manufacturing, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scam_tr_t0, scam_doubt_manufacturing, theater_ratio, 0, 0.35).
narrative_ontology:measurement(scam_tr_t20, scam_doubt_manufacturing, theater_ratio, 20, 0.5).
narrative_ontology:measurement(scam_tr_t40, scam_doubt_manufacturing, theater_ratio, 40, 0.65).

% Extraction over time
narrative_ontology:measurement(scam_be_t0, scam_doubt_manufacturing, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(scam_be_t20, scam_doubt_manufacturing, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(scam_be_t40, scam_doubt_manufacturing, base_extractiveness, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(scam_doubt_manufacturing, enforcement_mechanism).
narrative_ontology:affects_constraint(scam_doubt_manufacturing, regulatory_capture_fossil_fuels).
narrative_ontology:affects_constraint(scam_doubt_manufacturing, pharmaceutical_safety_obfuscation).
narrative_ontology:affects_constraint(scam_doubt_manufacturing, chemical_risk_postponement).

% DUAL FORMULATION NOTE:
% The doubt-manufacturing constraint is a meta-constraint that operates across multiple domain-specific constraints (fossil fuel regulation, pharmaceutical approval, chemical safety). The core mechanism is invariant across domains — coordinated funding of marginal research, amplification of dissenting voices, regulatory capacity exhaustion — but domain-specific stories should capture the particular industries and regulatory bodies involved. This story provides the structural template; downstream stories apply it to specific industries and harms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(scam_doubt_manufacturing, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
