% ============================================================================
% CONSTRAINT STORY: workplace_safety_standards
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_workplace_safety_standards, []).

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
 *   constraint_id: workplace_safety_standards
 *   human_readable: Workplace Safety Standards as Coordination and Extraction
 *   domain: occupational_health_and_safety/labor_regulation
 *
 * SUMMARY:
 *   Workplace safety standards represent a constraint that simultaneously
 *   coordinates worker protection and extracts value from smaller employers
 *   and precarious workers. The constraint operates across institutional,
 *   organizational, and individual scales: it establishes baseline safety
 *   norms (genuine coordination function), but it also concentrates
 *   compliance burden on smaller firms while large multinational employers
 *   can easily absorb costs and arbitrage across jurisdictions. The theater
 *   ratio has increased from 0.32 to 0.58 over the interval, indicating that
 *   the apparatus of compliance (audits, certifications, training programs)
 *   has become progressively more performative relative to actual injury
 *   prevention. Extractiveness has risen from 0.18 to 0.38, reflecting
 *   accumulating compliance costs and regulatory complexity layered onto the
 *   original safety function. Precarious workers — those with trapped exit
 *   options — experience the constraint as pure extraction with no
 *   coordination benefit: they cannot negotiate safety improvements, cannot
 *   exit unsafe workplaces without economic catastrophe, and cannot report
 *   hazards without risking deportation or termination. In contrast, large
 *   employers experience the same standards as coordination: they establish
 *   shared baseline norms, reduce liability exposure, and level competitive
 *   dynamics against smaller competitors with worse safety practices. The
 *   constraint is thus an exemplar of how the same institutional structure
 *   produces radically different classifications depending on structural
 *   position.
 *
 * KEY AGENTS:
 *   - Precarious Workers: Primary victims (powerless/trapped) — economic dependency and immigration status prevent exit; cannot use voice (reporting) without retaliation risk; bear injury costs with no control over workplace conditions
 *   - Small Employers: Secondary victims (moderate/constrained) — face significant compliance costs relative to revenue; cannot arbitrage across jurisdictions; benefit modestly from coordination function but extraction load exceeds benefit
 *   - Large Multinational Employers: Primary beneficiaries (institutional/arbitrage) — compliance costs negligible at scale; can arbitrage across regulatory jurisdictions; benefit from standards that establish competitive baselines and reduce liability; see constraint primarily as coordination
 *   - Workers' Unions: Organized victims-turned-advocates (organized/constrained) — capture enforcement legitimacy but lack resources to monitor compliance; benefit from standards as coordination mechanism but constrained by union density decline and retaliation against organizing
 *   - Occupational Health Movement: Organized advocates (organized/constrained) — see standards as temporary institutional form with sunset logic toward worker-controlled monitoring and outcome-based measurement; have exit vision but constrained by political power
 *   - Safety Compliance Industry: Beneficiaries-as-theater-maintainers (institutional/arbitrage) — vendors, consultants, certification bodies benefit from regulatory complexity; maintain theater through expanding compliance apparatus; degraded function (piton classification)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks false summit classification (naturalizing institutional distribution of risk as inherent to industrial production)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(workplace_safety_standards, 0.38).
domain_priors:suppression_score(workplace_safety_standards, 0.52).
domain_priors:theater_ratio(workplace_safety_standards, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(workplace_safety_standards, extractiveness, 0.38).
narrative_ontology:constraint_metric(workplace_safety_standards, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(workplace_safety_standards, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(workplace_safety_standards, tangled_rope).
narrative_ontology:human_readable(workplace_safety_standards, "Workplace Safety Standards as Coordination and Extraction").
narrative_ontology:topic_domain(workplace_safety_standards, "occupational_health_and_safety/labor_regulation").

domain_priors:requires_active_enforcement(workplace_safety_standards).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(workplace_safety_standards, large_employers).
narrative_ontology:constraint_beneficiary(workplace_safety_standards, regulatory_agencies).
narrative_ontology:constraint_beneficiary(workplace_safety_standards, safety_compliance_vendors).
narrative_ontology:constraint_victim(workplace_safety_standards, worker_injury_prevention).
narrative_ontology:constraint_victim(workplace_safety_standards, small_employers).
narrative_ontology:constraint_victim(workplace_safety_standards, precarious_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIOUS WORKER (SNARE) — Trapped by economic dependency and immigration status; cannot exit workplace despite safety violations. Suppression is structural: deportation threat, poverty, lack of alternative employment. Cannot report injuries without risking termination. Bears full extraction cost with no exit option.
constraint_indexing:constraint_classification(workplace_safety_standards, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL EMPLOYER (TANGLED ROPE) — Constrained by compliance costs, but also benefits from safety standards that protect against liability and worker turnover. Standards coordinate safety information (genuine benefit) while imposing asymmetric costs on smaller firms compared to large ones. Partial agency but significant extraction through regulatory burden.
constraint_indexing:constraint_classification(workplace_safety_standards, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LARGE MULTINATIONAL EMPLOYER (ROPE) — Can arbitrage across jurisdictions; compliance costs are negligible relative to operational scale. Standards serve coordination function: establish baseline norms, reduce liability exposure, level competitive playing field against smaller firms with worse practices. Net beneficiary from standardization.
constraint_indexing:constraint_classification(workplace_safety_standards, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: WORKERS' UNION (TANGLED ROPE) — Organized agents with constrained but not zero exit options. Standards provide genuine coordination function (shared safety norms, injury reporting mechanisms) and extraction function (unions capture enforcement legitimacy but lack resources to monitor all workplaces). Perspectival gap: union sees standards as incomplete protection; management sees them as sufficient regulation.
constraint_indexing:constraint_classification(workplace_safety_standards, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: OCCUPATIONAL HEALTH MOVEMENT (SCAFFOLD) — Organized agents (worker advocates, epidemiologists, injury prevention researchers) view safety standards as a temporary institutional form. Sunset logic: if worker power, inspection capacity, and injury prevention science matured, mandatory standards could transition to industry self-regulation or worker-controlled monitoring. Current theatrical compliance (audits, certifications) would be replaced by direct outcome measurement (injury rates, worker satisfaction). Low effective extraction because the movement has agency and sees exit path.
constraint_indexing:constraint_classification(workplace_safety_standards, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: SAFETY COMPLIANCE INDUSTRY (PITON) — Regulatory consultants, certification bodies, safety audit firms, compliance software vendors. Theater ratio is high (0.58): much of the compliance apparatus is performative — checkboxes on audits, certifications renewed annually without measurable injury reduction, training videos watched but not retained. The industry benefits from theatrical maintenance: expanding regulatory complexity sustains demand for compliance services. Degraded from genuine safety function to bureaucratic ritual.
constraint_indexing:constraint_classification(workplace_safety_standards, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, workplace injury is inherent to industrial production; prevention lag is a natural constraint on human capacity. Some injury rate is 'inevitable' given current technology and human performance limits. This view naturalizes what is actually an institutional allocation of risk. The engine's false summit detector will flag this as misclassification — the constraint is not immutable law but a contingent distributional choice about who bears safety costs.
constraint_indexing:constraint_classification(workplace_safety_standards, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(workplace_safety_standards_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(workplace_safety_standards, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(workplace_safety_standards, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(workplace_safety_standards, TR),
    TR >= 0.70.

:- end_tests(workplace_safety_standards_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts from smaller employers and precarious workers through compliance costs and risk concentration, but it is not as severe as a snare would be (ε ≥ 0.46). The extractiveness has increased over the interval as regulatory complexity accumulated and as small employers' capacity to comply has been tested by recession cycles. The rise from 0.18 to 0.38 reflects three mechanisms: (1) baseline standard proliferation (OSHA standards have expanded from ~50 in 1970 to >600), (2) compliance infrastructure growth (auditing, certification, consulting), and (3) consolidation of employer market power (large firms can absorb costs, small firms exit). Suppression (0.52): Moderate-high. Precarious workers face structural barriers to exit (economic dependency, immigration status, lack of alternative employment) and voice barriers (retaliation risk for injury reporting, union organizing suppression). Small employers face regulatory barriers (compliance cost, inspection burden). However, suppression is not total — some voice mechanisms exist (OSHA complaints, union organizing, regulatory appeal), some workers have geographic or occupational mobility, and some small employers succeed by specializing in safety-differentiated markets. Theater ratio (0.58): Moderate-high. The apparatus of workplace safety standards has become increasingly performative: safety training often does not reduce injury rates significantly; certification audits are checkboxes that employers hire consultants to pass; injuries are undercounted and underreported due to employer incentives to hide incidents. But theater is not complete — some standards (lockout/tagout procedures, hard-hat requirements) have direct mechanical safety benefits; some injury prevention is real. The theater_ratio rise from 0.32 to 0.58 reflects the accumulation of compliance ritual without corresponding injury rate improvements in many sectors.
 *
 * PERSPECTIVAL GAP:
 *   The precarious worker sees a snare: economic trap + safety hazard + suppression of voice = pure extraction. The small employer sees tangled rope: coordination benefit (liability reduction, competitive leveling) mixed with extraction burden (compliance costs). The large employer sees rope: coordination function, modest cost, net benefit from competitive advantage. The union sees tangled rope: genuine coordination (shared norms, injury reporting channels) but incomplete enforcement (under-resourcing, density decline, retaliation). The compliance industry sees piton: degraded function (audits are largely theatrical) but maintained through regulatory expansion. The occupational health movement sees scaffold: current standards are temporary institutional form that will sunset as worker power, inspection capacity, and injury prevention science mature. The civilizational observer risks false summit (naturalizing institutional risk allocation as inherent to production). Each perspective is structurally legitimate given the agent's position; the perspectival gap reveals the constraint's mixed nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by the agent's relationship to the extraction flow and their exit options. Precarious workers have high d (~0.95): full targets of extraction, trapped exit, victim status. Small employers have moderate d (~0.65): constrained exit, some victim status (compliance burden) but also some beneficiary status (standards reduce liability). Large employers have low d (~0.15): beneficiary status, arbitrage exit, institutional power. The sigmoid f(d) amplifies the extraction experienced by trapped agents (f(0.95) ≈ 1.42) and dampens it for beneficiaries (f(0.15) ≈ -0.01). The organizational perspective shows how the same regulatory constraint produces opposing directionalities: the large employer sees it flowing toward them as coordination benefit, the small employer sees it flowing away from them as compliance burden. This directionality gap is the diagnostic signature of a tangled rope — real coordination function (standards do coordinate safety norms and liability) combined with asymmetric extraction (burden falls disproportionately on smaller and more vulnerable agents).
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The mandatrophy is resolved by recognizing that workplace safety standards have BOTH a genuine coordination function AND asymmetric extraction. Coordination function: standards establish shared safety baselines, enable communication about hazards, reduce liability exposure, and level competitive dynamics among employers. This is real and valuable — it solves a coordination problem that employers, workers, and regulators all face. Asymmetric extraction: the compliance burden falls disproportionately on smaller employers and precarious workers who have fewer resources to absorb costs and fewer options to exit. Large employers can arbitrage across jurisdictions and amortize compliance costs; precarious workers cannot exit and cannot voice concerns. The theater ratio indicates that the apparatus of compliance has become partially performative (audits, training, certifications that don't correlate with injury reduction), which suggests the coordination function is degrading while extraction persists. This is the classic Tangled Rope trajectory: as institutions age, coordination mechanisms degrade but extraction mechanisms persist through inertia and regulatory capture. The mandatrophy reveals that calling this 'safety standards' (coordination framing) or 'regulatory burden' (extraction framing) are both partial truths. The full structure is coordination with an extraction overlay that concentrates costs on the least-resourced agents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    injury_causation_attribution,
    'Are workplace injuries caused by worker negligence (making standards a coordination aid) or by employer cost-cutting (making standards an extraction defense)?',
    'Longitudinal injury analysis by firm size, compliance investment, and worker training; correlation between injury rates and worker agency/autonomy measures; comparison of preventable vs inherent injury types',
    'If negligence-driven: standards are primarily coordination (Rope from more perspectives). If cost-cutting-driven: standards are primarily a cover for extraction (Snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(injury_causation_attribution, empirical, 'Whether injuries result from worker behavior vs employer choices').

omega_variable(
    compliance_theater_effectiveness,
    'Do compliance audits, certifications, and training programs actually reduce injury rates, or do they create the appearance of safety while injuries persist unchanged?',
    'Meta-analysis of workplace safety interventions; comparison of injury trends in industries with different compliance theater levels; worker experience surveys about training effectiveness and safety culture',
    'If effective: compliance apparatus has genuine function (theater_ratio should be lower, ~0.35). If ineffective: theater_ratio of 0.58 understates performativity, suggesting Piton may underestimate institutional degradation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compliance_theater_effectiveness, empirical, 'Whether compliance apparatus reduces injuries or merely appears to').

omega_variable(
    worker_power_as_exit_option,
    'Can workers realistically exercise voice (reporting hazards, union organization) as an exit-equivalent option, or are voice mechanisms themselves suppressed?',
    'Retaliation rates for injury reporting and union activity; union density and density trends in high-hazard sectors; comparative analysis of injury rates in unionized vs non-unionized workplaces; worker surveys about perceived safety reporting options',
    'If workers have voice: exit_options for precarious workers might upgrade from trapped to constrained. If voice is suppressed: trapped classification confirmed, and suppression value may be understated at 0.52.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_power_as_exit_option, empirical, 'Whether workers can use voice as functional alternative to exit').

omega_variable(
    regulatory_capture_by_industry,
    'Do large employers capture the regulatory process to set standards that benefit themselves while disadvantaging smaller competitors and precarious workers?',
    'Analysis of standard-setting participation by firm size; cost-benefit analysis of standards by firm size and worker category; correlation between regulatory complexity and small firm market exit rates; lobbying expenditure and standard language influence',
    'If captured: large employer perspective (rope, arbitrage) reveals extractive directionality masked as coordination. Would shift directionality_overrides for large employers toward higher d values, reducing their effective extraction below beneficiary baseline.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_by_industry, empirical, 'Whether standards reflect industry capture masking extraction as coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(workplace_safety_standards, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ws_tr_t0, workplace_safety_standards, theater_ratio, 0, 0.32).
narrative_ontology:measurement(ws_tr_t3, workplace_safety_standards, theater_ratio, 3, 0.45).
narrative_ontology:measurement(ws_tr_t6, workplace_safety_standards, theater_ratio, 6, 0.58).
narrative_ontology:measurement(ws_tr_t9, workplace_safety_standards, theater_ratio, 9, 0.58).

% Extraction over time
narrative_ontology:measurement(ws_be_t0, workplace_safety_standards, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(ws_be_t3, workplace_safety_standards, base_extractiveness, 3, 0.28).
narrative_ontology:measurement(ws_be_t6, workplace_safety_standards, base_extractiveness, 6, 0.35).
narrative_ontology:measurement(ws_be_t9, workplace_safety_standards, base_extractiveness, 9, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(workplace_safety_standards, enforcement_mechanism).
narrative_ontology:affects_constraint(workplace_safety_standards, labor_market_stratification).
narrative_ontology:affects_constraint(workplace_safety_standards, immigrant_worker_precarity).
narrative_ontology:affects_constraint(workplace_safety_standards, union_density_decline).

% DUAL FORMULATION NOTE:
% Workplace safety standards are upstream of labor market stratification (standards contribute to stratification by concentrating compliance burden on small employers and precarious workers) and immigrant worker precarity (standards become enforcement mechanism for immigration status when injury reporting is coupled with workplace documentation checks). The union density decline constraint shares causality — retaliation against union organizing is often framed as safety compliance issue.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(workplace_safety_standards, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
