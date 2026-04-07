% ============================================================================
% CONSTRAINT STORY: precision_manufacturing_automation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_precision_manufacturing_automation, []).

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
 *   constraint_id: precision_manufacturing_automation
 *   human_readable: Precision Manufacturing Automation Constraint
 *   domain: industrial/economic/labor
 *
 * SUMMARY:
 *   Precision manufacturing automation represents a hybrid constraint
 *   combining genuine coordination of technical standards with asymmetric
 *   extraction of automation's benefits and costs. The ecosystem requires
 *   compatibility across machine tools, software platforms, measurement
 *   systems, and quality control frameworks. This coordination function is
 *   real and reduces integration costs for manufacturers. However, the
 *   constraint also extracts value from workers displaced by automation and
 *   from small manufacturers unable to afford retooling, concentrating
 *   benefits among capital owners, automation vendors, and large integrated
 *   firms with existing scale. The extractiveness has increased over the
 *   measurement interval (0.32 → 0.64) as automation technologies mature and
 *   diffuse, pushing more workers into displacement. Theater ratio remains
 *   low (0.38) because the coordination function is substantive—technical
 *   standards genuinely solve compatibility problems—but the constraint's
 *   extractive mechanism is increasingly visible: productivity gains from
 *   automation flow to capital while displacement costs are borne by workers.
 *
 * KEY AGENTS:
 *   - Skilled Manufacturing Workers: Primary victims (powerless/trapped) — face permanent or very long-term exclusion from precision work; accumulated tacit knowledge becomes valueless
 *   - Small Precision Manufacturers: Secondary victims (moderate/constrained) — face capital barriers and vendor lock-in; can exit but at significant cost
 *   - Capital Owners (Large Manufacturers): Primary beneficiaries (institutional/arbitrage) — capture productivity gains and cost reduction from automation
 *   - Automation Vendors: Secondary beneficiaries (organized/arbitrage) — benefit from network effects and standardization driving equipment sales
 *   - Labor Organizing Coalition: Organized agents (organized/constrained) — see constraint as sunset-able through policy intervention; pushing for retraining, wage insurance, sectoral bargaining
 *   - Industrial Policy Apparatus: Institutional observer (institutional/arbitrage) — maintains standards bodies and industrial promotion frameworks; largely performative
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — maps full hybrid structure: genuine coordination + structural extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(precision_manufacturing_automation, 0.58).
domain_priors:suppression_score(precision_manufacturing_automation, 0.52).
domain_priors:theater_ratio(precision_manufacturing_automation, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(precision_manufacturing_automation, extractiveness, 0.58).
narrative_ontology:constraint_metric(precision_manufacturing_automation, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(precision_manufacturing_automation, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(precision_manufacturing_automation, tangled_rope).
narrative_ontology:human_readable(precision_manufacturing_automation, "Precision Manufacturing Automation Constraint").
narrative_ontology:topic_domain(precision_manufacturing_automation, "industrial/economic/labor").

domain_priors:requires_active_enforcement(precision_manufacturing_automation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(precision_manufacturing_automation, capital_owners).
narrative_ontology:constraint_beneficiary(precision_manufacturing_automation, automation_vendors).
narrative_ontology:constraint_beneficiary(precision_manufacturing_automation, export_competitive_firms).
narrative_ontology:constraint_victim(precision_manufacturing_automation, skilled_manufacturing_workers).
narrative_ontology:constraint_victim(precision_manufacturing_automation, small_manufacturers).
narrative_ontology:constraint_victim(precision_manufacturing_automation, labor_cost_bearing_regions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED MANUFACTURING WORKER (SNARE) — Skilled workers in precision manufacturing face severe barriers to exit. Retraining is expensive, geographically immobile, and age-discriminatory. Their accumulated tacit knowledge in manual precision work becomes valueless as automation replaces the task. No meaningful alternative employment at comparable wage in local labor market. Maximum experienced extraction — trapped both structurally (no jobs) and economically (no savings to relocate).
constraint_indexing:constraint_classification(precision_manufacturing_automation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL PRECISION MANUFACTURER (TANGLED ROPE) — Small shops benefit from coordination through shared automation standards, tool compatibility, and supply chain efficiency. But they face extraction through capital requirements and vendor lock-in. Cannot afford to retool independently; must join automation ecosystem or exit. Constrained by capital cost, not trapped — exit is possible at price (converting to repair/maintenance services, relocating to lower-cost region). Mixed experience: genuine coordination function (standards enable compatibility) alongside asymmetric extraction (vendors capture rents).
constraint_indexing:constraint_classification(precision_manufacturing_automation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LARGE INTEGRATED MANUFACTURER (ROPE) — Benefits from automation investment through productivity gains, cost reduction, and export competitiveness. Experiences the constraint as pure coordination: shared standards reduce integration costs, and the automation ecosystem enables economies of scale. Has arbitrage options (can automate or not; can compete in high-precision or mass-market segments). Net beneficiary — coordination mechanism subsidizes their competitive advantage.
constraint_indexing:constraint_classification(precision_manufacturing_automation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: AUTOMATION VENDOR (ROPE) — Equipment manufacturers and software firms benefit from standardization (larger addressable market, lower integration cost per sale). The constraint is a pure coordination mechanism from their perspective: common interfaces, compatible toolchains, and industry standards enable network effects and drive adoption. Has arbitrage options (can develop proprietary vs open systems, can target different market segments). Benefits from network growth without bearing extraction costs.
constraint_indexing:constraint_classification(precision_manufacturing_automation, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LABOR ORGANIZING COALITION (SCAFFOLD) — Union organizers and labor advocates see automation constraints as temporary structural problems solvable through policy intervention. Retraining funds, wage insurance, sectoral bargaining, and universal basic income represent exit paths. The constraint's extraction is high but framed as sunset-able through political organizing. Constrained by political power dynamics, but with agency to shift the terms through collective action. Theater ratio is lower from this perspective — material coordination (retraining infrastructure) is substituting for performative accommodation.
constraint_indexing:constraint_classification(precision_manufacturing_automation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INDUSTRIAL POLICY APPARATUS (PITON) — Government trade agencies, industrial development bureaus, and standards bodies maintain automation coordination frameworks through inertia. The institutional machinery persists (ISO standards bodies, equipment certification processes, export promotion agencies) despite questionable functional output: policy fails to meaningfully direct automation toward public benefit or to mitigate displacement. The apparatus is largely performative — maintains theater of industrial planning without steering outcomes. Theater ratio high; coordination function degraded.
constraint_indexing:constraint_classification(precision_manufacturing_automation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational scope, precision automation is a hybrid: genuine coordination of technical standards (tools, data formats, quality control systems) combined with asymmetric distribution of automation's benefits and costs. Workers bear displacement costs while capital captures productivity gains. The constraint persists because automation vendors and large manufacturers benefit from network effects; displacement costs are externalized onto workers and small firms. Classification: tangled rope. The coordination is real; the extraction is structural.
constraint_indexing:constraint_classification(precision_manufacturing_automation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(precision_manufacturing_automation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(precision_manufacturing_automation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(precision_manufacturing_automation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(precision_manufacturing_automation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(precision_manufacturing_automation, TR),
    TR >= 0.70.

:- end_tests(precision_manufacturing_automation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts significant value from displaced workers (lost wages, relocation costs, retraining requirements) and from small manufacturers (capital requirements, retooling costs). However, extraction is not maximal because the constraint does enable genuine productivity improvements that benefit the economy as a whole—the extraction is not pure rent-seeking but a redistribution of real gains. The rising trajectory (0.32 → 0.64) reflects that automation maturity increases displacement scope while efficiency gains narrow. Suppression (0.52): Moderate-high. Barriers to exit include capital requirements for automation (economic suppression of small firms), skill non-transferability (economic suppression of workers), geographic immobility (structural suppression of workers), and age discrimination in hiring (structural suppression of older displaced workers). Suppression is not total because some workers successfully retrain and some small firms adapt; policy interventions can reduce suppression. Theater ratio (0.38): Low-moderate. The coordination function is substantive—technical standards solve real compatibility problems—so the theater is lower than in purely extractive constraints. Industrial policy frameworks (standards bodies, certification processes) do perform coordination work, not just theater. The low theater reflects that the constraint solves a genuine technical problem, even as it extracts from workers and small firms.
 *
 * PERSPECTIVAL GAP:
 *   The constraint presents radically different faces depending on perspective. Large manufacturers see Rope (pure coordination mechanism enabling efficiency). Automation vendors see Rope (network effects driving adoption). Displaced workers see Snare (inescapable extraction of their labor market position). Small manufacturers see Tangled Rope (mixed: standards help them integrate, but capital requirements hurt them). Labor organizers see Scaffold (temporary problem solvable through policy). Industrial policy sees Piton (maintains coordination theater without steering outcomes toward workers). The analytical observer sees Tangled Rope (genuine coordination + structural extraction). The perspectival gap reveals that 'automation' as a natural force benefiting everyone is a beneficiary narrative—the constraint is not natural or inevitable but institutionally structured to extract from workers and small firms while concentrating gains in capital.
 *
 * DIRECTIONALITY LOGIC:
 *   Displaced workers (powerless/trapped) experience d ≈ 0.95 → f(d) ≈ 1.42 → high experienced extraction. They are the target of the constraint; have no exit; bear full cost. Small manufacturers (moderate/constrained) experience d ≈ 0.65 → f(d) ≈ 1.00 → moderate experienced extraction. They face high but surmountable costs. Large manufacturers (institutional/arbitrage) experience d ≈ 0.05 → f(d) ≈ -0.12 → negative effective extraction. They are beneficiaries; have exit options; benefits exceed costs. Automation vendors (organized/arbitrage) experience d ≈ 0.15 → f(d) ≈ -0.01 → near-zero extraction. They benefit from network effects; experience the constraint as pure coordination. The analytical observer (analytical/analytical) computes d ≈ 0.72 → f(d) ≈ 1.15 → moderate-high observed asymmetry in extraction distribution.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves through perspectival multiplicity. 'Automation' is not a monolithic force but a hybrid coordination-extraction system whose classification depends on structural position. Beneficiaries genuinely experience Rope (coordination reduces costs). Victims genuinely experience Snare (inescapable extraction). The analytical observer correctly identifies Tangled Rope because both the coordination function and the extraction mechanism are structurally real. The mandatrophy 'Is automation good or bad?' dissolves when classified as tangled rope: automation is simultaneously coordination (enabling productivity) and extraction (concentrating gains, externalizing costs). Policy interventions (wage insurance, retraining, sectoral bargaining) can reduce extraction without eliminating coordination—they shift the balance toward Rope by mitigating suppression. The constraint does not disappear; it changes type based on policy choices.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    displacement_permanence_timeline,
    'Are displaced workers permanently excluded from precision manufacturing, or is the exclusion temporary (technology learning curve) vs structural (capital substituting for labor)?',
    'Longitudinal employment tracking of displaced workers; correlation between age/experience and reemployment success; sector comparison with historical displacement patterns',
    'If temporary: snare classification is transient; worker exit is biographical (Rope from generational perspective). If permanent: snare is structural; economic mobility is broken.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_permanence_timeline, empirical, 'Whether worker displacement from automation is temporary or permanent').

omega_variable(
    automation_vendor_lock_in_severity,
    'Do small manufacturers face genuine vendor lock-in and switching costs, or are alternative toolchains and standards available at reasonable cost?',
    'Cost-benefit analysis of retooling scenarios; market analysis of competing automation platforms; survey of small manufacturers on actual switching barriers vs perceived barriers',
    'If lock-in is severe: small manufacturers are trapped (snare). If alternatives are viable: they are constrained (tangled rope). If switching is cheap: rope (coordination dominates).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(automation_vendor_lock_in_severity, empirical, 'Severity of vendor lock-in for small precision manufacturers').

omega_variable(
    automation_capital_requirement_threshold,
    'What is the minimum capital investment threshold below which a small firm cannot participate in precision automation, and how does this threshold compare to typical small manufacturer balance sheets?',
    'Equipment cost surveys; firm balance sheet analysis; capital adequacy ratios by firm size; comparison with wage requirements for skilled labor they would replace',
    'If threshold is > 50% of annual revenue: structural barrier (victim classification confirmed). If < 20%: constraint is more modest (victim classification overstated). Threshold determines whether extraction is by exclusion or by inclusion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(automation_capital_requirement_threshold, empirical, 'Capital investment threshold for precision automation participation').

omega_variable(
    skill_transferability_across_domains,
    'How transferable are precision manufacturing skills to adjacent technical sectors (aerospace parts, medical devices, semiconductor fabrication)?',
    'Skills mapping across manufacturing subsectors; wage comparison for transferred workers; employer surveys on cross-sector hiring',
    'If highly transferable: workers have hidden exit options (constrained, not trapped). If non-transferable: trap is structural; workers are truly powerless.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_transferability_across_domains, empirical, 'Transferability of precision manufacturing skills to other sectors').

omega_variable(
    policy_intervention_effectiveness,
    'Have sectoral bargaining, wage insurance, or retraining programs in peer economies (Germany, Denmark, Singapore) successfully mitigated automation displacement, or do they merely redistribute costs without solving the underlying constraint?',
    'Comparative analysis of countries with active automation adjustment policy vs passive displacement; employment and wage outcomes for displaced workers under different policy regimes; coalition analysis of whether interventions reduce extraction or merely legitimize it',
    'If effective: scaffold perspective is real — sunset is achievable through policy. If ineffective: scaffold is aspirational theater; the constraint persists regardless of policy performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_intervention_effectiveness, empirical, 'Whether active labor policy can resolve automation displacement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(precision_manufacturing_automation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prec_mfg_tr_t0, precision_manufacturing_automation, theater_ratio, 0, 0.28).
narrative_ontology:measurement(prec_mfg_tr_t3, precision_manufacturing_automation, theater_ratio, 3, 0.32).
narrative_ontology:measurement(prec_mfg_tr_t6, precision_manufacturing_automation, theater_ratio, 6, 0.38).
narrative_ontology:measurement(prec_mfg_tr_t9, precision_manufacturing_automation, theater_ratio, 9, 0.42).

% Extraction over time
narrative_ontology:measurement(prec_mfg_be_t0, precision_manufacturing_automation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(prec_mfg_be_t3, precision_manufacturing_automation, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(prec_mfg_be_t6, precision_manufacturing_automation, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(prec_mfg_be_t9, precision_manufacturing_automation, base_extractiveness, 9, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(precision_manufacturing_automation, resource_allocation).
narrative_ontology:affects_constraint(precision_manufacturing_automation, labor_market_geographic_immobility).
narrative_ontology:affects_constraint(precision_manufacturing_automation, skill_obsolescence_acceleration).
narrative_ontology:affects_constraint(precision_manufacturing_automation, capital_concentration_dynamics).

% DUAL FORMULATION NOTE:
% Precision manufacturing automation is upstream of several related constraints: the geographic immobility of workers in single-industry regions (they cannot exit locally because all jobs are manufacturing), skill obsolescence (tacit knowledge in manual precision becomes valueless), and capital concentration (only large firms can afford automation investment). These are linked through the same mechanism: automation extracts from workers and small firms while concentrating capital.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
