% ============================================================================
% CONSTRAINT STORY: integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_integration_primary, []).

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
 *   constraint_id: integration_primary
 *   human_readable: Integration-Primary Single Market: Mobile Labor vs. Local Protection
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   The integration-primary reading of federation membership asserts that
 *   free movement is constitutive — restrictions are presumptively
 *   illegitimate unless narrowly justified by compelling public policy. This
 *   constraint exhibits tangled-rope structure: it solves genuine
 *   coordination problems (efficient labor matching, capital allocation)
 *   while simultaneously extracting from local labor markets and welfare
 *   systems that cannot restrict access. The constraint's extractiveness has
 *   risen from 0.35 to 0.52 over 20 years as welfare-state heterogeneity and
 *   labor-market protection norms have created increasing friction between
 *   integration-primary doctrine and local institutional capacity. Theater
 *   ratio has similarly risen from 0.28 to 0.45, reflecting that enforcement
 *   has increasingly taken on performative character — supranational
 *   authority demonstrates commitment to the principle through aggressive
 *   nullification while the underlying coordination function has degraded.
 *   This story instantiates ONE reading of the federation_membership_treaty
 *   kernel. Two sibling readings exist: sovereignty_primary (member states'
 *   right to control borders and welfare systems is paramount) and
 *   subsidiarity_balance (integration and autonomy are balanced through
 *   flexible coordination). These three readings produce structurally
 *   distinct constraints with different ε values, beneficiary/victim sets,
 *   and perspectives.
 *
 * KEY AGENTS:
 *   - Mobile Workers: Primary beneficiaries (institutional/arbitrage) — gain presumptive right to work across jurisdictions without national restriction; capture wage arbitrage opportunities
 *   - Multinational Corporations: Primary beneficiaries (institutional/arbitrage) — deploy labor across borders without hiring friction; reduce labor cost variance across markets
 *   - Local Labor Markets: Primary victims (powerless/trapped) — face systematic suppression of employment protections and wage-setting capacity; cannot exit federation membership
 *   - Welfare Systems: Primary victims (powerless/trapped) — cannot restrict access to migrant-dependent services; face fiscal pressure without revenue instruments to compensate
 *   - National Governments: Secondary actors (organized/constrained) — bound by treaty obligation to enforce integration-primary; coordinate labor flows but lose immigration policy autonomy
 *   - Supranational Authority: Institutional enforcer (institutional/constrained) — maintains doctrine and nullification apparatus; theater ratio indicates enforcement ritual increasingly performative
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing political-economic choice as economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(integration_primary, 0.52).
domain_priors:suppression_score(integration_primary, 0.68).
domain_priors:theater_ratio(integration_primary, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(integration_primary, extractiveness, 0.52).
narrative_ontology:constraint_metric(integration_primary, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(integration_primary, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(integration_primary, tangled_rope).
narrative_ontology:human_readable(integration_primary, "Integration-Primary Single Market: Mobile Labor vs. Local Protection").
narrative_ontology:topic_domain(integration_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(integration_primary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(integration_primary, mobile_workers).
narrative_ontology:constraint_beneficiary(integration_primary, multinational_corporations).
narrative_ontology:constraint_beneficiary(integration_primary, service_sector_providers).
narrative_ontology:constraint_victim(integration_primary, local_labor_markets).
narrative_ontology:constraint_victim(integration_primary, welfare_systems).
narrative_ontology:constraint_victim(integration_primary, regional_employment_protection).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCAL LABOR MARKET (SNARE) — Cannot exit the single market regime; bears full cost of wage competition from mobile workers with lower reservation wages. Regional employment protections are systematically suppressed. No alternative coordination framework available. Maximum experienced extraction — abstract collective of incumbent workers has no advocate and no escape route.
constraint_indexing:constraint_classification(integration_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: WELFARE SYSTEM (SNARE) — Cannot restrict access without violating integration-primary doctrine. Faces fiscal pressure from migrant-dependent services (healthcare, education) while contribution bases erode. National capacity to regulate who accesses public goods is systematically suppressed. Extraction flows toward mobile agents; local welfare bears cost.
constraint_indexing:constraint_classification(integration_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: NATIONAL GOVERNMENT (TANGLED ROPE) — Constrained by treaty obligation to enforce integration-primary doctrine; coordinating labor flows serves genuine economic efficiency. But enforcement mechanism extracts sovereignty: cannot protect local labor, cannot set immigration thresholds, cannot adjust welfare eligibility. Mixed experience — some benefit from labor market efficiency, significant cost in lost policy autonomy.
constraint_indexing:constraint_classification(integration_primary, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MOBILE WORKER (ROPE) — Benefits from presumptive right of free movement; low barriers to entry and wage arbitrage. Experiences constraint as enabling coordination: right to work anywhere unlocks access to higher-wage jurisdictions. Net beneficiary — extraction runs toward this agent.
constraint_indexing:constraint_classification(integration_primary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MULTINATIONAL CORPORATION (ROPE) — Benefits from ability to deploy labor across borders without national restriction. Experiences constraint as enabling coordination: low-friction movement of workers reduces hiring friction and labor cost variance. Net beneficiary — extraction runs toward this agent.
constraint_indexing:constraint_classification(integration_primary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: SUPRANATIONAL AUTHORITY (PITON) — Maintains integration-primary doctrine and enforcement apparatus, but the underlying functional coordination (labor market matching, wage compression, efficient capital allocation) has degraded as welfare-state and labor-market heterogeneity have increased. The enforcement ritual persists (vigilant nullification of member state restrictions) despite declining real coordination function. Theater ratio indicates that much enforcement is performative — demonstrating commitment to the principle rather than solving actual coordination problems.
constraint_indexing:constraint_classification(integration_primary, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, free movement of factors of production is an economic law: integrated markets require frictionless factor mobility, and restrictions inevitably reduce efficiency. This perspective sees integration-primary as a natural consequence of federation, not a contingent institutional choice. However, structural data reveals beneficiaries and asymmetric extraction — the false summit detector will identify this as naturalization of a political-economic choice.
constraint_indexing:constraint_classification(integration_primary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(integration_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(integration_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(integration_primary, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(integration_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(integration_primary, TR),
    TR >= 0.70.

:- end_tests(integration_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, rising over interval. Base extraction reflects asymmetric distribution of benefits (mobile agents capture arbitrage gains) and costs (local institutions bear suppression of protective capacity). The rise from 0.35 to 0.52 indicates increasing strain as labor-market protection expectations and welfare heterogeneity have diverged from integration-primary requirements. Suppression (0.68): High. National governments face systematic suppression of immigration control, welfare eligibility restrictions, and labor-market protection. Member states cannot implement sectoral labor restrictions, duration limits, or welfare access conditions that would normally be legitimate policy tools. The suppression is not absolute — narrow public policy exceptions exist — but the burden is on member states to justify restrictions, not on mobile agents to justify entry. Tangled rope structure is confirmed: genuine coordination function (labor matching, capital efficiency) paired with high suppression of alternative mechanisms and asymmetric extraction.
 *
 * PERSPECTIVAL GAP:
 *   Integration-primary doctrine creates a systematic perspectival inversion: the agents who experience maximum extraction (local labor markets, welfare systems, national governments) have minimal formal voice in the constraint's definition, while the agents who benefit (mobile workers, multinational firms) have presumptive legitimacy built into the doctrine itself. The beneficiaries experience the constraint as enabling (rope); the victims experience it as coercive (snare). The supranational authority experiences it as degrading (piton) — the enforcement ritual persists but the underlying coordination function decays as institutional heterogeneity increases. The analytical observer risks naturalizing this arrangement as economic law rather than political choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile workers have arbitrage exit options and beneficiary status → d ≈ 0.10 → f(d) ≈ -0.12 → negative χ (rope). Multinational corporations similarly beneficiary + arbitrage → d ≈ 0.12 → f(d) ≈ -0.08 → negative χ (rope). National governments constrained by treaty + mixed beneficiary-victim status → d ≈ 0.55 → f(d) ≈ 0.75 (tangled rope). Local labor markets powerless + trapped + victim → d ≈ 0.92 → f(d) ≈ 1.38 (snare). Welfare systems powerless + trapped + victim → d ≈ 0.90 → f(d) ≈ 1.32 (snare). The directive function of the scope modifier σ(S) scales χ at continental scope (σ=1.1), amplifying effective extraction experienced by local agents. This accounts for the global diffusion of integration-primary doctrine reducing local capacity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that tangled_rope classification is appropriate: genuine coordination function exists (labor matching, capital allocation efficiency) paired with high suppression of alternative mechanisms and asymmetric extraction benefiting mobile agents. The risk of false classification as pure rope (ignoring victim extraction) or pure snare (ignoring coordination function) is high — the constraint uses coordination language to justify suppression. The tangled_rope classification holds both dimensions simultaneously: it solves problems for some agents while extracting from others through suppression of alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is this constraint a reading of federation_membership_treaty kernel instantiating integration_primary, or is an alternative reading (sovereignty_primary or subsidiarity_balance) the correct structural interpretation?',
    'Historical analysis of treaty text evolution, judicial precedent patterns, and member state policy shifts over time. Compare which reading best predicts actual enforcement patterns and which reading member states appeal to when defending restrictions.',
    'Integration-primary reading: ε = 0.52 (tangled rope), mobile workers as beneficiaries, local labor markets as victims, suppression ≥ 0.68. Sovereignty-primary reading: ε ≤ 0.35 (rope or scaffold), member states as beneficiaries, supranational authority as victim, suppression ≤ 0.45. Subsidiarity-balance reading: ε ≈ 0.40 (rope with theater), coordination across both axes, suppression ≈ 0.50.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which treaty reading (integration_primary vs sovereignty_primary vs subsidiarity_balance) is the correct structural interpretation').

omega_variable(
    welfare_fiscal_causality,
    'Does welfare system fiscal pressure originate from integration-primary suppression of member state immigration control, or from independent demographic and fiscal factors unrelated to the constraint?',
    'Counterfactual analysis: compare welfare expenditure trajectories in federations with strong integration-primary doctrine vs. those with weaker federation membership requirements. Isolate contribution from migrant-dependent services vs. aging populations and benefit expansions.',
    'If integration-primary is causal: welfare systems are genuine victims; suppression metric justified at 0.68+. If independent factors dominate: welfare cost is misattributed to the constraint; suppression should be 0.45-0.55.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_fiscal_causality, empirical, 'Whether welfare fiscal pressure is caused by integration-primary suppression of immigration control').

omega_variable(
    labor_market_extraction_threshold,
    'At what wage differential and unemployment displacement rate does mobile labor influx transition from efficiency coordination to extractive wage suppression?',
    'Econometric analysis of wage growth and employment rates in regions with high migrant inflows vs. baseline; identification of threshold wage compression that exceeds coordination benefits.',
    'If threshold crossed in observed data: extraction component of tangled_rope is empirically grounded. If threshold not crossed: constraint is closer to pure rope (coordination dominates). Affects whether local labor market should classify as snare or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_extraction_threshold, empirical, 'Labor market wage suppression threshold for mobile labor influx').

omega_variable(
    alternative_federation_models,
    'Would subsidiarity_balance reading or sovereignty_primary reading produce structurally different constraints with measurably different ε values and victim sets?',
    'Construct hypothetical constraint stories for sibling readings; compare ε, suppression, beneficiary/victim declarations, and perspectival gaps. If ε differs by >0.15, readings are structurally distinct constraints.',
    'If sibling readings are structurally distinct: three separate constraint stories should be written (federation_membership_treaty with three readings). If ε differences are marginal: integration_primary is the primary reading and siblings are perspectival variation. This omega gates the network decomposition decision.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_federation_models, conceptual, 'Whether sibling treaty readings generate structurally distinct constraints with different ε values').

omega_variable(
    supranational_enforcement_capacity,
    'Is the supranational authority''s enforcement capacity genuinely degrading (piton pattern), or is the constraint''s force and functional purpose shifting (tangled_rope with changing composition)?',
    'Longitudinal analysis of nullification rate (fraction of member state restrictions successfully challenged), enforcement timeline (speed of challenge to nullification), and member state compliance rate with adverse rulings.',
    'If degrading: piton classification confirmed — theater ratio ≥ 0.70, enforcement ritual persists without proportional function. If constant or increasing: enforcement is active and functional; constraint may be shifting type but not degrading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supranational_enforcement_capacity, empirical, 'Whether supranational enforcement capacity is degrading or shifting type').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(integration_primary, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(intprim_tr_t0, integration_primary, theater_ratio, 0, 0.28).
narrative_ontology:measurement(intprim_tr_t10, integration_primary, theater_ratio, 10, 0.38).
narrative_ontology:measurement(intprim_tr_t20, integration_primary, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(intprim_be_t0, integration_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(intprim_be_t10, integration_primary, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(intprim_be_t20, integration_primary, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(integration_primary, resource_allocation).
narrative_ontology:affects_constraint(integration_primary, sovereignty_primary).
narrative_ontology:affects_constraint(integration_primary, subsidiarity_balance).

% DUAL FORMULATION NOTE:
% Integration-primary is one reading of the federation_membership_treaty kernel. Two sibling readings (sovereignty_primary and subsidiarity_balance) produce structurally distinct constraints with different ε values, beneficiary/victim sets, and enforcement mechanisms. Each reading should be authored as a separate constraint story. The kernel contest is not perspectival variation but structural decomposition: readers produce different metrics and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
