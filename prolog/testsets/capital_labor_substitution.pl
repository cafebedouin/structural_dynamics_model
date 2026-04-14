% ============================================================================
% CONSTRAINT STORY: capital_labor_substitution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_capital_labor_substitution, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: capital_labor_substitution
 *   human_readable: Capital-Labor Substitution as Technological Constraint
 *   domain: economic/labor_markets/technology
 *
 * SUMMARY:
 *   Capital-labor substitution is the systematic displacement of human
 *   workers by capital equipment (machinery, automation, computation) in
 *   production processes. This constraint structures modern labor markets and
 *   generates distributional conflict between capital owners (who benefit
 *   from cost reduction and productivity gains) and workers (who bear
 *   displacement risk and wage suppression). The constraint exhibits high
 *   suppression (0.62) because displaced workers face severe barriers to
 *   exit: retraining costs, geographic immobility, age discrimination,
 *   credentialism in new fields, and absence of alternative employment at
 *   comparable skill levels. It exhibits moderate extractiveness (0.58)
 *   because some genuine coordination exists alongside extraction — capital
 *   and labor do cooperate to produce goods, and for skilled workers,
 *   complementarity rather than substitution is the primary relationship.
 *   Theater ratio (0.48) reflects that while some performative protection
 *   exists (unemployment insurance, retraining programs, labor regulations),
 *   these policies' functional capacity to shield workers from substitution
 *   has degraded as displacement has accelerated and institutional capacity
 *   has stagnated. The constraint classifies as tangled_rope in the base
 *   analysis: it has a genuine coordination function (capital and labor
 *   together create productive capacity) and systematic asymmetric extraction
 *   (captured by capital owners through ownership rights and pricing power).
 *   The perspectival analysis reveals the full complexity: from the displaced
 *   worker's position, it appears as a snare (pure extraction, no exit); from
 *   the capital owner's position, as pure rope (coordination with full
 *   agency); from the institutional position maintaining labor protections,
 *   as a piton (performative degraded ritual); from the organized reskilling
 *   coalition, as a scaffold with sunset (temporary problem being solved).
 *   The false mountain perspective (immutable economic law) naturalized what
 *   are actually contingent policy choices about intellectual property,
 *   capital taxation, and labor market regulation.
 *
 * KEY AGENTS:
 *   - Displaced Workers: Primary victim (powerless/trapped) — bear extraction through income loss, skill obsolescence, and barriers to reentry; no exit from labor market participation requirement
 *   - Capital Owners / Firms: Primary beneficiary (institutional/arbitrage) — extract through reduced labor costs and increased capital returns; full agency to reallocate capital and adjust strategy
 *   - Remaining Manufacturing Workers: Secondary victim (moderate/constrained) — experience mixed coordination and extraction; higher productivity but competitive wage pressure and deskilling risk
 *   - Skilled Technical Workers: Secondary beneficiary (powerful/mobile) — benefit from complementarity with capital investment; experience extraction through global arbitrage and specialization lock
 *   - Welfare State / Labor Protection Institutions: Institutional actor (institutional/constrained) — maintain performative protection policies; capacity degrades as substitution accelerates
 *   - Reskilling / Transition Coalition: Organized actor (organized/constrained) — building alternative labor market categories and pathways; see sunset mechanism in education and skill development
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional arrangements as economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(capital_labor_substitution, 0.58).
domain_priors:suppression_score(capital_labor_substitution, 0.62).
domain_priors:theater_ratio(capital_labor_substitution, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(capital_labor_substitution, extractiveness, 0.58).
narrative_ontology:constraint_metric(capital_labor_substitution, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(capital_labor_substitution, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(capital_labor_substitution, tangled_rope).
narrative_ontology:human_readable(capital_labor_substitution, "Capital-Labor Substitution as Technological Constraint").
narrative_ontology:topic_domain(capital_labor_substitution, "economic/labor_markets/technology").

domain_priors:requires_active_enforcement(capital_labor_substitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(capital_labor_substitution, capital_owners).
narrative_ontology:constraint_beneficiary(capital_labor_substitution, firms_with_automation_access).
narrative_ontology:constraint_victim(capital_labor_substitution, displaced_workers).
narrative_ontology:constraint_victim(capital_labor_substitution, low_skill_labor_supply).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED WORKER (SNARE) — Trapped by lack of retraining access, geographic immobility, age discrimination, and absence of alternative employment pathways. Bears full extraction: loses income, skill currency, and identity without reciprocal benefit. No exit available; suppression enforced through labor market collapse in deindustrialized regions.
constraint_indexing:constraint_classification(capital_labor_substitution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MANUFACTURING WORKER — REMAINING (TANGLED ROPE) — Constrained but not trapped: access to higher productivity through capital investment, higher wages for skilled roles, but also competitive pressure and deskilling risk. Mixed experience: genuine coordination of production processes alongside asymmetric extraction of effort differentiation and wage suppression through threat of automation.
constraint_indexing:constraint_classification(capital_labor_substitution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPITAL OWNER / FIRM (ROPE) — Direct beneficiary. Experiences the constraint as pure coordination: substitution enables economies of scale, cost reduction, and competitive advantage. Exit through arbitrage is unrestricted — can relocate capital, adjust capital allocation, or invest in competing automation paths. Net beneficiary with full agency.
constraint_indexing:constraint_classification(capital_labor_substitution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: WELFARE STATE / LABOR REGULATION (PITON) — Policy institutions maintain labor protections (minimum wage, unemployment insurance, retraining programs) that are increasingly performative as the underlying labor demand declines. The institutions persist through political inertia and historical obligation, but their functional capacity to protect workers from substitution degradation has eroded. Theater ratio high: protective policies create appearance of worker support while substitution proceeds regardless.
constraint_indexing:constraint_classification(capital_labor_substitution, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: RESKILLING / TRANSITION COALITION (SCAFFOLD) — Organized agents (education institutions, labor unions, development agencies, technology transfer initiatives) see substitution as a temporary coordination failure with a sunset: building alternative skill pathways, retraining infrastructure, and new labor market categories (data science, AI supervision, care work) that absorb displaced workers. Sees high suppression but finite horizon — expects reskilling to reduce extraction as new labor markets mature.
constraint_indexing:constraint_classification(capital_labor_substitution, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: SKILLED TECHNICAL WORKER (TANGLED ROPE) — Mobile at high skill level; benefits from complementarity with capital investment. Experiences coordination (capital + skilled labor creating new productive capacity) alongside extraction (wage pressure from global labor arbitrage, deskilling of adjacent roles creating downward wage pressure in technical occupations). Genuine agency but constrained by knowledge currency and specialization lock.
constraint_indexing:constraint_classification(capital_labor_substitution, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / PRODUCTIVITY DOCTRINE (MOUNTAIN) — From a civilizational view, capital-labor substitution appears as an immutable economic law: given competitive pressure, firms must substitute labor for capital or exit markets. The constraint emerges naturally from the logic of rational economic actors. However, this naturalizes policy choices (patent duration, capital tax structure, skill certification barriers) that shape substitution rates. The engine will detect this as a false summit.
constraint_indexing:constraint_classification(capital_labor_substitution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(capital_labor_substitution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(capital_labor_substitution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(capital_labor_substitution, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(capital_labor_substitution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(capital_labor_substitution, TR),
    TR >= 0.70.

:- end_tests(capital_labor_substitution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Capital-labor substitution extracts value from workers through displacement (income loss), wage suppression (from credible threat of substitution), and externalization of retraining costs. The value is not higher (0.65+) because genuine coordination exists — firms and remaining workers do cooperate, and productivity gains are real. The measurement reflects that extraction is significant but coexists with legitimate productive cooperation. Suppression (0.62): High. Barriers to worker exit are substantial: lack of retraining access, geographic immobility (housing costs, community ties), age discrimination (40+ workers face retraining barriers), skill specificity in existing roles, and certification requirements in new fields. Retraining programs exist but are underfunded and often lead to wage degradation. Unemployment insurance and social safety nets are inadequate to prevent household destabilization. Theater ratio (0.48): Moderate. Labor regulations, unemployment insurance, and retraining programs create appearance of worker protection, but their capacity has degraded. The theater is not as high (0.65+) as in purely ceremonial constraints because some actual protection occurs — the degradation is real. The theater has increased over the measurement interval as substitution has accelerated faster than institutional adaptation.
 *
 * PERSPECTIVAL GAP:
 *   Maximum divergence between beneficiary (rope) and powerless victim (snare) perspectives. The capital owner's arbitrage exit and beneficiary status produce d ≈ 0.10, f(d) ≈ -0.05, resulting in rope classification and experienced negative extraction. The displaced worker's trapped exit and victim status produce d ≈ 0.95, f(d) ≈ 1.42, resulting in snare classification and experienced maximum extraction. The gap reflects genuine structural divergence: one agent gains from the constraint (can exit, benefits from productivity), the other loses completely (cannot exit, bears all displacement cost). The institutional perspectives (welfare state, reskilling coalition) occupy middle positions but with qualitatively different agency: the piton sees degrading protective function while maintaining ritual; the scaffold sees a temporary problem with real sunset mechanisms. The skilled worker perspective (tangled_rope) shows partial overlap with beneficiaries in some roles (complementarity to capital) alongside extraction in others (global wage arbitrage). The analytical mountain perspective risks naturalizing this divergence as inevitable, but the policy-modifiability of substitution rates (shown through comparative institutional analysis) reveals that the perspectival gap reflects different institutional designs and policy choices, not different laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's structural position. Capital owners (beneficiaries with arbitrage exit) have d ≈ 0.10-0.15, producing negative or minimal χ — they experience the constraint as beneficial coordination. Displaced workers (victims with trapped exit) have d ≈ 0.95, producing maximum χ through f(d) — they experience full extraction with no escape. Remaining workers (victims with constrained exit) have d ≈ 0.65-0.75, experiencing moderate extraction with some agency. Skilled workers (beneficiaries with mobile exit) have d ≈ 0.40-0.50, experiencing mixed coordination and extraction. Reskilling coalition (organized agents with constrained exit but institutional power) have d ≈ 0.45, experiencing extraction but with collective capacity to shape outcomes. The piton perspective (institutional labor protection) has d ≈ 0.50-0.60, experiencing the constraint as increasingly ineffective coordination. The scale modifier σ(S) = 1.2 for global scope amplifies extractiveness for global perspectives, reflecting that capital mobility reduces local labor power.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves through perspectival decomposition and institutional stage analysis. At the analytical level, the constraint appears to approach mountainhood (immutable economic law of rational competition), but structural analysis reveals this is false: substitution rates vary with policy choices (intellectual property duration, capital taxation, labor market regulation, retraining investment). The constraint is a tangled_rope shaped by institutional design, not an immutable law. The extraction-to-coordination ratio shifts across career stages: early in automation adoption (t=0), coordination dominates (genuine productivity gains, some worker opportunity); as substitution accelerates (t=5-10), extraction increases (wage suppression, displacement, closure of pathways). The Scaffold perspective from the reskilling coalition reveals the sunset mechanism: if alternative labor markets (care work, AI supervision, data work) develop at scale, and if retraining succeeds in matching displaced workers to new roles, the constraint's extraction function declines over 15-20 years. However, if retraining fails and new labor categories saturate before absorbing all displaced workers, the snare classification persists. The piton perspective is diagnostically important: labor protections have become increasingly performative, suggesting the institutional response is not matching the structural problem's pace. The false mountain at the civilizational analytical level is the core mandatrophy — it naturalizes contingent institutional choices (capital ownership concentration, weak labor negotiation power, underfunded retraining) as economic necessity. Recognition that substitution rates are policy-modifiable reframes the constraint as a coordination problem amenable to institutional redesign rather than an inexorable force.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substitution_rate_determinacy,
    'Is the substitution rate (pace and scale of capital-labor displacement) determined by technology or by policy/institutional choices?',
    'Comparative analysis: sectors with high unionization vs. low unionization; countries with strong labor protections vs. weak protections; periods with robust retraining investment vs. periods without. If substitution rate varies inversely with policy intervention, policy is primary determinant.',
    'If technology-determined: constraint is closer to mountain (immutable). If policy-determined: constraint is tangled_rope or snare shaped by institutional choices and reversible through policy redesign.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substitution_rate_determinacy, empirical, 'Whether substitution rate is technology-determined or policy-determined').

omega_variable(
    reabsorption_capacity,
    'Do displaced workers reabsorb into the labor market at wages and skill levels comparable to pre-displacement, or does the reabsorption permanently degrade worker outcomes?',
    'Longitudinal wage tracking of displaced workers; comparison of pre-displacement earnings to post-displacement earnings 5, 10, 20 years after displacement; analysis of skill-matching efficiency in retraining pathways.',
    'If reabsorption is high-quality: scaffold sunset is real, extraction is temporary. If reabsorption is low-quality (gig work, wage degradation, skill mismatch): extraction persists beyond technology cycle, snare classification is more accurate than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reabsorption_capacity, empirical, 'Whether displaced workers reabsorb at comparable wages').

omega_variable(
    capital_immobility_constraints,
    'How immobile is capital in practice? Can capital owners costlessly relocate, adjust allocation, or reinvest, or do path dependencies, sunk costs, and skill specificity in capital constrain their mobility?',
    'Analysis of capital reallocation patterns post-displacement; firm-level data on reinvestment choices; comparison of capital mobility across industries and time periods.',
    'If capital is highly mobile: beneficiary perspective (arbitrage) is accurate. If capital is constrained: beneficiary may be identity_locked or constrained rather than mobile, reducing their directionality advantage and raising their experienced extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_immobility_constraints, empirical, 'Whether capital is actually costlessly mobile').

omega_variable(
    skill_complementarity_boundaries,
    'Is there a threshold skill level below which labor and capital become substitutes rather than complements, and above which new labor categories emerge faster than they can be displaced?',
    'Historical analysis of labor market bifurcation; skill-wage relationship over time; identification of occupational categories that expanded despite automation.',
    'If clear complementarity threshold exists: skilled workers are durably protected (tangled_rope), displaced workers are durably trapped (snare). If no threshold: even skilled work is eventually substitutable, and all labor faces long-term extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(skill_complementarity_boundaries, conceptual, 'Whether skill complementarity provides durable protection for some labor').

omega_variable(
    alternative_distribution_mechanisms,
    'Does capital-labor substitution require extraction-based income redistribution (concentrated capital ownership, weak labor protections), or can productivity gains be distributed through ownership democratization, cooperative structures, or universal basic income?',
    'Comparison of countries/sectors with high worker ownership participation vs. traditional capital structures; analysis of redistribution mechanisms; policy simulation models.',
    'If alternative distribution is feasible: constraint is tangled_rope with modifiable extraction (policy can reduce asymmetry). If alternative distribution is infeasible: extraction is structural to substitution, snare classification dominates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_distribution_mechanisms, preference, 'Whether alternative distribution mechanisms can eliminate extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(capital_labor_substitution, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cls_tr_t0, capital_labor_substitution, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cls_tr_t5, capital_labor_substitution, theater_ratio, 5, 0.38).
narrative_ontology:measurement(cls_tr_t10, capital_labor_substitution, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(cls_be_t0, capital_labor_substitution, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cls_be_t5, capital_labor_substitution, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(cls_be_t10, capital_labor_substitution, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(capital_labor_substitution, resource_allocation).
narrative_ontology:boltzmann_floor_override(capital_labor_substitution, 0.18).
narrative_ontology:affects_constraint(capital_labor_substitution, wage_stagnation).
narrative_ontology:affects_constraint(capital_labor_substitution, skill_certification_gatekeeping).
narrative_ontology:affects_constraint(capital_labor_substitution, geographic_immobility_housing).
narrative_ontology:affects_constraint(capital_labor_substitution, unemployment_insurance_adequacy).
narrative_ontology:affects_constraint(capital_labor_substitution, intellectual_property_duration).

% DUAL FORMULATION NOTE:
% Capital-labor substitution is the parent constraint affecting all downstream labor market constraints. Wage stagnation is the direct extraction outcome; skill certification and housing immobility are suppression mechanisms; unemployment insurance adequacy and IP duration are policy levers affecting substitution rates. Each downstream constraint has its own ε but all depend on the substitution mechanism's core extractiveness and suppression.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(capital_labor_substitution, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
