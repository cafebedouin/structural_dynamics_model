% ============================================================================
% CONSTRAINT STORY: diaspora_return_disincentive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_diaspora_return_disincentive, []).

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
 *   constraint_id: diaspora_return_disincentive
 *   human_readable: Diaspora Return Disincentive Structure
 *   domain: migration/economic_policy/national_identity
 *
 * SUMMARY:
 *   The diaspora return disincentive constraint operates at the intersection
 *   of individual rational choice and institutional design, creating a
 *   structure where departure from origin country becomes progressively
 *   irreversible. As diaspora members accumulate destination-country assets,
 *   credentials, social capital, and family ties, the economic and
 *   psychological cost of repatriation increases exponentially.
 *   Simultaneously, origin countries maintain formal commitment to diaspora
 *   return through ministries and engagement programs, yet preserve
 *   structural barriers (credential non-recognition, housing market
 *   discrimination, visa preferences favoring destination-country employment)
 *   that make repatriation economically irrational. The constraint exhibits
 *   all six DR types from different perspectives, revealing how the same
 *   institutional structure functions as pure extraction (Snare) for trapped
 *   diaspora agents, as coordination (Rope) for destination-country
 *   employers, as mixed benefit-burden (Tangled Rope) for origin-country
 *   policy makers, as performative ritual (Piton) for international mobility
 *   regimes, as a solvable problem with sunset (Scaffold) for organized
 *   diaspora movements, and as an immutable feature of human choice
 *   (Mountain) from the civilizational analytical view.
 *
 * KEY AGENTS:
 *   - Diaspora Member: Primary victim (powerless/trapped) — accumulates sunk costs in destination country; faces extraction through credential non-recognition and opportunity cost of return
 *   - Destination Country Employer: Primary beneficiary (institutional/arbitrage) — accesses vetted, educated labor without repatriation risk; benefits from retention mechanisms that lock in diaspora workers
 *   - Origin Country Policy Maker: Secondary actor (moderate/constrained) — loses human capital and tax revenue but benefits from remittances and knowledge links; experiences tangled rope constraint
 *   - International Mobility Regime: Institutional actor (institutional/arbitrage) — maintains performative diaspora engagement programs while preserving structural return barriers; sees degraded return-facilitation function (piton)
 *   - Diaspora Organizing Movements: Organized agents (organized/constrained) — build temporary coordination mechanisms (job matching, credential bridging) that lower return barriers with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional extraction as immutable consequence of individual rational choice and accumulated attachment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(diaspora_return_disincentive, 0.52).
domain_priors:suppression_score(diaspora_return_disincentive, 0.58).
domain_priors:theater_ratio(diaspora_return_disincentive, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(diaspora_return_disincentive, extractiveness, 0.52).
narrative_ontology:constraint_metric(diaspora_return_disincentive, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(diaspora_return_disincentive, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(diaspora_return_disincentive, tangled_rope).
narrative_ontology:human_readable(diaspora_return_disincentive, "Diaspora Return Disincentive Structure").
narrative_ontology:topic_domain(diaspora_return_disincentive, "migration/economic_policy/national_identity").

domain_priors:requires_active_enforcement(diaspora_return_disincentive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(diaspora_return_disincentive, destination_country_employers).
narrative_ontology:constraint_beneficiary(diaspora_return_disincentive, host_nation_tax_base).
narrative_ontology:constraint_beneficiary(diaspora_return_disincentive, receiving_country_institutions).
narrative_ontology:constraint_victim(diaspora_return_disincentive, diaspora_agents).
narrative_ontology:constraint_victim(diaspora_return_disincentive, origin_country_human_capital).
narrative_ontology:constraint_victim(diaspora_return_disincentive, family_separation_costs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DIASPORA MEMBER (SNARE) — Structurally trapped by accumulated costs of return. Each year abroad increases sunk costs in destination-country credentials, social capital, housing equity, and family ties. Repatriation means forfeiting these assets without equivalent recognition in origin country. Career trajectory is locked into destination-country institutional path. Identity increasingly fused with diaspora role. Maximum extraction experienced — cannot exit without catastrophic loss, yet extraction flow runs to destination-country employers and institutions.
constraint_indexing:constraint_classification(diaspora_return_disincentive, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DESTINATION COUNTRY EMPLOYER (ROPE) — Benefits from access to diaspora talent without repatriation risk. Diaspora workers represent stable, vetted labor supply with educational investment already completed elsewhere. Low extraction perception because employers experience the constraint as pure coordination: retention mechanisms (visa sponsorship, credential recognition barriers, social integration) solve the labor supply coordination problem. Net beneficiary position.
constraint_indexing:constraint_classification(diaspora_return_disincentive, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ORIGIN COUNTRY POLICY MAKER (TANGLED ROPE) — Bears loss of human capital and tax revenue (victim status) but also experiences coordination benefits from diaspora remittances, knowledge transfer, and innovation links (beneficiary status). Significant constraints on exit: cannot forcibly repatriate without violating international norms, cannot cut diaspora ties without losing remittance flow. Mixed extraction — the origin country is both losing skilled workers and benefiting from their external productivity and financial transfers.
constraint_indexing:constraint_classification(diaspora_return_disincentive, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL MOBILITY REGIME (PITON) — The formal institutional structures (visa systems, credential reciprocity agreements, professional licensing frameworks) that ostensibly enable return are substantially performative. Countries declare commitment to diaspora engagement, create diaspora ministry units, and organize diaspora bonds, but the core extraction mechanisms (visa discrimination, credential non-recognition, housing market exclusion from origin country) persist. Theater ratio high (0.65) reflects that many diaspora programs are ritualistic rather than functionally removing barriers. The regime persists through inertia and symbolic politics despite low actual return facilitation.
constraint_indexing:constraint_classification(diaspora_return_disincentive, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DIASPORA ORGANIZING MOVEMENTS (SCAFFOLD) — Organized diaspora agents (professional networks, community organizations, digital platforms) are building temporary coordination mechanisms that lower return barriers: job-matching services for repatriation, credential bridging programs, virtual participation options for origin-country institutions. These create exit pathways with declining suppression over time. The scaffold has a sunset clause: as return barriers formally reduce (through policy reform or institutional reciprocity), organized movement structures become unnecessary coordination layers and dissolve.
constraint_indexing:constraint_classification(diaspora_return_disincentive, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal, civilizational perspective, return disincentive appears to be an immutable feature of human migration: accumulated attachment to place, social capital, and economic integration inevitably create sunk costs that make repatriation irrational. This naturalizes return disincentive as a consequence of individual rational choice rather than structural extraction. However, the structural data contradicts this mountain classification — the suppression (0.58) and requires_active_enforcement (true) reveal that institutional arrangements, not natural human preferences, generate the disincentive.
constraint_indexing:constraint_classification(diaspora_return_disincentive, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(diaspora_return_disincentive_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(diaspora_return_disincentive, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(diaspora_return_disincentive, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(diaspora_return_disincentive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(diaspora_return_disincentive, TR),
    TR >= 0.70.

:- end_tests(diaspora_return_disincentive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts diaspora agent surplus through multiple channels: destination-country employers capture labor below its marginal revenue product due to return disincentive (the diaspora worker cannot credibly threaten exit); origin country loses human capital asset to destination country; diaspora agent bears full cost of credential non-recognition and housing market discrimination that discourages return. The extraction is not total (diaspora members do accumulate wealth, develop careers, build families) but systematic. Suppression (0.58): Moderate-high. Barriers to return include: credential non-recognition systems, housing market access discrimination, visa/employment sponsorship ties to destination country, family dependency on destination-country institutions, opportunity cost of leaving developed-market income for origin-country wage scales. Barriers are not total (some diaspora members do return) but substantial and increasingly accumulate. Theater ratio (0.65): Moderate-high. Origin country diaspora programs (bonds, ministries, engagement initiatives) are substantially performative. They signal commitment to diaspora return while preserving the structural mechanisms (credential discrimination, housing barriers, employment visa lock) that prevent it. The theater has increased as diaspora populations grew without corresponding institutional reform.
 *
 * PERSPECTIVAL GAP:
 *   The critical gap exists between diaspora members' Snare experience and destination-country employers' Rope experience of the same constraint. The diaspora agent sees pure extraction (trapped, maximum cost to exit); the employer sees pure coordination (solving labor supply stability). This gap reveals that the constraint's classification depends entirely on structural position — the agent trapped by accumulated sunk costs perceives immutability; the agent benefiting from that immutability perceives efficiency. The origin country occupies the middle position: it loses the human capital asset but gains the remittance flow, producing Tangled Rope experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from the agent's structural position relative to the extraction flow. Diaspora members trapped by credential/housing barriers occupy high d (target position, d≈0.90), experiencing maximum extraction. Destination-country employers with arbitrage options occupy low d (beneficiary position, d≈0.10), experiencing negative effective extraction. Origin-country policy makers constrained by both losses and gains occupy mid d (d≈0.50), producing symmetric experience. The piton perspective shows how institutional arrangements that appear to facilitate return (diaspora ministries, diaspora bonds) actually mask persistent extraction mechanisms. The scaffold perspective shows how organized diaspora movements can lower effective d through coordination tools. The mountain perspective risks d≈0.72 (analytical canonical), but structural data reveals this as naturalization of contingent extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through decomposition into multiple structural mechanisms. The false natural-law claim (Mountain) asserts that return disincentive is inherent to rational choice under sunk costs. But this naturalizes institutional design choices (credential non-recognition, visa discrimination, housing access barriers) as natural facts. The tangled rope classification captures the true structure: there is genuine coordination value (employers solving labor retention, origin countries solving remittance flows) combined with systematic asymmetric extraction (diaspora agents paying the full cost of credential/housing discrimination). The piton classification correctly identifies that many diaspora programs are performative theater masking persistent barriers. The scaffold classification correctly identifies that diaspora organizing movements can reduce suppression mechanisms over generational timescale, creating exit pathways. The snare classification from the trapped diaspora perspective correctly centers the extraction actually experienced by the primary target. No single type is 'correct' — the perspectival ensemble across all six types captures the full structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sunk_cost_vs_extraction,
    'Are return disincentives primarily sunk costs from legitimate destination-country investment or structural extraction mechanisms designed to trap diaspora labor?',
    'Comparative analysis of return barriers in origin countries with high credential reciprocity vs low reciprocity; correlation between institutional barriers and return rates controlling for individual financial gains',
    'If primarily sunk costs: classification shifts toward Rope (coordination to achieve labor supply stability). If primarily extraction: classification remains Snare/Tangled Rope with high suppression mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunk_cost_vs_extraction, empirical, 'Attribution of return barriers to sunk costs vs institutional extraction').

omega_variable(
    remittance_dependency_lock,
    'Does origin country dependency on diaspora remittances constitute genuine coordination (diaspora providing financial services) or reciprocal extraction (origin country dependent on diaspora extraction)?',
    'Structural analysis of remittance flows vs origin country fiscal capacity; historical comparison of countries pre/post remittance dependency; simulation of alternative human capital retention strategies',
    'If coordination: tangled rope classification confirmed. If reciprocal extraction: origin country may itself be victim of diaspora capital extraction, creating three-level extraction hierarchy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(remittance_dependency_lock, empirical, 'Whether remittances represent coordination or reciprocal extraction').

omega_variable(
    identity_lock_mechanism,
    'Is diaspora return resistance driven by identity fusion with destination-country institutions (identity_locked exit) or by material sunk costs (trapped/constrained exit)?',
    'Post-return ethnographic data on identity reorientation timelines; comparison of return intentions vs actual return for agents at different identity-fusion stages; analysis of return-migrate narratives about self-concept change',
    'If identity_locked: biographical time horizon produces Rope (perceives mutability) while trapped produces Mountain (perceives immutability). If material sunk costs: all horizons produce Mountain from trapped perspective. Defines whether the lock is cognitive or structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether diaspora return barriers are identity-based or material').

omega_variable(
    institutional_coordination_sincerity,
    'Are origin country diaspora engagement programs (bonds, ministries, visas) genuine coordination attempts or performative theater masking continued extraction disincentives?',
    'Comparative policy analysis: document credential reciprocity agreements, visa discrimination residuals, housing market barriers, employment discrimination by sector; cross-reference against diaspora return rates and program expenditures',
    'If sincere: theater_ratio should be lower (< 0.40). If performative: piton classification confirmed (theater_ratio > 0.70 with low functional return facilitation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_coordination_sincerity, empirical, 'Sincerity of institutional diaspora engagement programs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(diaspora_return_disincentive, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(diaspora_tr_t0, diaspora_return_disincentive, theater_ratio, 0, 0.48).
narrative_ontology:measurement(diaspora_tr_t5, diaspora_return_disincentive, theater_ratio, 5, 0.58).
narrative_ontology:measurement(diaspora_tr_t10, diaspora_return_disincentive, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(diaspora_be_t0, diaspora_return_disincentive, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(diaspora_be_t5, diaspora_return_disincentive, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(diaspora_be_t10, diaspora_return_disincentive, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(diaspora_return_disincentive, resource_allocation).
narrative_ontology:affects_constraint(diaspora_return_disincentive, brain_drain_human_capital_loss).
narrative_ontology:affects_constraint(diaspora_return_disincentive, remittance_dependency_lock).
narrative_ontology:affects_constraint(diaspora_return_disincentive, credential_non_recognition).

% DUAL FORMULATION NOTE:
% The diaspora return disincentive decomposes into three structurally distinct constraints with different ε values: credential non-recognition (institutional discrimination, ε≈0.35), remittance dependency (origin country fiscal lock, ε≈0.48), and brain drain (human capital asset loss, ε≈0.40). This story models the aggregate constraint structure; upstream stories model specific mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(diaspora_return_disincentive, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
