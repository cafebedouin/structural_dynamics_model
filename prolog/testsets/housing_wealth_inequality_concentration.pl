% ============================================================================
% CONSTRAINT STORY: housing_wealth_inequality_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_housing_wealth_inequality_concentration, []).

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
 *   constraint_id: housing_wealth_inequality_concentration
 *   human_readable: Housing Wealth Inequality Concentration Mechanism
 *   domain: economic/political/social
 *
 * SUMMARY:
 *   Housing wealth inequality concentration represents a structural
 *   constraint where the coordination mechanisms for allocating residential
 *   space and financing homeownership have become vehicles for wealth
 *   extraction that reinforces preexisting inequality. The constraint
 *   exhibits both genuine coordination functions (mortgage markets, property
 *   rights clarity, capital formation) and asymmetric extraction (wealth
 *   accumulation concentrated among existing owners, systematic barriers to
 *   new entrants, intergenerational wealth transfer effects). The
 *   extractiveness metric (0.58) reflects that the constraint has intensified
 *   over the measurement interval as housing costs have risen faster than
 *   incomes, down-payment requirements have become more prohibitive relative
 *   to typical wealth accumulation, and existing homeowner equity has grown
 *   exponentially. The suppression metric (0.65) captures multiple barriers:
 *   financial (down-payment accumulation, debt service capacity),
 *   informational (asymmetric knowledge about neighborhood dynamics,
 *   financing options), and institutional (zoning restrictions, credit
 *   gatekeeping). The theater ratio (0.55) reflects that policy responses
 *   have grown increasingly performative: inclusionary housing mandates with
 *   insufficient enforcement, affordability programs with limited funding,
 *   zoning reform proposals that generate political conflict without scaling
 *   solutions.
 *
 * KEY AGENTS:
 *   - First-time homebuyers: Primary victims (powerless/trapped) — face compounding barriers including wage stagnation relative to housing costs, down-payment accumulation requirements, and credit constraints with no structural exit path
 *   - Renters priced out of ownership: Secondary victims (powerless/trapped) — experience indefinite rental market extraction with no path to equity accumulation despite full-time employment
 *   - Future generations: Tertiary victims (powerless/identity_locked) — inherit both reduced wealth-building opportunities and identity-locked expectations that homeownership is necessary for economic security
 *   - Existing homeowners with equity: Primary beneficiaries (institutional/arbitrage) — capture appreciation without productive contribution; can exit into alternative assets or relocate with low transaction friction
 *   - Real estate investment firms: Secondary beneficiaries (institutional/arbitrage) — extract through property appreciation, rental income, and leverage; operate with superior access to capital and information
 *   - Financial institutions: Institutional extractors (institutional/constrained) — benefit from interest margins, origination fees, and servicing profits while coordinating credit allocation; regulatory constraints prevent complete exit
 *   - Housing policy reformers: Organized agents (organized/constrained) — perceive constraint as contingent and fixable through policy intervention (zoning reform, inclusionary housing, down-payment assistance); building alternative mechanisms with sunset logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(housing_wealth_inequality_concentration, 0.58).
domain_priors:suppression_score(housing_wealth_inequality_concentration, 0.65).
domain_priors:theater_ratio(housing_wealth_inequality_concentration, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(housing_wealth_inequality_concentration, extractiveness, 0.58).
narrative_ontology:constraint_metric(housing_wealth_inequality_concentration, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(housing_wealth_inequality_concentration, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(housing_wealth_inequality_concentration, tangled_rope).
narrative_ontology:human_readable(housing_wealth_inequality_concentration, "Housing Wealth Inequality Concentration Mechanism").
narrative_ontology:topic_domain(housing_wealth_inequality_concentration, "economic/political/social").

domain_priors:requires_active_enforcement(housing_wealth_inequality_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(housing_wealth_inequality_concentration, existing_homeowners_with_equity).
narrative_ontology:constraint_beneficiary(housing_wealth_inequality_concentration, real_estate_investment_firms).
narrative_ontology:constraint_beneficiary(housing_wealth_inequality_concentration, financial_institutions).
narrative_ontology:constraint_victim(housing_wealth_inequality_concentration, first_time_homebuyers).
narrative_ontology:constraint_victim(housing_wealth_inequality_concentration, renters_priced_out_of_ownership).
narrative_ontology:constraint_victim(housing_wealth_inequality_concentration, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCKED-OUT RENTER (SNARE) — Faces compounding barriers: stagnant wages relative to housing costs, high down-payment requirements, credit barriers, and geographic lock from where jobs exist. Cannot exit the rental market into ownership without exceptional income growth or inheritance. Bears full extraction through rent extraction and opportunity cost of foregone wealth accumulation.
constraint_indexing:constraint_classification(housing_wealth_inequality_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ASPIRING HOMEBUYER (TANGLED ROPE) — Constrained by down-payment accumulation requirements and debt service capacity but benefits from the housing market's coordination function: clear price signals, established financing mechanisms, and property rights clarity enable the market to allocate housing. High extraction burden but genuine coordination benefit exists. Can exit through family wealth transfer or exceptional earning, but at significant cost.
constraint_indexing:constraint_classification(housing_wealth_inequality_concentration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXISTING HOMEOWNER WITH EQUITY (ROPE) — Primary beneficiary. Experiences housing market as coordination mechanism for wealth preservation and appreciation. Benefits from the constraint without bearing its costs. Can exit into other asset classes, downsize, or relocate with minimal friction. Arbitrage options abundant. Effective extraction runs toward this agent.
constraint_indexing:constraint_classification(housing_wealth_inequality_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FINANCIAL INSTITUTION (TANGLED ROPE) — Institutional actor that both coordinates (mortgage origination, credit allocation) and extracts (interest margins, origination fees, servicing profits). Constrained by regulatory environment and capital requirements but benefits from the constraint through predictable extraction mechanisms. Genuine coordination function exists alongside asymmetric profit capture.
constraint_indexing:constraint_classification(housing_wealth_inequality_concentration, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: HOUSING POLICY REFORM COALITION (SCAFFOLD) — Organized actors (housing advocates, local governments, some progressive policymakers) see the constraint as a temporary institutional failure with a sunset clause: zoning reform, inclusionary housing mandates, down-payment assistance programs, and community land trusts represent alternative pathways. Sees the current extraction mechanism as contingent on policy choices, not inherent. Extraction declines if policy alternatives mature.
constraint_indexing:constraint_classification(housing_wealth_inequality_concentration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: REGULATORY FRAMEWORK (PITON) — Housing policy maintains substantial performative content: zoning regulations that claim to preserve neighborhood character while functioning as exclusionary wealth protection; fair housing laws that prohibit discrimination while systemic patterns persist; affordability mandates with token enforcement. The regulatory apparatus persists through institutional inertia despite degraded function. Theater ratio reflects gap between stated goals (housing access) and actual mechanisms (wealth concentration acceleration).
constraint_indexing:constraint_classification(housing_wealth_inequality_concentration, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / SCARCITY VIEW (MOUNTAIN) — From civilizational perspective, housing wealth inequality appears immutable: land scarcity, population growth, and coordination costs around property rights appear as natural constraints. This perspective naturalizes what are actually contingent institutional arrangements (zoning restrictions, financing mechanisms, tax treatment). The engine will flag this as a false summit revealing naturalization.
constraint_indexing:constraint_classification(housing_wealth_inequality_concentration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(housing_wealth_inequality_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(housing_wealth_inequality_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(housing_wealth_inequality_concentration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(housing_wealth_inequality_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(housing_wealth_inequality_concentration, TR),
    TR >= 0.70.

:- end_tests(housing_wealth_inequality_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, intensifying over time. The housing market coordinates legitimate functions — property rights allocation, financing mechanisms, price discovery — but extraction has grown as existing homeowner wealth appreciation has accelerated faster than wage growth, creating compounding wealth gaps. The 1980-2020 trajectory shows extractiveness rising from ~0.35 to ~0.58 as housing costs relative to income doubled in many markets. Base extraction reflects that new entrants must pay appreciating prices set by existing wealth, and financing costs are scaled to asset value rather than occupant income. Suppression (0.65): High. Multiple reinforcing barriers prevent exit: down-payment requirements (typically 10-20% of purchase price) require 5-10 years of savings for median-income households; debt-to-income ratio caps prevent borrowing even when cash is available; credit score gatekeeping excludes those with prior financial disruption; geographic constraints tie jobs to expensive housing markets. These barriers are not uniform — existing homeowners and investors have credit access and leverage that new entrants cannot match. Theater ratio (0.55): Moderate-high. Policy responses have grown performative: zoning variance processes create theater of deliberation without changing outcomes; inclusionary housing mandates lack enforcement mechanisms; affordability programs are under-funded relative to problem scale; 'smart growth' rhetoric masks exclusionary policies. The gap between policy activity and policy effect has widened as the constraint has intensified.
 *
 * PERSPECTIVAL GAP:
 *   The housing constraint demonstrates perspectival disagreement rooted in structural asymmetry, not subjective interpretation. The existing homeowner genuinely experiences the constraint as Rope — the market coordinates property transfer and capital formation, and they benefit from appreciation. This is not a false belief; it is a legitimate description of their structural experience. The locked-out renter genuinely experiences it as Snare — they face extraction through rent without exit and no mechanism for wealth accumulation. This is also accurate to their structural experience. Neither observer is wrong; they occupy different positions in the extraction flow. The gap reveals that the constraint is Tangled Rope at the analytic level — it contains both coordination (legitimate market functions) and asymmetric extraction (wealth concentration). The scaffold perspective (housing reformers) sees the constraint as fixable through policy redesign — the coordination functions could persist while extraction could be reduced through zoning reform, inclusionary housing, and alternative financing. The piton perspective (regulatory system) sees performative theater maintaining the status quo. The mountain perspective risks naturalizing what are actually contingent policy choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from structural position relative to extraction flow. Existing homeowners with equity start near d=0.1 (full beneficiaries with arbitrage exit — they capture appreciation without bearing costs and can exit into alternative assets). Locked-out renters approach d=0.95 (full victims with trapped exit — they bear rental extraction with no structural path to ownership). Aspiring homebuyers fall in the middle (~d=0.65) — they experience significant extraction through financing costs and can eventually enter ownership but only after substantial wealth accumulation burden. Financial institutions sit at ~d=0.35 (beneficiaries with constrained exit — they profit from interest and fees but face regulatory constraints). The sigmoid function f(d) converts these structural positions into effective power modifiers. Beneficiaries experience low or negative chi (effective extraction runs toward them); victims experience high chi (they bear the extraction). This explains why the beneficiary (existing homeowner) sees Rope (low extraction) while the victim (locked-out renter) sees Snare (high extraction) — they are measuring from different structural positions, not disagreeing about facts.
 *
 * MANDATROPHY ANALYSIS:
 *   The housing wealth inequality constraint resolves mandatrophy through perspectival decomposition. The constraint is claimed as Tangled Rope at the analytical level because it contains: (1) genuine coordination function — mortgage markets allocate capital efficiently, property rights enable investment and improvement, price signals coordinate supply and demand; (2) asymmetric extraction — wealth appreciation captured by existing owners without productive contribution, barriers prevent new entrant participation at equal terms, financing mechanisms extract interest and fees scaled to asset value rather than affordability. The mandatrophy is resolved by showing that all six types are valid perspectival readings: existing owners see Rope (coordination with positive asymmetry), locked-out renters see Snare (extraction with no exit), aspiring buyers see Tangled Rope (mixed coordination and extraction), financial institutions see Tangled Rope (coordinating credit with asymmetric extraction), policy reformers see Scaffold (temporary coordination failure with policy sunset), regulatory systems see Piton (performative theater), and the analytical observer risks seeing Mountain (naturalizing policy contingencies). The constraint claim is Tangled Rope because the extraction is not pure (coordination mechanisms are genuine) and cannot be eliminated without sacrificing the coordination function, but the asymmetry is not merely distributional — it is structural and self-reinforcing. The beneficiaries have power to maintain the constraint through regulatory capture and wealth influence. The victims lack power to exit or reform. Reform requires organized intervention that hasn't yet matured to full scale (hence Scaffold from reform coalition perspective).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    zoning_reform_feasibility,
    'Is broad zoning reform politically achievable, or does existing homeowner wealth concentration make it impossible to alter?',
    'Empirical analysis of zoning reform momentum in high-constraint markets; measurement of political opposition intensity correlated with homeowner equity concentration; tracking of successful vs failed reform attempts',
    'If reform feasible: scaffold sunset clause is real, constraint trajectories decline over time. If reform impossible: constraint becomes more mountain-like (structural immutability), and alternative extraction mechanisms (intensified rent extraction, intergenerational exclusion) dominate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zoning_reform_feasibility, empirical, 'Whether zoning reform can overcome incumbent wealth opposition').

omega_variable(
    inherent_coordination_extraction_boundary,
    'What proportion of observed housing extraction is inherent coordination cost (credit allocation, property administration) vs contingent institutional rent-seeking?',
    'Comparison of housing markets with different regulatory structures; analysis of lending margins, servicing costs, and origination fees across jurisdictions; identification of which components persist across regime changes',
    'High coordination cost suggests constraints should shift toward Rope for beneficiaries. Low coordination cost confirms Tangled Rope asymmetry. This determines whether the constraint is defendable as necessary or revealed as extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherent_coordination_extraction_boundary, empirical, 'Proportion of extraction attributable to coordination cost vs rent-seeking').

omega_variable(
    intergenerational_identity_lock_mechanism,
    'Is homeownership identity-locked for succeeding generations, or can exit options remain mobile despite intergenerational wealth disparity?',
    'Generational attitude surveys on homeownership necessity; analysis of generational political positions toward housing policy; tracking of generational wealth-building alternatives (education, retirement savings, equity participation)',
    'If identity-locked: future-generation perspectives should use identity_locked exit option, revealing cognitive capture independent of structural mobility. If mobile: exit options remain constrained but not identity-bound, and reform becomes possible through reframing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_identity_lock_mechanism, empirical, 'Whether homeownership becomes identity-fused across generations').

omega_variable(
    regulatory_theater_measurement,
    'How much of housing policy activity is performative (zoning variance processes, affordability mitigation that doesn''t scale) vs functionally effective?',
    'Cost-benefit analysis of housing policy mechanisms; measurement of policy output (units created, affordability achieved) vs policy activity (hearings, variance approvals, compliance documents); tracking of policy substitution (one mandate replaces another without net effect change)',
    'High theater ratio (>0.65) confirms piton degradation. Low theater ratio (<0.45) suggests regulatory system is gaining function and extractiveness is declining. Determines whether scaffold sunset is real or aspirational.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_theater_measurement, empirical, 'What proportion of housing regulation is performative vs functional').

omega_variable(
    wealth_concentration_feedback_stability,
    'Does housing wealth concentration create a self-reinforcing feedback (existing wealth → pricing power → more concentration) that prevents equilibration?',
    'Time-series analysis of wealth concentration metrics in relation to policy changes; modeling of equilibration dynamics under different policy regimes; identification of threshold conditions where feedback becomes unstable',
    'If self-reinforcing: constraint requires active intervention to prevent collapse into pure extraction (Snare dominance). If equilibrating: market mechanisms eventually create correction and constraint approaches Rope equilibrium. Determines whether scaffold policy intervention is necessary or merely accelerating natural market clearing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wealth_concentration_feedback_stability, empirical, 'Whether housing wealth concentration creates self-reinforcing feedback').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(housing_wealth_inequality_concentration, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hwic_tr_t0, housing_wealth_inequality_concentration, theater_ratio, 0, 0.4).
narrative_ontology:measurement(hwic_tr_t10, housing_wealth_inequality_concentration, theater_ratio, 10, 0.48).
narrative_ontology:measurement(hwic_tr_t20, housing_wealth_inequality_concentration, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(hwic_be_t0, housing_wealth_inequality_concentration, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hwic_be_t10, housing_wealth_inequality_concentration, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(hwic_be_t20, housing_wealth_inequality_concentration, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(housing_wealth_inequality_concentration, resource_allocation).
narrative_ontology:affects_constraint(housing_wealth_inequality_concentration, intergenerational_wealth_transfer).
narrative_ontology:affects_constraint(housing_wealth_inequality_concentration, urban_exclusionary_zoning).
narrative_ontology:affects_constraint(housing_wealth_inequality_concentration, mortgage_credit_gatekeeping).

% DUAL FORMULATION NOTE:
% Housing wealth inequality concentration decomposes into three structurally distinct constraints: (1) intergenerational_wealth_transfer (ε=0.45, Tangled Rope) — family property transfers concentrate existing wealth without productive contribution; (2) urban_exclusionary_zoning (ε=0.68, Snare) — zoning restrictions create artificial scarcity and prevent supply response to demand; (3) mortgage_credit_gatekeeping (ε=0.52, Tangled Rope) — credit allocation mechanisms coordinate capital but extract through interest margins and credit rationing. This story represents the higher-level aggregation of these mechanisms and their interaction effects. Each component story has its own ε value; the aggregated constraint's extractiveness reflects their combined effect.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(housing_wealth_inequality_concentration, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
