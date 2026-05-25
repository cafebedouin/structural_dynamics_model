% ============================================================================
% CONSTRAINT STORY: dublin_economic_desperation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dublin_economic_desperation, []).

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
 *   constraint_id: dublin_economic_desperation
 *   human_readable: Dublin Economic Desperation and Survival Extraction
 *   domain: urban_economics/labor_exploitation
 *
 * SUMMARY:
 *   Dublin's economic desperation constraint emerges from a collision between
 *   success (tech sector concentration attracting global investment and
 *   talent) and institutional failure (housing supply cannot scale due to
 *   zoning restrictions, planning delays, and property tax regime designed
 *   for different purposes). The constraint operates as a snare from the
 *   perspective of trapped workers: housing costs consume 50-70% of
 *   precarious incomes, forcing acceptance of exploitative wages,
 *   below-market benefits, and degraded working conditions. Desperation
 *   itself becomes the enforcement mechanism — no coercion is required when
 *   housing costs exceed income unless workers accept precarious terms. This
 *   is extraction machinery that runs on human necessity rather than
 *   institutional force. The constraint exhibits high suppression (0.72)
 *   because exit routes are structurally blocked: geographic job
 *   concentration in Dublin (multinationals cluster for ecosystem effects),
 *   lack of affordable alternatives in secondary Irish cities, and EU
 *   relocation friction (language, credential transfer, social networks)
 *   create a trapped population. Theater ratio (0.58) reflects policy
 *   discourse that invokes housing crisis language while maintaining
 *   zoning/planning mechanisms that perpetuate scarcity. The institutional
 *   actors (landlords, employers, property developers) each experience the
 *   constraint as a coordination mechanism (Rope) with high personal benefit
 *   and low enforcement cost — desperation provides voluntary compliance at
 *   scale.
 *
 * KEY AGENTS:
 *   - Precarious Workers: Primary victims (powerless/trapped) — service sector, gig workers, low-income earners forced into exploitative terms by housing desperation
 *   - Constrained Workers: Secondary victims (moderate/constrained) — skilled professionals with relocation capacity but face significant costs, partial exit available at high price
 *   - Landlords and Property Investors: Primary beneficiaries (institutional/arbitrage) — capture rent extraction powered by desperation; experience zero enforcement cost
 *   - Multinational Tech Employers: Secondary beneficiaries (institutional/arbitrage) — geographic wage arbitrage enabled by desperation; global exit optionality
 *   - Labor Unions: Organized but partially captured (organized/constrained) — can coordinate wages but blocked from addressing housing supply root cause
 *   - Housing Policy Framework: Institutional structure (institutional/arbitrage) — piton: zoning/planning mechanisms persist through inertia while extraction mechanisms remain active
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing policy-contingent desperation as inevitable city dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dublin_economic_desperation, 0.68).
domain_priors:suppression_score(dublin_economic_desperation, 0.72).
domain_priors:theater_ratio(dublin_economic_desperation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dublin_economic_desperation, extractiveness, 0.68).
narrative_ontology:constraint_metric(dublin_economic_desperation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dublin_economic_desperation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dublin_economic_desperation, snare).
narrative_ontology:human_readable(dublin_economic_desperation, "Dublin Economic Desperation and Survival Extraction").
narrative_ontology:topic_domain(dublin_economic_desperation, "urban_economics/labor_exploitation").

domain_priors:requires_active_enforcement(dublin_economic_desperation).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dublin_economic_desperation, landlords).
narrative_ontology:constraint_beneficiary(dublin_economic_desperation, service_sector_employers).
narrative_ontology:constraint_beneficiary(dublin_economic_desperation, property_developers).
narrative_ontology:constraint_victim(dublin_economic_desperation, precarious_workers).
narrative_ontology:constraint_victim(dublin_economic_desperation, low_income_residents).
narrative_ontology:constraint_victim(dublin_economic_desperation, service_sector_employees).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIOUS WORKER (SNARE) — Structurally trapped by housing costs consuming 50-70% of income, forcing acceptance of exploitative wage terms. Relocation out of Dublin is blocked by geographic job concentration and lack of affordable alternatives. Cannot exit without destroying economic viability. Experiences maximum extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(dublin_economic_desperation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MOBILE BUT CONSTRAINED WORKER (TANGLED ROPE) — Can physically relocate to secondary Irish cities or EU destinations, but faces significant costs: career interruption, social network dissolution, language/credential transfer friction. Constrained exit means accepting both housing extraction in Dublin and lower wages elsewhere. Mixed experience: some coordination (skill matching through job market), substantial extraction (forced cost arbitrage).
constraint_indexing:constraint_classification(dublin_economic_desperation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MULTINATIONAL EMPLOYER (ROPE) — Experiences Dublin desperation as coordination mechanism: geographic wage arbitrage allows cost-controlled operations, Dublin's international connectivity provides talent pool, housing crisis concentrates labor supply. High exit optionality (can relocate to Berlin, Austin, or Singapore); experiences constraint as coordination benefit with low personal extraction.
constraint_indexing:constraint_classification(dublin_economic_desperation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LANDLORD/PROPERTY INVESTOR (ROPE) — Pure beneficiary. Housing desperation is coordination mechanism for rent extraction. No enforcement cost — market mechanisms (desperation itself) enforce compliance. Zero suppression cost because desperation volunteers labor and compliance. Constraint appears as frictionless wealth transfer.
constraint_indexing:constraint_classification(dublin_economic_desperation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 5: LABOR UNION (TANGLED ROPE) — Organized but constrained. Can negotiate coordinated wage standards (coordination function: preventing race-to-bottom) but simultaneously blocked from addressing root extraction mechanism (housing supply). Union power proves partially illusory — wage gains are consumed by desperation-driven rent increases. Mixed: genuine coordination activity alongside ineffectual struggle against extraction.
constraint_indexing:constraint_classification(dublin_economic_desperation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: HOUSING POLICY FRAMEWORK (PITON) — Zoning restrictions, planning delays, and property tax design exist as vestigial mechanisms originally designed to protect residential neighborhoods and local control. The original functions (controlling sprawl, preserving green space) have atrophied while the extraction mechanisms persist. Theater ratio high: policy documents invoke liveability and community while constraining supply that would relieve desperation. Bureaucratic ritual maintains restrictions despite acknowledged housing crisis.
constraint_indexing:constraint_classification(dublin_economic_desperation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER — NATURALIZATION RISK (FALSE SUMMIT) — From civilizational/global scope, Dublin desperation can be naturalized as inevitable consequence of successful city attracting in-migration faster than housing supply can scale. 'Cities are expensive' becomes framed as natural law. However, the structural data reveals contingent policy choices (zoning, planning delays, property tax regime) that actively constrain supply. The mountain classification is a false summit — it naturalizes what is demonstrably a choice-dependent institutional arrangement.
constraint_indexing:constraint_classification(dublin_economic_desperation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dublin_economic_desperation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dublin_economic_desperation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dublin_economic_desperation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dublin_economic_desperation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dublin_economic_desperation, TR),
    TR >= 0.70.

:- end_tests(dublin_economic_desperation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The metric captures income diversion to housing at rates that exceed coordination cost. Base value of 0.48 at interval start reflects Dublin's attractiveness providing some genuine benefit (job access, infrastructure, networks). Rising to 0.68 by interval end reflects acceleration of housing costs outpacing wage growth, rendering benefit increasingly illusory. Suppression (0.72): Very high. Exit barriers are structural (geographic job concentration, lack of secondary-city alternatives, EU relocation friction, social network costs). Desperation functions as self-enforcing suppression — no external coercion needed. Theater ratio (0.58): Moderate-high and rising. Policy documents invoke 'housing crisis' language and 'affordability targets' while maintaining zoning restrictions that prevent supply scaling. The policy theater grows as the gap widens between stated goals (affordable housing) and maintained mechanisms (constrained supply). Economic desperation is real; policy response is substantially performative.
 *
 * PERSPECTIVAL GAP:
 *   The snare classification is not universal. From the landlord/employer perspective, this is coordination (Rope) — geographic concentration solves matching problems, desperation provides voluntary compliance. From the trapped worker perspective, this is pure extraction (Snare) — no alternative, no benefit, only forced acceptance. The piton perspective on policy recognizes that housing supply constraints are maintained by vestigial institutional mechanisms (zoning designed for neighborhood control, planning delays designed for public consultation) that persist despite acknowledged crisis. Theater rises as policy documents invoke affordability while mechanisms remain unchanged. The analytical observer risks naturalizing this as 'inevitable city dynamics' (mountain) when it is demonstrably policy-contingent (can be reversed by zoning reform, planning acceleration, property tax redesign).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position relative to extraction flow. Trapped workers with no exit (trapped exit) are full targets: d ≈ 0.95, f(d) ≈ 1.42 (powerless agent maximum extraction). Landlords and employers with full arbitrage (arbitrage exit) are full beneficiaries: d ≈ 0.05, f(d) ≈ -0.12 (institutional beneficiary). Constrained workers with high-cost exit (constrained exit) occupy middle ground: d ≈ 0.68, f(d) ≈ 1.00 (moderate power, facing extraction but with potential exit). The chi formula χ = ε × f(d) × σ(S) produces: for trapped workers at national scope, χ ≈ 0.68 × 1.42 × 1.0 = 0.96 (near-total effective extraction); for landlords, χ ≈ 0.68 × (-0.12) × 1.0 ≈ -0.08 (negative extraction — pure benefit). The directionality derivation explains why the same structural constraint produces opposite experienced extractiveness across agent positions.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The constraint is correctly classified as snare from the primary target (trapped worker) perspective. The mandatrophy is resolved by recognizing that snare classification does not require all perspectives to see extraction as primary. The snare gate requires (a) victims to exist (yes — precarious workers), (b) base extractiveness ≥ 0.46 (yes — 0.68), (c) suppression ≥ 0.60 (yes — 0.72), and (d) effective extraction χ ≥ 0.66 for trapped agents (yes — 0.96). The constraint simultaneously appears as rope from beneficiary perspectives (landlords, multinationals) — this is not a contradiction but a diagnostic feature: snares are experienced very differently by target and beneficiary. The high theater ratio (0.58) and rising trajectory suggest incipient pitonization — as policy response becomes performative while mechanisms persist, the constraint risks degrading into a maintained ritual rather than functional extraction. The false summit risk (mountain perspective) is real: 'Dublin is expensive because it's successful' naturalizes policy choices as natural laws.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    housing_supply_elasticity_threshold,
    'What rate of housing supply growth would deactivate the desperation extraction mechanism?',
    'Cross-country analysis: compare Dublin (restricted supply) to Vienna (regulated affordable stock), Singapore (public housing dominance), and US sunbelt (deregulated supply). Measure desperation-proxy metrics (rent-to-income, precarity indicators) against supply elasticity.',
    'If threshold < 5% annual growth: desperation mechanism is structural (entrenchment). If threshold > 15%: desperation is policy-dependent (reversible). If 5-15%: mixed institutional and market factors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(housing_supply_elasticity_threshold, empirical, 'Housing supply growth rate at which desperation extraction mechanism breaks').

omega_variable(
    multinational_concentration_counterfactual,
    'Would multinational wage levels remain competitive without Dublin geographic concentration and desperation-driven cost arbitrage?',
    'Analysis of wage differentials between Dublin multinationals and EU peer firms in cities with lower desperation (Lisbon, Prague, Berlin). Control for productivity, skill level, and role scope.',
    'If wages require desperation context: extraction is fundamental to business model sustainability. If wages are competitive independently: desperation accelerates profits but is not essential mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multinational_concentration_counterfactual, empirical, 'Whether multinationals depend on desperation for wage competitiveness').

omega_variable(
    policy_reform_reversibility,
    'Could rapid zoning reform and planning acceleration reverse the desperation mechanism within 10 years?',
    'Pilot analysis of accelerated planning districts (if any emerge); comparison to historical precedents (Tokyo post-1960s regulatory liberalization, South Korea rapid zoning changes). Model supply response to policy shock.',
    'If reversible within 10 years: piton classification is accurate — institutional inertia maintains constraint despite policy levers existing. If reversal requires 20+ years: desperation has locked in structural dependencies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_reform_reversibility, empirical, 'Timeline for desperation reversal through policy reform').

omega_variable(
    identity_locked_vs_constrained_worker_distinction,
    'For Dublin workers, is exit blockage primarily material (constrained) or identity-based (identity_locked to Dublin as career/social location)?',
    'Comparative exit analysis: measure actual relocation rates post-layoff vs post-promotion. Survey ex-Dublin workers on reasons for departure. Identify whether non-relocated workers cite material barriers vs identity/relationship attachment.',
    'If primarily material/constrained: housing reform directly addresses mechanism. If significantly identity_locked: workers carry Dublin lock even if material barriers removed — psychological entrenchment complicates policy solutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_vs_constrained_worker_distinction, empirical, 'Whether worker exit blockage is material or identity-based').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dublin_economic_desperation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dublin_tr_t0, dublin_economic_desperation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(dublin_tr_t5, dublin_economic_desperation, theater_ratio, 5, 0.5).
narrative_ontology:measurement(dublin_tr_t10, dublin_economic_desperation, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(dublin_be_t0, dublin_economic_desperation, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(dublin_be_t5, dublin_economic_desperation, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(dublin_be_t10, dublin_economic_desperation, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dublin_economic_desperation, resource_allocation).
narrative_ontology:affects_constraint(dublin_economic_desperation, irish_housing_supply_restriction).
narrative_ontology:affects_constraint(dublin_economic_desperation, multinational_geographic_clustering).
narrative_ontology:affects_constraint(dublin_economic_desperation, precarious_labor_market_dynamics).

% DUAL FORMULATION NOTE:
% Dublin economic desperation is downstream of housing supply policy (zoning, planning) and upstream of precarious labor market dynamics. The housing supply constraint (piton: performative policy) feeds desperation extraction (snare: trapped workers), which in turn enables wage suppression in service and tech sectors. Decomposition: separate stories for housing policy as piton, labor extraction as snare, and multinational wage arbitrage as tangled rope with geographic locking.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dublin_economic_desperation, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
