% ============================================================================
% CONSTRAINT STORY: sotu_1973_nixon_revenue_sharing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1973_nixon_revenue_sharing, []).

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
 *   constraint_id: sotu_1973_nixon_revenue_sharing
 *   human_readable: Nixon Revenue Sharing: Federal-State Fiscal Authority Redistribution (1973)
 *   domain: governance/federalism
 *
 * SUMMARY:
 *   The 1973 Nixon Revenue Sharing proposal represents a structural shift in
 *   the federal-state fiscal relationship, positioning decentralization of
 *   spending authority as a mechanism to enhance local autonomy while
 *   reducing federal policy leverage. The proposal coordinates legitimate
 *   local adaptation to regional needs while simultaneously extracting
 *   federal enforcement capacity in civil rights, environmental protection,
 *   and social safety nets. The constraint exhibits all six DR types from
 *   different observation points, with the critical diagnostic gap between
 *   beneficiaries (state leaders, regional autonomy advocates) and victims
 *   (powerless communities in low-tax regions, federal enforcement
 *   institutions). The theater ratio rises over the interval as revenue
 *   sharing becomes institutionalized ritual: state governments perform
 *   fiscal autonomy while federal power persists through conditional grants
 *   on remaining funds (Medicare, Medicaid, highway funding). The
 *   extractiveness increases moderately as states adapted to the new fiscal
 *   regime, with some using federal revenue-shared funds to reduce local
 *   taxes rather than maintain service levels.
 *
 * KEY AGENTS:
 *   - State and Local Government Leaders: Primary beneficiaries (institutional/arbitrage) — gain fiscal discretion, political credit, and reduced federal conditionality. Arbitrage exit allows adaptation to changing priorities.
 *   - Poor Communities in Low-Tax States: Primary victims (powerless/trapped) — trapped within regional fiscal systems with limited revenue bases. Federal safety net reduced. No geographic mobility without significant cost.
 *   - Civil Rights and Environmental Enforcement Organizations: Secondary victims (moderate/constrained) — federal agencies lose conditionality leverage as states receive unconditional revenue shares. Constrained by reduced enforcement funding and political support for federal mandates.
 *   - Federalism Reform Coalition: Organized advocates (organized/constrained) — view revenue sharing as transitional mechanism toward full decentralization. Constrained by dependence on federal political support to implement further devolution.
 *   - Federal Centralization Apparatus: Institutional actor (institutional/arbitrage) — maintains formal compliance with revenue sharing while using non-revenue-sharing mechanisms (conditional grants) to preserve federal leverage. Theater high as apparatus performs devolution while sustaining control.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent power redistribution as an immutable feature of governing large territories.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1973_nixon_revenue_sharing, 0.38).
domain_priors:suppression_score(sotu_1973_nixon_revenue_sharing, 0.42).
domain_priors:theater_ratio(sotu_1973_nixon_revenue_sharing, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1973_nixon_revenue_sharing, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_1973_nixon_revenue_sharing, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(sotu_1973_nixon_revenue_sharing, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1973_nixon_revenue_sharing, tangled_rope).
narrative_ontology:human_readable(sotu_1973_nixon_revenue_sharing, "Nixon Revenue Sharing: Federal-State Fiscal Authority Redistribution (1973)").
narrative_ontology:topic_domain(sotu_1973_nixon_revenue_sharing, "governance/federalism").

domain_priors:requires_active_enforcement(sotu_1973_nixon_revenue_sharing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1973_nixon_revenue_sharing, state_local_governments).
narrative_ontology:constraint_beneficiary(sotu_1973_nixon_revenue_sharing, regional_autonomy_advocates).
narrative_ontology:constraint_victim(sotu_1973_nixon_revenue_sharing, federal_policy_uniformity).
narrative_ontology:constraint_victim(sotu_1973_nixon_revenue_sharing, marginalized_communities_in_low_tax_regions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POOR COMMUNITIES IN LOW-TAX STATES (SNARE) — Trapped within regional fiscal systems that lack revenue to provide services. Revenue sharing shifts federal funds to state control, but those funds flow according to state spending priorities, which may not prioritize poor communities. No exit: geographic mobility is costly; federal safety net is reduced. Extraction maximized by both suppression (immobility) and the de facto redirection of federal resources away from needs-based allocation.
constraint_indexing:constraint_classification(sotu_1973_nixon_revenue_sharing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL RIGHTS ENFORCEMENT ORGANIZATIONS (TANGLED ROPE) — Constrained by reliance on federal enforcement leverage (EPA, DOJ, HHS compliance monitoring). Revenue sharing reduces federal conditionality: states receive funds with fewer strings attached. These organizations benefit from coordination aspects (clearer state-local resource allocation) but bear extraction as federal enforcement capacity declines. Can exit through litigation or advocacy, but at high institutional cost.
constraint_indexing:constraint_classification(sotu_1973_nixon_revenue_sharing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATE AND LOCAL GOVERNMENT LEADERS (ROPE) — Primary beneficiaries with arbitrage options. Revenue sharing expands fiscal discretion, eliminates federal mandates, and allows tailoring of spending to local priorities. Experiences constraint as coordination mechanism: funds flow predictably, decision-making is localized, political credit flows to local actors. Net positive — arbitrage exit means they can adapt spending as needs change.
constraint_indexing:constraint_classification(sotu_1973_nixon_revenue_sharing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: FEDERALISM REFORM COALITION (SCAFFOLD) — Organized advocates for decentralized governance view revenue sharing as temporary mechanism to rebalance federal-state power. Sunset clause implicit: as states prove capable of independent fiscal management, further devolution becomes possible. Theater ratio moderate-high because the 'new federalism' framing performs decentralization while maintaining federal-state fiscal interdependence. Constraint seen as transitional, not permanent.
constraint_indexing:constraint_classification(sotu_1973_nixon_revenue_sharing, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL CENTRALIZATION APPARATUS (PITON) — Federal bureaucratic institutions maintain formal commitment to revenue sharing compliance, but the actual function (federal control over state spending via conditional grants) persists through non-revenue-sharing mechanisms (Medicare, Medicaid, conditional grants on remaining budget). Theater high: revenue sharing appears to decentralize while the federal system continues steering state behavior through multiple channels. The constraint's formal function has atrophied; its enforcement persists through institutional inertia.
constraint_indexing:constraint_classification(sotu_1973_nixon_revenue_sharing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL FEDERALISM (MOUNTAIN) — Civilizational view: federalism is a structural necessity for governing large territories with diverse preferences. Revenue sharing becomes a natural law — any large polity must distribute fiscal authority across levels or collapse under central overhead. However, structural data contradicts this: identifiable beneficiaries (state leaders, regional elites) and victims (powerless communities, federal enforcement capacity) exist. This perspective risks naturalizing a contingent power redistribution as an immutable feature of governance.
constraint_indexing:constraint_classification(sotu_1973_nixon_revenue_sharing, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1973_nixon_revenue_sharing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1973_nixon_revenue_sharing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1973_nixon_revenue_sharing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1973_nixon_revenue_sharing, TR),
    TR >= 0.70.

:- end_tests(sotu_1973_nixon_revenue_sharing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Revenue sharing benefits state leaders and regional elites through expanded fiscal discretion (coordination function: ~0.15 extraction inherent to allocation). But the constraint also extracts from powerless communities and federal enforcement capacity through the mechanism of fiscal devolvement without corresponding reduction in inequality (extraction component: ~0.23). The net reflects genuine coordination mixed with genuine extraction. Suppression (0.42): Moderate. Barriers to exit include geographic immobility for poor households, federal enforcement dependence for civil rights organizations, and political path-dependence of state fiscal systems. Not total suppression — some migration is possible, some federal enforcement persists, but significant barriers exist. Theater ratio (0.55): Moderate-high. Revenue sharing performs federalism reform while maintaining de facto federal control through conditional grants on non-revenue-shared funds. The performance increases over the interval as the apparatus adapts.
 *
 * PERSPECTIVAL GAP:
 *   The critical gap separates beneficiaries from victims. State leaders perceive coordination (Rope) — revenue sharing solves the legitimate problem of tailoring spending to local needs. Poor communities perceive extraction (Snare) — devolution of authority without devolution of equalization mechanisms leaves them dependent on low-tax-base state budgets. Civil rights organizations perceive mixed coordination-extraction (Tangled Rope) — the system enables some state innovation in rights protection but reduces federal enforcement leverage. The analytical observer risks perceiving natural federalism (Mountain) — decentralization appears as an inevitable feature of governing large polities — but structural data reveals this as false summit: specific institutional actors benefit; specific vulnerable populations bear costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) for each perspective are derived from the agent's structural relationship to the extraction flow. State leaders have low d (beneficiaries with arbitrage exit options, ~0.15): they experience negative effective extraction, receiving fiscal benefits. Poor communities have high d (victims with trapped exit options, ~0.95): they experience maximum extraction, bearing costs of reduced federal redistribution without exit. Civil rights organizations have moderate d (victims with constrained exit, ~0.70): they lose federal leverage but can continue advocating and litigating. The federal apparatus has low d (beneficiary with arbitrage exit, ~0.20): it maintains leverage through alternative mechanisms. The piton classification emerges from theater ratio (0.55, rising) indicating performative compliance rather than functional decentralization.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that revenue sharing is genuinely Tangled Rope: it coordinates local adaptation while extracting federal enforcement capacity. The false-summit risk is the natural-law reading (Federalism is inherent; decentralization is inevitable). The actual structure is contingent institutional choice with distributional consequences. The piton perspective (federal apparatus) captures an important secondary dynamic: revenue sharing performs decentralization while the apparatus continues steering state behavior through remaining conditional grants. The scaffold perspective reveals that the constraint is not necessarily permanent — alternative mechanisms (direct federal spending, national minimum standards, federal enforcement without fiscal conditionality) could restructure the relationship. The snare perspective (poor communities) reveals the extraction mechanism: fiscal devolution without equalization is a way of reducing federal redistribution while claiming to enhance local autonomy. This is not mandatrophy collapse — each perspective is analytically sound — but rather a clarification that the constraint's classification depends critically on whether the observation includes the distributional consequences across income levels and federal functions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    federal_enforcement_substitution,
    'Do non-revenue-sharing federal mechanisms (Medicare, Medicaid, conditional grants) substitute for revenue-sharing federal control, or do they represent genuine decentralization?',
    'Analysis of federal leverage pathways: track federal leverage in healthcare, education, and environmental domains pre- vs post-revenue-sharing. Measure extent to which states must comply with federal standards to access remaining conditional funds.',
    'If substitution: revenue sharing is theater masking continued federal control. If genuine decentralization: extraction is lower than measured. Classification shifts from Tangled Rope toward Rope for federal-state relationship.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federal_enforcement_substitution, empirical, 'Whether federal non-revenue-sharing mechanisms substitute for devolved fiscal control').

omega_variable(
    state_fiscal_inequality_mechanism,
    'Do states with lower tax bases use revenue-shared federal funds to supplement essential services, or do they divert funds to reduce local tax burdens on wealthy taxpayers?',
    'State-level budget analysis comparing revenue-shared fund allocation: per-capita spending on education, healthcare, social services by state income quartile. Time-series analysis of state tax policy before/after revenue sharing.',
    'If diverted to tax reduction: extraction on poor communities increases. If used for services: extraction is lower. This determines whether victims classification is accurate or whether some states use revenue sharing for regressive redistribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_fiscal_inequality_mechanism, empirical, 'State allocation of revenue-shared federal funds: services vs tax relief').

omega_variable(
    cross_state_mobility_elasticity,
    'Does revenue sharing increase or decrease cross-state migration by poor households seeking services, and does that migration pressure affect state spending decisions?',
    'Migration flow analysis: interstate mobility patterns for low-income households pre- vs post-revenue-sharing. Regression analysis of state spending generosity vs observed migration flows. State policy response to migration (e.g., residency requirements, benefit restrictions).',
    'High migration elasticity: states reduce welfare spending to avoid attracting poor households (race-to-the-bottom). Low elasticity: revenue sharing enables service differentiation without competitive pressure. High elasticity supports Snare classification; low elasticity supports Rope or Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_state_mobility_elasticity, empirical, 'Whether revenue sharing triggers competitive reduction of social spending').

omega_variable(
    federal_enforcement_timeline,
    'How long after revenue sharing did federal enforcement leverage in civil rights, environmental protection, and labor standards measurably decline?',
    'Timeline analysis: EPA enforcement actions, DOJ civil rights litigation, OSHA enforcement rates in 1973-1978 vs 1978-1983. Correlation with revenue sharing implementation and state budget constraints.',
    'If decline occurs immediately: revenue sharing directly reduces federal capacity. If decline is delayed or absent: federal enforcement persists through non-fiscal mechanisms. Affects classification of victim group (federal enforcement orgs) impact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federal_enforcement_timeline, empirical, 'Temporal lag between revenue sharing and federal enforcement decline').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1973_nixon_revenue_sharing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_tr_t0, sotu_1973_nixon_revenue_sharing, theater_ratio, 0, 0.4).
narrative_ontology:measurement(sotu_tr_t3, sotu_1973_nixon_revenue_sharing, theater_ratio, 3, 0.48).
narrative_ontology:measurement(sotu_tr_t6, sotu_1973_nixon_revenue_sharing, theater_ratio, 6, 0.55).
narrative_ontology:measurement(sotu_tr_t9, sotu_1973_nixon_revenue_sharing, theater_ratio, 9, 0.6).

% Extraction over time
narrative_ontology:measurement(sotu_be_t0, sotu_1973_nixon_revenue_sharing, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(sotu_be_t3, sotu_1973_nixon_revenue_sharing, base_extractiveness, 3, 0.3).
narrative_ontology:measurement(sotu_be_t6, sotu_1973_nixon_revenue_sharing, base_extractiveness, 6, 0.38).
narrative_ontology:measurement(sotu_be_t9, sotu_1973_nixon_revenue_sharing, base_extractiveness, 9, 0.36).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1973_nixon_revenue_sharing, resource_allocation).
narrative_ontology:affects_constraint(sotu_1973_nixon_revenue_sharing, federal_conditional_grant_system).
narrative_ontology:affects_constraint(sotu_1973_nixon_revenue_sharing, medicaid_state_matching_requirement).
narrative_ontology:affects_constraint(sotu_1973_nixon_revenue_sharing, interstate_welfare_competition).

% DUAL FORMULATION NOTE:
% Revenue sharing operates upstream in the federal-state fiscal relationship; its effects propagate through conditional grant systems, Medicaid state matching, and interstate fiscal competition. Each downstream constraint has its own ε value reflecting observable-specific measurement. The family is linked through the decentralization mechanism: revenue sharing initiates the shift in fiscal authority that downstream constraints operationalize.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1973_nixon_revenue_sharing, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
