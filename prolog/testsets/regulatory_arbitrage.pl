% ============================================================================
% CONSTRAINT STORY: regulatory_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_arbitrage, []).

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
 *   constraint_id: regulatory_arbitrage
 *   human_readable: Regulatory Arbitrage: Exploitation of Jurisdictional Regulatory Gaps
 *   domain: economic/political/governance
 *
 * SUMMARY:
 *   Regulatory arbitrage exploits jurisdictional heterogeneity in labor,
 *   environmental, and tax regulations to extract value from immobile workers
 *   and high-regulation jurisdictions while concentrating benefits in mobile
 *   capital and low-regulation zones. The constraint manifests as a tangled
 *   coordination-extraction hybrid: it genuinely solves the coordination
 *   problem of matching capital to locations with different regulatory costs,
 *   but it systematically extracts from those trapped in high-regulation
 *   jurisdictions and unable to relocate. The empirical trajectory shows
 *   increasing extractiveness as globalization has reduced friction to
 *   capital mobility; theater ratio reflects that regulatory frameworks are
 *   presented as fixed constraints ('you cannot regulate capital, it will
 *   flee') while remaining contingent policy choices. The constraint
 *   exemplifies how a coordination mechanism for capital becomes an
 *   extraction mechanism for labor and the environment.
 *
 * KEY AGENTS:
 *   - Immobile workers: Primary victims (powerless/trapped) — cannot exit high-regulation jurisdictions; bear wage depression and job loss from capital flight
 *   - Environmental commons: Primary victims (powerless/trapped) — cannot exit; bears concentrated pollution and externalized costs from low-regulation production zones
 *   - High-regulation jurisdictions: Secondary victims (moderate/constrained) — face genuine coordination problem but also suffer tax base erosion and capital flight; some agency through multilateral coordination
 *   - Multinational firms: Primary beneficiaries (institutional/arbitrage) — can instantly relocate between jurisdictions; experience arbitrage as pure coordination benefit
 *   - Low-regulation jurisdictions: Secondary beneficiaries (institutional/arbitrage) — benefit from capital inflow, employment, and tax revenue; also experience some secondary costs (environmental degradation, labor exploitation, dependency on extractive industries)
 *   - Organized labor/NGO coalition: Organized victims (organized/constrained) — have organizational capacity to negotiate and advocate but face structural power asymmetry from capital mobility
 *   - Global minimum tax regime: Emerging scaffold structure (organized/constrained) — OECD, EU, CPTPP represent attempts to establish regulatory floors; partial sunset logic as coordination mechanisms mature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_arbitrage, 0.58).
domain_priors:suppression_score(regulatory_arbitrage, 0.62).
domain_priors:theater_ratio(regulatory_arbitrage, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_arbitrage, extractiveness, 0.58).
narrative_ontology:constraint_metric(regulatory_arbitrage, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(regulatory_arbitrage, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_arbitrage, tangled_rope).
narrative_ontology:human_readable(regulatory_arbitrage, "Regulatory Arbitrage: Exploitation of Jurisdictional Regulatory Gaps").
narrative_ontology:topic_domain(regulatory_arbitrage, "economic/political/governance").

domain_priors:requires_active_enforcement(regulatory_arbitrage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_arbitrage, mobile_capital).
narrative_ontology:constraint_beneficiary(regulatory_arbitrage, multinational_firms).
narrative_ontology:constraint_beneficiary(regulatory_arbitrage, low_regulation_jurisdictions).
narrative_ontology:constraint_victim(regulatory_arbitrage, high_regulation_jurisdictions).
narrative_ontology:constraint_victim(regulatory_arbitrage, immobile_workers).
narrative_ontology:constraint_victim(regulatory_arbitrage, environmental_commons).
narrative_ontology:constraint_victim(regulatory_arbitrage, tax_base).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IMMOBILE WORKER (SNARE) — Cannot exit the jurisdiction where their labor is located. Bears full cost of regulatory arbitrage as capital flees to lower-regulation zones, depressing wages, eliminating job categories, and reducing public revenue for local services. No coordination benefit. Maximal extraction.
constraint_indexing:constraint_classification(regulatory_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ENVIRONMENTAL COMMONS (SNARE) — Regulatory arbitrage concentrates polluting production in low-regulation jurisdictions. The commons bears extraction across jurisdictions but has no exit option and no organizational capacity. Pure extraction with intergenerational suppression.
constraint_indexing:constraint_classification(regulatory_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: HIGH-REGULATION JURISDICTION (TANGLED ROPE) — Faces genuine coordination problem: how to maintain worker protections and environmental standards while competing for mobile capital. But the constraint also extracts through tax base erosion and capital exit. High suppression of alternatives (race-to-the-bottom pressure) but some agency remains through multi-jurisdictional coordination attempts (CPTPP, EU directives, global minimum tax). Mixed coordination and extraction.
constraint_indexing:constraint_classification(regulatory_arbitrage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MULTINATIONAL FIRM (ROPE) — Experiences regulatory arbitrage as pure coordination: optimizing production location based on regulatory costs is a legitimate business coordination problem. The firm can exit any jurisdiction instantly (arbitrage exit). Net beneficiary. Constraint appears to solve a real coordination need.
constraint_indexing:constraint_classification(regulatory_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LOW-REGULATION JURISDICTION (ROPE) — Regulatory arbitrage solves the coordination problem of attracting mobile capital and generating employment from foreign investment. The jurisdiction benefits through tax revenue, employment, and infrastructure investment. From this perspective, the constraint is pure coordination with net positive sum.
constraint_indexing:constraint_classification(regulatory_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ORGANIZED LABOR/NGO COALITION (TANGLED ROPE) — Organized actors (unions, environmental NGOs, development organizations) see both coordination and extraction. The constraint does coordinate capital and labor across jurisdictions, but with asymmetric extraction toward capital. These actors have organizational capacity (constrained exit) and can negotiate, litigate, and advocate for regulatory floors, but face structural power asymmetry and race-to-the-bottom suppression. Can organize countervailing coalitions but cannot fully exit the system.
constraint_indexing:constraint_classification(regulatory_arbitrage, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: GLOBAL MINIMUM TAX REGIME (SCAFFOLD) — Recent agreements (OECD global minimum tax, CPTPP, EU directives) represent organized attempts to establish regulatory floors that sunset the arbitrage mechanism. These are temporary scaffolds with sunset logic: as coordination mechanisms mature (international tax information sharing, harmonized labor standards, environmental protocols), the arbitrage advantage diminishes. Theater remains moderate — implementation gaps and continued loopholes exist — but structural exit from arbitrage is occurring.
constraint_indexing:constraint_classification(regulatory_arbitrage, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: SOVEREIGN STATE SYSTEM (PITON) — The constraint emerges from the foundational assumption that states have exclusive regulatory authority within jurisdictions. This is largely a performative institutional arrangement at the civilizational scale — global capital flows, labor mobility, and supply chains have rendered exclusive state regulatory control increasingly theatrical. The constraint persists through institutional inertia (states maintain formal sovereignty claims) despite low functional capacity to prevent arbitrage. Piton classification driven by theater ratio and institutional degradation.
constraint_indexing:constraint_classification(regulatory_arbitrage, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/analytical perspective, regulatory arbitrage may appear as an immutable consequence of capital mobility and jurisdictional heterogeneity: given free capital movement and different regulatory costs, some actors will exploit the gap. This is a law-like feature of the system. However, the structural data contradicts the mountain classification — regulatory frameworks are designed choices, capital mobility is policy-contingent, and arbitrage is maintained by suppression of regulatory coordination. The mountain classification is a false summit revealing how naturalizing economic inevitability masks policy contingency.
constraint_indexing:constraint_classification(regulatory_arbitrage, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_arbitrage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_arbitrage, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_arbitrage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regulatory_arbitrage, TR),
    TR >= 0.70.

:- end_tests(regulatory_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts value from immobile workers and high-regulation jurisdictions through wage depression, job loss, tax base erosion, and regulatory degradation. But the extraction is not total — low-regulation jurisdictions gain employment and development capital; some multinational activity generates genuine coordination benefits. The value reflects that arbitrage creates real flows but at asymmetric cost. Suppression (0.62): Moderate-high. Barriers to escaping arbitrage include: capital mobility cannot be easily restricted without breaking the broader financial system; immobile workers face geographic and economic barriers to relocation; low-regulation jurisdictions face dependency on attracting capital and cannot unilaterally raise regulations without losing investment; high-regulation jurisdictions face collective action problems in enforcing regulatory coordination. These are structural but not absolute — recent global minimum tax agreements show suppression is reducible through coordinated enforcement. Theater ratio (0.58): Moderate-high. Regulatory frameworks are presented as fixed natural constraints ('we have no choice, capital will flee'), but they are policy contingencies. The theater has increased as narratives of inevitability have become more dominant; recent coordination attempts are reducing theater by making policy contingency visible.
 *
 * PERSPECTIVAL GAP:
 *   The immobile worker perceives pure extraction (snare) — they bear all costs with no benefit and no exit. The environmental commons perceives pure extraction across jurisdictions — transnational. The multinational firm perceives pure coordination (rope) — solving the legitimate problem of capital allocation. The low-regulation jurisdiction perceives coordination benefit (rope) — attracting development capital. But the high-regulation jurisdiction perceives the mixed constraint (tangled rope) — genuine coordination need but also extraction. The global minimum tax regime perceives a sunset structure (scaffold) — temporary arbitrage advantage being replaced by coordinated regulatory floors. The sovereign state system perspective risks seeing arbitrage as a natural law (mountain) — an inevitable consequence of capital mobility — but the structural data reveals this as naturalization: regulatory frameworks are designed choices, not laws of nature. The perspectival gap widens across power asymmetries: the more powerless the agent, the more extraction appears inevitable and unchangeable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position relative to the extraction flow. Immobile workers are trapped victims with no exit: d ≈ 0.95 → high f(d) → high experienced extraction. Multinational firms are beneficiaries with arbitrage exit: d ≈ 0.05 → negative f(d) → negative experienced extraction (they perceive coordination benefit). High-regulation jurisdictions are constrained victims with some agency: d ≈ 0.60 → moderate f(d) → moderate experienced extraction. Low-regulation jurisdictions are beneficiaries with constrained exit (dependent on capital): d ≈ 0.35 → low f(d) → low extraction. Organized coalition members have more agency than atomized workers but still face structural power asymmetry: d ≈ 0.65 → elevated f(d) → moderate-to-high extraction. The global minimum tax regime actors have arbitrage exit (can shift regulatory strategy): d ≈ 0.20 → low f(d) → low extraction, enabling the scaffold classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that regulatory arbitrage is genuinely a coordination mechanism (capital allocation) that has become an extraction mechanism (labor/environmental cost externalization) through asymmetric suppression. No single type is 'correct' — the presheaf over the observation site reveals: coordination for capital (rope), extraction for labor (snare), mixed hybrid for jurisdictions (tangled rope), temporary structure under sunset (scaffold), and naturalized performance (piton/mountain). The constraint does NOT reduce to pure extraction — low-regulation jurisdictions genuinely benefit from capital inflow and development. Nor does it reduce to pure coordination — immobile workers genuinely lose. The tangled rope classification captures the hybrid: real coordination function with asymmetric distribution of costs and benefits. The scaffold and piton perspectives reveal that the constraint's future depends on whether regulatory coordination (sunset) or institutional inertia (piton) dominates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capital_mobility_constraint,
    'Is regulatory arbitrage driven by genuine capital mobility or by policy-enabled capital flight?',
    'Counterfactual analysis comparing capital flows under different capital control regimes; historical comparison of pre-globalization periods when capital mobility was restricted',
    'If genuine mobility: regulatory arbitrage is a coordination problem requiring minimum-floor agreements. If policy-enabled: arbitrage is a choice to maintain asymmetric extraction; closing it requires political will, not economic restructuring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_mobility_constraint, empirical, 'Whether capital mobility is structural or policy-contingent').

omega_variable(
    race_to_bottom_inevitability,
    'Is the race-to-the-bottom inevitable, or can coordinated regulatory floors prevent arbitrage without destroying capital formation?',
    'Analysis of jurisdictions that implemented coordinated regulatory floors (EU, Nordic model) and maintained capital investment; comparison with low-regulation jurisdictions that competed on race-to-bottom and long-term outcomes',
    'If inevitable: suppression of regulatory coordination is structural (high omega); arbitrage is inescapable. If preventable: suppression is contingent on political coordination failure (medium omega); regulatory floors are achievable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(race_to_bottom_inevitability, empirical, 'Whether race-to-the-bottom is inevitable or preventable through coordination').

omega_variable(
    immobile_worker_organization,
    'Can immobile workers organize collective bargaining or political coalitions sufficient to enforce regulatory floors, or is their organizational capacity structurally suppressed?',
    'Historical analysis of labor organization under arbitrage conditions; comparative study of union density and wage outcomes in high-mobility vs low-mobility labor sectors',
    'If organizable: suppression is moderate and reducible through collective action. If structurally suppressed: suppression is high and institutional inertia prevents worker agency. Affects classification of victim perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immobile_worker_organization, empirical, 'Organizational capacity of immobile workers under arbitrage conditions').

omega_variable(
    tax_revenue_substitution,
    'Can high-regulation jurisdictions replace lost tax revenue through other mechanisms (wealth taxes, land taxes, financial transaction taxes) or is tax revenue loss from arbitrage irreversible?',
    'Analysis of alternative tax mechanisms in jurisdictions that successfully retained mobile capital; comparison of long-term fiscal outcomes between jurisdictions that competed on race-to-bottom vs those that coordinated regulatory floors',
    'If substitutable: revenue loss is contingent and reducible; arbitrage is less entrenched. If irreversible: revenue loss is structural; high-regulation jurisdictions face permanent fiscal weakness. Affects whether the tangled_rope classification for high-regulation jurisdictions is accurate or understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tax_revenue_substitution, empirical, 'Substitutability of lost tax revenue in high-regulation jurisdictions').

omega_variable(
    environmental_cost_visibility,
    'Do environmental costs of concentrated pollution in low-regulation zones become visible and actionable as transnational externalities, or do they remain suppressed through jurisdictional compartmentalization?',
    'Analysis of transnational pollution incidents (acid rain, ocean plastic, carbon emissions) and political response; tracking of environmental justice movements in low-regulation jurisdictions',
    'If visible and actionable: environmental commons can organize political response; snare classification softens. If suppressed: environmental costs remain externalized indefinitely; snare classification hardens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(environmental_cost_visibility, empirical, 'Visibility and actionability of environmental externalities across jurisdictions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_arbitrage, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regarbx_tr_t0, regulatory_arbitrage, theater_ratio, 0, 0.48).
narrative_ontology:measurement(regarbx_tr_t10, regulatory_arbitrage, theater_ratio, 10, 0.54).
narrative_ontology:measurement(regarbx_tr_t20, regulatory_arbitrage, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(regarbx_be_t0, regulatory_arbitrage, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(regarbx_be_t10, regulatory_arbitrage, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(regarbx_be_t20, regulatory_arbitrage, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_arbitrage, resource_allocation).
narrative_ontology:affects_constraint(regulatory_arbitrage, global_minimum_tax_enforcement).
narrative_ontology:affects_constraint(regulatory_arbitrage, labor_mobility_restrictions).
narrative_ontology:affects_constraint(regulatory_arbitrage, environmental_externality_pricing).
narrative_ontology:affects_constraint(regulatory_arbitrage, jurisdictional_tax_competition).

% DUAL FORMULATION NOTE:
% Regulatory arbitrage is part of a constraint family involving capital mobility, tax competition, and labor market dynamics. The upstream constraint is the capacity and willingness of states to maintain regulatory heterogeneity; the downstream constraints are the specific effects on labor markets, environmental quality, and tax bases. These stories should be decomposed along domain lines: arbitrage_capital_allocation (ε ≈ 0.25, rope), arbitrage_labor_extraction (ε ≈ 0.72, snare), arbitrage_environmental_cost (ε ≈ 0.65, snare), arbitrage_fiscal_competition (ε ≈ 0.55, tangled_rope).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regulatory_arbitrage, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
