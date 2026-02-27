% ============================================================================
% CONSTRAINT STORY: eu_affordable_housing_initiative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_affordable_housing_initiative, []).

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
 *   constraint_id: eu_affordable_housing_initiative
 *   human_readable: EU Affordable Housing Initiative (2025)
 *   domain: economic/political
 *
 * SUMMARY:
 *   The EU Affordable Housing Initiative (2025) represents an attempt to
 *   coordinate housing policy across 27 member states with vastly different
 *   housing markets, regulatory regimes, and fiscal capacity. Nominally, the
 *   initiative provides EU co-financing and harmonized standards to promote
 *   affordable housing production. Structurally, it creates a mixed
 *   coordination-extraction hybrid: member states and developers benefit from
 *   EU funding and streamlined permitting (coordination), but the funding
 *   comes with binding commitments and bureaucratic compliance that extract
 *   member state autonomy (extraction). Low-income renters, the nominal
 *   beneficiaries, often see rents stagnate or rise as construction redirects
 *   toward incentivized units while true subsidy is captured through
 *   developer profit, land appreciation, and regulatory capture. The
 *   initiative exhibits increasing theater over its first 6 years: targets
 *   are nominally met by redefining 'affordable' to include units at 70-80%
 *   of median rent, while genuine subsidy to the poorest households
 *   stagnates. Member states face pressure to demonstrate housing progress
 *   (political theater) while budgets remain constrained, leading to
 *   statistical redefinition rather than substantive access improvement.
 *
 * KEY AGENTS:
 *   - Low-Income Renters: Primary victims (powerless/trapped) — nominally targeted but often excluded as costs rise and units are captured by higher-income groups
 *   - Real Estate Developers: Primary beneficiaries (institutional/arbitrage) — capture subsidies and enjoy streamlined EU permitting
 *   - Member State Governments: Mixed (institutional/constrained) — benefit from EU co-financing but face enforcement costs and autonomy constraints
 *   - National Housing Authorities: Secondary coordinator (organized/constrained) — coordinate legitimate housing provision but constrained by EU mandates and budgets
 *   - EU Commission: Institutional maintainer (institutional/arbitrage) — initiative persists through reporting cycles and grant allocation despite modest empirical impact
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing housing scarcity as immutable when policy choices drive outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_affordable_housing_initiative, 0.48).
domain_priors:suppression_score(eu_affordable_housing_initiative, 0.52).
domain_priors:theater_ratio(eu_affordable_housing_initiative, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_affordable_housing_initiative, extractiveness, 0.48).
narrative_ontology:constraint_metric(eu_affordable_housing_initiative, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(eu_affordable_housing_initiative, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_affordable_housing_initiative, tangled_rope).
narrative_ontology:human_readable(eu_affordable_housing_initiative, "EU Affordable Housing Initiative (2025)").
narrative_ontology:topic_domain(eu_affordable_housing_initiative, "economic/political").

domain_priors:requires_active_enforcement(eu_affordable_housing_initiative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_affordable_housing_initiative, member_state_governments).
narrative_ontology:constraint_beneficiary(eu_affordable_housing_initiative, construction_sector).
narrative_ontology:constraint_victim(eu_affordable_housing_initiative, low_income_renters).
narrative_ontology:constraint_victim(eu_affordable_housing_initiative, housing_accessibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME RENTER (SNARE) — Trapped in regional housing markets with no exit. Nominally targeted by the initiative but bears the extraction cost: rents stagnate or rise as construction redirects toward incentivized 'affordable' units while true subsidy is captured by developers. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.67.
constraint_indexing:constraint_classification(eu_affordable_housing_initiative, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: NATIONAL HOUSING AUTHORITY (TANGLED ROPE) — Constrained by EU requirements and national budget limits, but also coordinating legitimate housing provision. Experiences the initiative as mixed: funding enables new construction (coordination benefit) but enforcement mechanisms extract administrative capacity and limits autonomy in site selection. d≈0.58, f(d)≈0.75, σ=1.0 → χ≈0.36.
constraint_indexing:constraint_classification(eu_affordable_housing_initiative, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REAL ESTATE DEVELOPER (ROPE) — Benefits from EU subsidies and tax incentives for 'affordable' development. Experiences the constraint as coordination: EU standards simplify permitting and reduce regulatory variance across member states, enabling cross-border capital deployment. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary through arbitrage.
constraint_indexing:constraint_classification(eu_affordable_housing_initiative, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MEMBER STATE GOVERNMENT (TANGLED ROPE) — Constrained by EU fiscal rules and political pressure to show housing progress. Benefits from EU co-financing and harmonized standards (coordination function) but also faces extraction via compliance costs and binding commitments that limit domestic policy autonomy. d≈0.48, f(d)≈0.63, σ=1.1 → χ≈0.33.
constraint_indexing:constraint_classification(eu_affordable_housing_initiative, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: EU COMMISSION BUREAUCRACY (PITON) — Initiative persists through institutional momentum and reporting requirements despite modest empirical impact on actual affordability. Theater ratio 0.58 reflects: targets are met via statistical redefinition of 'affordable' rather than substantive housing access improvement. Administrative apparatus maintains itself through grant cycles. d≈0.10, f(d)≈-0.08, σ=1.1 → χ≈-0.04. Nominal beneficiary but actual function degraded.
constraint_indexing:constraint_classification(eu_affordable_housing_initiative, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE MOUNTAIN) — May view housing access constraints as immutable economic laws (supply/demand inelasticity, urban land scarcity). But base properties (ε=0.48, suppression=0.52, theater=0.58) contradict mountain gates. The 'natural scarcity' framing naturalizes policy choices (zoning, subsidy allocation, regulatory capture) as physical limits. Engine detects false summit.
constraint_indexing:constraint_classification(eu_affordable_housing_initiative, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_affordable_housing_initiative_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_affordable_housing_initiative, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_affordable_housing_initiative, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_affordable_housing_initiative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_affordable_housing_initiative, TR),
    TR >= 0.70.

:- end_tests(eu_affordable_housing_initiative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The initiative extracts from low-income renters through opportunity cost: EU-subsidized units reduce market pressure for true affordability, while developers and member states capture the coordination gains. The extraction mechanism is subtle — not direct coercion, but incentive realignment that redirects resources away from those most in need. Suppression (0.52): Moderate. Barriers include: regulatory complexity that favors large developers, fiscal constraints that limit units produced, political pressure to meet targets quickly (driving quality/location tradeoffs), and lack of tenant power to demand affordability. But some mobility exists — EU citizens can (in theory) relocate to member states with better housing outcomes. Theater ratio (0.58): Moderate-high and rising. Early (t=0) theater at 0.38 reflects genuinely coordinated activity (EU-member state negotiation, permitting streamlining). By t=6, theater rises to 0.58 as statistical redefinition of 'affordable' becomes the primary mechanism for meeting targets — administrative apparatus persists (reporting, grant cycles) while actual affordability improvement stagnates.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between the developer/member state view (Rope with coordination benefits) and the low-income renter view (Snare with pure extraction). Developers see the initiative as coordination: EU standards reduce permitting variance, enabling capital efficiency. Member states see mixed coordination-extraction (Tangled Rope): funding enables housing production (coordination) but compliance costs and autonomy loss (extraction) create friction. Low-income renters see snare: the initiative's implementation often excludes them as developers build toward the 'affordable' target (70-80% median rent, not 30% for poorest households) or rely on gentrification-adjacent development that displaces existing residents. The EU Commission sees a piton perspective: the bureaucratic apparatus (reporting, grant cycles, coordination committees) persists because it solves a coordination problem between member states, but the functional impact on actual affordability is degraded — theater now dominates substantive output. The analytical observer risks seeing a mountain (housing scarcity as immutable), but the structural data reveals this as a false summit: policy choices (zoning, subsidy allocation, permit timelines, affordability definitions) drive outcomes, not inherent scarcity.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-income renters: Victim + trapped → d≈0.92, f(d)≈1.40. Maximal extraction; no exit option. Real estate developer: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; can leverage subsidies across markets. National housing authority: Victim + constrained → d≈0.58, f(d)≈0.75. Moderate extraction; constrained by EU mandates but can adapt implementation. Member state government: Both + constrained → d≈0.48, f(d)≈0.63. Mixed; coordination benefit from funding but extraction through autonomy loss. EU Commission: Institutional + arbitrage → d≈0.10, f(d)≈-0.08. Piton classification from theater gate, not high extraction. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is false summit; engine detects naturalization of policy-contingent constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED THROUGH PERSPECTIVAL DECOMPOSITION: The mandatrophy (is this coordination or extraction?) is resolved by showing that the initiative contains genuine coordination (EU funding reducing permitting variance, standards harmonization enabling developer capital efficiency) AND genuine extraction (low-income renters excluded, member state autonomy constrained, subsidies captured). The classification is not ambiguous — it is legitimately Tangled Rope: both coordination function AND asymmetric extraction present simultaneously. The perspectival gap confirms this: developers see primarily coordination (Rope), low-income renters see primarily extraction (Snare), member states see both (Tangled Rope). The rising theater ratio (0.38→0.58) indicates degradation over time: the initiative starts as a genuine coordination mechanism but drifts toward performance (meeting targets via definition inflation) rather than function (actual affordability). This drift is captured by omega variable 'affordable_definition_drift', which, if resolved empirically, would shift the classification toward Piton (ε stable but theater→0.75) or confirm Snare (ε→0.60 if subsidies are captured).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    affordable_definition_drift,
    'What constitutes ''affordable'' in the EU definition, and how does that definition shift over time to meet political targets rather than actual housing access?',
    'Historical comparison of affordability ratios (rent-to-income) for units labeled ''affordable'' vs market units; analysis of how member states adjust definitions to meet targets',
    'If definition inflates: initiative is primarily performative (theater_ratio→0.75, ε→0.55). If definitions remain stable and binding: initiative has real coordination function (ε→0.25, theater→0.35).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(affordable_definition_drift, empirical, 'Definition drift in affordability criteria to meet political targets').

omega_variable(
    subsidy_capture_mechanism,
    'What fraction of EU housing subsidies flow to actual end-user affordability (rent/mortgage reduction) vs to developer profit, land speculation, or administrative overhead?',
    'Fiscal analysis of disbursed funds; tracking of unit cost per affordable housing produced; comparative cost analysis across member states',
    'If <30% reaches end-users: constraint is primarily extractive snare (ε→0.65). If >60% reaches end-users: constraint is primarily coordination rope (ε→0.18).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subsidy_capture_mechanism, empirical, 'Fraction of subsidies reaching actual affordability vs capture').

omega_variable(
    gentrification_acceleration,
    'Does EU-funded ''affordable'' housing development accelerate neighborhood gentrification in adjacent non-targeted areas, displacing the very renters the initiative targets?',
    'Longitudinal rent tracking in neighborhoods with EU housing projects; analysis of displacement rates 2-5 years post-development; comparison to control neighborhoods without EU projects',
    'If yes: initiative is net extractive for low-income renters (ε→0.60, snare confirmed). If no: initiative is net positive coordination (ε→0.20, rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gentrification_acceleration, empirical, 'Whether affordable housing triggers gentrification and secondary displacement').

omega_variable(
    member_state_enforcement_variance,
    'How much do member states vary in actual enforcement of EU affordability requirements, and does variance correlate with political power to negotiate exemptions?',
    'Comparative analysis of enforcement rates; correlation between enforcement variance and member state bargaining power (GDP, EU voting weight); study of exemption requests and approval rates',
    'If high variance: initiative is primarily a Tangled Rope with strong extraction from smaller states (ε stable at 0.48, but χ varies by national power). If low variance: initiative is more uniform rope (ε→0.35).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(member_state_enforcement_variance, empirical, 'Variance in EU enforcement across member states').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_affordable_housing_initiative, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(euah_tr_t0, eu_affordable_housing_initiative, theater_ratio, 0, 0.38).
narrative_ontology:measurement(euah_tr_t3, eu_affordable_housing_initiative, theater_ratio, 3, 0.48).
narrative_ontology:measurement(euah_tr_t6, eu_affordable_housing_initiative, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(euah_be_t0, eu_affordable_housing_initiative, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(euah_be_t3, eu_affordable_housing_initiative, base_extractiveness, 3, 0.4).
narrative_ontology:measurement(euah_be_t6, eu_affordable_housing_initiative, base_extractiveness, 6, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_affordable_housing_initiative, resource_allocation).
narrative_ontology:affects_constraint(eu_affordable_housing_initiative, member_state_fiscal_autonomy).
narrative_ontology:affects_constraint(eu_affordable_housing_initiative, urban_land_speculation).

% DUAL FORMULATION NOTE:
% The EU Affordable Housing Initiative is downstream of member state housing markets and upstream of the fiscal autonomy constraint. The initiative attempts to coordinate housing provision across member states but creates new extraction mechanisms (compliance cost, bureaucratic overhead, definition drift). The urban land speculation constraint is affected by the initiative's incentive structure, which can accelerate gentrification in targeted neighborhoods.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_affordable_housing_initiative, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
