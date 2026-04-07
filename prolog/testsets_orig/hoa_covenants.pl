% ============================================================================
% CONSTRAINT STORY: hoa_covenants
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenants, []).

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
 *   constraint_id: hoa_covenants
 *   human_readable: HOA Architectural Review and Restrictive Covenants
 *   domain: economic/social/legal
 *
 * SUMMARY:
 *   Restrictive covenants in HOA communities encode aesthetic standards as
 *   legally binding constraints on private property modification. The
 *   constraint exhibits both genuine coordination value (collective aesthetic
 *   maintenance that supports property values and market stability) and
 *   asymmetric extraction (suppression of minority aesthetic expression,
 *   gatekeeping against lower-cost housing modifications, enforcement
 *   variance across demographic lines). The structural tension between these
 *   two functions defines the constraint's classification as Tangled Rope: it
 *   solves a real coordination problem (free-rider risk from uncoordinated
 *   exterior modifications) while simultaneously enabling extractive
 *   gatekeeping (enforcement apparatus tilted toward incumbent
 *   wealth-preservers). Over the 40-year interval, the theater ratio has
 *   increased (0.35 → 0.64) as covenants became more performative—original
 *   functional concern (preventing genuine property damage) was largely
 *   satisfied, leaving enforcement to focus on aesthetic gatekeeping. Base
 *   extractiveness has also increased (0.38 → 0.52), reflecting growing
 *   divergence between stated coordination goal (aesthetic harmony) and
 *   actual enforcement function (economic rationing of housing access). The
 *   constraint creates a classic mandatrophy scenario: covenants present
 *   themselves as neutral aesthetic coordination, but structural analysis
 *   reveals identity-correlated enforcement, supply-side housing constraints,
 *   and generational wealth preservation.
 *
 * KEY AGENTS:
 *   - Property value preservationists: Primary beneficiary (institutional/arbitrage) — benefit from market premium maintained by covenant enforcement; can exit to different aesthetic regimes
 *   - Constrained homeowners: Primary victim (powerless/trapped) — cannot modify property without approval; cannot exit without selling; face liens/fines for violations
 *   - Minority property owners: Secondary victim (moderate/constrained) — face asymmetric enforcement of aesthetic restrictions on cultural/religious expression; selling requires finding buyers willing to accept constraints
 *   - HOA board enforcement apparatus: Organized enforcer (organized/constrained) — maintain uniform enforcement but benefit personally from property value preservation; structural conflict of interest
 *   - Low-income modification seekers: Distributed victim (powerless/trapped) — cannot afford energy efficiency/ADU modifications that covenants restrict; trapped in energy-inefficient older housing stock
 *   - Legal enforcement infrastructure: Institutional maintainer (institutional/arbitrage) — enforces deed covenants through courts; benefits from fees and institutional continuity despite origin in racial exclusion regimes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenants, 0.52).
domain_priors:suppression_score(hoa_covenants, 0.68).
domain_priors:theater_ratio(hoa_covenants, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenants, extractiveness, 0.52).
narrative_ontology:constraint_metric(hoa_covenants, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(hoa_covenants, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenants, tangled_rope).
narrative_ontology:human_readable(hoa_covenants, "HOA Architectural Review and Restrictive Covenants").
narrative_ontology:topic_domain(hoa_covenants, "economic/social/legal").

domain_priors:requires_active_enforcement(hoa_covenants).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenants, property_value_preservationists).
narrative_ontology:constraint_beneficiary(hoa_covenants, hoa_board_enforcement_apparatus).
narrative_ontology:constraint_victim(hoa_covenants, property_owners_constrained_aesthetic).
narrative_ontology:constraint_victim(hoa_covenants, minority_homeowners).
narrative_ontology:constraint_victim(hoa_covenants, low_income_modifications).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTRAINED HOMEOWNER (SNARE) — Trapped by recorded deed covenant that runs with the land. Cannot exit without selling (which itself is constrained by buyer pool willing to accept covenants). Bears full cost of aesthetic restriction. No meaningful exit option; suppression is near-total — violation risks liens, fines, forced removal of improvements. Maximum experienced extraction.
constraint_indexing:constraint_classification(hoa_covenants, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MINORITY PROPERTY OWNER (TANGLED ROPE) — Constrained exit (can sell but buyer pool is filtered by covenant restrictions). Benefits from property value stability maintained by covenants (coordination function), but enforcement is asymmetric: restrictions enforced rigorously against cultural/religious expression (restrictive covenants on exterior religious symbols, non-European landscaping aesthetics). Asymmetric extraction based on identity-correlated enforcement variance.
constraint_indexing:constraint_classification(hoa_covenants, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PROPERTY VALUE PRESERVATIONIST (ROPE) — Benefits from coordinated aesthetic standard that creates stable market expectations. Experiences covenants as coordination tool: unified appearance maintains market premium. Low suppression from this agent's perspective — they perceive rules as legitimate coordination, not coercion. Arbitrage exit (can relocate to different aesthetic regime).
constraint_indexing:constraint_classification(hoa_covenants, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: HOA BOARD ENFORCEMENT APPARATUS (SNARE-ADJACENT) — Organized enforcement actor (architectural review committee, legal compliance staff). Experiences pressure to maintain uniform enforcement, but board members are themselves homeowners bound by the same covenants. Conflict of interest: enforcement power serves incumbent board members' interests (property values stay high) while constraining newer/minority residents. Sees the mechanism as maintenance of order, but extraction mechanism runs toward incumbent preservationists.
constraint_indexing:constraint_classification(hoa_covenants, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: LEGAL ENFORCEMENT INFRASTRUCTURE (PITON) — Historical inertia: covenants originated in 1960s-1980s as tools for suburban market segmentation and racial/ethnic exclusion (explicit racial covenants now unenforceable, but aesthetic covenants serve as proxies). The mechanism persists through institutional inertia even as original functions (preventing 'blight', maintaining class/racial homogeneity) are legally neutered. Theater ratio is high: architectural review boards perform legitimacy review but lack technical expertise to evaluate structural safety, energy efficiency, or modern building codes. Degraded from original extraction function to performative gatekeeping.
constraint_indexing:constraint_classification(hoa_covenants, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees both genuine coordination value (market expectations, neighborhood stability, collective action against free-riding on aesthetics) AND asymmetric extraction (enforcement variance across demographic lines, suppression of minority aesthetic expression, gatekeeping that limits housing supply and affordability). The constraint provides both goods (Rope coordination) and extraction (Snare mechanism). Classification settles at hybrid Tangled Rope rather than false symmetry.
constraint_indexing:constraint_classification(hoa_covenants, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenants_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hoa_covenants, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hoa_covenants, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hoa_covenants, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hoa_covenants, TR),
    TR >= 0.70.

:- end_tests(hoa_covenants_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderate-high. The constraint captures genuine extraction value: incumbent homeowners preserve property values while excluding lower-income modifications and constrained residents; HOA boards maintain their enforcement power; legal enforcement infrastructure preserves institutional role. However, the extraction is not total (chi ≥ 0.66 snare-level) because coordination benefits are real—collective aesthetic maintenance does support market expectations and prevent pure free-riding scenarios. Suppression (0.68): High. Violation consequences are severe: liens (debt), fines ($500-$5,000 per violation), forced removal of improvements, architectural review gatekeeping that denies 30-50% of variance requests in high-variance communities. Exit options are genuinely constrained—sale takes 6+ months, buyer pool is filtered by covenant acceptance, legal challenge costs $10,000-$50,000. Theater ratio (0.64): High and increasing. Architectural review committees often lack technical expertise in structural engineering, energy efficiency, or building code compliance. Review focuses on subjective aesthetics (color, landscaping style, material finish) rather than safety/functionality. As housing pressure increased post-2000, theater ratio rose—boards became more focused on gatekeeping aesthetic compliance than solving genuine coordination problems.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximum between constrained homeowner and property value preservationist. The preservationist sees Rope (coordination tool maintaining market); the constrained homeowner sees Snare (extraction mechanism with no exit). Minority homeowners see Tangled Rope—they experience both coordination benefit (property value maintenance) and asymmetric extraction (identity-correlated enforcement). The HOA board experiences role ambiguity: maintains coordination function on behalf of preservationists while enforcing extraction against constrained residents. The analytical observer sees the constraint as genuinely hybrid—both functions are structurally real, not perceptual artifacts. This gap is NOT resolved by claiming one perspective is 'correct'; rather, it reveals that the constraint's true structure is asymmetric: beneficiaries experience coordination, victims experience extraction, from the same mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) range from 0.05 for property value preservationists (full beneficiary + arbitrage exit → low d) to 0.95 for constrained homeowners (full victim + trapped exit → high d). Minority property owners occupy middle ground (0.60-0.75 depending on enforcement variance they experience): they are victims of identity-correlated enforcement but also benefit from property value stability. HOA board members present a complexity—they are organized enforcer actors (d ≈ 0.40 from organizational perspective) but also individuals bound by same covenants (d ≈ 0.65 from individual perspective). The engine derives these values from beneficiary/victim declarations and exit option mapping without explicit computation.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL MANDATROPHY CASE: Covenants present themselves as neutral aesthetic coordination ('maintain neighborhood character') but structural analysis reveals they function as extraction mechanisms (property value preservation for incumbents, housing supply rationing for outsiders). The mandatrophy is resolved by: (1) declaring beneficiaries (property preservationists, HOA boards) and victims (constrained homeowners, minority residents, low-income modification seekers), (2) noting that enforcement asymmetry is identity-correlated not purely aesthetic-correlated, (3) measuring that theater ratio is high and increasing (enforcement focus shifts from functional coordination to aesthetic gatekeeping), (4) observing that covenants create housing supply bottleneck by preventing affordable modifications. The Tangled Rope classification captures both the real coordination function (aesthetic harmony reduces free-riding risk) and the real extraction function (gatekeeping preserves property values for incumbents). The false summit risk is high: analytical observers can naturalize covenants as 'just maintaining neighborhood standards' without noticing the asymmetric enforcement and housing supply effects. The measurement timeline shows extraction creeping upward as theater dominates—original functional problem (genuine property damage) is solved, leaving pure gatekeeping.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_asymmetry_measurement,
    'How much of the observed enforcement variance across demographic groups reflects genuine aesthetic deviation vs. identity-correlated enforcement discretion?',
    'Comparative analysis of enforcement records: documentation of violations by violation type (color, landscaping, structural) cross-referenced with demographic data of violators and enforcement action types; natural language analysis of architectural review comments for identity-coded language',
    'If asymmetry is substantial (>40% variance unexplained by aesthetic deviation): constraint shifts from Rope-dominant to Snare-dominant at demographic perspectives. If asymmetry is minimal: coordination function is primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_asymmetry_measurement, empirical, 'Degree to which enforcement variance correlates with demographics vs. aesthetic deviation').

omega_variable(
    property_value_causation,
    'Do restrictive covenants causally produce property value premiums, or do covenants cluster in neighborhoods that would maintain high values regardless?',
    'Comparative market analysis: panel data regression of property price appreciation in covenant vs. non-covenant neighborhoods controlling for initial price, location, school district, market trends; natural experiments where covenants were struck down or weakened',
    'If causal effect is large: coordination function is real and captures genuine beneficiary value. If effect is negligible: covenants are extractive gatekeeping with minimal coordination benefit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(property_value_causation, empirical, 'Causal effect of covenants on property value appreciation').

omega_variable(
    exit_option_accessibility,
    'What fraction of constrained homeowners have meaningful exit options (sell within reasonable timeframe to buyers who accept covenants, or exit through legal challenge)?',
    'Market data: time-to-sale analysis for covenant-constrained properties vs. unconstrained; legal data: success rates and costs of covenant challenges by homeowner demographic; interview data: perceived exit options among homeowners',
    'If exit options are rare (<20%): constraint appears as trapped Snare for most. If exit options are moderate (40%+): constraint appears as constrained Tangled Rope. If high (>60%): classification shifts to mobile Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_accessibility, empirical, 'Availability and cost of exit options for constrained homeowners').

omega_variable(
    covenant_modernization_barrier,
    'How much do restrictive covenants prevent adaptive reuse, energy efficiency upgrades, and density increases that would reduce housing costs and environmental impact?',
    'Regulatory analysis: comparison of allowable modifications in covenant vs. non-covenant neighborhoods; case studies of denied/approved variance requests for solar, ADUs, modern building materials; cost impact analysis',
    'If barrier is substantial: covenants function as supply-side constraint on housing affordability, making extraction mechanism social-level (rationing housing to higher-income residents). If barrier is minimal: coordination function dominates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(covenant_modernization_barrier, empirical, 'Impact of covenants on housing supply constraints and modernization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenants, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa_tr_t0, hoa_covenants, theater_ratio, 0, 0.35).
narrative_ontology:measurement(hoa_tr_t20, hoa_covenants, theater_ratio, 20, 0.52).
narrative_ontology:measurement(hoa_tr_t40, hoa_covenants, theater_ratio, 40, 0.64).

% Extraction over time
narrative_ontology:measurement(hoa_be_t0, hoa_covenants, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(hoa_be_t20, hoa_covenants, base_extractiveness, 20, 0.46).
narrative_ontology:measurement(hoa_be_t40, hoa_covenants, base_extractiveness, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenants, enforcement_mechanism).
narrative_ontology:affects_constraint(hoa_covenants, housing_supply_constraint).
narrative_ontology:affects_constraint(hoa_covenants, aesthetic_gatekeeping_enforcement).
narrative_ontology:affects_constraint(hoa_covenants, property_value_preservation_regime).

% DUAL FORMULATION NOTE:
% HOA covenants are downstream of broader property rights regimes (deed covenants, fee simple title restrictions). The architectural review process is a distinct constraint mechanism. The aesthetic gatekeeping function can be decomposed into formal enforcement (architectural review committee decisions) and informal enforcement (social pressure, market filtering of buyers).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hoa_covenants, organized, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
