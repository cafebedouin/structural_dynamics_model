% ============================================================================
% CONSTRAINT STORY: canada_germany_ai_pact
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_canada_germany_ai_pact, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: canada_germany_ai_pact
 *   human_readable: Canada-Germany AI Supercluster Partnership Agreement
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The 2024 Canada-Germany AI Supercluster Partnership is a bilateral
 *   declaration of intent designed to foster collaboration, align on
 *   standards, and create joint research and commercialization opportunities
 *   in Artificial Intelligence. While framed as a pure coordination mechanism
 *   (a Rope), its structure involves the redirection of public funds to
 *   specific corporate and academic entities, creating an in-group of
 *   beneficiaries and an out-group of competitors. This introduces an
 *   extractive component, making it a classic example of a Tangled Rope.
 *
 * KEY AGENTS:
 *   - Affiliated AI Firms and Labs: Primary beneficiaries (institutional/arbitrage) who gain funding, access, and reduced collaborative friction.
 *   - Government Agencies: Secondary beneficiaries (institutional/arbitrage) who administer the pact, increasing their budget and influence.
 *   - General Taxpayers: Primary victims (powerless/trapped) whose funds are redirected to specific interests for a diffuse and uncertain public return.
 *   - Non-Affiliated AI Competitors: Secondary victims (moderate/constrained) who are disadvantaged by the creation of a subsidized bloc.
 *   - Geopolitical Strategists: Powerful observers (powerful/mobile) who view the pact as a tool for industrial and foreign policy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(canada_germany_ai_pact, 0.35).
domain_priors:suppression_score(canada_germany_ai_pact, 0.45).
domain_priors:theater_ratio(canada_germany_ai_pact, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(canada_germany_ai_pact, extractiveness, 0.35).
narrative_ontology:constraint_metric(canada_germany_ai_pact, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(canada_germany_ai_pact, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(canada_germany_ai_pact, tangled_rope).
narrative_ontology:human_readable(canada_germany_ai_pact, "Canada-Germany AI Supercluster Partnership Agreement").
narrative_ontology:topic_domain(canada_germany_ai_pact, "technological/economic").

domain_priors:requires_active_enforcement(canada_germany_ai_pact).
narrative_ontology:has_sunset_clause(canada_germany_ai_pact).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(canada_germany_ai_pact, affiliated_ai_firms_and_labs).
narrative_ontology:constraint_beneficiary(canada_germany_ai_pact, government_agencies_managing_pact).
narrative_ontology:constraint_victim(canada_germany_ai_pact, non_affiliated_ai_competitors).
narrative_ontology:constraint_victim(canada_germany_ai_pact, general_taxpayers_canada_germany).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GENERAL TAXPAYER (TANGLED ROPE) — Experiences the pact as an extractive mechanism where public funds are diverted to specific corporate interests with diffuse public benefit. As a trapped victim, directionality is maximal (d≈0.95), yielding χ≈0.50. This is below the Snare threshold but firmly in the Tangled Rope category, reflecting extraction that is not directly coercive.
constraint_indexing:constraint_classification(canada_germany_ai_pact, tangled_rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: AFFILIATED AI FIRM (ROPE) — As a primary beneficiary with arbitrage exit options (can seek other funding), the firm experiences the pact as pure coordination. It reduces friction for international collaboration and provides access to new markets and talent pools. Directionality is minimal (d≈0.05), yielding a negative effective extraction (χ≈-0.05), indicating a net subsidy.
constraint_indexing:constraint_classification(canada_germany_ai_pact, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: NON-AFFILIATED COMPETITOR (TANGLED ROPE) — This actor is a victim of the pact's soft suppression. While not actively targeted, they are constrained from accessing the pact's resources, creating a competitive disadvantage. They see both the coordination function (which benefits their rivals) and the extractive nature (which disadvantages them). d≈0.85, χ≈0.46.
constraint_indexing:constraint_classification(canada_germany_ai_pact, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: GEOPOLITICAL STRATEGIST (SCAFFOLD) — Views the pact as a temporary support structure to build a transatlantic AI bloc against other global powers. The goal is to bootstrap an ecosystem, not to create a permanent institution. The implicit sunset clause is the point at which the ecosystem is self-sustaining or geopolitical priorities shift. The low effective extraction (χ≈0.23) reflects this temporary, goal-oriented nature.
constraint_indexing:constraint_classification(canada_germany_ai_pact, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — The default analytical view recognizes the dual nature of the pact. It has a genuine coordination function (beneficiaries exist) and an asymmetric extractive function (victims exist), and requires active management. This meets the canonical definition of a Tangled Rope. The analytical d≈0.72 yields χ≈0.49.
constraint_indexing:constraint_classification(canada_germany_ai_pact, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(canada_germany_ai_pact_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(canada_germany_ai_pact, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(canada_germany_ai_pact, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(canada_germany_ai_pact_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.35): Moderate. The constraint extracts value from the general tax base and directs it to a narrow set of beneficiaries. It is not severely coercive, but represents a significant redirection of public resources. Suppression (0.45): Moderate. The pact creates preferential pathways for funding and collaboration, which softly suppresses non-affiliated competitors by raising their relative costs. It does not forbid alternatives but makes them less attractive. Theater Ratio (0.55): Significant. International partnership announcements carry a large performative element for political signaling, which may exceed the immediate, tangible collaborative output. Has Sunset Clause (true): Justified from a strategic perspective. Tech-specific industrial policies like this are rarely permanent; they are intended to catalyze an industry and are expected to become obsolete or be replaced as technology and geopolitical landscapes evolve over a 5-15 year horizon.
 *
 * PERSPECTIVAL GAP:
 *   The gap is significant. Beneficiaries (affiliated firms) experience a pure Rope, a mechanism that helps them coordinate. Powerless victims (taxpayers) and constrained victims (competitors) experience a Tangled Rope, seeing the extractive and suppressive side-effects. Powerful strategic observers see a temporary Scaffold, a means to a geopolitical end. The analytical view confirms the Tangled Rope classification, acknowledging both the coordination function and the asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The classification diversity stems directly from the directionality derivation. The institutional beneficiary with arbitrage exit options has a minimal 'd' value, resulting in negative effective extraction (a subsidy). The powerless, trapped taxpayer has a maximal 'd', leading to the highest effective extraction. The moderate, constrained competitor falls in between. Each perspective's structural relationship to the flow of value determines its classification of the same underlying constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This case resolves the mandatrophy by correctly identifying the constraint as a hybrid. Labeling it a pure Rope (as its proponents would) ignores the extraction from taxpayers and suppression of competitors. Labeling it a Snare (as a harsh critic might) would be an overstatement, as it lacks the high coercion and suppression of a true Snare and possesses a genuine coordination function. The Tangled Rope classification, as seen from the analytical perspective, provides the most accurate structural description.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(canada_germany_ai_pact, 2024, 2034).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(canada_germany_ai_pact, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
