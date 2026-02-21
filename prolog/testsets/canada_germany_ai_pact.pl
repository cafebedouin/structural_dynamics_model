% ============================================================================
% CONSTRAINT STORY: canada_germany_ai_pact
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1.

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
 *   A 2024 declaration of intent between Canada and Germany to foster
 *   collaboration in Artificial Intelligence. The agreement structures
 *   cooperation and funding around government-designated "superclusters" in
 *   each country, aiming to set global standards and boost economic growth.
 *   This creates a formal, state-backed channel for AI development, which
 *   simultaneously enables coordination for members and creates barriers for
 *   non-members.
 *
 * KEY AGENTS (by structural relationship):
 *   - Non-member AI SMEs: Primary target (powerless/trapped) — bears costs of exclusion and market concentration.
 *   - Supercluster Member Corporations: Primary beneficiary (institutional/arbitrage) — receives funding, legitimacy, and preferred partner access.
 *   - National Governments: Secondary beneficiary/architect (institutional/arbitrage) — achieves geopolitical and economic policy goals.
 *   - National Competition Bureaus: Inter-institutional victim (institutional/constrained) - faces a state-sanctioned concentration of market power.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(canada_germany_ai_pact, 0.48).
domain_priors:suppression_score(canada_germany_ai_pact, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(canada_germany_ai_pact, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(canada_germany_ai_pact, extractiveness, 0.48).
narrative_ontology:constraint_metric(canada_germany_ai_pact, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(canada_germany_ai_pact, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this human-constructed constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(canada_germany_ai_pact, tangled_rope).

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(canada_germany_ai_pact). % Not a scaffold
domain_priors:requires_active_enforcement(canada_germany_ai_pact). % Required for Tangled Rope. Enforcement is via funding/access control.

% --- Emergence flag (required for mountain constraints) ---
% N/A for this human-constructed constraint.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(canada_germany_ai_pact, supercluster_member_corporations).
narrative_ontology:constraint_beneficiary(canada_germany_ai_pact, national_governments).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(canada_germany_ai_pact, non_member_ai_smes).
narrative_ontology:constraint_victim(canada_germany_ai_pact, excluded_academic_researchers).
narrative_ontology:constraint_victim(canada_germany_ai_pact, national_competition_bureaus).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% A non-member SME sees a state-backed cartel that extracts opportunity from
% the market. Their d is high due to victim status and trapped exit.
% χ = 0.48 * f(0.95) * 1.0 ≈ 0.48 * 1.42 = 0.68. This meets snare thresholds
% (ε≥0.46, suppression≥0.60, χ≥0.66).
constraint_indexing:constraint_classification(canada_germany_ai_pact, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% A corporation within a supercluster sees a pure coordination mechanism.
% Their d is low due to beneficiary status and arbitrage exit.
% χ = 0.48 * f(0.05) * 1.1 ≈ 0.48 * -0.12 * 1.1 = -0.06. This is a clear Rope.
constraint_indexing:constraint_classification(canada_germany_ai_pact, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The observer sees both the coordination function and the asymmetric extraction.
% χ = 0.48 * f(0.73) * 1.2 ≈ 0.48 * 1.15 * 1.2 = 0.66. This is in the Tangled
% Rope band (0.40 ≤ χ ≤ 0.90), and the structural requirements are met.
constraint_indexing:constraint_classification(canada_germany_ai_pact, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---

% Perspective 4A: The Policy Architect (Gov't Ministry) (ROPE)
% Sees the pact as a successful policy tool. Structurally identical to the
% primary corporate beneficiary.
constraint_indexing:constraint_classification(canada_germany_ai_pact, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% Perspective 4B: National Competition Bureau (TANGLED ROPE)
% This institutional actor is a 'victim' of the policy, as it creates market
% concentration they must now manage. Their exit is constrained; they cannot
% simply ignore a major government initiative. This results in a moderate d
% and a Tangled Rope classification. They see the coordination benefits but
% are acutely aware of the extractive, anti-competitive side effects.
constraint_indexing:constraint_classification(canada_germany_ai_pact, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(canada_germany_ai_pact_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify perspectival gap between target (SME) and beneficiary (Corp).
    constraint_indexing:constraint_classification(canada_germany_ai_pact, snare, context(agent_power(powerless), _, trapped, _)),
    constraint_indexing:constraint_classification(canada_germany_ai_pact, rope, context(agent_power(institutional), _, arbitrage, _)),
    true.

test(perspectival_gap_inter_institutional) :-
    % Verify gap between the policy architect and the competition bureau.
    constraint_indexing:constraint_classification(canada_germany_ai_pact, rope, context(agent_power(institutional), _, arbitrage, _)),
    constraint_indexing:constraint_classification(canada_germany_ai_pact, tangled_rope, context(agent_power(institutional), _, constrained, _)),
    true.

test(analytical_claim_matches) :-
    % The analytical observer's view should match the declared claim type.
    constraint_indexing:constraint_classification(canada_germany_ai_pact, Type, context(agent_power(analytical), _, _, _)),
    narrative_ontology:constraint_claim(canada_germany_ai_pact, Type).

test(tangled_rope_structural_gates_pass) :-
    % Verify all three required properties for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(canada_germany_ai_pact, _),
    narrative_ontology:constraint_victim(canada_germany_ai_pact, _),
    domain_priors:requires_active_enforcement(canada_germany_ai_pact).

:- end_tests(canada_germany_ai_pact_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (ε=0.48) is set high to reflect that public-private
 *   partnerships, while framed as coordination, often function as mechanisms
 *   to transfer resources and opportunity from a broad base (taxpayers,
 *   the wider market) to a select group of politically connected firms.
 *   The suppression score (0.65) is high because a state-backed international
 *   agreement is very difficult for outsiders to compete with. The theater
 *   ratio (0.20) is low as the pact is new and ostensibly functional.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For a member corporation (institutional/arbitrage), the
 *   pact is a pure Rope (χ < 0), a subsidy and coordination channel. For an
 *   excluded SME (powerless/trapped), it is a Snare (χ ≈ 0.68) that locks them
 *   out of the primary channel for funding and legitimacy, concentrating the
 *   market and extracting their future opportunities. The analytical view
 *   resolves this into a Tangled Rope, acknowledging both functions.
 *
 * DIRECTIONALITY LOGIC:
 *   The `supercluster_member_corporations` are declared beneficiaries, as they
 *   directly receive funding and preferential access. This gives them a low
 *   derived `d`. The `non_member_ai_smes` are declared victims, as the pact
 *   creates an exclusionary in-group that raises their cost of business and
 *   reduces their opportunities. This gives them a high derived `d`. This
 *   mapping directly reflects the structural creation of an economic in-group
 *   and out-group.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This story highlights a key inter-institutional tension. The government as
 *   a policy architect (e.g., Ministry of Innovation) sees a Rope, a tool to
 *   achieve its goals. However, another part of the same government (e.g.,
 *   a Competition Bureau) sees a Tangled Rope. They are a victim of the pact's
 *   extractive side-effects (market concentration) and are constrained in
 *   their ability to act against it. This difference is captured by their
 *   different exit options (`arbitrage` vs. `constrained`), which generates
 *   the different classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification avoids two common errors. It does not naively accept
 *   the pact's stated purpose and classify it as a pure Rope, ignoring the
 *   extractive consequences for outsiders. It also avoids cynically dismissing
 *   the coordination function and calling it a pure Snare. The Tangled Rope
 *   classification correctly identifies that the constraint has BOTH a genuine
 *   coordination function AND an asymmetric extractive component, which is
 *   the hallmark of many public-private partnerships.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_canada_germany_ai_pact,
    'Will the pact generate net innovation through coordination, or will it primarily entrench incumbents and stifle competition by creating a taxpayer-funded cartel?',
    'Longitudinal study (10yr) comparing innovation metrics (patents, startup formation, market entry) inside vs. outside the superclusters, against a control group.',
    'If net positive, it is a justifiable Tangled Rope. If net negative, it is a Snare masquerading as a Tangled Rope, with significant misallocated capital.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(canada_germany_ai_pact, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. This model shows a constraint that
% starts with high theater (announcements), becomes functional (mid-life),
% and then experiences creeping extraction and rent-seeking over time.
% Required because base_extractiveness (0.48) > 0.46.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(cda_ger_ai_tr_t0, canada_germany_ai_pact, theater_ratio, 0, 0.40).
narrative_ontology:measurement(cda_ger_ai_tr_t5, canada_germany_ai_pact, theater_ratio, 5, 0.20).
narrative_ontology:measurement(cda_ger_ai_tr_t10, canada_germany_ai_pact, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(cda_ger_ai_ex_t0, canada_germany_ai_pact, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(cda_ger_ai_ex_t5, canada_germany_ai_pact, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(cda_ger_ai_ex_t10, canada_germany_ai_pact, base_extractiveness, 10, 0.52).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This pact is primarily about directing funding and collaborative efforts.
narrative_ontology:coordination_type(canada_germany_ai_pact, resource_allocation).

% Network relationships (structural influence edges)
% Such a pact would structurally influence national policies on talent
% acquisition to supply the superclusters with personnel.
narrative_ontology:affects_constraint(canada_germany_ai_pact, skilled_worker_immigration_policy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this story. The standard derivation chain,
% using beneficiary/victim declarations and exit options, accurately captures
% the structural relationships and generates the correct perspectival gaps,
% including the inter-institutional one.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */