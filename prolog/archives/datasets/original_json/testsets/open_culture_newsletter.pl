% ============================================================================
% CONSTRAINT STORY: open_culture_newsletter
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_open_culture_newsletter, []).

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
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: open_culture_newsletter
 *   human_readable: The "Free Newsletter for Email" Exchange
 *   domain: technological/economic
 *
 * SUMMARY:
 *   This constraint models the common online value exchange where a user provides
 *   their email address to a platform (Open Culture) in return for access to a
 *   curated newsletter. While presented as a simple, free transaction, it
 *   involves an asymmetric exchange of value: the user receives curated content,
 *   while the platform acquires a durable, monetizable asset (the user's contact
 *   information and attention). This structure has both a genuine coordination
 *   function (connecting users with content) and an extractive one (data capture).
 *
 * KEY AGENTS (by structural relationship):
 *   - Newsletter Subscribers (Powerless): Primary target (powerless/trapped) — users with low technical literacy or high dependency on the content who cannot easily exit.
 *   - Newsletter Subscribers (Moderate): Secondary target (moderate/mobile) — typical users who can easily unsubscribe.
 *   - Platform Owner (Open Culture): Primary beneficiary (institutional/arbitrage) — gains an email list, a valuable asset for audience building and potential monetization.
 *   - Analytical Observer: Sees the full hybrid structure of the exchange.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(open_culture_newsletter, 0.38).
domain_priors:suppression_score(open_culture_newsletter, 0.45).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(open_culture_newsletter, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(open_culture_newsletter, extractiveness, 0.38).
narrative_ontology:constraint_metric(open_culture_newsletter, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(open_culture_newsletter, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(open_culture_newsletter, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(open_culture_newsletter). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(open_culture_newsletter, platform_owner).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(open_culture_newsletter, newsletter_subscribers).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three) -> MET

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (POWERLESS SUBSCRIBER)
% A user who lacks the technical literacy to unsubscribe or is highly dependent
% on the content, making their exit option 'trapped'.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42
% χ = 0.38 * 1.42 * 1.2 (global scope) ≈ 0.65.
% This χ value is in the Tangled Rope range (0.40-0.90). The constraint's
% base extraction (0.38) and suppression (0.45) are too low for a Snare, so
% even for the most vulnerable agent, it remains a highly extractive Tangled Rope.
constraint_indexing:constraint_classification(open_culture_newsletter, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (PLATFORM OWNER)
% The owner sees this as a pure coordination mechanism to build an audience.
% The extractive element is perceived as a subsidy or profit.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12
% χ = 0.38 * -0.12 * 1.2 (global scope) ≈ -0.05. This is a clear Rope.
constraint_indexing:constraint_classification(open_culture_newsletter, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the coordination function and the asymmetric extraction.
% Engine derives canonical d ≈ 0.73 → f(d) ≈ 1.15 for this perspective.
% χ = 0.38 * 1.15 * 1.2 (global scope) ≈ 0.52. This identifies the hybrid
% nature as the objective reality, matching the target's classification.
constraint_indexing:constraint_classification(open_culture_newsletter, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE MODERATE SUBSCRIBER (TANGLED ROPE)
% A typical user who wants the content. Exit is mobile (can unsubscribe), so they are
% not powerless. They perceive a useful service (coordination) but also
% an extractive element (giving up data).
% Engine derives d from: victim membership + mobile exit → d ≈ 0.85 → f(d) ≈ 1.15
% χ = 0.38 * 1.15 * 1.2 (global scope) ≈ 0.52. This falls in the Tangled Rope range.
constraint_indexing:constraint_classification(open_culture_newsletter, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(open_culture_newsletter_tests).

test(perspectival_gap_powerless_vs_owner) :-
    % Verify the powerless subscriber and owner disagree.
    constraint_indexing:constraint_classification(open_culture_newsletter, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(open_culture_newsletter, TypeOwner, context(agent_power(institutional), _, _, _)),
    assertion(TypePowerless == tangled_rope),
    assertion(TypeOwner == rope),
    assertion(TypePowerless \= TypeOwner).

test(perspectival_gap_moderate_vs_owner) :-
    % Verify the moderate subscriber and owner also disagree.
    constraint_indexing:constraint_classification(open_culture_newsletter, TypeModerate, context(agent_power(moderate), _, _, _)),
    constraint_indexing:constraint_classification(open_culture_newsletter, TypeOwner, context(agent_power(institutional), _, _, _)),
    assertion(TypeModerate == tangled_rope),
    assertion(TypeOwner == rope),
    assertion(TypeModerate \= TypeOwner).

test(analytical_view_matches_claim) :-
    % The analytical classification must match the declared constraint claim.
    narrative_ontology:constraint_claim(open_culture_newsletter, ClaimedType),
    constraint_indexing:constraint_classification(open_culture_newsletter, AnalyticalType, context(agent_power(analytical), _, _, _)),
    assertion(ClaimedType == AnalyticalType).

test(tangled_rope_gate_requirements_met) :-
    % Verify all three structural requirements for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(open_culture_newsletter, _),
    narrative_ontology:constraint_victim(open_culture_newsletter, _),
    domain_priors:requires_active_enforcement(open_culture_newsletter).

:- end_tests(open_culture_newsletter_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.38): This score reflects the non-trivial but non-predatory
 *     value extraction. The user's email and attention are valuable assets, but the
 *     exchange for high-quality curated content is not grossly unfair. It's high
 *     enough to be more than a simple Rope, but low enough not to be a Snare.
 *   - Suppression (0.45): While other newsletters exist, Open Culture has a strong
 *     brand and a unique curation style, creating a moderate barrier to finding
 *     an exact substitute. It doesn't suppress all alternatives, but it's not
 *     a frictionless commodity.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the Platform Owner (Rope) and the Subscribers (Tangled Rope) is
 *   the core of the "product is you" business model. The owner sees a value-additive
 *   coordination tool (building a list), resulting in a negative effective extraction (χ).
 *   The subscriber experiences a useful service that comes with a cost (data/privacy),
 *   perceiving both coordination and extraction simultaneously. The directionality
 *   derivation engine captures this perfectly: the beneficiary's arbitrage exit and
 *   the victim's mobile/trapped exit produce vastly different `d` values, splitting the
 *   classification.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `platform_owner`. They directly benefit by acquiring a list of engaged
 *     users, an asset that can be leveraged for growth, influence, or monetization.
 *   - Victim: `newsletter_subscribers`. They bear the cost by surrendering personal
 *     data, accepting the risk of its misuse, and dedicating their attention.
 *   This clear beneficiary/victim structure is the input that allows the engine to
 *   calculate the perspectivally-divergent χ values and produce the Rope/Tangled Rope split.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two common errors.
 *   1. It avoids mislabeling this as a pure Rope, which would ignore the real economic
 *      value of the data being extracted from users.
 *   2. It avoids mislabeling it as a Snare, which would ignore the genuine coordination
 *      value provided by the curation service and the user's ability to easily exit.
 *   The Tangled Rope classification acknowledges the hybrid nature: it's a useful
 *   coordination service with an attached, non-trivial extractive mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_open_culture_newsletter,
    'What is the platform owner''s undisclosed future monetization strategy for the email list?',
    'An internal strategy document leak, a public change in terms of service, or the sale of the company.',
    'If the strategy involves selling data or aggressive advertising, the constraint''s base extractiveness (ε) would sharply increase, potentially drifting it into a Snare. If it remains focused on audience engagement, it remains a Tangled Rope.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_open_culture_newsletter, empirical, 'Future monetization strategy for the acquired email list.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(open_culture_newsletter, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is not a high-extraction constraint (ε < 0.46), so temporal data is
% not strictly required. However, we model a slight increase in extractiveness
% over time as the value of email lists became more widely understood in the
% digital economy.

% Theater ratio over time (remains low):
narrative_ontology:measurement(ocn_tr_t0, open_culture_newsletter, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ocn_tr_t5, open_culture_newsletter, theater_ratio, 5, 0.08).
narrative_ontology:measurement(ocn_tr_t10, open_culture_newsletter, theater_ratio, 10, 0.10).

% Extraction over time (slight accumulation):
narrative_ontology:measurement(ocn_ex_t0, open_culture_newsletter, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(ocn_ex_t5, open_culture_newsletter, base_extractiveness, 5, 0.34).
narrative_ontology:measurement(ocn_ex_t10, open_culture_newsletter, base_extractiveness, 10, 0.38).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This constraint functions to curate and distribute information, fitting the
% 'information_standard' type.
narrative_ontology:coordination_type(open_culture_newsletter, information_standard).

% Network relationships (structural influence edges)
% This constraint is a component of a larger system of online value exchange.
narrative_ontology:affects_constraint(open_culture_newsletter, ad_tech_surveillance_economy).
narrative_ontology:affects_constraint(platform_monetization_models, open_culture_newsletter).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation based
% on `constraint_beneficiary` and `constraint_victim` declarations, combined
% with the agents' respective exit options (`arbitrage` vs. `mobile`/`trapped`),
% accurately computes the directionality `d` for each perspective.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */