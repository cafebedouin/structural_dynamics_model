% ============================================================================
% CONSTRAINT STORY: visa_judgment_sharing_agreement
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_visa_judgment_sharing_agreement, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: visa_judgment_sharing_agreement
 *   human_readable: Visa Judgment Sharing Agreement (AMEX Antitrust Case)
 *   domain: legal/economic
 *
 * SUMMARY:
 *   A contractual coordination mechanism between Visa U.S.A. Inc. and its
 *   signatory member banks to allocate financial liability arising from
 *   antitrust litigation (specifically the American Express case). This
 *   agreement prevents individual member banks from being destroyed by
 *   joint and several liability, thereby stabilizing the network to
 *   facilitate Visa's IPO.
 *
 * KEY AGENTS (by structural relationship):
 *   - signatory_member_banks: Primary target (organized/trapped) — bear the allocated financial liability.
 *   - visa_inc_shareholders: Primary beneficiary (institutional/arbitrage) — benefit from the de-risked IPO and stabilized network.
 *   - american_express: External pressure (institutional/mobile) — the plaintiff whose legal victory creates the liability.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(visa_judgment_sharing_agreement, 0.40).
domain_priors:suppression_score(visa_judgment_sharing_agreement, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(visa_judgment_sharing_agreement, 0.14).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(visa_judgment_sharing_agreement, extractiveness, 0.40).
narrative_ontology:constraint_metric(visa_judgment_sharing_agreement, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(visa_judgment_sharing_agreement, theater_ratio, 0.14).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(visa_judgment_sharing_agreement, tangled_rope).
narrative_ontology:human_readable(visa_judgment_sharing_agreement, "Visa Judgment Sharing Agreement (AMEX Antitrust Case)").
narrative_ontology:topic_domain(visa_judgment_sharing_agreement, "legal/economic").

% --- Binary flags ---
domain_priors:requires_active_enforcement(visa_judgment_sharing_agreement). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(visa_judgment_sharing_agreement, visa_inc_shareholders).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(visa_judgment_sharing_agreement, signatory_member_banks).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three)

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

% PERSPECTIVE 1: THE PRIMARY TARGET (TANGLED ROPE)
% A smaller signatory bank. They are forced to sign an agreement that binds
% them to pay for the network's past behavior. Leaving the network during
% the IPO restructuring is prohibitively costly.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42
% χ ≈ 0.40 * 1.42 * 1.0 = 0.568. This is not a Snare (χ < 0.66) but a Tangled Rope.
constraint_indexing:constraint_classification(visa_judgment_sharing_agreement, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Visa's legal team or new IPO shareholders. The agreement is a pure
% coordination tool that transforms unquantifiable systemic risk into a
% structured, manageable liability, enabling the IPO to proceed.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
constraint_indexing:constraint_classification(visa_judgment_sharing_agreement, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. Recognizes both the essential coordination
% function (stabilizing the network) and the coercive, extractive nature
% of forcing members to cover liabilities under high exit costs.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15. Scope is global (σ=1.2).
% χ ≈ 0.40 * 1.15 * 1.2 = 0.552. This is a clear Tangled Rope.
constraint_indexing:constraint_classification(visa_judgment_sharing_agreement, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(visa_judgment_sharing_agreement_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(visa_judgment_sharing_agreement, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(visa_judgment_sharing_agreement, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget = tangled_rope,
    TypeBeneficiary = rope,
    TypeTarget \= TypeBeneficiary.

test(analytical_classification_matches_claim) :-
    narrative_ontology:constraint_claim(visa_judgment_sharing_agreement, ClaimedType),
    constraint_indexing:constraint_classification(visa_judgment_sharing_agreement, AnalyticalType, context(agent_power(analytical), _, _, _)),
    ClaimedType == AnalyticalType.

test(tangled_rope_structural_properties_present) :-
    narrative_ontology:constraint_beneficiary(visa_judgment_sharing_agreement, _),
    narrative_ontology:constraint_victim(visa_judgment_sharing_agreement, _),
    domain_priors:requires_active_enforcement(visa_judgment_sharing_agreement).

:- end_tests(visa_judgment_sharing_agreement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base metrics (ε=0.40, S=0.70) strongly indicate a hybrid constraint.
 *   The extractiveness represents the mandatory financial contributions from
 *   member banks to cover the legal judgment. The high suppression score
 *   reflects the prohibitive cost for a bank to exit the Visa network,
 *   especially during the IPO restructuring, making participation in the
 *   agreement non-optional. The constraint requires active legal enforcement,
 *   has clear beneficiaries (shareholders) and victims (member banks), fitting
 *   the Tangled Rope profile perfectly.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For Visa Inc. and its new shareholders (institutional),
 *   the agreement is a pure Rope: a brilliant legal instrument for risk
 *   coordination that makes a multi-billion dollar IPO possible. For a
 *   signatory bank (powerless/trapped), it's a Tangled Rope: they are coerced
 *   into paying for the network's past actions to protect an upside (the IPO)
 *   that primarily benefits the central corporation and its new owners. They
 *   are simultaneously coordinated and extracted from.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `visa_inc_shareholders`. They receive the primary benefit
 *     of a de-risked, stable, publicly-traded company. Their relationship is
 *     one of pure upside, shielded from the direct liability.
 *   - Victims: `signatory_member_banks`. They bear the direct financial cost
 *     of the settlement. While they also benefit from network stability, the
 *     cost is imposed upon them with no viable alternative, making them the
 *     structural victims of the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a classic example of a Tangled Rope that could be mislabeled.
 *   A purely institutional view would call it a Rope, ignoring the coercive
 *   extraction from members. A purely victim-focused view might call it a Snare,
 *   ignoring the genuine and critical coordination function it serves in
 *   preventing network collapse. The Tangled Rope classification correctly
 *   captures this duality: it is a tool of both coordination and extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_visa_judgment_sharing_agreement,
    'Is the liability allocation formula based on proportional historical revenue (fair coordination) or on political leverage within the Visa association (asymmetric extraction)?',
    'Audit of the confidential exhibit filed with the SEC, which details the allocation formula.',
    'If revenue-based, the constraint is closer to a pure Rope. If leverage-based, the extractive component is stronger, reinforcing the Tangled Rope/Snare classification for smaller members.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_visa_judgment_sharing_agreement, empirical, 'The fairness of the liability allocation formula, which is detailed in a confidential legal exhibit.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(visa_judgment_sharing_agreement, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is < 0.46, so temporal measurements are not required.
% narrative_ontology:measurement(visa_judgment_sharing_agreement_tr_t0, visa_judgment_sharing_agreement, theater_ratio, 0, 0.14).
% narrative_ontology:measurement(visa_judgment_sharing_agreement_tr_t5, visa_judgment_sharing_agreement, theater_ratio, 5, 0.14).
% narrative_ontology:measurement(visa_judgment_sharing_agreement_tr_t10, visa_judgment_sharing_agreement, theater_ratio, 10, 0.14).
%
% narrative_ontology:measurement(visa_judgment_sharing_agreement_ex_t0, visa_judgment_sharing_agreement, base_extractiveness, 0, 0.40).
% narrative_ontology:measurement(visa_judgment_sharing_agreement_ex_t5, visa_judgment_sharing_agreement, base_extractiveness, 5, 0.40).
% narrative_ontology:measurement(visa_judgment_sharing_agreement_ex_t10, visa_judgment_sharing_agreement, base_extractiveness, 10, 0.40).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The agreement allocates a shared financial burden (a resource).
narrative_ontology:coordination_type(visa_judgment_sharing_agreement, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations and exit options accurately models the dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */