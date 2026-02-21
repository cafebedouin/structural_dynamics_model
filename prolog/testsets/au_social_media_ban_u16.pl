% ============================================================================
% CONSTRAINT STORY: au_social_media_ban_u16
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_au_social_media_ban_u16, []).

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
 *   constraint_id: au_social_media_ban_u16
 *   human_readable: Australian Under-16 Social Media Ban
 *   domain: social_technological
 *
 * SUMMARY:
 *   A proposed Australian federal law to ban social media access for children
 *   under the age of 16, enforced through mandatory age verification systems
 *   implemented by platforms. The policy is presented as a measure to protect
 *   youth mental health, but creates a significant clash of interests between
 *   the government, tech companies, parents, and teenagers.
 *
 * KEY AGENTS (by structural relationship):
 *   - Australian Teens (13-15): Primary target (powerless/trapped) — lose social access and autonomy.
 *   - Australian Government / Parents' Groups: Primary beneficiaries (institutional/arbitrage) — achieve policy goals and perceived safety.
 *   - Social Media Platforms: Secondary victims (institutional/constrained) — bear implementation costs and user loss.
 *   - Analytical Observer: Analytical observer — sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(au_social_media_ban_u16, 0.48).
domain_priors:suppression_score(au_social_media_ban_u16, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(au_social_media_ban_u16, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(au_social_media_ban_u16, extractiveness, 0.48).
narrative_ontology:constraint_metric(au_social_media_ban_u16, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(au_social_media_ban_u16, theater_ratio, 0.30).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(au_social_media_ban_u16, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(au_social_media_ban_u16). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% N/A for this constraint.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(au_social_media_ban_u16, australian_government).
narrative_ontology:constraint_beneficiary(au_social_media_ban_u16, australian_parents_groups).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(au_social_media_ban_u16, australian_teens_13_15).
narrative_ontology:constraint_victim(au_social_media_ban_u16, social_media_platforms).
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
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% For a teenager, the ban is purely coercive and extractive, removing a key
% social utility with no immediate, tangible benefit.
constraint_indexing:constraint_classification(au_social_media_ban_u16, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
% The government sees this as a pure coordination tool to solve a public
% health problem, with costs externalized onto others.
constraint_indexing:constraint_classification(au_social_media_ban_u16, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context sees both coordination and extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(au_social_media_ban_u16, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: INTER-INSTITUTIONAL VICTIM (TANGLED ROPE)
% Social media platforms are powerful but are victims of the mandate and have
% constrained exit options (cannot easily abandon the Australian market).
% Engine derives d from: victim membership + constrained exit -> d > 0.5.
% They see the coordination function but also the heavy, targeted cost.
constraint_indexing:constraint_classification(au_social_media_ban_u16, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(au_social_media_ban_u16_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    % Verify the core perspectival gap between the target and beneficiary.
    constraint_indexing:constraint_classification(au_social_media_ban_u16, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(au_social_media_ban_u16, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(au_social_media_ban_u16, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation_for_tangled_rope) :-
    % A Tangled Rope must have significant extraction and suppression.
    domain_priors:base_extractiveness(au_social_media_ban_u16, E), E >= 0.30,
    domain_priors:suppression_score(au_social_media_ban_u16, S), S >= 0.40.

:- end_tests(au_social_media_ban_u16_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): High. The constraint imposes a significant non-monetary cost on teenagers (loss of social networks, communities, information access) and a direct financial cost on tech platforms (implementation of age verification, loss of engagement).
 *   - Suppression Score (0.75): High. This is a blanket legal prohibition that makes alternative social media use for this age group illegal, relying on platform-level enforcement. While circumvention (VPNs) is possible, it is suppressed.
 *   - Theater Ratio (0.30): Moderate. The policy is functional and will have real effects, but it also serves a strong performative function for a government wanting to be "seen taking action" on youth mental health, a salient public issue.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For a teenager (powerless, trapped), the ban is a Snare. It removes a valued part of their life without their consent for a paternalistic benefit they may not agree with. The extraction is immediate and personal. For the government (institutional, arbitrage), it is a Rope. It's a tool to coordinate a societal response to a perceived problem (youth mental health crisis), and the costs are almost entirely externalized onto teens and tech companies.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: The Australian Government and advocacy groups for parents benefit politically and ideologically. They are the architects and proponents.
 *   - Victims: Teenagers (13-15) are the primary targets, losing autonomy and social access. Social media platforms are secondary victims, forced to bear the costs of implementation and enforcement. This clear division drives the directionality calculation and the resulting perspectival gaps.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The constraint creates a conflict between two institutional actors: the Australian government and social media platforms. The government, with `arbitrage` exit (it can repeal or amend its own law), sees a Rope. The platforms, with `constrained` exit (they cannot afford to leave the Australian market), experience it as a costly, coercive mandate. Their victim status and constrained exit yield a higher directionality `d`, correctly classifying it as a Tangled Rope from their perspective, reflecting the extractive component they face.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a canonical Tangled Rope. Classifying it as a pure Rope (the government's view) would be mandatrophy, ignoring the severe, targeted extraction imposed on a powerless group. Classifying it as a pure Snare (the teenager's view) would ignore the genuine (though debatable) coordination function related to public health. The Tangled Rope classification correctly holds both truths in tension: it is a tool for coordination that operates via asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_au_social_media_ban_u16,
    'Does the ban produce a net positive mental health outcome for teens that outweighs the costs of social disconnection, reduced autonomy, and pushing activity to less-moderated platforms?',
    'Longitudinal studies over 5-10 years comparing mental health metrics in Australian teens against a control population, combined with qualitative studies on social behavior.',
    'If true, the coordination function is validated, strengthening its Rope-like character. If false, the policy is pure extraction for political theater, making it functionally a Snare.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(au_social_media_ban_u16, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. This constraint has high extraction (ε>0.46)
% and thus requires temporal tracking. We model a slight increase in theater over
% time as the initial policy goal is diluted by implementation realities.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(au_social_media_ban_u16_tr_t0, au_social_media_ban_u16, theater_ratio, 0, 0.20).
narrative_ontology:measurement(au_social_media_ban_u16_tr_t5, au_social_media_ban_u16, theater_ratio, 5, 0.25).
narrative_ontology:measurement(au_social_media_ban_u16_tr_t10, au_social_media_ban_u16, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
% Modeled as slightly increasing as loopholes are closed.
narrative_ontology:measurement(au_social_media_ban_u16_ex_t0, au_social_media_ban_u16, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(au_social_media_ban_u16_ex_t5, au_social_media_ban_u16, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(au_social_media_ban_u16_ex_t10, au_social_media_ban_u16, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This is a government mandate, a form of enforcement.
narrative_ontology:coordination_type(au_social_media_ban_u16, enforcement_mechanism).

% Network relationships (structural influence edges)
% The enforcement of this ban may require or accelerate the creation of a
% national digital identity system.
narrative_ontology:affects_constraint(au_social_media_ban_u16, digital_identity_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% from beneficiary/victim declarations and exit options accurately models the
% power dynamics between the government, teens, and platforms.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */