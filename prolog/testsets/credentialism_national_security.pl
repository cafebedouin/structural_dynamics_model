% ============================================================================
% CONSTRAINT STORY: credentialism_national_security
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_credentialism_national_security, []).

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
 *   constraint_id: credentialism_national_security
 *   human_readable: "The Harvard Rule" - Elite Credentialism in US National Security
 *   domain: political/social
 *
 * SUMMARY:
 *   An informal but highly powerful constraint in the US national security
 *   establishment that effectively requires candidates for top-tier positions
 *   (e.g., Secretary of Defense, National Security Advisor) to hold degrees
 *   from a small handful of elite universities. This acts as both a
 *   coordination mechanism for the establishment and an exclusionary barrier
 *   for outsiders, creating significant perspectival gaps. The case of Pete
 *   Hegseth, a credentialed populist, highlights the tension between holding
 *   the credential and adhering to the ideology it's presumed to represent.
 *
 * KEY AGENTS (by structural relationship):
 *   - non_credentialed_outsiders: Primary target (powerless/trapped) — bears extraction by being excluded from consideration.
 *   - national_security_establishment: Primary beneficiary (institutional/arbitrage) — benefits from a predictable, ideologically aligned talent pool that reinforces its power.
 *   - elite_academic_institutions: Secondary beneficiary (institutional/arbitrage) - benefits from the prestige and influence derived from being the gatekeeper.
 *   - analytical_observer: Analytical observer — sees both the coordination function and the asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(credentialism_national_security, 0.52).
domain_priors:suppression_score(credentialism_national_security, 0.85).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(credentialism_national_security, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(credentialism_national_security, extractiveness, 0.52).
narrative_ontology:constraint_metric(credentialism_national_security, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(credentialism_national_security, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A. This is a socially constructed constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(credentialism_national_security, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(credentialism_national_security). % Required for Tangled Rope. Maintained by hiring norms, social networks, media.

% --- Emergence flag (required for mountain constraints) ---
% N/A. This constraint is not natural.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(credentialism_national_security, national_security_establishment).
narrative_ontology:constraint_beneficiary(credentialism_national_security, elite_academic_institutions).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(credentialism_national_security, non_credentialed_outsiders).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three met)

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
% Agent who bears the most extraction (non_credentialed_outsiders).
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
% For this group, the constraint is a pure barrier with no coordination benefit.
constraint_indexing:constraint_classification(credentialism_national_security, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most (national_security_establishment). Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ.
% They perceive it as a pure, beneficial coordination mechanism for vetting talent.
constraint_indexing:constraint_classification(credentialism_national_security, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context (civilizational/analytical/global).
% The observer sees both the coordination function and the severe asymmetric
% extraction, classifying it as a Tangled Rope.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(credentialism_national_security, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE CREDENTIALED REBEL (TANGLED ROPE)
% An agent like Pete Hegseth, who holds the credential but rejects the ideology.
% For him, the constraint has provided access (coordination) but now demands
% ideological conformity (extraction). His power is 'organized' due to his
% political movement, and his exit is 'constrained' (he can't easily jump to a
% think tank if he alienates the establishment).
constraint_indexing:constraint_classification(credentialism_national_security, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(credentialism_national_security_tests).

test(perspectival_gap) :-
    % Verify the core perspectival gap between target (Snare) and beneficiary (Rope).
    constraint_indexing:constraint_classification(credentialism_national_security, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(credentialism_national_security, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('Perspectival gap validated: Target sees Snare, Beneficiary sees Rope.~n').

test(tangled_rope_conditions_met) :-
    % Verify that the structural conditions for a Tangled Rope classification are met.
    narrative_ontology:constraint_beneficiary(credentialism_national_security, _),
    narrative_ontology:constraint_victim(credentialism_national_security, _),
    domain_priors:requires_active_enforcement(credentialism_national_security).

test(analytical_claim_matches) :-
    % The analytical observer's classification must match the declared claim.
    narrative_ontology:constraint_claim(credentialism_national_security, Claim),
    constraint_indexing:constraint_classification(credentialism_national_security, Claim, context(agent_power(analytical), _, _, _)).

:- end_tests(credentialism_national_security_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.52): This value is high, reflecting the significant societal cost of excluding a vast pool of potential talent and diverse perspectives from national security leadership. The extraction is not monetary but one of opportunity and cognitive diversity.
 *   - Suppression (S=0.85): This score is very high because alternative pathways to these top positions are virtually non-existent. The constraint acts as an extremely effective, near-total filter.
 *   - Theater Ratio (T=0.20): While there is a performative aspect, the beneficiaries genuinely believe in its functional utility for coordination and vetting, preventing a Piton classification.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the 'national_security_establishment' (beneficiary), the constraint is a Rope. It solves a coordination problem by creating a trusted network with a shared intellectual foundation, reducing friction. For them, the extraction is zero or negative (a benefit).
 *   For 'non_credentialed_outsiders' (target), the constraint is a Snare. It is a pure barrier to entry that extracts their career potential for the benefit of others. The coordination function is irrelevant to them as they are excluded from the coordinated group.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: The 'national_security_establishment' and 'elite_academic_institutions' gain power, influence, and stability from this system. The engine correctly derives a low directionality (d) for them, resulting in a negative effective extraction (χ) and a Rope classification.
 *   - Victims: 'non_credentialed_outsiders' bear the full cost. The engine derives a high directionality (d) for this powerless, trapped group, leading to a high χ and a Snare classification.
 *   This structure correctly models the asymmetric distribution of costs and benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   This framework prevents mislabeling. A naive analysis might see only the exclusion and call it a pure Snare. Conversely, an insider analysis might see only the coordination and call it a pure Rope. The Deferential Realism model, by requiring multiple indexed perspectives, reveals the true nature of the constraint as a Tangled Rope from a systemic viewpoint. It possesses both a genuine coordination function for one group and a severe extractive function for another. The potential for a coalition of the excluded to gain 'organized' power (as seen in populist movements) demonstrates how classifications can shift as power dynamics change.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_credentialism_ns,
    'Is the coordination function of elite credentialism still substantively effective, or has it degraded into pure theatrical gatekeeping?',
    'A comparative empirical study of decision-making quality and policy outcomes from the few exceptional non-credentialed leaders vs. the credentialed norm in national security roles.',
    'If still effective -> Justifies the Tangled Rope classification. If degraded -> The constraint is drifting towards a Piton (high theater, low function), where the primary purpose is inertial self-preservation.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(credentialism_national_security, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data modeling the post-WWII solidification of this norm.
% The constraint grew more rigid and extractive over time.
% Base_extractiveness > 0.46, so this section is required.

% Theater ratio over time (slight increase in performative aspect):
narrative_ontology:measurement(cns_tr_t0, credentialism_national_security, theater_ratio, 0, 0.10).
narrative_ontology:measurement(cns_tr_t5, credentialism_national_security, theater_ratio, 5, 0.15).
narrative_ontology:measurement(cns_tr_t10, credentialism_national_security, theater_ratio, 10, 0.20).

% Extraction over time (increasing exclusion and narrowing of viewpoint):
narrative_ontology:measurement(cns_ex_t0, credentialism_national_security, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cns_ex_t5, credentialism_national_security, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(cns_ex_t10, credentialism_national_security, base_extractiveness, 10, 0.52).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This constraint functions as a standard for vetting human capital and
% ensuring a shared intellectual/social framework.
narrative_ontology:coordination_type(credentialism_national_security, information_standard).

% Network relationships (structural influence edges)
% This credentialing system is a major upstream driver of ideological conformity
% or "groupthink" in US foreign policy.
narrative_ontology:affects_constraint(credentialism_national_security, us_foreign_policy_groupthink).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The standard derivation chain,
% using the declared beneficiary/victim groups and their respective exit
% options (arbitrage vs. trapped), accurately models the directionality
% of the constraint and produces the correct perspectival gap.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */