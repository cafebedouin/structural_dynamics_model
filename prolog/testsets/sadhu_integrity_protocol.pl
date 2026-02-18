% ============================================================================
% CONSTRAINT STORY: sadhu_integrity_protocol
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_sadhu_integrity_protocol, []).

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
 *   constraint_id: sadhu_integrity_protocol
 *   human_readable: The Integrity Requirement (Sadhu's Sugar)
 *   domain: social/ethical
 *
 * SUMMARY:
 *   This constraint mandates that an advisor must embody their own counsel before
 *   delivering it to others. In the source narrative, a Sadhu (holy man) delays
 *   giving advice to a boy to stop eating sugar for two weeks because the Sadhu
 *   was still consuming sugar himself. The constraint posits that effective
 *   social coordination (advice) requires internal synchronization (integrity),
 *   extracting time from those seeking immediate solutions to ensure efficacy.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Mother & Son (seekers_of_immediate_remedies): Primary targets (powerless/trapped) — bear the cost of the two-week delay.
 *   - The Sadhu (upholders_of_tradition): Primary beneficiary (institutional/mobile) — benefits from enhanced authority and social efficacy.
 *   - The Community (implicit): Secondary beneficiary — benefits from a system of trustworthy counsel.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: The protocol extracts two weeks of time and effort from the mother and son.
% It is not predatory, but an investment in the quality of the resulting change.
domain_priors:base_extractiveness(sadhu_integrity_protocol, 0.30).

% Rationale: It suppresses the "easy path" of immediate, hypocritical advice,
% making the quick fix invisible or invalid.
domain_priors:suppression_score(sadhu_integrity_protocol, 0.40).

% Rationale: The protocol is almost entirely functional. The Sadhu's action is
% private and only revealed later. Low theater.
domain_priors:theater_ratio(sadhu_integrity_protocol, 0.14).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sadhu_integrity_protocol, extractiveness, 0.30).
narrative_ontology:constraint_metric(sadhu_integrity_protocol, suppression_requirement, 0.40).
narrative_ontology:constraint_metric(sadhu_integrity_protocol, theater_ratio, 0.14).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(sadhu_integrity_protocol, tangled_rope).
narrative_ontology:human_readable(sadhu_integrity_protocol, "The Integrity Requirement (Sadhu's Sugar)").
narrative_ontology:topic_domain(sadhu_integrity_protocol, "social/ethical").

% --- Binary flags ---
% The Sadhu must actively enforce this rule upon himself.
domain_priors:requires_active_enforcement(sadhu_integrity_protocol).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(sadhu_integrity_protocol, upholders_of_tradition).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(sadhu_integrity_protocol, seekers_of_immediate_remedies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function.
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0, etc.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE MOTHER & SON (PRIMARY TARGETS)
% To the mother and son, the Sadhu's delay is a frustrating but necessary cost.
% They are trapped by their need for his specific, effective intervention.
% The constraint is a Tangled Rope: it has a clear coordination goal (effective
% advice) but extracts time and patience from them asymmetrically.
constraint_indexing:constraint_classification(sadhu_integrity_protocol, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE SADHU (PRIMARY BENEFICIARY)
% To the Sadhu, integrity is a "Rope"—a functional coordination mechanism.
% He uses the two-week delay as a tool to align his internal state with his
% external words, ensuring that his influence is "grounded" in reality.
% The cost to him (giving up sugar) is part of the coordination function, not extraction.
constraint_indexing:constraint_classification(sadhu_integrity_protocol, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: THE SON (POST-ADVICE)
% After the Sadhu reveals his sacrifice, the advice becomes a Snare. The boy's
% freedom to continue his habit is extracted because the authority is now
% "inescapable," being backed by demonstrated integrity. The social cost of
% defiance becomes extremely high.
constraint_indexing:constraint_classification(sadhu_integrity_protocol, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% The analyst sees both the coordination function (making advice effective) and
% the asymmetric extraction (the seeker pays the time-cost). This dual nature
% is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(sadhu_integrity_protocol, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sadhu_integrity_protocol_tests).

test(perspectival_gap) :-
    % Verify the Sadhu (beneficiary) and Mother (target) see different structures.
    constraint_indexing:constraint_classification(sadhu_integrity_protocol, TypeSadhu,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(sadhu_integrity_protocol, TypeMother,
        context(agent_power(powerless), time_horizon(biographical), _, _)),
    TypeSadhu \= TypeMother,
    TypeSadhu = rope,
    TypeMother = tangled_rope.

test(analytical_claim_matches_type) :-
    % The analytical perspective must match the declared constraint_claim.
    narrative_ontology:constraint_claim(sadhu_integrity_protocol, ClaimType),
    constraint_indexing:constraint_classification(sadhu_integrity_protocol, AnalyticalType,
        context(agent_power(analytical), _, _, _)),
    ClaimType == AnalyticalType.

:- end_tests(sadhu_integrity_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.30) and suppression (0.40) are moderate. The
 *   constraint isn't predatory, but it imposes real costs (time, effort, delay)
 *   and forecloses the easier path of hypocritical advice. This places it firmly
 *   in the Tangled Rope category from a structural standpoint.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The Sadhu (beneficiary) sees a pure Rope, a tool for
 *   coordinating his inner state with his social role to produce a good outcome.
 *   For him, the cost is functional. The Mother and Son (targets) experience
 *   this same mechanism as a Tangled Rope or Snare, where their agency and time
 *   are extracted to pay for the efficacy of the coordination. They bear the
 *   asymmetric cost of the Sadhu's integrity.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `upholders_of_tradition` (the Sadhu). The protocol reinforces
 *     their social standing and the efficacy of their role.
 *   - Victim: `seekers_of_immediate_remedies` (the Mother/Son). They desire a
 *     quick fix and instead must pay a "time tax" for an authentic one. This
 *     maps directly to the structural relationship driving the d-value derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   This story is a classic case of resolving Mandatrophy. A purely cynical
 *   analysis might label the Sadhu's delay a Snare of social control. However,
 *   the framework correctly identifies the genuine coordination function
 *   (making advice effective) and classifies the overall structure as a
 *   Tangled Rope, acknowledging both the function and the asymmetric cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_sadhu_integrity_protocol,
    "Does the boy's compliance stem from the Sadhu's internal change, or merely from the *story* of the Sadhu's change?",
    "Comparison of change-rates in subjects receiving advice from congruent advisors vs. hypocritical advisors whose hypocrisy is hidden.",
    "If internal change matters: Integrity is a law with real effect. If only the story matters: Integrity is a narrative Scaffold.",
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_sadhu_integrity_protocol, empirical, "Causality of advisor's internal state vs. narrative of their state on advisee compliance.").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(sadhu_integrity_protocol, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% No temporal data required as base_extractiveness (0.30) is below the
% monitoring threshold of 0.46.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint establishes a standard for authentic information transfer.
narrative_ontology:coordination_type(sadhu_integrity_protocol, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations accurately models the directionality in this narrative.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */