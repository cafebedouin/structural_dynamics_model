% ============================================================================
% CONSTRAINT STORY: creative_commons_licensing
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_creative_commons_licensing, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: creative_commons_licensing
 *   human_readable: Creative Commons Licensing Framework
 *   domain: legal/technological
 *
 * SUMMARY:
 *   The Creative Commons (CC) licensing framework provides a standardized way
 *   for creators to grant public permissions for their work, moving beyond the
 *   binary of "All Rights Reserved" copyright and the public domain. It functions
 *   as a coordination mechanism for collaborative culture, but its modular
 *   requirements (e.g., ShareAlike, NonCommercial) create asymmetric obligations,
 *   making it a Tangled Rope that both enables coordination and extracts value
 *   (in the form of compliance costs or foregone commercial rights).
 *
 * KEY AGENTS (by structural relationship):
 *   - Content Creators & Users (e.g., students, Wikipedians): Primary beneficiaries (moderate/mobile) — gain access to a rich commons for reuse and remixing.
 *   - Legacy Publishers: Primary targets (institutional/constrained) — their business model, based on copyright exclusivity, is undermined by the alternative CC provides.
 *   - Analytical Observer: Sees the dual function of coordination and asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creative_commons_licensing, 0.40).
domain_priors:suppression_score(creative_commons_licensing, 0.45).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(creative_commons_licensing, 0.05).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creative_commons_licensing, extractiveness, 0.40).
narrative_ontology:constraint_metric(creative_commons_licensing, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(creative_commons_licensing, theater_ratio, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(creative_commons_licensing, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(creative_commons_licensing). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(creative_commons_licensing, remix_culture_participants).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(creative_commons_licensing, legacy_publishers).
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
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE CONTENT USER (e.g., Student, Wikipedian)
% Agent who benefits from the coordination function. Engine derives d from:
%   beneficiary membership + mobile exit → d ≈ 0.15 → f(d) ≈ -0.01 → low/negative χ
constraint_indexing:constraint_classification(creative_commons_licensing, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: THE LEGACY PUBLISHER
% Agent whose business model is targeted by the constraint. Engine derives d from:
%   victim membership + constrained exit → d ≈ 0.90 → f(d) ≈ 1.35 → high χ
constraint_indexing:constraint_classification(creative_commons_licensing, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context. Sees both the coordination and extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(creative_commons_licensing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(creative_commons_licensing_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(creative_commons_licensing, TypeTarget, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(creative_commons_licensing, TypeBeneficiary, context(agent_power(powerless), _, _, _)),
    TypeTarget \= TypeBeneficiary,
    TypeTarget == tangled_rope,
    TypeBeneficiary == rope.

test(analytical_claim_consistency) :-
    narrative_ontology:constraint_claim(creative_commons_licensing, ClaimedType),
    constraint_indexing:constraint_classification(creative_commons_licensing, AnalyticalType, context(agent_power(analytical), _, _, _)),
    ClaimedType == AnalyticalType.

:- end_tests(creative_commons_licensing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint is modeled as a Tangled Rope because it has both a genuine
 *   coordination function and an asymmetric extractive component.
 *   - Base Extractiveness (ε=0.40): This reflects the non-monetary extraction
 *     inherent in the licenses. Conditions like ShareAlike (extracting future
 *     works into the commons), NonCommercial (extracting commercial potential),
 *     and Attribution (extracting reputational value) impose real costs on users.
 *   - Suppression (0.45): The network effects of the CC ecosystem and its
 *     integration into major platforms (Google, Flickr, Wikipedia) suppress
 *     the viability of alternative, more bespoke, or more restrictive licensing
 *     schemes, pushing creators towards the standardized CC options.
 *   - The combination of `constraint_beneficiary`, `constraint_victim`, and
 *     `requires_active_enforcement` meets the structural requirements for a
 *     Tangled Rope classification.
 *
 * PERSPECTIVAL GAP:
 *   - Beneficiaries (students, creators, Wikipedians) experience the system
 *     primarily through its coordination function. For them, the extraction
 *     is a small, acceptable price for access to a vast commons. The effective
 *     extraction (χ) is negative due to their beneficiary status, resulting in a
 *     Rope classification.
 *   - Victims (legacy publishers) experience the system as a direct threat to
 *     their business model of monetizing exclusivity. The CC framework extracts
 *     market share and relevance from them. Their victim status and constrained
 *     exit options yield a high directionality (d), leading to a high effective
 *     extraction (χ) and a Tangled Rope classification.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `remix_culture_participants`. This group directly benefits
 *     from the low-friction coordination CC provides for sharing and building
 *     upon creative works.
 *   - Victim: `legacy_publishers`. This group's structural position is harmed
 *     as CC provides a viable, low-cost alternative to their high-extraction
 *     copyright monopoly model.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope, rather than a pure Rope, correctly
 *   identifies that "free" and "open" systems are not without costs. The
 *   framework avoids mislabeling the real (though non-monetary) extraction
 *   as pure coordination. It captures the dual nature of the system: it builds
 *   a commons (Rope) by extracting compliance from its users and market share
 *   from its competitors (the "Tangled" aspect).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_cc_enforcement,
    'Can CC licenses survive a targeted legal challenge by AI companies claiming that training on data constitutes "Fair Use", thereby bypassing license terms like Share-Alike or Non-Commercial?',
    'Establishment of new case law specifically addressing the use of licensed material in large-scale AI training sets.',
    'If Fair Use wins, the CC Rope''s extractive component is nullified for AI, degrading its function. If CC terms hold, the Tangled Rope remains a strong coordination and control tool for creators.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_cc_enforcement, empirical, 'Legal enforceability of CC licenses against AI training models claiming fair use.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(creative_commons_licensing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data shows the maturation of the CC framework from a nascent idea
% to an established part of the digital infrastructure.
%
% Theater ratio over time (remains low):
narrative_ontology:measurement(cc_tr_t0, creative_commons_licensing, theater_ratio, 0, 0.0).
narrative_ontology:measurement(cc_tr_t5, creative_commons_licensing, theater_ratio, 5, 0.02).
narrative_ontology:measurement(cc_tr_t10, creative_commons_licensing, theater_ratio, 10, 0.05).

% Extraction over time (increases as network effects solidify obligations):
narrative_ontology:measurement(cc_ex_t0, creative_commons_licensing, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cc_ex_t5, creative_commons_licensing, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(cc_ex_t10, creative_commons_licensing, base_extractiveness, 10, 0.40).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(creative_commons_licensing, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% status and exit options accurately models the directionality for this constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */