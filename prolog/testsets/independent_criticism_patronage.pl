% ============================================================================
% CONSTRAINT STORY: independent_criticism_patronage
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_independent_criticism_patronage, []).

:- use_module(library(plunit)).
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
 *   constraint_id: independent_criticism_patronage
 *   human_readable: "The Patronage Model for Independent Cultural Criticism"
 *   domain: economic/technological
 *
 * SUMMARY:
 *   This constraint describes the system supporting niche, long-form
 *   cultural criticism through direct audience patronage, as exemplified
 *   by platforms like Patreon or Substack funding blogs like Colin Marshall's.
 *   It coordinates creators with interested audiences but extracts significant
 *   uncompensated labor related to brand-building and platform maintenance,
 *   while operating in an environment where stable institutional alternatives
 *   have been suppressed.
 *
 * KEY AGENTS (by structural relationship):
 *   - Aspiring Creators: Primary target (powerless/trapped) — face high barriers and extractive terms to enter the system.
 *   - Established Creators: Dual role (moderate/constrained) — benefit from the platform but also bear the cost of precarious labor.
 *   - Niche Audiences & Patronage Platforms: Primary beneficiaries (moderate/institutional) — receive value (content/revenue) with minimal friction.
 *   - Analytical Observer: Sees the full hybrid structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(independent_criticism_patronage, 0.48).
domain_priors:suppression_score(independent_criticism_patronage, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(independent_criticism_patronage, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(independent_criticism_patronage, extractiveness, 0.48).
narrative_ontology:constraint_metric(independent_criticism_patronage, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(independent_criticism_patronage, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(independent_criticism_patronage, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(independent_criticism_patronage). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(independent_criticism_patronage, established_creators).
narrative_ontology:constraint_beneficiary(independent_criticism_patronage, niche_audiences).
narrative_ontology:constraint_beneficiary(independent_criticism_patronage, patronage_platforms).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(independent_criticism_patronage, established_creators).
narrative_ontology:constraint_victim(independent_criticism_patronage, aspiring_creators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function.
   The engine derives d from beneficiary/victim membership + exit_options.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE ASPIRING CREATOR (TARGET)
% Sees a highly extractive system with high barriers to entry and low
% probability of success. The lack of alternatives makes it a trap.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(independent_criticism_patronage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PATRONAGE PLATFORM (BENEFICIARY)
% Sees a pure coordination mechanism that generates revenue by connecting
% creators and consumers. Extraction is externalized onto the creator.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
constraint_indexing:constraint_classification(independent_criticism_patronage, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ESTABLISHED CREATOR (DUAL ROLE)
% Experiences both the benefits of creative freedom and audience connection, and
% the costs of precarity and constant self-promotion. Exit is constrained by
% loss of income and social capital. This dual role is best modeled as a
% Tangled Rope. An override is used to reflect that the costs (precarity)
% often feel more salient than the benefits.
constraint_indexing:constraint_classification(independent_criticism_patronage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% Sees the complete structure: a genuine coordination function layered with
% significant, asymmetric extraction from creators. This is the canonical
% definition of a Tangled Rope.
constraint_indexing:constraint_classification(independent_criticism_patronage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilization),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(independent_criticism_patronage_tests).

test(perspectival_gap_target_beneficiary, [nondet]) :-
    constraint_indexing:constraint_classification(independent_criticism_patronage, snare,
        context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(independent_criticism_patronage, rope,
        context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope, [nondet]) :-
    constraint_indexing:constraint_classification(independent_criticism_patronage, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements) :-
    narrative_ontology:constraint_beneficiary(independent_criticism_patronage, _),
    narrative_ontology:constraint_victim(independent_criticism_patronage, _),
    domain_priors:requires_active_enforcement(independent_criticism_patronage).

:- end_tests(independent_criticism_patronage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): High. Represents the creator's precarious
 *     labor, the need for constant content production, and the uncompensated
 *     work of self-branding and audience management required to subsist.
 *   - Suppression Score (0.65): High. The decline of salaried positions in
 *     journalism and cultural institutions suppresses the alternative of a
 *     stable career path, forcing critics into this more volatile model.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. Patronage platforms (e.g., Substack) and niche audiences
 *   experience a clean coordination service (Rope) where they pay for a desired
 *   good. In contrast, aspiring creators face a system with high barriers and
 *   extractive potential (Snare). The established creator lives inside the
 *   ambiguity, benefiting from the coordination while bearing the costs of
 *   extraction, classifying it as a Tangled Rope. The analytical view confirms
 *   this hybrid nature.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `patronage_platforms` gain revenue; `niche_audiences` get
 *     specialized content; `established_creators` get a career path, however
 *     precarious.
 *   - Victims: `aspiring_creators` face a daunting, often insurmountable,
 *     barrier to entry. `established_creators` are also victims of the system's
 *     precarity, making them a dual-role agent. This duality is the defining
 *     feature of this Tangled Rope. A directionality override for the `moderate`
 *     power agent reflects the weight of this precarity.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the hybrid nature of the system.
 *   A simplistic analysis might label it a pure Rope ("it connects people!") or
 *   a pure Snare ("it exploits artists!"). The Tangled Rope classification
 *   mandated by the analytical perspective correctly holds both truths in
 *   tension: it has an essential coordination function AND a significant,
 *   asymmetric extraction function. This prevents mislabeling and allows for
 *   a more nuanced policy or design response.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_independent_criticism_patronage,
    'Is the patronage model a transitional phase (Scaffold) towards a new creative economy, or a stable, extractive equilibrium (Tangled Rope)?',
    'Long-term (10-20 year) data on creator income stability, career longevity, and burnout rates across multiple platforms.',
    'If Scaffold, current extraction is a temporary cost for building a better system. If Tangled Rope, the extraction is a permanent feature to be mitigated.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(independent_criticism_patronage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This model evolved from a low-stakes hobby (early blogs) to a high-stakes
% professionalized system. The data reflects increasing extraction and
% performative theater over the last decade. Required because ε > 0.46.

% Theater ratio over time (metric_substitution):
narrative_ontology:measurement(icp_tr_t0, independent_criticism_patronage, theater_ratio, 0, 0.05).
narrative_ontology:measurement(icp_tr_t5, independent_criticism_patronage, theater_ratio, 5, 0.15).
narrative_ontology:measurement(icp_tr_t10, independent_criticism_patronage, theater_ratio, 10, 0.20).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(icp_ex_t0, independent_criticism_patronage, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(icp_ex_t5, independent_criticism_patronage, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(icp_ex_t10, independent_criticism_patronage, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(independent_criticism_patronage, information_standard).

% Network relationships (structural influence edges)
% The rise of this model directly impacts the viability of older models.
narrative_ontology:affects_constraint(independent_criticism_patronage, legacy_media_funding_models).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% An override is used for the established creator (moderate power). As both a
% beneficiary and a victim with constrained exit, the automatic derivation might
% land near d=0.5. We override to d=0.75 to reflect that the precarity and
% constant pressure (victimhood) are more structurally significant than the
% creative freedom (benefit), pushing them closer to the target experience.
constraint_indexing:directionality_override(independent_criticism_patronage, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */