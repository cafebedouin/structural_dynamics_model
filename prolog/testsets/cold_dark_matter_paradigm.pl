% ============================================================================
% CONSTRAINT STORY: cold_dark_matter_paradigm
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_cold_dark_matter_paradigm, []).

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
    constraint_indexing:directionality_override/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cold_dark_matter_paradigm
 *   human_readable: "The Lambda-CDM model's 'cold dark matter' tenet as a dominant scientific paradigm"
 *   domain: scientific
 *
 * SUMMARY:
 *   The Lambda-CDM (ΛCDM) model is the standard model of Big Bang cosmology.
 *   Its assertion that dark matter is "cold" (non-relativistic) and non-interacting
 *   acts as a powerful constraint on research. It provides a common framework for
 *   thousands of scientists (coordination), but also directs funding and publication
 *   priority towards confirming evidence, while suppressing alternative theories (extraction).
 *   Recent JWST observations of galaxy cluster MACS J0417 challenge this tenet,
 *   as the observed formation of ultra-diffuse galaxies is better explained by
 *   Self-Interacting Dark Matter (SIDM), a competing theory.
 *
 * KEY AGENTS (by structural relationship):
 *   - early_career_dissident_researcher: Primary target (powerless/trapped) — bears career risk and funding exclusion.
 *   - paradigm_aligned_cosmologists: Primary beneficiary (institutional/arbitrage) — benefits from coordinated research and resource allocation.
 *   - alternative_cosmology_proponents: Organized victim group (organized/constrained) — actively resists the paradigm but faces high barriers.
 *   - analytical_observer: Sees both the coordination function and the extractive suppression.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% ε is high due to the opportunity cost: billions in funding for direct-detection
% experiments (that have found nothing) and other ΛCDM-aligned projects that
% could have funded alternative research avenues.
domain_priors:base_extractiveness(cold_dark_matter_paradigm, 0.48).

% Suppression is high: peer review, funding committees, and hiring are dominated by
% proponents of the standard model, creating a high barrier for novel theories.
domain_priors:suppression_score(cold_dark_matter_paradigm, 0.65).

% Theater is moderate. While much work is genuine, a portion of research involves
% minor tweaks or confirmations of the paradigm, which can be seen as theatrical
% maintenance rather than fundamental discovery.
domain_priors:theater_ratio(cold_dark_matter_paradigm, 0.30).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cold_dark_matter_paradigm, extractiveness, 0.48).
narrative_ontology:constraint_metric(cold_dark_matter_paradigm, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(cold_dark_matter_paradigm, theater_ratio, 0.30).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(cold_dark_matter_paradigm, tangled_rope).

% --- Binary flags ---
% Enforcement is active through peer review, grant allocation, and academic hiring.
% This is required for a Tangled Rope classification.
domain_priors:requires_active_enforcement(cold_dark_matter_paradigm).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.

% Who benefits from this constraint existing?
% The large community of researchers whose work, careers, and experiments are
% built on the foundation of the ΛCDM model.
narrative_ontology:constraint_beneficiary(cold_dark_matter_paradigm, paradigm_aligned_cosmologists).

% Who bears disproportionate cost?
% Researchers promoting alternative models (SIDM, MOND, etc.) and early-career
% scientists whose novel ideas are suppressed by the dominant paradigm.
narrative_ontology:constraint_victim(cold_dark_matter_paradigm, alternative_cosmology_proponents).
narrative_ontology:constraint_victim(cold_dark_matter_paradigm, early_career_dissident_researcher).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function. The engine derives d
   from beneficiary/victim membership + exit_options.
   Scope is 'global' (σ=1.2) as cosmology is a global scientific field.
   ========================================================================== */

% PERSPECTIVE 1: THE EARLY-CAREER DISSIDENT RESEARCHER (SNARE)
% A postdoc or grad student whose work contradicts the paradigm. They risk their
% career, face rejection from journals/funders, and are effectively trapped.
% Victim + Trapped -> d ≈ 0.95 -> f(d) ≈ 1.42.
% χ = 0.48 * 1.42 * 1.2 ≈ 0.82 (Snare).
constraint_indexing:constraint_classification(cold_dark_matter_paradigm, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PARADIGM-ALIGNED COSMOLOGIST (ROPE)
% An established professor whose work uses and supports ΛCDM. For them, it is a
% pure coordination tool that enables collaboration and secures funding.
% Beneficiary + Arbitrage -> d ≈ 0.05 -> f(d) ≈ -0.12.
% χ = 0.48 * -0.12 * 1.2 ≈ -0.07 (Rope).
constraint_indexing:constraint_classification(cold_dark_matter_paradigm, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The system view, recognizing both the vital coordination function and the
% severe extractive/suppressive effect on alternative inquiry.
% Analytical -> d ≈ 0.72 -> f(d) ≈ 1.15.
% χ = 0.48 * 1.15 * 1.2 ≈ 0.66 (Tangled Rope).
constraint_indexing:constraint_classification(cold_dark_matter_paradigm, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ORGANIZED PROPONENT OF AN ALTERNATIVE THEORY (SNARE)
% A researcher in a competing school of thought (e.g., SIDM). They are organized
% but constrained by the dominant paradigm's control over resources. For them, it
% is a coercive barrier to progress.
% Victim + Constrained -> d ≈ 0.90 -> f(d) ≈ 1.34.
% χ = 0.48 * 1.34 * 1.2 ≈ 0.77 (Snare).
constraint_indexing:constraint_classification(cold_dark_matter_paradigm, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cold_dark_matter_paradigm_tests).

test(perspectival_gap_is_rope_vs_snare) :-
    constraint_indexing:constraint_classification(cold_dark_matter_paradigm, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(cold_dark_matter_paradigm, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(cold_dark_matter_paradigm, tangled_rope, context(agent_power(analytical), _, _, _)).

test(organized_opposition_sees_snare) :-
    constraint_indexing:constraint_classification(cold_dark_matter_paradigm, snare, context(agent_power(organized), _, exit_options(constrained), _)).

test(is_high_extraction_and_suppression) :-
    domain_priors:base_extractiveness(cold_dark_matter_paradigm, E), E >= 0.46,
    domain_priors:suppression_score(cold_dark_matter_paradigm, S), S >= 0.60.

:- end_tests(cold_dark_matter_paradigm_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.48) is high, representing the immense opportunity
 *   cost of directing decades of research, funding, and careers down a path that
 *   may be flawed. If ΛCDM is incorrect, this represents a massive misallocation of
 *   scientific resources. The suppression score (0.65) reflects the institutional
 *   mechanisms of science (peer review, funding panels) that enforce conformity to
 *   the dominant paradigm, making it difficult for competing ideas to emerge.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For a beneficiary within the paradigm (`paradigm_aligned_cosmologists`),
 *   it is a perfect coordination device (Rope), providing a common language and
 *   predictive framework. For someone outside or challenging it (`early_career_dissident_researcher`),
 *   it is a career-threatening trap (Snare) that extracts their potential and suppresses
 *   their contributions. This is a classic signature of a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are the incumbent researchers and institutions whose work
 *   is validated and funded by the paradigm. The victims are the challengers and
 *   proponents of alternative theories who bear the cost of suppression and
 *   resource denial. The `beneficiary` and `victim` declarations map directly to
 *   this power dynamic between the scientific establishment and its dissidents.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two common errors. It does not mistake
 *   the paradigm for a Mountain of settled fact, as ongoing observations like JWST's
 *   show it is contestable. It also does not mistake it for a pure Rope, as doing
 *   so would ignore the very real extractive costs borne by those who question it.
 *   The Tangled Rope classification acknowledges both its genuine, powerful coordination
 *   function and its coercive, suppressive side-effects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_cdm,
    'Is the "cold, non-interacting" tenet of ΛCDM fundamentally correct despite growing anomalies, or are these anomalies fatal cracks in the paradigm?',
    'Decisive null results from all next-generation WIMP direct-detection experiments, combined with consistent validation of SIDM or MOND predictions by future telescopes (e.g., Roman Space Telescope).',
    'If correct, the constraint is revealed to be a Mountain of physics (ε -> 0), and the suppression of alternatives was justified. If incorrect, it was a multi-decade, globally scoped Snare/Piton that cost billions and delayed progress.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing. Interval represents the paradigm's
% dominance from the late 90s to the era of JWST challenges.
narrative_ontology:interval(cold_dark_matter_paradigm, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε=0.48 > 0.46), so temporal tracking
% is required to model its lifecycle drift.
% T=0: ~1998 (paradigm solidifies after supernova data)
% T=5: ~2008 (LHC begins, peak hope for WIMP detection)
% T=10: ~2028 (post-JWST era, paradigm under stress from anomalies)

% Theater ratio rises as confirming the model becomes more routine and less novel.
narrative_ontology:measurement(cdm_tr_t0, cold_dark_matter_paradigm, theater_ratio, 0, 0.05).
narrative_ontology:measurement(cdm_tr_t5, cold_dark_matter_paradigm, theater_ratio, 5, 0.20).
narrative_ontology:measurement(cdm_tr_t10, cold_dark_matter_paradigm, theater_ratio, 10, 0.30).

% Extraction rises as the paradigm becomes more entrenched, its opportunity cost
% grows, and it more actively suppresses increasingly plausible alternatives.
narrative_ontology:measurement(cdm_ex_t0, cold_dark_matter_paradigm, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(cdm_ex_t5, cold_dark_matter_paradigm, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(cdm_ex_t10, cold_dark_matter_paradigm, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: It serves as the primary standard for information,
% models, and assumptions in modern cosmology.
narrative_ontology:coordination_type(cold_dark_matter_paradigm, information_standard).

% --- Network Decomposition (Constraint Families) ---
% This story focuses on the *specifics* of dark matter being 'cold'. This is
% distinct from the more fundamental constraint that *some* dark matter is needed.
% The ε-invariance principle requires decomposition.
%
% DUAL FORMULATION NOTE:
% This constraint is one of 2 stories decomposed from the colloquial label "dark matter".
% Decomposed because ε differs across observables (ε-invariance principle).
% Related stories:
%   - cosmological_dark_matter_necessity (ε≈0.05, Mountain)
%
% This story (cold_dark_matter_paradigm) is downstream from the more fundamental claim.
narrative_ontology:affects_constraint(cosmological_dark_matter_necessity, cold_dark_matter_paradigm).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The automatic derivation chain,
% using the declared beneficiary/victim groups and their exit options,
% accurately computes the directionality (d) for each perspective and captures
% the core dynamics of the scientific paradigm.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */