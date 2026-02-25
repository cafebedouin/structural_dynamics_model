% ============================================================================
% CONSTRAINT STORY: lambda_cdm_research_paradigm
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lambda_cdm_research_paradigm, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: lambda_cdm_research_paradigm
 *   human_readable: Lambda-CDM as a Dominant Research Paradigm
 *   domain: cosmology/sociology_of_science
 *
 * SUMMARY:
 *   The recent confirmation of J0613+52, a galaxy composed of 99.99% dark
 *   matter, serves as powerful evidence for the Standard Model of Cosmology
 *   (Lambda-CDM). This constraint story models the social and institutional
 *   *paradigm* that this evidence reinforces, rather than the physical law
 *   itself. The Lambda-CDM model acts as a powerful constraint on the field,
 *   coordinating research efforts on a global scale while simultaneously
 *   suppressing alternative theories of gravity and cosmology. The discovery
 *   of a 'dark galaxy' strengthens the paradigm's hold, making it more
 *   difficult for competing models to gain traction.
 *
 * KEY AGENTS:
 *   - Mainstream Cosmologists: Primary beneficiaries (institutional/arbitrage) — Their research framework is validated, easing access to funding and publication.
 *   - Alternative Gravity Theorists: Primary victims (powerless/trapped) — Their competing models are further marginalized, facing increased difficulty in securing resources and academic acceptance.
 *   - Funding Agencies/Peer Review Panels: Enforcement agents (institutional/constrained) — They actively maintain the paradigm by prioritizing proposals and papers that conform to its assumptions.
 *   - Science Journalists and Public Communicators: Amplifiers — They broadcast the paradigm's successes, often presenting it to the public as settled fact (a Mountain).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lambda_cdm_research_paradigm, 0.48).
domain_priors:suppression_score(lambda_cdm_research_paradigm, 0.62).
domain_priors:theater_ratio(lambda_cdm_research_paradigm, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lambda_cdm_research_paradigm, extractiveness, 0.48).
narrative_ontology:constraint_metric(lambda_cdm_research_paradigm, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(lambda_cdm_research_paradigm, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lambda_cdm_research_paradigm, tangled_rope).
narrative_ontology:human_readable(lambda_cdm_research_paradigm, "Lambda-CDM as a Dominant Research Paradigm").
narrative_ontology:topic_domain(lambda_cdm_research_paradigm, "cosmology/sociology_of_science").

domain_priors:requires_active_enforcement(lambda_cdm_research_paradigm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lambda_cdm_research_paradigm, lambda_cdm_cosmologists).
narrative_ontology:constraint_beneficiary(lambda_cdm_research_paradigm, particle_physics_experimentalists).
narrative_ontology:constraint_beneficiary(lambda_cdm_research_paradigm, large_observatory_projects).
narrative_ontology:constraint_victim(lambda_cdm_research_paradigm, alternative_gravity_theorists).
narrative_ontology:constraint_victim(lambda_cdm_research_paradigm, proponents_of_non_dm_cosmologies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HETERODOX THEORIST (SNARE) — A researcher promoting an alternative model (e.g., MOND) faces immense structural barriers. Peer review, funding panels, and hiring committees enforce the dominant paradigm. From this view, the coordination benefit is invisible; they only experience the suppression of their work and the extraction of their career opportunities. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.82.
constraint_indexing:constraint_classification(lambda_cdm_research_paradigm, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MAINSTREAM PI (ROPE) — For a cosmologist working within the paradigm, Lambda-CDM is a powerful coordination tool. It provides a shared language, well-defined problems, and a clear framework for interpreting data like that from J0613+52. This structure facilitates large-scale collaboration and directs funding efficiently. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.07.
constraint_indexing:constraint_classification(lambda_cdm_research_paradigm, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (TANGLED ROPE) — This observer sees both the immense coordination function that allows the field of cosmology to advance (the Rope) and the simultaneous suppression of alternative viewpoints and extraction from their proponents (the Snare). The discovery of J0613+52 is seen as an event that tightens the Tangled Rope, reinforcing both functions.
constraint_indexing:constraint_classification(lambda_cdm_research_paradigm, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE LAYPERSON (MOUNTAIN) — Through popular science media, the existence of dark matter is presented as a confirmed, unchangeable fact of nature. The nuances of paradigm debate are flattened, and the model is perceived as a fixed, natural law. This is a classic 'false summit' classification, mistaking a dominant but contingent theory for physical bedrock.
constraint_indexing:constraint_classification(lambda_cdm_research_paradigm, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lambda_cdm_research_paradigm_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lambda_cdm_research_paradigm, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lambda_cdm_research_paradigm, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(lambda_cdm_research_paradigm, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lambda_cdm_research_paradigm_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.48) represents the significant opportunity cost imposed on alternative research programs, which are starved of funding, talent, and publications. Suppression (0.62) reflects the high structural barriers to entry for non-standard cosmological models within elite journals and institutions. The paradigm requires active enforcement through peer review and funding allocation. The base extractiveness has steadily increased since the early 90s as confirming evidence (like COBE, WMAP, Planck, and now J0613+52) has mounted, solidifying the consensus.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. An insider (mainstream PI) experiences a highly effective coordination mechanism (Rope) that enables progress. An outsider (heterodox theorist) experiences an extractive and coercive trap (Snare) that stifles their career. The public, receiving a filtered summary, perceives an unassailable law of nature (Mountain). The analytical observer identifies the structure as a Tangled Rope, acknowledging both its immense coordinative function and its coercive, extractive consequences for minority scientific views.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (mainstream cosmologists) work within a system that subsidizes their research; with high exit options (within the paradigm), they perceive negative extraction (Rope). Victims (alternative theorists) are structurally targeted by the system's enforcement mechanisms; with trapped career options, they perceive high positive extraction (Snare). This perspectival divergence is a hallmark of a Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This story correctly avoids the mandatrophy of mislabeling a dominant scientific paradigm as a pure Mountain. While it is based on strong evidence pointing towards a potential physical law, the social structure built around it has clear coordinative and extractive features. By classifying it as a Tangled Rope, the framework captures the reality that even in science, consensus can function as a coercive constraint that extracts a cost from those outside it, preventing the false naturalization of a contingent social structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    paradigm_vs_reality,
    'Is the Lambda-CDM paradigm dominant because it correctly describes physical reality (a Mountain), or due to sociological path dependence and institutional inertia?',
    'A future observation that decisively falsifies a core prediction of the Lambda-CDM model which cannot be explained with minor adjustments.',
    'If falsified, the paradigm would rapidly degrade from a Tangled Rope to a Piton, creating a vacuum for a new coordinative Rope to form around a successor theory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paradigm_vs_reality, empirical, 'Whether the paradigm''s dominance reflects truth or sociological lock-in').

omega_variable(
    dm_particle_nature,
    'What is the physical nature of the dark matter particle or field?',
    'Direct or indirect detection of a WIMP, axion, or other candidate particle, or definitive evidence for a different physical origin.',
    'Direct detection of a specific particle would massively reinforce the existing paradigm, likely increasing both its suppression and extractiveness by invalidating entire classes of alternative theories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dm_particle_nature, empirical, 'The unknown physical identity of dark matter').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lambda_cdm_research_paradigm, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lamb_tr_t1992, lambda_cdm_research_paradigm, theater_ratio, 1992, 0.1).
narrative_ontology:measurement(lamb_tr_t2013, lambda_cdm_research_paradigm, theater_ratio, 2013, 0.15).
narrative_ontology:measurement(lamb_tr_t2024, lambda_cdm_research_paradigm, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(lamb_be_t1992, lambda_cdm_research_paradigm, base_extractiveness, 1992, 0.25).
narrative_ontology:measurement(lamb_be_t2003, lambda_cdm_research_paradigm, base_extractiveness, 2003, 0.35).
narrative_ontology:measurement(lamb_be_t2013, lambda_cdm_research_paradigm, base_extractiveness, 2013, 0.42).
narrative_ontology:measurement(lamb_be_t2024, lambda_cdm_research_paradigm, base_extractiveness, 2024, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lambda_cdm_research_paradigm, information_standard).

% DUAL FORMULATION NOTE:
% This constraint models the social and institutional paradigm of Lambda-CDM. It is structurally downstream of the physical claim `lambda_cdm_physical_law` (ε≈0.05, Mountain), which posits dark matter as a real physical entity. The high empirical confidence in the physical claim is what grants the social paradigm its coercive and coordinative power.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
