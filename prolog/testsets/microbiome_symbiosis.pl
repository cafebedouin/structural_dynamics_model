% ============================================================================
% CONSTRAINT STORY: microbiome_symbiosis
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_microbiome_symbiosis, []).

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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: microbiome_symbiosis
 *   human_readable: The Microbial-Immune Symbiosis
 *   domain: biological/healthcare
 *
 * SUMMARY:
 *   The relationship between the human host and their gut microbiome is a
 *   fundamental biological constraint. In a state of symbiosis, it provides
 *   essential coordination for metabolism and immunity. In a state of dysbiosis,
 *   it becomes a source of chronic illness and systemic extraction. Modern
 *   science has reframed this from an unknowable fate to a manipulable system,
 *   revealing its dual nature as both a coordination mechanism and a potential trap.
 *
 * KEY AGENTS (by structural relationship):
 *   - Human Host (in dysbiosis): Primary target (powerless/trapped) — suffers from chronic illness, where their own internal ecosystem extracts from their health.
 *   - Human Host (in symbiosis): Primary beneficiary (powerless/mobile) — benefits from metabolic and immune coordination.
 *   - Modern Bio-Tech Company: Secondary beneficiary (institutional/arbitrage) — leverages knowledge of the system to provide coordination tools (probiotics, testing) for profit.
 *   - Analytical Observer: Sees the full dual-function structure (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: Microbes influence drug efficacy and metabolic flow. There is a
% net flow of information and energy between host and microbe. The 0.45 value
% reflects the significant potential for extraction in a dysbiotic state.
domain_priors:base_extractiveness(microbiome_symbiosis, 0.45).

% Rationale: For centuries, the "germ theory" model of health suppressed the
% recognition of this deep symbiotic influence, treating all microbes as hostile.
domain_priors:suppression_score(microbiome_symbiosis, 0.65).

% Rationale: The system is primarily functional, but wellness marketing can
% introduce theatrical elements (e.g., non-efficacious probiotics).
domain_priors:theater_ratio(microbiome_symbiosis, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(microbiome_symbiosis, extractiveness, 0.45).
narrative_ontology:constraint_metric(microbiome_symbiosis, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(microbiome_symbiosis, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
% Rationale: From a pre-scientific perspective, the effects of the microbiome
% were an immutable part of nature. The underlying mechanisms were completely
% inaccessible (collapse=0.95), and meaningful resistance was incoherent (resistance=0.05).
% These are required because emerges_naturally/1 is true and one perspective is mountain.
narrative_ontology:constraint_metric(microbiome_symbiosis, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(microbiome_symbiosis, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(microbiome_symbiosis, tangled_rope).
narrative_ontology:human_readable(microbiome_symbiosis, "The Microbial-Immune Symbiosis").
narrative_ontology:topic_domain(microbiome_symbiosis, "biological/healthcare").

% --- Binary flags ---
% Rationale: In a state of dysbiosis, the biological feedback loops become
% coercive, "enforcing" the chronic illness. This is a natural, not social,
% enforcement, but it is required for the Tangled Rope classification.
domain_priors:requires_active_enforcement(microbiome_symbiosis).

% --- Emergence flag (required for mountain constraints) ---
% Rationale: The symbiotic relationship is a product of co-evolution, not human design.
domain_priors:emerges_naturally(microbiome_symbiosis).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(microbiome_symbiosis, human_host).
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(microbiome_symbiosis, human_host).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE ANCIENT ROMAN (MOUNTAIN)
% To ancient humans, the effects of gut health were an immutable part of nature's
% 'Mountain'—uncontrollable, mysterious, and a matter of fate or natural law.
constraint_indexing:constraint_classification(microbiome_symbiosis, mountain,
    context(agent_power(powerless),
            time_horizon(historical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MODERN BIO-TECH COMPANY (ROPE)
% For modern medicine and commerce, the microbiome is a 'Rope'—a functional
% mechanism that can be measured and manipulated (via diet, transplants) to
% coordinate health outcomes. It's a powerful and profitable tool.
constraint_indexing:constraint_classification(microbiome_symbiosis, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: PATIENT WITH CHRONIC ILLNESS (SNARE)
% When microbial-immune communication fails (dysbiosis), it can trigger
% debilitating conditions. The symbiotic constraint becomes a 'Snare' where
% the host's internal ecosystem actively harms them, creating a trap of chronic illness.
constraint_indexing:constraint_classification(microbiome_symbiosis, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The observer sees the complete picture: a system with a vital coordination
% function (Rope) that also has a failure mode of severe, asymmetric extraction
% (Snare), enforced by biological feedback loops. This dual nature is the
% definition of a Tangled Rope.
constraint_indexing:constraint_classification(microbiome_symbiosis, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(microbiome_symbiosis_tests).

test(perspectival_gap) :-
    % Verify that the constraint is seen differently by those trapped and those with power.
    constraint_indexing:constraint_classification(microbiome_symbiosis, TypeTarget, context(agent_power(powerless), immediate, trapped, _)),
    constraint_indexing:constraint_classification(microbiome_symbiosis, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    (TypeTarget == snare ; TypeTarget == mountain),
    TypeBeneficiary == rope,
    TypeTarget \= TypeBeneficiary.

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(microbiome_symbiosis, tangled_rope, context(agent_power(analytical), _, _, _)).

:- end_tests(microbiome_symbiosis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The scores reflect the dual nature of the microbiome. Base extractiveness (0.45)
 *   and suppression (0.65) are high because dysbiosis is a severe extractive
 *   state, and the historical "germ theory" paradigm actively suppressed
 *   understanding of symbiosis. The constraint `emerges_naturally` but also
 *   `requires_active_enforcement` because the "enforcement" is the coercive
 *   biological feedback loop of chronic illness, a natural process.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For a pre-scientific human, it's an unchangeable
 *   Mountain. For a modern bio-tech firm, it's a Rope for coordinating health.
 *   For a patient with a chronic illness linked to dysbiosis, it's a Snare
 *   from which there is no easy escape. The analytical view must synthesize
 *   these realities, leading to the Tangled Rope classification.
 *
 * DIRECTIONALITY LOGIC:
 *   The `human_host` is declared as both beneficiary and victim. This is the
 *   core of the Tangled Rope. In a state of symbiosis (health), the host is the
 *   beneficiary. In a state of dysbiosis (illness), the host becomes the victim
 *   of their own internal ecosystem. The directionality of extraction is
 *   contingent on the state of the system.
 *
 * MANDATROPHY ANALYSIS:
 *   This case shows how a single biological system can be mislabeled. Viewing
 *   it only through the lens of a healthy person misses the Snare component.
 *   Viewing it only through the lens of a sick person misses the essential
 *   Rope (coordination) function. The Tangled Rope classification correctly
 *   captures this duality, preventing the oversimplification that would come
 *   from labeling it purely as a Rope or a Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% The core uncertainty is the direction of causality in chronic illness.
omega_variable(
    omega_microbiome_symbiosis,
    "Does microbial dysfunction cause conditions like Parkinson's, or is it a downstream symptom of host failure?",
    "Longitudinal studies tracking microbial shifts prior to symptomatic onset in at-risk populations.",
    "If cause: The 'Snare' is a primary driver of the illness. If symptom: It's merely a biomarker of an underlying 'Mountain' of host biology.",
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(omega_microbiome_symbiosis, empirical, "Distinguishing microbial dysfunction as a cause versus a symptom of chronic illness.").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(microbiome_symbiosis, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The "extraction" of the microbiome constraint has increased over time as
% modern lifestyles, diets, and antibiotic use have promoted dysbiosis,
% turning a largely symbiotic relationship into a more frequently extractive one.
% Theater remains low as the effects are primarily biological, not performative.

% Theater ratio over time:
narrative_ontology:measurement(microbiome_symbiosis_tr_t0, microbiome_symbiosis, theater_ratio, 0, 0.10).
narrative_ontology:measurement(microbiome_symbiosis_tr_t5, microbiome_symbiosis, theater_ratio, 5, 0.15).
narrative_ontology:measurement(microbiome_symbiosis_tr_t10, microbiome_symbiosis, theater_ratio, 10, 0.22).

% Extraction over time:
narrative_ontology:measurement(microbiome_symbiosis_ex_t0, microbiome_symbiosis, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(microbiome_symbiosis_ex_t5, microbiome_symbiosis, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(microbiome_symbiosis_ex_t10, microbiome_symbiosis, base_extractiveness, 10, 0.45).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% No Boltzmann or Network data for this constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed; the structural derivation from beneficiary/victim is sufficient.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */