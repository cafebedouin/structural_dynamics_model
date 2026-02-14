% ============================================================================
% CONSTRAINT STORY: lcdm_hubble_tension
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_lcdm_hubble_tension, []).

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
 *   constraint_id: lcdm_hubble_tension
 *   human_readable: The Hubble Tension within the Lambda-CDM Cosmological Model
 *   domain: scientific
 *
 * SUMMARY:
 *   The Lambda-CDM (ΛCDM) model is the standard framework for cosmology.
 *   However, a significant discrepancy exists between the universe's expansion
 *   rate as predicted by the model from early-universe data (the CMB) and
 *   the rate measured directly in the local universe (the "distance ladder").
 *   This "Hubble Tension" acts as a powerful constraint, forcing observational
 *   data into conflict with the dominant theory and suggesting either unknown
 *   systematic errors or the need for "new physics" beyond the standard model.
 *
 * DUAL FORMULATION NOTE:
 *   This constraint is one of 2 stories decomposed from "The Lambda-CDM Model".
 *   Decomposed because ε differs across observables (ε-invariance principle).
 *   Related stories:
 *     - lcdm_cmb_concordance (ε≈0.05, Mountain): The model's success in predicting
 *       the Cosmic Microwave Background.
 *   This story focuses on the model's predictive failure for the local universe.
 *
 * KEY AGENTS (by structural relationship):
 *   - Observational Cosmologists (e.g., SH0ES team): Primary target (organized/constrained) — Their precise measurements are put into question by the model's predictive failure.
 *   - Early-Career Researchers: Secondary target (powerless/trapped) — Their careers are constrained by the conflict between the model and data, lacking the power to resolve it.
 *   - Standard Model Theorists: Primary beneficiary (institutional/arbitrage) — Benefit from the immense coordinating power of the ΛCDM framework.
 *   - "New Physics" Proponents: Secondary beneficiary (organized/mobile) — The tension creates a "market" for their alternative theories.
 *   - Analytical Observer: A philosopher or physicist viewing the entire scientific conflict.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% The discrepancy is ~8%, a significant predictive failure, thus high extraction.
domain_priors:base_extractiveness(lcdm_hubble_tension, 0.47).
% The standard model is deeply entrenched; challenging it requires an
% extraordinary burden of proof.
domain_priors:suppression_score(lcdm_hubble_tension, 0.75).
% The work is genuine science, not performance.
domain_priors:theater_ratio(lcdm_hubble_tension, 0.10).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lcdm_hubble_tension, extractiveness, 0.47).
narrative_ontology:constraint_metric(lcdm_hubble_tension, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(lcdm_hubble_tension, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(lcdm_hubble_tension, tangled_rope).

% --- Binary flags ---
% The model is "enforced" by peer review, funding, and institutional consensus.
domain_priors:requires_active_enforcement(lcdm_hubble_tension). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
narrative_ontology:constraint_beneficiary(lcdm_hubble_tension, standard_model_theorists).
narrative_ontology:constraint_victim(lcdm_hubble_tension, observational_cosmologists).

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

% PERSPECTIVE 1: THE OBSERVATIONAL COSMOLOGISTS (TARGET)
% As victims with constrained exit (they cannot simply abandon the framework,
% they must work within it), they experience high effective extraction. The
% model provides coordination but at a high cost of invalidating their data.
constraint_indexing:constraint_classification(lcdm_hubble_tension, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE STANDARD MODEL THEORISTS (BENEFICIARY)
% As beneficiaries with arbitrage exit (they can propose model tweaks), they
% experience the tension as a puzzle within a highly functional Rope.
% The derived negative extraction reflects the model's immense value to them.
constraint_indexing:constraint_classification(lcdm_hubble_tension, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The analytical view sees both the powerful coordination function and the
% severe extractive cost of the discrepancy, classifying it as a Tangled Rope.
% This is the basis for the constraint_claim.
constraint_indexing:constraint_classification(lcdm_hubble_tension, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE EARLY-CAREER RESEARCHER (POWERLESS VICTIM)
% A graduate student or postdoc whose work is caught in the crossfire. They lack
% the institutional power to challenge the standard model or the resources to
% definitively resolve the measurement discrepancy. For them, the tension is a
% career-threatening Snare, forcing them to align with one camp or the other
% under high uncertainty.
constraint_indexing:constraint_classification(lcdm_hubble_tension, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lcdm_hubble_tension_tests).

test(perspectival_gap_target_beneficiary) :-
    constraint_indexing:constraint_classification(lcdm_hubble_tension, TypeTarget, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(lcdm_hubble_tension, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    assertion(TypeTarget == snare),
    assertion(TypeBeneficiary == rope),
    assertion(TypeTarget \= TypeBeneficiary).

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(lcdm_hubble_tension, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    assertion(TypeAnalytical == tangled_rope).

test(tangled_rope_gate_requirements_met) :-
    narrative_ontology:constraint_beneficiary(lcdm_hubble_tension, _),
    narrative_ontology:constraint_victim(lcdm_hubble_tension, _),
    domain_priors:requires_active_enforcement(lcdm_hubble_tension).

:- end_tests(lcdm_hubble_tension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.47): Represents the ~8% discrepancy between the measured
 *     and predicted Hubble constant. This is a severe predictive failure, qualifying
 *     it as a high-extraction constraint.
 *   - Suppression (0.75): Reflects the immense institutional inertia and success
 *     of the ΛCDM model. Proposing alternatives or claiming systematic error in
 *     the CMB-derived value carries a very high burden of proof.
 *   - Classification (Tangled Rope): The constraint perfectly embodies this type.
 *     It provides a vital coordination function (the entire field uses ΛCDM) while
 *     simultaneously imposing a severe, asymmetric cost (the "tension") on a
 *     specific group (observationalists).
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For institutional theorists, the model is a Rope—a powerful
 *   coordination tool whose benefits vastly outweigh a frustrating anomaly.
 *   For organized observational teams, it's a Tangled Rope—a necessary framework
 *   that also actively works to invalidate their painstakingly gathered data.
 *   For powerless early-career researchers, the coordination function is overshadowed
 *   by the coercive pressure, making it a Snare that threatens their career.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries (`standard_model_theorists`): This group's professional and
 *     intellectual capital is tied to the success and stability of the ΛCDM model.
 *     They benefit from its coordinating power.
 *   - Victims (`observational_cosmologists`): This group bears the direct cost of
 *     the discrepancy. The model's prediction implicitly claims their local
 *     measurements contain unknown systematic errors, a position that requires
 *     constant defense and re-verification. Their `constrained` exit reflects that
 *     they cannot simply ignore the standard model.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification avoids two key errors. It does not label ΛCDM as a pure
 *   Snare, which would ignore its undeniable and colossal success as a coordinating
 *   framework (captured in the `lcdm_cmb_concordance` sibling story). It also
 *   avoids calling it a pure Rope, which would dismiss the very real, extractive
 *   cost the Hubble Tension imposes on a generation of observational work. The
 *   Tangled Rope classification correctly identifies it as a hybrid system with both
 *   functions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_lcdm_hubble_tension,
    'Is the Hubble Tension caused by unknown systematic errors in the distance ladder measurements, or does it require new fundamental physics beyond the standard ΛCDM model?',
    'Continued observation with next-generation instruments (JWST, Euclid, Roman Space Telescope) to reduce measurement uncertainties below the level of the discrepancy, or independent verification from other methods like gravitational waves.',
    'If due to systematics, the constraint is a Piton (a measurement error artifact). If new physics, it is a genuine Tangled Rope pointing the way to a more complete theory.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(lcdm_hubble_tension, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The tension has intensified over the last decade as error bars on both sides
% have shrunk, increasing the statistical significance and thus the "extraction".
% This models the constraint's evolution from a minor curiosity to a major crisis.
% Base extraction is > 0.46, so temporal data is required.

% Theater ratio over time (consistently low, as this is genuine research).
narrative_ontology:measurement(lcdm_hubble_tension_tr_t0, lcdm_hubble_tension, theater_ratio, 0, 0.10).
narrative_ontology:measurement(lcdm_hubble_tension_tr_t5, lcdm_hubble_tension, theater_ratio, 5, 0.10).
narrative_ontology:measurement(lcdm_hubble_tension_tr_t10, lcdm_hubble_tension, theater_ratio, 10, 0.10).

% Extraction over time (growing as the discrepancy became statistically significant).
narrative_ontology:measurement(lcdm_hubble_tension_ex_t0, lcdm_hubble_tension, base_extractiveness, 0, 0.15). % Early hints
narrative_ontology:measurement(lcdm_hubble_tension_ex_t5, lcdm_hubble_tension, base_extractiveness, 5, 0.30). % Becoming significant
narrative_ontology:measurement(lcdm_hubble_tension_ex_t10, lcdm_hubble_tension, base_extractiveness, 10, 0.47). % Current crisis

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: ΛCDM is the quintessential information standard for cosmology.
narrative_ontology:coordination_type(lcdm_hubble_tension, information_standard).

% Network relationships: The tension (this story) only exists because of the
% success of the model in other domains (the sibling story).
narrative_ontology:affects_constraint(lcdm_cmb_concordance, lcdm_hubble_tension).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The automatic derivation
% based on the declared beneficiary/victim groups and their exit options
% (arbitrage vs. constrained) accurately captures the structural dynamics
% of the scientific conflict.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */