% ============================================================================
% CONSTRAINT STORY: ape_cognition_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ape_cognition_framework, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ape_cognition_framework
 *   human_readable: The scientific and ethical framework defining the limits of ape cognition
 *   domain: social/scientific
 *
 * SUMMARY:
 *   The framework governing research on great ape cognition is a complex
 *   social construct blending scientific inquiry with evolving ethical norms.
 *   It functions to coordinate research but does so by enabling the study of
 *   a powerless, non-consenting group. This creates a structural conflict
 *   where the same set of rules can be perceived as a benign coordination
 *   tool, a coercive trap, a degraded ritual, or a natural boundary,
 *   depending on the observer's position relative to the flow of extraction.
 *
 * KEY AGENTS:
 *   - Great Apes (as a class): Primary victim (powerless/trapped) — the subjects of study.
 *   - Research Institutions: Primary beneficiary (institutional/arbitrage) — gain knowledge, funding, and prestige.
 *   - Mainstream Researchers: Beneficiary (institutional/arbitrage) — operate within the framework to build careers.
 *   - Ethics Committees: Institutional actor (institutional/constrained) — enforce the framework, often performatively.
 *   - Animal Rights Advocates: Organized opposition (organized/constrained) — seek to dismantle or reform the framework.
 *   - Analytical Observers: External analysts (analytical/analytical) — classify the structure from different philosophical premises.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ape_cognition_framework, 0.55).
domain_priors:suppression_score(ape_cognition_framework, 0.75).
domain_priors:theater_ratio(ape_cognition_framework, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ape_cognition_framework, extractiveness, 0.55).
narrative_ontology:constraint_metric(ape_cognition_framework, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ape_cognition_framework, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ape_cognition_framework, tangled_rope).
narrative_ontology:human_readable(ape_cognition_framework, "The scientific and ethical framework defining the limits of ape cognition").
narrative_ontology:topic_domain(ape_cognition_framework, "social/scientific").

domain_priors:requires_active_enforcement(ape_cognition_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ape_cognition_framework, research_institutions).
narrative_ontology:constraint_beneficiary(ape_cognition_framework, human_society).
narrative_ontology:constraint_victim(ape_cognition_framework, great_apes_as_a_class).
narrative_ontology:constraint_victim(ape_cognition_framework, dissenting_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE GREAT APE (SNARE) — Experiences the framework as a justification for confinement, coercive study, and the removal of autonomy. As the primary victim with no exit, the effective extraction is maximized. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.62. This χ is just below the snare threshold of 0.66, but the high suppression (0.75) and victim status make Snare the most fitting classification.
constraint_indexing:constraint_classification(ape_cognition_framework, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE MAINSTREAM RESEARCHER (ROPE) — Experiences the framework as a pure coordination mechanism that enables funding, ethical approval, and peer-reviewed publication. As a primary beneficiary with arbitrage, extraction is negative. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.08.
constraint_indexing:constraint_classification(ape_cognition_framework, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANIMAL RIGHTS ADVOCATE (TANGLED ROPE) — Sees both the coordinating function (a shared language for debate) and the severe extraction from apes. As an organized but constrained agent, they perceive a system with both utility and high coercive cost. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.50.
constraint_indexing:constraint_classification(ape_cognition_framework, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ETHICS COMMITTEE (PITON) — The function of ensuring 'ethical' research has degraded into a bureaucratic, liability-mitigating ritual. The high theater_ratio (0.75) meets the Piton gate. The committee maintains the framework out of institutional inertia, not a belief in its current functional efficacy.
constraint_indexing:constraint_classification(ape_cognition_framework, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HUMAN EXCEPTIONALISM (MOUNTAIN) — This philosophical stance frames the cognitive divide between humans and apes as a fixed, natural law. The framework is seen merely as the process of discovering this immutable boundary. The engine will flag this as a false summit, as the high base extractiveness and suppression are inconsistent with a natural law.
constraint_indexing:constraint_classification(ape_cognition_framework, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER (TANGLED ROPE) — The default analytical view recognizes all components: a genuine coordination function for science, asymmetric extraction from non-consenting subjects, and active enforcement by institutions. This matches the canonical definition of a Tangled Rope. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(ape_cognition_framework, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ape_cognition_framework_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ape_cognition_framework, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ape_cognition_framework, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ape_cognition_framework, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ape_cognition_framework, TR),
    TR >= 0.70.

:- end_tests(ape_cognition_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Represents the significant, non-monetary extraction of autonomy, natural habitat, and bodily integrity from apes for human knowledge. Suppression (0.75): High. Operating outside this framework is nearly impossible due to universal requirements for ethical review (IACUC/IRB), funding agency rules, and journal publication standards. Theater Ratio (0.75): High. Much of the ethical review process has become a bureaucratic checklist to mitigate legal liability rather than a deep philosophical engagement with the ethics of the research, making it highly performative.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the researcher, the framework is a Rope enabling science. For the ape, it is a Snare justifying captivity. For the ethics committee, it is a Piton of institutional inertia. For a philosopher of human exceptionalism, it is a Mountain—a natural law being discovered. For an advocate, and for the final analytical view, it is a Tangled Rope, acknowledging both its coordinating function and its deeply extractive nature. This diversity demonstrates that the constraint's 'type' is not a monolithic property but an indexical one.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (researchers, institutions) have arbitrage exit and low 'd' values, perceiving a Rope. Victims (apes) are trapped, leading to a high 'd' and the perception of a Snare. Constrained actors (advocates, committees) fall in between. The analytical 'Mountain' perspective is a framing choice, not a structural reality, and is revealed as a false summit by the system's metrics.
 *
 * MANDATROPHY ANALYSIS:
 *   This story resolves the mandatrophy by showing that a single, coherent set of base properties (ε=0.55, S=0.75, T=0.75) can and does generate classifications across the entire DR spectrum. The question is not 'Which type is it?' but 'From which structural position is it being observed?'. The framework is simultaneously a Rope to its beneficiaries and a Snare to its victims; its analysis requires acknowledging the validity of all perspectives derived from the underlying structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consciousness_qualia,
    'Is the subjective experience (qualia) of a great ape qualitatively distinct from or merely quantitatively different to that of a human?',
    'A theoretical and empirical breakthrough in the science of consciousness, likely requiring novel neuro-imaging or analytical techniques.',
    'If qualitatively distinct, it might reinforce the existing framework. If merely quantitatively different, it would undermine the ethical basis for extraction, shifting the classification toward Snare from more perspectives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consciousness_qualia, conceptual, 'Whether ape consciousness is qualitatively or quantitatively different.').

omega_variable(
    non_invasive_methods,
    'Can non-invasive methods (e.g., passive observation, advanced neuro-imaging) fully replace the knowledge gained from coercive or captive studies?',
    'Longitudinal studies comparing the data yield and scientific impact of non-invasive vs. traditional research programs.',
    'If yes, the justification for suppression and extraction collapses, and the constraint could evolve into a pure Rope. If no, the Tangled Rope structure remains stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_invasive_methods, empirical, 'Sufficiency of non-invasive research methods to replace coercive ones.').

omega_variable(
    personhood_threshold,
    'What specific, measurable cognitive abilities would be sufficient to grant great apes legal personhood or a protected ''rights-bearing'' status?',
    'Legal and philosophical debate informed by scientific findings, culminating in legislative or judicial precedent.',
    'Defining a clear threshold would resolve the central ambiguity. It would either codify the current Tangled Rope or dismantle it in favor of a new framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(personhood_threshold, preference, 'The societal and legal threshold for granting legal personhood.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ape_cognition_framework, 1966, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ape__tr_t0, ape_cognition_framework, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ape__tr_t30, ape_cognition_framework, theater_ratio, 30, 0.5).
narrative_ontology:measurement(ape__tr_t60, ape_cognition_framework, theater_ratio, 60, 0.75).

% Extraction over time
narrative_ontology:measurement(ape__be_t0, ape_cognition_framework, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ape__be_t30, ape_cognition_framework, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(ape__be_t60, ape_cognition_framework, base_extractiveness, 60, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ape_cognition_framework, information_standard).
narrative_ontology:affects_constraint(ape_cognition_framework, animal_welfare_legislation).
narrative_ontology:affects_constraint(ape_cognition_framework, funding_for_basic_science).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
