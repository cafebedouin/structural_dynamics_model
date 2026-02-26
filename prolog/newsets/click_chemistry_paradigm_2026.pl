% ============================================================================
% CONSTRAINT STORY: click_chemistry_paradigm_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_click_chemistry_paradigm_2026, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: click_chemistry_paradigm_2026
 *   human_readable: Click Chemistry Paradigm
 *   domain: scientific/technological
 *
 * SUMMARY:
 *   Click chemistry, a term coined by K. Barry Sharpless in 2001, is a
 *   chemical philosophy that favors simple, reliable, and modular reactions.
 *   Instead of complex, low-yield traditional synthesis, it uses a small set
 *   of powerful, 'spring-loaded' reactions that work reliably in diverse
 *   conditions. This constraint represents the adoption of this paradigm,
 *   which functions as a powerful coordination mechanism for the global
 *   chemistry community, enabling faster drug discovery, novel materials
 *   science, and simplified chemical biology.
 *
 * KEY AGENTS:
 *   - Research Chemists & Industry: Primary beneficiaries (institutional/arbitrage) — gain a powerful, efficient toolkit that accelerates research and development.
 *   - Traditional Synthesis Specialists: Structural victims (moderate/constrained) — possess deep expertise in older, more complex methods that are partially obsoleted by the new paradigm.
 *   - Analytical Observer: Sees the full structure as a coordination standard.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(click_chemistry_paradigm_2026, 0.05).
domain_priors:suppression_score(click_chemistry_paradigm_2026, 0.1).
domain_priors:theater_ratio(click_chemistry_paradigm_2026, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(click_chemistry_paradigm_2026, extractiveness, 0.05).
narrative_ontology:constraint_metric(click_chemistry_paradigm_2026, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(click_chemistry_paradigm_2026, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(click_chemistry_paradigm_2026, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(click_chemistry_paradigm_2026, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(click_chemistry_paradigm_2026, rope).
narrative_ontology:human_readable(click_chemistry_paradigm_2026, "Click Chemistry Paradigm").
narrative_ontology:topic_domain(click_chemistry_paradigm_2026, "scientific/technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(click_chemistry_paradigm_2026, research_chemists).
narrative_ontology:constraint_beneficiary(click_chemistry_paradigm_2026, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(click_chemistry_paradigm_2026, materials_scientists).
narrative_ontology:constraint_victim(click_chemistry_paradigm_2026, traditional_synthesis_specialists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PHARMACEUTICAL DEVELOPER (ROPE) — Experiences the paradigm as a pure coordination good. It lowers R&D costs, accelerates discovery, and standardizes production. Exit is trivial (use other methods), and they are a primary beneficiary. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.01. Negative extraction signifies a net subsidy.
constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: DISPLACED SPECIALIST (ROPE) — An expert in older, more complex synthesis methods whose skills are devalued. They are a structural 'victim' of the paradigm shift. However, because base extractiveness (ε) is so low, the constraint is still a Rope even from their perspective. The harm is from creative destruction, not from extraction inherent in the constraint. d≈0.75, f(d)≈1.10, σ=1.0 → χ≈0.06. The positive chi reflects the real cost of re-tooling, but it's far below the Tangled Rope threshold.
constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (ROPE) — The default analytical view correctly identifies the paradigm as a low-extraction, low-suppression coordination mechanism that solves collective action problems in synthesis. It is a canonical example of a Rope. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.07.
constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: 'NATURAL LAW' PROPONENT (MOUNTAIN - FALSE SUMMIT) — This perspective frames the paradigm as the 'one true way' to do synthesis, an inevitable and optimal endpoint. The engine will flag this as a false summit. The base properties (ε=0.05, suppression=0.10, emerges_naturally=false, resistance=0.80) fail the Mountain classification gates, revealing that this is a contingent technological choice, not a fundamental law of nature.
constraint_indexing:constraint_classification(click_chemistry_paradigm_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(click_chemistry_paradigm_2026_tests).
:- end_tests(click_chemistry_paradigm_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.05): Extremely low. The paradigm is an open-source set of principles and reactions. It does not inherently extract value; it creates it by increasing efficiency. Suppression (0.10): Very low. Adoption is voluntary and merit-based. Chemists are free to use other methods, but the effectiveness of click chemistry for certain applications creates competitive pressure. Theater Ratio (0.08): Very low. The paradigm is highly functional; its value is in the successful synthesis of molecules, not in performative ritual.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is minimal, demonstrating a uniform-type constraint. Nearly all actors experience it as a Rope. The most significant 'victim'—a specialist in older methods—still classifies it as a Rope because the base extractiveness is too low to constitute a Tangled Rope or Snare. Their personal cost is real but stems from technological displacement, not from coercive extraction by the constraint itself. The only major perspectival deviation is the 'false summit' Mountain classification, where an observer mistakes a highly effective convention for a law of nature. The engine's metrics correctly identify this as a misclassification.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (chemists, industry) have arbitrage exit and benefit directly, leading to a low 'd' value and negative effective extraction (χ < 0), signifying a net subsidy. The structural victims (displaced specialists) have constrained exit and bear the costs of the paradigm shift, leading to a higher 'd' value and a positive, but still very low, effective extraction (χ > 0). This correctly models the asymmetric costs of technological adoption without mislabeling it as a coercive or extractive system.
 *
 * MANDATROPHY ANALYSIS:
 *   This story is a canonical example of a Rope, preventing mandatrophy by correctly identifying a beneficial scientific paradigm as a coordination mechanism. It avoids the error of classifying the creative destruction inherent in technological progress as a form of extraction. The low base metrics (ε and suppression) ensure that even from the perspective of those negatively impacted, the constraint's fundamental nature as a coordination tool is preserved. It demonstrates that not all structural change that produces winners and losers is a Tangled Rope or Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(click_chemistry_paradigm_2026, 2001, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(click_chemistry_paradigm_2026, information_standard).
narrative_ontology:affects_constraint(click_chemistry_paradigm_2026, drug_discovery_pipelines).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
