% ============================================================================
% CONSTRAINT STORY: alzheimers_nlrp3_inflammasome
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_alzheimers_nlrp3_inflammasome, []).

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
 *   constraint_id: alzheimers_nlrp3_inflammasome
 *   human_readable: Alzheimer's Disease Pathogenesis via NLRP3 Inflammasome
 *   domain: medical/biological
 *
 * SUMMARY:
 *   This constraint models the pathological role of the NLRP3 inflammasome, a
 *   component of the innate immune system, in driving the neuroinflammation
 *   and neuronal death characteristic of Alzheimer's Disease (AD). While the
 *   pathway is a natural biological mechanism, its chronic activation by
 *   AD-related proteins like amyloid-beta creates a self-perpetuating cycle
 *   of damage. The classification of this constraint is highly dependent on
 *   the observer's structural position: it is simultaneously a fundamental
 *   law of biology to a researcher, a solvable technical challenge to a drug
 *   company, and a devastating, life-destroying trap to a patient.
 *
 * KEY AGENTS:
 *   - Patients and Families: Primary victims (powerless/trapped) — bear the full, irreversible cost of the disease's progression.
 *   - Neuronal Cells: The direct biological target of the extractive process.
 *   - Pharmaceutical Developers: Institutional beneficiaries (institutional/arbitrage) — the existence of a clear, targetable pathway creates a market for therapeutic intervention.
 *   - Public Health Systems: Organized victims (organized/constrained) — must manage the societal costs of care and lost productivity.
 *   - Analytical Researchers: Observers (analytical/analytical) — seek to understand the mechanism as a natural process.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(alzheimers_nlrp3_inflammasome, 0.72).
domain_priors:suppression_score(alzheimers_nlrp3_inflammasome, 0.95).
domain_priors:theater_ratio(alzheimers_nlrp3_inflammasome, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(alzheimers_nlrp3_inflammasome, extractiveness, 0.72).
narrative_ontology:constraint_metric(alzheimers_nlrp3_inflammasome, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(alzheimers_nlrp3_inflammasome, theater_ratio, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(alzheimers_nlrp3_inflammasome, tangled_rope).
narrative_ontology:human_readable(alzheimers_nlrp3_inflammasome, "Alzheimer's Disease Pathogenesis via NLRP3 Inflammasome").
narrative_ontology:topic_domain(alzheimers_nlrp3_inflammasome, "medical/biological").

domain_priors:requires_active_enforcement(alzheimers_nlrp3_inflammasome).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(alzheimers_nlrp3_inflammasome, pharmaceutical_developers).
narrative_ontology:constraint_beneficiary(alzheimers_nlrp3_inflammasome, innate_immune_system_signaling).
narrative_ontology:constraint_victim(alzheimers_nlrp3_inflammasome, patients_and_families).
narrative_ontology:constraint_victim(alzheimers_nlrp3_inflammasome, neuronal_cells).
narrative_ontology:constraint_victim(alzheimers_nlrp3_inflammasome, public_health_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE PATIENT (SNARE) — Experiences the biological pathway as a purely extractive process, removing cognitive function, identity, and life. They are trapped within their own biology with no exit. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.82. This is a clear Snare.
constraint_indexing:constraint_classification(alzheimers_nlrp3_inflammasome, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PHARMACEUTICAL DEVELOPER (ROPE) — The well-defined biological pathway is a valuable target. It coordinates research, investment, and drug development. For this agent, the constraint is a problem to be solved, and its existence enables a multi-billion dollar industry. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.12. The negative effective extraction signifies a net beneficiary.
constraint_indexing:constraint_classification(alzheimers_nlrp3_inflammasome, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: PUBLIC HEALTH SYSTEM (TANGLED ROPE) — Faces a dual problem: coordinating a massive care and resource allocation effort for millions of patients (coordination function) while bearing the immense, unrecoverable costs of the disease (extractive function). It is constrained by the biological reality of the disease. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.54.
constraint_indexing:constraint_classification(alzheimers_nlrp3_inflammasome, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — Views the NLRP3 pathway as a fundamental, unchangeable law of biology. From this perspective, it's a mechanism to be understood, not a system of extraction. However, the engine will flag this as a 'false summit': the high base extractiveness (ε=0.72) and suppression (0.95) are inconsistent with a true Mountain, revealing that what appears as natural law is, in its effect, a highly coercive system.
constraint_indexing:constraint_classification(alzheimers_nlrp3_inflammasome, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(alzheimers_nlrp3_inflammasome_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(alzheimers_nlrp3_inflammasome, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(alzheimers_nlrp3_inflammasome, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(alzheimers_nlrp3_inflammasome, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(alzheimers_nlrp3_inflammasome_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.72) is very high, representing the severe and irreversible loss of cognitive function and neuronal life caused by the chronic neuroinflammatory process. Suppression (0.95) is near-total, as there are no biological alternatives for a neuron to 'opt out' of the inflammatory environment. The 'requires_active_enforcement' flag is true because the continuous presence of pathological proteins like Aβ and tau actively triggers and sustains the inflammasome's activation. The theater ratio (0.10) is low, as this is a direct and non-performative biological process.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For a patient, the disease is a Snare (χ≈0.82), a pure extraction of self. For a pharmaceutical company, the same mechanism is a Rope (χ≈-0.12), a coordination point for R&D that promises immense profit. The most significant gap is with the analytical observer, who frames the mechanism as a Mountain—a law of nature. The system's 'false summit' detection mechanism is critical here: it uses the high base metrics (ε=0.72) to show that this 'Mountain' is an illusion that naturalizes a deeply extractive process. What one agent sees as an object of study, another experiences as a trap.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the clear victim/beneficiary structure. Patients and their families are the ultimate victims with no exit, leading to a high directionality (d≈0.95) and the Snare classification. Pharmaceutical developers are clear beneficiaries who can choose to invest or not (arbitrage), leading to a low, even negative, directionality (d≈0.05) and the Rope classification. The public health system is a victim but has organizational capacity, placing it in the middle as a Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This case resolves the mandatrophy by demonstrating that a single, high-extraction biological process can be correctly classified as a Snare, a Rope, and a (false) Mountain simultaneously. The resolution is not to pick one 'correct' type, but to recognize that the classification is an indexical property of the observer's relationship to the constraint. The framework correctly identifies the patient's experience as a Snare while also modeling the pharma company's experience as a Rope, preventing the mislabeling of a devastating disease as a mere 'coordination problem' from the victim's perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causality_vs_correlation,
    'Is chronic NLRP3 inflammasome activation a primary cause of Alzheimer''s neurodegeneration, or a downstream consequence of other pathologies like amyloid-beta accumulation?',
    'Longitudinal human studies with early-stage intervention trials targeting NLRP3. If inhibition halts cognitive decline, causality is strongly supported.',
    'If causal, the Snare classification is correct. If merely correlational, the constraint is misidentified; NLRP3 is a symptom, and the true Snare lies further upstream (e.g., in Aβ production).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causality_vs_correlation, empirical, 'Distinguishing whether NLRP3 activation is a root cause or a symptom of AD.').

omega_variable(
    therapeutic_tractability,
    'Can the NLRP3 pathway be safely and effectively inhibited in humans over long periods without compromising essential immune functions?',
    'Completion of Phase II and III clinical trials for NLRP3 inhibitors, assessing both efficacy in slowing AD and long-term safety profiles.',
    'If tractable, the constraint transforms into a Scaffold for patients (a temporary problem being managed). If intractable due to side effects, it remains a hard Snare/Mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(therapeutic_tractability, empirical, 'Whether NLRP3 inhibitors are a viable long-term therapy for AD.').

omega_variable(
    off_target_effects,
    'What are the systemic consequences of downregulating a core component of the innate immune system in an elderly population?',
    'Post-market surveillance and long-term observational studies of patients on NLRP3 inhibitors.',
    'If off-target effects are severe (e.g., increased susceptibility to infections), the ''solution'' creates a new Snare, trading one disease for another. If manageable, the Rope/Scaffold view holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(off_target_effects, empirical, 'Unintended consequences of long-term NLRP3 inhibition in the elderly.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(alzheimers_nlrp3_inflammasome, 2001, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(alzh_tr_t0, alzheimers_nlrp3_inflammasome, theater_ratio, 0, 0.05).
narrative_ontology:measurement(alzh_tr_t12, alzheimers_nlrp3_inflammasome, theater_ratio, 12, 0.08).
narrative_ontology:measurement(alzh_tr_t25, alzheimers_nlrp3_inflammasome, theater_ratio, 25, 0.1).

% Extraction over time
narrative_ontology:measurement(alzh_be_t0, alzheimers_nlrp3_inflammasome, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(alzh_be_t12, alzheimers_nlrp3_inflammasome, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(alzh_be_t25, alzheimers_nlrp3_inflammasome, base_extractiveness, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(alzheimers_nlrp3_inflammasome, enforcement_mechanism).
narrative_ontology:affects_constraint(alzheimers_nlrp3_inflammasome, amyloid_beta_cascade).
narrative_ontology:affects_constraint(alzheimers_nlrp3_inflammasome, tau_pathology_propagation).

% DUAL FORMULATION NOTE:
% This constraint is downstream of the mechanisms that produce amyloid-beta and tau pathologies. While those constraints have their own ε values related to protein misfolding, this constraint's ε=0.72 specifically measures the extractive damage caused by the immune system's response to them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
