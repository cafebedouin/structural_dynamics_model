% ============================================================================
% CONSTRAINT STORY: competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: competence_reading
 *   human_readable: Competence Reading: Live Exercised Knowledge in Disaster Preparedness
 *   domain: governance/disaster_preparedness/institutional_memory
 *
 * SUMMARY:
 *   The competence reading instantiates preparedness as a *functional
 *   coordination mechanism for preserving live knowledge*. In this reading,
 *   drills and inspections are low-extraction activities whose primary
 *   function is maintaining operational capacity through practice and
 *   organizational memory reinforcement. The constraint operates in
 *   institutional contexts (fire departments, civil protection agencies,
 *   emergency management) where procedural knowledge degrades without
 *   continuous exercise. The reading emphasizes that competence is *lived
 *   practice*, not ceremonial compliance—the boundary between functional
 *   drill and performative theater is observable and empirically measurable.
 *   This reading coexists with two sibling readings: the husk_reading (which
 *   frames preparedness as ceremony masking competence decay) and the
 *   hybrid_reading (which sees allocation dynamics between functional and
 *   performative components). The competence reading is distinct in
 *   prioritizing the functional component and treating theater as a secondary
 *   cost rather than the primary mechanism.
 *
 * KEY AGENTS:
 *   - Operational Personnel (organized/mobile): Primary beneficiaries and coordinators—experience drills as competence-maintenance mechanisms aligned with their own operational capacity needs
 *   - Governing Authority (institutional/arbitrage): Legitimate coordinator of the constraint; no extraction accrues; orchestrates knowledge preservation
 *   - Individual Responder (moderate/constrained): Secondary participant; career constraints limit exit but competence developed is genuinely transferable
 *   - Population Safety (analytical/analytical): Ultimate beneficiary; receives response capacity as emergent outcome of competence maintenance
 *   - Analytical Observer (analytical/analytical): Recognizes pure coordination function; identifies sunset logic if synthetic training advances
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_reading, 0.22).
domain_priors:suppression_score(competence_reading, 0.18).
domain_priors:theater_ratio(competence_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(competence_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(competence_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_reading, rope).
narrative_ontology:human_readable(competence_reading, "Competence Reading: Live Exercised Knowledge in Disaster Preparedness").
narrative_ontology:topic_domain(competence_reading, "governance/disaster_preparedness/institutional_memory").

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(competence_reading, distributed).
narrative_ontology:cs_authority_grounding(competence_reading, practice).
narrative_ontology:cs_interpretation_layer_present(competence_reading).
narrative_ontology:cs_kernel_id(competence_reading, preparedness_retention).
narrative_ontology:cs_reading_relation(competence_reading, husk_reading, coexists_with).
narrative_ontology:cs_reading_relation(competence_reading, hybrid_reading, coexists_with).
narrative_ontology:cs_axiom(competence_reading, foundational, live_exercise_retains_competence).
narrative_ontology:cs_axiom_status(live_exercise_retains_competence, holdable).
narrative_ontology:cs_axiom_grounding(competence_reading, live_exercise_retains_competence, empirically_contingent).
narrative_ontology:cs_axiom(competence_reading, secondary, competence_grounds_legitimacy).
narrative_ontology:cs_axiom_status(competence_grounds_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding(competence_reading, competence_grounds_legitimacy, instrumental).
narrative_ontology:cs_reference_frame(competence_reading, functional_knowledge_preservation).
narrative_ontology:cs_drift_state(competence_reading, contemporary_bureaucratization, gap(practice_drift, minor, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_reading, population_safety).
narrative_ontology:constraint_beneficiary(competence_reading, operational_personnel).
narrative_ontology:constraint_beneficiary(competence_reading, response_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPERATIONAL PERSONNEL (ROPE) — Organized agents (fire departments, emergency management, civil protection) experience drills and inspections as pure coordination mechanisms. Low-cost exercises maintain collective skill repertoire and organizational memory. Personnel can exit (retrain, cross-train) but benefit from the constraint's coordination function. Extractiveness is minimal — the constraint aligns incentives with operational capacity.
constraint_indexing:constraint_classification(competence_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: GOVERNING AUTHORITY (ROPE) — Institutional actors (government, regulatory bodies) see drills and inspections as legitimate coordination overhead—the necessary cost of maintaining response capacity. No extraction runs toward any particular beneficiary; the constraint solves a collective action problem (preserving knowledge that would otherwise degrade). Arbitrage exit exists: could outsource to private contractors, but that changes the constraint type (shifts toward snare). Current arrangement is coordinated.
constraint_indexing:constraint_classification(competence_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: INDIVIDUAL RESPONDER (ROPE) — At biographical timescale, an individual firefighter or responder experiences drills as time-intensive coordination activity. Constrained exit: cannot skip training without career consequences, but the competence developed is genuinely transferable and valued. The constraint maintains their own operational capacity alongside the collective's. Low experienced extraction because the beneficiary alignment is direct.
constraint_indexing:constraint_classification(competence_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (SCAFFOLD) — From a civilizational perspective, the constraint has an implicit sunset logic built into its success: as competence is truly maintained through live exercise, the *need* for expensive, ritual-heavy exercises decreases in a mature operational system. Simulation, synthetic training environments, and knowledge formalization could eventually reduce the coordination cost. Current theater ratio reflects that many exercises remain somewhat performative (satisfying regulatory requirements) alongside genuinely competence-preserving activities. The scaffold framing captures this: temporary coordination overhead declining as alternatives mature.
constraint_indexing:constraint_classification(competence_reading, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: CIVILIZATIONAL ANALYTICAL (ROPE) — At the longest timescale and broadest scope, live exercise and inspection are revealed as pure coordination mechanisms for a genuine collective action problem: complex procedural knowledge decays without practice; organizational memory requires continuous reinforcement; the coordination function (maintaining response capacity) is the entire point of the constraint. No extraction accrues to any agent. The low theater ratio (0.35) reflects that the functional benefit genuinely justifies most of the activity—this is not a performative system masquerading as competence-preserving.
constraint_indexing:constraint_classification(competence_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_reading_tests).
:- end_tests(competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.22): Low. The constraint's primary function is coordinating a collective action problem (maintaining knowledge that decays without practice). No agent captures disproportionate benefit; the beneficiaries (population safety, operational capacity) are diffuse and aligned with the constraint's stated purpose. The modest upward drift (0.18 → 0.25 over 20 years) reflects minor accumulation of regulatory overhead and ceremony beyond the functional minimum, but the core mechanism remains coordinated. Suppression (0.18): Low. Participants can exit (retrain, cross-train, leave the profession) at moderate cost; the constraint does not prevent alternatives; organizational autonomy is preserved. Theater ratio (0.35): Moderate. A significant portion of drills and inspections is genuinely functional—scenario-based training, skill verification, equipment checks directly improve response capacity. The 35% theater reflects regulatory documentation requirements and ceremonial aspects (after-action reviews, compliance signoffs) that are real but not the primary mechanism. The modest upward drift reflects increasing bureaucratization of competence requirements over time, not fundamental change in the constraint's type.
 *
 * PERSPECTIVAL GAP:
 *   The competence reading produces consistent Rope classifications across perspectives, with a Scaffold edge from the analytical observer. This uniformity is not coincidental: it reflects that the constraint genuinely solves a coordination problem without producing systematic extraction. Compare this to the husk_reading (which would produce high theater, Piton from most perspectives) and the hybrid_reading (which would show Tangled Rope—mixed coordination and extraction). The perspectival gap between competence and husk readings is not about who benefits, but about whether drills *actually maintain competence* (competence reading) or merely maintain the *appearance* of competence (husk reading). The gap is empirically resolvable through measurement of skill retention and deployment performance.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is low because the constraint's structure produces diffuse, aligned benefits rather than concentrated extraction. Operational personnel benefit from competence maintained and organizational memory preserved (d ≈ 0.35 for organized agents with mobile exit). The governing authority experiences the constraint as legitimate coordination overhead with no capture (d ≈ 0.10 for institutional arbitrage agents). Individual responders experience constrained exit but gain genuinely transferable competence (d ≈ 0.45 for moderate constrained agents). Population safety experiences pure benefit with no cost (d ≈ 0.05 for analytical observers). The low directionality values across all perspectives drive the rope classification: effective extraction χ ≤ 0.35 because f(d) values are all low. No agent experiences this constraint as extractive.
 *
 * MANDATROPHY ANALYSIS:
 *   The competence reading resolves mandatrophy by instantiating pure coordination without the hidden extraction that would produce snare or tangled_rope classifications. The constraint's legitimacy claim ('we maintain competence through drills') maps directly to its structural function (knowledge retention through live exercise). The coherence between claim and function is what allows the rope classification to hold. If the constraint were actually a husk (ceremony masking decay), the claim-function mismatch would produce snare or piton. If the constraint were hybrid (mixed ceremony and competence), the mixed mechanism would produce tangled_rope. This reading's mandatrophy is resolved by asserting that the functional component genuinely dominates—drill outcomes measurably improve response capacity, not just satisfy regulators. The theater ratio (0.35) acknowledges ceremony exists but is not the primary mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_vs_husk_boundary,
    'What observable distinguishes live competence from performative husk: ceremony without retention?',
    'Measurement of skill retention rates post-drill; comparison of performance in controlled exercises vs. actual deployment scenarios; longitudinal tracking of personnel turnover and knowledge loss in high-ceremony vs. high-exercise regimes',
    'If exercises genuinely retain competence: this reading (rope with low theater) is correct. If exercises are mostly ceremonial: constraint is husk_reading (piton with high theater).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_vs_husk_boundary, empirical, 'Boundary between live competence and performative ceremony').

omega_variable(
    kernel_reading_identity,
    'Is preparedness fundamentally about *live exercised knowledge* (competence_reading), *institutional theater masking decay* (husk_reading), or *hybrid allocation between ceremony and skill-building* (hybrid_reading)?',
    'Comparative case study: high-drill/low-ceremony regimes (Singapore, Israel civil defense) vs. high-ceremony/low-actual-drill regimes (some European bureaucracies); measurement of actual response capacity outcomes; interviews with personnel about whether drills improve actual performance',
    'Reading choice determines the constraint''s type, beneficiary structure, and legitimacy assessment. This omega is the committer-axis axis: which reading of the preparedness_retention kernel does this story instantiate?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Which reading of the preparedness kernel this constraint instantiates').

omega_variable(
    synthetic_training_displacement,
    'Can advanced simulation, VR-based training, and formal knowledge systems ultimately replace live drill and inspection as competence-preservation mechanisms?',
    'Empirical comparison of competence retention in simulation-trained vs. live-trained personnel; measurement of transfer learning from synthetic to actual scenarios; cost-benefit analysis as simulation technology matures',
    'If true: competence_reading includes genuine sunset logic—drills decline as alternatives mature (scaffold framing). If false: live exercise is fundamentally irreplaceable (rope framing sustained indefinitely).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synthetic_training_displacement, empirical, 'Whether synthetic training can replace live exercise for competence retention').

omega_variable(
    extraction_through_political_instrumentalization,
    'Can the preparedness constraint be weaponized to extract resources, concentrate power, or suppress organizational autonomy through the logic of ''competence maintenance''?',
    'Historical analysis of preparedness regimes used to justify centralization, budget capture, or suppression of local response capacity; case studies where ''competence requirements'' were used to eliminate competing organizations',
    'If frequent: the constraint has latent snare dynamics that activate under certain governance conditions (hybrid_reading). If rare: the rope reading is robust across institutional contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_through_political_instrumentalization, empirical, 'Whether competence logic can be instrumentalized for extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(comp_tr_t10, competence_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(comp_tr_t20, competence_reading, theater_ratio, 20, 0.38).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(comp_be_t10, competence_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(comp_be_t20, competence_reading, base_extractiveness, 20, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_reading, husk_reading).
narrative_ontology:affects_constraint(competence_reading, hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_retention kernel decomposes into three structurally distinct constraints: competence_reading (ε=0.22, Rope—functional skill maintenance), husk_reading (ε≈0.65, Piton—ceremonial compliance masking decay), and hybrid_reading (ε≈0.40, Tangled Rope—mixed functional and extractive allocation). All three observe the same phenomena (drills, inspections, training) but have different ε values because they measure the constraint through different observable choices: Does the drill actually improve response capacity? Is the ceremony decoupled from competence? What is the allocation ratio between functional and performative? The three readings coexist as live positions in institutional practice and governance discourse. No single reading dominates; each is institutionalized in different contexts (competence reading dominates in Singapore/Israel civil defense; husk reading dominates in some European bureaucracies; hybrid reading dominates in adaptive organizations).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
