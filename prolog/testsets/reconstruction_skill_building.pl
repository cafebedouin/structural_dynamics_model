% ============================================================================
% CONSTRAINT STORY: reconstruction_skill_building
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reconstruction_skill_building, []).

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
 *   constraint_id: reconstruction_skill_building
 *   human_readable: Reconstruction Skill Building Through Deliberate Practice
 *   domain: cognitive_science/cultural_theory/media_studies
 *
 * SUMMARY:
 *   The reconstruction skill building constraint describes how specific
 *   deliberate practices—acting training, sustained reading of complex
 *   fiction, perspective-taking exercises—build motivational logic libraries
 *   through repeated rehearsal. This constraint is downstream of the
 *   automatic_vs_cultivated_mentalizing mountain: given that mentalizing
 *   capacity can be cultivated beyond automatic baselines, this constraint
 *   describes one coordination mechanism for doing so. The primary observable
 *   is the correlation between practice hours and performance on
 *   unfamiliar-agent perspective tasks, with library size metrics as the
 *   proposed mechanism. The constraint exhibits rope classification from all
 *   perspectives because it represents a genuine coordination solution with
 *   minimal extraction: agents who invest time in deliberate practice
 *   reliably improve their perspective-taking capacity, the mechanism is
 *   accessible (reading and acting training are widely available), and
 *   participation is voluntary. The slight increase in extractiveness over
 *   the interval (0.15 → 0.18) reflects growing commercialization of
 *   perspective-taking training programs, but extraction remains low. Theater
 *   ratio is also low (0.25), indicating that the practice-performance
 *   correlation is genuine rather than performative.
 *
 * KEY AGENTS:
 *   - Deliberate Practitioners: Primary beneficiaries (moderate/mobile) — agents investing time in acting training or sustained reading; experience direct skill gains
 *   - Acting Students: Beneficiary subset (moderate/mobile) — specific population using method acting and character work to build motivational logic libraries
 *   - Sustained Readers: Beneficiary subset (moderate/mobile) — agents using literary fiction engagement to cultivate perspective-taking capacity
 *   - Educational Institutions: Institutional beneficiaries (institutional/arbitrage) — drama programs and literature curricula validated by practice-performance correlation
 *   - Acting Training Community: Organized beneficiaries (organized/mobile) — professional schools and method acting traditions whose pedagogical approach is supported by the constraint
 *   - Perspective-Taking Educators: Beneficiary subset (moderate/mobile) — teachers and trainers whose methods are validated by measurable outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reconstruction_skill_building, 0.18).
domain_priors:suppression_score(reconstruction_skill_building, 0.12).
domain_priors:theater_ratio(reconstruction_skill_building, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reconstruction_skill_building, extractiveness, 0.18).
narrative_ontology:constraint_metric(reconstruction_skill_building, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(reconstruction_skill_building, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reconstruction_skill_building, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(reconstruction_skill_building, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reconstruction_skill_building, rope).
narrative_ontology:human_readable(reconstruction_skill_building, "Reconstruction Skill Building Through Deliberate Practice").
narrative_ontology:topic_domain(reconstruction_skill_building, "cognitive_science/cultural_theory/media_studies").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reconstruction_skill_building, deliberate_practitioners).
narrative_ontology:constraint_beneficiary(reconstruction_skill_building, acting_students).
narrative_ontology:constraint_beneficiary(reconstruction_skill_building, sustained_readers).
narrative_ontology:constraint_beneficiary(reconstruction_skill_building, perspective_taking_educators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DELIBERATE PRACTITIONER (ROPE) — Agent investing time in acting training or sustained reading to build motivational logic libraries. Experiences the constraint as pure coordination: practice hours correlate with improved perspective-taking performance. Low extraction, voluntary participation, clear benefit.
constraint_indexing:constraint_classification(reconstruction_skill_building, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: EDUCATIONAL INSTITUTION (ROPE) — Drama programs, literature curricula, and perspective-taking training programs benefit from the practice-performance correlation. The constraint enables curriculum design with predictable outcomes. Net beneficiary with high mobility.
constraint_indexing:constraint_classification(reconstruction_skill_building, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: ACTING TRAINING COMMUNITY (ROPE) — Professional acting schools and method acting traditions have organized around the practice-skill correlation. The constraint validates their pedagogical approach and provides measurable outcomes. Coordination function with minimal extraction.
constraint_indexing:constraint_classification(reconstruction_skill_building, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: SUSTAINED READER (ROPE) — Agent engaging with complex literary fiction to build perspective-taking capacity. Experiences the constraint as a reliable coordination mechanism: time invested in reading correlates with improved unfamiliar-agent modeling. Voluntary, low-cost, high-benefit.
constraint_indexing:constraint_classification(reconstruction_skill_building, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — From the analytical perspective, this constraint represents a genuine coordination solution to the problem of cultivating mentalizing capacity beyond automatic baselines. The practice-performance correlation is empirically robust, the mechanism is accessible, and the extraction is minimal. Pure coordination function.
constraint_indexing:constraint_classification(reconstruction_skill_building, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reconstruction_skill_building_tests).
:- end_tests(reconstruction_skill_building_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Very low. The constraint represents a coordination mechanism with minimal extraction. Agents who practice gain skills; those who don't practice don't lose anything they previously had. The slight extraction reflects opportunity costs (time spent practicing could be spent on other activities) and the growing commercialization of training programs (some agents pay for access to structured practice environments). But the core mechanism—reading books, rehearsing characters—is accessible at minimal cost. Suppression (0.12): Very low. Participation is voluntary, alternatives exist (other methods of cultivating perspective-taking), and exit is trivial (stop practicing). The modest suppression reflects time and attention constraints (practice requires sustained effort) and potential access barriers to formal training programs, but these are not coercive. Theater ratio (0.25): Low. The practice-performance correlation is empirically robust across multiple studies. Acting training and sustained reading do reliably correlate with improved perspective-taking performance. Some theater exists (training programs may oversell transfer effects, library size metrics have measurement validity questions), but the core coordination function is genuine. Accessibility collapse (0.22): Low. The constraint is highly accessible—books and acting exercises are widely available, practice can be self-directed, and the mechanism is well-understood. Resistance (0.15): Low. The constraint is easy to adopt (start reading or practicing) and easy to abandon (stop). No lock-in effects or path dependencies.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap—all five perspectives classify as rope. This uniformity is diagnostic: when a constraint appears as pure coordination from every structural position, it is likely a genuine coordination mechanism rather than extraction disguised as coordination. The deliberate practitioner, the educational institution, the acting training community, the sustained reader, and the analytical observer all experience the constraint as a reliable practice-performance correlation with voluntary participation and clear benefits. The lack of perspectival gap distinguishes this constraint from tangled ropes (which appear as rope to beneficiaries but snare to victims) and false summits (which appear as mountain to some observers but reveal extraction under structural analysis). The omega variables identify remaining empirical uncertainties (transfer specificity, measurement validity, threshold effects), but these uncertainties do not create perspectival disagreement about the constraint's type—all perspectives agree it is coordination, and the omegas address how well that coordination function works.
 *
 * DIRECTIONALITY LOGIC:
 *   All agents in this constraint are beneficiaries. Deliberate practitioners, acting students, and sustained readers invest time and gain measurable perspective-taking skills—they are net beneficiaries with mobile exit options (can stop practicing at any time). Educational institutions and the acting training community benefit from the constraint's validation of their pedagogical approaches—they are institutional/organized beneficiaries with arbitrage/mobile exit options. No agents are victims: the constraint does not extract from non-practitioners (they simply don't gain the skill benefits), and practitioners gain more than they invest. The directionality values are all low (beneficiary side of the spectrum), producing low or negative effective extraction across all perspectives. This uniform beneficiary structure is characteristic of pure coordination mechanisms—the constraint solves a collective action problem (how to cultivate mentalizing capacity) without creating asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that low extraction and low suppression can coexist with genuine coordination function. The mandatrophy risk for rope constraints is misclassifying extraction as coordination—beneficiaries may experience a snare as rope if they don't see the costs borne by victims. This constraint avoids that risk through its structural symmetry: there are no victims. Non-practitioners are not harmed by the constraint's existence; they simply don't gain the benefits that practitioners gain. The practice-performance correlation is empirically robust (low theater ratio), participation is voluntary (low suppression), and the mechanism is accessible (low accessibility collapse). The slight extractiveness (0.18) reflects real opportunity costs and commercialization pressures, not hidden coercion. The analytical perspective confirms the rope classification: from a civilizational view, this constraint represents a genuine solution to the problem of cultivating mentalizing capacity, with minimal extraction and maximal accessibility. The constraint's downstream relationship to the automatic_vs_cultivated_mentalizing mountain is also diagnostic: given that mentalizing can be cultivated (mountain), this constraint describes how to do so (rope). The mountain establishes the possibility space; the rope describes a coordination mechanism within that space.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transfer_specificity,
    'Do motivational logic libraries built through acting training transfer to real-world perspective-taking, or are they domain-specific performance artifacts?',
    'Longitudinal studies comparing acting students'' perspective-taking performance in theatrical contexts vs. naturalistic social situations; cross-domain transfer tests',
    'If transfer is limited: the coordination function is narrower than claimed, and the constraint may have higher theater_ratio in non-performance contexts. If transfer is robust: the rope classification holds across domains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_specificity, empirical, 'Whether acting training transfers to real-world perspective-taking').

omega_variable(
    library_size_measurement,
    'What constitutes a valid metric for ''motivational logic library size''? Are we measuring retrieval fluency, pattern diversity, or application accuracy?',
    'Psychometric validation of library size measures; comparison of different operationalizations (reaction time, verbal protocol analysis, behavioral prediction accuracy)',
    'If measurement validity is low: the practice-performance correlation may be an artifact of shared method variance rather than genuine skill building. If validity is high: the coordination mechanism is well-characterized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(library_size_measurement, empirical, 'Validity of motivational logic library size metrics').

omega_variable(
    practice_threshold,
    'Is there a minimum practice threshold below which library building does not occur, or is the relationship linear from zero hours?',
    'Dose-response studies with varying practice durations; identification of potential threshold effects or plateau points',
    'If threshold exists: access barriers may be higher than the low suppression score suggests, particularly for time-constrained agents. If linear: the coordination function is maximally accessible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practice_threshold, empirical, 'Whether practice-skill relationship has threshold effects').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reconstruction_skill_building, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(recon_skill_tr_t0, reconstruction_skill_building, theater_ratio, 0, 0.2).
narrative_ontology:measurement(recon_skill_tr_t5, reconstruction_skill_building, theater_ratio, 5, 0.22).
narrative_ontology:measurement(recon_skill_tr_t10, reconstruction_skill_building, theater_ratio, 10, 0.25).

% Extraction over time
narrative_ontology:measurement(recon_skill_be_t0, reconstruction_skill_building, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(recon_skill_be_t5, reconstruction_skill_building, base_extractiveness, 5, 0.16).
narrative_ontology:measurement(recon_skill_be_t10, reconstruction_skill_building, base_extractiveness, 10, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reconstruction_skill_building, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of automatic_vs_cultivated_mentalizing (mountain). The upstream constraint establishes that mentalizing capacity can be cultivated beyond automatic baselines; this constraint describes one specific coordination mechanism for doing so. The epsilon values differ by design: the upstream constraint has very low extraction (ε ≈ 0.05) because it is a natural law about cognitive architecture; this constraint has slightly higher extraction (ε = 0.18) because it describes a social coordination mechanism with opportunity costs and commercialization pressures. The two constraints are structurally distinct: one is a fact about human cognition, the other is a practice for exploiting that fact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
