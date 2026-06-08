% ============================================================================
% CONSTRAINT STORY: discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_discontinuity_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: discontinuity_reading
 *   human_readable: Discontinuity Reading: Classical Latin as Exclusive Correctness Standard
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   The discontinuity reading of correct Latin emerged during the Renaissance
 *   as humanist scholars recovered Classical texts and declared medieval
 *   Latin a corrupt deviation requiring reconstruction. This reading
 *   establishes a rupture between Classical forms (correct, preserved in
 *   ancient texts) and medieval forms (incorrect, requiring external
 *   correction from textual sources). The constraint coordinates a real
 *   scholarly function — establishing rigorous standards for Classical
 *   textual criticism — while extracting from scholars whose work focuses on
 *   post-Classical periods. The discontinuity reading is one of three major
 *   framings of the 'correct_latin' kernel; sibling readings
 *   (continuity_reading: medieval Latin as legitimate evolution;
 *   hybrid_reading: register distinction without rupture) organize the same
 *   historical linguistic data around different legitimacy boundaries and
 *   produce different beneficiary sets. The measurements show increasing
 *   extractiveness and suppression over the 300-year interval as the
 *   discontinuity reading became institutionally entrenched in university
 *   curricula and professional philology, while theater ratio increased as
 *   the enforcement mechanism became more performative (correctness policing
 *   detached from actual linguistic analysis).
 *
 * KEY AGENTS:
 *   - Classical Philologists: Primary beneficiaries (institutional/arbitrage) — capture definitional authority, institutional resources, and prestige through the discontinuity reading's privileging of Classical period
 *   - Medieval Latinists: Primary victims (powerless/identity_locked) — professional identity delegitimized by the rupture narrative; their subject matter reclassified as corruption rather than legitimate linguistic data
 *   - University Latin Instructors: Mixed position (moderate/constrained) — benefit from standardized pedagogy but constrained by restricted teaching scope
 *   - Historical Linguistics Community: Organized alternative (organized/mobile) — building empirical frameworks that treat medieval Latin as regular evolution, creating sunset pressure on the discontinuity reading
 *   - Prescriptive Grammar Authority: Institutional enforcer (institutional/arbitrage) — maintains the rupture narrative through performative correctness policing despite empirical linguistics undermining the corruption framing
 *   - Living Latin Practitioners: Secondary victims (moderate/constrained) — contemporary Latin users delegitimized by the discontinuity reading's restriction of legitimate forms to reconstructed Classical standard
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(discontinuity_reading, 0.58).
domain_priors:suppression_score(discontinuity_reading, 0.67).
domain_priors:theater_ratio(discontinuity_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(discontinuity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(discontinuity_reading, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(discontinuity_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(discontinuity_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(discontinuity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(discontinuity_reading, "Discontinuity Reading: Classical Latin as Exclusive Correctness Standard").
narrative_ontology:topic_domain(discontinuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(discontinuity_reading, '55734995-85cc-48c7-a23f-3d1c6c99ea54').
narrative_ontology:cs_kernel_codification('55734995-85cc-48c7-a23f-3d1c6c99ea54', fixed_text).
narrative_ontology:cs_authority_grounding('55734995-85cc-48c7-a23f-3d1c6c99ea54', lineage).
narrative_ontology:cs_interpretation_layer_present('55734995-85cc-48c7-a23f-3d1c6c99ea54').
narrative_ontology:cs_reading_relation('55734995-85cc-48c7-a23f-3d1c6c99ea54', discontinuity_reading__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('55734995-85cc-48c7-a23f-3d1c6c99ea54', discontinuity_reading__hybrid_reading, influences).
narrative_ontology:cs_axiom('55734995-85cc-48c7-a23f-3d1c6c99ea54', foundational, classical_forms_exclusively_legitimate).
narrative_ontology:cs_axiom_status(classical_forms_exclusively_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('55734995-85cc-48c7-a23f-3d1c6c99ea54', classical_forms_exclusively_legitimate, conventional).
narrative_ontology:cs_axiom('55734995-85cc-48c7-a23f-3d1c6c99ea54', foundational, medieval_deviation_is_corruption).
narrative_ontology:cs_axiom_status(medieval_deviation_is_corruption, holdable).
narrative_ontology:cs_axiom_grounding('55734995-85cc-48c7-a23f-3d1c6c99ea54', medieval_deviation_is_corruption, empirically_contingent).
narrative_ontology:cs_axiom('55734995-85cc-48c7-a23f-3d1c6c99ea54', secondary, textual_reconstruction_recovers_correctness).
narrative_ontology:cs_axiom_status(textual_reconstruction_recovers_correctness, holdable).
narrative_ontology:cs_axiom_grounding('55734995-85cc-48c7-a23f-3d1c6c99ea54', textual_reconstruction_recovers_correctness, instrumental).
narrative_ontology:cs_reference_frame('55734995-85cc-48c7-a23f-3d1c6c99ea54', classical_textual_corpus).
narrative_ontology:cs_drift_state('55734995-85cc-48c7-a23f-3d1c6c99ea54', post_historical_linguistics_emergence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('55734995-85cc-48c7-a23f-3d1c6c99ea54', '').
narrative_ontology:cs_kernel_id(discontinuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(discontinuity_reading, classical_philologists).
narrative_ontology:constraint_beneficiary(discontinuity_reading, humanist_scholars).
narrative_ontology:constraint_beneficiary(discontinuity_reading, textual_reconstruction_specialists).
narrative_ontology:constraint_victim(discontinuity_reading, medieval_latinists).
narrative_ontology:constraint_victim(discontinuity_reading, continuity_tradition_scholars).
narrative_ontology:constraint_victim(discontinuity_reading, living_latin_practitioners).
narrative_ontology:constraint_vindicates(discontinuity_reading, textual_purity_doctrine).
narrative_ontology:constraint_vindicates(discontinuity_reading, classical_supremacy_thesis).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MEDIEVAL LATINIST (SNARE) — Identity-locked within a scholarly tradition that the discontinuity reading delegitimizes. Cannot exit without abandoning professional identity and expertise domain. Experiences maximum extraction: their subject matter is reclassified as corrupt deviation rather than legitimate linguistic evolution. The binding is cognitive — structurally they could shift fields, but their identity is constituted through medieval Latin scholarship.
constraint_indexing:constraint_classification(discontinuity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: UNIVERSITY LATIN INSTRUCTOR (TANGLED ROPE) — Constrained by institutional curriculum requirements that privilege Classical forms. Benefits from the coordination function (standardized pedagogy, shared textual canon) but bears costs through restricted teaching scope and delegitimization of post-Classical sources. Mixed experience: the standard enables their work while limiting what counts as legitimate Latin.
constraint_indexing:constraint_classification(discontinuity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CLASSICAL PHILOLOGY DEPARTMENT (ROPE) — Primary beneficiary. The discontinuity reading concentrates institutional resources, prestige, and definitional authority in Classical studies. Experiences the constraint as coordination: a shared standard for correctness that enables rigorous textual scholarship. Net beneficiary — extraction flows toward this institutional position.
constraint_indexing:constraint_classification(discontinuity_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: HISTORICAL LINGUISTICS COMMUNITY (SCAFFOLD) — Organized scholars building alternative frameworks (sociolinguistics, corpus linguistics, evolutionary models) that treat medieval Latin as legitimate data rather than corruption. See the discontinuity reading as a temporary Renaissance-era framing being replaced by empirical linguistic methods. Sunset logic: as diachronic linguistics matures, the rupture narrative loses explanatory force.
constraint_indexing:constraint_classification(discontinuity_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PRESCRIPTIVE GRAMMAR AUTHORITY (PITON) — The enforcement mechanism (textual reconstruction from Classical sources as the sole legitimate method) persists through institutional inertia despite empirical linguistics demonstrating that medieval Latin follows regular evolutionary patterns. The ritual of declaring forms 'corrupt' continues because the institutional structure depends on it, not because the linguistic analysis is sound. Theater ratio reflects that much of the correctness policing is performative maintenance of disciplinary boundaries.
constraint_indexing:constraint_classification(discontinuity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the discontinuity reading coordinates a real scholarly function (establishing textual standards for Classical sources) while extracting from scholars whose work falls outside the privileged period. The rupture narrative is a contingent framing choice, not a linguistic necessity — languages evolve continuously, and the Classical/medieval boundary is an imposed discontinuity that serves institutional interests. Tangled rope: genuine coordination function with embedded asymmetric extraction.
constraint_indexing:constraint_classification(discontinuity_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(discontinuity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(discontinuity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(discontinuity_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(discontinuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(discontinuity_reading, TR),
    TR >= 0.70.

:- end_tests(discontinuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The discontinuity reading extracts from medieval studies scholars by delegitimizing their subject matter, from students by restricting pedagogical scope, and from contemporary Latin users by declaring post-Classical forms illegitimate. The extraction is substantial but not maximal — medieval Latin scholarship persists despite the rupture narrative, and alternative frameworks (historical linguistics, sociolinguistics) provide exit paths. The value increased over the interval as institutional entrenchment concentrated resources in Classical philology. Suppression (0.67): High. Significant barriers to legitimizing medieval Latin within the discontinuity framework: institutional curriculum requirements privilege Classical texts, professional advancement in philology depends on Classical specialization, publication venues enforce the rupture narrative, and the textual reconstruction method is presented as the only rigorous approach. Suppression increased over the interval as the discontinuity reading became the default institutional framing. Theater ratio (0.48): Moderate. The enforcement mechanism has substantial performative content — much correctness policing is boundary maintenance rather than linguistic analysis — but the underlying coordination function (textual criticism standards) remains real. Theater increased over the interval as the rupture narrative became ritualized.
 *
 * PERSPECTIVAL GAP:
 *   The discontinuity reading produces a wide perspectival gap. Classical philology departments experience pure coordination (Rope) — the rupture narrative enables their core scholarly function and concentrates resources in their domain. Medieval latinists experience pure extraction (Snare) — their professional identity is constituted through a subject matter the discontinuity reading delegitimizes, creating identity-lock. University instructors experience mixed coordination and extraction (Tangled Rope) — standardized pedagogy benefits their work while restricted scope constrains it. The historical linguistics community sees a temporary framing being replaced by empirical methods (Scaffold) — the rupture narrative is losing explanatory force as diachronic linguistics matures. The prescriptive grammar authority sees its own degraded ritual (Piton) — correctness policing persists through institutional inertia despite empirical undermining. The analytical observer sees genuine coordination with embedded extraction (Tangled Rope) — the discontinuity reading solves a real problem (Classical textual standards) while extracting from post-Classical scholarship. The gap reveals that the rupture narrative is a contingent institutional framing, not a linguistic necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position relative to the Classical/medieval boundary. Classical philologists are primary beneficiaries — the discontinuity reading concentrates definitional authority and resources in their domain, producing low d and negative effective extraction (they collect from the constraint). Medieval latinists are primary victims — their subject matter is delegitimized, producing high d and high effective extraction (the constraint extracts from them). University instructors have mixed position — they benefit from standardized pedagogy (coordination function) but bear costs through restricted scope (extraction function), producing moderate d. The historical linguistics community has mobile exit options — they can build alternative frameworks outside the discontinuity reading's jurisdiction, producing low d. The prescriptive grammar authority has arbitrage exit — they maintain the enforcement mechanism but could abandon it without cost, producing low d. Identity-lock appears for medieval latinists because their professional identity is constituted through the delegitimized subject matter — exit would require becoming a different kind of scholar, not just changing research focus.
 *
 * MANDATROPHY ANALYSIS:
 *   The discontinuity reading resolves mandatrophy by demonstrating that the Classical/medieval rupture is a contingent framing choice that coordinates one scholarly function (Classical textual criticism) while extracting from another (medieval linguistic studies). The constraint is not 'pure coordination' (the medieval latinists' experience is real extraction) and not 'pure extraction' (the Classical textual criticism function is genuine). The tangled_rope classification from the analytical perspective captures this: the discontinuity reading solves a real coordination problem (how to establish rigorous standards for Classical sources) while embedding asymmetric extraction (delegitimizing post-Classical forms and concentrating authority in Classical philology). The mandatrophy is resolved by recognizing that the same structural arrangement can be both coordination and extraction depending on which scholarly community you measure from — and that the rupture narrative itself is what creates the asymmetry. Alternative readings of the correct_latin kernel (continuity_reading, hybrid_reading) would produce different extraction patterns from the same historical linguistic data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the discontinuity reading''s rupture narrative a discovered linguistic fact or a constructed institutional boundary?',
    'Historical analysis of when and why the Classical/medieval boundary was established; comparison with other language traditions that lack this rupture framing; examination of whether the boundary tracks linguistic features or institutional interests.',
    'If discovered fact: discontinuity reading is mountain (natural linguistic boundary). If constructed boundary: discontinuity reading is tangled_rope or snare (institutional extraction mechanism). The omega documents that this constraint is one reading of the ''correct_latin'' kernel; sibling readings (continuity_reading, hybrid_reading) frame the same historical linguistic data differently.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the Classical/medieval rupture is linguistic fact or institutional construction').

omega_variable(
    textual_reconstruction_legitimacy,
    'Does reconstructing Classical forms from texts constitute recovery of a natural standard or imposition of an artificial one?',
    'Comparison of reconstructed Classical Latin with actual usage patterns in Classical period inscriptions, graffiti, and non-literary sources; assessment of whether ''Classical Latin'' is itself an idealized construct rather than a spoken reality.',
    'If recovery: discontinuity reading''s method is legitimate restoration. If imposition: the method creates the standard it claims to discover, and medieval forms are delegitimized by fiat rather than by deviation from a real baseline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_reconstruction_legitimacy, empirical, 'Whether textual reconstruction recovers or constructs the Classical standard').

omega_variable(
    evolutionary_continuity_evidence,
    'Do medieval Latin forms follow regular sound change and morphological evolution patterns from Classical Latin, or do they represent random corruption?',
    'Application of comparative method and historical linguistics to Latin diachronic data; identification of systematic vs. unsystematic changes; comparison with other documented language evolutions.',
    'If regular evolution: discontinuity reading''s ''corruption'' framing is empirically false, and the constraint is pure extraction (snare from more perspectives). If random corruption: discontinuity reading''s rupture narrative has empirical support, and the constraint is coordination (rope from more perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(evolutionary_continuity_evidence, empirical, 'Whether medieval Latin shows systematic evolution or random corruption').

omega_variable(
    sibling_reading_structural_delta,
    'What structural elements differ between the discontinuity reading and its sibling readings (continuity_reading, hybrid_reading)?',
    'Cross-reading comparison: continuity_reading treats medieval forms as legitimate evolutionary stage; hybrid_reading distinguishes registers (literary vs. vernacular) without rupture; discontinuity_reading declares medieval forms illegitimate. The delta is in the legitimacy boundary and the beneficiary set.',
    'The discontinuity reading concentrates authority in Classical philology departments; continuity reading distributes authority across medieval studies; hybrid reading creates parallel legitimacy tracks. Same kernel, different extraction patterns.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Structural differences between discontinuity and sibling readings of correct_latin kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(discontinuity_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(disc_lat_theater_renaissance, discontinuity_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(disc_lat_theater_enlightenment, discontinuity_reading, theater_ratio, 150, 0.42).
narrative_ontology:measurement(disc_lat_theater_modern, discontinuity_reading, theater_ratio, 300, 0.48).

% Extraction over time
narrative_ontology:measurement(disc_lat_extract_renaissance, discontinuity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(disc_lat_extract_enlightenment, discontinuity_reading, base_extractiveness, 150, 0.52).
narrative_ontology:measurement(disc_lat_extract_modern, discontinuity_reading, base_extractiveness, 300, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(disc_lat_suppress_renaissance, discontinuity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(disc_lat_suppress_enlightenment, discontinuity_reading, suppression_requirement, 150, 0.63).
narrative_ontology:measurement(disc_lat_suppress_modern, discontinuity_reading, suppression_requirement, 300, 0.67).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(discontinuity_reading, identity_coordination).
narrative_ontology:affects_constraint(discontinuity_reading, continuity_reading).
narrative_ontology:affects_constraint(discontinuity_reading, hybrid_reading).

% DUAL FORMULATION NOTE:
% The discontinuity_reading is one of three major framings of the correct_latin kernel. It is linked to continuity_reading and hybrid_reading as sibling readings of the same kernel. Each reading organizes the same historical linguistic data around different legitimacy boundaries and produces different extraction patterns. The discontinuity reading is upstream in institutional influence (it became the dominant academic framing) but downstream in empirical support (historical linguistics evidence favors continuity or hybrid framings).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
