% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanafi_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: usul_al_fiqh_method__hanafi_reading
 *   human_readable: Hanafi Jurisprudential Method: Expansive Analogical Reasoning with Jurist Preference
 *   domain: legal/theological
 *
 * SUMMARY:
 *   The Hanafi school of Islamic jurisprudence is one of four classical Sunni
 *   methodological traditions. Its distinguishing feature is the expansive
 *   scope granted to jurist-driven methods—qiyas (analogical reasoning from
 *   textual cases), ra'y (independent reasoned opinion), and istihsan
 *   (juristic preference to override analogy when public interest demands
 *   it). This reading instantiates the Hanafi approach as a constraint: the
 *   framework legally binds the reasoning methods available to jurists,
 *   legitimizes departures from textual literalism, and transfers authority
 *   from the text's surface to the jurist's rational judgment. The constraint
 *   coordinates the problem of legal lacunae while simultaneously extracting
 *   interpretive monopoly in favor of the rationalist ulama class. The Hanafi
 *   reading sits in structural tension with the Hanbali reading (which
 *   minimizes analogical reasoning and prefers textual narrowness) and
 *   coexists with Maliki and Shafi'i alternatives, each offering different
 *   solutions to the same founding problem.
 *
 * KEY AGENTS:
 *   - Hanafi jurists with rationalist training: institutional beneficiaries; set the boundaries of analogical reasoning and control application of ra'y and istihsan
 *   - Textualist legal schools (especially Hanbali): powerful payers; their methodological claims are systematically subordinated when rationalist principles take precedence
 *   - Literalist hadith interpreters: organized victims; explicit transmission is treated as secondary to analogical extension and jurist reasoning
 *   - Qadi administrators: institutional beneficiaries; gain structured discretion to resolve novel cases and adapt law to local conditions
 *   - Merchant networks: powerful beneficiaries; benefit from commercial law flexibility via analogical extension to new transactions
 *   - Theological conservatives: identity-locked payers; trapped between desire for textual fidelity and institutional pressure to accept rationalist methods
 *   - Textualist ulama observers: excluded; their objections are recorded but do not shape Hanafi-dominant jurisprudential deliberation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, 0.68).
domain_priors:suppression_score(usul_al_fiqh_method__hanafi_reading, 0.52).
domain_priors:theater_ratio(usul_al_fiqh_method__hanafi_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanafi_reading, "Hanafi Jurisprudential Method: Expansive Analogical Reasoning with Jurist Preference").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanafi_reading, "legal/theological").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanafi_reading, '0ed75b26-8759-4883-9c9a-5dd76b30e0ba').
narrative_ontology:cs_kernel_codification('0ed75b26-8759-4883-9c9a-5dd76b30e0ba', distributed).
narrative_ontology:cs_authority_grounding('0ed75b26-8759-4883-9c9a-5dd76b30e0ba', lineage).
narrative_ontology:cs_interpretation_layer_present('0ed75b26-8759-4883-9c9a-5dd76b30e0ba').
narrative_ontology:cs_reading_relation('0ed75b26-8759-4883-9c9a-5dd76b30e0ba', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_reading_relation('0ed75b26-8759-4883-9c9a-5dd76b30e0ba', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('0ed75b26-8759-4883-9c9a-5dd76b30e0ba', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_axiom('0ed75b26-8759-4883-9c9a-5dd76b30e0ba', foundational, qiyas_and_reason_coordinate_with_text).
narrative_ontology:cs_axiom_status(qiyas_and_reason_coordinate_with_text, holdable).
narrative_ontology:cs_axiom_grounding('0ed75b26-8759-4883-9c9a-5dd76b30e0ba', qiyas_and_reason_coordinate_with_text, deontological).
narrative_ontology:cs_axiom('0ed75b26-8759-4883-9c9a-5dd76b30e0ba', foundational, juristic_discretion_serves_public_interest).
narrative_ontology:cs_axiom_status(juristic_discretion_serves_public_interest, holdable).
narrative_ontology:cs_axiom_grounding('0ed75b26-8759-4883-9c9a-5dd76b30e0ba', juristic_discretion_serves_public_interest, instrumental).
narrative_ontology:cs_reference_frame('0ed75b26-8759-4883-9c9a-5dd76b30e0ba', juristic_reasoning_as_coordinate_source).
narrative_ontology:cs_drift_state('0ed75b26-8759-4883-9c9a-5dd76b30e0ba', contemporary_rationalist_dominance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0ed75b26-8759-4883-9c9a-5dd76b30e0ba', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, hanafi_jurists_with_rationalist_training).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, textualist_legal_schools).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, literalist_hadith_interpretations).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanafi_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(usul_al_fiqh_method__hanafi_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanafi_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 because the Hanafi framework systematically transfers interpretive authority from the text's explicit boundaries to the jurist's judgment, creating a durable rent for the rationalist scholar class. The extraction accumulates over the interval (0.45 → 0.68) as the Hanafi reading becomes institutionalized in major centers and jurists develop increasingly sophisticated arguments that qiyas, ra'y, and istihsan warrant ever-broader applications. Suppression is moderate (0.52) because textualist objections persist across centuries but are never permitted to reshape the framework—they are managed through subsidiary texts and minority positions rather than suppressed violently. Theater is low (0.28) because the Hanafi method remains functionally engaged with genuine legal problems (lacunae in the texts) throughout the interval; the performative element grows modestly as istihsan becomes a cover for preferences that override analogy without explicit principled justification, but the core function (analogical extension) remains primary. Accessibility of alternatives is moderate-low (0.45) because textualist methods remain available as theoretical positions but are institutionally foreclosed in Hanafi-dominant jurisdictions; a jurist can adopt Hanbali methodology, but only at cost of exile from rationalist centers. Resistance is substantial (0.71) because textualist and literalist schools mount principled objections across the entire interval, producing a sustained methodological debate that forces rationalist jurists to continuously justify their framework.
 *
 * PERSPECTIVAL GAP:
 *   The textualist ulama would classify this constraint as snare or extractive piton—a reading that claims coordination (solving lacunae) but operates as pure monopoly capture by the rationalist class. The Hanafi jurists would classify it as genuine rope—coordination of legal development with minimal coercive overhead, justified by the necessity of analogical reasoning. The qadi administrators would classify it as beneficial tangled rope—coordination that also concentrates authority, but appropriately so because jurists have expertise textualists lack. The theological conservatives would classify it as false mountain—presented as natural jurisprudential necessity but actually a constructed choice that benefits identifiable parties. The engine, reading the structural data, will compute per-seat classifications that capture these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Hanafi jurists with rationalist training are the structural beneficiaries: they control the interpretive boundary, teach the method, and their reasoning capacity is the constraint's primary resource. Their directionality d ≈ 0.15 (near the beneficiary end) because the constraint subsidizes their professional authority and legitimizes their discretion. Textualist schools and literalist hadith interpreters are victims: their methodological claims are subordinated, and their exit options are severely constrained. Their d ≈ 0.85 (near the target end) because the constraint extracts interpretive authority from them. Qadi administrators and merchant classes are secondary beneficiaries: they benefit from the flexibility but do not control the boundary. Their d ≈ 0.35–0.45 (slightly beneficiary-leaning) because they gain practical utility without bearing the cost of defending the method. Theological conservatives are trapped victims: they identify with textual fidelity (high identity lock) but cannot exit the framework without abandoning scholarly standing. Their d ≈ 0.80 because they pay the cost of methodological subordination and have minimal exit. The constraint's effective extraction χ is highest for identity-locked textualists and lowest for mobile rationalist jurists.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legal lacunae in the texts) remains live and well-attested by independent observers (Maliki and Shafi'i schools acknowledge the same problem, offering different solutions). The coordination function (analogical reasoning as a solution) is genuinely necessary and actively deployed. However, the extraction function (transferring interpretive monopoly to the rationalist jurist class) has accumulated over the interval: early Hanafi jurisprudence presents qiyas and ra'y as disciplined methods constrained by the text's underlying rationales, while later jurisprudence (especially under rationalist-dominant dynasties) presents istihsan as a principle that can override analogy when jurists deem public interest demands it. This drift—from 'justified by the text's logic' to 'justified by the jurist's judgment'—is captured in the rising base_extractiveness and theater_ratio over the interval. The constraint has NOT undergone mandatrophy (the founding problem has not died), but it has accumulated extraction layered atop its coordination function. The Hanafi reading remains defensible as coordination, but the measured extraction reveals how much of the constraint's operation now serves the jurist class's institutional interests rather than solving legal lacunae.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    illa_justification_drift,
    'Does istihsan''s departure from strict analogy remain grounded in identifying the underlying ''illa (rationale) of the text, or has it drifted into pure jurist preference disconnected from textual reasoning?',
    'Comparative analysis of early istihsan cases (al-Kasani, al-Sarakhsi) versus later practice (Ottoman-period fatwa collections) to measure whether stated justifications continue to reference textual rationales or increasingly invoke public interest as independent ground. Examination of textualist critiques and Hanafi responses over time.',
    'If istihsan has drifted into pure preference, the constraint is more extractive than measured (the theater_ratio should be higher and base_extractiveness should rise further). If istihsan remains grounded in textual logic, the constraint is closer to genuine coordination than extracted authority. This divergence is the mandate-rot question: has the method evolved legitimately or been corrupted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(illa_justification_drift, empirical, 'Whether istihsan remains textually grounded or has become disconnected from revealed sources.').

omega_variable(
    textual_lacunae_extent,
    'How many novel legal situations actually require qiyas, ra''y, or istihsan because the texts are silent, versus how many invoke these methods where textual guidance exists but jurists prefer to override it?',
    'Systematic classification of major istihsan cases from canonical Hanafi jurisprudence: (1) true lacunae where the text provides no rule; (2) cases where a textual rule exists but jurists override it via istihsan. Frequency distribution across the corpus.',
    'If most cases are true lacunae, the extraction measure underestimates the real coordination function—the constraint is more legitimate than measured. If most cases override available textual rules, the extraction measure understates jurist monopoly—the constraint is more extractive than measured, and the theater_ratio is actually higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_lacunae_extent, empirical, 'Whether the Hanafi method is primarily used to fill genuine textual gaps or to override available textual rules.').

omega_variable(
    reading_contest_boundary,
    'What specifically distinguishes the Hanafi reading from the Hanbali reading at the structural level? Is the contest about the EXTENT of analogical reasoning (Hanafi: expansive; Hanbali: minimal) or the LEGITIMACY of reason as a source (Hanafi: reason is coordinate with text; Hanbali: reason is subordinate)?',
    'Analysis of foundational texts in each tradition (al-Usul by al-Sarakhsi for Hanafi; Ibn Qudama for Hanbali) to identify whether the disagreement is about scope of application or about the nature of authority itself. Examination of how each school responds to the other''s objections.',
    'If the contest is scope-only, the readings COEXIST (each grants some role to qiyas, just differently). If it is about nature of authority, the readings FORECLOSE each other (one treats reason as inherently subordinate to text; the other does not). This affects how the engine computes the reading_relations: coexists_with vs. forecloses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_boundary, conceptual, 'Whether the Hanafi-Hanbali contest is about the scope of analogical reasoning or the fundamental nature of juristic authority.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the subordination of textualist claims in Hanafi-dominant regions structural (institutional displacement, lack of positions in teaching circles) or internalized (textualist jurists adopt rationalist frameworks, fusing their identity with the rationalist method)?',
    'Examination of biographical trajectories of textualist-trained jurists in rationalist centers: do they maintain distinct methodological positions (structural suppression) or do they gradually internalize the rationalist framework (internalized suppression)? Analysis of fatwa collections to identify whether textualist positions are documented as live alternatives or are mentioned only to be dismissed.',
    'If suppression is primarily structural, the constraint''s effective extraction is lower than measured because exit remains available (jurists can maintain a textualist position, though at institutional cost). If suppression is internalized, the constraint''s extraction is understated because textualist resistance has been psychologically incorporated—jurists no longer experience their position as suppressed but as superseded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether methodological subordination is structural or internalized in the Hanafi framework.').

omega_variable(
    rationalist_training_prerequisite_gatekeeping,
    'Does access to rationalist jurisprudential authority require explicit training in Greek philosophical logic and the methods of analogical reasoning, or can it be acquired through immersion in jurisprudential texts alone?',
    'Historical analysis of madrasah curricula in Hanafi-dominant centers; examination of biographical records of major Hanafi jurists to identify training pathways. Analysis of whether non-Greek-trained jurists have achieved prominent interpretive authority in the Hanafi tradition.',
    'If rationalist training is a strict prerequisite, the beneficiary class is tightly gated and the constraint operates more as an elite monopoly on authority. If the methods can be acquired through jurisprudential immersion alone, the gate is more permeable and the constraint is less extractive. This affects the directionality computation for moderate-power jurists who lack philosophical training.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rationalist_training_prerequisite_gatekeeping, empirical, 'Whether access to the Hanafi methodological framework is gated by formal rationalist training.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanafi_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__hanafi_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(usul_tr_t8, usul_al_fiqh_method__hanafi_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(usul_tr_t16, usul_al_fiqh_method__hanafi_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(usul_tr_t25, usul_al_fiqh_method__hanafi_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement(usul_tr_t35, usul_al_fiqh_method__hanafi_reading, theater_ratio, 35, 0.26).
narrative_ontology:measurement(usul_tr_t50, usul_al_fiqh_method__hanafi_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(usul_be_t8, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(usul_be_t16, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(usul_be_t25, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 25, 0.65).
narrative_ontology:measurement(usul_be_t35, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 35, 0.67).
narrative_ontology:measurement(usul_be_t50, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(usul_su_t8, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(usul_su_t16, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(usul_su_t25, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 25, 0.49).
narrative_ontology:measurement(usul_su_t35, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 35, 0.51).
narrative_ontology:measurement(usul_su_t50, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanafi_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__hanafi_reading, 0.18).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__hanbali_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__shafii_reading).

% DUAL FORMULATION NOTE:
% The constraint 'usul_al_fiqh_method__hanafi_reading' is one reading of the contested kernel 'usul_al_fiqh_method'. Four sibling readings (Hanbali, Maliki, Shafi'i) instantiate alternative methodological frameworks governing the same foundational problem: how to derive legal rules when textual sources are incomplete. Each reading has structurally distinct ε-values (Hanafi is highest-extraction, Hanbali is lowest, Maliki and Shafi'i are intermediate). They are networked via affects_constraints because changes in the institutional dominance of one reading (e.g., rationalist centers adopting Hanafi methods more aggressively) create pressure on the alternatives. The readings coexist across different regions and institutional contexts but do not share the same jurisprudential authority structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(usul_al_fiqh_method__hanafi_reading, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
