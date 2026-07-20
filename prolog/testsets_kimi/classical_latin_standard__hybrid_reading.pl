% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_classical_latin_standard__hybrid_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: classical_latin_standard__hybrid_reading
 *   human_readable: Hybrid Classical Latin Standard (Classical Fidelity + Domain Accommodation)
 *   domain: historical_linguistics/philology/commitment_systems
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid_reading of the
 *   classical_latin_standard kernel: it holds that Correct Latin requires
 *   fidelity to Classical norms while recognizing legitimate post-Classical
 *   developments in technical and ecclesiastical domains. It is one of three
 *   readings (continuity, hybrid, reconstruction) of a contested kernel. The
 *   constraint operates as a linguistic standard enforced by humanist
 *   scholars and adopted by ecclesiastical institutions, extracting from
 *   vernacular-influenced writers through delegitimization of their forms as
 *   barbarisms. The hybrid reading reduces the victim set relative to pure
 *   reconstruction but maintains active enforcement of the
 *   Classical/barbarism boundary.
 *
 * KEY AGENTS:
 *   - Humanist scholars (agenda_setter): Define and police the hybrid standard across European academies and cathedral schools.
 *   - Ecclesiastical institutions (beneficiary): Retain legitimized technical/ecclesiastical vocabulary while benefiting from Classical prestige.
 *   - Vernacular-influenced writers (payer): Bear the cost of delegitimization when their Latin deviates from the hybrid norm.
 *   - Medieval continuators (excluded): Absent voices who treat Latin as a living, evolving language.
 *   - Philological analysts (observer): Analytical seat observing the contest between readings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__hybrid_reading, 0.58).
domain_priors:suppression_score(classical_latin_standard__hybrid_reading, 0.55).
domain_priors:theater_ratio(classical_latin_standard__hybrid_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(classical_latin_standard__hybrid_reading, "Hybrid Classical Latin Standard (Classical Fidelity + Domain Accommodation)").
narrative_ontology:topic_domain(classical_latin_standard__hybrid_reading, "historical_linguistics/philology/commitment_systems").

domain_priors:requires_active_enforcement(classical_latin_standard__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__hybrid_reading, 'f0887a36-986b-4f9d-adca-e43fade5a884').
narrative_ontology:cs_kernel_codification('f0887a36-986b-4f9d-adca-e43fade5a884', fixed_text).
narrative_ontology:cs_authority_grounding('f0887a36-986b-4f9d-adca-e43fade5a884', lineage).
narrative_ontology:cs_interpretation_layer_present('f0887a36-986b-4f9d-adca-e43fade5a884').
narrative_ontology:cs_reading_relation('f0887a36-986b-4f9d-adca-e43fade5a884', classical_latin_standard__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('f0887a36-986b-4f9d-adca-e43fade5a884', classical_latin_standard__reconstruction_reading, influences).
narrative_ontology:cs_axiom('f0887a36-986b-4f9d-adca-e43fade5a884', foundational, classical_textual_fidelity_primary).
narrative_ontology:cs_axiom_status(classical_textual_fidelity_primary, holdable).
narrative_ontology:cs_axiom_grounding('f0887a36-986b-4f9d-adca-e43fade5a884', classical_textual_fidelity_primary, conventional).
narrative_ontology:cs_axiom('f0887a36-986b-4f9d-adca-e43fade5a884', foundational, technical_ecclesiastical_accommodation).
narrative_ontology:cs_axiom_status(technical_ecclesiastical_accommodation, holdable).
narrative_ontology:cs_axiom_grounding('f0887a36-986b-4f9d-adca-e43fade5a884', technical_ecclesiastical_accommodation, conventional).
narrative_ontology:cs_reference_frame('f0887a36-986b-4f9d-adca-e43fade5a884', classical_golden_age_norm).
narrative_ontology:cs_drift_state('f0887a36-986b-4f9d-adca-e43fade5a884', high_medieval_synthesis, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f0887a36-986b-4f9d-adca-e43fade5a884', '').
narrative_ontology:cs_kernel_id(classical_latin_standard__hybrid_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, ecclesiastical_institutions).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, vernacular_influenced_writers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and administer the hybrid standard, determining which post-Classical forms in technical and ecclesiastical domains are legitimate and which constitute barbarisms. They publish grammars, edit texts, and train clerics in approved usage. Their authority depends on maintaining the boundary between Classical fidelity and illegitimate drift.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, humanist_scholars, agenda_setter,
    organized, generational, mobile, continental).

% Adopt Classical norms for liturgical, theological, and diplomatic communication while retaining legitimized medieval technical and ecclesiastical vocabulary. They benefit from the prestige of Classical continuity without having to purge all post-Classical developments. Their exit is constrained by doctrinal commitment to tradition and the sunk cost of centuries of Latin textual production.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, ecclesiastical_institutions, beneficiary,
    institutional, generational, constrained, continental).

% Write in Latin influenced by local vernacular syntax and vocabulary. Their forms are systematically delegitimized as barbarisms under the hybrid standard, excluding them from ecclesiastical office, university admission, and scholarly publication. They may abandon Latin entirely or undergo costly humanist retraining to gain legitimacy.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, vernacular_influenced_writers, payer,
    moderate, biographical, constrained, regional).

% Practice Latin as a living, evolving language following natural drift patterns of the medieval period. They are absent from the standard-setting institutions because their acceptance of broad linguistic change contradicts the hybrid reading's commitment to Classical textual fidelity. They would argue for continuity but are not represented in the humanist academies or reform councils.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, medieval_continuators, excluded,
    moderate, biographical, constrained, regional).

% Observe the contest between continuity, hybrid, and reconstruction readings from a historical and sociolinguistic perspective, analyzing how the hybrid standard functions as both coordination mechanism and gatekeeping structure across the medieval-to-early-modern transition.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, philological_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(classical_latin_standard__hybrid_reading, diffuse).
narrative_ontology:fixing_cost_class(classical_latin_standard__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, prestigious written medium for transnational religion, scholarship, and diplomacy across fragmented vernacular Europe, solving the coordination problem of mutual intelligibility and trust in high-register communication.
% TRANSFER_FUNCTION: Moves communicative legitimacy and institutional access from writers of delegitimized barbarous forms to practitioners of the hybrid standard; concentrates gatekeeping authority and cultural prestige in humanist scholars and classical-educated clergy.
% ABSENT_VOICES: Medieval continuators who treat Latin as a naturally evolving language; reconstructionist philologists who reject all medieval accommodation; vernacular communities for whom the standard is an inaccessible imposition. These voices are excluded from the academies, cathedral schools, and curial offices where the standard is codified.
% DISAPPEARANCE_RATIONALE: If the hybrid standard vanished overnight, ecclesiastical and scholarly communication would lose its shared prestige medium; educational curricula would reorganize around vernaculars or pure reconstruction; the boundary between legitimate and illegitimate Latin would dissolve, rearranging the linguistic hierarchy of early modern Europe.
% FOUNDING_PROBLEM: The collapse of the Roman Empire fragmented written communication across Europe, creating a need for a transnational language of religion, scholarship, and diplomacy that vernaculars could not yet supply.
% FOUNDING_PROBLEM_CORROBORATION: Medieval historians and historical sociolinguists outside the beneficiary institutions corroborate that the coordination need was genuine in the early medieval period; they contest whether the hybrid standard still serves that function or has become an instrument of cultural gatekeeping.
narrative_ontology:disappearance_verdict(classical_latin_standard__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(classical_latin_standard__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__hybrid_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(classical_latin_standard__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(classical_latin_standard__hybrid_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(classical_latin_standard__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(classical_latin_standard__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(classical_latin_standard__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as tangled_rope because it combines a genuine coordination function (transnational high-register communication) with asymmetric extraction (delegitimization of vernacular-influenced forms to the benefit of classically trained elites). Extractiveness is moderate (0.58) because the hybrid accommodation legitimates some post-Classical developments, reducing the scope of extraction compared to a pure reconstructionist standard. Suppression is moderate (0.55) because the standard requires active policing of the barbarism boundary but does not suppress all alternatives. Theater_ratio (0.45) reflects increasing performative maintenance of Classical purity as humanist ideology intensifies. Accessibility_collapse (0.50) captures the partial collapse of alternatives: vernaculars exist but lose legitimacy in high-status domains. Resistance (0.45) reflects ongoing contestation from continuators and vernacular writers.
 *
 * PERSPECTIVAL GAP:
 *   From the humanist scholar seat, the constraint is a necessary restoration of textual fidelity that preserves civilization against decay. From the ecclesiastical institution seat, it is a pragmatic compromise between purity and functional tradition. From the vernacular-influenced writer seat, the same structure is an exclusionary gate that reserves institutional access for those with costly humanist training. The engine computes these divergent per-seat classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (ecclesiastical_institutions) derive legitimacy and continuity from the standard, placing them near the beneficiary end of directionality. Victims (vernacular_influenced_writers) bear the delegitimization cost, placing them near the target end. Humanist scholars, as agenda_setters, derive prestige and authority from administering the boundary; their directionality is closer to beneficiary than target because the constraint subsidizes their gatekeeping role. Medieval continuators are excluded from the conversation entirely, receiving no directionality weight.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was post-Roman communication fragmentation. The hybrid reading is specifically designed to resist mandatrophy by accommodating functional post-Classical developments, preventing the standard from becoming a pure piton of irrelevant archaism. However, if the coordination need for a transnational Latin medium has been superseded by vernacular standardization and modern state languages, the constraint may persist as cultural gatekeeping despite a dead founding problem. The contested founding_problem_status flags this ambiguity without resolving it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    barbarism_boundary_legitimacy,
    'Who decides the boundary between legitimate post-Classical technical/ecclesiastical development and illegitimate barbarism, and is that boundary principled or arbitrary?',
    'Historical corpus analysis comparing hybrid-standard grammars against actual usage patterns in technical and ecclesiastical texts to determine whether the boundary tracks functional differentiation or social exclusion.',
    'If the boundary is arbitrary, effective extraction is higher than measured because the suppression of vernacular-influenced forms serves status maintenance rather than coordination; if principled, the extraction is partly defensible as functional differentiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(barbarism_boundary_legitimacy, empirical, 'Whether the hybrid standard''s barbarism boundary is functionally principled or arbitrary').

omega_variable(
    kernel_reading_stability,
    'Does the hybrid reading represent a stable equilibrium, or is it a transitional stage that will slide toward continuity under vernacular pressure or toward reconstruction under purist pressure?',
    'Track institutional adoption over time: if hybrid grammars progressively accommodate more forms, they slide toward continuity; if they progressively restrict accommodation, they slide toward reconstruction.',
    'If transitional, the current moderate metrics misrepresent the constraint''s terminal type; a slide toward reconstruction would increase extractiveness and suppression, while a slide toward continuity would decrease them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_stability, conceptual, 'Whether the hybrid reading is stable or transitional between continuity and reconstruction').

omega_variable(
    extraction_vs_coordination_cost,
    'Is the delegitimization of barbarisms a necessary cost of maintaining a coordination standard across transnational domains, or does it constitute extractive gatekeeping by humanist institutions?',
    'Counterfactual analysis: compare communicative efficiency and institutional trust outcomes under the hybrid standard versus a more permissive continuity standard in comparable ecclesiastical or technical contexts.',
    'If the coordination benefit requires the exclusion, the extraction is partially offset by genuine coordination gain; if not, the constraint is more extractive than its tangled_rope classification suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_vs_coordination_cost, conceptual, 'Whether delegitimization is necessary coordination cost or extractive gatekeeping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__hybrid_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clas_tr_t0, classical_latin_standard__hybrid_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(clas_tr_t20, classical_latin_standard__hybrid_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(clas_tr_t40, classical_latin_standard__hybrid_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement(clas_tr_t60, classical_latin_standard__hybrid_reading, theater_ratio, 60, 0.39).
narrative_ontology:measurement(clas_tr_t80, classical_latin_standard__hybrid_reading, theater_ratio, 80, 0.42).
narrative_ontology:measurement(clas_tr_t100, classical_latin_standard__hybrid_reading, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(clas_be_t0, classical_latin_standard__hybrid_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(clas_be_t20, classical_latin_standard__hybrid_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(clas_be_t40, classical_latin_standard__hybrid_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(clas_be_t60, classical_latin_standard__hybrid_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(clas_be_t80, classical_latin_standard__hybrid_reading, base_extractiveness, 80, 0.55).
narrative_ontology:measurement(clas_be_t100, classical_latin_standard__hybrid_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clas_su_t0, classical_latin_standard__hybrid_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(clas_su_t20, classical_latin_standard__hybrid_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(clas_su_t40, classical_latin_standard__hybrid_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement(clas_su_t60, classical_latin_standard__hybrid_reading, suppression_requirement, 60, 0.5).
narrative_ontology:measurement(clas_su_t80, classical_latin_standard__hybrid_reading, suppression_requirement, 80, 0.53).
narrative_ontology:measurement(clas_su_t100, classical_latin_standard__hybrid_reading, suppression_requirement, 100, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__hybrid_reading, identity_coordination).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, classical_latin_standard__continuity_reading).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, classical_latin_standard__reconstruction_reading).

% DUAL FORMULATION NOTE:
% The classical_latin_standard kernel decomposes into three structurally distinct constraints: continuity_reading (living tradition, low extraction), hybrid_reading (Classical+domain accommodation, moderate extraction), and reconstruction_reading (archaeological return, high extraction). Each has a different epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
