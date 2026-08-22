% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__syncretic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__syncretic_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: shinbutsu_ontological_commitment__syncretic_reading
 *   human_readable: Honji-Suijaku Ontological Integration of Kami and Buddhas
 *   domain: religious/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   Under honji-suijaku (本地垂迹) metaphysics, Japanese kami are understood as
 *   trace manifestations (suijaku) of buddhas or bodhisattvas who are their
 *   original ground (honji). This reading instantiates a unified cosmological
 *   order where Buddhist doctrinal hierarchy provides the ontological
 *   framework and Shinto kami occupy determinate positions within it. The
 *   constraint operates from late Nara through Kamakura periods (c.
 *   750–1250), achieving peak institutional integration in Heian when
 *   temple-shrine multiplexes (jingūji) became the dominant religious
 *   institution. Buddhist hierarchies benefit through control of ritual
 *   economy, landholding, and doctrinal authority; Shinto shrine lineages
 *   lose autonomous ritual prerogatives and are administratively absorbed.
 *   The constraint requires active enforcement through institutional merger,
 *   ritual standardization, and doctrinal policing. Extraction is moderate —
 *   the coordination function (unified ritual cosmos, shared sacred
 *   geography) is genuine but asymmetrically priced.
 *
 * KEY AGENTS:
 *   - buddhist_institutional_hierarchy: Primary beneficiary (institutional/generational/arbitrage) — controls doctrinal framework, ritual economy, temple-shrine multiplex administration
 *   - autonomous_shinto_shrine_lineages: Primary victim (organized/biographical/constrained) — absorbed into multiplexes, lose independent ritual authority, land base reduced
 *   - syncretic_ritual_specialists: Secondary beneficiary (organized/biographical/mobile) — specialized honji-suijaku ritualists who mediate the integration
 *   - imperial_court_ritual_bureaucracy: Agenda setter (institutional/generational/arbitrage) — authorizes honji-suijaku mappings, regulates jingūji system
 *   - indigenous_ritual_practitioners: Victim (powerless/biographical/trapped) — local kami cults with no institutional protection, fully subordinated
 *   - non_integrated_local_kami_cults: Victim (powerless/biographical/trapped) — kami without honji assignments, marginalized or suppressed
 *   - analytical_observer: Observer (analytical/civilizational/analytical) — sees full structural asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__syncretic_reading, 0.32).
domain_priors:suppression_score(shinbutsu_ontological_commitment__syncretic_reading, 0.58).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__syncretic_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__syncretic_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__syncretic_reading, "Honji-Suijaku Ontological Integration of Kami and Buddhas").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__syncretic_reading, "religious/japanese_history/ontology_of_practice").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment__syncretic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__syncretic_reading, '9bfc7379-c97a-47a3-b919-a1f44ad211a3').
narrative_ontology:cs_kernel_codification('9bfc7379-c97a-47a3-b919-a1f44ad211a3', distributed).
narrative_ontology:cs_authority_grounding('9bfc7379-c97a-47a3-b919-a1f44ad211a3', practice).
narrative_ontology:cs_interpretation_layer_present('9bfc7379-c97a-47a3-b919-a1f44ad211a3').
narrative_ontology:cs_reading_relation('9bfc7379-c97a-47a3-b919-a1f44ad211a3', shinbutsu_ontological_commitment__partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('9bfc7379-c97a-47a3-b919-a1f44ad211a3', shinbutsu_ontological_commitment__incoherence_reading, influences).
narrative_ontology:cs_axiom('9bfc7379-c97a-47a3-b919-a1f44ad211a3', foundational, kami_are_suijaku_of_buddha_honji).
narrative_ontology:cs_axiom_status(kami_are_suijaku_of_buddha_honji, holdable).
narrative_ontology:cs_axiom_grounding('9bfc7379-c97a-47a3-b919-a1f44ad211a3', kami_are_suijaku_of_buddha_honji, deontological).
narrative_ontology:cs_axiom('9bfc7379-c97a-47a3-b919-a1f44ad211a3', secondary, unified_cosmology_requires_buddhist_doctrinal_frame).
narrative_ontology:cs_axiom_status(unified_cosmology_requires_buddhist_doctrinal_frame, holdable).
narrative_ontology:cs_axiom_grounding('9bfc7379-c97a-47a3-b919-a1f44ad211a3', unified_cosmology_requires_buddhist_doctrinal_frame, conventional).
narrative_ontology:cs_reference_frame('9bfc7379-c97a-47a3-b919-a1f44ad211a3', heian_jinguji_equilibrium).
narrative_ontology:cs_drift_state('9bfc7379-c97a-47a3-b919-a1f44ad211a3', kamakura_shinto_revival_emergence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9bfc7379-c97a-47a3-b919-a1f44ad211a3', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, buddhist_institutional_hierarchy).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, syncretic_ritual_specialists).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, imperial_court_ritual_bureaucracy).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, autonomous_shinto_shrine_lineages).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, indigenous_ritual_practitioners).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, non_integrated_local_kami_cults).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the honji-suijaku doctrinal framework: assigns honji to kami, authorizes jingūji temple-shrine multiplexes, collects revenue from integrated ritual economies, and holds doctrinal authority over the unified cosmology. Can reformulate honji assignments without losing institutional base — exit is doctrinal pivot, not institutional collapse.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, buddhist_institutional_hierarchy, beneficiary,
    institutional, generational, arbitrage, national).

% Hereditary shrine lineages (shake) that previously held autonomous ritual authority over kami. Under honji-suijaku, they are absorbed into temple-shrine multiplexes under Buddhist administrative control, lose independent ritual prerogatives, and see land holdings transferred to temple complexes. Exit options: negotiate favorable honji assignment (constrained), go underground as folk practice (marginalized), or accept absorption.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, autonomous_shinto_shrine_lineages, payer,
    organized, biographical, constrained, regional).

% Specialist ritualists (often from onmyōdō or esoteric Buddhist lineages) who perform honji-suijaku rituals, produce honji assignment texts, and mediate kami-buddha integration at court and major temples. They benefit from the constraint's enforcement (demand for their expertise) but can pivot to other ritual specializations if the framework shifts — mobile exit.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, syncretic_ritual_specialists, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__syncretic_reading, syncretic_ritual_specialists, agenda_setter).

% Court bureaus (Jingikan, later Shinto affairs offices) that authorize honji assignments, regulate jingūji system, and integrate the unified cosmology into state ritual calendar. They set the agenda for which kami receive which honji and which temples control which shrines. Can pivot to alternative ritual frameworks (as Meiji shows) — arbitrage exit.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, imperial_court_ritual_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).

% Local ritual practitioners (miko, ascetics, folk specialists) serving kami without institutional affiliation. They bear the full cost of honji-suijaku imposition: their kami are assigned Buddhist honji without consultation, their rituals are standardized or suppressed, and they have no institutional vehicle for resistance. Exit means abandoning practice or going fully underground — trapped.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, indigenous_ritual_practitioners, payer,
    powerless, biographical, trapped, local).

% Kami cults that lack honji assignments or temple affiliation — mountain kami, boundary kami, tutelary kami of marginal communities. They are structurally invisible to the honji-suijaku system: either ignored (no extraction but no recognition) or suppressed as 'unenlightened' manifestations. No exit within the system; persistence only at margins.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, non_integrated_local_kami_cults, payer,
    powerless, biographical, trapped, local).

% Modern scholarly position that sees the full structural asymmetry: Buddhist doctrinal framework as active agent, Shinto autonomy as passive recipient, honji-suijaku as the mechanism. Neither collects nor pays; analyzes the constraint's operation across the interval.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified cosmological and ritual order integrating kami worship into Buddhist doctrinal framework: solves the 'two religions' problem by giving every kami a determinate Buddhist honji, enabling shared sacred geography, standardized ritual calendar, and single institutional regulation (jingūji system).
% TRANSFER_FUNCTION: Moves institutional authority (ritual prerogative, land control, doctrinal definition) from autonomous Shinto shrine lineages to Buddhist temple hierarchies and court bureaucracy, via the honji assignment mechanism that makes kami subordinate manifestations of buddhas.
% ABSENT_VOICES: Autonomous Shinto lineages that maintained independent ritual genealogies (e.g., Ise, Izumo before full integration), folk practitioners of non-honji-assigned kami, and early medieval Shinto revivalists (Yoshida, Ise Shinto precursors) who would later contest the synthesis — they were structurally excluded from honji assignment authority.
% DISAPPEARANCE_RATIONALE: If honji-suijaku vanished overnight, the jingūji institutional system would collapse, shrine-temple multiplexes would separate, Buddhist doctrinal authority over kami would evaporate, and Shinto lineages would reclaim autonomous ritual authority — the entire Heian-Kamakura religious institutional landscape would reorganize.
% FOUNDING_PROBLEM: How to integrate kami worship into a Buddhist cosmological order without denying the reality of either — i.e., how to make kami intelligible within a Buddhist framework that claims universal explanatory scope, while preserving the ritual efficacy that made kami worship politically and socially indispensable.
% FOUNDING_PROBLEM_CORROBORATION: Buddhist institutional tradition (Tendai, Shingon, Tendai-enryaku records) attests the problem is still live: kami still require honji for soteriological inclusion. Shinto revivalist traditions (Yoshida, Ise, Hirata Atsutane) attest the problem is dead: integration was always subordination, not synthesis. Modern scholarship (Kuroda Toshio, Teeuwen, Rambelli) corroborates the contested status from outside both traditions — the founding problem's status depends on which institutional lineage's self-understanding one credits.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__syncretic_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__syncretic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__syncretic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__syncretic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__syncretic_reading, 0.32, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__syncretic_reading_tests).
:- end_tests(shinbutsu_ontological_commitment__syncretic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.32) reflects genuine coordination (unified ritual cosmos solves fragmentation) but asymmetric pricing: Buddhist hierarchy captures institutional rents (land, prerogative, doctrinal monopoly) while Shinto lineages pay through absorption. Suppression (0.58) is structural: jingūji system administratively merges shrines into temples, honji assignments are imposed by Buddhist authorities, resistance met with institutional override. Theater ratio (0.28) rises over time as honji-suijaku becomes ritual orthodoxy — later enforcement performs doctrinal conformity more than coordinates practice. Accessibility collapse (0.67) is high: once honji-suijaku framework is accepted, alternative ontologies (autonomous kami, domain separation) become cognitively and institutionally difficult. Resistance (0.42) is moderate: some shrine lineages negotiate honji assignments, folk practice persists at margins, but no coherent counter-framework emerges until medieval Shinto revival.
 *
 * PERSPECTIVAL GAP:
 *   From the Buddhist hierarchy seat, honji-suijaku is genuine coordination — it extends Buddhist compassion to kami, unifies the ritual cosmos, solves the 'two religions' problem. From the autonomous shrine seat, the same structure is extraction — their autonomy is the price of integration. From the court bureaucracy seat, it is administrative rationalization — one system to regulate instead of two. The engine computes per-seat types from these structural positions; the syncretic reading claims tangled_rope because both coordination and asymmetric extraction are structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist institutional hierarchy is the structural beneficiary: it sets the ontological terms (honji assignments), collects the institutional rents (temple-shrine multiplex revenue, doctrinal authority), and holds arbitrage-grade exit (can reformulate doctrine without losing institutional base). Autonomous Shinto lineages are structural targets: they bear absorption costs, have constrained exit (must negotiate within Buddhist framework or go underground), and hold organized but not institutional power. Imperial court bureaucracy sits near agenda_setter with arbitrage exit — it authorizes the system but can pivot to alternative ritual frameworks (as seen in Meiji). Indigenous practitioners are trapped: no institutional vehicle, exit means cultural erasure. The directionality derivation from beneficiary/victim declarations + power/exit produces the expected d-gradient.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Heian: 'how to integrate kami worship into a Buddhist cosmological order without denying either') is contested in status. Buddhist tradition holds it live (kami still need honji); Shinto revivalists hold it dead (integration was always subordination); modern scholars contest. The constraint persists past its founding moment because the institutional machinery (jingūji, honji assignment bureaucracy) develops self-sustaining interests. Mandatrophy is unresolved: the coordination function (unified ritual order) atrophies into doctrinal performance while extraction (Buddhist institutional capture) persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_kernel_reading,
    'Is the honji-suijaku metaphysics a genuine cosmological synthesis or a strategic doctrinal framework that subordinates Shinto autonomy under Buddhist institutional authority?',
    'Comparative analysis of textual doctrine (honji-suijaku treatises) against institutional power distributions (temple-shrine multiplex control, landholding patterns, ritual prerogatives) across the Heian-Kamakura transition.',
    'If strategic subordination, the constraint is a tangled_rope with Buddhist hierarchy as primary beneficiary and Shinto autonomy as structural victim; if genuine synthesis, the coordination function is more symmetrical and extraction lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_kernel_reading, conceptual, 'Whether this reading represents authentic ontological integration or institutional capture framed as synthesis').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of autonomous Shinto lineages structural (institutional absorption into temple-shrine multiplexes, land expropriation, ritual prerogative transfer) or internalized (Shinto practitioners adopting Buddhist frameworks as self-evident cosmology)?',
    'Longitudinal study of shrine documentary records: persistence of independent ritual genealogies, resistance to honji-suijaku categorization in local practice, and post-Meiji restoration of ''pure'' Shinto forms.',
    'If substantially internalized, effective suppression is higher than institutional measures suggest — the constraint persists in practitioner cognition after structural enforcement relaxes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of Shinto autonomy under honji-suijaku').

omega_variable(
    kernel_reading_boundary,
    'Does the syncretic_reading''s claim of unified cosmological order FORECLOSE the partition_reading''s domain separation, or do they COEXIST as different institutional framings?',
    'Examine whether any single institutional actor (temple complex, court bureau) simultaneously operated both frameworks for different purposes — i.e., honji-suijaku for doctrinal legitimacy, domain separation for ritual administration.',
    'If forecloses, the readings are mutually exclusive within one framework; if coexists_with, they are complementary institutional strategies deployed situationally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Logical relationship between syncretic and partition readings of shinbutsu ontological commitment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__syncretic_reading, 750, 1250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_syncretic_tr_t750, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 750, 0.12).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t850, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 850, 0.18).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t950, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 950, 0.25).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t1050, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1050, 0.3).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t1150, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1150, 0.28).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t1250, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1250, 0.28).

% Extraction over time
narrative_ontology:measurement(shinbutsu_syncretic_be_t750, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 750, 0.18).
narrative_ontology:measurement(shinbutsu_syncretic_be_t850, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 850, 0.24).
narrative_ontology:measurement(shinbutsu_syncretic_be_t950, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 950, 0.31).
narrative_ontology:measurement(shinbutsu_syncretic_be_t1050, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1050, 0.34).
narrative_ontology:measurement(shinbutsu_syncretic_be_t1150, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1150, 0.33).
narrative_ontology:measurement(shinbutsu_syncretic_be_t1250, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1250, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_syncretic_su_t750, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 750, 0.35).
narrative_ontology:measurement(shinbutsu_syncretic_su_t850, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 850, 0.48).
narrative_ontology:measurement(shinbutsu_syncretic_su_t950, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 950, 0.6).
narrative_ontology:measurement(shinbutsu_syncretic_su_t1050, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1050, 0.62).
narrative_ontology:measurement(shinbutsu_syncretic_su_t1150, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1150, 0.58).
narrative_ontology:measurement(shinbutsu_syncretic_su_t1250, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1250, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__syncretic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_commitment__syncretic_reading, 0.1).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment__partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment__incoherence_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, jinguji_institutional_system).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, honji_assignment_bureaucracy).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, meiji_shinbutsu_bunri_policy).

% DUAL FORMULATION NOTE:
% The shinbutsu_ontological_commitment kernel decomposes into three constraint stories: syncretic_reading (this file, honji-suijaku as genuine synthesis with asymmetric extraction), partition_reading (domain separation without ontological integration, lower extraction), and incoherence_reading (no stable commitment, minimal coordination function). The syncretic_reading has highest extractiveness because it posits an active doctrinal framework that subordinates Shinto; the partition_reading has lower extractiveness because domain separation allows mutual autonomy; the incoherence_reading has near-zero extractiveness because no enforcement machinery exists. They are linked by shared institutional history (jingūji system, honji assignments) but differ in ε because they identify different standing arrangements under contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_ontological_commitment__syncretic_reading, institutional, 0.15).
constraint_indexing:directionality_override(shinbutsu_ontological_commitment__syncretic_reading, organized, 0.75).
constraint_indexing:directionality_override(shinbutsu_ontological_commitment__syncretic_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
