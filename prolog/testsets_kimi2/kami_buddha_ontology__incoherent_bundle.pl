% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__incoherent_bundle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__incoherent_bundle, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: kami_buddha_ontology__incoherent_bundle
 *   human_readable: Shinbutsu-shugo Incoherent Bundle
 *   domain: religious_studies/japanese_religious_history
 *
 * SUMMARY:
 *   Shinbutsu-shugo â the historical fusion of kami and buddha worship in
 *   Japan â is analyzed here under the incoherent_bundle reading of the
 *   kami_buddha_ontology kernel. This reading holds that the arrangement
 *   constitutes no coherent ontological kernel, but rather an institutionally
 *   sustained bundle of contradictory commitments: simultaneous fusion and
 *   separation, hierarchical and reciprocal, systematized and unsystematized.
 *   The constraint persists not because a single theory justifies it, but
 *   because shrine-temple multiplex institutions and ritual specialists
 *   extract authority and material support from the ambiguity, while ordinary
 *   practitioners bear the cognitive and financial costs of unresolved
 *   contradiction, and shinbutsu bunri reformers bear the costs of
 *   suppression. Separation attempts have historically failed because the
 *   institutional mesh actively resists clarification. This story treats the
 *   bundle as the standing arrangement under contest, assessed by this
 *   reading's own lights.
 *
 * KEY AGENTS:
 *   - shrine_temple_institutions: Primary agenda-setter (institutional/arbitrage) â sustains the bundle and resists separation.
 *   - ritual_specialist_class: Primary beneficiary (organized/constrained) â derives professional status from navigating contradictions.
 *   - ordinary_practitioners: Primary target (powerless/identity_locked) â bears material and cognitive costs of ontological ambiguity.
 *   - shinbutsu_bunri_reformers: Secondary target (moderate/constrained) â advocates of doctrinal clarity suppressed by the institutional mesh.
 *   - modern_scholars: Analytical observer (analytical/analytical) â evaluates the bundle from outside benefiting institutions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, 0.62).
domain_priors:suppression_score(kami_buddha_ontology__incoherent_bundle, 0.58).
domain_priors:theater_ratio(kami_buddha_ontology__incoherent_bundle, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, extractiveness, 0.62).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__incoherent_bundle, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__incoherent_bundle, "Shinbutsu-shugo Incoherent Bundle").
narrative_ontology:topic_domain(kami_buddha_ontology__incoherent_bundle, "religious_studies/japanese_religious_history").

domain_priors:requires_active_enforcement(kami_buddha_ontology__incoherent_bundle).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__incoherent_bundle, '0654a4e0-b464-4038-bacd-8d5ca906bb48').
narrative_ontology:cs_kernel_codification('0654a4e0-b464-4038-bacd-8d5ca906bb48', implicit).
narrative_ontology:cs_authority_grounding('0654a4e0-b464-4038-bacd-8d5ca906bb48', extraction).
narrative_ontology:cs_interpretation_layer_present('0654a4e0-b464-4038-bacd-8d5ca906bb48').
narrative_ontology:cs_reading_relation('0654a4e0-b464-4038-bacd-8d5ca906bb48', kami_buddha_ontology__honji_suijaku_monism, coexists_with).
narrative_ontology:cs_reading_relation('0654a4e0-b464-4038-bacd-8d5ca906bb48', kami_buddha_ontology__domain_partition, coexists_with).
narrative_ontology:cs_axiom('0654a4e0-b464-4038-bacd-8d5ca906bb48', foundational, ritual_efficacy_independent_of_ontology).
narrative_ontology:cs_axiom_status(ritual_efficacy_independent_of_ontology, holdable).
narrative_ontology:cs_axiom_grounding('0654a4e0-b464-4038-bacd-8d5ca906bb48', ritual_efficacy_independent_of_ontology, instrumental).
narrative_ontology:cs_axiom('0654a4e0-b464-4038-bacd-8d5ca906bb48', secondary, institutional_ambiguity_as_authority_source).
narrative_ontology:cs_axiom_status(institutional_ambiguity_as_authority_source, holdable).
narrative_ontology:cs_axiom_grounding('0654a4e0-b464-4038-bacd-8d5ca906bb48', institutional_ambiguity_as_authority_source, conventional).
narrative_ontology:cs_reference_frame('0654a4e0-b464-4038-bacd-8d5ca906bb48', premodern_syncretic_practice).
narrative_ontology:cs_drift_state('0654a4e0-b464-4038-bacd-8d5ca906bb48', post_meiji_modernity, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('0654a4e0-b464-4038-bacd-8d5ca906bb48', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, shrine_temple_institutions).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, ritual_specialist_class).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, imperial_state).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, ordinary_practitioners).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, shinbutsu_bunri_reformers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the multiplex institutions that fuse kami and buddha worship, deriving authority, land, and revenue from the ambiguous boundary. Actively resist doctrinal separation because clarification would reduce institutional flexibility and bifurcate their resource base.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, shrine_temple_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Perform rituals that simultaneously invoke kami and buddhas, benefiting professionally from the lack of clear doctrinal boundaries that would force exclusive specialization. Their social status depends on expertise in navigating contradictions that laypeople cannot resolve.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, ritual_specialist_class, beneficiary,
    organized, biographical, constrained, regional).

% Participate in community rituals and life-cycle ceremonies that demand simultaneous commitment to contradictory ontologies, such as worshipping a kami that is also a buddha. Bear the material costs of dual affiliation and the cognitive costs of unresolved doctrinal tension.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, ordinary_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Advocated for clear separation of Shinto and Buddhism on doctrinal, political, or purificatory grounds. Their campaigns were institutionally suppressed, absorbed, or failed because the multiplex structure had no interest in clarification. They bear the cost of marginalization and failed reform.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, shinbutsu_bunri_reformers, payer,
    moderate, generational, constrained, national).

% Analyze the historical record and conclude that no coherent ontology governed the practice; observe the institutional maintenance of contradiction from a position outside the benefiting institutions.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, modern_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective religious practice across shrines and temples by allowing simultaneous worship of kami and buddhas without requiring doctrinal resolution, enabling shared festivals, multiplex institutions, and integrated ritual calendars.
% TRANSFER_FUNCTION: Moves material offerings, labor, and symbolic deference from ordinary practitioners to shrine-temple institutions and ritual specialists, while moving the cost of unresolved ontological contradiction and suppressed reform onto practitioners and shinbutsu bunri advocates.
% ABSENT_VOICES: Shinbutsu bunri reformers seeking doctrinal purity were historically marginalized or excluded from institutional councils; ordinary practitioners who might prefer a single, coherent religious identity have no sanctioned channel to demand it within the multiplex structure.
% DISAPPEARANCE_RATIONALE: If the bundle vanished, shrines and temples would be forced to clarify exclusive ontological claims, ritual calendars would split, practitioner affiliations would reorganize around clarified identities, and the institutional multiplex would collapse into separate religious orders.
% FOUNDING_PROBLEM: How to integrate immigrant Buddhist institutions and indigenous kami worship into a functioning religious economy without forcing resolution of incompatible ontological claims.
% FOUNDING_PROBLEM_CORROBORATION: Modern historians and scholars of Japanese religion attest from outside the benefiting institutions that the founding integration problem was resolved centuries ago and the bundle now persists by inertia and institutional interest; shrine-temple institutions themselves claim the fusion remains metaphysically necessary.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__incoherent_bundle, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__incoherent_bundle, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__incoherent_bundle, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kami_buddha_ontology__incoherent_bundle, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__incoherent_bundle, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__incoherent_bundle_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kami_buddha_ontology__incoherent_bundle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-to-high: the bundle extracts material offerings, labor, and compliance from practitioners by preventing ontological resolution that would reduce dependence on institutional mediation. Suppression (0.58) reflects active institutional resistance to shinbutsu bunri and the structural foreclosure of doctrinal clarity. Theater ratio (0.55) is elevated because a significant portion of institutional activity involves performative assertions of harmony or seamless integration that mask underlying logical contradiction. Accessibility collapse (0.60) is substantial because alternatives (exclusive Shinto or Buddhism as comprehensive life options) are institutionally foreclosed for participants. Resistance (0.45) is moderate: reform movements have repeatedly challenged the bundle but remain marginalized. The measurement series share a single time grid to prevent misalignment.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional seat, the bundle is a living, functional coordination of Japanese religious life; from the practitioner and reformer seats, it is an extractive structure that forecloses clarity and exit. The engine computes this divergence from the structural data rather than adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   Shrine-temple institutions and the ritual specialist class are structural beneficiaries (low d): the ambiguity subsidizes their authority, revenue, and professional niche. Ordinary practitioners are targets (high d): they pay in resources and identity-locked participation. Shinbutsu bunri reformers are also high-d targets whose suppression is an enforcement input. Modern scholars occupy an analytical seat with neutral d. The engine will compute divergent per-seat classifications from this asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â integrating Buddhist and kami cults into a single religious economy â was substantially resolved by the early modern period. The bundle's persistence beyond that point, combined with rising theater ratio and continued suppression of separation, signals mandatrophy: the coordination function has atrophied into institutional maintenance. However, because ritual efficacy and social integration remain real for practitioners, the constraint is not a pure piton; it retains a genuine coordination function alongside extraction, warranting tangled_rope rather than snare or piton. The R5 genealogy interview (founding_problem_status dead, disappearance_verdict world_rearranges) flags the mandatrophy for downstream piton-path analysis without forcing reclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_inertia_vs_cultural_imperative,
    'Is the persistence of the shinbutsu-shugo bundle driven primarily by institutional self-interest of shrine-temple multiplexes, or by a deep-seated cultural preference for non-exclusive religious participation?',
    'Comparative analysis with other religious cultures that lack institutional multiplexes but show similar syncretic practice; assessment of practitioner behavior after the Meiji separation when institutional incentives were removed.',
    'If cultural imperative dominates, the constraint is closer to identity_coordination with lower extraction; if institutional inertia dominates, extraction is higher and the constraint is more snare-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_inertia_vs_cultural_imperative, empirical, 'Whether the bundle''s persistence is cultural or institutional.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of separation attempts structural (state enforcement, institutional discipline) or internalized (practitioners believe the fusion is natural and unproblematic)?',
    'Post-Meiji practitioner surveys and behavior: if separation continued to be resisted by practitioners even after state enforcement ended, suppression is partially internalized.',
    'If internalized, effective suppression is higher than structural measures suggest, and the constraint is more deeply entrenched than institutional analysis indicates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism.').

omega_variable(
    bundle_reading_framing,
    'Does the incoherent-bundle reading accurately describe the premodern system, or does it project modern scholarly skepticism onto actors who experienced the system as coherent?',
    'Close historical study of premodern doctrinal texts and ritual manuals to assess whether practitioners themselves perceived contradiction or seamless integration.',
    'If premodern actors perceived coherence, the incoherent-bundle reading is an analytical misprojection and the constraint should be classified as a commitment system with a functioning interpretation layer rather than a degraded bundle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bundle_reading_framing, conceptual, 'Whether the incoherence is emic or etic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__incoherent_bundle, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__incoherent_bundle, theater_ratio, 0, 0.15).
narrative_ontology:measurement(kami_tr_t20, kami_buddha_ontology__incoherent_bundle, theater_ratio, 20, 0.22).
narrative_ontology:measurement(kami_tr_t40, kami_buddha_ontology__incoherent_bundle, theater_ratio, 40, 0.35).
narrative_ontology:measurement(kami_tr_t60, kami_buddha_ontology__incoherent_bundle, theater_ratio, 60, 0.45).
narrative_ontology:measurement(kami_tr_t80, kami_buddha_ontology__incoherent_bundle, theater_ratio, 80, 0.52).
narrative_ontology:measurement(kami_tr_t100, kami_buddha_ontology__incoherent_bundle, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(kami_be_t20, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(kami_be_t40, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(kami_be_t60, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(kami_be_t80, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 80, 0.6).
narrative_ontology:measurement(kami_be_t100, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 100, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kami_buddha_ontology__incoherent_bundle, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__incoherent_bundle, identity_coordination).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, honji_suijaku_monism).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, domain_partition).

% DUAL FORMULATION NOTE:
% The kami_buddha_ontology kernel decomposes into three structurally distinct readings: honji_suijaku_monism asserts ontological identity; domain_partition asserts clean separation; incoherent_bundle denies both and claims the kernel is an institutionally sustained bundle of contradictions. Each reading has a different epsilon, stakeholder structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
