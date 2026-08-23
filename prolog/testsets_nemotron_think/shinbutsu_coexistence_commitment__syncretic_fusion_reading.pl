% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__syncretic_fusion_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: shinbutsu_coexistence_commitment__syncretic_fusion_reading
 *   human_readable: Honji Suijaku Ontological Fusion (Syncretic Fusion Reading)
 *   domain: religious/historical/japanese
 *
 * SUMMARY:
 *   The honji suijaku (original ground, manifest trace) doctrine asserts that
 *   Japanese kami are local manifestations (suijaku) of universal Buddhist
 *   deities (honji). This reading, dominant in Tendai and Shingon
 *   institutional theology from the Heian through Edo periods, presents a
 *   single coherent ontology in which Buddhist truth is the ground and kami
 *   worship its skillful adaptation to Japanese conditions. The jinguji
 *   (shrine-temple complexes) served as the structural embodiment — physical
 *   sites where the unified ontology was ritually enacted and institutionally
 *   administered. Theological elites (Tendai/Shingon scholar-monks) held
 *   interpretive authority, policing doctrinal boundaries through texts like
 *   the Honjaku Engi and ritual manuals. The constraint coordinates a unified
 *   soteriological framework across the archipelago but extracts authority,
 *   land, labor, and cognitive assent from subordinated Shinto priesthoods
 *   and lay practitioners. The Meiji separation (shinbutsu bunri) was the
 *   constraint's violent dissolution, not its natural sunset.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.58).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.42).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "Honji Suijaku Ontological Fusion (Syncretic Fusion Reading)").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "religious/historical/japanese").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__syncretic_fusion_reading, '20b8dc03-0d89-41d4-b2c1-fae2a93ebbaf').
narrative_ontology:cs_kernel_codification('20b8dc03-0d89-41d4-b2c1-fae2a93ebbaf', formalized).
narrative_ontology:cs_authority_grounding('20b8dc03-0d89-41d4-b2c1-fae2a93ebbaf', lineage).
narrative_ontology:cs_interpretation_layer_present('20b8dc03-0d89-41d4-b2c1-fae2a93ebbaf').
narrative_ontology:cs_reading_relation('20b8dc03-0d89-41d4-b2c1-fae2a93ebbaf', shinbutsu_coexistence_commitment__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('20b8dc03-0d89-41d4-b2c1-fae2a93ebbaf', shinbutsu_coexistence_commitment__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('20b8dc03-0d89-41d4-b2c1-fae2a93ebbaf', foundational, buddha_primacy_over_kami).
narrative_ontology:cs_axiom_status(buddha_primacy_over_kami, holdable).
narrative_ontology:cs_axiom_grounding('20b8dc03-0d89-41d4-b2c1-fae2a93ebbaf', buddha_primacy_over_kami, deontological).
narrative_ontology:cs_axiom('20b8dc03-0d89-41d4-b2c1-fae2a93ebbaf', foundational, honji_suijaku_as_exhaustive_ontology).
narrative_ontology:cs_axiom_status(honji_suijaku_as_exhaustive_ontology, holdable).
narrative_ontology:cs_axiom_grounding('20b8dc03-0d89-41d4-b2c1-fae2a93ebbaf', honji_suijaku_as_exhaustive_ontology, deontological).
narrative_ontology:cs_axiom('20b8dc03-0d89-41d4-b2c1-fae2a93ebbaf', secondary, single_soteriological_framework).
narrative_ontology:cs_axiom_status(single_soteriological_framework, holdable).
narrative_ontology:cs_axiom_grounding('20b8dc03-0d89-41d4-b2c1-fae2a93ebbaf', single_soteriological_framework, conventional).
narrative_ontology:cs_reference_frame('20b8dc03-0d89-41d4-b2c1-fae2a93ebbaf', heian_tendai_shingon_synthesis).
narrative_ontology:cs_drift_state('20b8dc03-0d89-41d4-b2c1-fae2a93ebbaf', edo_tera_uke_peak, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('20b8dc03-0d89-41d4-b2c1-fae2a93ebbaf', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_theological_elite).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_institutional_network).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, lay_practitioners).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinto_priesthood_subordinated).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, heterodox_local_cults).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddha_primacy_over_kami).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__syncretic_fusion_reading, honji_suijaku_as_exhaustive_ontology).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__syncretic_fusion_reading, single_soteriological_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Tendai and Shingon scholar-monks who authored honji suijaku texts, controlled the interpretive curriculum, and administered the jinguji network. They could move between temples, attract imperial patronage, and define orthodoxy. Their exit options were high — they could shift institutional allegiance or retire to mountain monasteries — but they chose to maintain the system because it concentrated interpretive authority and resource flows in their hands.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_theological_elite, agenda_setter,
    institutional, generational, arbitrage, national).

% The network of shrine-temple complexes that physically embodied the unified ontology. They received shrine lands, tax exemptions, and ritual monopolies over local populations. Their exit was constrained by fixed assets (temple buildings, land grants) and institutional identity — a jinguji could not easily become a pure shrine or pure temple without losing its economic base. They maintained the system because it secured their material position.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_institutional_network, beneficiary,
    organized, generational, constrained, regional).

% Hereditary shrine priests (shinshoku) who lost independent ritual authority and land control when their shrines were absorbed into jinguji. They became functionaries performing Buddhist-led rites for kami redefined as Buddha's traces. Their exit was identity-locked: their priestly identity was hereditarily fused to the shrine, and the shrine's new Buddhist administration controlled their livelihood. They could not leave without abandoning their ancestral vocation and community standing.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinto_priesthood_subordinated, payer,
    organized, biographical, identity_locked, local).

% Village and urban commoners who gained a unified ritual calendar and shared pilgrimage routes but paid through temple registration (tera-uke system), mandatory donations, labor for temple construction, and cognitive assimilation to a Buddhist-centric worldview. Their exit was constrained by the tera-uke system (mandatory temple affiliation for legal status) and the absence of alternative ritual frameworks — the honji suijaku ontology made kami-only worship doctrinally incoherent within the dominant discourse.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, lay_practitioners, payer,
    moderate, biographical, constrained, local).

% Folk religious groups, mountain ascetics (shugenja), and local deity cults that did not fit the honji suijaku taxonomy. Their practices were reclassified as 'superstition' (meishin) or forcibly absorbed as 'manifest traces' of Buddhist deities. They had no voice in the theological discourse and faced active suppression when they resisted absorption. Their exit was trapped — geographic isolation and lack of institutional recognition left them no pathway to legitimacy.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, heterodox_local_cults, excluded,
    powerless, immediate, trapped, local).

% The court patronized both kami and Buddhist institutions, using the unified framework for legitimacy (emperor as descendant of kami, protected by Buddhas). It could observe the system from above, occasionally intervening to balance power (e.g., regulating jinguji land disputes), but was not structurally bound by the doctrinal constraint. Its exit was analytical — it could shift patronage or sponsor counter-theorizations (later Yoshida Shinto) without losing its own institutional coherence.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, imperial_court, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a single coherent soteriological framework integrating indigenous kami worship with imported Buddhism, enabling unified ritual calendars, shared pilgrimage networks, and a common moral cosmology across the archipelago — solving the coordination problem of two competing salvific systems claiming the same population.
% TRANSFER_FUNCTION: Moves interpretive authority, land tenure, tax revenue, ritual monopoly, and cognitive assent from Shinto priesthoods and lay practitioners to the Buddhist theological elite and jinguji institutional network, justified as the price of ontological unity.
% ABSENT_VOICES: Women's ritual communities (miko, onna-daishi), rural folk practitioners outside the tera-uke system, Korean and Chinese resident communities with distinct ritual traditions, and the heterodox_local_cults listed above — all structurally excluded from the theological discourse that defined the unified ontology.
% DISAPPEARANCE_RATIONALE: If honji suijaku vanished overnight, the jinguji network would lose its doctrinal justification and land claims; Shinto priesthoods would reclaim independent ritual authority; lay practitioners would face a fragmented ritual landscape without the unified calendar; the tera-uke registration system would collapse; and the imperial court would lose its primary cosmological legitimization framework. The Meiji shinbutsu bunri (1868) was the empirical test: the world violently rearranged.
% FOUNDING_PROBLEM: How to integrate indigenous Japanese kami worship with imported Buddhist soteriology without civilizational rupture, doctrinal contradiction, or political fragmentation — posed in the 7th-8th centuries as Buddhism gained state sponsorship alongside native rites.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested as dead by the Meiji state's own separation edicts (which presuppose the integration was complete and now must be undone), by Yoshida Shinto's 15th-century counter-theorization (which could only arise after the fusion was entrenched), and by kokugaku scholars (Motoori Norinaga, 18th century) who explicitly argue the fusion was a historical accident, not a living necessity. No corroboration from outside the Buddhist beneficiary set claims the problem remained live after the 12th century.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__syncretic_fusion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__syncretic_fusion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__syncretic_fusion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_coexistence_commitment__syncretic_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the systematic redirection of patronage, land, and ritual authority from kami institutions to Buddhist ones — the jinguji network captured shrine lands and redirected donor intent. Suppression (0.42) is moderate: the constraint operated more through doctrinal absorption than violent coercion, though heterodox local cults were disciplined. Theater ratio (0.28) reflects that the coordination function (unified ritual calendar, shared pilgrimage routes) was genuine, but a growing share of institutional activity served Buddhist institutional maintenance. Accessibility collapse (0.62) is high because the honji suijaku framework made alternative kami-only ontologies doctrinally illegible within the dominant discourse. Resistance (0.35) was present but fragmented — Yoshida Shinto's later counter-theorization and nativist (kokugaku) critiques emerged but could not displace the framework until state power intervened.
 *
 * PERSPECTIVAL GAP:
 *   From the Buddhist theological elite seat, the constraint appears as a genuine intellectual synthesis (rope-flavored) that solved the coordination problem of integrating two salvific systems. From the Shinto priesthood subordinated seat, it appears as a hostile takeover (snare-flavored) that stripped their tradition of ontological autonomy. From the lay practitioner seat, it appears as a workable but costly coordination (tangled_rope-flavored) — the unified framework simplified ritual life but imposed Buddhist institutional demands. The engine computes this divergence from the structural data; the authored claim (tangled_rope) captures the hybrid reality without forcing a single-seat verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist theological elite (agenda_setter) sit at d ≈ 0.15 — they authored the ontology, controlled the interpretive apparatus, and captured the institutional gains. Jinguji institutional network (beneficiary) at d ≈ 0.25 — they received land, tax exemptions, and ritual monopoly but bore maintenance costs. Shinto priesthood subordinated (payer) at d ≈ 0.75 — they lost independent ritual authority, land control, and doctrinal voice, becoming functionaries in Buddhist-led complexes. Lay practitioners (payer) at d ≈ 0.6 — they gained a unified soteriological map but paid through temple registration (tera-uke), land donations, and cognitive assimilation to a Buddhist-centric framework. Heterodox local cults (excluded) at d ≈ 0.9 — their practices were reclassified as 'superstition' or absorbed as 'manifest traces' without consent. Imperial court (observer) at d ≈ 0.4 — it patronized both sides but ultimately relied on the unified framework for legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to integrate indigenous kami worship with imported Buddhism without civilizational rupture — was live in the 8th-9th centuries. By the 12th century, the integration was complete and the problem was dead, yet the constraint persisted for 600+ years through institutional inertia and the theological elite's vested authority. The Meiji state's forced separation reveals the mandatrophy: the constraint had long outlived its founding coordination function and survived only because the Buddhist institutional network could enforce it. The theater_ratio rise from 0.12 to 0.28 tracks this drift from coordination to extraction-maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine doctrinal synthesis or a Buddhist colonial imposition on indigenous kami traditions?',
    'Comparative analysis of early honji suijaku texts vs. pre-Buddhist kami ritual records; archaeological evidence of jinguji construction patterns.',
    'If colonial imposition, the constraint is snare-flavored from the shinto_priesthood_subordinated seat; if genuine synthesis, it is rope-flavored from the lay_practitioners seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ontological status of honji suijaku: synthesis vs. subordination').

omega_variable(
    doctrinal_consistency_vs_practice_gap,
    'Did the theological elite''s doctrinal consistency constraint actually govern village-level practice, or was practical syncretism largely decoupled from elite theory?',
    'Ethnographic records from medieval temple registers (tera-uke), folk ritual documentation, and miracle tale collections (setsuwa).',
    'If decoupled, the constraint''s suppression and accessibility_collapse are lower than elite texts suggest; the engine would compute lower effective extraction for lay_practitioners.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrinal_consistency_vs_practice_gap, empirical, 'Gap between elite doctrinal constraint and lived religious practice').

omega_variable(
    jinguji_as_coordination_or_extraction,
    'Were jinguji (shrine-temple complexes) primarily coordination mechanisms for unified worship or extraction mechanisms for Buddhist institutional resource capture?',
    'Land tenure records, tax exemption documents, and construction patronage patterns across the Heian-Kamakura transition.',
    'If extraction-dominant, the jinguji_institutional_network beneficiary declaration is validated and the constraint leans snare; if coordination-dominant, the rope component strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jinguji_as_coordination_or_extraction, empirical, 'Structural function of jinguji institutions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 794, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_syncretic_fusion_tr_t794, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 794, 0.12).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_tr_t900, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 900, 0.18).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_tr_t1050, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1050, 0.25).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_tr_t1185, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1185, 0.28).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_tr_t1333, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1333, 0.3).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_tr_t1467, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1467, 0.32).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_tr_t1600, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1600, 0.29).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_tr_t1868, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1868, 0.28).

% Extraction over time
narrative_ontology:measurement(shinbutsu_syncretic_fusion_be_t794, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 794, 0.25).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_be_t900, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 900, 0.38).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_be_t1050, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1050, 0.52).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_be_t1185, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1185, 0.55).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_be_t1333, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1333, 0.58).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_be_t1467, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1467, 0.61).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_be_t1600, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1600, 0.59).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_be_t1868, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1868, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_syncretic_fusion_su_t794, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 794, 0.15).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_su_t900, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 900, 0.28).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_su_t1050, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1050, 0.4).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_su_t1185, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1185, 0.42).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_su_t1333, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1333, 0.45).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_su_t1467, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1467, 0.48).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_su_t1600, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1600, 0.42).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_su_t1868, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1868, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__syncretic_fusion_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.08).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, meiji_shinbutsu_bunri_policy).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, yoshida_shinto_counter_theorization).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, kokugaku_nativist_critique).

% DUAL FORMULATION NOTE:
% This constraint is one member of the shinbutsu_coexistence_commitment family. The kernel decomposes into three structurally distinct constraints with different ε values: this syncretic_fusion_reading (ε=0.58, tangled_rope), domain_partition_reading (ε≈0.25, rope — genuine coordination without ontological subordination), and incoherent_bundle_reading (ε≈0.7, snare — ambiguity as extraction cover). The ε-invariance principle requires separate stories because measuring 'shinbutsu coexistence' via honji suijaku texts yields high extraction; measuring via domain-partition ritual practice yields low extraction. These are different constraints, not different measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_coexistence_commitment__syncretic_fusion_reading, institutional, 0.15).
constraint_indexing:directionality_override(shinbutsu_coexistence_commitment__syncretic_fusion_reading, organized, 0.75).
constraint_indexing:directionality_override(shinbutsu_coexistence_commitment__syncretic_fusion_reading, moderate, 0.6).
constraint_indexing:directionality_override(shinbutsu_coexistence_commitment__syncretic_fusion_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
