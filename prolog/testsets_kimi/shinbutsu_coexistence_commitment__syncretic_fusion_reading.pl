% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: shinbutsu_coexistence_commitment__syncretic_fusion_reading
 *   human_readable: Honji Suijaku Syncretic Ontology (Kami as Buddhist Traces)
 *   domain: religious/philosophical/historical
 *
 * SUMMARY:
 *   In medieval Japan, the honji suijaku (original nature, trace
 *   manifestation) framework subordinated indigenous kami to Buddhist cosmic
 *   ontology by interpreting each kami as a local manifestation of a
 *   universal Buddhist dharma-body. This reading instantiates the syncretic
 *   fusion reading of the shinbutsu coexistence commitment kernel: a single
 *   coherent ontology administered by esoteric Buddhist exegetes and
 *   physically embodied in jinguji (temple-shrine) institutions. The
 *   constraint coordinates Shinto and Buddhist cults into a unified religious
 *   field while asymmetrically extracting ritual authority and economic
 *   resources from shrine priests and local communities. It is classified as
 *   tangled rope because it simultaneously solves a genuine coordination
 *   problem (integrating two religious systems) and enforces an asymmetric
 *   extraction of interpretive power.
 *
 * KEY AGENTS:
 *   - esoteric_buddhist_elite: agenda_setter (institutional/universal) â develops doctrine, controls jinguji, captures authority
 *   - jinguji_institutions: beneficiary (institutional/national) â receives patronage and land as structural embodiment
 *   - shrine_priests: payer (moderate/national) â serves kami under Buddhist oversight, theological autonomy subordinated
 *   - local_kami_communities: excluded (powerless/local) â practices rescripted without doctrinal voice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.62).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.58).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "Honji Suijaku Syncretic Ontology (Kami as Buddhist Traces)").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "religious/philosophical/historical").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__syncretic_fusion_reading, '80f0f4dc-6bdf-4b38-b9c5-0858bec0b881').
narrative_ontology:cs_kernel_codification('80f0f4dc-6bdf-4b38-b9c5-0858bec0b881', fixed_text).
narrative_ontology:cs_authority_grounding('80f0f4dc-6bdf-4b38-b9c5-0858bec0b881', lineage).
narrative_ontology:cs_interpretation_layer_present('80f0f4dc-6bdf-4b38-b9c5-0858bec0b881').
narrative_ontology:cs_reading_relation('80f0f4dc-6bdf-4b38-b9c5-0858bec0b881', shinbutsu_coexistence_commitment__domain_partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('80f0f4dc-6bdf-4b38-b9c5-0858bec0b881', shinbutsu_coexistence_commitment__incoherent_bundle_reading, forecloses).
narrative_ontology:cs_axiom('80f0f4dc-6bdf-4b38-b9c5-0858bec0b881', foundational, honji_suijaku_ontology).
narrative_ontology:cs_axiom_status(honji_suijaku_ontology, holdable).
narrative_ontology:cs_axiom_grounding('80f0f4dc-6bdf-4b38-b9c5-0858bec0b881', honji_suijaku_ontology, theological).
narrative_ontology:cs_axiom('80f0f4dc-6bdf-4b38-b9c5-0858bec0b881', foundational, esoteric_exegesis_supremacy_over_kami_ritual).
narrative_ontology:cs_axiom_status(esoteric_exegesis_supremacy_over_kami_ritual, holdable).
narrative_ontology:cs_axiom_grounding('80f0f4dc-6bdf-4b38-b9c5-0858bec0b881', esoteric_exegesis_supremacy_over_kami_ritual, theological).
narrative_ontology:cs_reference_frame('80f0f4dc-6bdf-4b38-b9c5-0858bec0b881', buddhist_universal_dominance_with_local_trace_manifestations).
narrative_ontology:cs_drift_state('80f0f4dc-6bdf-4b38-b9c5-0858bec0b881', late_medieval_sengoku_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('80f0f4dc-6bdf-4b38-b9c5-0858bec0b881', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, esoteric_buddhist_elite).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_institutions).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shrine_priests).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, local_kami_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and teach honji suijaku correspondences between specific kami and Buddhist buddhas and bodhisattvas. Administer jinguji complexes, train monks in esoteric rituals that incorporate kami, and receive aristocratic and imperial land grants and ritual fees justified by their role as interpreters of universal Buddhist dharma. Their institutional position depends on maintaining the doctrinal framework that subordinates kami cults to Buddhist ontology.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, esoteric_buddhist_elite, agenda_setter,
    institutional, generational, constrained, universal).

% Combined temple-shrine institutions physically housing both Buddhist halls and kami shrines, governed by Buddhist monastic codes. Receive consolidated patronage from aristocrats, warriors, and peasants; manage agricultural estates; and perform funerary and calendrical rites under Buddhist administrative hierarchy. Their economic and legal standing derives from the syncretic structure.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, jinguji_institutions, beneficiary,
    institutional, generational, constrained, national).

% Hereditary priests serving enshrined kami who are officially designated as local manifestations of Buddhist figures. Required to participate in Buddhist rituals, accept Buddhist clerical oversight, and refrain from asserting independent kami theology. Their ritual autonomy and inheritance of shrine offices are contingent on compliance with the honji suijaku schema.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shrine_priests, payer,
    moderate, biographical, identity_locked, national).

% Lay communities attached to shrines within jinguji complexes who continue agricultural and ancestral rites. Their festivals and offerings are rescripted with Buddhist meanings by temple administrators, and they lack formal representation in doctrinal decisions about their deity's ontological status.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, local_kami_communities, excluded,
    powerless, biographical, identity_locked, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates kami worship and Buddhist cult into a single, non-contradictory religious field by assigning each kami a determinate place within Buddhist cosmic ontology, preventing doctrinal conflict and institutional competition between shrine and temple institutions.
% TRANSFER_FUNCTION: Moves ritual authority, patronage resources, and interpretive control from independent shrine priests and local kami communities to esoteric Buddhist exegetes and jinguji institutions administered by Buddhist clergy.
% ABSENT_VOICES: Independent shrine priests advocating pure kami theology without Buddhist subordination; local agricultural communities whose ancestral kami practices were reinterpreted without their doctrinal participation; proto-Shinto restorationists who would later resist Buddhist overlay but were not present in the medieval doctrinal formation.
% DISAPPEARANCE_RATIONALE: If the ontological unification vanished, jinguji institutions would split back into separate temples and shrines, patronage flows would reorganize around independent cultic centers, shrine priests would reclaim theological autonomy, and the Buddhist clerical monopoly over kami interpretation would collapse â the medieval Japanese religious field would reconstitute along polycentric lines.
% FOUNDING_PROBLEM: Religious pluralism in early medieval Japan threatened constant doctrinal conflict and institutional competition between immigrant Buddhist cults and indigenous kami worship, risking social disorder and fragmented aristocratic and imperial patronage.
% FOUNDING_PROBLEM_CORROBORATION: Modern historians of Japanese religion attest that while doctrinal conflict was a real early Heian concern, the persistence of jinguji structures into the late medieval period exceeded the original coordination need. Shrine-side texts and early modern kokugaku scholars attest the subordination problem from outside the Buddhist beneficiary set.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__syncretic_fusion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) reflects substantial but not total extraction: the coordination function (religious integration, shared ritual calendar, reduced inter-cult conflict) is real, but the price is the subordination of shrine priests and the redirection of patronage to Buddhist-led jinguji. Suppression (0.58) captures active doctrinal enforcement through monastic training, jinguji administrative control, and state-backed patronage networks that disadvantaged independent shrines. Theater ratio (0.40) acknowledges that the elaborate honji-suijaku correspondence tables and esoteric ritual architecture served genuine integrative functions but also contained performative elements asserting Buddhist supremacy beyond operational need. Accessibility collapse (0.65) registers that independent kami theology and autonomous shrine institutions became difficult to maintain outside the jinguji system, though never fully extinguished. Resistance (0.45) reflects recurring shrine-priest resentment and early modern kokugaku proto-resistance, kept subordinate through most of the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the esoteric Buddhist elite seat, the constraint appears as necessary theological coordination that prevents doctrinal chaos and preserves universal truth through local adaptation; the costs to shrine priests appear as legitimate ritual hierarchy. From the shrine-priest seat, the same structure appears as an imposed ontological straitjacket that expropriates their deities and subordinates their hereditary offices to an alien clerical hierarchy. The engine computes this divergence from the same structural facts: agenda-setter and beneficiary seats carry low directionality (subsidized by the constraint's authority structure), while payer and excluded seats carry high directionality (extracted via ontological subordination).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (esoteric_buddhist_elite, jinguji_institutions) are structurally subsidized by the constraint: it amplifies their authority, land base, and interpretive monopoly, yielding low directionality. Victims (shrine_priests, local_kami_communities) bear the costs of theological subordination and ritual appropriation, yielding high directionality. The spatial_scope differential (universal for Buddhist dharma versus local for lay communities) further amplifies extraction for the powerless local seat relative to the institutional universal seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â religious pluralism and potential doctrinal conflict in early Heian Japan â was live when the constraint formed. By the Kamakura and Muromachi periods, the coordination problem had largely been solved, yet the jinguji system and honji suijaku doctrine persisted as an entrenched power structure. The classification as tangled rope prevents mislabeling the late-medieval persistence as pure coordination (rope) by preserving the victim structure and active enforcement requirement; it also prevents mislabeling it as pure snare by acknowledging the genuine coordination function that prevented sectarian violence and fragmented patronage.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_internalization_depth,
    'To what degree is the constraint''s suppression internalized (shrine priests genuinely adopt honji suijaku as theological conviction) versus purely structural (external institutional subordination without belief)?',
    'Archaeological and ritual evidence of priestly theological production independent of Buddhist framing; post-Meiji shinbutsu bunri attestation of whether priests immediately reverted to kami-centric theology.',
    'If primarily internalized, effective suppression is higher than institutional measures suggest; if purely structural, the constraint is a thinner enforcement layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_internalization_depth, empirical, 'Structural versus internalized suppression in theological subordination').

omega_variable(
    coordination_extraction_boundary,
    'Is the ontological unification structurally necessary for the coexistence of kami and Buddhist cults, or does the coordination function (peaceful coexistence) survive without the extraction (hierarchical subordination)?',
    'Comparative analysis of religious syncretisms that achieve coexistence without ontological subordination; evidence of local shrines and temples cooperating without jinguji control.',
    'If separable, the hierarchical ontology is extractive overhead on a coordination function; if inseparable, the extraction is the price of the coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether coordination and extraction are structurally separable').

omega_variable(
    kernel_reading_contest,
    'Does the syncretic fusion reading accurately describe the kernel''s operation, or is the incoherent bundle reading more descriptively true?',
    'Historical institutional analysis of whether jinguji complexes operated under a single coherent ontology or pragmatic ambiguity.',
    'Determines whether this constraint''s epsilon is stable or should be decomposed into multiple thinner constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the kernel is a coherent ontology or an incoherent bundle').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_syncretic_tr_t0, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t10, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t20, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t30, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t40, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t50, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(shinbutsu_syncretic_be_t0, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(shinbutsu_syncretic_be_t10, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(shinbutsu_syncretic_be_t20, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(shinbutsu_syncretic_be_t30, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(shinbutsu_syncretic_be_t40, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(shinbutsu_syncretic_be_t50, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_syncretic_su_t0, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(shinbutsu_syncretic_su_t10, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(shinbutsu_syncretic_su_t20, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(shinbutsu_syncretic_su_t30, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(shinbutsu_syncretic_su_t40, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(shinbutsu_syncretic_su_t50, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__syncretic_fusion_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the shinbutsu_coexistence_commitment kernel, decomposed per the epsilon-invariance principle from the colloquial label 'shinbutsu-shugo'. Sibling readings handle the domain-partition and incoherent-bundle interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
