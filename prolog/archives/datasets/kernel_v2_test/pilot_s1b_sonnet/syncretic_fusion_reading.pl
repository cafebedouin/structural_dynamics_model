% ============================================================================
% CONSTRAINT STORY: syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_syncretic_fusion_reading, []).

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
 *   constraint_id: syncretic_fusion_reading
 *   human_readable: Syncretic Fusion Reading of Shinbutsu Ontology
 *   domain: religious_studies/japanese_history/commitment_systems
 *
 * SUMMARY:
 *   The syncretic fusion reading of shinbutsu ontology holds that kami and
 *   buddhas are metaphysically unified: honji suijaku (original ground -
 *   manifested trace) describes genuine ontological structure, not merely
 *   institutional convenience. Under this reading, kami are manifested traces
 *   of underlying buddha-nature, and the unified worship framework reflects
 *   deep metaphysical truth. This constraint is one of three sibling readings
 *   of the shinbutsu_ontological_substrate kernel. The
 *   domain_partition_reading holds that kami and buddhas occupy separate
 *   ontological domains and honji suijaku is heuristic coordination rather
 *   than metaphysical claim. The incoherent_bundle_reading holds that the
 *   framework contains unresolved contradictions maintained through
 *   institutional power rather than coherent doctrine. The syncretic fusion
 *   reading was the dominant institutional framework from the Nara period
 *   through the Edo period, coordinating worship practices across jinguji
 *   temple-shrine complexes and enabling Buddhist institutional expansion
 *   into indigenous kami worship networks. The Meiji-era forced separation
 *   (shinbutsu bunri, 1868) attempted to dissolve the syncretic framework
 *   through administrative fiat, revealing whether the fusion was contingent
 *   institutional arrangement or metaphysically deep commitment.
 *
 * KEY AGENTS:
 *   - Syncretic Institutions (jinguji complexes): Primary beneficiaries (institutional/constrained) — coordinated unified worship, controlled dual ritual calendars, benefited from institutional integration
 *   - Dual Priesthood Lineages: Beneficiaries (institutional/constrained) — maintained authority over both kami and buddha ritual, benefited from doctrinal unification
 *   - Lay Practitioners: Participants (moderate/mobile) — experienced unified framework as coherent religious practice, low coordination costs
 *   - Buddhist Establishment: Beneficiaries (institutional/constrained) — expanded institutional reach through kami shrine networks, gained access to indigenous worship base
 *   - Post-Meiji Separatist Institutions: Constrained actors (institutional/constrained) — faced exit costs from historical fusion when attempting doctrinal purification
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — evaluates whether coordination story naturalizes extraction or describes genuine synthesis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(syncretic_fusion_reading, 0.28).
domain_priors:suppression_score(syncretic_fusion_reading, 0.42).
domain_priors:theater_ratio(syncretic_fusion_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(syncretic_fusion_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(syncretic_fusion_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(syncretic_fusion_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(syncretic_fusion_reading, rope).
narrative_ontology:human_readable(syncretic_fusion_reading, "Syncretic Fusion Reading of Shinbutsu Ontology").
narrative_ontology:topic_domain(syncretic_fusion_reading, "religious_studies/japanese_history/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(syncretic_fusion_reading, '6691a2db-aa0a-48d0-8b90-965aae89d4f8').
narrative_ontology:cs_kernel_codification('6691a2db-aa0a-48d0-8b90-965aae89d4f8', fixed_text).
narrative_ontology:cs_authority_grounding('6691a2db-aa0a-48d0-8b90-965aae89d4f8', lineage).
narrative_ontology:cs_interpretation_layer_present('6691a2db-aa0a-48d0-8b90-965aae89d4f8').
narrative_ontology:cs_reading_relation('6691a2db-aa0a-48d0-8b90-965aae89d4f8', syncretic_fusion_reading__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('6691a2db-aa0a-48d0-8b90-965aae89d4f8', syncretic_fusion_reading__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('6691a2db-aa0a-48d0-8b90-965aae89d4f8', foundational, ontological_monism_kami_buddha).
narrative_ontology:cs_axiom_status(ontological_monism_kami_buddha, holdable).
narrative_ontology:cs_axiom_grounding('6691a2db-aa0a-48d0-8b90-965aae89d4f8', ontological_monism_kami_buddha, theological).
narrative_ontology:cs_axiom('6691a2db-aa0a-48d0-8b90-965aae89d4f8', foundational, honji_suijaku_metaphysical_necessity).
narrative_ontology:cs_axiom_status(honji_suijaku_metaphysical_necessity, overridden).
narrative_ontology:cs_axiom_grounding('6691a2db-aa0a-48d0-8b90-965aae89d4f8', honji_suijaku_metaphysical_necessity, theological).
narrative_ontology:cs_reference_frame('6691a2db-aa0a-48d0-8b90-965aae89d4f8', nara_heian_honji_suijaku_synthesis).
narrative_ontology:cs_drift_state('6691a2db-aa0a-48d0-8b90-965aae89d4f8', post_meiji_separation, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('6691a2db-aa0a-48d0-8b90-965aae89d4f8', '').
narrative_ontology:cs_kernel_id(syncretic_fusion_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(syncretic_fusion_reading, syncretic_institutions).
narrative_ontology:constraint_beneficiary(syncretic_fusion_reading, jinguji_temple_complexes).
narrative_ontology:constraint_beneficiary(syncretic_fusion_reading, dual_priesthood_lineages).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SYNCRETIC INSTITUTION (ROPE) — Coordinates worship practices, ritual calendars, and institutional authority through unified kami-buddha framework. The fusion doctrine solves genuine coordination problems: shared sacred sites, integrated ritual cycles, unified doctrine for practitioners. Extraction is low because the institution benefits from coordination without substantial asymmetric extraction from practitioners.
constraint_indexing:constraint_classification(syncretic_fusion_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 2: LAY PRACTITIONER (ROPE) — Experiences the unified framework as coherent religious practice. The ontological fusion simplifies devotion: no need to choose between kami and buddha veneration, no doctrinal conflict to navigate. Low extraction because the coordination solves real problems (ritual coherence, doctrinal simplicity) without imposing substantial costs.
constraint_indexing:constraint_classification(syncretic_fusion_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: SEPARATIST INSTITUTION POST-MEIJI (TANGLED ROPE) — After the forced separation (shinbutsu bunri), institutions attempting to maintain pure Shinto or pure Buddhist identity face genuine coordination costs from the historical fusion. Property disputes, ritual disentanglement, and doctrinal reconstruction are necessary but costly. The syncretic fusion reading is experienced as both coordination (it WAS solving real problems) and extraction (its persistence creates exit costs for those attempting separation).
constraint_indexing:constraint_classification(syncretic_fusion_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: BUDDHIST ESTABLISHMENT PRE-SEPARATION (ROPE) — The honji suijaku framework coordinates Buddhist institutional expansion with indigenous kami worship. Low extraction because the coordination is genuinely functional: Buddhist temples gain access to kami shrine networks, kami shrines gain doctrinal elaboration and institutional resources. The beneficiary structure is symmetric enough to classify as coordination.
constraint_indexing:constraint_classification(syncretic_fusion_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — From the civilizational analytical position, the syncretic fusion reading describes a coordination mechanism that emerged from genuine religious synthesis. The ontological unification solved real coordination problems in medieval Japan: it allowed kami worship to persist within Buddhist institutional frameworks, prevented doctrinal conflict, and enabled shared ritual practice. Extraction is low because the arrangement was not imposed asymmetrically — it emerged from bilateral institutional negotiation and practitioner acceptance.
constraint_indexing:constraint_classification(syncretic_fusion_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(syncretic_fusion_reading_tests).
:- end_tests(syncretic_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The syncretic fusion framework coordinated genuine religious practice with modest asymmetric benefits flowing to Buddhist institutional hierarchies. Buddhist temples gained access to kami shrine networks and practitioner bases; kami shrines gained doctrinal elaboration and institutional resources. The extraction is not negligible (Buddhist institutional dominance extracted some rents through doctrinal authority and resource control) but is substantially lower than pure extraction mechanisms. The moderate value reflects that the coordination was genuinely functional while also serving institutional interests. Suppression (0.42): Moderate. Alternative frameworks (pure Shinto practice, Buddhist practice without kami syncretism) faced institutional barriers and doctrinal delegitimization but were not impossible. The suppression increased over the interval as the syncretic framework became institutionally entrenched and alternatives became harder to maintain. Post-Meiji separation revealed the suppression's magnitude: disentanglement was administratively implementable but institutionally costly. Theater ratio (0.35): Moderate-low. The honji suijaku doctrine was not purely performative — it reflected genuine ontological commitments held by practitioners and institutions. However, the metaphysical claims also served institutional coordination functions (legitimating unified control, preventing doctrinal conflict), so some theater component exists. The theater ratio increased over the interval as the framework became more institutionally routinized and less tied to lived metaphysical conviction.
 *
 * PERSPECTIVAL GAP:
 *   The syncretic fusion reading produces a rope classification from most perspectives because the coordination function is genuine and extraction is modest. Syncretic institutions see the unified framework as solving real coordination problems (shared ritual calendars, integrated doctrine, institutional authority). Lay practitioners experience coherent religious practice without substantial costs. The Buddhist establishment coordinated expansion through the framework with symmetric enough benefits to avoid tangled rope classification. The analytical observer sees genuine religious synthesis with modest institutional extraction. The only tangled rope classification comes from post-Meiji separatist institutions attempting to exit the historical fusion: for them, the coordination story is complicated by the exit costs of disentanglement. The perspectival gap is narrow because the constraint operated primarily as coordination across most of its interval. The gap widens at the separation moment when exit costs become salient.
 *
 * DIRECTIONALITY LOGIC:
 *   Syncretic institutions and dual priesthood lineages are primary beneficiaries: they coordinate worship practices and control ritual authority through the unified framework. Their directionality is low (beneficiary status + constrained exit options → low d → low effective extraction, potentially negative). Lay practitioners experience the framework as coordination solving genuine devotional problems: their directionality is near-neutral (mobile exit options + neither clear beneficiary nor victim → mid-range d → moderate effective extraction, near zero). Post-Meiji separatist institutions experience the historical fusion as extraction: their directionality is moderate-high (constrained exit + victim of historical entanglement → higher d → moderate effective extraction). The Buddhist establishment pre-separation experiences symmetric coordination benefits: low directionality (beneficiary status + constrained exit → low d). The analytical observer evaluates the constraint from the civilizational perspective: the syncretic fusion reading describes a coordination mechanism that emerged from genuine religious synthesis, with modest asymmetric extraction favoring Buddhist institutional hierarchies. No victims are declared because the constraint operated primarily as coordination with extraction as a secondary feature, not as a mechanism targeting identifiable victim groups.
 *
 * MANDATROPHY ANALYSIS:
 *   The syncretic fusion reading demonstrates that rope classification is appropriate when coordination is genuine and extraction is modest, even when the coordination serves institutional interests. The constraint is not mandatrophy because it is not purely extraction disguised as coordination — the unified framework solved real religious coordination problems (ritual coherence, doctrinal integration, institutional authority) while also providing modest institutional benefits to Buddhist hierarchies. The Meiji separation serves as an empirical test: if the fusion was pure extraction, separation would have been simple administrative disentanglement. The historical record shows separation was implementable but costly, suggesting the fusion was genuine coordination with embedded extraction rather than extraction with coordination as cover. The mandatrophy analysis distinguishes this from false mountains: the syncretic fusion reading does not claim natural law status (it is explicitly a doctrinal framework), and the beneficiary structure is declared rather than hidden. The constraint is rope from most perspectives because the coordination-to-extraction ratio favors coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_ambiguity_metaphysics_vs_arrangement,
    'Is honji suijaku a metaphysical truth claim (kami are manifestations of buddhas) or an institutional arrangement (kami and buddha worship are coordinated through doctrine)?',
    'Historical analysis of institutional vs doctrinal debates; textual analysis of whether the fusion claim was defended on ontological or pragmatic grounds; examination of whether separation was conceptually possible within the framework''s own terms.',
    'If metaphysical truth claim: the syncretic fusion reading is a commitment-system constraint grounded in lineage and interpretive authority. If institutional arrangement: the constraint is primarily coordination with doctrinal theater as its enforcement mechanism. The sibling readings diverge on this question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_ambiguity_metaphysics_vs_arrangement, conceptual, 'Whether honji suijaku is metaphysical claim or institutional coordination').

omega_variable(
    extraction_asymmetry_buddhist_dominance,
    'Did Buddhist institutional dominance extract rents from kami worship through the honji suijaku framework, or was the arrangement genuinely symmetric?',
    'Analysis of resource flows, institutional control, and doctrinal authority within jinguji complexes; examination of whether kami shrines retained independent authority or became subordinate to Buddhist temples; comparison of pre-fusion and post-fusion institutional arrangements.',
    'If extraction is substantial: reclassify from rope to tangled_rope at institutional perspectives. If symmetric: rope classification is appropriate. This omega determines whether the ''coordination'' story is cover for asymmetric extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_asymmetry_buddhist_dominance, empirical, 'Whether Buddhist institutions extracted rents from kami worship').

omega_variable(
    reading_choice_determinants,
    'What structural factors determined which reading (syncretic fusion, domain partition, or incoherent bundle) historical actors adopted?',
    'Comparative institutional analysis: which readings were held by which institutional actors, and what beneficiary structures corresponded to each reading. Analysis of whether the syncretic fusion reading was held primarily by institutions that benefited from unified control.',
    'If the syncretic fusion reading correlates with institutional beneficiaries of unified control: the reading''s coordination story may naturalize extraction. If readings distribute independently of beneficiary structure: the kernel genuinely supports multiple framings without one being a cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_choice_determinants, empirical, 'Structural determinants of reading adoption by historical actors').

omega_variable(
    meiji_separation_as_test,
    'Does the Meiji-era forced separation (shinbutsu bunri) demonstrate that the syncretic fusion was contingent institutional arrangement rather than metaphysical necessity?',
    'Historical analysis of the separation''s success and resistance patterns. If separation was implementable with primarily administrative costs (property division, ritual disentanglement), the fusion was institutional. If separation required fundamental reconceptualization of religious practice and met sustained ontological resistance, the fusion was metaphysically deep.',
    'If separation was implementable: the syncretic fusion reading is revealed as institutional coordination, weakening its metaphysical truth claim. If separation met deep resistance: the fusion reading''s metaphysical claim is corroborated. This omega tests whether the constraint''s claimed_type (rope) matches its structural reality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(meiji_separation_as_test, empirical, 'Whether Meiji separation reveals fusion as contingent or metaphysically deep').

omega_variable(
    committer_framing_foundational_axiom,
    'This constraint is the syncretic_fusion_reading of the shinbutsu_ontological_substrate kernel. The sibling readings are domain_partition_reading (kami and buddhas occupy separate domains; honji suijaku is heuristic, not ontology) and incoherent_bundle_reading (the framework contains unresolved contradictions maintained through institutional power). What determines which framing an observer adopts?',
    'Cross-reading analysis: what structural position makes the syncretic fusion reading compelling vs the partition reading vs the incoherence reading? The syncretic fusion reading is held by institutions that benefited from unified control and by practitioners for whom the fusion solved genuine devotional problems. The partition reading is held by post-Meiji institutions attempting doctrinal purification. The incoherence reading is held by external analysts observing institutional conflicts. Framing choice tracks structural position relative to the constraint''s beneficiary structure.',
    'If framing choice tracks beneficiary structure: the syncretic fusion reading may naturalize institutional extraction as metaphysical truth. If framing choice is independent: the kernel genuinely supports multiple ontological interpretations without one being extractive cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_foundational_axiom, conceptual, 'Structural determinants of kernel reading adoption').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(syncretic_fusion_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(syncfus_theater_nara, syncretic_fusion_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(syncfus_theater_heian, syncretic_fusion_reading, theater_ratio, 3, 0.28).
narrative_ontology:measurement(syncfus_theater_kamakura, syncretic_fusion_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement(syncfus_theater_muromachi, syncretic_fusion_reading, theater_ratio, 9, 0.38).

% Extraction over time
narrative_ontology:measurement(syncfus_extract_nara, syncretic_fusion_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(syncfus_extract_heian, syncretic_fusion_reading, base_extractiveness, 3, 0.22).
narrative_ontology:measurement(syncfus_extract_kamakura, syncretic_fusion_reading, base_extractiveness, 6, 0.28).
narrative_ontology:measurement(syncfus_extract_muromachi, syncretic_fusion_reading, base_extractiveness, 9, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(syncfus_suppress_nara, syncretic_fusion_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(syncfus_suppress_heian, syncretic_fusion_reading, suppression_requirement, 3, 0.35).
narrative_ontology:measurement(syncfus_suppress_kamakura, syncretic_fusion_reading, suppression_requirement, 6, 0.42).
narrative_ontology:measurement(syncfus_suppress_muromachi, syncretic_fusion_reading, suppression_requirement, 9, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(syncretic_fusion_reading, identity_coordination).
narrative_ontology:affects_constraint(syncretic_fusion_reading, domain_partition_reading).
narrative_ontology:affects_constraint(syncretic_fusion_reading, incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% The syncretic_fusion_reading is one of three constraint stories describing the shinbutsu_ontological_substrate kernel. Each reading has its own epsilon value reflecting different structural relationships: syncretic fusion (ε ≈ 0.28, coordination-dominant); domain partition (ε expected lower, pragmatic coordination without metaphysical claims); incoherent bundle (ε expected higher, contradictions maintained through institutional power). The readings are linked as sibling constraints within a kernel family, not as alternative measurements of a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
