% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__literary_revival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__literary_revival_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: hebrew_living_language__literary_revival_reading
 *   human_readable: Hebrew as Living Language Through Haskalah Literary Generative Competence
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the literary-revival reading of the contested
 *   'Hebrew living language' kernel: the claim that Haskalah-era Hebrew
 *   literary production (roughly 1780-1900) constitutes Hebrew's status as a
 *   living language through demonstrated generative written competence — new
 *   vocabulary, extended syntax, novel prose and poetic forms — even though
 *   no community spoke Hebrew as a native daily vernacular during this
 *   period. The constraint here is the definitional commitment itself: that
 *   written generativity, not native speech or liturgical continuity, is the
 *   criterion that counts. This is deliberately NOT the same constraint as
 *   the liturgical-continuity reading (which locates vitality in unbroken
 *   recitation/study) or the native-generation reading (which locates it in
 *   cradle-tongue daily speech production) — each of those readings has a
 *   different beneficiary/victim structure and a different epsilon, and are
 *   authored as separate sibling stories linked through the kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__literary_revival_reading, 0.08).
domain_priors:suppression_score(hebrew_living_language__literary_revival_reading, 0.12).
domain_priors:theater_ratio(hebrew_living_language__literary_revival_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__literary_revival_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__literary_revival_reading, "Hebrew as Living Language Through Haskalah Literary Generative Competence").
narrative_ontology:topic_domain(hebrew_living_language__literary_revival_reading, "historical_linguistics/language_revitalization/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__literary_revival_reading, 'c66d1323-4066-4f40-a47e-670e5b54a89e').
narrative_ontology:cs_kernel_codification('c66d1323-4066-4f40-a47e-670e5b54a89e', distributed).
narrative_ontology:cs_authority_grounding('c66d1323-4066-4f40-a47e-670e5b54a89e', practice).
narrative_ontology:cs_interpretation_layer_present('c66d1323-4066-4f40-a47e-670e5b54a89e').
narrative_ontology:cs_reading_relation('c66d1323-4066-4f40-a47e-670e5b54a89e', hebrew_living_language__liturgical_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('c66d1323-4066-4f40-a47e-670e5b54a89e', hebrew_living_language__native_generation_reading, influences).
narrative_ontology:cs_axiom('c66d1323-4066-4f40-a47e-670e5b54a89e', foundational, generative_written_output_constitutes_life).
narrative_ontology:cs_axiom_status(generative_written_output_constitutes_life, holdable).
narrative_ontology:cs_axiom_grounding('c66d1323-4066-4f40-a47e-670e5b54a89e', generative_written_output_constitutes_life, conventional).
narrative_ontology:cs_axiom('c66d1323-4066-4f40-a47e-670e5b54a89e', secondary, native_daily_speech_not_required_for_vitality).
narrative_ontology:cs_axiom_status(native_daily_speech_not_required_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('c66d1323-4066-4f40-a47e-670e5b54a89e', native_daily_speech_not_required_for_vitality, conventional).
narrative_ontology:cs_reference_frame('c66d1323-4066-4f40-a47e-670e5b54a89e', haskalah_literary_standard).
narrative_ontology:cs_drift_state('c66d1323-4066-4f40-a47e-670e5b54a89e', post_ben_yehuda_revival, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c66d1323-4066-4f40-a47e-670e5b54a89e', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__literary_revival_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, haskalah_maskilim_writers).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, hebrew_periodical_publishers).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, modern_hebrew_literary_canon).
narrative_ontology:constraint_vindicates(hebrew_living_language__literary_revival_reading, written_generative_competence_constitutes_linguistic_life).
narrative_ontology:constraint_vindicates(hebrew_living_language__literary_revival_reading, literary_productivity_sufficient_for_vitality_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% 19th-century Jewish Enlightenment writers (Mapu, Smolenskin, Mendele) who composed novels, essays, poetry, and periodicals in Hebrew, generating new vocabulary, syntax innovations, and registers to describe modern life — a language that had no continuous native vernacular community but was pressed into full literary productivity by their own labor. They set the terms of what counts as Hebrew being 'alive': generative written output rather than cradle-tongue transmission.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, haskalah_maskilim_writers, agenda_setter,
    moderate, generational, constrained, continental).

% Editors and printers of Hebrew-language newspapers and journals (Hamagid, Hashiloach, and successors) whose enterprises depended on the claim that Hebrew was a viable medium for contemporary discourse — science, politics, fiction. Their commercial and cultural standing rose with every demonstration that Hebrew could carry modern content.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, hebrew_periodical_publishers, beneficiary,
    moderate, biographical, constrained, regional).

% The body of literary work itself — not an actor, but the accumulated corpus that the literary-revival reading treats as evidence of vitality. Its existence retroactively validates the claim that literary productivity constitutes linguistic life, independent of whether it was ever anyone's mother tongue.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, modern_hebrew_literary_canon, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(hebrew_living_language__literary_revival_reading, modern_hebrew_literary_canon).

% Later Hebraists (Ben-Yehuda and the Second Aliyah revivalists) who held that written literary competence, however sophisticated, did not make Hebrew 'living' — only a community raising children who spoke it as a first, generative daily tongue could. From their vantage the Haskalah's achievement, however real, was not yet the thing being claimed; their objection is structurally excluded from this reading because this reading defines vitality by literary output alone.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, native_hebrew_speech_advocates, excluded,
    organized, generational, mobile, regional).

% Communities sustaining Hebrew through unbroken prayer, study, and textual recitation across centuries without literary innovation in the Haskalah sense. Their claim to continuity operates on entirely different grounds (transmission and use, not generative production) and is not adjudicated by this reading, which is silent on whether their form of continuity counts as 'living.'
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, diaspora_liturgical_communities, excluded,
    organized, civilizational, constrained, global).

% Scholars evaluating whether written generative competence without a native-speaker community satisfies standard linguistic criteria for a 'living language,' and whether the Haskalah corpus represents genuine linguistic productivity (new coinages, syntactic extension, register expansion) as opposed to translation-bound imitation of European literary forms.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, historical_linguists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a dispersed, non-co-located literate elite around a shared written standard capable of expressing modern concepts, enabling communication and cultural production across geographically separated Jewish communities without requiring any of them to speak Hebrew daily.
% TRANSFER_FUNCTION: Moves prestige and cultural capital toward the maskilim and periodical publishers who produce and control access to modern Hebrew literary output; moves comparatively little in the way of material extraction, since the practice is elective, unpaid or thinly paid literary labor rather than a rent-collecting structure.
% ABSENT_VOICES: Native-speech revivalists and diaspora liturgical communities hold competing accounts of what 'living' means for Hebrew and are not addressed by this reading's definitional move — they would object that literary output, however sophisticated, is not the same phenomenon as either daily generative speech or unbroken devotional use.
% DISAPPEARANCE_RATIONALE: If Haskalah literary production had never occurred, the modern Hebrew revival still might have proceeded through the native-speech route (as partisans of that reading argue), or Hebrew might have remained more narrowly liturgical. But the actual historical record shows the literary corpus supplying vocabulary, registers, and legitimacy that the subsequent native-speech revival drew on directly — so whether 'the world rearranges' depends on which downstream reading you hold, which is exactly the kernel-level dispute this story does not resolve.
% FOUNDING_PROBLEM: Jewish Enlightenment intellectuals needed a shared, prestige-bearing medium to carry modern secular ideas (science, politics, the novel form) into Jewish cultural life without requiring assimilation into the vernacular languages of host nations, and Hebrew's existing liturgical register lacked the vocabulary and prose forms to do this.
% FOUNDING_PROBLEM_CORROBORATION: Historical linguists and historians of the Haskalah (writing well after the native-speech revival succeeded) attest that the specific 19th-century problem — a prestige literary vehicle for secular modernization distinct from vernacular assimilation — was resolved by the rise of spoken Israeli Hebrew, which absorbed and superseded the literary register's function; this corroboration comes from scholarship outside the circle of maskilim and their publishing beneficiaries.
narrative_ontology:disappearance_verdict(hebrew_living_language__literary_revival_reading, contested).
narrative_ontology:founding_problem_status(hebrew_living_language__literary_revival_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__literary_revival_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_living_language__literary_revival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__literary_revival_reading, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__literary_revival_reading_tests).
:- end_tests(hebrew_living_language__literary_revival_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored very low (0.08 at interval end) because Haskalah literary production was an elective, largely unpaid or thinly remunerated intellectual practice among a self-selected literate elite; no coercive mechanism compelled participation and no identifiable victim group bore costs so others could benefit. Suppression is low (0.12) — the maskilim faced social resistance from traditionalist and Orthodox communities who saw secular Hebrew literature as ideologically threatening, but this is resistance TO the constraint's proponents, not suppression exercised BY the constraint against a captive population. Accessibility collapse is moderate (0.35), reflecting that alternative Hebrew futures (liturgical-only, or waiting for native speech) remained live and were not foreclosed by literary production continuing. Theater ratio rises modestly over the interval (0.05 to 0.15) as periodical Hebrew increasingly served status-signaling and nationalist identity performance alongside its original modernizing function.
 *
 * DIRECTIONALITY LOGIC:
 *   Maskilim writers and periodical publishers are beneficiaries: their cultural and (modest) commercial standing rose directly with the plausibility of the claim that Hebrew was a living literary medium. There are no victims in the structural sense — this reading, unlike a snare or tangled rope, imposes no asymmetric cost on any group in order to fund the benefit; the closest thing to a cost bearer is traditionalist resistance, which is a resistance dynamic, not an extraction target. The excluded parties (native-speech advocates, liturgical communities) are excluded from the DEFINITION this reading uses, not from any material flow — their absence is definitional exclusion, not extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for a prestige secular literary vehicle) is authored as dead: spoken Israeli Hebrew now performs that function and more. But the literary corpus itself persists as a foundational layer within the modern Hebrew literary canon, so mandatrophy here is partial — the SPECIFIC historical function (bridging Enlightenment modernity into a liturgical-register language) is obsolete, but the accumulated literary tradition was absorbed rather than discarded. This prevents mislabeling the Haskalah's afterlife as pure extraction-by-inertia (piton); it is better read as successful function transfer to a successor institution (spoken revival), which is why disappearance_verdict is authored contested rather than world_rearranges outright.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    written_competence_sufficiency,
    'Does sustained generative written literary competence, absent any native-speaker community, satisfy standard linguistic criteria for a language being ''alive,'' or does that status require daily generative spoken production (as the native_generation_reading holds)?',
    'Comparative sociolinguistic analysis of other cases of written-only literary languages (e.g. Classical Latin during periods of active humanist composition, Sanskrit''s continued literary production) against accepted vitality criteria (UNESCO language vitality framework, intergenerational transmission measures) to determine whether ''literary vitality'' is a recognized distinct category or a courtesy label.',
    'If written generative competence is judged linguistically insufficient on its own, this reading''s central claim is definitionally weaker than the native_generation_reading''s, though the literary-revival constraint itself remains structurally coherent as a claim about literary practice rather than language vitality per se.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(written_competence_sufficiency, conceptual, 'Whether literary generativity alone meets recognized linguistic-vitality criteria.').

omega_variable(
    reachability_from_literary_to_native_practice,
    'Was the Haskalah''s literary corpus a necessary causal precursor to the native-speech revival (supplying vocabulary, registers, and legitimacy Ben-Yehuda''s project depended on), or would native Hebrew speech have emerged through an independent path (e.g., directly from liturgical Hebrew plus European vernacular contact) with comparable content?',
    'Historical-linguistic tracing of specific lexical items and syntactic constructions used in early spoken Israeli Hebrew back to their first attested Haskalah literary usage versus liturgical or independent coinage, to establish the actual transmission pathway.',
    'If strong reachability is established, this reading gains standing as the load-bearing precursor within the kernel''s overall narrative, strengthening its claim to explanatory priority over the liturgical_continuity_reading. If reachability is weak, the three readings are more nearly independent, parallel claims rather than a sequence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reachability_from_literary_to_native_practice, empirical, 'Whether literary Hebrew was causally necessary for the later native-speech revival.').

omega_variable(
    constructed_vs_natural_vitality_claim,
    'Is ''Hebrew was living through Haskalah literary production'' a natural historical description, or is it a retrospectively constructed narrative that serves the modern Hebrew literary canon''s self-legitimation (i.e., a mild false-summit pattern where a contested vitality claim is treated as settled because the literary establishment that benefits from the framing controls its historiography)?',
    'Examine whether historiographical accounts of the Haskalah written by scholars outside the modern Hebrew literary/academic establishment (e.g., general historical linguists, non-Hebraist sociolinguists) converge on or diverge from the vitality claim.',
    'If outside scholarship diverges significantly, the beneficiary-declared claim here would be closer to a constructed narrative serving institutional interests (canon-formation, academic Hebrew departments) than a neutral historical description, which would push this reading toward a tangled_rope reading rather than a clean rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constructed_vs_natural_vitality_claim, conceptual, 'Whether the vitality claim is naturally descriptive or serves the interests of those who benefit from the literary canon''s prestige.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__literary_revival_reading, 1780, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1780, hebrew_living_language__literary_revival_reading, theater_ratio, 1780, 0.05).
narrative_ontology:measurement(hebr_tr_t1800, hebrew_living_language__literary_revival_reading, theater_ratio, 1800, 0.06).
narrative_ontology:measurement(hebr_tr_t1820, hebrew_living_language__literary_revival_reading, theater_ratio, 1820, 0.08).
narrative_ontology:measurement(hebr_tr_t1840, hebrew_living_language__literary_revival_reading, theater_ratio, 1840, 0.1).
narrative_ontology:measurement(hebr_tr_t1860, hebrew_living_language__literary_revival_reading, theater_ratio, 1860, 0.12).
narrative_ontology:measurement(hebr_tr_t1880, hebrew_living_language__literary_revival_reading, theater_ratio, 1880, 0.14).
narrative_ontology:measurement(hebr_tr_t1900, hebrew_living_language__literary_revival_reading, theater_ratio, 1900, 0.15).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1780, hebrew_living_language__literary_revival_reading, base_extractiveness, 1780, 0.03).
narrative_ontology:measurement(hebr_be_t1800, hebrew_living_language__literary_revival_reading, base_extractiveness, 1800, 0.04).
narrative_ontology:measurement(hebr_be_t1820, hebrew_living_language__literary_revival_reading, base_extractiveness, 1820, 0.05).
narrative_ontology:measurement(hebr_be_t1840, hebrew_living_language__literary_revival_reading, base_extractiveness, 1840, 0.06).
narrative_ontology:measurement(hebr_be_t1860, hebrew_living_language__literary_revival_reading, base_extractiveness, 1860, 0.07).
narrative_ontology:measurement(hebr_be_t1880, hebrew_living_language__literary_revival_reading, base_extractiveness, 1880, 0.08).
narrative_ontology:measurement(hebr_be_t1900, hebrew_living_language__literary_revival_reading, base_extractiveness, 1900, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_living_language__literary_revival_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__literary_revival_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_living_language__literary_revival_reading, 0.08).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__liturgical_continuity_reading).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__native_generation_reading).

% DUAL FORMULATION NOTE:
% Part of the hebrew_living_language kernel family (3 readings). This story (literary_revival_reading) is authored with very low extractiveness and no victim set, reflecting elite, elective literary practice. The liturgical_continuity_reading sibling addresses an entirely different mechanism (unbroken recitation/study) with its own epsilon and stakeholder set. The native_generation_reading sibling addresses daily generative speech and is expected to carry a different — likely higher scrutiny of exclusion — profile toward those whose Hebrew competence remained written-only. Each reading is a distinct constraint per the epsilon-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
