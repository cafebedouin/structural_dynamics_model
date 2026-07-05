% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__reformist_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_corpus_social_prescription__reformist_spiritual_reading, []).

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
 *   constraint_id: vedic_corpus_social_prescription__reformist_spiritual_reading
 *   human_readable: Reformist Spiritual Reading of the Vedic Corpus (Non-Prescriptive Cosmology)
 *   domain: religious_studies/hermeneutics/social_stratification
 *
 * SUMMARY:
 *   This story instantiates the reformist_spiritual_reading of the
 *   vedic_corpus_social_prescription kernel: the claim that the Vedic corpus
 *   (Samhitas, Brahmanas, Upanishads) contains spiritual and cosmological
 *   content but no binding social prescription, and that passages read
 *   literalistically as caste mandate (notably the Purusha Sukta) are either
 *   later interpolations, metaphors for cosmic function, or corruptions of an
 *   originally non-hierarchical teaching. This reading is generated as its
 *   own clean, ε-invariant constraint per Rule 1 — it does not describe or
 *   adjudicate the orthodox_varna_reading or colonial_orientalist_reading,
 *   which are separate constraint stories with their own beneficiary/victim
 *   structures and their own (much higher) extraction profiles. The three
 *   readings are linked via network.affects_constraints as siblings in one
 *   kernel family.
 *
 * KEY AGENTS:
 *   - reformist_hindu_congregations: Primary beneficiary (organized/mobile) — gains non-hierarchical scriptural warrant
 *   - cross_caste_spiritual_seekers: Primary beneficiary (moderate/mobile) — gains study access previously denied
 *   - vedanta_universalist_teachers: Agenda-setter (moderate/mobile) — articulates and transmits the reading persuasively, no coercive power
 *   - orthodox_varna_traditionalists: Excluded from this reading's internal discourse but not materially harmed — doctrinal rival, not victim
 *   - comparative_religion_scholars: Analytical observer — documents textual heterogeneity underlying all three readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.06).
domain_priors:suppression_score(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.08).
domain_priors:theater_ratio(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__reformist_spiritual_reading, rope).
narrative_ontology:human_readable(vedic_corpus_social_prescription__reformist_spiritual_reading, "Reformist Spiritual Reading of the Vedic Corpus (Non-Prescriptive Cosmology)").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__reformist_spiritual_reading, "religious_studies/hermeneutics/social_stratification").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__reformist_spiritual_reading, '1a582795-e303-44ff-bc7c-84f73c022bb2').
narrative_ontology:cs_kernel_codification('1a582795-e303-44ff-bc7c-84f73c022bb2', fixed_text).
narrative_ontology:cs_authority_grounding('1a582795-e303-44ff-bc7c-84f73c022bb2', practice).
narrative_ontology:cs_interpretation_layer_present('1a582795-e303-44ff-bc7c-84f73c022bb2').
narrative_ontology:cs_reading_relation('1a582795-e303-44ff-bc7c-84f73c022bb2', vedic_corpus_social_prescription__orthodox_varna_reading, coexists_with).
narrative_ontology:cs_reading_relation('1a582795-e303-44ff-bc7c-84f73c022bb2', vedic_corpus_social_prescription__colonial_orientalist_reading, coexists_with).
narrative_ontology:cs_axiom('1a582795-e303-44ff-bc7c-84f73c022bb2', foundational, varna_passages_are_metaphorical_not_prescriptive).
narrative_ontology:cs_axiom_status(varna_passages_are_metaphorical_not_prescriptive, holdable).
narrative_ontology:cs_axiom_grounding('1a582795-e303-44ff-bc7c-84f73c022bb2', varna_passages_are_metaphorical_not_prescriptive, conventional).
narrative_ontology:cs_axiom('1a582795-e303-44ff-bc7c-84f73c022bb2', foundational, vedic_metaphysical_unity_doctrine).
narrative_ontology:cs_axiom_status(vedic_metaphysical_unity_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('1a582795-e303-44ff-bc7c-84f73c022bb2', vedic_metaphysical_unity_doctrine, deontological).
narrative_ontology:cs_reference_frame('1a582795-e303-44ff-bc7c-84f73c022bb2', pre_classical_vedantic_universalism).
narrative_ontology:cs_drift_state('1a582795-e303-44ff-bc7c-84f73c022bb2', post_colonial_reform_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('1a582795-e303-44ff-bc7c-84f73c022bb2', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_hindu_congregations).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, cross_caste_spiritual_seekers).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, vedanta_universalist_teachers).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_metaphysical_unity_doctrine).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__reformist_spiritual_reading, varna_as_symbolic_not_literal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities (Arya Samaj-descended, Vedantic universalist, Brahmo-influenced) that read the Vedic corpus as spiritual and metaphorical, using this reading to organize worship and identity without caste-based exclusion. They gain a coherent, defensible textual basis for cross-caste fellowship and are free to leave the reading for another tradition without cost.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_hindu_congregations, beneficiary,
    organized, generational, mobile, national).

% Individuals from historically excluded or lower-Varna backgrounds who use this reading to access Vedic study, ritual participation, and interpretive authority previously denied them under literalist readings. Their access under this reading is voluntary and additive; nothing compels them to adopt it and no penalty attaches to declining it.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, cross_caste_spiritual_seekers, beneficiary,
    moderate, biographical, mobile, national).

% Scholars and teachers (in the lineage of figures like Vivekananda, Dayananda, Radhakrishnan) who articulate and transmit the metaphorical/non-prescriptive reading. They administer the interpretive tradition through commentary, teaching, and institution-building, but hold no coercive enforcement power over rival readings — their authority is persuasive, resting on textual argument and pedagogical reputation.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, vedanta_universalist_teachers, agenda_setter,
    moderate, generational, mobile, national).

% Hold that the corpus does prescribe hierarchy and regard the reformist reading as a modern dilution or erasure of textual meaning. They are not physically or economically excluded from anything by this reading — they simply reject its hermeneutic claim and continue to practice under a different reading. Their objection is doctrinal, not material.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, orthodox_varna_traditionalists, excluded,
    organized, generational, mobile, national).

% Philologists and historians of religion who examine textual layers (Samhita, Brahmana, Upanishad strata) to assess which passages are cosmological/metaphorical versus which carry social-prescriptive content (e.g., Purusha Sukta's varna verse). They document that the corpus is textually heterogeneous and that all three readings selectively foreground different strata.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a textual and doctrinal basis for organizing worship, study, and spiritual community without requiring caste-based exclusion — solving the coordination problem of how a diverse, mobile modern population can share a scriptural inheritance without the hierarchy some strata of that inheritance describe.
% TRANSFER_FUNCTION: Moves interpretive authority and access to Vedic study from a narrow hereditary priestly custodianship toward open congregational and individual access; no material resource, labor, or money is transferred from a victim class to a beneficiary class under this reading.
% ABSENT_VOICES: Orthodox Varna traditionalists and colonial-legal codifiers are not silenced or barred from speaking — they publish, teach, and litigate their own readings freely — but within THIS reading's own community discourse their textual claims are treated as superseded rather than engaged on equal footing, which they experience as an erasure of their hermeneutic position rather than a genuine debate.
% DISAPPEARANCE_RATIONALE: If this reading vanished, reformist congregations and cross-caste seekers would lose their primary textual warrant for non-hierarchical practice and would have to either adopt an orthodox reading, secularize their practice, or construct a new doctrinal basis from other sources (Bhakti poetry, Upanishadic philosophy alone) — a real reorganization of religious life for an organized population, even though no coercive apparatus depends on the reading's survival.
% FOUNDING_PROBLEM: Nineteenth- and twentieth-century reform movements needed to reconcile reverence for the Vedic corpus as foundational scripture with rejection of caste hierarchy and untouchability, which they held to be a moral and social evil inconsistent with the corpus's deeper spiritual content.
% FOUNDING_PROBLEM_CORROBORATION: Independent historians of religion (outside the reform movements themselves) corroborate that caste-based exclusion remains a live social reality in many Hindu communities, and that this reading continues to function as an active theological response rather than a settled or obsolete historical episode; comparative philologists additionally corroborate that the corpus does contain heterogeneous strata, supporting the plausibility (though not the exclusivity) of the metaphorical reading.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__reformist_spiritual_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__reformist_spiritual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__reformist_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.06, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__reformist_spiritual_reading_tests).
:- end_tests(vedic_corpus_social_prescription__reformist_spiritual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.06) because this reading identifies no transfer mechanism: no group's labor, status, or resources are extracted through operation of this specific reading. Suppression is low (0.08) because adoption is voluntary and adherents retain full ability to exit toward an orthodox or secular reading with no penalty. Theater ratio is low (0.10): the reading is genuinely load-bearing for the communities that hold it, not primarily performative. Resistance is moderate (0.35) reflecting active doctrinal contestation from orthodox traditionalists, but this resistance is textual/argumentative, not coercive or violent — it does not indicate suppression of the reading's adherents.
 *
 * PERSPECTIVAL GAP:
 *   From inside the reformist community, this reading is experienced as liberation and coordination — a shared scripture without hierarchy. From the orthodox traditionalist seat, the same textual corpus is experienced as having a plain prescriptive meaning that this reading erases through selective emphasis. The engine computes both seats from the same structural data; the divergence is expected and is the doctrinal contest itself, not a defect in either reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist congregations and cross-caste seekers are declared beneficiaries because the reading demonstrably expands their access and removes a doctrinal barrier that previously constrained them under literalist readings — this pulls their derived directionality toward the beneficiary end. Vedanta universalist teachers occupy the agenda-setter seat but hold no extraction lever: they administer interpretation, not enforcement, and their power is persuasive rather than institutional. No victim group is declared because no identifiable party bears a cost through the mechanism of THIS reading's operation — orthodox traditionalists lose interpretive market share, not material resources, and this is captured as excluded/doctrinal rejection rather than victimhood.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling scriptural reverence with rejection of caste hierarchy) remains live per the corroboration on record, so this is not a mandatrophy case — the reading continues to do the work it was built to do rather than persisting as a hollowed-out shell. Classifying this as a low-epsilon rope rather than forcing it into either an idealized mountain (no natural-law claim is made) or a snare (no victim exists) prevents the analytical error of treating a genuinely low-coercion doctrinal choice as either falsely inevitable or falsely predatory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_stratification_ambiguity,
    'Does the Vedic corpus, read in its full textual context including passages like the Purusha Sukta (Rigveda 10.90), contain genuine social-prescriptive content, or is such content confined to later strata (Brahmanas, Dharmashastra) that this reading correctly excludes from ''the Vedic corpus proper''?',
    'Philological dating and stratigraphic analysis of Samhita versus Brahmana versus Upanishadic layers, cross-referenced against independent historical linguistics scholarship not affiliated with any of the three reformist/orthodox/colonial reading communities.',
    'If genuine early-strata prescriptive content is confirmed, the reformist reading''s claim of a purely non-prescriptive corpus is textually contestable and its zero-victim structure rests on a selective canon rather than the full corpus; if confirmed as late interpolation, the reformist reading''s textual claim strengthens considerably.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_stratification_ambiguity, empirical, 'Whether social-prescriptive content is genuinely present in earliest strata or confined to later accretions.').

omega_variable(
    reading_selection_as_soft_power,
    'Is the choice among these three readings (reformist, orthodox, colonial) purely a matter of hermeneutic argument, or does the reformist reading''s ascendance in certain reform institutions function as a soft mechanism of its own — displacing orthodox practitioners'' interpretive authority through institutional capture (educational curricula, English-medium Hindu reform organizations) rather than through argument alone?',
    'Institutional history of which reading is taught in which schools, seminaries, and reform organizations, and whether access to interpretive authority correlates with prior social capital (English education, urban location) rather than open textual argument.',
    'If institutional capture is found, this reading''s currently-authored zero suppression may understate a soft exclusionary mechanism operating through educational access rather than coercion — this would not change victim declaration but would inform the suppression metric upward in a future revision.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_as_soft_power, conceptual, 'Whether reformist ascendance reflects pure argument or institutional access asymmetry.').

omega_variable(
    kernel_disagreement_location,
    'Where exactly does the disagreement between this reading and the orthodox_varna_reading live — is it a disagreement about what the text SAYS (semantic/philological), or about what authority the text SHOULD have given contemporary moral commitments (normative)?',
    'Careful separation, in comparative-religion scholarship, of philological claims (what a passage denotes) from normative claims (what weight a modern community should give a passage) — the two are frequently conflated by adherents of both readings.',
    'If the disagreement is primarily normative rather than semantic, then this reading''s textual argument functions partly as a normatively-motivated retrofit rather than a discovery, which would not change its low-extraction structure but would reframe the axiom vedic_metaphysical_unity_doctrine as instrumentally rather than purely descriptively grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_disagreement_location, conceptual, 'Whether the reformist/orthodox split is semantic or normative in character.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__reformist_spiritual_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(vedi_tr_t30, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(vedi_tr_t60, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 60, 0.09).
narrative_ontology:measurement(vedi_tr_t90, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 90, 0.1).
narrative_ontology:measurement(vedi_tr_t120, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 120, 0.1).
narrative_ontology:measurement(vedi_tr_t150, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 150, 0.1).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(vedi_be_t30, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 30, 0.05).
narrative_ontology:measurement(vedi_be_t60, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 60, 0.06).
narrative_ontology:measurement(vedi_be_t90, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 90, 0.06).
narrative_ontology:measurement(vedi_be_t120, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 120, 0.06).
narrative_ontology:measurement(vedi_be_t150, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 150, 0.06).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(vedic_corpus_social_prescription__reformist_spiritual_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, orthodox_varna_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, colonial_orientalist_reading).

% DUAL FORMULATION NOTE:
% Three-member constraint family on the vedic_corpus_social_prescription kernel. reformist_spiritual_reading (this story, low-epsilon rope, no victims) coexists with orthodox_varna_reading (higher-epsilon tangled_rope/snare-leaning, victims among lower-Varna/Dalit communities under literalist enforcement) and colonial_orientalist_reading (extraction via administrative codification of customary law into fixed 'Hindu law', victims among communities whose plural customary practices were overridden). All three readings interpret the SAME textual corpus but instantiate structurally distinct constraints with different ε, different beneficiary/victim sets, and different enforcement mechanisms — per the ε-invariance principle, this is three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
