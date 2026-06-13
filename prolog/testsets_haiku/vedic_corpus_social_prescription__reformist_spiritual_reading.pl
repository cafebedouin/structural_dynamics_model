% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__reformist_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: vedic_corpus_social_prescription__reformist_spiritual_reading
 *   human_readable: Vedic Texts as Spiritual Unity and Metaphorical Cosmology (Reformist Reading)
 *   domain: religious_studies/hermeneutics/social_stratification
 *
 * SUMMARY:
 *   The reformist spiritual reading of the Vedic corpus asserts that the
 *   Upanishads and early Vedic hymns describe cosmological unity and
 *   metaphorical imagery with no prescriptive mandate for hereditary social
 *   hierarchy (varna). This reading emerged from 19th-century Hindu reformism
 *   (Raja Mohan Roy, Swami Vivekananda, Dayananda Saraswati) as a response to
 *   both colonial administration (which extracted Vedic texts as a 'Hindu law
 *   code') and internal modernization pressure (rejection of caste as
 *   incompatible with spiritual universalism). The reformist reading is one
 *   of three structurally distinct constraints in the
 *   vedic_corpus_social_prescription kernel: it coexists with the orthodox
 *   varna reading (which asserts literal, prescriptive hierarchy) and opposes
 *   the colonial orientalist reading (which treats the Vedas as
 *   administrative law). This story models ONLY the reformist reading as a
 *   clean, ε-invariant constraint; the siblings are separate constraint files
 *   linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - Reformist Vedic scholars (19th-20th century onward): Hindu and later global interpreters (Vivekananda, modern academics) who reframe Vedic content as spiritual philosophy divorced from social prescription.
 *   - Anti-caste movements: Social actors (Dalit activists, neo-Buddhist converts, egalitarian reformers) who use the reformist reading as intellectual ammunition against caste hierarchy.
 *   - Modern practitioners: Yoga students, diaspora communities, seekers who adopt Vedic spirituality under the reformist reading while maintaining egalitarian values.
 *   - Orthodox varna defenders: Conservative scholars and institutions defending the literal prescriptive reading — excluded from the reformist conversation.
 *   - Secular academic interpreters: Indologists and historians of religion who analyze Vedic texts within historical and comparative frameworks.
 *   - Colonial administrators: Historical witnesses who created the rival 'law code' reading that prompted reformist reinterpretation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.18).
domain_priors:suppression_score(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.12).
domain_priors:theater_ratio(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__reformist_spiritual_reading, rope).
narrative_ontology:human_readable(vedic_corpus_social_prescription__reformist_spiritual_reading, "Vedic Texts as Spiritual Unity and Metaphorical Cosmology (Reformist Reading)").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__reformist_spiritual_reading, "religious_studies/hermeneutics/social_stratification").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__reformist_spiritual_reading, '521abdce-4f04-4b83-afd8-750d48cbe733').
narrative_ontology:cs_kernel_codification('521abdce-4f04-4b83-afd8-750d48cbe733', fixed_text).
narrative_ontology:cs_authority_grounding('521abdce-4f04-4b83-afd8-750d48cbe733', lineage).
narrative_ontology:cs_interpretation_layer_present('521abdce-4f04-4b83-afd8-750d48cbe733').
narrative_ontology:cs_reading_relation('521abdce-4f04-4b83-afd8-750d48cbe733', vedic_corpus_social_prescription__orthodox_varna_reading, coexists_with).
narrative_ontology:cs_reading_relation('521abdce-4f04-4b83-afd8-750d48cbe733', vedic_corpus_social_prescription__colonial_orientalist_reading, influences).
narrative_ontology:cs_axiom('521abdce-4f04-4b83-afd8-750d48cbe733', foundational, vedic_content_metaphorical_not_prescriptive).
narrative_ontology:cs_axiom_status(vedic_content_metaphorical_not_prescriptive, holdable).
narrative_ontology:cs_axiom_grounding('521abdce-4f04-4b83-afd8-750d48cbe733', vedic_content_metaphorical_not_prescriptive, deontological).
narrative_ontology:cs_axiom('521abdce-4f04-4b83-afd8-750d48cbe733', secondary, caste_hierarchy_later_corruption).
narrative_ontology:cs_axiom_status(caste_hierarchy_later_corruption, holdable).
narrative_ontology:cs_axiom_grounding('521abdce-4f04-4b83-afd8-750d48cbe733', caste_hierarchy_later_corruption, empirically_contingent).
narrative_ontology:cs_reference_frame('521abdce-4f04-4b83-afd8-750d48cbe733', vedic_spiritual_universalism).
narrative_ontology:cs_drift_state('521abdce-4f04-4b83-afd8-750d48cbe733', contemporary_postcolonial_context, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('521abdce-4f04-4b83-afd8-750d48cbe733', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, brahmanical_reformist_scholars).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, anti_caste_movement_advocates).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, modern_hindu_practitioners_seeking_spiritual_universalism).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__reformist_spiritual_reading, 'none', 1).

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
 *   Extractiveness is low (0.18) because the reformist reading involves no coercive mechanism and no asymmetric gain — the beneficiary groups (reformist scholars, anti-caste movements, universal practitioners) gain intellectual and spiritual access but do not extract rents from anyone. There is no victim set. Suppression is minimal (0.12) because no alternative reading is forcibly excluded — the orthodox reading remains live, argued by defenders. The reformist reading wins through persuasion and institutional authority, not through prohibition. Theater is very low (0.08) because the coordination function is genuine: the reading actually permits practitioners to engage Vedic material without endorsing hierarchy. The modest upward drift in extractiveness (0.12→0.18) reflects a gradual shift toward more scholastic gatekeeping (reformist interpretation becoming institutionalized in universities and modern religious centers), which raises the implicit cost of challenging the reading. Suppression and theater also rise slightly as the reformist reading solidifies as institutional orthodoxy in some contexts (universities, diaspora organizations), but remain low overall. Accessibility collapse is low (0.25) because alternatives (orthodox, secular, orientalist readings) remain intellectually available and defended. Resistance is moderate (0.35) reflecting continued pushback from orthodox defenders and secular scholars who dispute the reformist hermeneutics.
 *
 * PERSPECTIVAL GAP:
 *   From the reformist scholar seat: the reading is genuine intellectual work recovering the Vedas' true spiritual content and separating it from later corruptions (caste hierarchies embedded in Dharmashastra). From the orthodox defender seat: the reading is a capitulation to modern pressure that strips the Vedas of their prescriptive force. From the anti-caste movement seat: the reading is a resource, politically necessary for claiming the Vedic tradition does not mandate oppression. From the secular academic seat: all three readings (reformist, orthodox, colonial) are hermeneutical artifacts reflecting their historical contexts; none is the 'true' Vedic meaning. The engine computes these as different seats with different power levels and exit options; the divergence emerges from the structural data, not from author reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist scholars and anti-caste beneficiaries sit at low d (beneficiaries: they gain intellectual prestige and movement resources without bearing extraction costs). The organized varna defenders sit at moderate d (they bear the cost of being positioned as non-scriptural defenders, but retain institutional bases and exit options). Secular scholars sit at d≈0.5 (symmetric: they gain analytical authority, but the reformist reading constrains their freedom to describe all Vedic claims as equally contingent). The constraint's directionality derives from beneficiary presence (reformist and anti-caste groups) and absence of victim groups. No seat is trapped or identity-locked; all have exit options (scholars can move to rival readings, practitioners can adopt secular or orthodox framings). This grounds the low effective extraction χ: beneficiaries are near 0.0, no one is trapped at 1.0.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live (the tension between inherited spiritual authority and egalitarian ethics persists), and the reformist reading is an active solution to it. There is no mandatrophy: the reading was built to solve a real coordination problem (how to access Hindu spirituality without endorsing caste) and still solves it. If the reformist reading is classified by the engine as a rope rather than something more extractive, the classification confirms that the low metrics are descriptively accurate — genuine coordination without hidden victim sets. If the engine were to classify it as tangled rope (coordination + extraction), that would signal that the reformist reading itself has become a gatekeeping mechanism that benefits scholastic elites at the expense of practitioners or traditional interpreters — which is an empirical claim about institutional capture, not about the reading's founding function. The measurement data (low and stable metrics) suggests no such capture has yet occurred at scale.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spiritual_vs_prescriptive_boundary,
    'Is the distinction between ''spiritual cosmology'' (metaphorical, non-prescriptive) and ''social prescription'' (literal, binding) itself a modern hermeneutical imposition on the Vedas, or does it reflect a real content distinction in the texts?',
    'Close textual analysis of passages claimed as ''purely spiritual'' under the reformist reading, cross-checked against how the same passages function in contexts where they do appear to justify or regulate social practice (Dharmashastra commentaries, ritual authority structures). Determine whether the boundary is in the text or in the reader''s interpretive frame.',
    'If the boundary is a modern imposition, the reformist reading is less a recovery of original Vedic content and more a creative reinterpretation for modern purposes — which would suggest higher extractiveness (using tradition selectively for contemporary goals) and potentially classify the constraint as tangled rope rather than rope. If the boundary is textually grounded, the reformist reading is descriptively accurate and the classification as rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spiritual_vs_prescriptive_boundary, conceptual, 'Whether the spiritual/prescriptive distinction is textually immanent or hermeneutically imposed.').

omega_variable(
    reformist_scholarly_gatekeeping,
    'As the reformist reading becomes institutionalized in universities and diaspora religious centers, does it function as a gatekeeping mechanism that excludes orthodox and secular readings from legitimate interpretive authority?',
    'Institutional analysis: survey curriculum inclusion, citation patterns in academic and popular literature, accessibility of competing readings in institutional contexts. Measure whether orthodox and secular interpretations are presented as live alternatives or as refuted positions.',
    'If gatekeeping is occurring, the theater_ratio would rise significantly and extractiveness would increase (beneficiary scholars would be extracting prestige and institutional position from monopolizing interpretive authority). The constraint might reclassify as tangled rope (coordination function + institutional extraction). If competitive pluralism persists, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformist_scholarly_gatekeeping, empirical, 'Whether institutional acceptance of the reformist reading suppresses competing interpretations.').

omega_variable(
    anti_caste_movement_dependency,
    'How much of the reformist reading''s persistence depends on its utility to anti-caste social movements versus its intrinsic scholarly merits?',
    'Historical analysis of reformist scholarship independent of movement support; measure citation and institutional adoption in contexts (academic philosophy, mystical practice) where anti-caste political utility is low. Determine the reading''s stability absent movement demand.',
    'High dependency would suggest the reading is sustained partly by instrumental need rather than scholarly conviction, raising extractiveness slightly (the reading serves movement interests, not pure coordination). Low dependency would confirm genuine philosophical commitment independent of politics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(anti_caste_movement_dependency, empirical, 'The degree to which the reformist reading''s persistence reflects movement utility versus scholarly consensus.').

omega_variable(
    contested_kernel_underspecification,
    'Is the reformist reading''s claim that Vedic texts contain ''no prescriptive social content'' an accurate characterization, or is it a hermeneutical choice to read prescriptive passages as metaphorical or later interpolations?',
    'Textual scholarship on passages containing explicit or implicit social norms (ritual authority, gender roles, occupational roles). Determine whether these passages are integral to the Vedic corpus or later additions. Evaluate the reformist move to treat varna-relevant passages as metaphorical rather than prescriptive.',
    'If prescriptive content IS integral to the Vedas, the reformist reading involves selective reading and higher extractiveness (choosing which content to privilege). If prescriptive content is genuinely absent or later-interpolated, the reading is descriptively accurate and extractiveness remains low. This affects whether the constraint is a pure rope or a tangled-rope with hidden selectivity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contested_kernel_underspecification, empirical, 'Whether Vedic texts contain inherent prescriptive social content that the reformist reading elides.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__reformist_spiritual_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement(vedi_tr_t5, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 5, 0.065).
narrative_ontology:measurement(vedi_tr_t10, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 10, 0.072).
narrative_ontology:measurement(vedi_tr_t15, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 15, 0.078).
narrative_ontology:measurement(vedi_tr_t20, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 20, 0.083).
narrative_ontology:measurement(vedi_tr_t25, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 25, 0.08).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(vedi_be_t5, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 5, 0.14).
narrative_ontology:measurement(vedi_be_t10, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 10, 0.16).
narrative_ontology:measurement(vedi_be_t15, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 15, 0.17).
narrative_ontology:measurement(vedi_be_t20, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(vedi_be_t25, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 25, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(vedi_su_t5, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 5, 0.09).
narrative_ontology:measurement(vedi_su_t10, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement(vedi_su_t15, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 15, 0.11).
narrative_ontology:measurement(vedi_su_t20, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 20, 0.115).
narrative_ontology:measurement(vedi_su_t25, vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 25, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__reformist_spiritual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.06).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription__orthodox_varna_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription__colonial_orientalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the vedic_corpus_social_prescription kernel. The three sibling readings (reformist_spiritual_reading, orthodox_varna_reading, colonial_orientalist_reading) are structurally distinct constraints with different ε values and beneficiary/victim sets, emitted from the same contested textual kernel. The reformist reading asserts low extraction with no victim set (genuine coordination). The orthodox reading asserts higher extraction with a victim set (non-Brahmins subject to varna hierarchy). The colonial reading asserts institutional extraction by colonial administrators codifying Vedic texts as law. All three readings coexist in contemporary discourse; none logically forecloses the others, but they influence operating conditions (the reformist reading's institutional success constrains the orthodox reading's public legitimacy; the colonial reading's historical fact constrains both Indian readings' operating environment). Decomposition justified by ε-invariance principle: the same textual kernel yields different constraint structures under different readings, because the readings differ on what the Vedas prescribe. Each reading is a separate constraint with its own ε, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
