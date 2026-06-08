% ============================================================================
% CONSTRAINT STORY: discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_discontinuity_reading, []).

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
 *   constraint_id: discontinuity_reading
 *   human_readable: Discontinuity Reading: Classical Latin as Extinct Form Requiring Reconstruction
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   This constraint embodies one reading of a contested kernel: what
 *   constitutes 'correct' or 'proper' Latin. The discontinuity reading
 *   asserts a rupture between Classical Latin (the form preserved in
 *   canonical texts from Cicero through Boethius, treated as pure and
 *   normative) and Medieval Latin (the evolving language of monastic,
 *   administrative, and ecclesiastical institutions from ~500–1500 CE,
 *   treated as corrupt deviation from classical norms requiring
 *   reconstruction and correction). This is not a neutral descriptive
 *   classification but a legitimacy claim: it establishes classical forms as
 *   the standard and relegates medieval variants to the status of error. The
 *   discontinuity reading has deep roots in Renaissance humanist philology
 *   (Petrarch, Valla) where recovering classical purity was a declared
 *   agenda. It persists in modern academia as the canonical framework for
 *   Latin instruction and scholarship, enforced through curriculum design,
 *   publishing standards, hiring criteria, and the organization of philology
 *   departments. The constraint exhibits dual character: it serves a genuine
 *   coordination function (establishing a shared canonical form enables
 *   textual scholarship and teaching) while simultaneously extracting
 *   authority from medieval Latin practitioners and scholarship. The
 *   theater_ratio trajectory (0.25 → 0.48) reflects that as empirical
 *   continuities became harder to ignore, the discontinuity reading
 *   increasingly relied on rhetorical performance and institutional
 *   gate-keeping rather than philological argument.
 *
 * KEY AGENTS:
 *   - Classical Philology Establishment: Institutional beneficiary (institutional/arbitrage) — maintains methodological supremacy and curricular centrality through the discontinuity framing
 *   - Medieval Latin Speaking Communities: Primary victim (powerless/trapped) — their language reclassified as corrupt error; no alternative legitimacy frame within dominant institutions
 *   - Medieval Manuscript Scholars: Secondary victim/participant (moderate/constrained) — benefit from reconstructive methodology but subordinated to classical standard
 *   - Digital Humanities Movements: Organized alternative agents (organized/mobile) — building parallel analysis frameworks that bypass the discontinuity assumption
 *   - Reconstruction Ritual Apparatus: Institutional performance (institutional/arbitrage) — critical editions, emendation apparatus, conjectural reconstruction persist through theater
 *   - Analytical Observer: Long-view assessment (analytical/analytical) — risks naturalizing the legitimacy hierarchy as temporal fact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(discontinuity_reading, 0.65).
domain_priors:suppression_score(discontinuity_reading, 0.58).
domain_priors:theater_ratio(discontinuity_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(discontinuity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(discontinuity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(discontinuity_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(discontinuity_reading, "Discontinuity Reading: Classical Latin as Extinct Form Requiring Reconstruction").
narrative_ontology:topic_domain(discontinuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(discontinuity_reading, '35fde87d-1509-49b5-b028-8651221e8e6f').
narrative_ontology:cs_kernel_codification('35fde87d-1509-49b5-b028-8651221e8e6f', fixed_text).
narrative_ontology:cs_authority_grounding('35fde87d-1509-49b5-b028-8651221e8e6f', lineage).
narrative_ontology:cs_interpretation_layer_present('35fde87d-1509-49b5-b028-8651221e8e6f').
narrative_ontology:cs_reading_relation('35fde87d-1509-49b5-b028-8651221e8e6f', discontinuity_reading__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('35fde87d-1509-49b5-b028-8651221e8e6f', discontinuity_reading__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('35fde87d-1509-49b5-b028-8651221e8e6f', foundational, classical_form_is_pure_norm).
narrative_ontology:cs_axiom_status(classical_form_is_pure_norm, holdable).
narrative_ontology:cs_axiom_grounding('35fde87d-1509-49b5-b028-8651221e8e6f', classical_form_is_pure_norm, conventional).
narrative_ontology:cs_axiom('35fde87d-1509-49b5-b028-8651221e8e6f', foundational, medieval_forms_are_corrupt_deviation).
narrative_ontology:cs_axiom_status(medieval_forms_are_corrupt_deviation, overridden).
narrative_ontology:cs_axiom_grounding('35fde87d-1509-49b5-b028-8651221e8e6f', medieval_forms_are_corrupt_deviation, empirically_contingent).
narrative_ontology:cs_reference_frame('35fde87d-1509-49b5-b028-8651221e8e6f', classical_purity_standard).
narrative_ontology:cs_drift_state('35fde87d-1509-49b5-b028-8651221e8e6f', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('35fde87d-1509-49b5-b028-8651221e8e6f', '').
narrative_ontology:cs_kernel_id(discontinuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(discontinuity_reading, classical_philology_establishment).
narrative_ontology:constraint_beneficiary(discontinuity_reading, reconstruction_methodology).
narrative_ontology:constraint_victim(discontinuity_reading, medieval_latin_practitioners).
narrative_ontology:constraint_victim(discontinuity_reading, linguistic_continuity_understanding).
narrative_ontology:constraint_vindicates(discontinuity_reading, classical_supremacy_doctrine).
narrative_ontology:constraint_vindicates(discontinuity_reading, rupture_between_periods).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MEDIEVAL LATIN SPEAKERS (SNARE) — Structurally trapped. Their living language (medieval Latin as spoken/written in monastic, administrative, liturgical contexts) is redefined as corrupt deviation from a dead form. No exit from the classification; no alternative legitimacy frame available within the dominance of classical philology. Maximum extraction: their language is reclassified as error requiring correction from external texts rather than recognized as legitimate adaptation.
constraint_indexing:constraint_classification(discontinuity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MEDIEVAL MANUSCRIPT SCHOLARS (TANGLED_ROPE) — Experience coordination function (texts must be read, manuscripts must be studied) alongside extraction. Constrained by institutional authority of classical philology departments and publishing gatekeepers; also benefit from access to rich manuscript traditions and collaborative textual study. The discontinuity reading creates both a working methodology (reconstructive emendation) and a subordinate position (medieval texts are primary only as evidence for recovering classical forms).
constraint_indexing:constraint_classification(discontinuity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CLASSICAL PHILOLOGY ESTABLISHMENT (ROPE) — Beneficiary with full arbitrage options. The discontinuity reading vindicates their methodological supremacy: classical texts are the legitimate form, medieval variants are deviations to be corrected back toward classical purity. Experiences the constraint as coordination — establishing a normative standard enables textual scholarship. Net beneficiary: institutional authority, methodological canon, hiring/publication gatekeeping, curricular centrality derive from the claim that classical is the true form and medieval is corrupt deviation.
constraint_indexing:constraint_classification(discontinuity_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DIGITAL HUMANITIES MOVEMENTS (SCAFFOLD) — Organized agents (digital text projects, computational analysis, machine-learning text classification) see the discontinuity reading as a temporary institutional gate that new methodologies can bypass. Sunset logic: large-scale computational comparison of classical and medieval forms reveals continuities (lexical drift, morphological variation, systematic sound changes) that the discontinuity reading cannot accommodate. Mobile exit: build parallel digital archives and analysis frameworks outside the classical philology establishment. Generational timeframe reflects the maturation of these alternative epistemic communities over 2-3 academic generations.
constraint_indexing:constraint_classification(discontinuity_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: THE RECONSTRUCTION RITUAL (PITON) — The apparatus of 'restoring' classical forms from medieval corruptions (critical editions, emendation apparatus, reconstructive conjectures) persists as performance. The high theater_ratio (0.48 is deceptive — the apparatuses are mostly theatrical) reflects that conjectures about lost originals from medieval copies are substantially unfalsifiable; the ritual maintains scholarly authority through technical sophistication rather than epistemic productivity. The ritual persists through institutional inertia even as its evidentiary foundations erode.
constraint_indexing:constraint_classification(discontinuity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — TEMPORAL PRIORITY (MOUNTAIN) — From a long view, the classical form IS temporally prior and IS the ancestor of medieval forms. Temporal priority appears natural and irreducible: earlier texts cannot be dependent on later texts. However, this perspective naturalizes the asymmetry of authority (prior = pure, later = corrupt) which is a hermeneutic choice, not a logical necessity. The engine will flag this as a false summit: temporal priority is being used as a cover for legitimacy hierarchy that would not survive scrutiny.
constraint_indexing:constraint_classification(discontinuity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(discontinuity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(discontinuity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(discontinuity_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(discontinuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(discontinuity_reading, TR),
    TR >= 0.70.

:- end_tests(discontinuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): Moderate-high, rising. The discontinuity reading extracts substantial authority from medieval Latin practitioners by reclassifying their language as deviation rather than adaptation. The extraction increases over time as empirical continuities become harder to deny and institutional enforcement substitutes for philological argument. Base value (0.65) reflects the current strength of institutional gate-keeping. Suppression (0.58): Moderate-high. Barriers to legitimizing medieval Latin as continuous innovation include: curricula organized around classical priority, publishing gatekeepers favoring classical focus, departmental hiring centered on classical expertise, textbook framing that treats medieval as 'decline'. Medieval practitioners cannot exit without institutional cost. Theater ratio (0.48): Moderate, rising. The reconstruction apparatus (critical editions with conjectures, emendation marks, reconstructed texts) maintains scholarly authority through technical sophistication, but as evidence for lost classical originals from medieval copies it is substantially unfalsifiable. The ratio rises over time as the apparatus becomes more elaborate while its evidentiary foundation erodes.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates irreducible perspectival divergence. Classical philologists see coordination and legitimacy: establishing a canonical form enables scholarship. Medieval specialists see extraction and subordination: their work is instrumentalized for recovering classical forms. Digital humanists see a temporary institutional gate with an alternative pathway: computational analysis will eventually reveal continuities that the discontinuity frame cannot accommodate. The analytical observer from a long temporal view risks naturalizing the hierarchy (classical is prior, therefore pure) — but this confuses temporal sequence with normative authority. The false summit here is the claim that discontinuity is an objective fact about language rather than a hermeneutic choice about legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from its structural position relative to the extraction flow. Medieval speakers are trapped with no alternative legitimacy frame — they experience d near 1.0 (full target). Medieval scholars are constrained by institutional subordination but also benefit from methodological access — they experience d around 0.65. Classical philologists are institutional beneficiaries with full arbitrage options — they experience d near 0.0 (full beneficiary). Digital humanists have organized agency and mobile exit via alternative methodologies — they experience d around 0.40. The analytical observer at civilizational scale risks experiencing d near 0.0 (the temporal priority view that benefits the current power structure) — the engine's false summit detector should flag this as naturalization.
 *
 * MANDATROPHY ANALYSIS:
 *   The discontinuity reading originally had a coherent mandate: during the Renaissance, recovering classical texts from medieval corruptions was a genuine intellectual project with clear function (restoring lost learning). The mandate has outlived its function: modern scholarship has sufficient access to classical texts that the systematic reconstruction of classical originals from medieval copies is no longer the primary epistemic problem. Yet the institutional apparatus (the discontinuity framing, classical supremacy in curricula, philological authority hierarchy) persists. This is classical mandatrophy: the goal that justified the structure (recovering lost classical knowledge) is accomplished, but the structure itself — now functioning primarily as a legitimacy hierarchy and gate-keeping mechanism — remains. The constraint should be marked `mandatrophy_resolved: true` but is not institutionally recognized as mandatrophied, which is precisely why the discontinuity reading persists as tangled_rope rather than collapsing to pure snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the discontinuity reading a coherent constraint or a cover story for classical supremacy?',
    'Comparative analysis of reading_relations: does the discontinuity reading logically foreclose the continuity reading, or do both remain live within competing epistemologies? If the latter, the ''discontinuity'' is not a natural fact but a perspectival choice.',
    'If forecloses: discontinuity is defensible as a knowledge claim about rupture. If coexists_with or influences: discontinuity is a constructed legitimacy hierarchy, reclassifying to snare from analytical perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether discontinuity reading logically forecloses continuity reading or both remain live').

omega_variable(
    medieval_form_legitimacy,
    'Are medieval Latin forms deviations from classical norms, or legitimate linguistic innovation within the constraints of a changed communication environment?',
    'Diachronic sociolinguistic analysis: comparison with other known language communities undergoing institutional/communicative shift (early modern English, post-classical Greek, modern Romance languages). Do the medieval forms follow predictable drift patterns or represent random corruption?',
    'If legitimate innovation: the discontinuity reading is false; medieval Latin should be classified as adaptive continuity. If corruption: discontinuity reading is empirically supported. Current scholarship increasingly supports the innovation reading (Banniard, Mohrmann, Mantello).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medieval_form_legitimacy, empirical, 'Whether medieval Latin forms are corruption or legitimate linguistic innovation').

omega_variable(
    reconstruction_evidential_status,
    'Do conjectures about lost classical originals from medieval copies have the same evidential status as attested classical texts?',
    'Epistemological audit: examination of how conjectures (marked with daggers and brackets in critical editions) are cited and used in secondary scholarship. Are they treated as evidence or as working hypotheses?',
    'If same status: reconstruction apparatus is largely theatrical (piton classification confirmed). If different status: classical philology maintains a coherent two-tier system (attested vs reconstructed), and the theater_ratio should be lower.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reconstruction_evidential_status, empirical, 'Whether conjectures about lost originals have evidential parity with attested texts').

omega_variable(
    authority_grounding_shift,
    'Has the legitimacy ground for the discontinuity reading shifted from philological argument to institutional gate-keeping?',
    'Historical analysis of how the discontinuity reading entered the academy (humanist philology, 15th-17th century) vs how it is currently enforced (curriculum, publishing standards, hiring criteria). Did grounds of argument precede grounds of authority?',
    'If grounding shifted: extraction component increased over time (true for authority_erosion drift direction). The constraint should show rising theater_ratio across historical measurements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_shift, empirical, 'Whether discontinuity reading''s legitimacy ground shifted from argument to institutional gate-keeping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(discontinuity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(discont_tr_t0, discontinuity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(discont_tr_t10, discontinuity_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(discont_tr_t20, discontinuity_reading, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(discont_be_t0, discontinuity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(discont_be_t10, discontinuity_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(discont_be_t20, discontinuity_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(discont_su_t0, discontinuity_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(discont_su_t10, discontinuity_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(discont_su_t20, discontinuity_reading, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(discontinuity_reading, information_standard).
narrative_ontology:affects_constraint(discontinuity_reading, continuity_reading).
narrative_ontology:affects_constraint(discontinuity_reading, hybrid_reading).

% DUAL FORMULATION NOTE:
% The discontinuity reading, continuity reading, and hybrid reading form a constraint family organized around a single kernel ('correct_latin'). Each reading instantiates a different ε value and different victim/beneficiary structure because they make structurally different claims about what constitutes legitimacy. The discontinuity reading is most extractive (0.65) because it establishes a hierarchy that subordinates medieval forms. The continuity reading would show lower extractiveness by recognizing medieval innovation as legitimate. The hybrid reading would show negligible extraction by dissolving the hierarchy altogether. All three are readings of the same kernel; each should be authored as a separate constraint story with its own perspectives and metrics. Network links declare the family membership.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
