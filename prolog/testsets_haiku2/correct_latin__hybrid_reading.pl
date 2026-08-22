% ============================================================================
% CONSTRAINT STORY: correct_latin__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin__hybrid_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: correct_latin__hybrid_reading
 *   human_readable: Correct Latin: Hybrid Reading (Medieval Practice with Textual Correction)
 *   domain: intellectual_history/historical_linguistics
 *
 * SUMMARY:
 *   The hybrid reading frames Correct Latin as the Classical form preserved
 *   in ancient texts but transmitted and partially evolved through medieval
 *   practice. Medieval forms retain legitimacy insofar as they preserve the
 *   grammatical core, but orthography, vocabulary, and stylistic choices are
 *   subject to correction via comparison with recovered Classical
 *   manuscripts. This is neither full continuity (medieval Latin is not
 *   simply valid evolved Classical) nor full discontinuity (medieval practice
 *   is not mere corruption). The reading partitions legitimacy: medieval
 *   *transmission* is partially valid; medieval *deviation* from textual
 *   sources is a correctable error. The constraint extracts authority from
 *   medieval institutions and concentrates it in humanist scholarship, while
 *   claiming only to refine practice toward a recoverable standard. The
 *   measurement series tracks the rising authority of humanist textual
 *   scholarship from 1200 (early manuscript recovery) to 1600 (humanist
 *   methodology stabilized), with a plateau after 1500 as the reading became
 *   institutionalized.
 *
 * KEY AGENTS:
 *   - Medieval Ecclesiastical Authority: Sets the initial standard, controls transmission through schools, claims continuity with Classical practice
 *   - Humanist Reformers: Recover Classical texts, gain authority via textual scholarship, reframe medieval practice as correctable deviation
 *   - Medieval Copyists: Produce the transmitted texts, adapt Classical forms to practical use, have no authority to defend their choices
 *   - Non-Elite Latin Speakers: Learn and use the medieval register, are marked as speaking corrupt forms under the hybrid reading
 *   - Classical Text Archive: Serves as the normative referent (via humanist interpretation) for what is correct
 *   - Continuity Advocates: Defend medieval Latin as legitimate evolution; excluded from the authority structure that defines correctness
 *   - Textual Scholar Profession: Modern observer seat that can track the constraint's asymmetries and the rise of humanist authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin__hybrid_reading, 0.48).
domain_priors:suppression_score(correct_latin__hybrid_reading, 0.52).
domain_priors:theater_ratio(correct_latin__hybrid_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(correct_latin__hybrid_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin__hybrid_reading, "Correct Latin: Hybrid Reading (Medieval Practice with Textual Correction)").
narrative_ontology:topic_domain(correct_latin__hybrid_reading, "intellectual_history/historical_linguistics").

domain_priors:requires_active_enforcement(correct_latin__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin__hybrid_reading, '4a455ce9-2141-4805-b10a-138ba162067e').
narrative_ontology:cs_kernel_codification('4a455ce9-2141-4805-b10a-138ba162067e', distributed).
narrative_ontology:cs_authority_grounding('4a455ce9-2141-4805-b10a-138ba162067e', lineage).
narrative_ontology:cs_interpretation_layer_present('4a455ce9-2141-4805-b10a-138ba162067e').
narrative_ontology:cs_reading_relation('4a455ce9-2141-4805-b10a-138ba162067e', correct_latin__continuity_reading, influences).
narrative_ontology:cs_reading_relation('4a455ce9-2141-4805-b10a-138ba162067e', correct_latin__discontinuity_reading, coexists_with).
narrative_ontology:cs_axiom('4a455ce9-2141-4805-b10a-138ba162067e', foundational, medieval_transmission_partially_legitimate).
narrative_ontology:cs_axiom_status(medieval_transmission_partially_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('4a455ce9-2141-4805-b10a-138ba162067e', medieval_transmission_partially_legitimate, conventional).
narrative_ontology:cs_axiom('4a455ce9-2141-4805-b10a-138ba162067e', foundational, textual_evidence_corrects_deviation).
narrative_ontology:cs_axiom_status(textual_evidence_corrects_deviation, holdable).
narrative_ontology:cs_axiom_grounding('4a455ce9-2141-4805-b10a-138ba162067e', textual_evidence_corrects_deviation, empirically_contingent).
narrative_ontology:cs_reference_frame('4a455ce9-2141-4805-b10a-138ba162067e', medieval_transmitted_practice_with_continuity).
narrative_ontology:cs_drift_state('4a455ce9-2141-4805-b10a-138ba162067e', early_humanist_emergence_1400s, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4a455ce9-2141-4805-b10a-138ba162067e', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(correct_latin__hybrid_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, medieval_ecclesiastical_authority).
narrative_ontology:constraint_beneficiary(correct_latin__hybrid_reading, humanist_reformers).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, medieval_copyists).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, non_elite_latin_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(correct_latin__hybrid_reading, humanist_reformers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Church maintained Latin as the liturgical and institutional language, transmitting forms through monastic and cathedral schools. They controlled which texts counted as authoritative, which forms were acceptable in sacred writing, and which deviations were permissible in practical use. They claimed continuity with Classical Latin while accepting medieval innovations (the 'living language' framing). Their material interest is in preserving institutional unity: breaking transmission would splinter the Church's administrative apparatus.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, medieval_ecclesiastical_authority, agenda_setter,
    institutional, civilizational, constrained, continental).

% From the 14th century onward, they claimed exclusive access to true Classical forms via manuscript recovery and textual scholarship. They gained prestige, patronage, and institutional authority by positioning themselves as the keepers of correct Latin against medieval corruption. They paid the cost of this identity by mastering an archive-dependent discipline and accepting a dependent relationship to ancient texts. They benefit by colonizing the definition of correctness itself.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, humanist_reformers, beneficiary,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin__hybrid_reading, humanist_reformers, payer).

% Produced the texts in daily use, adapted Classical forms to practical writing (abbreviations, phonetic spelling, functional innovations). They had no formal authority to declare their choices legitimate. Under the hybrid reading, their orthographic and grammatical choices are subject to correction by humanist authority, making their centuries of practice retroactively wrong. They cannot defend their choices against texts they did not control.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, medieval_copyists, payer,
    powerless, biographical, trapped, local).

% Learned and used the Latin transmitted through schools and church. Under medieval practice, their dialect was legitimate functional Latin. Under the hybrid reading, they are users of a corrupt register, marked as not-quite-right. Their professional identity (clerk, monk, scholar) is fused to the language they speak; exit means departing from the linguistic community that constituted them.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, non_elite_latin_speakers, payer,
    powerless, biographical, identity_locked, regional).

% The recovered corpus of Classical authors (Cicero, Virgil, etc.) functions as the normative referent under the hybrid reading. It is not a party, but the archive's authority (via humanist interpretation) is the machinery that corrects medieval practice. The archive itself has no interests, but treating it as authoritative transfers power from medieval institutions to textual scholars.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, classical_text_archive, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(correct_latin__hybrid_reading, classical_text_archive).

% The living practice of producing authoritative texts — the conventions, abbreviations, and orthographic choices embedded in scribal manuals and institutional training. It is not a party to the constraint, but under the hybrid reading it is retroactively declared corrupt, requiring correction. The tradition itself cannot defend its accumulated choices.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, ecclesiastical_scribal_tradition, payer,
    moderate, civilizational, constrained, continental).
narrative_ontology:stakeholder_non_agent(correct_latin__hybrid_reading, ecclesiastical_scribal_tradition).

% Medieval scholars and some modern defenders of medieval practice who argued (or would argue) that medieval Latin was a legitimate evolution, not corruption. They are structurally absent from the hybrid reading's authority structure — their voices were marginalized as humanist authority rose. They are kept out of the conversation that defines correctness.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, continuity_advocates, excluded,
    moderate, biographical, constrained, continental).

% Modern historical linguists and philologists who study Latin across both registers without privileging either. They observe the hybrid reading's authority structure and can detect its asymmetries: the archive is authoritative because scholars say it is; medieval forms are wrong because humanist authority says they are. They track how the constraint shifts over time with new manuscript discoveries and interpretive frameworks.
narrative_ontology:constraint_stakeholder(correct_latin__hybrid_reading, textual_scholar_profession, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin__hybrid_reading, humanist_reformers).
narrative_ontology:fixing_cost_class(correct_latin__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared normative standard for Latin across a fragmented medieval Europe: instead of regional dialects diverging irretrievably, a recoverable Classical core provides a common reference point that enables cross-regional written communication and preserves the institutional unity of the Church.
% TRANSFER_FUNCTION: Transfers interpretive authority from medieval ecclesiastical institutions (which controlled transmission through schools) to humanist scholars (who control interpretation of the Classical archive). Medieval copyists' centuries of adaptive practice are reframed as error; humanist mastery of texts becomes the access point to correctness. Prestige and patronage flow to those who claim closest fidelity to Classical forms.
% ABSENT_VOICES: Continuity advocates — defenders of medieval Latin as a legitimate living evolution — are structurally absent from the authority structure that defines correctness. Copyists and non-elite speakers have no formal seat at the table of judgment. They are affected by the standard but excluded from setting it.
% DISAPPEARANCE_RATIONALE: Medieval ecclesiastical authority would argue the constraint still serves coordination: removing the textual correction apparatus would not collapse medieval institutional Latin, which continued to function as a communication standard for centuries. Humanist scholars would argue that without the constraint, the Classical archive loses its normative force and correctness becomes purely contingent on transmitted practice. The parties disagree about whether the constraint sustains or merely privileges a particular reading of correctness.
% FOUNDING_PROBLEM: After the classical collapse of Roman political unity, the Latin language fragmented into regional scripts, dialects, and functional variants across monastic, ecclesiastical, and administrative contexts. Without a shared written standard, institutional communication across Christendom risked becoming mutually unintelligible. The constraint was built to preserve a recoverable Classical core that all educated Latin users could recognize and align toward.
% FOUNDING_PROBLEM_CORROBORATION: Medieval ecclesiastical authorities attest the founding problem was live and urgent: the need to maintain intelligibility across scattered monasteries and dioceses. Humanist reformers attest the problem persists but is being solved wrongly (through medieval corruption rather than textual recovery). Modern philologists attest the founding problem was real but note that medieval practice successfully maintained mutual intelligibility without Classical purism — the problem was solved pragmatically, not through the hybrid reading's mechanism.
narrative_ontology:disappearance_verdict(correct_latin__hybrid_reading, contested).
narrative_ontology:founding_problem_status(correct_latin__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin__hybrid_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The hybrid reading is a tangled rope: it coordinates around a shared written standard (genuine coordination benefit) while simultaneously transferring interpretive authority from medieval institutions to humanist scholars (asymmetric extraction). Extractiveness rises from 0.32 to 0.48 over 400 years as humanist authority consolidates and medieval forms are increasingly retroactively delegitimized. Suppression rises from 0.28 to 0.52 as the constraint requires active enforcement — copyists must be corrected, medieval speakers must be taught they are wrong, continuity advocates must be excluded from the authority structure. Theater ratio rises sharply from 0.18 to 0.42 between 1200 and 1500 (humanist methods are performatively deployed to establish authority) and plateaus after 1500 (the reading becomes institutionalized and the performative work diminishes). The constraint is not pure extraction because it solves a real coordination problem (shared written standard); it is not pure coordination because the solution concentrates authority in a new institutional seat that benefits from the old seat's delegitimization.
 *
 * PERSPECTIVAL GAP:
 *   From the medieval ecclesiastical authority seat, the constraint looks like legitimate refinement: medieval practice is preserved, only error-correction is added. From the humanist seat, the constraint looks like the recovery and restoration of true Classical forms against medieval corruption. From the copyist and non-elite speaker seats, the constraint looks like retroactive wrongness: centuries of legitimate adaptive practice are suddenly declared corrupt by an authority they did not choose and cannot appeal. From the observer seat, the gap is structural: the hybrid reading's partitioning of legitimacy (medieval transmission valid, medieval deviation correctable) is maintained by humanist authority's exclusive access to the Classical archive. If copyists had equal access to Classical texts and equal authority to interpret them, the constraint would be genuinely symmetric; the extraction comes from the asymmetry of authority, not from the standard itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Medieval ecclesiastical authority is the beneficiary (controls the coordination standard, preserves institutional unity) and the payer (cedes interpretive authority to humanists). Their directionality is near symmetric (d ≈ 0.48) because they benefit from the coordination but are actively displaced by the reading's mechanism. Humanist reformers are the beneficiaries (gain prestige, authority, patronage) with minor cost (must master texts) — d ≈ 0.25. Medieval copyists are the pure targets (their choices are retroactively wrong, they have no formal appeal) — d ≈ 0.85. Non-elite speakers are trapped targets (their professional identity is fused to the language they speak, identity_locked exit) — d ≈ 0.82. The classical archive and ecclesiastical tradition are non-agents (they have no interests, though they are affected). Continuity advocates are excluded (d → the analysis does not compute d for excluded seats; they are present but not in the authority structure). The textual scholar profession is analytical (observer seat, d undefined).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining intelligible Latin across fragmented Christendom) is contested in status: medieval ecclesiastical authority and modern philologists attest it was solved pragmatically through transmitted practice; humanist reformers attest it is solved correctly only through textual recovery. Under the hybrid reading, the mandate never dies — there is always textual correction work to do. But the mandate does shift: from 'maintain intelligible shared standard' to 'refine toward Classical purity.' The constraint avoids mandatrophy (a dead founding problem maintained theatrically) by continually redeclaring the problem: medieval forms are still being discovered to be wrong, so the correction work is perpetually necessary. However, the theater ratio rises during the humanist ascendancy (1300–1500) and plateaus (1500–1600), suggesting the performative work of establishing humanist authority peaks and then settles into routine enforcement, a piton-adjacent dynamic. The hybrid reading is not yet a piton (the coordination function is real, authority is still being actively exercised), but it is vulnerable to becoming one if the Classical archive stabilizes and humanist authority becomes self-evident.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medieval_legitimacy_boundary,
    'What linguistic features count as preserving the grammatical core (legitimate medieval adaptation) versus deviating from it (error requiring correction)?',
    'Comparative analysis across the interval: identify which medieval features humanists accepted as legitimate and which they corrected. Map the boundary and test whether it follows from structural linguistic principles (phonetic evolution, morphological regularity) or from humanist authority.',
    'If the boundary follows linguistic principles, the hybrid reading''s partition is structurally defensible. If it follows humanist authority (some features are accepted because the humanist preferred them, others rejected because they didn''t), the reading is more extractive than claimed — legitimate medieval forms are being retroactively declared wrong by a seat with power to do so.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(medieval_legitimacy_boundary, empirical, 'Whether medieval legitimacy is determined by linguistic principles or by humanist authority.').

omega_variable(
    textual_authority_source,
    'Why are the recovered Classical texts treated as authoritative? Is it because they preserve objective linguistic facts, or because they carry the prestige of Antiquity and humanist scholars'' exclusive access to them?',
    'Historical analysis of how the Classical archive was assembled, which texts were prioritized, which manuscript variants were chosen, and what alternatives were suppressed. Comparison with medieval manuscript traditions and modern linguistic analysis to determine whether Classical texts actually represent optimal linguistic evidence.',
    'If Classical texts are authoritative because they preserve genuine linguistic principles, the humanist correction is a neutral refinement. If they are authoritative because of prestige and humanist gatekeeping, the extraction is high — authority is conferred by access and interpretation, not by structural merit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_authority_source, conceptual, 'Whether Classical textual authority is objective or constructed via humanist scholarship.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.52) primarily structural (copyists and speakers have no formal appeal against humanist authority) or internalized (speakers believe they genuinely speak wrong Latin, independent of enforcement)?',
    'Post-humanist linguistic evidence: if suppression persists after humanist authority declines (early modern period onward), the suppression is substantially internalized. If it declines with humanist institutional authority, it is primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — targets carry the suppression with them even after institutional coercion is removed. If primarily structural, suppression could be rapidly reduced by removing the authority mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression of medieval forms is structural or internalized.').

omega_variable(
    coordination_vs_extraction_inseparability,
    'Could the coordination function (shared written standard) be achieved without the extraction mechanism (humanist authority concentrating interpretive power)? Is the standard structurally dependent on textual validation?',
    'Counterfactual analysis: construct a scenario where the Classical archive is publicly accessible and medieval forms are treated as valid variants rather than errors. Would the coordination benefit (mutual intelligibility) be lost?',
    'If the standard can be maintained without textual exclusivity, the extraction is separable and the constraint is more snare-like than tangled rope. If the standard requires the Classical archive as a reference point, the extraction is inseparable from the coordination and the tangled rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_inseparability, conceptual, 'Whether the coordination function requires humanist textual authority or is structurally independent.').

omega_variable(
    continuity_reading_foreclosure,
    'Does the hybrid reading logically foreclose the continuity reading, or do they coexist as live positions within the medieval framework?',
    'Examine medieval ecclesiastical authorities: do they explicitly reject the continuity reading in favor of the hybrid reading, or do they accept both? Do they distinguish medieval forms as legitimate variants or as errors?',
    'If the hybrid reading forecloses continuity within the medieval authority structure, the readings are in direct logical opposition. If both readings coexist (some authorities holding continuity, others accepting hybrid), the readings are structurally coexistent and the constraint describes a contested transition, not a logical displacement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(continuity_reading_foreclosure, empirical, 'Whether the hybrid reading logically forecloses the continuity reading or coexists with it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin__hybrid_reading, 1200, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t1200, correct_latin__hybrid_reading, theater_ratio, 1200, 0.18).
narrative_ontology:measurement(corr_tr_t1300, correct_latin__hybrid_reading, theater_ratio, 1300, 0.24).
narrative_ontology:measurement(corr_tr_t1400, correct_latin__hybrid_reading, theater_ratio, 1400, 0.35).
narrative_ontology:measurement(corr_tr_t1500, correct_latin__hybrid_reading, theater_ratio, 1500, 0.42).
narrative_ontology:measurement(corr_tr_t1550, correct_latin__hybrid_reading, theater_ratio, 1550, 0.41).
narrative_ontology:measurement(corr_tr_t1600, correct_latin__hybrid_reading, theater_ratio, 1600, 0.41).

% Extraction over time
narrative_ontology:measurement(corr_be_t1200, correct_latin__hybrid_reading, base_extractiveness, 1200, 0.32).
narrative_ontology:measurement(corr_be_t1300, correct_latin__hybrid_reading, base_extractiveness, 1300, 0.38).
narrative_ontology:measurement(corr_be_t1400, correct_latin__hybrid_reading, base_extractiveness, 1400, 0.45).
narrative_ontology:measurement(corr_be_t1500, correct_latin__hybrid_reading, base_extractiveness, 1500, 0.48).
narrative_ontology:measurement(corr_be_t1550, correct_latin__hybrid_reading, base_extractiveness, 1550, 0.48).
narrative_ontology:measurement(corr_be_t1600, correct_latin__hybrid_reading, base_extractiveness, 1600, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t1200, correct_latin__hybrid_reading, suppression_requirement, 1200, 0.28).
narrative_ontology:measurement(corr_su_t1300, correct_latin__hybrid_reading, suppression_requirement, 1300, 0.35).
narrative_ontology:measurement(corr_su_t1400, correct_latin__hybrid_reading, suppression_requirement, 1400, 0.44).
narrative_ontology:measurement(corr_su_t1500, correct_latin__hybrid_reading, suppression_requirement, 1500, 0.52).
narrative_ontology:measurement(corr_su_t1550, correct_latin__hybrid_reading, suppression_requirement, 1550, 0.52).
narrative_ontology:measurement(corr_su_t1600, correct_latin__hybrid_reading, suppression_requirement, 1600, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin__hybrid_reading, information_standard).
narrative_ontology:boltzmann_floor_override(correct_latin__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(correct_latin__hybrid_reading, correct_latin__continuity_reading).
narrative_ontology:affects_constraint(correct_latin__hybrid_reading, correct_latin__discontinuity_reading).

% DUAL FORMULATION NOTE:
% The correct_latin kernel decomposes into three constraint stories corresponding to three distinct readings of what counts as correct Latin. The hybrid reading partitions legitimacy between medieval transmission (valid) and medieval deviation (correctable), creating an intermediate position. The continuity reading privileges transmission as fully legitimate; the discontinuity reading privileges texts as the sole source of legitimacy. Each reading has its own epsilon (extractiveness of the standing arrangement), its own authority structure, and its own beneficiary/victim topology. The three readings are not three measurements of the same constraint — they are three different constraints arising from different framings of the same kernel. The readings coexist historically and logically: medieval authorities held some variant of continuity; humanist reformers advanced discontinuity; the hybrid reading emerged as a compromise position that most modern scholarship occupies. The network links track the logical influence among readings: discontinuity influenced the hybrid reading (which borrows textual authority from discontinuity); the hybrid reading influences continuity (which is now the abandoned pole, defended only in retrospect by modern scholars). Each reading's ε is distinct and reading-indexed: a continuity scholar assigns high legitimacy to medieval forms (low ε for extraction), a discontinuity scholar assigns low legitimacy (high ε), a hybrid scholar assigns conditional legitimacy (medium ε). The three files form a constraint family documenting how the kernel is read differently across interpretive traditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(correct_latin__hybrid_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
