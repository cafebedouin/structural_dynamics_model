% ============================================================================
% CONSTRAINT STORY: biblical_source_text__critical_reconstructive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_source_text__critical_reconstructive_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: biblical_source_text__critical_reconstructive_reading
 *   human_readable: Critical Reconstructive Reading of Biblical Source Text
 *   domain: religious/academic/textual
 *
 * SUMMARY:
 *   The critical reconstructive reading of biblical source text instantiates
 *   a commitment to historical recovery of hypothetical original forms as the
 *   primary epistemological priority. The reading asserts that neither the
 *   structure nor meaning of a biblical text can be definitively established
 *   until its textual basis is clarified — specifically, until the earliest
 *   recoverable form is reconstructed through manuscript genealogy, variant
 *   analysis, and historical-critical method. This reading competes within a
 *   contested kernel (the status and authority of biblical texts) with
 *   formal-equivalence and dynamic-equivalence readings, each prioritizing
 *   different values. The critical reconstructive reading destabilizes
 *   received textual authority (high extractiveness for confessional
 *   communities), concentrates interpretive power in academic institutions,
 *   and creates persistent friction between seminary educators (caught
 *   between two incompatible epistemologies) and non-specialist believers
 *   (excluded from the technical apparatus). The measurement series shows
 *   rising extractiveness and suppression intensity through the first forty
 *   time points (corresponding to the progressive professionalization of
 *   biblical studies from the 19th century onward), with a slight plateau and
 *   decline in the final period (reflecting recent counter-movements
 *   emphasizing meaning-centered or reception-historical approaches).
 *
 * KEY AGENTS:
 *   - Academic biblical scholars (institutional beneficiary, agenda-setter): control the textual-critical apparatus; defend the priority of source recovery; hold epistemic authority in universities and accredited seminaries.
 *   - Confessional faith communities (organized payer, identity-locked): whose received text is destabilized by the reconstructive reading; whose hermeneutical practice assumes textual stability; exit is constrained by identity fusion with tradition.
 *   - Textual critics and paleographers (institutional beneficiary, high mobility): specialists in manuscript genealogy; benefit from the perpetual expansion of technical complexity; have transferable skills.
 *   - Seminary educators (moderate payer, constrained exit): tasked with teaching both the critical apparatus and pastoral theology; occupy the institutional fault line where two incompatible text-epistemologies collide.
 *   - Translation committees (institutional observer, constrained): consume reconstructive output and propagate it into pastoral contexts via new translations.
 *   - Non-specialist believers (powerless, excluded, trapped): structurally absent from academic discourse; would reject the priority of 'original text' recovery as foreign to faith praxis; have no formal voice in the methodology that shapes the texts they receive.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__critical_reconstructive_reading, 0.68).
domain_priors:suppression_score(biblical_source_text__critical_reconstructive_reading, 0.52).
domain_priors:theater_ratio(biblical_source_text__critical_reconstructive_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__critical_reconstructive_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__critical_reconstructive_reading, "Critical Reconstructive Reading of Biblical Source Text").
narrative_ontology:topic_domain(biblical_source_text__critical_reconstructive_reading, "religious/academic/textual").

domain_priors:requires_active_enforcement(biblical_source_text__critical_reconstructive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__critical_reconstructive_reading, '6c1b2a45-cc8e-4efa-835d-061ae710be14').
narrative_ontology:cs_kernel_codification('6c1b2a45-cc8e-4efa-835d-061ae710be14', fixed_text).
narrative_ontology:cs_authority_grounding('6c1b2a45-cc8e-4efa-835d-061ae710be14', extraction).
narrative_ontology:cs_interpretation_layer_present('6c1b2a45-cc8e-4efa-835d-061ae710be14').
narrative_ontology:cs_reading_relation('6c1b2a45-cc8e-4efa-835d-061ae710be14', biblical_source_text__formal_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('6c1b2a45-cc8e-4efa-835d-061ae710be14', biblical_source_text__dynamic_equivalence_reading, influences).
narrative_ontology:cs_axiom('6c1b2a45-cc8e-4efa-835d-061ae710be14', foundational, historical_recovery_epistemically_primary).
narrative_ontology:cs_axiom_status(historical_recovery_epistemically_primary, holdable).
narrative_ontology:cs_axiom_grounding('6c1b2a45-cc8e-4efa-835d-061ae710be14', historical_recovery_epistemically_primary, empirically_contingent).
narrative_ontology:cs_axiom('6c1b2a45-cc8e-4efa-835d-061ae710be14', foundational, structure_meaning_contingent_on_textual_basis).
narrative_ontology:cs_axiom_status(structure_meaning_contingent_on_textual_basis, holdable).
narrative_ontology:cs_axiom_grounding('6c1b2a45-cc8e-4efa-835d-061ae710be14', structure_meaning_contingent_on_textual_basis, deontological).
narrative_ontology:cs_reference_frame('6c1b2a45-cc8e-4efa-835d-061ae710be14', scientific_textual_criticism).
narrative_ontology:cs_drift_state('6c1b2a45-cc8e-4efa-835d-061ae710be14', contemporary_postmodern_hermeneutics_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('6c1b2a45-cc8e-4efa-835d-061ae710be14', '').
narrative_ontology:cs_kernel_id(biblical_source_text__critical_reconstructive_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholars).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, confessional_faith_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, textual_critics_and_paleographers).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, seminary_educators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the methodological apparatus of textual criticism. Operate within universities, research centers, and internationally coordinated scholarly societies. Benefit from the authority granted to source-text recovery as the epistemically primary path to textual meaning. Their interpretive decisions about which manuscripts, readings, and reconstructions are 'most original' carry institutional weight and shape which texts enter seminaries, translations, and pastoral contexts. They can exit by shifting domains (studying classical texts, or adopting new methodologies) if the extractive character becomes undeniable.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholars, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholars, agenda_setter).

% Receive the destabilizing message that the text they hold as sacred is a late construction, divergent from hypothetical originals. Their hermeneutical practice rests on the assumption that the text is stable and faithfully transmitted. They cannot exit without exiting their faith identity itself; exit is identity-locked. They pay by absorbing cognitive dissonance, reworking theology to accommodate textual instability, or resisting the reading entirely (incurring social/intellectual isolation from credentialed educators).
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, confessional_faith_communities, payer,
    organized, civilizational, identity_locked, global).

% Specialists in manuscript genealogy and scribal transmission. Their expertise is indispensable to the critical reconstructive reading and generates publication, funding, and professional prestige. Every new manuscript discovery, every uncatalogued variant, every redactional layer expands their domain. They have exit options: their skills transfer to other text traditions (classical texts, Islamic manuscripts, literary transmission).
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, textual_critics_and_paleographers, beneficiary,
    institutional, biographical, mobile, global).

% Tasked with teaching future clergy using accredited curricula (which require critical-reconstructive foundations) while pastoring faith communities whose beliefs rest on received-text stability. They occupy an institutional fault line and pay by managing two incompatible epistemologies simultaneously. Their exit is constrained: they cannot ignore accreditation requirements without professional cost, and they cannot ignore pastoral credibility without failing their community role.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, seminary_educators, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__critical_reconstructive_reading, seminary_educators, observer).

% Produce translations (NIV, ESV, NRSV, etc.) that consume the output of source-text criticism (critical apparatus, reconstructed texts, attestation scores). They observe the constraint from the implementation vantage: they must translate a text that exists only as a scholarly hypothesis, often marked with brackets, probability scores, and confessional alternatives. Their translation choices propagate the critical reconstructive reading downstream into pastoral contexts where it encounters confessional believers directly.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, translation_committees, observer,
    institutional, biographical, constrained, global).

% Constitute the vast majority of faith practitioners whose hermeneutics rest on the assumption that the text they hold is the text their tradition received and preserved. They are structurally absent from academic biblical studies discourse and have no formal voice in the methodologies that shape the texts they receive. If present, they would argue that 'original text' recovery is intellectually baroque and spiritually irrelevant, that the received text carries authority through tradition, and that textual uncertainty is an artifact of academic methodology.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, non_specialist_believers, excluded,
    powerless, biographical, trapped, global).

% Museums, libraries, and archaeological authorities that hold manuscript collections and adjudicate access for scholarly analysis. They take an analytical stance on the constraint: their interests are preservation, provenance integrity, and evidence security. They enable the critical reconstructive reading by granting access to manuscripts, but do not have a stake in the interpretive conclusions scholars draw.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, conservation_authorities, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholars).
narrative_ontology:fixing_cost_class(biblical_source_text__critical_reconstructive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common methodological language and evidential apparatus for biblical studies across institutional and denominational boundaries. Enables scholars to discuss textual variants, manuscript genealogies, and reconstructive hypotheses using shared technical vocabulary and comparative evidence. Solves the coordination problem: how to adjudicate between competing textual readings without deferring to the authority of any single tradition or confession.
% TRANSFER_FUNCTION: Transfers epistemic authority for determining 'what the text says' from confessional communities (who maintain the text through tradition) to academic institutions (who claim the power to recover 'what the text originally said'). Moves scholarly prestige, research funding, and publication access to those who advance source-critical methodologies. Moves the burden of textual uncertainty and cognitive dissonance to seminary educators and confessional believers who must reconcile incompatible epistemologies.
% ABSENT_VOICES: Non-specialist believers, who would argue that the constraint privileges intellectual reconstruction over lived faith practice and treats textual uncertainty as an intellectual virtue rather than a practical problem. Confessional theologians working outside the critical paradigm, who would argue the reading is epistemologically self-defeating (it destabilizes the text it purports to ground interpretation in). Oral-tradition-keepers and performers, who would argue the reading treats texts as static objects rather than as live practices, where variation and adaptation are features, not corruption.
% DISAPPEARANCE_RATIONALE: Academic institutions and scholarly societies would argue the world would rearrange significantly: theological education would lose a common philological language, denominational silos would harden, and archaeological textual evidence would lack an interpretive framework. Confessional communities would argue the world would remain substantially unchanged, or actually improve: liturgical and pastoral practice would continue without the anxiety of textual uncertainty, faith communities would regain confidence in received texts, and seminary education would integrate more coherently with pastoral reality. The dispute is over what constitutes rearrangement: loss of institutional coordination (academic perspective) or loss of institutional extraction (confessional perspective).
% FOUNDING_PROBLEM: Textual variants and manuscript divergence are real: by the 2nd-4th centuries, multiple versions of biblical texts circulated; scribes altered, harmonized, and recopied texts over centuries; and the question arises: which text should be the basis for interpretation and theological authority? The critical reconstructive reading answers: the earliest recoverable form, established through manuscript genealogy and historical-critical analysis.
% FOUNDING_PROBLEM_CORROBORATION: Textual critics and paleographers attest the founding problem is live: manuscript evidence exists, variant genealogies are recoverable with reasonable confidence, and earlier texts are discoverable through analysis. Confessional theologians attest the problem is misconceived: textual variation is not an obstacle to authority but a feature of living transmission; treating the 'original' as the goal of interpretation mistakes the nature of scripture as a communal practice. Non-specialist believers attest the problem is invented: the received text has functioned for fifteen centuries without historical-critical resolution; the problem appears only from within a scholarly framework that privileges original recovery as a condition for meaning.
narrative_ontology:disappearance_verdict(biblical_source_text__critical_reconstructive_reading, contested).
narrative_ontology:founding_problem_status(biblical_source_text__critical_reconstructive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__critical_reconstructive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_source_text__critical_reconstructive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_source_text__critical_reconstructive_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_source_text__critical_reconstructive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_source_text__critical_reconstructive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_source_text__critical_reconstructive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the critical reconstructive reading concentrates interpretive authority in academic institutions and destabilizes the textual foundation confessional communities built their hermeneutics on. The reading does not merely offer an alternative; it privileges source recovery as the condition for meaning-making itself, making the reconstructive method non-negotiable for any interpreter claiming scholarly credibility. Suppression measures the active enforcement required to maintain this priority: academic gatekeeping (peer review, accreditation standards), institutional credential requirements, the control of publishing venues, and the marginalization of pre-critical and non-academic hermeneutical methods. The measured rise in suppression (0.28 to 0.54 across the interval) reflects the professionalization of biblical studies — increasingly rigorous enforcement of the critical apparatus as a requirement for legitimate participation. Theater ratio is lower (0.28 at interval end) because the reconstructive function is substantive: the apparatus genuinely produces new textual knowledge, manuscript discoveries do occur, and genealogies are built on real evidence — but the rise in theater (0.12 to 0.29) reflects the growing performative character of methodological complexity, where technical virtuosity in apparatus maintenance becomes decoupled from interpretive payoff. The plateau and slight decline in the final period corresponds to critiques of source-critical ossification and the rise of meaning-centered approaches (reader-response, reception history, performance criticism) that relativize the priority of source recovery. Accessibility collapse (0.71) reflects that alternatives to the critical reading are technically available but professionally costly: a scholar can adopt pre-critical or post-critical methods but does so at the cost of institutional standing and publication access. Resistance (0.74) is substantial because confessional hermeneutics traditions continue to resist the destabilization of received text; evangelical seminaries and conservative faith communities actively contest the reading; and emerging methodological movements challenge source-recovery priority. The asymmetry between extractiveness and suppression reflects that the reading's power operates partly through intellectual authority (the reading is genuinely persuasive within academic circles) and partly through institutional enforcement (the reading is required for credentialing, regardless of persuasiveness).
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (academic scholars) and the payer seat (confessional communities) should compute radically different types. From the beneficiary perspective, the constraint is genuine coordination: establishing a shared methodological language across denominational boundaries, enabling collaborative knowledge-building, and solving the real problem of how to adjudicate between textual variants. From the payer perspective, the constraint is extraction: the reading destabilizes received authority without offering viable alternatives for faith praxis, requires cognitive labor (holding two incompatible epistemologies), and forecloses traditional hermeneutical approaches by treating them as methodologically illegitimate. The engine should compute Rope (coordination-focused) from the beneficiary seat and Tangled Rope or Snare (extraction with coordination cover) from the payer seat. The engine computes this divergence from the structural data: high extractiveness, active suppression (required to exclude alternative methods), identity-locked exit for payers (they cannot leave confessional identity), and arbitrage exit for beneficiaries (scholars can shift domains). This divergence is not an error — it is the measurement the framework exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Academic scholars are low-d beneficiaries (d near 0.0-0.2): the constraint subsidizes their interpretive authority, creates research opportunities, and secures institutional resources. Confessional communities are high-d targets (d near 0.8-0.95): the constraint extracts textual authority, demands cognitive labor to integrate incompatible epistemologies, and forecloses hermeneutical alternatives. Seminary educators are near-symmetric (d ≈ 0.45-0.55): they benefit from credibility in both academic and pastoral contexts but pay by enduring structural cognitive dissonance. Textual critics are high-beneficiary (d ≈ 0.15): their professional identity and publication record depend on the constraint's perpetual unfolding of textual complexity. Non-specialist believers sit off-chart: they are excluded from the constraint's structural logic, though they pay its costs indirectly (confusion between what they are taught in seminary and what they practice in faith). The directionality asymmetry is driven by exit options: beneficiaries have arbitrage-grade mobility (scholars can shift to other methodologies or manuscript traditions); payers have identity-locked constraint (confessional believers cannot exit the tradition without exiting the identity that constitutes them as members of that community).
 *
 * MANDATROPHY ANALYSIS:
 *   The critical reconstructive reading is NOT mandatrophic in the sense of a dead founding problem. The founding problem is contested, not dead: textual variants do exist, manuscript genealogies are real, and the question of which text to use for interpretation is live. However, the reading shows signs of function-displacement: the original coordinating function (enabling scholars to communicate across denominational lines about textual evidence) persists, but an increasing share of institutional energy goes to performance and gatekeeping rather than to new textual discoveries. The theater ratio rises from 0.12 to 0.29, suggesting that maintaining the reading's authority increasingly requires performative complexity (elaborate apparatus, technical virtuosity) rather than new textual findings. This is consistent with Piton dynamics but does not reach full Piton classification because the coordination function remains substantial and the extractive power is actively defended (high suppression), not merely inert. The reading remains Tangled Rope: genuine coordination (scholars need a shared methodological language) coupled with asymmetric extraction (the reading privileges academic authority and destabilizes confessional textuality). The rise in theater and the contestation of foundational status by counter-methodologies (reader-response, performance criticism) are signals to monitor for future Piton migration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_text_recoverability,
    'Can a determinate ''original text'' be recovered with sufficient confidence to constitute a binding textual basis for interpretation, or is the ''original'' itself a scholarly construct that cannot be recovered beyond a certain level of probability?',
    'Continued manuscript discovery and genetic analysis; also philosophical investigation of whether the concept ''original text'' is coherent for texts that were transmitted orally, redacted over centuries, and copied by scribes with hermeneutical intentionality.',
    'If originals cannot be recovered with high confidence, the priority claim of source-text recovery becomes questionable; the reading''s foundational axiom (that textual basis precedes meaning) loses force. If originals can be recovered reliably, the reading''s authority is substantially vindicated. This is the core contestation between critical-reconstructive and formal-equivalence readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(original_text_recoverability, conceptual, 'Whether the ''original text'' is an achievable target or a regulative fiction.').

omega_variable(
    extraction_vs_coordination_boundary,
    'Is the high extractiveness from confessional communities a necessary feature of the reading''s truth-seeking function, or an artifact of institutional power dynamics that could be separated from the methodological apparatus?',
    'Institutional redesign: can universities develop hermeneutical frameworks that preserve source-critical knowledge while according equal legitimacy to confessional and traditional readings? Can seminary educators teach critical reconstruction without destabilizing students'' received textual authority? Historical case studies: have non-academic communities successfully integrated source-critical findings while maintaining confessional hermeneutics?',
    'If the extraction is necessary to the methodology, the reading remains Tangled Rope. If extraction is institutional (contingent on power asymmetries) rather than methodological, the constraint could be redesigned as pure Rope through collaborative hermeneutics and power-sharing. This question drives the academic interest in ''including the voices'' of confessional scholars and indigenous interpreters.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, conceptual, 'Whether the constraint''s extractive character flows from its epistemology or from institutional gatekeeping.').

omega_variable(
    suppression_mechanism_internalized,
    'How much of the suppression of alternative readings (pre-critical, confessional, performance-centered) is structural (institutional barriers to publication, credentialing, funding) versus internalized (scholars have internalized the belief that non-critical methods are intellectually illegitimate)?',
    'Post-institutional analysis: if scholars taught outside accredited frameworks retained their confidence in alternative methods, or if accreditation requirements were relaxed, would suppression drop? Do students who leave academic biblical studies report that their internalized conviction of critical superiority persists or fades?',
    'Structural suppression can be remedied by institutional change; internalized suppression persists after the external barrier is removed and indicates deeper identity-fusion (the scholar''s self-concept has become bound to critical methodology). High internalization would suggest the reading has become identity-locked for its beneficiaries, shifting the constraint toward Piton (inertial, maintained by identity performance rather than active enforcement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Proportion of suppression that is external (institutional) versus internalized (cognitive/identity).').

omega_variable(
    sibling_reading_foreclosure,
    'Does the critical reconstructive reading logically foreclose the formal-equivalence reading (structure is primary), or do the two coexist as live alternatives held by different parties and frameworks?',
    'Philosophical analysis of whether prioritizing source recovery and prioritizing source structure are logically incompatible or merely differently weighted commitments. Do scholars working in the critical-reconstructive reading use formal-equivalence insights, or reject them as incoherent?',
    'If foreclosed, the relationship should be updated to forecloses; if coexisting, the reading_relations entry remains coexists_with. This affects how the engine models the kernel contest — whether it is a true disagreement (multiple parties hold incompatible premises) or a structured alternative (both operate coherently but with different values).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether formal-equivalence and critical-reconstructive readings are logically incompatible or live alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__critical_reconstructive_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_source_text__critical_reconstructive_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(bibl_tr_t8, biblical_source_text__critical_reconstructive_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(bibl_tr_t16, biblical_source_text__critical_reconstructive_reading, theater_ratio, 16, 0.17).
narrative_ontology:measurement(bibl_tr_t24, biblical_source_text__critical_reconstructive_reading, theater_ratio, 24, 0.21).
narrative_ontology:measurement(bibl_tr_t32, biblical_source_text__critical_reconstructive_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(bibl_tr_t40, biblical_source_text__critical_reconstructive_reading, theater_ratio, 40, 0.29).
narrative_ontology:measurement(bibl_tr_t50, biblical_source_text__critical_reconstructive_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(bibl_be_t8, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(bibl_be_t16, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(bibl_be_t24, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(bibl_be_t32, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement(bibl_be_t40, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 40, 0.71).
narrative_ontology:measurement(bibl_be_t50, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(bibl_su_t8, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 8, 0.32).
narrative_ontology:measurement(bibl_su_t16, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 16, 0.38).
narrative_ontology:measurement(bibl_su_t24, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 24, 0.44).
narrative_ontology:measurement(bibl_su_t32, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 32, 0.5).
narrative_ontology:measurement(bibl_su_t40, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 40, 0.54).
narrative_ontology:measurement(bibl_su_t50, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__critical_reconstructive_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(biblical_source_text__critical_reconstructive_reading, 0.18).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, biblical_source_text__formal_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, biblical_source_text__dynamic_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, modern_bible_translation_authority).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, seminary_hermeneutics_curricular_tension).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'biblical_source_text'. The critical_reconstructive_reading prioritizes historical recovery of hypothetical originals; sibling readings prioritize source-language structure (formal_equivalence) and communicative effectiveness (dynamic_equivalence). Each reading instantiates a distinct constraint with its own beneficiary/victim structure and computed type. The network edges show structural influence: critical-reconstructive reading destabilizes formal-equivalence's text-authority assumption; both influence translation-authority constraints downstream. The kernel contest is unresolved; no reading forecloses the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
