% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__isaac_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__isaac_covenant_reading, []).

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
 *   constraint_id: abrahamic_covenant__isaac_covenant_reading
 *   human_readable: Covenant Transmitted Exclusively Through Isaac (Genesis 17:19-21 Reading)
 *   domain: religious_studies/comparative_theology/institutional_authority
 *
 * SUMMARY:
 *   Genesis 17:19-21 narrates God's covenant promise to Abraham, specifying
 *   that the covenant will be established with Isaac: 'But My covenant I will
 *   establish with Isaac, whom Sarah shall bear to you' (17:21). Jewish
 *   rabbinic tradition interprets this passage as limiting the covenant to
 *   Isaac's line and explicitly excluding Ishmael, despite Abraham's prior
 *   plea on Ishmael's behalf (17:18) and despite 17:20 promising that Ishmael
 *   will become 'a great nation.' This constraint story instantiates the
 *   ISAAC-EXCLUSIVE READING of the Abrahamic covenant kernel. It is ONE of
 *   three competing readings: the ishmael_covenant_reading argues that
 *   Genesis promises are inclusive and that Islamic prophetic succession
 *   validates Ishmael's covenantal inheritance; the land_promise_constraint
 *   focuses on the territorial element of the covenant and how competing
 *   readings map onto modern territorial disputes. This story focuses
 *   exclusively on the transmission-lineage question and declares Isaac the
 *   sole legitimate heir. The constraint is presented here as claimed
 *   tangled_rope (genuine coordination function for Jewish institutional
 *   continuity + asymmetric extraction from Ishmael-traced lineages + active
 *   enforcement through interpretive authority), though an alternative
 *   analysis might emphasize the extractive and suppressive character more
 *   strongly, pushing toward snare classification.
 *
 * KEY AGENTS:
 *   - Jewish institutional authority: sets the interpretive frame, declares Isaac exclusivity, controls the textual apparatus
 *   - Jewish faith community: benefits from covenant election claim and identity continuity
 *   - Ishmael lineage claimants: bear the cost of explicit exclusion from the covenantal narrative
 *   - Islamic prophetic tradition: excluded from the interpretive conversation while contesting the reading's validity
 *   - Rabbinic interpretive community: enforces and transmits the exclusivity through exegetical apparatus
 *   - Modern biblical scholars: provide external (often skeptical) corroboration/refutation of the reading's textual claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__isaac_covenant_reading, 0.82).
domain_priors:suppression_score(abrahamic_covenant__isaac_covenant_reading, 0.71).
domain_priors:theater_ratio(abrahamic_covenant__isaac_covenant_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__isaac_covenant_reading, tangled_rope).
narrative_ontology:human_readable(abrahamic_covenant__isaac_covenant_reading, "Covenant Transmitted Exclusively Through Isaac (Genesis 17:19-21 Reading)").
narrative_ontology:topic_domain(abrahamic_covenant__isaac_covenant_reading, "religious_studies/comparative_theology/institutional_authority").

domain_priors:requires_active_enforcement(abrahamic_covenant__isaac_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__isaac_covenant_reading, '743fa71e-520f-4703-8792-b63808d62b59').
narrative_ontology:cs_kernel_codification('743fa71e-520f-4703-8792-b63808d62b59', fixed_text).
narrative_ontology:cs_authority_grounding('743fa71e-520f-4703-8792-b63808d62b59', extraction).
narrative_ontology:cs_interpretation_layer_present('743fa71e-520f-4703-8792-b63808d62b59').
narrative_ontology:cs_reading_relation('743fa71e-520f-4703-8792-b63808d62b59', abrahamic_covenant__ishmael_covenant_reading, forecloses).
narrative_ontology:cs_reading_relation('743fa71e-520f-4703-8792-b63808d62b59', abrahamic_covenant__land_promise_constraint, influences).
narrative_ontology:cs_axiom('743fa71e-520f-4703-8792-b63808d62b59', foundational, isaac_exclusive_covenant_heir).
narrative_ontology:cs_axiom_status(isaac_exclusive_covenant_heir, holdable).
narrative_ontology:cs_axiom_grounding('743fa71e-520f-4703-8792-b63808d62b59', isaac_exclusive_covenant_heir, empirically_contingent).
narrative_ontology:cs_axiom('743fa71e-520f-4703-8792-b63808d62b59', secondary, textual_univocity_of_exclusion).
narrative_ontology:cs_axiom_status(textual_univocity_of_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('743fa71e-520f-4703-8792-b63808d62b59', textual_univocity_of_exclusion, empirically_contingent).
narrative_ontology:cs_reference_frame('743fa71e-520f-4703-8792-b63808d62b59', isaac_exclusive_covenant_transmission).
narrative_ontology:cs_drift_state('743fa71e-520f-4703-8792-b63808d62b59', contemporary_scholarship_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('743fa71e-520f-4703-8792-b63808d62b59', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, jewish_institutional_continuity).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, rabbinic_interpretive_authority).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, ishmael_lineage_claimants).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, islamic_prophetic_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, jewish_faith_community).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, rabbinic_interpretive_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and transmits the Abrahamic covenant through rabbinical hermeneutics, declaring Isaac the sole legitimate heir and Ishmael explicitly excluded. Sets the boundary of legitimate covenant membership and inheritance of promises. Their institutional legitimacy depends on this reading's continuity and authority over textual interpretation.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, jewish_institutional_authority, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Receives identity, election claim, and covenantal status as the chosen people inheriting Abraham's promises through Isaac. Their collective self-understanding and religious legitimacy are constituted through this reading. The constraint grants them a foundational narrative of unique relationship to God and historical purpose.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, jewish_faith_community, beneficiary,
    organized, civilizational, identity_locked, global).

% Bear the cost of explicit textual exclusion from Abraham's covenant and its attendant promises of election, land, and progeny. Are narratively positioned as outside the sacred lineage despite genealogical descent from Abraham. Have no institutional voice in the interpretation machinery that declares their exclusion, and their historical claims to covenantal inheritance are structurally delegitimized by this reading.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, ishmael_lineage_claimants, payer,
    powerless, civilizational, identity_locked, global).

% Excluded from the covenant-transmission narrative despite Islamic theology's own claims of continuity with Abrahamic monotheism and assertion of Muhammad as the final prophet in Abraham's lineage (often tracing descent through Ishmael). Must contest the Isaac-exclusive reading while operating outside the rabbinic interpretive framework that established it. Their alternative reading of the kernel is delegitimized within Jewish institutional discourse.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, islamic_prophetic_tradition, payer,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__isaac_covenant_reading, islamic_prophetic_tradition, excluded).

% Develops and maintains the hermeneutical apparatus (Talmud, midrash, medieval commentaries, modern responsa) that sustains this reading across centuries. Their exegetical authority is grounded in the claim that this reading correctly interprets Genesis 17:19-21 as mandating Isaac-exclusive covenant transmission. They collect institutional power and interpretive legitimacy from maintaining this boundary.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, rabbinic_interpretive_community, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__isaac_covenant_reading, rabbinic_interpretive_community, beneficiary).

% Examine the textual history, source criticism, and comparative ancient Near Eastern context. Many argue that Genesis 17:19-21 reflects post-exilic Priestly redaction serving institutional Jewish boundary maintenance rather than univocal ancient covenant intent. Provide external (often skeptical) corroboration or refutation of the reading's textual claims to univocal mandate.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, modern_biblical_scholars, observer,
    institutional, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(abrahamic_covenant__isaac_covenant_reading, jewish_institutional_authority).
narrative_ontology:fixing_cost_class(abrahamic_covenant__isaac_covenant_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified narrative line of covenant transmission: a single, unambiguous lineage of promise-bearers from Abraham through Isaac to Jacob to the Jewish people. This solves the coordination problem of maintaining collective identity and covenantal status across generations despite historical fragmentation and diaspora.
% TRANSFER_FUNCTION: Transfers exclusive claim to Abrahamic election and covenantal promise from all potential Abraham descendants to Isaac's lineage alone. Moves interpretive authority from open textual exegesis to rabbinic institutional control. Transfers religious legitimacy and historical vindication to the Jewish institutional tradition and denies it to Ishmael-traced lineages (Arab peoples, later Islamic tradition).
% ABSENT_VOICES: Ishmael's own lineage (historical Arab claimants) have no voice in the interpretive machinery that defines them as excluded. The Islamic tradition, which later claims Abraham and Ishmael as foundational to its own narrative, is structurally absent from the rabbinical interpretive conversation where this covenant boundary was fixed. Modern biblical scholars who argue the exclusivity was a post-exilic editorial innovation are marginal to this reading's authority structure.
% DISAPPEARANCE_RATIONALE: If this reading (Isaac-exclusive covenant) disappeared and an inclusive or Ishmael-inclusive reading replaced it, Jewish institutional identity would require reformulation — the narrative of unique election and covenantal status would be radically altered. Islamic theology would gain compatibility with Abrahamic continuity claims. However, the question is contested: those who hold this reading as textually mandated argue the text itself (Genesis 17:19-21) would reassert the exclusivity; those who see it as editorial construction argue a competing reading would leave the underlying covenant narrative largely intact but reinterpreted.
% FOUNDING_PROBLEM: Abraham's covenant promise must be inherited by a specific lineage to remain a binding, particularistic commitment. The problem is textual and theological: Genesis depicts both Isaac and Ishmael as Abraham's sons; without a mechanism to privilege one over the other, the covenant's status as a specific election to one people becomes ambiguous. The founding problem is the need to secure exclusive transmission through Isaac against the apparent textual inclusion of Ishmael.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic tradition and institutional Jewish theology attest the problem is still live: the exclusion of Ishmael is presented as textually necessary and foundational to Jewish identity. Modern biblical scholars attest the problem is an artifact of later editorial concerns: Genesis 17:19-21 is seen as a redactional addition (Priestly source, post-exilic period) that imposed exclusivity retrospectively. Islamic tradition attests that the problem as framed (the need to exclude Ishmael) is itself a misreading: Abraham's covenant includes both sons, and Islamic prophetology demonstrates Ishmael's covenantal inheritance. No independent party external to all three traditions can corroborate the founding problem's existence as objective (all attestations are from parties to the dispute).
narrative_ontology:disappearance_verdict(abrahamic_covenant__isaac_covenant_reading, contested).
narrative_ontology:founding_problem_status(abrahamic_covenant__isaac_covenant_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__isaac_covenant_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(abrahamic_covenant__isaac_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__isaac_covenant_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__isaac_covenant_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(abrahamic_covenant__isaac_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(abrahamic_covenant__isaac_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because this reading assigns exclusive covenantal status to one lineage and explicitly denies it to another, creating a structured hierarchy of religious legitimacy with material consequences (historical validation, institutional authority, narrative inheritance). The constraint persists through active enforcement (rabbinic interpretive authority must continually defend this reading against alternative readings and against the apparent textual inclusion of Ishmael). Suppression (0.71) is substantial because: (1) alternative readings are pushed to the margins of Jewish institutional authority; (2) Jews who adopt inclusive readings may experience identity friction or institutional alienation; (3) Islamic and Ishmaelite claims are structurally excluded from the interpretive machinery that declared them invalid. Theater ratio (0.42) is moderate: rabbinic exegesis provides genuine theological argumentation (not purely theatrical), but an increasing share of the constraint's maintenance involves defending against modern scholarship that questions whether Genesis 17:19-21 is univocally exclusive or is a later redactional construction. The measurement series show extraction and suppression rising over the interval (0-25), consistent with increasing pressure from competing readings and modern scholarship that necessitates more elaborate defense of the exclusivity claim — what appears as a permanent textual truth requires increasingly intense institutional reinforcement. Accessibility_collapse (0.78) is high because once a Jewish reader has internalized this reading as foundational to Jewish identity, alternatives feel not merely wrong but threatening to identity itself — the psychological barrier to alternative readings is high. Resistance (0.64) is substantial because Islamic theology and modern scholars actively contest the reading, and Ishmael-traced historical lineages have their own counter-narratives.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (Jewish institutional authority) and the beneficiary (Jewish faith community) should compute as coordinated or low-extraction, seeing the constraint as genuine theological truth that explains Jewish continuity and election. The payer (Ishmael lineage claimants) and the excluded (Islamic tradition) should compute as high-extraction targets, experiencing the constraint as an unjust boundary that denies them religious and historical legitimacy. The engine computes per-seat classifications from power + exit + directionality; the interpretive authority seat (powerful, identity-locked to the framework) should see this as rope or coordination; the excluded seat (powerless or organized, identity-locked outside) should see this as snare. The divergence itself is the measurement: a constraint that looks like necessary theological truth from inside the beneficiary seat looks like enforced exclusion from the victim seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish institutional authority: d near 0.0 (full beneficiary) — they control the interpretive apparatus, benefit from maintaining covenant exclusivity, have mobile exit options (could adopt other readings but choose not to) but are identity-locked to preserving Jewish continuity. Jewish faith community: d near 0.2-0.3 (minor beneficiary with contingent cost) — they benefit from the identity and election claim the covenant confers, but bear indirect suppression cost through identity-fusion that makes alternative readings feel unthinkable. Ishmael lineage claimants: d near 0.95 (near-full target) — they are textually excluded, bear the cost of denied legitimacy, are powerless to change the interpretive frame, and are identity-locked to their own genealogical claims (cannot exit into Isaac's lineage). Islamic tradition: d near 0.85 (substantial target) — they are excluded from the interpretive machinery that declared them invalid, bear the cost of religious delegitimization, have institutional power but are trapped outside the rabbinic framework (identity-locked to Islamic theology, which cannot be reconciled with Jewish exclusivity without fundamental reformulation).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (securing exclusive transmission of the covenant through Isaac) is declared by rabbinic tradition as LIVE and NECESSARY. However, modern biblical scholarship attests the founding problem is actually DEAD or CONSTRUCTED: Genesis 17:19-21 may be post-exilic redaction, the textual justification for exclusivity may be editorial rather than original, and the problem it purports to solve (ambiguity about covenant inheritance) could be solved under alternative readings. If the founding problem is actually dead (the textual urgency for Isaac-exclusive transmission is an artifact of redaction), then the constraint persists through institutional inertia and identity-fusion rather than functional necessity. This is a mandatrophy signature: the arrangement persists because it is woven into Jewish identity and institutional authority, but the functional reason it was erected (securing unambiguous covenant transmission) has atrophied or been revealed as spurious. The theater ratio increasing from 0.28 to 0.42 over the interval supports this reading: as scholarship accumulates questioning the textual basis, more of the institutional activity becomes defensive posturing and identity-maintenance (theater) rather than fresh theological exposition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_univocity_ambiguity,
    'Does Genesis 17:19-21 univocally mandate Isaac-exclusive covenant transmission, or is the exclusivity a post-exilic redactional imposition reflecting institutional concerns rather than original authorial intent?',
    'Source-critical analysis comparing Genesis 17:19-21 with parallel covenant passages (Genesis 12, 15, 21) and with Priestly redactional markers; comparison with earlier textual strata and adjacent ancient Near Eastern covenant formulas.',
    'If univocal mandate: the reading''s claim to textual fidelity is upheld and the exclusion appears structurally necessary. If redactional imposition: the reading is revealed as an interpretive construction serving institutional interests rather than a discovery of textual meaning — the constraint becomes a snare rather than a tangled_rope, and extraction rises further.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_univocity_ambiguity, empirical, 'Whether the Isaac-exclusive reading is textually mandated or editorially constructed.').

omega_variable(
    identity_fusion_foreclosure,
    'Is the Isaac-exclusive reading essential to Jewish identity, or could Jewish institutional and religious continuity be maintained under an inclusive Abrahamic-lineage reading?',
    'Theological and phenomenological investigation: do Jewish communities that engage with more inclusive covenant readings experience identity coherence or fragmentation? Can Jewish chosenness theology survive without Ishmael''s exclusion?',
    'If identity-essential: the suppression and accessibility_collapse remain high because exit from this reading is experienced as apostasy. If contingent: the high suppression reveals internalized identity-lock rather than structural necessity — the constraint''s persistence mechanism is more extractive (identity capture) than coordinating.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_foreclosure, conceptual, 'Whether the Isaac-exclusive reading is identity-constitutive for Jewish continuity or contingently adopted.').

omega_variable(
    kernel_framing_choice,
    'This constraint is a READING of the Abrahamic covenant kernel. Alternative readings (ishmael_covenant_reading, land_promise_constraint) instantiate the same kernel differently. Which reading captures the ''true'' covenant intent: the exclusive Isaac reading, the inclusive Ishmael reading, or a land-centered reading? Or is the kernel itself underdetermined, permitting multiple equally valid framings?',
    'Comparative textual analysis across all sibling readings; examination of the kernel''s internal coherence under each reading; investigation of whether the kernel codification (formalized text + distributed authority across Jewish, Christian, Islamic traditions) creates structural space for coexisting readings rather than foreclosure.',
    'If the kernel permits multiple coexisting readings: this constraint''s exclusivity claim is a reading-internal choice, not a textual discovery — the exclusion of Ishmaelite claimants is a policy of this reading, not an inevitable inference. The constraint''s structure (beneficiary to Jewish institutional authority, victims to Ishmael-traced lineages) becomes transparently reading-indexed rather than cosmically mandated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'The kernel underdetermination at the heart of the Abrahamic covenant contest — whether the covenant text determines a single reading or permits irreducible reading pluralism.').

omega_variable(
    institutional_suppression_mechanism,
    'Is the measured suppression (0.71) structural (the alternative reading is genuinely hard to hold given textual evidence) or internalized (Jewish readers trained from childhood in this reading experience it as natural, not enforced, even when the alternative reading is logically available)?',
    'Phenomenological investigation of Jewish readers encountering the ishmael_covenant_reading for the first time: does suppression persist after exposure and explanation, or does it dissolve? Historical investigation of Jewish institutional responses to rival readings (Karaite challenges, Islamic claims): was the response interpretive argumentation or institutional suppression (excommunication, textual censorship)?',
    'If structural: the suppression metric stands as written. If internalized: the constraint''s effective suppression is higher than the 0.71 scalar suggests — the reading is carried in identity-fusion that outlasts argument or institutional removal. This shifts the constraint''s character toward greater extractiveness and makes remediation harder (cannot be solved by institutional reform alone).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_suppression_mechanism, empirical, 'Whether the constraint''s suppression is externally enforced or internalized in identity-formation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__isaac_covenant_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abra_tr_t0, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(abra_tr_t5, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(abra_tr_t10, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(abra_tr_t15, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(abra_tr_t20, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(abra_tr_t25, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(abra_be_t0, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(abra_be_t5, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 5, 0.71).
narrative_ontology:measurement(abra_be_t10, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 10, 0.76).
narrative_ontology:measurement(abra_be_t15, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 15, 0.8).
narrative_ontology:measurement(abra_be_t20, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 20, 0.81).
narrative_ontology:measurement(abra_be_t25, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 25, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(abra_su_t0, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(abra_su_t5, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(abra_su_t10, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(abra_su_t15, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(abra_su_t20, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(abra_su_t25, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__isaac_covenant_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(abrahamic_covenant__isaac_covenant_reading, 0.12).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant__ishmael_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant__land_promise_constraint).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, jewish_institutional_continuity).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, islamic_prophetic_lineage_claim).

% DUAL FORMULATION NOTE:
% The Abrahamic covenant kernel decomposes into THREE constraint stories: isaac_covenant_reading (this story, exclusive transmission through Isaac), ishmael_covenant_reading (inclusive transmission through both sons, validated by Islamic prophetology), and land_promise_constraint (territorial element, modern territorial implications). These are not different measurements of one constraint — they are three structurally distinct claims with different epsilon values, different beneficiary/victim structures, and different institutional framings. The kernel underdetermination (one textual corpus, three competing readings) is resolved by treating each reading as instantiating a separate constraint. Siblings are linked via network.affects_constraints and documented via omega variables addressing framing choice and reading-internal axiom commitments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(abrahamic_covenant__isaac_covenant_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
