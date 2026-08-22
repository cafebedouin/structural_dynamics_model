% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__ishmael_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__ishmael_covenant_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: abrahamic_covenant__ishmael_covenant_reading
 *   human_readable: Ishmael Covenant Reading: Inclusive Abrahamic Lineage
 *   domain: religious/institutional/textual authority
 *
 * SUMMARY:
 *   The ishmael_covenant_reading is one reading of the abrahamic_covenant
 *   kernel—a contested interpretation of Genesis and its relationship to
 *   Quranic and Islamic theology. The reading asserts that Abraham's covenant
 *   is NOT restricted to Isaac's line but continues through Ishmael to
 *   Muhammad and the Islamic prophetic tradition. This reading challenges the
 *   Jewish-exclusivity interpretation (isaac_covenant_reading) and operates
 *   as a third position in the classical Christian-Jewish supersessionism
 *   debate. The reading is institutionally embedded in Islamic theology,
 *   Quranic exegesis, and comparative theology. It has both a genuine
 *   coordination function—establishing textual continuity across Abrahamic
 *   traditions—and extractive elements: by redefining Jewish covenant claims
 *   as incomplete or secondary, it retroactively shifts epistemic authority.
 *   The constraint is CLAIMED as tangled_rope (genuine coordination function
 *   plus asymmetric extraction and active enforcement to maintain exclusivity
 *   of the reading's legitimacy) while the authored metrics describe moderate
 *   extraction with substantial suppression, reflecting the active
 *   institutional labor required to sustain and enforce this reading against
 *   competing exegetical claims.
 *
 * KEY AGENTS:
 *   - Islamic communities: interpret covenant as inclusive, identity-locked beneficiary
 *   - Quranic exegetes: agenda-setters who maintain textual foundation and exegetical authority
 *   - Jewish exclusivity claims: institutional payer, identity-locked, directly challenged
 *   - Jewish legal tradition: payer, built on exclusivity, loses covenantal uniqueness under this reading
 *   - Supersessionist Christian readings: partially excluded third party, now one option among three
 *   - Comparative theology scholarship: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__ishmael_covenant_reading, 0.58).
domain_priors:suppression_score(abrahamic_covenant__ishmael_covenant_reading, 0.62).
domain_priors:theater_ratio(abrahamic_covenant__ishmael_covenant_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__ishmael_covenant_reading, tangled_rope).
narrative_ontology:human_readable(abrahamic_covenant__ishmael_covenant_reading, "Ishmael Covenant Reading: Inclusive Abrahamic Lineage").
narrative_ontology:topic_domain(abrahamic_covenant__ishmael_covenant_reading, "religious/institutional/textual authority").

domain_priors:requires_active_enforcement(abrahamic_covenant__ishmael_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__ishmael_covenant_reading, 'afaca967-6e1a-4e77-a93f-3f9d59942052').
narrative_ontology:cs_kernel_codification('afaca967-6e1a-4e77-a93f-3f9d59942052', fixed_text).
narrative_ontology:cs_authority_grounding('afaca967-6e1a-4e77-a93f-3f9d59942052', lineage).
narrative_ontology:cs_interpretation_layer_present('afaca967-6e1a-4e77-a93f-3f9d59942052').
narrative_ontology:cs_reading_relation('afaca967-6e1a-4e77-a93f-3f9d59942052', abrahamic_covenant__isaac_covenant_reading, forecloses).
narrative_ontology:cs_reading_relation('afaca967-6e1a-4e77-a93f-3f9d59942052', abrahamic_covenant__christian_supersessionist_reading, coexists_with).
narrative_ontology:cs_axiom('afaca967-6e1a-4e77-a93f-3f9d59942052', foundational, covenant_ishmael_transmission).
narrative_ontology:cs_axiom_status(covenant_ishmael_transmission, holdable).
narrative_ontology:cs_axiom_grounding('afaca967-6e1a-4e77-a93f-3f9d59942052', covenant_ishmael_transmission, empirically_contingent).
narrative_ontology:cs_axiom('afaca967-6e1a-4e77-a93f-3f9d59942052', foundational, muhammadan_prophetic_legitimacy).
narrative_ontology:cs_axiom_status(muhammadan_prophetic_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('afaca967-6e1a-4e77-a93f-3f9d59942052', muhammadan_prophetic_legitimacy, deontological).
narrative_ontology:cs_reference_frame('afaca967-6e1a-4e77-a93f-3f9d59942052', abraham_covenant_inclusive_lineage).
narrative_ontology:cs_drift_state('afaca967-6e1a-4e77-a93f-3f9d59942052', contemporary_comparative_theology, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('afaca967-6e1a-4e77-a93f-3f9d59942052', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__ishmael_covenant_reading, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, islamic_communities).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, quranic_exegetes).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, islamic_jurisprudence_tradition).
narrative_ontology:constraint_victim(abrahamic_covenant__ishmael_covenant_reading, jewish_exclusivity_claims).
narrative_ontology:constraint_victim(abrahamic_covenant__ishmael_covenant_reading, supersessionist_christian_readings).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(abrahamic_covenant__ishmael_covenant_reading, jewish_legal_tradition).
narrative_ontology:constraint_vindicates(abrahamic_covenant__ishmael_covenant_reading, abrahamic_prophecy_continuity).
narrative_ontology:constraint_vindicates(abrahamic_covenant__ishmael_covenant_reading, muhammadan_prophetic_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claim direct scriptural grounding in Genesis for Islamic legitimacy through Ishmael's line. The reading vindicates Islamic prophetic tradition as covenantal continuation rather than later innovation. Exit would mean abandoning a foundational legitimacy narrative woven into Islamic theology, law, and identity across 1,400 years.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, islamic_communities, beneficiary,
    organized, civilizational, identity_locked, global).

% The reading directly challenges the interpretation of Genesis 17:19-21 that restricts covenant transmission to Isaac's descendants. Jewish legal tradition, institutional authority, and covenantal self-understanding have been built on this exclusivity claim. The ishmael_reading functions as a structural competitor that, if accepted, would retroactively redefine Jewish covenant claims as incomplete or secondary.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, jewish_exclusivity_claims, payer,
    institutional, civilizational, identity_locked, global).

% Interpret and maintain the textual foundation for this reading—primarily Quran 2:124-129, 3:33-34, 19:54-58, and related verses treating Ishmael as covenantal heir and Muhammad as prophetic fulfillment. They set scholarly standards for how Genesis-Quran continuity is established and defended. Their interpretive authority depends on sustaining the coherence and textual grounding of the reading.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, quranic_exegetes, agenda_setter,
    institutional, civilizational, mobile, global).

% Christian supersessionist theology (the Church replaces Israel as the covenant people) is structurally distinct from the ishmael_reading but competes for the same theological real estate: does covenant continue and if so, to whom? The ishmael_reading's affirmation of ongoing Islamic prophecy creates a third claimant, complicating the binary supersessionist framing that had dominated Christian-Jewish theological discourse. They are partially excluded from the scriptural contest itself, as their claim to supersession becomes one option among three rather than a resolution.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, supersessionist_christian_readings, payer,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__ishmael_covenant_reading, supersessionist_christian_readings, excluded).

% Academic study of Abrahamic traditions and textual correspondence. Observes and analyzes the exegetical stakes: which readings are textually defensible, how communities deploy them, what institutional consequences follow from each reading's acceptance or rejection.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, comparative_theology_scholarship, observer,
    institutional, generational, analytical, global).

% Built explicitly on the exclusivity of Abrahamic covenant through Isaac. Halakha, covenant theology, and institutional Jewish self-understanding for two millennia have rested on the interpretation that Genesis 17:19-21 restricts the covenant to Isaac's line. The ishmael_reading does not merely offer an alternative interpretation; it retroactively redefines Jewish covenantal claims as incomplete if not wrong.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, jewish_legal_tradition, payer,
    institutional, civilizational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(abrahamic_covenant__ishmael_covenant_reading, islamic_communities).
narrative_ontology:fixing_cost_class(abrahamic_covenant__ishmael_covenant_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared scriptural foundation across Islamic, Jewish, and Christian traditions: all three claim Abraham as patriarch and legitimacy through interpretations of Genesis. The ishmael_reading coordinates an Islamic reading of covenantal continuity that positions Islamic prophecy not as innovation but as scriptural continuation, creating a narrative space where multiple Abrahamic traditions claim the same Genesis text.
% TRANSFER_FUNCTION: Moves theological legitimacy from Jewish-exclusive covenant interpretations to an expanded, Islam-inclusive reading. Also moves exegetical authority: the reading validates Islamic scholarly interpretation of Genesis-Quran continuity as textually grounded rather than post-hoc. The 'transfer' is not material but epistemic and institutional: it shifts what counts as valid covenant theology in comparative Abrahamic discourse.
% ABSENT_VOICES: Secular scholars who read Genesis as culturally contingent (not normative for any modern community), historical-critical scholars who treat covenantal claims as textual constructions rather than binding authority, Jewish communities that reject the exclusivity reading in favor of universalist or pluralist interpretations (they exist but are institutionally marginal within Jewish tradition). Also absent: voices of communities who live the on-the-ground consequences (Palestinians, Israeli communities) for whom covenant theology maps onto territorial and political claims but who have limited standing in the academic theological contest itself.
% DISAPPEARANCE_RATIONALE: If the ishmael_reading ceased to have institutional authority within Islamic theology and Quranic exegesis, Islamic legitimacy claims grounded in Genesis would lose their most systematic scholarly foundation. Islamic communities would lose a key argument for covenantal continuity with prior Abrahamic traditions. Comparative theology would revert to a binary (Jewish exclusivity vs. Christian supersessionism) rather than a three-way contest. Jewish and Christian theological self-understanding might shift if the Islamic challenge to exclusivity disappeared. The institutional landscape of Abrahamic comparative theology would reorganize around different theological configurations.
% FOUNDING_PROBLEM: After the rise of Islam in the 7th century, Muslim communities needed to establish that Islamic prophecy was not a break from Abrahamic tradition but its continuation. Quranic claims of covenant through Ishmael and prophecy through Muhammad required scriptural grounding that showed Islam as the fulfillment rather than the falsification of Abraham's covenant. The reading was constructed to meet that legitimacy need: establishing textual warrant in Genesis for what the Quran asserts.
% FOUNDING_PROBLEM_CORROBORATION: Islamic theology and Quranic exegesis continue to affirm and develop this reading as foundational (attestation from the beneficiary seat). Comparative theologians and scholars of Islamic tradition (external observers) confirm that this reading remains institutionally live—it structures Islamic theological identity and prophetic self-understanding. However, Jewish and Christian scholars contest its textual validity; they attest that Genesis 17:19-21 does NOT clearly warrant the ishmael_reading, and that the reading represents later post-Quranic exegetical elaboration imposed onto Genesis rather than discovered there. The founding problem is therefore contested: Islamic communities attest it is live and scripturally necessary; Jewish and Christian scholars attest it reflects a reading choice, not a textual constraint.
narrative_ontology:disappearance_verdict(abrahamic_covenant__ishmael_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__ishmael_covenant_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__ishmael_covenant_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(abrahamic_covenant__ishmael_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__ishmael_covenant_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__ishmael_covenant_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(abrahamic_covenant__ishmael_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(abrahamic_covenant__ishmael_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the reading accomplishes genuine coordination—it establishes a shared scriptural framework across traditions—but simultaneously extracts epistemic authority from Jewish-exclusivity readings. The reading does not merely offer an alternative; it repositions Jewish covenant claims as incomplete. Suppression is substantial (0.62) because the reading's persistence requires active institutional maintenance: theological argumentation against competing exegeses, scholarly defense of textual warrant, institutional reinforcement through educational curricula and authoritative commentary. Theater is moderate-high (0.41) because approximately 40% of the institutional work maintaining this reading involves performative rehearsal of exegetical conclusions already settled within Islamic tradition, rather than genuine new scriptural discovery. The reading was established by the 8th-9th centuries in Islamic exegetical literature; modern maintenance involves repetition and institutional reinforcement of settled conclusions. Measurement series show gradual extraction increase from early establishment (t=0, projected 0.45) to modern period (t=1400, observed 0.58), with suppression intensifying in early centuries (0-600) as competing readings hardened, then stabilizing as the reading achieved institutional entrenchment. The leveling of extraction at t=1300-1400 reflects the reading's stable institutional position in modern Islamic theology.
 *
 * PERSPECTIVAL GAP:
 *   The ishmael_reading produces sharply divergent perceptions across stakeholder seats. From the agenda-setter seat (Quranic exegetes), this is genuine coordination work: establishing scriptural continuity validates Islamic prophecy and vindicates Abrahamic legitimacy. From the payer seats (Jewish exclusivity claims, Jewish legal tradition), the reading is a structural attack on covenantal uniqueness—it retroactively redefines Jewish covenant claims as incomplete or secondary. The engine computes these divergent perceptions from the structural data: the agenda-setter benefits from expanding the covenant set and validating their interpretive authority; the payers lose exclusive claim to Abraham and face a competitor for the same textual foundation. Neither perception is 'wrong'—they reflect different positions inside the same constraint structure. From the analytical seat, both are true simultaneously: the reading coordinates Abrahamic traditions while extracting authority from Jewish-exclusivity claims.
 *
 * DIRECTIONALITY LOGIC:
 *   Islamic communities sit as strong beneficiaries (d approaching 0.2): they gain scriptural grounding, prophetic legitimacy, and expanded claim to Abrahamic continuity. Quranic exegetes sit as agenda-setters who collects interpretive authority (d near beneficiary end, ~0.15). Jewish exclusivity claims sit as victims (d near 0.85): they lose unique covenant claim and face a competitor positioned as equally textually valid. Jewish legal tradition sits similarly (d ~0.80) because halakha explicitly rests on Isaac-only transmission; the ishmael_reading retroactively redefines that exclusivity as a reading choice rather than a textual necessity. Supersessionist Christian readings sit in a complex middle position: they are partially excluded (no longer the third pole in a binary; now one option among three), and their suppression depends on active defense of their own textual reading against both Jewish exclusivity AND Islamic inclusion (d ~0.65, moderately targeted but with some structural relief as attention diverts to the new three-way contest). The directionality profile reflects that this constraint's extraction operates primarily through epistemic repositioning—shifting what counts as valid covenant theology—rather than through direct coercive power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Islamic need for scriptural legitimacy in Genesis) was live at t=0 (early Islamic period, 7th-8th centuries) and remains live in contemporary Islamic theology. However, the reading faces a secondary mandatrophy: as Islamic scholarship has developed and institutionalized, the ishmael_reading has become an established fact of Islamic theology rather than a *solution* to the legitimacy problem. The problem itself has been absorbed into Islamic institutional identity. Modern Quranic exegetes maintain the reading not primarily to solve the founding problem but to preserve institutional continuity with classical Islamic tradition. The reading persists partly through genuine scholarly commitment to covenantal continuity but increasingly through institutional inertia—it is part of 'how Islamic theology is done.' The theater ratio (0.41 at t=1400) captures this partial degradation: while the reading remains institutionally alive, a growing proportion of the work maintaining it is performative rehearsal of settled conclusions rather than fresh scriptural engagement. The constraint is not yet a piton (the founding problem is still live and the reading still functions) but shows early mandatrophy indicators: the ratio of genuine coordination work to theatrical maintenance is declining over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_warrant_genesis_17_19_21,
    'Does Genesis 17:19-21 actually restrict covenant transmission to Isaac, or does it permit (or even support) an inclusive reading through Ishmael?',
    'Systematic comparative analysis of Hebrew grammar, ancient Near Eastern covenant terminology, and manuscript variants. Higher-criticism study of compositional history. Evaluation by scholars working from within Jewish textual traditions and from external comparative perspective.',
    'If the text clearly restricts to Isaac, the ishmael_reading is exegetically weak and relies on non-textual Islamic sources (Quran) to override Genesis. If the text is ambiguous or permits inclusion, the reading gains textual legitimacy and shifts from ''Islamic innovation'' to ''recovered inclusive reading.'' The epistemic authority of the ishmael_reading depends on this resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_warrant_genesis_17_19_21, empirical, 'Textual basis of the ishmael_reading''s warrant in Genesis.').

omega_variable(
    quran_genesis_continuity_vs_supersession,
    'When the Quran affirms Ishmael and Muhammad as covenantal heirs, is it presenting a reading of Genesis (textual continuity) or a supersession of Genesis (new revelation overriding prior text)?',
    'Analysis of Quranic language and Islamic theological tradition''s own account of revelation relations. Study of how Islamic exegetes themselves frame the relationship between Quranic claims and Genesis narrative.',
    'If the Quran intends textual continuity (the ishmael_reading''s framework), the reading is hermeneutically coherent. If the Quran intends supersession, the reading is re-narrating what is actually a competitive claim as if it were interpretive continuity. This affects whether the reading coordinates Abrahamic traditions (continuity) or replaces one with another (supersession).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quran_genesis_continuity_vs_supersession, conceptual, 'Whether the ishmael_reading frames Islamic prophecy as Genesis interpretation or as Quranic supersession.').

omega_variable(
    institutional_cost_to_jewish_tradition,
    'To what extent does institutional acceptance of the ishmael_reading require Jewish communities to abandon or significantly revise their own covenant theology?',
    'Engagement between Jewish theologians (both those maintaining exclusivity readings and those open to pluralist alternatives) and Islamic exegetes. Lived experience of communities holding multiple Abrahamic readings. Institutional policy decisions by Jewish and Islamic organizations about covenantal claims.',
    'If the reading can be accepted without requiring Jewish abandonment of covenantal identity, the extraction component (retroactive repositioning of Jewish claims) is reduced. If genuine covenantal pluralism emerges, the reading shifts toward rope (pure coordination). If communities maintain incompatible readings, the reading persists as tangled_rope or moves toward snare (enforced exclusivity of Islamic reading at the expense of Jewish tradition).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_cost_to_jewish_tradition, preference, 'Whether accepting the ishmael_reading is compatible with maintaining Jewish covenantal theology or requires its subordination.').

omega_variable(
    enforcement_mechanism_institutional_suppression,
    'Is the suppression measured in this reading (0.62) structural (the reading naturally sustains itself because it is textually sound) or institutional (the reading must be actively defended against competing exegeses and alternative readings)?',
    'Post-exit analysis: if Islamic communities faced institutional pressure to engage competing readings on equal ground, would the ishmael_reading remain persuasive or would it require sustained institutional protection? Historical counterfactual: in periods where Jewish exclusivity readings had greater institutional power (medieval period), how much did they suppress the ishmael_reading through force vs. argument?',
    'If suppression is primarily structural (the reading is textually compelling), the constraint is genuinely coordinative. If suppression is primarily institutional (the reading requires active defense), the constraint has stronger extractive character. The distinction affects whether the reading counts as tangled_rope (genuine coordination plus enforcement) or approaches snare (extraction defended as coordination).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_mechanism_institutional_suppression, empirical, 'Whether the ishmael_reading''s persistence depends on structural textual merit or institutional suppression of alternatives.').

omega_variable(
    kernel_reading_or_textual_imposition,
    'Is this a genuine kernel reading (the abrahamic_covenant text permits and rewards this interpretation) or a textual imposition (Islamic theology is read back into Genesis post-hoc)?',
    'Systematic exegetical analysis asking: if Genesis existed in isolation without Islamic theology, would careful readers naturally arrive at the ishmael_reading? Or does the reading depend on Islamic Quranic claims as interpretive leverage? Comparison with other readings of the same kernel to assess their claim to textual warrant vs. extra-textual imposition.',
    'If genuine reading: the constraint is a valid kernel reading. If imposition: the constraint is a committer frame error—what appears to be one kernel has actually split into two (Genesis-alone vs. Genesis-plus-Quran), and the ishmael_reading lives in the second kernel, not the first. This affects whether the kernel_id is correctly applied and whether the reading''s epistemic legitimacy derives from Genesis or from Islamic textual authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_or_textual_imposition, conceptual, 'Whether the ishmael_reading is a defensible Genesis interpretation or an imposition of Islamic theology onto Genesis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__ishmael_covenant_reading, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abra_tr_t0, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(abra_tr_t0, projected).
narrative_ontology:measurement(abra_tr_t200, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 200, 0.37).
narrative_ontology:measurement_basis(abra_tr_t200, observed).
narrative_ontology:measurement(abra_tr_t600, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 600, 0.4).
narrative_ontology:measurement_basis(abra_tr_t600, observed).
narrative_ontology:measurement(abra_tr_t1000, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 1000, 0.42).
narrative_ontology:measurement_basis(abra_tr_t1000, observed).
narrative_ontology:measurement(abra_tr_t1300, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 1300, 0.41).
narrative_ontology:measurement_basis(abra_tr_t1300, observed).
narrative_ontology:measurement(abra_tr_t1400, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 1400, 0.41).
narrative_ontology:measurement_basis(abra_tr_t1400, observed).

% Extraction over time
narrative_ontology:measurement(abra_be_t0, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(abra_be_t0, projected).
narrative_ontology:measurement(abra_be_t200, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 200, 0.5).
narrative_ontology:measurement_basis(abra_be_t200, observed).
narrative_ontology:measurement(abra_be_t600, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 600, 0.55).
narrative_ontology:measurement_basis(abra_be_t600, observed).
narrative_ontology:measurement(abra_be_t1000, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 1000, 0.57).
narrative_ontology:measurement_basis(abra_be_t1000, observed).
narrative_ontology:measurement(abra_be_t1300, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 1300, 0.58).
narrative_ontology:measurement_basis(abra_be_t1300, observed).
narrative_ontology:measurement(abra_be_t1400, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 1400, 0.58).
narrative_ontology:measurement_basis(abra_be_t1400, observed).

% Suppression requirement over time
narrative_ontology:measurement(abra_su_t0, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(abra_su_t0, projected).
narrative_ontology:measurement(abra_su_t200, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 200, 0.48).
narrative_ontology:measurement_basis(abra_su_t200, observed).
narrative_ontology:measurement(abra_su_t600, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 600, 0.58).
narrative_ontology:measurement_basis(abra_su_t600, observed).
narrative_ontology:measurement(abra_su_t1000, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 1000, 0.62).
narrative_ontology:measurement_basis(abra_su_t1000, observed).
narrative_ontology:measurement(abra_su_t1300, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 1300, 0.61).
narrative_ontology:measurement_basis(abra_su_t1300, observed).
narrative_ontology:measurement(abra_su_t1400, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 1400, 0.62).
narrative_ontology:measurement_basis(abra_su_t1400, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__ishmael_covenant_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(abrahamic_covenant__ishmael_covenant_reading, 0.12).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, abrahamic_covenant__isaac_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, abrahamic_covenant__land_promise_constraint).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, abrahamic_covenant__christian_supersessionist_reading).

% DUAL FORMULATION NOTE:
% The abrahamic_covenant kernel decomposes into at least three structurally distinct constraint stories: (1) isaac_covenant_reading—covenant restricted to Isaac, exclusively Jewish; (2) ishmael_covenant_reading (this story)—covenant continues through Ishmael to Muhammad, validating Islamic prophecy; (3) land_promise_constraint—territorial component of the covenant, distinct from prophetic succession question. These three readings have different epsilon values, different victim/beneficiary structures, and different institutional enforcement mechanisms. The isaac_reading and ishmael_reading directly contradict each other's core premises within a single interpretive framework (Jewish or Islamic tradition), yet coexist across traditions. The land_promise_constraint operates on a partly orthogonal axis (territorial rather than prophetic succession). Each reading is a separate constraint story with its own ε, its own stakeholders, and its own classification. The three are linked via network.affects_constraints because contested interpretation of the same Genesis kernel creates structural pressure across readings: if the ishmael_reading gains institutional authority, the isaac_reading loses unique covenantal claim; if the land_promise reading is applied to validate Israeli territorial claims, it influences the stakes of both prophetic succession readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(abrahamic_covenant__ishmael_covenant_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
