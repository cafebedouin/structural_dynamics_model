% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__ishmael_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: abrahamic_covenant__ishmael_covenant_reading
 *   human_readable: Ishmaelite/Islamic Reading of the Abrahamic Covenant (Prophetic Succession to Muhammad)
 *   domain: religious_studies/comparative_theology/institutional_authority
 *
 * SUMMARY:
 *   This constraint represents the Islamic theological reading of the
 *   Abrahamic covenant kernel: that the covenant God made with Abraham
 *   continues through Ishmael's line to Muhammad, and that the Genesis
 *   promise should be read as inclusive of Ishmael's descendants rather than
 *   exclusive to Isaac's. This reading is one of three competing
 *   instantiations of the same underlying kernel (the Abrahamic covenant text
 *   and tradition); it is generated here as its own clean, ε-invariant
 *   constraint, structurally distinct from the isaac_covenant_reading
 *   (exclusive transmission through Isaac) and the
 *   christian_supersessionist_reading (fulfillment/supersession through
 *   Christ). Each reading has its own beneficiary set, its own victims, and
 *   its own extraction profile — they are not the same constraint viewed from
 *   different angles.
 *
 * KEY AGENTS:
 *   - islamic_ummah: Primary beneficiary (organized/identity_locked) — draws religious legitimacy and communal identity from this reading
 *   - islamic_clergy_and_scholars: Agenda-setter (institutional/identity_locked) — articulates, transmits, and defends the reading through tafsir and jurisprudence
 *   - jewish_covenantal_exclusivity_claim: Primary payer (organized/trapped) — the rival exclusivist claim this reading directly challenges
 *   - rabbinic_authority_structures: Institutional payer (institutional/constrained) — bears diffuse erosion of exclusivity-grounded authority
 *   - christian_supersessionist_claimants: Secondary payer (institutional/constrained) — loses exclusive-claimant status in a now three-way field
 *   - comparative_religion_scholars: Analytical observer — documents the structure without adjudicating theological truth
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__ishmael_covenant_reading, 0.42).
domain_priors:suppression_score(abrahamic_covenant__ishmael_covenant_reading, 0.38).
domain_priors:theater_ratio(abrahamic_covenant__ishmael_covenant_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__ishmael_covenant_reading, tangled_rope).
narrative_ontology:human_readable(abrahamic_covenant__ishmael_covenant_reading, "Ishmaelite/Islamic Reading of the Abrahamic Covenant (Prophetic Succession to Muhammad)").
narrative_ontology:topic_domain(abrahamic_covenant__ishmael_covenant_reading, "religious_studies/comparative_theology/institutional_authority").

domain_priors:requires_active_enforcement(abrahamic_covenant__ishmael_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__ishmael_covenant_reading, 'ba9ee075-207b-4371-87e0-74b24f4de6d2').
narrative_ontology:cs_kernel_codification('ba9ee075-207b-4371-87e0-74b24f4de6d2', fixed_text).
narrative_ontology:cs_authority_grounding('ba9ee075-207b-4371-87e0-74b24f4de6d2', lineage).
narrative_ontology:cs_interpretation_layer_present('ba9ee075-207b-4371-87e0-74b24f4de6d2').
narrative_ontology:cs_reading_relation('ba9ee075-207b-4371-87e0-74b24f4de6d2', abrahamic_covenant__isaac_covenant_reading, forecloses).
narrative_ontology:cs_reading_relation('ba9ee075-207b-4371-87e0-74b24f4de6d2', abrahamic_covenant__christian_supersessionist_reading, coexists_with).
narrative_ontology:cs_axiom('ba9ee075-207b-4371-87e0-74b24f4de6d2', foundational, covenant_transmits_inclusively_through_multiple_sons).
narrative_ontology:cs_axiom_status(covenant_transmits_inclusively_through_multiple_sons, holdable).
narrative_ontology:cs_axiom_grounding('ba9ee075-207b-4371-87e0-74b24f4de6d2', covenant_transmits_inclusively_through_multiple_sons, conventional).
narrative_ontology:cs_axiom('ba9ee075-207b-4371-87e0-74b24f4de6d2', secondary, prophetic_succession_validates_retroactive_covenantal_inclusion).
narrative_ontology:cs_axiom_status(prophetic_succession_validates_retroactive_covenantal_inclusion, holdable).
narrative_ontology:cs_axiom_grounding('ba9ee075-207b-4371-87e0-74b24f4de6d2', prophetic_succession_validates_retroactive_covenantal_inclusion, theological).
narrative_ontology:cs_reference_frame('ba9ee075-207b-4371-87e0-74b24f4de6d2', quranic_hagar_ishmael_legitimation_tradition).
narrative_ontology:cs_drift_state('ba9ee075-207b-4371-87e0-74b24f4de6d2', contemporary_interfaith_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('ba9ee075-207b-4371-87e0-74b24f4de6d2', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__ishmael_covenant_reading, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, islamic_ummah).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, islamic_clergy_and_scholars).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, muhammads_prophetic_legitimacy_claim).
narrative_ontology:constraint_victim(abrahamic_covenant__ishmael_covenant_reading, jewish_covenantal_exclusivity_claim).
narrative_ontology:constraint_victim(abrahamic_covenant__ishmael_covenant_reading, rabbinic_authority_structures).
narrative_ontology:constraint_victim(abrahamic_covenant__ishmael_covenant_reading, christian_supersessionist_claimants).
narrative_ontology:constraint_vindicates(abrahamic_covenant__ishmael_covenant_reading, genesis_promise_as_inclusive_reading).
narrative_ontology:constraint_vindicates(abrahamic_covenant__ishmael_covenant_reading, islamic_prophetic_succession_doctrine).
narrative_ontology:constraint_vindicates(abrahamic_covenant__ishmael_covenant_reading, hagar_ishmael_legitimacy_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The global Muslim community holds religious identity and salvific standing on the claim that Ishmael, not only Isaac, carries the Abrahamic covenant forward to Muhammad. Their standing as heirs of Abraham's blessing is constituted by this reading; abandoning it would not merely change a doctrine but dissolve a core plank of communal self-understanding.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, islamic_ummah, beneficiary,
    organized, civilizational, identity_locked, global).

% Ulama, exegetes (mufassirun), and institutions of Islamic jurisprudence articulate, teach, and defend the reading that the covenant runs through Ishmael to Muhammad's prophethood. They administer the interpretive tradition (tafsir, hadith transmission) that stabilizes this reading against rival claims and shape religious education, sermons, and doctrine around it.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, islamic_clergy_and_scholars, agenda_setter,
    institutional, civilizational, identity_locked, global).

% The traditional Jewish reading that the covenant is transmitted exclusively through Isaac (and thus through the Jewish people alone) is directly contested by this reading's inclusive interpretation. Rabbinic communities cannot simply exit the dispute — their claim to unique covenantal chosenness is the thing being structurally challenged, and no negotiation restores exclusivity once a rival lineage claim gains adherents and political weight.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, jewish_covenantal_exclusivity_claim, payer,
    organized, civilizational, trapped, global).

% Institutions of rabbinic Judaism that ground communal legitimacy and interpretive authority in exclusive covenantal transmission through Isaac bear a diffuse but real cost: the Ishmaelite reading's spread erodes the uniqueness claim that undergirds Jewish theological distinctiveness in interfaith and political contexts (including disputes over the Land of Canaan).
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, rabbinic_authority_structures, payer,
    institutional, civilizational, constrained, global).

% Christian traditions that read the covenant as fulfilled and superseded through Christ (a third rival claim) also lose ground when a third lineage — Ishmael to Muhammad — enters as a live competing claimant; the field of covenantal successor-claims becomes three-way rather than two-way, diluting each exclusivist claim's persuasive monopoly.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, christian_supersessionist_claimants, payer,
    institutional, civilizational, constrained, global).

% The narrative tradition itself — the story of Hagar and Ishmael's expulsion and divine care (echoed in Quranic and hadith material) — is elevated from a peripheral Genesis episode to a central legitimating narrative. It is not an actor but a vindicated proposition that gains doctrinal centrality through this reading.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, hagar_ishmael_narrative_tradition, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(abrahamic_covenant__ishmael_covenant_reading, hagar_ishmael_narrative_tradition).

% Historians of religion and textual critics analyze how each Abrahamic tradition constructs covenantal legitimacy from a shared textual corpus, without adjudicating theological truth. They document how political and communal interests shape which reading a community adopts and maintains.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, comparative_religion_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the Islamic community with a coherent theological account of its place within (rather than outside) the Abrahamic prophetic lineage, coordinating religious identity, communal solidarity, and doctrinal continuity across a global and diverse ummah.
% TRANSFER_FUNCTION: Moves interpretive legitimacy and covenantal standing away from an exclusive Isaac-line claim and toward an inclusive lineage recognizing Ishmael and Muhammad; in political and interfaith contexts this also carries weight in disputes over religious primacy and, adjacently, territorial legitimacy claims.
% ABSENT_VOICES: Rabbinic authorities and Christian theological institutions who hold rival exclusivist or supersessionist readings would object that this reading reinterprets their foundational texts against their own transmitted tradition; they are not absent from the broader discourse but are structurally external to the interpretive community that produces and sustains this specific reading.
% DISAPPEARANCE_RATIONALE: If this reading disappeared, Islamic theological identity would not dissolve (the ummah's core practice and creed do not depend solely on this covenantal argument), but a significant plank of interfaith legitimacy discourse and a key doctrinal bridge to the Hebrew Bible's authority would vanish, altering Islamic apologetics, interfaith dialogue framing, and some strands of religious education. Whether the 'world rearranges' or stays 'unchanged' is itself disputed between the reading's proponents (who see it as load-bearing) and critics (who see it as one argument among many for Islamic legitimacy).
% FOUNDING_PROBLEM: Early Islamic theology needed to account for Muhammad's prophethood within, rather than as a rupture from, the biblical prophetic tradition already authoritative among Jews and Christians in Arabia and the wider region — establishing continuity rather than novelty.
% FOUNDING_PROBLEM_CORROBORATION: Historians of late antique religion (outside both the Islamic and Jewish interpretive communities) attest that establishing prophetic continuity was a live concern for nascent religious movements competing for legitimacy in a shared scriptural environment; this corroboration comes from academic religious history rather than from either benefiting tradition's own theologians.
narrative_ontology:disappearance_verdict(abrahamic_covenant__ishmael_covenant_reading, contested).
narrative_ontology:founding_problem_status(abrahamic_covenant__ishmael_covenant_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__ishmael_covenant_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(abrahamic_covenant__ishmael_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__ishmael_covenant_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__ishmael_covenant_reading_tests).
:- end_tests(abrahamic_covenant__ishmael_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at a moderate 0.42 because this reading does not extract material resources so much as contested interpretive legitimacy — a real but non-material transfer. Suppression (0.38) reflects that this reading does not require coercive enforcement against rival readings in the way a state-backed doctrine might; its persistence depends on communal transmission, education, and apologetics rather than force. Theater ratio (0.30) is moderate: substantial genuine coordination function (identity formation, doctrinal coherence) coexists with performative apologetic contest against rival traditions. Resistance is high (0.72) because this reading is actively and continuously contested by two rival interpretive communities with their own institutional weight — it does not sit unchallenged.
 *
 * DIRECTIONALITY LOGIC:
 *   The islamic_ummah and islamic_clergy_and_scholars sit toward the beneficiary end: the reading constitutes their religious standing and interpretive authority. The jewish_covenantal_exclusivity_claim and rabbinic_authority_structures sit toward the target end: their claim to unique covenantal status is what this reading structurally displaces, and their exit options are trapped/constrained because the dispute is definitional to their own tradition's core self-understanding, not a negotiable policy. Christian supersessionist claimants experience a secondary, diluting cost — this reading doesn't target them directly but weakens their own exclusivity-style argument by crowding the field with a third claimant.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — establishing prophetic continuity between Muhammad's message and the prior biblical tradition — remains live rather than dead: it continues to do real theological and apologetic work in interfaith contexts and Islamic education, rather than persisting as a hollow institutional shell. This classification (tangled_rope) prevents mislabeling the reading as pure extraction: it does coordinate real communal identity and doctrinal coherence for a global ummah (the rope function), while simultaneously imposing a real cost on rival exclusivity claims through direct contestation (the extraction/tangled function) — both are structurally present, which is why tangled_rope rather than pure rope or pure snare is the authored claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genesis_text_inclusive_vs_exclusive_ambiguity,
    'Does the Hebrew text of Genesis 17:19-21 (and surrounding covenant passages) admit an inclusive reading in which Ishmael retains a subordinate but real covenantal share, or does it definitively exclude him in favor of Isaac alone?',
    'Comparative textual-critical analysis of the Hebrew source text, its ancient Near Eastern context, and the earliest attested interpretive traditions (rabbinic midrash, pre-Islamic Arabian tradition) independent of any single tradition''s theological commitments.',
    'If the text is genuinely ambiguous, this reading''s claim to inclusiveness is textually defensible and competes on more equal footing with the exclusivist reading; if the text is unambiguous in one direction, one reading''s extraction from the other is more clearly a constructed rather than textually grounded claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genesis_text_inclusive_vs_exclusive_ambiguity, empirical, 'Whether the underlying Genesis text supports an inclusive or exclusive covenant reading.').

omega_variable(
    kernel_framing_lineage_vs_legitimacy_narrative,
    'Should this constraint be framed as a claim about literal covenantal lineage (biological/genealogical descent through Ishmael), or as a claim about narrative legitimacy (a theological argument using genealogy as a rhetorical/legitimating device rather than an empirical genealogical claim)?',
    'Examine how the reading is actually deployed in Islamic theological argument: is genealogical descent load-bearing (would a genetic disproof of descent undermine the claim), or is the genealogy primarily a narrative/typological device (in which case only the narrative''s persuasive force matters, not its historicity)?',
    'Under the lineage framing, this constraint is more vulnerable to empirical/historical challenge and its extraction from rival claims is more direct. Under the legitimacy-narrative framing, the constraint operates more like a coexisting theological-rhetorical tradition with lower direct extraction and more resemblance to a rope than a tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_lineage_vs_legitimacy_narrative, conceptual, 'Whether this reading is a genealogical claim or a legitimacy narrative — this framing choice affects classification.').

omega_variable(
    political_instrumentalization_of_reading,
    'To what extent is the persistence and intensity of this reading (versus remaining a minor theological point) driven by ongoing interfaith political competition (including territorial and geopolitical disputes) rather than purely theological/exegetical concerns?',
    'Track correlation between periods of heightened political tension (e.g., regional conflict, colonial-era religious competition, modern geopolitical disputes) and intensified doctrinal emphasis on covenantal lineage claims across traditions.',
    'If political instrumentalization is substantial, the extraction and resistance metrics understate the degree to which this is a proxy conflict for material/political stakes rather than a purely theological dispute — suggesting a higher effective extractiveness than the base theological reading alone would carry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_instrumentalization_of_reading, empirical, 'Whether political conflict amplifies the theological dispute beyond its purely doctrinal content.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__ishmael_covenant_reading, 600, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abra_tr_t600, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 600, 0.2).
narrative_ontology:measurement(abra_tr_t900, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 900, 0.22).
narrative_ontology:measurement(abra_tr_t1200, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 1200, 0.24).
narrative_ontology:measurement(abra_tr_t1500, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 1500, 0.26).
narrative_ontology:measurement(abra_tr_t1800, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 1800, 0.27).
narrative_ontology:measurement(abra_tr_t1948, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 1948, 0.29).
narrative_ontology:measurement(abra_tr_t2025, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(abra_be_t600, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 600, 0.3).
narrative_ontology:measurement(abra_be_t900, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 900, 0.34).
narrative_ontology:measurement(abra_be_t1200, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 1200, 0.36).
narrative_ontology:measurement(abra_be_t1500, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 1500, 0.38).
narrative_ontology:measurement(abra_be_t1800, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 1800, 0.4).
narrative_ontology:measurement(abra_be_t1948, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 1948, 0.41).
narrative_ontology:measurement(abra_be_t2025, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 2025, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(abrahamic_covenant__ishmael_covenant_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, isaac_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, christian_supersessionist_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, land_promise_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings decomposed from the natural-language 'Abrahamic covenant' kernel per the ε-invariance principle. isaac_covenant_reading claims exclusive transmission through Isaac (this reading's direct foreclosure target); christian_supersessionist_reading claims fulfillment/transfer through Christ (a coexisting rather than foreclosing rival); land_promise_constraint addresses a structurally distinct territorial-grant question that intersects with but is not identical to the lineage-legitimacy question addressed here. Each carries its own ε and stakeholder set; none should be merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
