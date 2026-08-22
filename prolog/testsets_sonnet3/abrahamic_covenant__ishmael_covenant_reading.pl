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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Ishmaelite/Islamic Reading of the Abrahamic Covenant
 *   domain: religious_studies/comparative_theology/institutional_authority
 *
 * SUMMARY:
 *   This story authors the Ishmael-covenant reading of the contested
 *   Abrahamic covenant kernel: the claim that Genesis's covenant promise,
 *   rather than being narrowed to Isaac's line, extends through Ishmael to
 *   Muhammad, making the Islamic community a legitimate — and in some
 *   readings the corrective completion of — Abraham's covenantal heirs. This
 *   reading is authored as its own ε-invariant constraint. It does not
 *   average or hedge against the Isaac-exclusive reading or the Christian
 *   supersessionist reading; those are separate constraints
 *   (isaac_covenant_reading, christian_supersessionist_reading) linked here
 *   only via network edges. The referent for extractiveness is the standing
 *   arrangement under contest — the ongoing institutional and interpretive
 *   competition over covenantal legitimacy — assessed by this reading's own
 *   lights, not by the alternative it endorses.
 *
 * KEY AGENTS:
 *   - islamic_scholarly_authorities: institutional agenda-setter and beneficiary — codifies and teaches the reading, derives institutional legitimacy from it
 *   - muslim_believers: organized beneficiary, identity-locked — receive communal identity and Abrahamic belonging through this reading
 *   - jewish_covenantal_exclusivists: organized payer, trapped — the reading directly contests their exclusive-inheritance claim
 *   - rabbinic_authorities_defending_isaac_line: institutional payer, constrained — must actively contest the reading to preserve interpretive authority
 *   - christian_theological_bodies: excluded institutional actor holding a third, structurally adjacent reading
 *   - comparative_religion_scholars: analytical observer of the genealogical contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__ishmael_covenant_reading, 0.42).
domain_priors:suppression_score(abrahamic_covenant__ishmael_covenant_reading, 0.38).
domain_priors:theater_ratio(abrahamic_covenant__ishmael_covenant_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(abrahamic_covenant__ishmael_covenant_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__ishmael_covenant_reading, tangled_rope).
narrative_ontology:human_readable(abrahamic_covenant__ishmael_covenant_reading, "Ishmaelite/Islamic Reading of the Abrahamic Covenant").
narrative_ontology:topic_domain(abrahamic_covenant__ishmael_covenant_reading, "religious_studies/comparative_theology/institutional_authority").

domain_priors:requires_active_enforcement(abrahamic_covenant__ishmael_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__ishmael_covenant_reading, '85521ca7-4013-408a-b0c2-4780ecde0358').
narrative_ontology:cs_kernel_codification('85521ca7-4013-408a-b0c2-4780ecde0358', fixed_text).
narrative_ontology:cs_authority_grounding('85521ca7-4013-408a-b0c2-4780ecde0358', lineage).
narrative_ontology:cs_interpretation_layer_present('85521ca7-4013-408a-b0c2-4780ecde0358').
narrative_ontology:cs_reading_relation('85521ca7-4013-408a-b0c2-4780ecde0358', abrahamic_covenant__isaac_covenant_reading, forecloses).
narrative_ontology:cs_reading_relation('85521ca7-4013-408a-b0c2-4780ecde0358', abrahamic_covenant__christian_supersessionist_reading, coexists_with).
narrative_ontology:cs_axiom('85521ca7-4013-408a-b0c2-4780ecde0358', foundational, covenant_transmission_is_inclusive_not_exclusive).
narrative_ontology:cs_axiom_status(covenant_transmission_is_inclusive_not_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('85521ca7-4013-408a-b0c2-4780ecde0358', covenant_transmission_is_inclusive_not_exclusive, conventional).
narrative_ontology:cs_axiom('85521ca7-4013-408a-b0c2-4780ecde0358', foundational, prophetic_succession_validates_lineage_claim).
narrative_ontology:cs_axiom_status(prophetic_succession_validates_lineage_claim, holdable).
narrative_ontology:cs_axiom_grounding('85521ca7-4013-408a-b0c2-4780ecde0358', prophetic_succession_validates_lineage_claim, theological).
narrative_ontology:cs_reference_frame('85521ca7-4013-408a-b0c2-4780ecde0358', quranic_confirmatory_revelation).
narrative_ontology:cs_drift_state('85521ca7-4013-408a-b0c2-4780ecde0358', contemporary_interfaith_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('85521ca7-4013-408a-b0c2-4780ecde0358', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__ishmael_covenant_reading, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, islamic_scholarly_authorities).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, muslim_believers).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__ishmael_covenant_reading, islamic_prophetic_institutions).
narrative_ontology:constraint_victim(abrahamic_covenant__ishmael_covenant_reading, jewish_covenantal_exclusivists).
narrative_ontology:constraint_victim(abrahamic_covenant__ishmael_covenant_reading, rabbinic_authorities_defending_isaac_line).
narrative_ontology:constraint_vindicates(abrahamic_covenant__ishmael_covenant_reading, khatam_al_nabiyyin_doctrine).
narrative_ontology:constraint_vindicates(abrahamic_covenant__ishmael_covenant_reading, abrahamic_universalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Codify and teach the doctrine that the Abrahamic covenant passed through Ishmael and culminated in Muhammad's prophethood, drawing on Qur'anic verses (e.g. 2:124-129, 37:100-113) that name Ishmael as covenant-bearer alongside Abraham. This reading grounds Islam's claim to be the corrective completion of prior revelation, and its institutional authority (fiqh councils, university seats, state religious ministries in many countries) depends on this genealogical claim holding.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, islamic_scholarly_authorities, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__ishmael_covenant_reading, islamic_scholarly_authorities, beneficiary).

% Receive religious identity, legitimacy, and belonging through inclusion in the Abrahamic promise via Ishmael. Their communal self-understanding as heirs of Abraham's covenant is constituted by this reading; abandoning it would require reconceiving core elements of Islamic self-identity, not merely updating a belief.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, muslim_believers, beneficiary,
    organized, generational, identity_locked, global).

% Mosques, seminaries, and religious endowments whose continued authority rests partly on Muhammad's status as the final prophet in an unbroken Abrahamic line through Ishmael; funding, waqf structures, and pilgrimage economies (Hajj, Ka'ba narratives tied to Abraham and Ishmael) are structurally tied to this genealogical claim.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, islamic_prophetic_institutions, beneficiary,
    institutional, civilizational, arbitrage, global).

% Hold that the covenant runs exclusively through Isaac and Jacob per Genesis 17:19-21 and rabbinic tradition. The Ishmael reading directly contests their claim to sole covenantal inheritance and, in some political contexts, is invoked to contest land and legitimacy claims tied to that inheritance. They cannot exit the dispute without conceding a foundational element of their self-understanding as covenant people.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, jewish_covenantal_exclusivists, payer,
    organized, civilizational, trapped, global).

% Institutional bodies (rabbinical courts, seminaries) whose interpretive authority over Genesis is challenged by a rival, textually-grounded reading claiming the same source material yields the opposite covenantal outcome. They must actively contest the Ishmael reading in apologetics, education, and interfaith dialogue to maintain their community's exclusive-inheritance self-understanding.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, rabbinic_authorities_defending_isaac_line, payer,
    institutional, civilizational, constrained, global).

% Hold a third reading (supersessionist, covenant fulfilled/transferred through Christ) that is neither validated nor directly contested by the Ishmael reading, but is structurally adjacent — both readings claim to supersede or extend the Isaac-exclusive line, and neither engages the other directly in most theological literature.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, christian_theological_bodies, excluded,
    institutional, civilizational, analytical, global).

% Study the three readings as competing genealogical claims over the same textual kernel, documenting how each community's institutional interests shape its interpretive choices without adjudicating which reading is theologically correct.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__ishmael_covenant_reading, comparative_religion_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides Muslim communities with a coherent genealogical and theological identity linking their prophetic tradition to the older Abrahamic revelation, enabling continuity claims (shared patriarchs, shared sacred geography, shared moral law) that support interfaith self-location and communal cohesion.
% TRANSFER_FUNCTION: Moves interpretive and legitimacy authority over the Genesis covenant text away from exclusive Jewish custodianship and toward a broader, Islamic-inclusive reading; in practice this also affects claims to symbolic capital (patriarchal inheritance, sacred-site narratives) and, in some political contexts, downstream legitimacy arguments about land and peoplehood.
% ABSENT_VOICES: Karaite and other minority Jewish traditions with distinct exclusivity readings are rarely engaged directly; secular historians of the Hijaz and pre-Islamic Arabia who read the Ishmael-Arabia genealogy as a later theological construction rather than a historical transmission are largely absent from confessional debate on either side.
% DISAPPEARANCE_RATIONALE: If the Ishmael covenant reading vanished, Islamic theology's claim to Abrahamic continuity would lose a central textual anchor; pilgrimage narratives tied to Abraham and Ishmael at the Ka'ba, interfaith dialogue framings, and apologetic literature defending Muhammad's prophethood as covenant-fulfillment would all require reconstruction on different grounds — a substantial theological and institutional rearrangement, not a cosmetic one.
% FOUNDING_PROBLEM: Seventh-century Arabia lacked a settled account of how a new prophetic revelation related to the older, already-authoritative Abrahamic scriptural tradition; without a genealogical bridge, the new revelation risked being read as discontinuous with, rather than a corrective continuation of, prior monotheism.
% FOUNDING_PROBLEM_CORROBORATION: Islamic scholarly tradition attests the founding problem as live and resolved by revelation itself (the Qur'an's own claim to confirm and correct prior scripture). Independent historians of religion (outside both Muslim and Jewish confessional communities) corroborate that genealogical covenant claims of this kind commonly emerged to solve exactly this legitimacy problem for new prophetic movements, while noting the historical evidence for a literal unbroken Ishmael-to-Muhammad transmission is itself a matter of faith rather than independent historical record.
narrative_ontology:disappearance_verdict(abrahamic_covenant__ishmael_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__ishmael_covenant_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__ishmael_covenant_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at a moderate 0.42: the reading transfers real interpretive and symbolic legitimacy away from exclusive Jewish custodianship of the covenant narrative, but it does not depend on material coercion — the transfer operates through textual argument, theological education, and institutional teaching, not force. Suppression (0.38) reflects the requirement that Islamic institutions actively maintain and teach the reading against a live rival claim, and that this maintenance work has intensified modestly over the 1400-year interval as interfaith contact and apologetics literature grew. Theater ratio stays low (0.22) because the doctrinal function (grounding Islamic prophetic legitimacy, structuring pilgrimage narrative, organizing communal identity) remains substantively active rather than merely performative. Accessibility collapse is moderate (0.35): a believer raised within the tradition experiences the reading as settled, but the rival readings remain visibly live in interfaith and academic discourse, so alternatives have not collapsed the way they would for a genuine natural law. Resistance is comparatively high (0.62) because Jewish and rabbinic authorities mount sustained, organized theological resistance to this reading — this is a genuinely contested kernel, not a settled claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Islamic scholarly authorities and institutions sit near the beneficiary end: they set the interpretive agenda, and their institutional standing is partly constituted by the reading holding. Muslim believers are also beneficiaries but identity-locked — the belief is not merely useful to them but partially constitutive of communal self-understanding, which is why their exit option is coded identity_locked rather than mobile or arbitrage. Jewish covenantal exclusivists and their institutional defenders sit near the target end: the reading directly displaces their exclusive-inheritance claim, and their exit option is trapped/constrained because abandoning resistance would concede a foundational element of their own covenantal self-understanding. This is not a case of one party being materially coerced by the other; it is a case of competing legitimacy claims over the same textual kernel, where being 'the reading contested by rivals' is itself the cost borne by the payer seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — bridging a new prophetic revelation to prior Abrahamic authority in seventh-century Arabia — is coded contested rather than dead: Islamic tradition holds it permanently live (revelation continually re-grounds itself in the older covenant), while outside historians read the genealogical claim as a theological construction whose original legitimacy-bridging function has been thoroughly absorbed into settled doctrine and now operates mostly as identity infrastructure rather than active argument. This divergence is exactly why founding_problem_status is 'contested' rather than 'dead' — collapsing it to 'dead' would mislabel a still-functioning, actively-taught coordination doctrine as pure legacy extraction; collapsing it to 'live' in an unqualified sense would ignore that the original Arabian legitimacy problem it solved no longer exists in its original historical form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genealogical_claim_historicity,
    'Is the Ishmael-to-Muhammad covenantal transmission a historically continuous lineage claim, or a retrospective theological construction formed to solve a seventh-century legitimacy problem?',
    'Comparative historical-critical scholarship on pre-Islamic Arabian genealogical traditions and their relationship to Qur''anic Ishmael narratives; textual dating of the relevant Genesis and Qur''anic passages; independent (non-confessional) historiography of Abrahamic genealogy claims across late antiquity.',
    'If the claim is a retrospective construction, the reading''s extractiveness toward Jewish institutional authority is better understood as a legitimacy-generation mechanism for a new religious movement rather than a recovery of suppressed textual truth — this would not change ε but would sharpen the omega on beneficiary structure. If genuinely continuous, the reading''s claim to correct rather than compete with the Isaac reading gains independent support.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genealogical_claim_historicity, empirical, 'Whether the Ishmael transmission claim is historically continuous or a later theological construction.').

omega_variable(
    kernel_reading_location_of_disagreement,
    'Where exactly do the Isaac and Ishmael readings diverge in the shared textual kernel — is it a translation/interpretation dispute over Genesis 17:19-21''s exclusivity language, or a disagreement over which extra-biblical prophetic tradition (Qur''an vs. rabbinic Talmud) has interpretive authority over the base text?',
    'Close textual-critical comparison of Genesis 17:19-21 across Masoretic, Qur''anic-referential, and comparative Semitic-philological readings; identification of whether the divergence is located in the base text itself or in which secondary authoritative tradition is permitted to gloss it.',
    'If the divergence is purely textual (a genuine ambiguity in Genesis itself), the two readings genuinely coexist as live alternative interpretations of one ambiguous kernel. If the divergence is really about which secondary tradition (Talmud vs. Qur''an) gets interpretive priority, the dispute is less about the kernel text and more about competing authority structures layered above it — which would sharpen the axiom conflict already declared in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location_of_disagreement, conceptual, 'Whether the Isaac/Ishmael divergence is located in the base text or in competing secondary interpretive authorities.').

omega_variable(
    political_instrumentalization_of_reading,
    'To what extent is the Ishmael covenant reading, in modern usage, instrumentalized to support political claims (e.g., regarding land, sovereignty, or communal legitimacy) beyond its original theological function?',
    'Discourse analysis of contemporary religious-political rhetoric invoking the Ishmael reading in the context of the Israeli-Palestinian dispute, compared against pre-modern theological usage of the same reading.',
    'If substantial modern political instrumentalization exists, the reading''s effective extractiveness in contemporary contexts may be higher than its historically-averaged value suggests, and it would warrant closer linkage to the land_promise_constraint sibling constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_instrumentalization_of_reading, empirical, 'Whether modern political usage has amplified the reading''s extractive function beyond its theological origin.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__ishmael_covenant_reading, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abra_tr_t0, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(abra_tr_t200, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 200, 0.17).
narrative_ontology:measurement(abra_tr_t500, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 500, 0.18).
narrative_ontology:measurement(abra_tr_t800, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 800, 0.2).
narrative_ontology:measurement(abra_tr_t1100, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 1100, 0.21).
narrative_ontology:measurement(abra_tr_t1400, abrahamic_covenant__ishmael_covenant_reading, theater_ratio, 1400, 0.22).

% Extraction over time
narrative_ontology:measurement(abra_be_t0, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(abra_be_t200, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 200, 0.34).
narrative_ontology:measurement(abra_be_t500, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 500, 0.37).
narrative_ontology:measurement(abra_be_t800, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 800, 0.39).
narrative_ontology:measurement(abra_be_t1100, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 1100, 0.4).
narrative_ontology:measurement(abra_be_t1400, abrahamic_covenant__ishmael_covenant_reading, base_extractiveness, 1400, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(abra_su_t0, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(abra_su_t200, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 200, 0.28).
narrative_ontology:measurement(abra_su_t500, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 500, 0.3).
narrative_ontology:measurement(abra_su_t800, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 800, 0.32).
narrative_ontology:measurement(abra_su_t1100, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 1100, 0.35).
narrative_ontology:measurement(abra_su_t1400, abrahamic_covenant__ishmael_covenant_reading, suppression_requirement, 1400, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__ishmael_covenant_reading, identity_coordination).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, isaac_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, christian_supersessionist_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__ishmael_covenant_reading, land_promise_constraint).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the abrahamic_covenant kernel (isaac_covenant_reading, ishmael_covenant_reading, christian_supersessionist_reading), plus a fourth related-but-distinct axis (land_promise_constraint) concerning territorial rather than lineage covenant claims. Each reading carries its own ε: the Isaac reading's extractiveness concerns defense of exclusive inheritance against two rival claims; this Ishmael reading's extractiveness concerns the transfer of interpretive legitimacy toward Islamic institutions; the supersessionist reading's extractiveness (authored separately) concerns transfer toward Christian institutions via a spiritualized rather than genealogical covenant claim. None of these values were averaged or hedged against each other — each was authored independently from its own reading's structural position, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
