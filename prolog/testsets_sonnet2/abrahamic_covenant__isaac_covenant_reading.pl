% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__isaac_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: abrahamic_covenant__isaac_covenant_reading
 *   human_readable: Isaac-Exclusive Covenant Reading (Genesis 17:19-21)
 *   domain: religious/institutional
 *
 * SUMMARY:
 *   This constraint isolates the specific reading of Genesis 17:19-21 that
 *   holds the Abrahamic covenant transmits exclusively through Isaac, with
 *   Ishmael explicitly and permanently excluded from covenantal inheritance
 *   even while receiving a separate blessing of nationhood. This is one of
 *   three structurally distinct readings of a single contested kernel (the
 *   Abrahamic covenant): the Ishmael-inclusive reading (routing covenant
 *   through Ishmael to Muhammad), and the land-promise reading (concerning
 *   the territorial grant's conditionality) are separate constraints with
 *   their own ε values, authored as siblings, not folded into this one. The
 *   Isaac-exclusive reading functions within Jewish tradition as a genuine
 *   coordination device — it settles a question that would otherwise fracture
 *   communal self-understanding — while simultaneously operating as an
 *   extractive boundary against Ishmaelite and later Islamic claimants who
 *   have no standing within the interpretive community that ratifies the
 *   reading.
 *
 * KEY AGENTS:
 *   - rabbinic_jewish_institutional_continuity: institutional agenda-setter and interpretive authority, transmits and enforces the reading
 *   - israelite_lineage_claimants: beneficiary group, receives settled covenantal identity
 *   - ishmaelite_lineage_claimants: payer, textually named and excluded, no interpretive standing
 *   - islamic_prophetic_tradition: payer, institutional-scale tradition whose founding lineage claim is foreclosed by this specific reading
 *   - comparative_religion_scholars: analytical observer of reception history across traditions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__isaac_covenant_reading, 0.62).
domain_priors:suppression_score(abrahamic_covenant__isaac_covenant_reading, 0.58).
domain_priors:theater_ratio(abrahamic_covenant__isaac_covenant_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__isaac_covenant_reading, tangled_rope).
narrative_ontology:human_readable(abrahamic_covenant__isaac_covenant_reading, "Isaac-Exclusive Covenant Reading (Genesis 17:19-21)").
narrative_ontology:topic_domain(abrahamic_covenant__isaac_covenant_reading, "religious/institutional").

domain_priors:requires_active_enforcement(abrahamic_covenant__isaac_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__isaac_covenant_reading, '155952c0-e162-4e8c-9621-bb9bfedf3026').
narrative_ontology:cs_kernel_codification('155952c0-e162-4e8c-9621-bb9bfedf3026', fixed_text).
narrative_ontology:cs_authority_grounding('155952c0-e162-4e8c-9621-bb9bfedf3026', lineage).
narrative_ontology:cs_interpretation_layer_present('155952c0-e162-4e8c-9621-bb9bfedf3026').
narrative_ontology:cs_reading_relation('155952c0-e162-4e8c-9621-bb9bfedf3026', abrahamic_covenant__ishmael_covenant_reading, forecloses).
narrative_ontology:cs_reading_relation('155952c0-e162-4e8c-9621-bb9bfedf3026', abrahamic_covenant__christian_supersessionist_reading, coexists_with).
narrative_ontology:cs_axiom('155952c0-e162-4e8c-9621-bb9bfedf3026', foundational, isaac_line_exclusive_covenantal_election).
narrative_ontology:cs_axiom_status(isaac_line_exclusive_covenantal_election, holdable).
narrative_ontology:cs_axiom_grounding('155952c0-e162-4e8c-9621-bb9bfedf3026', isaac_line_exclusive_covenantal_election, conventional).
narrative_ontology:cs_axiom('155952c0-e162-4e8c-9621-bb9bfedf3026', secondary, genesis_17_textual_literalism_on_lineage).
narrative_ontology:cs_axiom_status(genesis_17_textual_literalism_on_lineage, holdable).
narrative_ontology:cs_axiom_grounding('155952c0-e162-4e8c-9621-bb9bfedf3026', genesis_17_textual_literalism_on_lineage, conventional).
narrative_ontology:cs_reference_frame('155952c0-e162-4e8c-9621-bb9bfedf3026', rabbinic_masoretic_covenant_transmission).
narrative_ontology:cs_drift_state('155952c0-e162-4e8c-9621-bb9bfedf3026', contemporary_interfaith_and_political_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('155952c0-e162-4e8c-9621-bb9bfedf3026', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, rabbinic_jewish_institutional_continuity).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, israelite_lineage_claimants).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, ishmaelite_lineage_claimants).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, islamic_prophetic_tradition).
narrative_ontology:constraint_vindicates(abrahamic_covenant__isaac_covenant_reading, isaac_line_covenantal_legitimacy).
narrative_ontology:constraint_vindicates(abrahamic_covenant__isaac_covenant_reading, textual_literalism_of_genesis_17).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains and transmits the textual and interpretive tradition holding that the covenant of Genesis 17:19-21 passes exclusively through Isaac. Rabbinic authorities canonize this reading in liturgy, halakha, and communal identity; the reading underwrites claims to unbroken covenantal continuity and to the land promise attached to it. Exit from the reading would mean exit from the tradition's self-understanding.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, rabbinic_jewish_institutional_continuity, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__isaac_covenant_reading, rabbinic_jewish_institutional_continuity, beneficiary).

% Jewish communities and individuals whose religious identity and communal belonging are constituted by descent-through-Isaac narrative. They receive standing as covenant heirs, access to associated ritual and legal frameworks, and a settled account of chosenness that does not require contesting Ishmael's status directly since the text is read as already resolving it.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, israelite_lineage_claimants, beneficiary,
    organized, generational, identity_locked, global).

% Communities and traditions tracing descent or spiritual lineage through Ishmael. Under this reading their line is textually named and then explicitly set outside the covenant of promise (though blessed separately per Genesis 17:20). They cannot exit the constraint because it operates on a shared founding text they do not control the canonical interpretation of; the exclusion is asserted about them, not negotiated with them.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, ishmaelite_lineage_claimants, payer,
    organized, civilizational, trapped, global).

% A civilizational tradition that affirms Abrahamic descent through Ishmael as prophetically significant and culminating in Muhammad. This reading of Genesis positions that tradition's foundational lineage claim as textually foreclosed within the Isaac-exclusive frame, even though Islamic tradition holds its own scripture and interpretive authority and is not dependent on rabbinic ratification. Its constraint is reputational and interfaith rather than material: the Isaac-exclusive reading is cited against it in interfaith and polemical contexts.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, islamic_prophetic_tradition, payer,
    institutional, civilizational, constrained, global).

% Hold a third reading (spiritual/typological fulfillment of the covenant transferred to the Church) that neither affirms the Isaac-exclusive genealogical reading nor the Ishmael-inclusive one. They are not a party to this specific reading's contest but are affected by its persistence, since a strict literal-lineage frame competes with their typological one. Not represented in this reading's own interpretive community.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, christian_supersessionist_communities, excluded,
    institutional, civilizational, constrained, global).

% Study the textual history of Genesis 17, the redaction layers, and the divergent reception histories across Jewish, Christian, and Islamic traditions. They can describe how each tradition's canonical reading serves that tradition's institutional continuity without adjudicating theological truth.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, comparative_religion_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate answer to 'who are the covenant people' that lets a religious community organize law, ritual calendar, land claims, and communal boundaries around a single unambiguous lineage rather than a permanently contested one.
% TRANSFER_FUNCTION: Moves interpretive legitimacy, claim to covenantal land promise, and status as 'chosen line' to Isaac's descendants and away from Ishmael's descendants and traditions built on Ishmael's prophetic significance, using the same founding text as the instrument of both grants.
% ABSENT_VOICES: Ishmaelite claimants and, historically, the Islamic tradition that would later formalize an alternative reading are not party to the rabbinic canonization process that fixed this interpretation; the text is read and ratified within one interpretive community while its exclusionary force operates on communities outside that community's authority.
% DISAPPEARANCE_RATIONALE: If the Isaac-exclusive reading were not authoritative within Jewish tradition, claims to exclusive covenantal continuity, some strands of religious-nationalist land-promise argument, and certain interfaith polemical arguments against Islamic prophetic legitimacy would lose a textual anchor; Jewish identity would need to rest on other grounds (peoplehood, law, practice) rather than exclusive genealogical covenant-election, and interfaith argument over Abrahamic legitimacy would shift register.
% FOUNDING_PROBLEM: Ancient Israelite tradition needed to explain and legitimate its own distinct communal identity and territorial claims against surrounding peoples by grounding them in a divinely sanctioned, textually fixed lineage rather than in contestable political or genealogical fact.
% FOUNDING_PROBLEM_CORROBORATION: Historical-critical biblical scholars (outside both the rabbinic and Islamic interpretive communities) attest that the Genesis 17 text itself contains layered promises to both lines and that the exclusive reading is a later interpretive choice rather than a self-evident textual fact; Islamic tradition independently attests a rival reading of the same events. No party outside the beneficiary tradition corroborates the exclusivity claim as textually necessitated rather than interpretively chosen.
narrative_ontology:disappearance_verdict(abrahamic_covenant__isaac_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__isaac_covenant_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__isaac_covenant_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(abrahamic_covenant__isaac_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__isaac_covenant_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.62) reflects that the reading actively forecloses a rival lineage claim using the same text that grounds the beneficiary's own legitimacy — this is not neutral scriptural description but a boundary-drawing act with real consequences for out-group standing in interfaith and political contexts. Suppression (0.58) is substantial but not maximal: the exclusion operates through canonical authority and communal identity formation rather than through direct coercive enforcement against Ishmaelite communities, who retain their own independent scriptural and interpretive traditions unconstrained by rabbinic ratification. Accessibility collapse (0.6) is moderate-high within the rabbinic interpretive community (the reading is close to unquestionable there) but far lower globally, since Islamic tradition maintains a fully developed counter-reading. Resistance (0.55) is real and organized, coming from an entire civilizational tradition with its own textual authority, not from isolated dissenters.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic Jewish institutional continuity and israelite lineage claimants sit near the beneficiary end: the reading is authored, maintained, and ratified within their interpretive community and grounds settled communal identity and land-promise claims. Ishmaelite lineage claimants and Islamic prophetic tradition sit near the target end: they are named within the very text used to exclude them, have no vote in the canonization process, and bear the reputational and interfaith cost of a fixed exclusion. Their exit options differ importantly: Ishmaelite claimants as a category are trapped (they cannot escape being the excluded term in someone else's founding text), while Islamic tradition as an institutional civilization has constrained rather than trapped exit — it holds independent scriptural authority and simply operates in a world where a rival tradition's canonical reading forecloses its own lineage narrative.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (an ancient community's need for a determinate identity-grounding narrative distinct from surrounding peoples) is contested rather than dead: Jewish communal identity today rests on far more than this single genealogical claim (law, practice, language, shared history), yet the covenant-exclusivity reading persists with its full boundary-drawing force in liturgy and theology, and increasingly in religious-nationalist political argument, long after the original tribal-distinction function has been supplemented by other identity anchors. This is precisely the tangled-rope signature: real coordination function for the in-group persists alongside asymmetric extraction from the out-group through the same textual mechanism, and the classification prevents both over-reading it as pure ancient tribal bookkeeping (ignoring the ongoing extractive use) and under-reading it as pure modern-invented extraction (ignoring the genuine ancient coordination need it once and still partly serves).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_determinacy_of_exclusion,
    'Does Genesis 17:19-21 itself textually necessitate the exclusive reading, or is exclusivity an interpretive choice layered onto a text that could support inclusive readings (as Islamic tradition holds)?',
    'Historical-critical and comparative philological analysis of the Hebrew text, its redaction history, and its earliest reception across Second Temple Jewish, early Christian, and pre-Islamic Arabian traditions, cross-checked against the interpretive moves each later tradition makes.',
    'If the text is genuinely indeterminate and exclusivity is a later interpretive ratification serving communal boundary-maintenance, the constraint is better understood as substantially constructed rather than naturally read off scripture, sharpening the tangled-rope classification. If the text is more determinately exclusive than comparative readings suggest, the coordination function is closer to textually mandated and the extraction component is correspondingly smaller.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_determinacy_of_exclusion, conceptual, 'Whether the exclusive reading is textually necessitated or interpretively constructed.').

omega_variable(
    kernel_framing_choice,
    'Is the more analytically important framing of this kernel the genealogical-legitimacy axis (Isaac vs. Ishmael as covenant heir) or the interpretive-authority axis (who has standing to canonize a reading of a shared founding text)?',
    'Compare classification outcomes under each framing: the genealogical framing centers beneficiary/victim lineage groups (as authored here); an authority-centered framing would instead center which institution''s canonization process is treated as dispositive, potentially shifting the primary victim from ''Ishmaelite claimants'' to ''traditions lacking canonization power over a shared text.''',
    'Under the genealogical framing (adopted here), the constraint reads as tangled_rope with named lineage-group victims. Under an authority-centered framing, the same facts could be read as a broader constraint about scriptural canonization power, with a different and larger victim class (any tradition lacking ratification authority over the shared text) and a different classification profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Alternative framings of the kernel contest (genealogical vs. interpretive-authority) and their effect on classification.').

omega_variable(
    modern_political_amplification,
    'How much of the constraint''s present-day extractiveness derives from the ancient interpretive tradition itself versus modern political use of covenant-exclusivity arguments in territorial and national-identity disputes?',
    'Trace citation and argumentative use of the Isaac-exclusive reading across historical periods, comparing theological usage in classical rabbinic sources against usage in 19th-20th century religious-nationalist and contemporary political discourse.',
    'If modern political use accounts for most of the measured extractiveness increase, the temporal drift reflects instrumentalization of an old reading for new ends rather than the reading itself intensifying, which would matter for any remedy analysis distinguishing theological reform from political dispute resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modern_political_amplification, empirical, 'Whether rising extractiveness reflects ancient interpretation or modern political instrumentalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__isaac_covenant_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abra_tr_t0, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(abra_tr_t20, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(abra_tr_t40, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 40, 0.14).
narrative_ontology:measurement(abra_tr_t60, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 60, 0.16).
narrative_ontology:measurement(abra_tr_t80, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 80, 0.18).
narrative_ontology:measurement(abra_tr_t100, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(abra_be_t0, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(abra_be_t20, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 20, 0.46).
narrative_ontology:measurement(abra_be_t40, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(abra_be_t60, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 60, 0.56).
narrative_ontology:measurement(abra_be_t80, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 80, 0.6).
narrative_ontology:measurement(abra_be_t100, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 100, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(abra_su_t0, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(abra_su_t20, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(abra_su_t40, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement(abra_su_t60, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 60, 0.5).
narrative_ontology:measurement(abra_su_t80, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 80, 0.55).
narrative_ontology:measurement(abra_su_t100, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__isaac_covenant_reading, identity_coordination).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, ishmael_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, land_promise_constraint).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the abrahamic_covenant kernel. isaac_covenant_reading and ishmael_covenant_reading share the same founding text (Genesis 17) but diverge on whether the covenant is exclusive or inclusive of Ishmael's line — they are not the same constraint measured two ways; each has its own ε, beneficiary/victim structure, and classification, linked here rather than merged. land_promise_constraint is downstream of both: whichever lineage reading is adopted structurally conditions which communities' territorial claims the land promise is read to support.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
