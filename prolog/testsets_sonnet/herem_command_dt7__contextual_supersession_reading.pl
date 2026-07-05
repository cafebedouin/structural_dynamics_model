% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__contextual_supersession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__contextual_supersession_reading, []).

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
 *   constraint_id: herem_command_dt7__contextual_supersession_reading
 *   human_readable: Herem Command (Deut 7) — Contextual Supersession Reading
 *   domain: religious/ethical/hermeneutical
 *
 * SUMMARY:
 *   This story instantiates one reading of the herem_command_dt7 kernel: the
 *   contextual supersession reading, which holds that the herem directive in
 *   Deuteronomy 7 was a historically-bounded command addressed to ancient
 *   Israel's settlement circumstances, and that its moral force has been
 *   superseded by later prophetic universalism (Micah 6:8, Isaiah's oracles
 *   against exclusivist violence) and, in Christian tradition, by the
 *   universalist covenant. This reading is authored as a distinct constraint
 *   from its siblings (durable_separation_reading and
 *   allegorical_displacement_reading) — each instantiates a structurally
 *   different constraint with a different beneficiary/victim profile and
 *   different ε, per the ε-invariance principle. The supersession reading is
 *   the mainstream position in most non-fundamentalist denominational
 *   teaching today; its structural effect is to relocate the operative
 *   boundary from ethnicity to consent/belief and to substantially reduce the
 *   constraint's practical extractiveness relative to a literal or
 *   durable-separation reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__contextual_supersession_reading, 0.18).
domain_priors:suppression_score(herem_command_dt7__contextual_supersession_reading, 0.22).
domain_priors:theater_ratio(herem_command_dt7__contextual_supersession_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__contextual_supersession_reading, piton).
narrative_ontology:human_readable(herem_command_dt7__contextual_supersession_reading, "Herem Command (Deut 7) — Contextual Supersession Reading").
narrative_ontology:topic_domain(herem_command_dt7__contextual_supersession_reading, "religious/ethical/hermeneutical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__contextual_supersession_reading, '5f50f76e-b940-472f-902e-a5505629306b').
narrative_ontology:cs_kernel_codification('5f50f76e-b940-472f-902e-a5505629306b', fixed_text).
narrative_ontology:cs_authority_grounding('5f50f76e-b940-472f-902e-a5505629306b', lineage).
narrative_ontology:cs_interpretation_layer_present('5f50f76e-b940-472f-902e-a5505629306b').
narrative_ontology:cs_reading_relation('5f50f76e-b940-472f-902e-a5505629306b', herem_command_dt7__durable_separation_reading, forecloses).
narrative_ontology:cs_reading_relation('5f50f76e-b940-472f-902e-a5505629306b', herem_command_dt7__allegorical_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('5f50f76e-b940-472f-902e-a5505629306b', foundational, herem_historically_bounded_to_settlement_period).
narrative_ontology:cs_axiom_status(herem_historically_bounded_to_settlement_period, holdable).
narrative_ontology:cs_axiom_grounding('5f50f76e-b940-472f-902e-a5505629306b', herem_historically_bounded_to_settlement_period, empirically_contingent).
narrative_ontology:cs_axiom('5f50f76e-b940-472f-902e-a5505629306b', foundational, prophetic_ethics_supersede_conquest_ethics).
narrative_ontology:cs_axiom_status(prophetic_ethics_supersede_conquest_ethics, holdable).
narrative_ontology:cs_axiom_grounding('5f50f76e-b940-472f-902e-a5505629306b', prophetic_ethics_supersede_conquest_ethics, deontological).
narrative_ontology:cs_reference_frame('5f50f76e-b940-472f-902e-a5505629306b', conquest_era_literal_application).
narrative_ontology:cs_drift_state('5f50f76e-b940-472f-902e-a5505629306b', post_enlightenment_prophetic_universalism_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('5f50f76e-b940-472f-902e-a5505629306b', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__contextual_supersession_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, mainline_denominational_leadership).
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, interfaith_and_intermarried_congregants).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, congregants_under_residual_fundamentalist_enforcement).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, progressive_revelation_doctrine).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, prophetic_ethical_universalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teaches that the herem command was a time-bound, situation-specific directive addressed to ancient Israel's settlement crisis and has been morally superseded by later prophetic ethics (Micah, Isaiah) and, in Christian tradition, by the universalist covenant. This reading lets denominations retain the text as scripture while declining to apply its literal mandate — they administer the interpretive move and gain doctrinal flexibility and reduced reputational exposure from it.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, mainline_denominational_leadership, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__contextual_supersession_reading, mainline_denominational_leadership, beneficiary).

% Under a durable-separation reading, their marriages or conversions would sit under a categorical taboo tied to ethnic/religious boundary-keeping. Under this supersession reading, the relevant boundary is relocated from ethnicity to individual consent and belief, removing that pressure. They benefit directly from the reading's narrowing of the command's scope.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, interfaith_and_intermarried_congregants, beneficiary,
    moderate, biographical, mobile, national).

% In congregations or family networks where a minority of leaders reject the supersession reading and continue to apply herem-adjacent separation logic informally (shunning intermarriage, treating outsiders as categorically impure), individuals still bear real social and familial costs. The supersession reading exists at the denominational level but does not reach every local enforcement pocket; these congregants pay where the reading has not actually displaced the older practice.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, congregants_under_residual_fundamentalist_enforcement, payer,
    powerless, biographical, constrained, local).

% Assess the supersession reading against the textual and archaeological record: was Deuteronomy 7 addressed to a bounded historical circumstance, and does the canon actually stage its own supersession through the prophets? They can corroborate or contest the reading from outside the institutions that benefit from adopting it.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, biblical_scholars_historical_critical, observer,
    analytical, civilizational, analytical, global).

% Hold that herem encodes a timeless mandate for bounded identity and categorical separation, not a historically-bounded directive. Within denominational bodies that have adopted the supersession reading as official teaching, their view is treated as a minority or fringe position and is not represented in mainstream doctrinal formation, though it persists in some congregations and movements.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, durable_separation_adherents, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows a religious tradition to retain a violent conquest text as canonical scripture while providing a doctrinally coherent basis for not applying its literal command in the present — coordinating continued reverence for the text with contemporary ethical practice.
% TRANSFER_FUNCTION: Moves interpretive authority from a literal-timeless reading toward denominational leadership and historical-critical scholarship; relocates the practical burden of the command away from ethnic outsiders and toward voluntary belief/consent boundaries, and moves social standing away from adherents of the durable-separation reading within mainstream institutions.
% ABSENT_VOICES: Adherents of the durable-separation reading are largely absent from the rooms where denominational doctrine on this text is set; they would argue the supersession move is theologically unwarranted revisionism, not legitimate development, but are treated as a minority position rather than invited into the doctrinal formation process.
% DISAPPEARANCE_RATIONALE: If the supersession reading vanished from official denominational teaching, most congregants would notice little day-to-day change since the command is not literally enforced in mainstream practice regardless; but institutions would lose their doctrinal justification for de-fanging the text, and durable-separation and allegorical readings would compete more directly for the vacated interpretive space — whether that constitutes 'the world rearranging' is itself disputed between the reading's proponents and its critics.
% FOUNDING_PROBLEM: Modern communities holding Deuteronomy as scripture needed a way to affirm the text's canonical authority while declining to enact herem's literal command against categorical outsiders — the founding problem was reconciling scriptural inheritance with post-Enlightenment and prophetic-universalist ethics.
% FOUNDING_PROBLEM_CORROBORATION: Historical-critical biblical scholars outside denominational leadership corroborate that the text shows internal tension with later prophetic universalist material and that a developmental reading has textual support; some of the same scholars, however, note the supersession reading can be adopted more for its convenient present-day ethics than for exegetical necessity, so corroboration is partial rather than unanimous.
narrative_ontology:disappearance_verdict(herem_command_dt7__contextual_supersession_reading, contested).
narrative_ontology:founding_problem_status(herem_command_dt7__contextual_supersession_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__contextual_supersession_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(herem_command_dt7__contextual_supersession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__contextual_supersession_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__contextual_supersession_reading_tests).
:- end_tests(herem_command_dt7__contextual_supersession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18 at interval end, declining from 0.28) because the reading actively dissolves the command's application to present-day outsiders — there is little ongoing extraction once the boundary is relocated to voluntary belief. Suppression is low-moderate (0.22): almost no institutional coercion enforces the superseded reading's alternative (durable separation) out of existence, though social pressure within some communities persists. Theater ratio is moderate and rising (0.30 to 0.42) because as literal application recedes, an increasing share of the doctrinal activity around this text is performative — sermons, catechetical material, and apologetics addressing 'what to do with the violent texts' function more to reassure congregants that the tradition is ethically coherent than to resolve any live practical dispute, since the command was not being enacted anyway. Accessibility collapse is low (0.25): the durable-separation and allegorical readings remain fully available and are actively held by identifiable communities. Resistance is moderate (0.55): durable-separation adherents and biblical literalists actively contest the supersession move as illegitimate revisionism.
 *
 * DIRECTIONALITY LOGIC:
 *   Denominational leadership is the agenda-setter and structural beneficiary — it administers the interpretive transition and gains doctrinal flexibility. Interfaith and intermarried congregants are direct beneficiaries: the reading removes a categorical taboo that would otherwise apply to them under a durable-separation reading. The narrow victim group — congregants under residual fundamentalist enforcement — bears cost specifically because the supersession reading has not fully displaced older practice at the local level; they are targets of a still-operating enforcement pocket, not of the mainstream reading itself. This matches the expected structural delta: low extractiveness on intermarriage and a narrow victim set limited to residual enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling scriptural inheritance with post-Enlightenment/prophetic ethics) is still live in the sense that communities still hold the text as canon and still need an account of it, but the specific application the command once had (literal conquest-era ethnic separation) is dead in virtually all mainstream practice. The piton classification reflects this: the interpretive apparatus around the text persists and has hardened into routine teaching (rising theater_ratio) even though the practical extraction it once justified has largely evaporated. This is not a mandate that outlived its function in the extractive sense — the classification instead flags that a large teaching apparatus is now devoted to a problem (how to read the violent text ethically) whose sharpest edge (actual application) no longer exists for the overwhelming majority of adherents, while a genuine residual pocket of enforcement (the named victim group) shows the supersession has not universally landed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supersession_vs_original_intent,
    'Does the biblical canon itself stage a genuine ethical development that renders herem''s application obsolete, or is ''supersession'' a retrospective imposition of modern universalist ethics onto a text that intended timeless application?',
    'Historical-critical and canonical analysis of whether prophetic texts (Micah, Isaiah, Jonah) explicitly revise or merely extend the conquest-narrative ethic; comparative analysis of how the herem material was read by intervening interpretive communities (Second Temple Judaism, rabbinic tradition, patristic exegesis) prior to modern universalism.',
    'If the canon does not itself stage the supersession, this reading is a modern ethical overlay rather than an internal textual development, which would weaken its claim to be discovering rather than constructing the moral boundary — shifting the reading''s status from ''live scholarly consensus'' toward ''contested doctrinal innovation.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supersession_vs_original_intent, conceptual, 'Whether supersession is textually internal or a retrospective ethical overlay.').

omega_variable(
    residual_enforcement_scope,
    'How widespread is enforcement of durable-separation-style boundary practices (against intermarriage, conversion, or outsider inclusion) within communities that officially teach the supersession reading?',
    'Survey and ethnographic data on congregational practice versus official denominational doctrine, tracking gaps between stated teaching and lived enforcement.',
    'A wide gap would mean the narrow victim set declared here understates actual harm and the piton''s theater_ratio is higher than authored; a narrow gap would confirm the supersession reading has substantially displaced practice, not just doctrine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(residual_enforcement_scope, empirical, 'Whether official supersession teaching actually displaces local separation practice.').

omega_variable(
    committer_framing_alternative,
    'Could this constraint instead be framed as a reading of moral progress in revelation generally (progressive revelation doctrine) rather than as a reading specifically of the herem_command_dt7 kernel?',
    'Compare classification outcomes if the kernel were redefined at the level of ''biblical violence texts generally'' versus the narrower Deuteronomy 7 text; assess whether beneficiary/victim structure and extractiveness shift materially.',
    'A broader kernel framing would pull in additional violent commands (e.g., Amalek narrative, conquest of Canaan generally) and could change the victim set and extractiveness profile; the narrower framing chosen here keeps ε stable and specific to Deuteronomy 7''s herem command, consistent with the ε-invariance principle, but the choice of kernel boundary is itself a judgment call documented here as Ω_C.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_alternative, conceptual, 'Whether the kernel boundary (Deut 7 specifically vs. biblical violence broadly) is the only defensible framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__contextual_supersession_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__contextual_supersession_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(here_tr_t10, herem_command_dt7__contextual_supersession_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(here_tr_t20, herem_command_dt7__contextual_supersession_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(here_tr_t30, herem_command_dt7__contextual_supersession_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(here_tr_t40, herem_command_dt7__contextual_supersession_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(here_tr_t50, herem_command_dt7__contextual_supersession_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement(here_tr_t60, herem_command_dt7__contextual_supersession_reading, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(here_be_t10, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement(here_be_t20, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(here_be_t30, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 30, 0.2).
narrative_ontology:measurement(here_be_t40, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 40, 0.19).
narrative_ontology:measurement(here_be_t50, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 50, 0.185).
narrative_ontology:measurement(here_be_t60, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 60, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(herem_command_dt7__contextual_supersession_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__contextual_supersession_reading, identity_coordination).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, herem_command_dt7__durable_separation_reading).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, herem_command_dt7__allegorical_displacement_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language label 'the meaning of herem in Deuteronomy 7,' per the ε-invariance principle. The contextual_supersession_reading (this file) has low, declining extractiveness and a narrow victim set limited to residual fundamentalist enforcement. The durable_separation_reading (sibling) carries high extractiveness and a wide victim set (all intermarriage/conversion cases treated as categorical boundary violations). The allegorical_displacement_reading (sibling) approaches mountain-like negligible extraction since it removes ethnic referents entirely. The three are linked here and in each sibling's own network.affects_constraints; they must not be merged into one constraint with an averaged ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
