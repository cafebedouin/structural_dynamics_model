% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__religious_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__religious_covenant_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: jewish_self_determination__religious_covenant_reading
 *   human_readable: Divine Covenant Mandate for Jewish Territorial Sovereignty
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint story captures the religious covenant reading of Jewish
 *   self-determination: the claim that Jewish sovereignty over the Land of
 *   Israel (Eretz Yisrael) derives from an eternal divine covenant (Brit)
 *   with the patriarchs and at Sinai, making territorial control a religious
 *   obligation (mitzvah) that supersedes secular politics, international law,
 *   and democratic procedure. The reading presents itself as a mountain —
 *   divine command as immutable natural law — but operates as a tangled rope:
 *   religious authority (rabbinate, settler movement) is entangled with state
 *   power (IDF, government, legal system), requires active enforcement
 *   (military occupation, legal suppression of Palestinian claims), and
 *   extracts land, resources, and rights from Palestinians and secular
 *   Israelis. The claim/metric gap is structural: the reading claims zero
 *   extraction (divine command is absolute, not extractive), while the
 *   authored metrics describe the actual operation of the constraint in the
 *   world — high extraction, high suppression, rising theater ratio as the
 *   religious rationale increasingly covers a secular-political settlement
 *   project.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, 0.75).
domain_priors:suppression_score(jewish_self_determination__religious_covenant_reading, 0.85).
domain_priors:theater_ratio(jewish_self_determination__religious_covenant_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__religious_covenant_reading, mountain).
narrative_ontology:human_readable(jewish_self_determination__religious_covenant_reading, "Divine Covenant Mandate for Jewish Territorial Sovereignty").
narrative_ontology:topic_domain(jewish_self_determination__religious_covenant_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__religious_covenant_reading).
domain_priors:emerges_naturally(jewish_self_determination__religious_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__religious_covenant_reading, 'b8504833-2970-4954-91e6-f0de178d2dcd').
narrative_ontology:cs_kernel_codification('b8504833-2970-4954-91e6-f0de178d2dcd', fixed_text).
narrative_ontology:cs_authority_grounding('b8504833-2970-4954-91e6-f0de178d2dcd', extraction).
narrative_ontology:cs_interpretation_layer_present('b8504833-2970-4954-91e6-f0de178d2dcd').
narrative_ontology:cs_reading_relation('b8504833-2970-4954-91e6-f0de178d2dcd', jewish_self_determination__diasporist_reading, forecloses).
narrative_ontology:cs_reading_relation('b8504833-2970-4954-91e6-f0de178d2dcd', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('b8504833-2970-4954-91e6-f0de178d2dcd', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('b8504833-2970-4954-91e6-f0de178d2dcd', jewish_self_determination__settler_colonial_reading, forecloses).
narrative_ontology:cs_axiom('b8504833-2970-4954-91e6-f0de178d2dcd', foundational, divine_covenant_mandates_territorial_sovereignty).
narrative_ontology:cs_axiom_status(divine_covenant_mandates_territorial_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('b8504833-2970-4954-91e6-f0de178d2dcd', divine_covenant_mandates_territorial_sovereignty, theological).
narrative_ontology:cs_axiom('b8504833-2970-4954-91e6-f0de178d2dcd', foundational, secular_political_frameworks_subordinate_to_divine_command).
narrative_ontology:cs_axiom_status(secular_political_frameworks_subordinate_to_divine_command, holdable).
narrative_ontology:cs_axiom_grounding('b8504833-2970-4954-91e6-f0de178d2dcd', secular_political_frameworks_subordinate_to_divine_command, theological).
narrative_ontology:cs_axiom('b8504833-2970-4954-91e6-f0de178d2dcd', secondary, rabbinic_interpretation_is_authoritative_mediator_of_covenant).
narrative_ontology:cs_axiom_status(rabbinic_interpretation_is_authoritative_mediator_of_covenant, holdable).
narrative_ontology:cs_axiom_grounding('b8504833-2970-4954-91e6-f0de178d2dcd', rabbinic_interpretation_is_authoritative_mediator_of_covenant, theological).
narrative_ontology:cs_reference_frame('b8504833-2970-4954-91e6-f0de178d2dcd', sinaitic_covenant_land_mandate).
narrative_ontology:cs_drift_state('b8504833-2970-4954-91e6-f0de178d2dcd', post_1967_settler_movement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b8504833-2970-4954-91e6-f0de178d2dcd', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__religious_covenant_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, religious_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, settlement_enterprise).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, rabbinic_authority_structure).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, palestinian_population).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, secular_israeli_citizens).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, international_diplomatic_framework).
narrative_ontology:constraint_vindicates(jewish_self_determination__religious_covenant_reading, divine_promise_to_abraham).
narrative_ontology:constraint_vindicates(jewish_self_determination__religious_covenant_reading, sinaitic_covenant_land_mandate).
narrative_ontology:constraint_vindicates(jewish_self_determination__religious_covenant_reading, jewish_theological_centrality_of_eretz_yisrael).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Political-religious movement (Gush Emunim, Religious Zionist Party, settler rabbinate) that advances settlement as divine obligation. Sets the theological-political agenda, controls settlement policy through coalition leverage, and receives state resources for yeshivot and settlement infrastructure. Exit from the framework means abandoning core religious identity and messianic vocation.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, religious_zionist_movement, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__religious_covenant_reading, religious_zionist_movement, beneficiary).

% Network of settlements, outposts, regional councils, and associated economic enterprises (agriculture, tourism, construction) in the West Bank. Receives state funding, military protection, and legal legitimation. Depends on the divine covenant claim for its legitimacy; without it, the enterprise is exposed as illegal under international law.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, settlement_enterprise, beneficiary,
    organized, biographical, constrained, regional).

% Chief Rabbinate, yeshiva heads, and settler rabbis who interpret the covenant as mandating maximal territorial control. Their interpretive authority over Jewish law derives from the claim that divine will is mediated through them. They administer conversion, marriage, and burial, and their rulings on land (e.g., prohibition on ceding territory) bind the religious public and influence state policy.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, rabbinic_authority_structure, agenda_setter,
    institutional, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__religious_covenant_reading, rabbinic_authority_structure, beneficiary).

% Subject to military occupation, land expropriation, movement restrictions, and denial of political rights justified by the divine claim. The covenant narrative renders their presence and claims religiously illegitimate. No exit from the framework: they cannot accept the religious premise without surrendering their own national/religious identity, and physical exit is blocked.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, palestinian_population, payer,
    powerless, generational, trapped, regional).

% Bear the costs of settlement (taxes, military service, international isolation, democratic erosion) without sharing the theological commitment. Subject to religious law in personal status (marriage, divorce, burial) because the covenant claim underwrites rabbinic monopoly. Exit options: emigration (costly), political opposition (increasingly marginalized), or acquiescence.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, secular_israeli_citizens, payer,
    moderate, biographical, constrained, national).

% UN, ICC, ICJ, and major powers operating on international law (Fourth Geneva Convention, self-determination, prohibition on acquisition of territory by force). The divine covenant claim is structurally incommensurable with this framework — it cannot be negotiated, only rejected or ignored. Excluded from the religious framework's internal logic; their objections are categorically dismissed as irrelevant to divine mandate.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, international_diplomatic_framework, excluded,
    institutional, biographical, analytical, global).

% Israeli Jews who support Jewish self-determination on national/historical grounds but reject the divine covenant as political mandate. They see the constraint as a threat to democracy and international legitimacy. They have exit options (emigration, political disengagement) but remain analytically engaged.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, liberal_zionist_opposition, observer,
    moderate, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish collective life around an absolute, non-negotiable divine mandate for territorial sovereignty; provides a legitimacy source that cannot be voted away, compromised, or delegitimized by secular actors.
% TRANSFER_FUNCTION: Transfers territorial control and political authority from Palestinian inhabitants and international legal frameworks to the Jewish settlement enterprise; transfers state resources (land, water, military protection, funding) to religious-Zionist institutions; transfers democratic decision-making over territory to rabbinic authority.
% ABSENT_VOICES: Palestinian voices (excluded by the religious framework's categorical denial of their legitimate claim); secular Israeli voices (marginalized as 'Hellenizers' lacking authentic Jewish commitment); international legal voices (dismissed as gentile nations opposing God's will). The religious framework has no structural mechanism to hear them — their exclusion is constitutive.
% DISAPPEARANCE_RATIONALE: If the divine covenant claim vanished overnight, the settlement enterprise would lose its foundational legitimacy, the rabbinic authority's political mandate would collapse, the Israeli right's coalition logic would fracture, and the entire architecture of occupation would face immediate legitimization crisis. The Palestinian national movement would gain unprecedented diplomatic leverage. The Israeli state would be forced to confront the occupation as a purely secular-political problem.
% FOUNDING_PROBLEM: The problem of Jewish existential vulnerability in exile: statelessness, persecution, and the inability to fulfill the divine mandate of collective life in the land. The covenant claim was mobilized in the late 19th/early 20th century (Rabbi Kook, religious Zionism) as a theological answer to secular Zionism's nationalist solution — making return to the land not a political choice but a religious obligation.
% FOUNDING_PROBLEM_CORROBORATION: Religious Zionist rabbis (e.g., Rabbi Kook lineage, settler rabbinate) attest the founding problem is live — redemption is incomplete until full sovereignty over biblical borders. Historians (e.g., Gideon Aran, Aviezer Ravitzky) document that religious Zionism emerged as a specific 20th-century response to secular nationalism and the Holocaust, not as a continuous uninterrupted tradition. Palestinian historians (e.g., Rashid Khalidi) and international lawyers attest the 'problem' was never recognized as legitimating displacement. No corroborating source outside the beneficiary set accepts the divine mandate as a valid political claim.
narrative_ontology:disappearance_verdict(jewish_self_determination__religious_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__religious_covenant_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__religious_covenant_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_self_determination__religious_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__religious_covenant_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__religious_covenant_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, ExtMetricName, E),
    domain_priors:suppression_score(jewish_self_determination__religious_covenant_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(jewish_self_determination__religious_covenant_reading),
    narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(jewish_self_determination__religious_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because the constraint operates through state power to transfer land and resources from Palestinians to settlers, and to subordinate secular law to rabbinic authority. Suppression is very high (0.85) because the constraint's persistence depends on military enforcement, legal exclusion of Palestinian claims, and delegitimization of dissenting Jewish voices. Theater ratio is moderate (0.4) — the religious study, settlement rituals, and messianic discourse are genuine coordination for believers, but a growing share of enforcement activity serves the settlement enterprise rather than the theological claim. Accessibility collapse is very high (0.9) within the religious framework: once the covenant is accepted as binding, no alternative territorial compromise is thinkable. Resistance is high (0.8) from Palestinian national movement, international law, and Israeli democracy advocates. The measurement series tracks the constraint's intensification from 1967 (Six-Day War, beginning of settlement) to 2024 (maximalist government, judicial overhaul, ICJ proceedings).
 *
 * PERSPECTIVAL GAP:
 *   From the religious Zionist seat (agenda_setter/beneficiary, identity_locked), the constraint is a mountain — divine will made manifest, zero extraction, absolute coordination. From the Palestinian seat (payer, trapped), it is a snare — pure extraction enforced by military power, no coordination function for them. From the secular Israeli seat (payer, constrained), it is a tangled rope — they get some coordination (Jewish state existence) but pay extraction (occupation costs, democratic erosion) without consent. From the international seat (excluded, analytical), it is an incoherent claim — neither mountain nor rope, just a categorically different legitimacy framework. The engine computes this divergence; the authored claim (mountain) reflects only the beneficiary seat's self-understanding.
 *
 * DIRECTIONALITY LOGIC:
 *   The religious Zionist movement and rabbinic authority are structural beneficiaries (d near 0) — they set the agenda, collect state resources, and their identity is fused with the constraint (identity_locked exit). The settlement enterprise is a beneficiary with constrained exit (organized power, biographical horizon). Palestinians are full targets (d near 1, trapped, powerless) — they bear the extraction with no voice in the framework. Secular Israelis are payers with constrained exit (moderate power, biographical horizon) — they fund and fight for a project they don't theologically own. The international framework is excluded (analytical exit) — it observes but cannot influence the internal logic. The engine will compute per-seat types from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jewish vulnerability in exile) is contested as live vs dead. The religious Zionist reading claims it remains live (redemption incomplete). Historians and Palestinian voices say the problem was solved by state creation in 1948, and the constraint now serves a different function (settler colonialism). The mandate has atrophied for the secular majority but intensified for the religious minority that captured state institutions. This is not a piton (inertial decay) — it is actively intensifying (rising extraction, suppression). The constraint persists not by inertia but by active theological-political mobilization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_vs_tangled_rope_ambiguity,
    'Is the divine covenant claim a genuine mountain (immutable theological fact) or a tangled rope (religious authority entangled with state power extracting from Palestinians)?',
    'Test: if the religious framework were decoupled from state power (no state funding for settlements, no military enforcement of rabbinic land rulings, no rabbinic monopoly on personal status), would the claim still operate as a coordination mechanism for believers? If yes, mountain coordination persists; if the settlement enterprise collapses without state enforcement, the extraction was the point.',
    'If mountain: the constraint''s extraction is an epiphenomenon of state implementation, not the covenant itself. If tangled rope: the religious claim is the legitimating cover for an extraction machine. FSM (false summit mountain) signature would trigger given declared beneficiaries on a mountain claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_vs_tangled_rope_ambiguity, conceptual, 'Whether the mountain claim is a false summit masking tangled rope operation.').

omega_variable(
    committer_structure_kernel_reading,
    'How does this reading''s structural relationship to the kernel jewish_self_determination differ from sibling readings, and where is the disagreement located?',
    'Map each sibling reading''s beneficiary/victim structure, claimed type, and epsilon referent. The disagreement is located in: (1) the kernel''s codification (fixed divine text vs. historical/national/colonial fact), (2) authority grounding (lineage/extraction vs. expertise/practice/distributed), (3) the axiom_overriding drift direction (authority_erosion vs. revival_pressure vs. repudiation_pressure).',
    'If this reading forecloses siblings (as declared), the kernel cannot be resolved by consensus — it requires structural displacement. If siblings coexist, the kernel is a permanent contested field. The engine''s cs_axiom_contradiction will compute foreclosure from the declared axioms and drift states.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Committer-frame structural positioning of this reading within the kernel.').

omega_variable(
    internalized_suppression_palestinians,
    'Is Palestinian suppression primarily structural (military, legal) or partly internalized (acceptance of religious framework''s legitimacy, theological resignation)?',
    'Post-exit suppression trajectory: if a Palestinian population were granted full rights and the occupation ended, would the religious claim''s psychological grip persist? Surveys of Palestinian attitudes to Jewish religious claims; theological engagement patterns.',
    'If internalized suppression is significant, the constraint''s effective suppression exceeds the structural measure — the target carries the suppression internally. This affects the omega variable for suppression mechanism ambiguity and the directionality derivation for identity_locked agents.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_suppression_palestinians, empirical, 'Structural vs. internalized suppression mechanism for Palestinian population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__religious_covenant_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jsd_rc_tr_t1967, jewish_self_determination__religious_covenant_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(jsd_rc_tr_t1977, jewish_self_determination__religious_covenant_reading, theater_ratio, 1977, 0.2).
narrative_ontology:measurement(jsd_rc_tr_t1987, jewish_self_determination__religious_covenant_reading, theater_ratio, 1987, 0.25).
narrative_ontology:measurement(jsd_rc_tr_t1993, jewish_self_determination__religious_covenant_reading, theater_ratio, 1993, 0.3).
narrative_ontology:measurement(jsd_rc_tr_t2000, jewish_self_determination__religious_covenant_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(jsd_rc_tr_t2010, jewish_self_determination__religious_covenant_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(jsd_rc_tr_t2024, jewish_self_determination__religious_covenant_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(jsd_rc_be_t1967, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1967, 0.3).
narrative_ontology:measurement(jsd_rc_be_t1977, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1977, 0.45).
narrative_ontology:measurement(jsd_rc_be_t1987, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1987, 0.55).
narrative_ontology:measurement(jsd_rc_be_t1993, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1993, 0.6).
narrative_ontology:measurement(jsd_rc_be_t2000, jewish_self_determination__religious_covenant_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(jsd_rc_be_t2010, jewish_self_determination__religious_covenant_reading, base_extractiveness, 2010, 0.72).
narrative_ontology:measurement(jsd_rc_be_t2024, jewish_self_determination__religious_covenant_reading, base_extractiveness, 2024, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(jsd_rc_su_t1967, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1967, 0.5).
narrative_ontology:measurement(jsd_rc_su_t1977, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1977, 0.6).
narrative_ontology:measurement(jsd_rc_su_t1987, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1987, 0.7).
narrative_ontology:measurement(jsd_rc_su_t1993, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1993, 0.75).
narrative_ontology:measurement(jsd_rc_su_t2000, jewish_self_determination__religious_covenant_reading, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(jsd_rc_su_t2010, jewish_self_determination__religious_covenant_reading, suppression_requirement, 2010, 0.83).
narrative_ontology:measurement(jsd_rc_su_t2024, jewish_self_determination__religious_covenant_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__religious_covenant_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__religious_covenant_reading, 0.08).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__diasporist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, israeli_occupation_regime).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, palestinian_self_determination).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, international_law_territorial_integrity).

% DUAL FORMULATION NOTE:
% This reading decomposes the kernel 'jewish_self_determination' into a theological claim (mountain claim) and its state-implemented operation (tangled rope). The epsilon referent is the standing arrangement of Israeli control over the West Bank, assessed by this reading's lights (divine mandate = zero extraction). Sibling readings assess the same arrangement differently: liberal_nationalist sees rope/tangled_rope, indigenous_return sees rope/scaffold, diasporist sees snare, settler_colonial sees snare. The network edges represent the structural influence of this reading's theological absolutism on the political viability of compromise-based readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_self_determination__religious_covenant_reading, institutional, 0.1).
constraint_indexing:directionality_override(jewish_self_determination__religious_covenant_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
