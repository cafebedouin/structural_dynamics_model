% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__religious_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__religious_restoration_reading, []).

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
 *   constraint_id: zionist_legitimacy_basis__religious_restoration_reading
 *   human_readable: Zionism as Divine Promise Fulfillment and Messianic Process (Religious Zionist Reading, post-1967)
 *   domain: political_history/nationalism/settler_colonialism_studies
 *
 * SUMMARY:
 *   This constraint story captures the religious Zionist interpretation
 *   (post-1967) that frames Zionism as the fulfillment of divine promise and
 *   an active messianic process. The Six-Day War victory is read as divine
 *   acceleration of redemption, making territorial maximalism (biblical
 *   borders) theologically mandated rather than politically negotiable. The
 *   constraint claims Mountain status — divine law, irreducible, emerging
 *   naturally from covenant. However, identifiable beneficiaries exist:
 *   religious Zionist settlers who gain land and ideological purpose, the
 *   Israeli state apparatus that gains territorial depth and demographic
 *   rationale, and national-religious rabbinic authorities who gain
 *   interpretive authority. Victims are the Palestinian population subject to
 *   displacement, military rule, and legal exclusion. The constraint requires
 *   active enforcement (military, legal, settlement infrastructure) and shows
 *   rising extraction and suppression over the interval. The claimed_type
 *   (mountain) and authored metrics (high extraction, suppression, theater)
 *   diverge — this divergence is the measurement, not an error.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__religious_restoration_reading, 0.82).
domain_priors:suppression_score(zionist_legitimacy_basis__religious_restoration_reading, 0.78).
domain_priors:theater_ratio(zionist_legitimacy_basis__religious_restoration_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__religious_restoration_reading, mountain).
narrative_ontology:human_readable(zionist_legitimacy_basis__religious_restoration_reading, "Zionism as Divine Promise Fulfillment and Messianic Process (Religious Zionist Reading, post-1967)").
narrative_ontology:topic_domain(zionist_legitimacy_basis__religious_restoration_reading, "political_history/nationalism/settler_colonialism_studies").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__religious_restoration_reading).
domain_priors:emerges_naturally(zionist_legitimacy_basis__religious_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__religious_restoration_reading, 'be4b20a3-c9b2-4e40-ba09-f3847a966e7f').
narrative_ontology:cs_kernel_codification('be4b20a3-c9b2-4e40-ba09-f3847a966e7f', formalized).
narrative_ontology:cs_authority_grounding('be4b20a3-c9b2-4e40-ba09-f3847a966e7f', lineage).
narrative_ontology:cs_interpretation_layer_present('be4b20a3-c9b2-4e40-ba09-f3847a966e7f').
narrative_ontology:cs_reading_relation('be4b20a3-c9b2-4e40-ba09-f3847a966e7f', zionist_legitimacy_basis__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('be4b20a3-c9b2-4e40-ba09-f3847a966e7f', zionist_legitimacy_basis__settler_colonial_reading, forecloses).
narrative_ontology:cs_axiom('be4b20a3-c9b2-4e40-ba09-f3847a966e7f', foundational, divine_land_promise_eternal).
narrative_ontology:cs_axiom_status(divine_land_promise_eternal, holdable).
narrative_ontology:cs_axiom_grounding('be4b20a3-c9b2-4e40-ba09-f3847a966e7f', divine_land_promise_eternal, theological).
narrative_ontology:cs_axiom('be4b20a3-c9b2-4e40-ba09-f3847a966e7f', foundational, messianic_process_requires_territorial_maximalism).
narrative_ontology:cs_axiom_status(messianic_process_requires_territorial_maximalism, holdable).
narrative_ontology:cs_axiom_grounding('be4b20a3-c9b2-4e40-ba09-f3847a966e7f', messianic_process_requires_territorial_maximalism, theological).
narrative_ontology:cs_axiom('be4b20a3-c9b2-4e40-ba09-f3847a966e7f', secondary, secular_political_considerations_subordinate_to_halakha).
narrative_ontology:cs_axiom_status(secular_political_considerations_subordinate_to_halakha, holdable).
narrative_ontology:cs_axiom_grounding('be4b20a3-c9b2-4e40-ba09-f3847a966e7f', secular_political_considerations_subordinate_to_halakha, theological).
narrative_ontology:cs_reference_frame('be4b20a3-c9b2-4e40-ba09-f3847a966e7f', biblical_covenant_restoration).
narrative_ontology:cs_drift_state('be4b20a3-c9b2-4e40-ba09-f3847a966e7f', post_1967_victory, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('be4b20a3-c9b2-4e40-ba09-f3847a966e7f', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_settlers).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, national_religious_rabbinic_authorities).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, palestinian_population).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, displaced_palestinians).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, palestinian_refugees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, secular_zionist_israelis).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, secular_zionist_israelis).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__religious_restoration_reading, divine_land_promise_to_jewish_people).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__religious_restoration_reading, messianic_redemption_through_territorial_restoration).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__religious_restoration_reading, biblical_borders_as_eternal_inheritance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiate and populate settlements in biblical Judea/Samaria (West Bank) as religious obligation. Receive state funding, military protection, infrastructure, and ideological validation. Their identity is fused to the land — exit means abandoning the redemptive mission. They drive the territorial maximalist agenda through political parties (Mafdal, Bayit Yehudi, Religious Zionism) and settlement movements (Gush Emunim, Amana).
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_settlers, agenda_setter,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_settlers, beneficiary).

% Provides legal framework (military orders, planning regimes), military enforcement, budgetary allocation, and diplomatic cover for settlements. Gains strategic depth, demographic rationale for Jewish majority, and nationalist legitimacy. Constrained exit: could freeze/reverse settlements but faces domestic political collapse from right-wing coalition dependence and ideological capture of state institutions.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__religious_restoration_reading, israeli_state_apparatus, beneficiary).

% Produce halakhic rulings legitimizing settlement, forbidding territorial compromise, framing military service as mitzvah. Gain interpretive authority over redemption theology, control of religious-zionist educational network (yeshivot, mechinot), and political influence via rabbinic endorsements. Identity-locked: their authority derives entirely from being the interpreters of the messianic process.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, national_religious_rabbinic_authorities, beneficiary,
    organized, civilizational, identity_locked, regional).

% Subject to military law, movement restrictions, land expropriation, home demolitions, water allocation discrimination, and denial of political rights. Extraction takes form of lost land, restricted livelihoods, curtailed development, and statutory inequality. Exit is identity-locked — connection to land is existential, not optional; displacement means cultural erasure. Resistance takes armed, civil, legal, and cultural forms.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, palestinian_population, payer,
    powerless, biographical, identity_locked, local).

% Internally displaced (1948, 1967) or refugees in neighboring states. Bear the historical extraction of land and property; current constraint forecloses return. Trapped exit: no right of return, no citizenship in host states, no viable integration. Their situation is the constraint's historical foundation — the theological mandate requires their absence.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, displaced_palestinians, payer,
    powerless, generational, trapped, regional).

% Descendants of 1948/1967 refugees in diaspora (Lebanon, Jordan, Syria, Gaza, West Bank, global). Statutorily excluded from the land by the same theological mandate that legitimizes Jewish return. Trapped exit: no state recognition, limited rights in host countries, UNRWA dependency. The constraint's maximalist theology (no right of return) is the primary structural barrier to resolution.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, palestinian_refugees, payer,
    powerless, generational, trapped, global).

% Bear fiscal cost of settlements (subsidies, security), moral cost of occupation, and demographic risk to Jewish democracy. Benefit from security narrative and national cohesion. Mobile exit: can emigrate (yerida) — ~10% have. Their ambivalence sustains the constraint: they neither fully resist nor fully endorse, providing the silent majority that enables continuity.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, secular_zionist_israelis, payer,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__religious_restoration_reading, secular_zionist_israelis, beneficiary).

% UN, ICJ, ICC, EU, US State Department, human rights NGOs. Document violations, issue resolutions, condition aid, pursue accountability. Analytical exit: can shift policy but cannot enforce. Their intermittent pressure creates oscillation in suppression (Oslo, disengagement, renewed expansion) but has not altered the constraint's theological core.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, international_community, observer,
    institutional, generational, analytical, global).

% PA/PLO leadership excluded from final-status negotiations on terms of theological maximalism. Their consent is structurally unnecessary to the constraint's operation. Constrained exit: collaboration with occupation apparatus for limited autonomy creates dependency; resistance risks destruction of governing capacity. Their exclusion is the constraint's operational requirement — a partner who accepts the theological frame cannot exist.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, palestinian_national_leadership, excluded,
    moderate, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish collective action around a theological narrative of redemption: settlement as mitzvah, military service as holy war, political compromise as sin. Solves the coordination problem of mobilizing a minority population for territorial expansion against international opposition by framing it as divine command rather than political choice.
% TRANSFER_FUNCTION: Moves land, water, sovereignty, civil rights, and development potential from Palestinian population (payers) to religious Zionist settlers and Israeli state (beneficiaries). The transfer is justified as 'return' rather than 'taking' — the theological frame converts extraction into restoration.
% ABSENT_VOICES: Palestinian refugees in diaspora (structurally excluded from any negotiation), Palestinian citizens of Israel (subject to Jewish Nation-State Law as second-tier), secular Israelis who oppose occupation but lack political representation, Jewish diaspora critics of religious maximalism (labeled anti-Zionist), international legal institutions whose rulings are ignored. These voices would object to the theological mandate overriding human rights and international law.
% DISAPPEARANCE_RATIONALE: If the religious restoration constraint vanished overnight, the settlement enterprise would lose its primary legitimating ideology. The Israeli state would face immediate pressure to withdraw to 1967 borders (as international law demands). Settlers would lose ideological cohesion and state protection. Palestinian political horizon would shift from resistance to negotiation. The regional order built on 'no partner for peace' would collapse. The world rearranges because the constraint is the keystone of the current equilibrium.
% FOUNDING_PROBLEM: Establishing Jewish sovereignty over the biblical Land of Israel as fulfillment of divine covenant and acceleration of messianic redemption. The problem is theological: exile is punishment; return is redemption; the land must be fully possessed for the messianic process to complete.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the religious Zionist rabbinic tradition (Rav Kook, Rav Zvi Yehuda Kook, contemporary Religious Zionist yeshiva heads) and the settlement movement's own historiography. No corroborating source outside the benefiting tradition exists — secular historians, Palestinian narratives, international law, and non-Zionist Jewish theology all reject the divine mandate premise. The absence of external corroboration is itself signal: the problem is live only within the closed epistemic community that benefits from it.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__religious_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__religious_restoration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__religious_restoration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__religious_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__religious_restoration_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__religious_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__religious_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, ExtMetricName, E),
    domain_priors:suppression_score(zionist_legitimacy_basis__religious_restoration_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(zionist_legitimacy_basis__religious_restoration_reading),
    narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(zionist_legitimacy_basis__religious_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the constraint transfers land, water, sovereignty, and civil rights from Palestinians to Jewish settlers/state under theological cover. Suppression (0.78) is high because alternatives (Palestinian state, binational democracy, 1967 borders) are actively foreclosed by settlement facts, military orders, and legal architecture. Theater ratio (0.45) is moderate — genuine religious conviction exists but a growing share of enforcement activity serves territorial expansion rather than spiritual practice. Accessibility collapse (0.88) is very high because the theological frame renders compromise structurally illegitimate within the reading's own logic. Resistance (0.72) is high — Palestinian armed and civil resistance, international legal challenges, and internal Israeli dissent all contest the constraint. The measurement series shows monotonic escalation post-1967, consistent with messianic acceleration theology.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (settlers, rabbinic authorities), the constraint appears as Mountain — divine law, non-negotiable, zero degrees of freedom. From the victim seats (Palestinians), it computes as Snare — pure extraction maintained by overwhelming force, no coordination function for them. The agenda-setter seat (state apparatus) experiences it as Tangled Rope — genuine coordination of Jewish demographic-solidarity functions fused with asymmetric extraction of Palestinian resources. The engine computes this seat divergence from the structural data; the authored claim (mountain) reflects only the beneficiary seat's self-understanding.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious Zionist settlers are structural beneficiaries (d near 0.0) — they receive land, subsidies, ideological validation, and state protection. Israeli state apparatus benefits (d ~ 0.15) — gains strategic depth, demographic buffer, and nationalist legitimacy. National-religious rabbinic authorities benefit (d ~ 0.1) — interpretive monopoly over redemption theology. Palestinian population are full targets (d near 1.0) — bear extraction of land, rights, movement, with trapped exit (identity_locked to land). Displaced Palestinians and refugees are targets with constrained exit. Secular Israelis (not listed as stakeholders but affected) sit near symmetric — some security benefit, some moral cost. International community observers are analytical (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jewish sovereignty in biblical lands as divine mandate) remains live from this reading's perspective — redemption is incomplete until full biblical borders are realized. However, the coordination function (Jewish national survival) has been substantially achieved by 1967 borders; post-1967 expansion serves theological maximalism, not survival. This is a classic mandatrophy: the arrangement's mandate (divine restoration) has outlived its coordination function (secure Jewish homeland) but persists because the theological frame forbids recognizing the mismatch. The theater ratio rise tracks this — more performance (settlement as religious act) substitutes for the exhausted coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_vs_constructed_claim,
    'Is the constraint''s claim to divine mandate a genuine natural-law Mountain, or a constructed theological narrative that benefits identifiable agents (settlers, state, rabbinic authorities)?',
    'Comparative analysis of pre-1967 vs post-1967 religious Zionist theology; examination of whether territorial maximalism was doctrinally necessary or politically contingent. Independent theological critique from non-Zionist Jewish authorities.',
    'If constructed, the constraint is a False Summit Mountain masking a Tangled Rope or Snare; FSM signature would trigger reclassification. If genuine natural law, Mountain classification holds despite beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_mandate_vs_constructed_claim, conceptual, 'Whether the divine mandate claim is structurally natural or politically constructed').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of Palestinian alternatives structural (military occupation, legal restrictions, settlement facts-on-ground) or internalized (Palestinian acceptance of inevitability, theological fatalism, leadership co-optation)?',
    'Post-exit trajectory analysis: if Palestinian resistance persists despite structural barriers, suppression is primarily structural. If resistance collapses when structural pressure eases, internalized component is significant. Compare First vs Second Intifada dynamics.',
    'If substantially internalized, effective suppression exceeds structural measure — the constraint carries its own enforcement inside the target population. This amplifies χ for Palestinian seats beyond what structural metrics alone indicate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism for Palestinian population').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (religious_restoration_reading) of the contested kernel zionist_legitimacy_basis. How do sibling readings (national_liberation_reading, settler_colonial_reading) structurally relate to this reading''s classification?',
    'Author separate constraint stories for each sibling reading with their own ε, beneficiaries, victims, and claimed_type. Link via network.affects_constraints. The engine will compute per-seat classifications for each reading independently.',
    'If sibling readings produce different classifications for the same structural arrangement, the kernel itself is the site of classificatory contestation — not a measurement artifact. This validates the ε-invariance decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer-frame structure: kernel identity, sibling readings, and structural delta').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__religious_restoration_reading, 1967, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zionist_legitimacy_basis__religious_restoration_reading_tr_t1967, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1967, 0.25).
narrative_ontology:measurement(zionist_legitimacy_basis__religious_restoration_reading_tr_t1977, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1977, 0.32).
narrative_ontology:measurement(zionist_legitimacy_basis__religious_restoration_reading_tr_t1987, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1987, 0.38).
narrative_ontology:measurement(zionist_legitimacy_basis__religious_restoration_reading_tr_t1993, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1993, 0.4).
narrative_ontology:measurement(zionist_legitimacy_basis__religious_restoration_reading_tr_t2000, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(zionist_legitimacy_basis__religious_restoration_reading_tr_t2010, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2010, 0.44).
narrative_ontology:measurement(zionist_legitimacy_basis__religious_restoration_reading_tr_t2020, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2020, 0.45).

% Extraction over time
narrative_ontology:measurement(zionist_legitimacy_basis__religious_restoration_reading_be_t1967, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1967, 0.45).
narrative_ontology:measurement(zionist_legitimacy_basis__religious_restoration_reading_be_t1977, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1977, 0.58).
narrative_ontology:measurement(zionist_legitimacy_basis__religious_restoration_reading_be_t1987, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1987, 0.65).
narrative_ontology:measurement(zionist_legitimacy_basis__religious_restoration_reading_be_t1993, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1993, 0.68).
narrative_ontology:measurement(zionist_legitimacy_basis__religious_restoration_reading_be_t2000, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2000, 0.74).
narrative_ontology:measurement(zionist_legitimacy_basis__religious_restoration_reading_be_t2010, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2010, 0.79).
narrative_ontology:measurement(zionist_legitimacy_basis__religious_restoration_reading_be_t2020, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2020, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(zionist_legitimacy_basis__religious_restoration_reading_su_t1967, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1967, 0.55).
narrative_ontology:measurement(zionist_legitimacy_basis__religious_restoration_reading_su_t1977, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1977, 0.62).
narrative_ontology:measurement(zionist_legitimacy_basis__religious_restoration_reading_su_t1987, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1987, 0.7).
narrative_ontology:measurement(zionist_legitimacy_basis__religious_restoration_reading_su_t1993, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1993, 0.72).
narrative_ontology:measurement(zionist_legitimacy_basis__religious_restoration_reading_su_t2000, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(zionist_legitimacy_basis__religious_restoration_reading_su_t2010, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2010, 0.77).
narrative_ontology:measurement(zionist_legitimacy_basis__religious_restoration_reading_su_t2020, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2020, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__religious_restoration_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(zionist_legitimacy_basis__religious_restoration_reading, 0.08).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis__national_liberation_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis__settler_colonial_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, israeli_settlement_enterprise).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, palestinian_displacement_regime).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, jerusalem_status_quo).

% DUAL FORMULATION NOTE:
% This reading decomposes the 'Zionist legitimacy' label into a theologically mandated Mountain-claim. The national_liberation_reading likely computes as Rope or Tangled Rope (coordination of Jewish survival with some extraction). The settler_colonial_reading likely computes as Snare (pure extraction). The three readings are not perspectives on one constraint — they are three constraints with different ε, linked by network.affects_constraints. This story's high ε (0.82) vs the national liberation reading's expected lower ε demonstrates ε-invariance: the label 'Zionism' covers structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(zionist_legitimacy_basis__religious_restoration_reading, institutional, 0.15).
constraint_indexing:directionality_override(zionist_legitimacy_basis__religious_restoration_reading, organized, 0.05).
constraint_indexing:directionality_override(zionist_legitimacy_basis__religious_restoration_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
