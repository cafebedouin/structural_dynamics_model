% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__land_promise_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__land_promise_constraint, []).

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
 *   constraint_id: abrahamic_covenant__land_promise_constraint
 *   human_readable: Abrahamic Covenant Land Promise — Territorial Legitimacy Constraint
 *   domain: religious/political/territorial
 *
 * SUMMARY:
 *   The Genesis covenant is a contested kernel—a single textual commitment
 *   (Genesis 12:1-7, 17:1-21) that different religious traditions and
 *   political actors read differently. THIS story instantiates ONE reading:
 *   the land promise as a territorial grant to Abraham's descendants through
 *   Isaac, interpreted as unconditional and perpetual. This reading has
 *   become institutionalized in Israeli state legitimacy, settlement law, and
 *   nationalist theology. It functions as a snare: the reading itself
 *   justifies territorial dispossession of Palestinians, suppresses
 *   alternative readings (particularly the Islamic reading through Ishmael),
 *   and extracts land, property, and access from Palestinian populations who
 *   bear no voice in the institutional frame where the reading is applied.
 *   The sibling readings (isaac_covenant_reading treating the promise as
 *   exclusive to Isaac, ishmael_covenant_reading interpreting it as inclusive
 *   through Muhammad) are SEPARATE constraints in separate files; this file
 *   does not describe them or hedge the land-promise reading across multiple
 *   interpretations. The ε-invariance principle: if this reading's core claim
 *   changed—if the promise became conditional rather than unconditional, or
 *   expired rather than eternal—the constraint would structurally transform;
 *   those are different readings (different constraints). This reading's ε
 *   represents how the land-promise-as-unconditional operates in the present
 *   institutional and geopolitical frame.
 *
 * KEY AGENTS:
 *   - israeli_state_institutions (agenda-setter, institutional power): interprets covenant as territorial foundation, institutionalizes through law and settlement, maintains enforcement apparatus
 *   - palestinian_arab_populations (victims, powerless, trapped): dispossessed by the reading's enforcement, excluded from the interpretive frame, bear restriction of movement and land access
 *   - displaced_palestinian_refugees (victims, powerless, trapped): most severe extraction: expulsion justified by covenant reading as theological legitimacy for denying return
 *   - christian_zionist_constituencies (beneficiaries, organized, mobile): strengthen the reading's institutional reach through political/financial support, validate it through their own theology
 *   - islamic_theological_authorities (excluded, organized, mobile): hold competing Ishmael reading, have no institutional seat in the constraint's enforcement frame
 *   - international_legal_system (observer, institutional, analytical): operates from secular territorial law, friction with covenant-based legitimacy claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, 0.82).
domain_priors:suppression_score(abrahamic_covenant__land_promise_constraint, 0.88).
domain_priors:theater_ratio(abrahamic_covenant__land_promise_constraint, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, extractiveness, 0.82).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__land_promise_constraint, snare).
narrative_ontology:human_readable(abrahamic_covenant__land_promise_constraint, "Abrahamic Covenant Land Promise — Territorial Legitimacy Constraint").
narrative_ontology:topic_domain(abrahamic_covenant__land_promise_constraint, "religious/political/territorial").

domain_priors:requires_active_enforcement(abrahamic_covenant__land_promise_constraint).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__land_promise_constraint, 'bea44e19-fddf-46cd-b7ea-571e2d624a7a').
narrative_ontology:cs_kernel_codification('bea44e19-fddf-46cd-b7ea-571e2d624a7a', fixed_text).
narrative_ontology:cs_authority_grounding('bea44e19-fddf-46cd-b7ea-571e2d624a7a', lineage).
narrative_ontology:cs_interpretation_layer_present('bea44e19-fddf-46cd-b7ea-571e2d624a7a').
narrative_ontology:cs_reading_relation('bea44e19-fddf-46cd-b7ea-571e2d624a7a', abrahamic_covenant__isaac_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('bea44e19-fddf-46cd-b7ea-571e2d624a7a', abrahamic_covenant__ishmael_covenant_reading, forecloses).
narrative_ontology:cs_axiom('bea44e19-fddf-46cd-b7ea-571e2d624a7a', foundational, land_promise_unconditional_perpetual).
narrative_ontology:cs_axiom_status(land_promise_unconditional_perpetual, holdable).
narrative_ontology:cs_axiom_grounding('bea44e19-fddf-46cd-b7ea-571e2d624a7a', land_promise_unconditional_perpetual, deontological).
narrative_ontology:cs_axiom('bea44e19-fddf-46cd-b7ea-571e2d624a7a', foundational, covenant_lineage_exclusive_to_isaac).
narrative_ontology:cs_axiom_status(covenant_lineage_exclusive_to_isaac, holdable).
narrative_ontology:cs_axiom_grounding('bea44e19-fddf-46cd-b7ea-571e2d624a7a', covenant_lineage_exclusive_to_isaac, empirically_contingent).
narrative_ontology:cs_reference_frame('bea44e19-fddf-46cd-b7ea-571e2d624a7a', mosaic_covenant_territorial_grant).
narrative_ontology:cs_drift_state('bea44e19-fddf-46cd-b7ea-571e2d624a7a', contemporary_institutional_settlement, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bea44e19-fddf-46cd-b7ea-571e2d624a7a', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(abrahamic_covenant__land_promise_constraint, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, israeli_state_institutions).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, palestinian_arab_populations).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, displaced_palestinian_refugees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, christian_zionist_constituencies).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, secular_israeli_citizens).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, secular_israeli_citizens).
narrative_ontology:constraint_vindicates(abrahamic_covenant__land_promise_constraint, biblical_inerrancy_doctrine).
narrative_ontology:constraint_vindicates(abrahamic_covenant__land_promise_constraint, divine_election_of_israel_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the official reading of the covenant through education, law (Law of Return, settlement law, Palestinian land confiscation authority), military control, and diplomatic claims. Institutionalizes the reading as the legitimate interpretation; alternative readings are excluded from state policy. The Israeli state owns the enforcement apparatus and uses it to maintain territorial control justified by the covenant reading.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, israeli_state_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Residents of the West Bank and fragmented territories; subject to the Israeli institutional reading and its enforcement without effective voice in the interpretive frame. Experience restricted movement, land confiscation, settlement expansion on their territory, and legal structures that privilege the Israeli reading. Their contestation is heard within Palestinian civil society but has no institutional seat where Israeli policy is made.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, palestinian_arab_populations, payer,
    powerless, biographical, trapped, national).

% Expelled from former Palestinian territory (1948 and 1967); live in diaspora, neighboring countries, or refugee camps. Denied return-of-property rights and repatriation; their expulsion is justified by the Israeli reading of the covenant (the land belongs to Israel by divine grant). Remain stateless, displaced, and legally unable to return to property or land.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, displaced_palestinian_refugees, payer,
    powerless, biographical, trapped, regional).

% Christian communities (especially evangelical Protestants) who hold a parallel theological reading: the covenant land promise is fulfilled in modern Israel and justifies Jewish territorial claims. Their reading validates and strengthens the Israeli institutional reading; they provide political support, financial aid, and diplomatic backing. Their participation increases the reading's perceived legitimacy and makes contestation appear anti-Christian or antisemitic.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, christian_zionist_constituencies, beneficiary,
    organized, biographical, mobile, global).

% Islamic scholars and authorities who interpret the covenant as continuing through Ishmael to Muhammad and Islam. Hold that the covenant is universal (applying to all Abraham's descendants and all believers), not exclusive to Isaac's line; interpret Palestinian Arab populations as Abrahamic covenant people. Their reading is excluded from Israeli state policy and institutional decision-making; they have no seat where territorial legitimacy is adjudicated.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, islamic_theological_authorities, excluded,
    organized, biographical, mobile, global).

% International law (UN, ICJ, human rights bodies) operates from secular principles (self-determination, territorial acquisition, refugee law) rather than covenant interpretation. Observes the Israeli-Palestinian conflict as a territorial and human rights matter, not as a theological debate. Attempts to apply international law principles (refugee return rights, occupation law, settlements as illegal) which conflict with the Israeli covenant reading justification.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, international_legal_system, observer,
    institutional, generational, analytical, global).

% Israelis who may not personally endorse the covenant reading but benefit from state institutions (security, resources, national identity, legal property rights) built on the covenant-based legitimacy frame. Some bear costs (military service, conflict exposure); others experience the constraint as institutional background without theological endorsement. The reading operates through state power even for those who reject or ignore it theologically.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, secular_israeli_citizens, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__land_promise_constraint, secular_israeli_citizens, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(abrahamic_covenant__land_promise_constraint, israeli_state_institutions).
narrative_ontology:fixing_cost_class(abrahamic_covenant__land_promise_constraint, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None substantive. The reading does not solve a mutual coordination problem; it resolves territorial allocation through theological assertion. The arrangement does not coordinate a collective-action problem for its participants—it imposes an asymmetric allocation on Palestinian populations who did not consent to the theological frame.
% TRANSFER_FUNCTION: Transfers territorial control, property rights, land access, and movement rights from Palestinian Arab populations to Israeli state institutions and affiliated settler populations. The transfer is justified through the covenant reading: the territory is claimed to belong to Abraham's Israelite descendants by divine grant, and Palestinians are treated as occupying or residing on Israeli-claimed territory.
% ABSENT_VOICES: Islamic theological authorities (hold competing Ishmael reading, excluded from Israeli policy); secular Palestinian national movements (contest covenant relevance entirely, treated as illegitimate by the institutional frame); indigenous Palestinian Christians (maintain alternative theological readings, unheard); international legal authorities (oppose covenant-based claims as violating self-determination and refugee law); secular Israelis who contest covenant-based legitimacy for modern territorial claims (marginalized in policy discourse).
% DISAPPEARANCE_RATIONALE: If this institutional reading of the land-promise covenant vanished and ceased to function as state legitimacy, the Israeli-Palestinian conflict would not disappear but would reorganize. Territorial claims would be adjudicated by international law (self-determination, prior occupation, refugee return rights) rather than by covenant interpretation. Settlement policy would lose its theological-legal justification layer. Refugee return claims would shift from theological to legal grounds. The absence of the reading would remove one class of legitimacy claims but would not solve the underlying territorial dispute—it would reframe it from theological irreconcilability to secular political negotiation.
% FOUNDING_PROBLEM: The founding problem (from the reading's perspective): How to establish Jewish territorial sovereignty in a land with other inhabitants, based on historical and theological claim? The reading answers: the covenant promise of territory to Abraham's descendants provides divine legal title, making Jewish sovereignty legitimately grounded independent of current Palestinian presence.
% FOUNDING_PROBLEM_CORROBORATION: Jewish religious authorities and Israeli scholars of biblical interpretation affirm the founding problem is live and the reading is the appropriate answer. Christian Zionist constituencies affirm the reading as fulfilling eschatological promise. Palestinian Islamic authorities, secular historians, and international legal scholars contest the founding problem's framing as erasing Palestinian presence and contest the reading as confusing ancient theological narrative with modern territorial law. No consensus corroboration exists outside the reading's beneficiary constituency and allied Christian constituencies. Secular scholars studying the dispute note that the founding problem is constructed—a modern nationalist reframing of ancient covenant text—not a discoverable fact.
narrative_ontology:disappearance_verdict(abrahamic_covenant__land_promise_constraint, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__land_promise_constraint, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__land_promise_constraint, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(abrahamic_covenant__land_promise_constraint, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__land_promise_constraint, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__land_promise_constraint_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(abrahamic_covenant__land_promise_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 endpoint) and rising over the interval because the reading's enforcement increasingly concentrates territorial control in Israeli hands while Palestinian access, property claims, and settlement options decline. At t0 (pre-state framing), the reading is nascent—theological without enforcement machinery; by t75 (contemporary), it is fully institutionalized through military control, settlement law, refugee-return denial, and educational infrastructure that reproduces the reading as normal. Suppression is higher than extractiveness (0.88) because the constraint's persistence requires active enforcement: Palestinian alternative readings must be silenced or relocated, Palestinian presence must be reduced through expulsion or confinement, territorial claims must be legally foreclosed. Theater is moderate (0.41) because some enforcement functions (security review, settlement administration) carry genuine collective-action problems underneath the extraction, but a rising share of enforcement machinery (land confiscation justifications, refugee-return denial, settlement expansion) is purely about maintaining the reading's territorial consequence. The leveled coercion grid shows asymmetric pressure: at the individual level, Palestinian civilians experience maximal suppression (0.84 endpoint) and maximal accessibility collapse (0.76)—their exit options narrow to exodus, acceptance of minority status, or violent resistance. At the structural level, the suppression reaches even deeper (0.91) because the entire legal-territorial order is reorganized around the reading. Resistance is present (0.79-0.82 across levels) but trapped—Palestinian resistance exists but the institutional frame where the reading is applied lacks mechanisms to translate Palestinian contestation into policy change.
 *
 * PERSPECTIVAL GAP:
 *   From the israeli_state_institutions seat: the reading is a foundational legitimacy claim, a proper interpretation of covenantal promise, and justifies territorial sovereignty. From this seat, the constraint is not extraction but lawful exercise of religious-historical right. From the palestinian_arab_populations seat: the reading is an imposed alien interpretation used to justify dispossession. From this seat, the constraint is pure coercion backed by state force, not legitimate authority. The engine computes both perspectives: the institutional seat experiences low directionality (d near beneficiary), the Palestinian seat experiences high directionality (d near target). The same structural fact—the land-promise reading institutionalized in state power—produces different classifications depending on seat position. This gap is NOT a defect; it is the measurement the framework exists to surface.
 *
 * DIRECTIONALITY LOGIC:
 *   israeli_state_institutions: agenda-setter role, institutional power, arbitrage exit (can choose how to deploy covenant reading, can shift emphasis if political cost rises), benefits directly from the reading's territorial consequence—low d (beneficiary directionality), near 0.1. palestinian_arab_populations: payer role, powerless, trapped exit (cannot leave the land they are dispossessed from; cannot exit Palestinian identity; cannot unheard their contestation)—high d (target directionality), near 0.9. displaced_palestinian_refugees: even higher d (0.95) because their exit is even more constrained—they are doubly trapped by legal refugee status and by the reading's denial of return rights. christian_zionist_constituencies: beneficiary role, organized power, mobile exit (can choose to support or withdraw support; can migrate; support is voluntary)—low d (0.25), dampened beneficiary because their benefit is coalitional rather than primary. islamic_theological_authorities: excluded from the enforcement frame entirely but hold a structurally opposed reading—neither beneficiary nor payer in this constraint's seat structure, but their exclusion is the enforcement mechanism—d around 0.6 (neither full target nor full beneficiary, but adversarially positioned). No directionality overrides needed; the derivation from beneficiary/victim + exit captures the structural asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jewish territorial sovereignty based on covenantal promise) was live in pre-state theological discourse. It remains live as a legitimacy claim for the Israeli state's existence and territorial extent. However, the mandatrophy question: has the founding problem's justification become detached from its function? The constraint now functions primarily to MAINTAIN territorial control and SUPPRESS Palestinian claims, not to SOLVE the Jewish sovereignty problem (that problem is solved—the state exists). The rising theater_ratio (0.18 to 0.41) indicates that an increasing share of enforcement activity is purely defensive (maintaining the reading against Palestinian contestation, enforcing refugee-return denial, justifying settlement expansion) rather than solving the original problem. The suppression requirement (0.62 to 0.88) rising faster than extractiveness (0.45 to 0.82) is the diagnostic signal of mandatrophy drift: more coercive force is required to hold the arrangement because the original problem justification is weaker, and the arrangement now persists through pure enforcement rather than through participant consent. The constraint is drifting toward a piton-like state—it persists by inertia and force, not by solving a live coordination problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenant_conditionality_ambiguity,
    'Is the land promise in Genesis unconditional and perpetual, or is it conditioned on Israelite adherence to covenant obligations, with breach consequences (expulsion, loss of territory)?',
    'Textual exegesis with corroboration from mainstream Jewish interpretive tradition; examination of later Deuteronomic conditional-covenant language and its application to the territorial promise; investigation of whether breach-consequence frameworks in Jewish law cancel land claims.',
    'If unconditional: the reading supports perpetual Israeli territorial claim independent of current occupation or Palestinian presence. If conditional: the reading opens vulnerability—breach would forfeit the territorial claim or require restoration if conditions are unmet. Conditionality would reclassify the constraint from pure extraction (snare) to conditional extraction (tangled rope with escape clause).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenant_conditionality_ambiguity, empirical, 'Whether the covenant promise is unconditional or contains forfeiture clauses.').

omega_variable(
    fulfillment_vs_ongoing_promise_ambiguity,
    'Has the territorial promise already been fulfilled (via Joshua''s conquest, or via the current state of Israel), or is it an ongoing perpetual promise that continues to justify territorial expansion?',
    'Analysis of how different Jewish theological schools treat the conquest as fulfillment vs. partial fulfillment vs. eschatological future fulfillment; examination of whether current borders match historical Canaan boundaries and how discrepancies are theologized.',
    'If fulfilled: the promise justifies the current state but not further expansion; territory already allocated ends the claim. If ongoing: the promise justifies continued territorial expansion toward historical Canaan boundaries, making the constraint extractive indefinitely. Classification would shift from snare (extractive maintenance) to expanding extraction if ongoing is the consensus reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fulfillment_vs_ongoing_promise_ambiguity, conceptual, 'Whether the covenant promise represents a completed transaction or an ongoing directive.').

omega_variable(
    exclusive_vs_inclusive_lineage_reading,
    'Does the covenant promise apply ONLY to Isaac''s line (and thus exclude Ishmael''s descendants, including Palestinian Arabs and Muslims), or does it include Ishmael (and thus create shared covenant status)?',
    'Detailed examination of Genesis 17:19-21 (''My covenant I will establish with Isaac'') and competing Islamic and Palestinian theological interpretations; analysis of whether covenantal promises in Hebrew scripture apply to multiple lineages or single exclusive line; ethnogenetic evidence about whether Palestinians descend from Ishmael or other lineages.',
    'Exclusive reading (this constraint): Palestinians are not covenant beneficiaries and have no claim to the territory; they are structurally outside the covenant frame. Inclusive reading (Ishmael_covenant_reading): Palestinians and Muslims are also Abrahamic covenant people; territorial sharing becomes theologically required. This is the foundational schism between this reading and the Ishmael reading—it determines victim set, beneficiary set, and legitimacy of Palestinian claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exclusive_vs_inclusive_lineage_reading, conceptual, 'Whether the land promise applies exclusively to Isaac''s line or inclusively to both Isaac and Ishmael.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression primarily structural (military occupation, legal restrictions, economic barriers that exist external to Palestinian consciousness) or internalized (Palestinians believe the Israeli reading is legitimate, accept dispossession as divinely ordained, or internalize inferiority)?',
    'Post-suppression trajectory analysis: if Palestinian resistance and land claims persist after legal restrictions are removed, suppression is primarily structural. If Palestinian contestation dissolves after access to education presenting alternative readings, suppression was partly internalized (absorbed through institutional narrative). Comparison with historical periods when suppression was lighter vs. intense.',
    'If primarily structural: the measured suppression (0.88) understates effective constraint power—removal of barriers alone would not stop Palestinian resistance. If internalized: the constraint is even more extractive than measured (carries suppression internally). If mixed: some share of Palestinian acceptance is genuine (coerced belief in the reading''s legitimacy) and some is strategic adaptation to overwhelming power.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression operates through external barriers or through absorbed belief in the reading''s legitimacy.').

omega_variable(
    kernel_framing_uncertainty,
    'Is the operative kernel ''the Genesis covenant'' (a theological text), or is it ''modern territorial legitimacy'' (a geopolitical framework)? Do the readings truly disagree on the same object, or does each reading attach the covenant to different political projects?',
    'Examine whether Israeli, Palestinian, and Christian perspectives all treat the Genesis text as authoritative for modern territorial claims, or whether Palestinians contest the relevance of the text entirely. Investigate whether the ''constraint'' is a theological disagreement about the text''s meaning or a political disagreement about whether ancient texts should govern modern land law.',
    'If readings genuinely disagree on the same kernel (Genesis meaning): this is a kernel-reading constraint family, and the framework correctly models the contest. If readings incommensurably attach the text to different political frameworks (Israelis use covenant to ground state legitimacy, Palestinians use different religious/secular grounds entirely): the operative contest is not exegetical but political, and the theological framing may be post-hoc rationalization. Classification might shift from kernel-reading snare to pure political extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_uncertainty, conceptual, 'Whether the contested kernel is the Genesis text''s meaning or the political use of theological claims to ground territorial sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__land_promise_constraint, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abra_tr_t0, abrahamic_covenant__land_promise_constraint, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(abra_tr_t0, projected).
narrative_ontology:measurement(abra_tr_t10, abrahamic_covenant__land_promise_constraint, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(abra_tr_t10, observed).
narrative_ontology:measurement(abra_tr_t25, abrahamic_covenant__land_promise_constraint, theater_ratio, 25, 0.33).
narrative_ontology:measurement_basis(abra_tr_t25, observed).
narrative_ontology:measurement(abra_tr_t45, abrahamic_covenant__land_promise_constraint, theater_ratio, 45, 0.39).
narrative_ontology:measurement_basis(abra_tr_t45, observed).
narrative_ontology:measurement(abra_tr_t60, abrahamic_covenant__land_promise_constraint, theater_ratio, 60, 0.4).
narrative_ontology:measurement_basis(abra_tr_t60, observed).
narrative_ontology:measurement(abra_tr_t75, abrahamic_covenant__land_promise_constraint, theater_ratio, 75, 0.41).
narrative_ontology:measurement_basis(abra_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(abra_be_t0, abrahamic_covenant__land_promise_constraint, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(abra_be_t0, projected).
narrative_ontology:measurement(abra_be_t10, abrahamic_covenant__land_promise_constraint, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(abra_be_t10, observed).
narrative_ontology:measurement(abra_be_t25, abrahamic_covenant__land_promise_constraint, base_extractiveness, 25, 0.72).
narrative_ontology:measurement_basis(abra_be_t25, observed).
narrative_ontology:measurement(abra_be_t45, abrahamic_covenant__land_promise_constraint, base_extractiveness, 45, 0.79).
narrative_ontology:measurement_basis(abra_be_t45, observed).
narrative_ontology:measurement(abra_be_t60, abrahamic_covenant__land_promise_constraint, base_extractiveness, 60, 0.81).
narrative_ontology:measurement_basis(abra_be_t60, observed).
narrative_ontology:measurement(abra_be_t75, abrahamic_covenant__land_promise_constraint, base_extractiveness, 75, 0.82).
narrative_ontology:measurement_basis(abra_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(abra_su_t0, abrahamic_covenant__land_promise_constraint, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(abra_su_t0, projected).
narrative_ontology:measurement(abra_su_t10, abrahamic_covenant__land_promise_constraint, suppression_requirement, 10, 0.74).
narrative_ontology:measurement_basis(abra_su_t10, observed).
narrative_ontology:measurement(abra_su_t25, abrahamic_covenant__land_promise_constraint, suppression_requirement, 25, 0.82).
narrative_ontology:measurement_basis(abra_su_t25, observed).
narrative_ontology:measurement(abra_su_t45, abrahamic_covenant__land_promise_constraint, suppression_requirement, 45, 0.86).
narrative_ontology:measurement_basis(abra_su_t45, observed).
narrative_ontology:measurement(abra_su_t60, abrahamic_covenant__land_promise_constraint, suppression_requirement, 60, 0.87).
narrative_ontology:measurement_basis(abra_su_t60, observed).
narrative_ontology:measurement(abra_su_t75, abrahamic_covenant__land_promise_constraint, suppression_requirement, 75, 0.88).
narrative_ontology:measurement_basis(abra_su_t75, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=75
narrative_ontology:measurement(abra_grid_01, abrahamic_covenant__land_promise_constraint, accessibility_collapse(class), 0, 0.62).
narrative_ontology:measurement(abra_grid_02, abrahamic_covenant__land_promise_constraint, accessibility_collapse(class), 75, 0.74).
narrative_ontology:measurement(abra_grid_03, abrahamic_covenant__land_promise_constraint, accessibility_collapse(individual), 0, 0.58).
narrative_ontology:measurement(abra_grid_04, abrahamic_covenant__land_promise_constraint, accessibility_collapse(individual), 75, 0.76).
narrative_ontology:measurement(abra_grid_05, abrahamic_covenant__land_promise_constraint, accessibility_collapse(organizational), 0, 0.65).
narrative_ontology:measurement(abra_grid_06, abrahamic_covenant__land_promise_constraint, accessibility_collapse(organizational), 75, 0.79).
narrative_ontology:measurement(abra_grid_07, abrahamic_covenant__land_promise_constraint, accessibility_collapse(structural), 0, 0.68).
narrative_ontology:measurement(abra_grid_08, abrahamic_covenant__land_promise_constraint, accessibility_collapse(structural), 75, 0.81).
narrative_ontology:measurement(abra_grid_09, abrahamic_covenant__land_promise_constraint, resistance(class), 0, 0.75).
narrative_ontology:measurement(abra_grid_10, abrahamic_covenant__land_promise_constraint, resistance(class), 75, 0.81).
narrative_ontology:measurement(abra_grid_11, abrahamic_covenant__land_promise_constraint, resistance(individual), 0, 0.71).
narrative_ontology:measurement(abra_grid_12, abrahamic_covenant__land_promise_constraint, resistance(individual), 75, 0.82).
narrative_ontology:measurement(abra_grid_13, abrahamic_covenant__land_promise_constraint, resistance(organizational), 0, 0.68).
narrative_ontology:measurement(abra_grid_14, abrahamic_covenant__land_promise_constraint, resistance(organizational), 75, 0.78).
narrative_ontology:measurement(abra_grid_15, abrahamic_covenant__land_promise_constraint, resistance(structural), 0, 0.64).
narrative_ontology:measurement(abra_grid_16, abrahamic_covenant__land_promise_constraint, resistance(structural), 75, 0.73).
narrative_ontology:measurement(abra_grid_17, abrahamic_covenant__land_promise_constraint, stakes_inflation(class), 0, 0.61).
narrative_ontology:measurement(abra_grid_18, abrahamic_covenant__land_promise_constraint, stakes_inflation(class), 75, 0.85).
narrative_ontology:measurement(abra_grid_19, abrahamic_covenant__land_promise_constraint, stakes_inflation(individual), 0, 0.52).
narrative_ontology:measurement(abra_grid_20, abrahamic_covenant__land_promise_constraint, stakes_inflation(individual), 75, 0.83).
narrative_ontology:measurement(abra_grid_21, abrahamic_covenant__land_promise_constraint, stakes_inflation(organizational), 0, 0.68).
narrative_ontology:measurement(abra_grid_22, abrahamic_covenant__land_promise_constraint, stakes_inflation(organizational), 75, 0.87).
narrative_ontology:measurement(abra_grid_23, abrahamic_covenant__land_promise_constraint, stakes_inflation(structural), 0, 0.72).
narrative_ontology:measurement(abra_grid_24, abrahamic_covenant__land_promise_constraint, stakes_inflation(structural), 75, 0.89).
narrative_ontology:measurement(abra_grid_25, abrahamic_covenant__land_promise_constraint, suppression(class), 0, 0.62).
narrative_ontology:measurement(abra_grid_26, abrahamic_covenant__land_promise_constraint, suppression(class), 75, 0.87).
narrative_ontology:measurement(abra_grid_27, abrahamic_covenant__land_promise_constraint, suppression(individual), 0, 0.58).
narrative_ontology:measurement(abra_grid_28, abrahamic_covenant__land_promise_constraint, suppression(individual), 75, 0.84).
narrative_ontology:measurement(abra_grid_29, abrahamic_covenant__land_promise_constraint, suppression(organizational), 0, 0.64).
narrative_ontology:measurement(abra_grid_30, abrahamic_covenant__land_promise_constraint, suppression(organizational), 75, 0.89).
narrative_ontology:measurement(abra_grid_31, abrahamic_covenant__land_promise_constraint, suppression(structural), 0, 0.68).
narrative_ontology:measurement(abra_grid_32, abrahamic_covenant__land_promise_constraint, suppression(structural), 75, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__land_promise_constraint, identity_coordination).
narrative_ontology:boltzmann_floor_override(abrahamic_covenant__land_promise_constraint, 0.12).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, isaac_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, ishmael_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, palestinian_refugee_law_constraint).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, israeli_settlement_expansion_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the abrahamic_covenant kernel. Sibling readings (isaac_covenant_reading and ishmael_covenant_reading) instantiate alternative interpretations of the same Genesis covenant text. Each reading produces different victim sets, beneficiary structures, and territorial consequences. The three constraints form a constraint family linked by network.affects_constraints. This reading (land_promise_constraint) is most extractive and most directly instantiated in modern Israeli state power; the sibling readings remain in theological discourse without comparable institutional enforcement machinery in the Israeli context (though ishmael_covenant_reading is institutionalized in Islamic law and Palestinian resistance frames). The family decomposition follows the ε-invariance principle: different readings produce different ε values because they make different empirical claims (about who benefits, who is harmed, what territory is claimed) and different normative claims (about covenant interpretation, lineage, conditionality). They are not the same constraint viewed from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
