% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__religious_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: zionist_legitimacy_basis__religious_restoration_reading
 *   human_readable: Zionist Legitimacy Grounded in Divine Restoration (Religious Post-1967 Interpretation)
 *   domain: political/historical/religious
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested kernel
 *   'zionist_legitimacy_basis': the religious restoration reading, which
 *   interprets Zionism as fulfillment of divine promise and messianic
 *   process, particularly intensified post-1967. Under this reading,
 *   territorial possession of the land of Israel is a religious obligation
 *   grounded in scriptural covenant, and territorial maximalism (including
 *   settlement expansion and continued occupation) is theologically mandated.
 *   This reading competes with two sibling readings—national liberation
 *   (Zionism as secular response to persecution and indigenous return) and
 *   settler-colonial (Zionism as European-origin colonial project)—each of
 *   which specifies different beneficiaries, extraction mechanisms, and
 *   victims. The religious restoration reading produces a constraint where
 *   religious obligation overrides secular political constraints,
 *   accessibility to alternative frameworks collapses for identity-locked
 *   actors, and resistance is suppressed through theological authority
 *   claims. The measurement trajectory from 1967 to 2024 shows extractiveness
 *   and suppression rising, theater ratio stabilizing at moderate-high
 *   levels, and organized Palestinian resistance declining relative to
 *   structural/class-level suppression intensification.
 *
 * KEY AGENTS:
 *   - jewish_settlers: Primary beneficiary (land allocation, ideological legitimacy, state protection); identity-locked exit due to religious obligation framing
 *   - religious_zionist_authority: Agenda-setter (interprets scripture, mobilizes communities, shapes state policy); institutional power to define theological truth
 *   - palestinian_displaced_populations: Primary victims (land expropriation, settlement enclosure, legal subordination); trapped with no sovereignty alternative
 *   - secular_israeli_political_center: Constrained beneficiary (mobilization gains, international legitimacy claims); but bears escalating costs as religious reading radicalizes
 *   - muslim_christian_minorities: Secondary victims (subordinate legal status, displacement pressure); constrained exit between subordination or exile
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__religious_restoration_reading, 0.68).
domain_priors:suppression_score(zionist_legitimacy_basis__religious_restoration_reading, 0.72).
domain_priors:theater_ratio(zionist_legitimacy_basis__religious_restoration_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__religious_restoration_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__religious_restoration_reading, "Zionist Legitimacy Grounded in Divine Restoration (Religious Post-1967 Interpretation)").
narrative_ontology:topic_domain(zionist_legitimacy_basis__religious_restoration_reading, "political/historical/religious").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__religious_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__religious_restoration_reading, '5cf7ccc6-124c-4ceb-820e-ee0e7963b1a8').
narrative_ontology:cs_kernel_codification('5cf7ccc6-124c-4ceb-820e-ee0e7963b1a8', fixed_text).
narrative_ontology:cs_authority_grounding('5cf7ccc6-124c-4ceb-820e-ee0e7963b1a8', lineage).
narrative_ontology:cs_interpretation_layer_present('5cf7ccc6-124c-4ceb-820e-ee0e7963b1a8').
narrative_ontology:cs_reading_relation('5cf7ccc6-124c-4ceb-820e-ee0e7963b1a8', zionist_legitimacy_basis__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('5cf7ccc6-124c-4ceb-820e-ee0e7963b1a8', zionist_legitimacy_basis__settler_colonial_reading, influences).
narrative_ontology:cs_axiom('5cf7ccc6-124c-4ceb-820e-ee0e7963b1a8', foundational, territorial_possession_divine_obligation).
narrative_ontology:cs_axiom_status(territorial_possession_divine_obligation, holdable).
narrative_ontology:cs_axiom_grounding('5cf7ccc6-124c-4ceb-820e-ee0e7963b1a8', territorial_possession_divine_obligation, deontological).
narrative_ontology:cs_axiom('5cf7ccc6-124c-4ceb-820e-ee0e7963b1a8', foundational, messianic_return_eschatological_mandate).
narrative_ontology:cs_axiom_status(messianic_return_eschatological_mandate, holdable).
narrative_ontology:cs_axiom_grounding('5cf7ccc6-124c-4ceb-820e-ee0e7963b1a8', messianic_return_eschatological_mandate, theological).
narrative_ontology:cs_reference_frame('5cf7ccc6-124c-4ceb-820e-ee0e7963b1a8', jewish_scriptural_covenant_to_land).
narrative_ontology:cs_drift_state('5cf7ccc6-124c-4ceb-820e-ee0e7963b1a8', post_1967_religious_radicalization, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5cf7ccc6-124c-4ceb-820e-ee0e7963b1a8', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, jewish_settlers).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_authority).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, palestinian_displaced_populations).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, muslim_christian_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_political_center).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, jewish_settlers).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_political_center).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__religious_restoration_reading, divine_promise_to_abraham).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__religious_restoration_reading, jewish_historical_claim_to_land).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__religious_restoration_reading, messianic_return_theology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Settlers claim both religious and historical entitlement to territorial possession; they receive land allocation, state resources, and institutional legitimacy. They also bear costs of ongoing security, economic sanctions, and diplomatic isolation. Exit is structured as theological apostasy—abandoning the territorial claim requires rejecting the divine mandate reading, which is identity-constitutive for many.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, jewish_settlers, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__religious_restoration_reading, jewish_settlers, payer).

% Religious and political leadership articulates the divine restoration frame, interprets scripture to support territorial maximalism, and enforces theological legitimacy through rabbinical authority, educational control, and political mobilization. Collects institutional authority and shapes Israeli state policy toward expansion.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_authority, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Bear land expropriation, displacement, settlement enclosure, and restricted access to resources. Political representation is excluded or marginalized. Exit would require accepting permanent exile or subordinate legal status within a state structured around Jewish sovereignty and theological belonging.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, palestinian_displaced_populations, payer,
    powerless, generational, trapped, regional).

% Inhabit territories subject to the same religious-nationalist claim; face second-class legal status, restricted property rights, and displacement pressure justified by the theological framing that privileges Jewish historical/religious claim. Can negotiate within subordinate categories but not exit the territorial frame.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, muslim_christian_minorities, payer,
    moderate, biographical, constrained, regional).

% Collects nationalist mobilization and international legitimacy claims grounded in the restoration narrative but also bears diplomatic costs, security escalation, and internal factionalization as the religious reading radicalizes and institutional secular authorities lose veto over settlement expansion. Constrained because formally disowning the divine-right frame would fracture the state's foundational legitimacy claims.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_political_center, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_political_center, payer).

% Judges the constraint through multiple competing lenses: human rights law (displacement as harm), indigenous rights frameworks (Palestinian autochthony), international law (occupation/annexation legality), religious pluralism norms (majoritarian theological exclusion), and postcolonial critique. Divided alignment prevents unified enforcement.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, international_observer_community, observer,
    institutional, biographical, analytical, global).

% Christian, Islamic, and secular humanist readings of the same territories yield incompatible legitimacy claims. These narratives are not seated parties but competing frameworks that contest the religious Zionist reading's exclusive authority to define historical truth and divine will.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, competing_religious_narratives, excluded,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(zionist_legitimacy_basis__religious_restoration_reading, competing_religious_narratives).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_authority).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__religious_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The religious restoration frame coordinates Jewish diaspora mobilization around territorial reconstitution: it transforms land acquisition and settler expansion into a religiously mandated project rather than optional political choice, binding previously secular and religious communities through shared theological narrative.
% TRANSFER_FUNCTION: Moves land and state resources (settlement subsidies, military protection, legal authority) from Palestinian and minority populations to Jewish settlers and religious-nationalist institutions; simultaneously moves theological authority and interpretive power from secular legal frameworks to rabbinical and religious leadership.
% ABSENT_VOICES: Palestinian nationalist movements are structurally excluded from the religious legitimacy framework—their historical presence and displacement are either erased (prior occupation narratives) or reframed as temporary residency subject to Jewish priority. Muslim and Christian theological readings of the same lands are marginalized or criminalized. Secular universalist critics within Israeli society are present but institutionally subordinated.
% DISAPPEARANCE_RATIONALE: If the religious restoration claim disappeared—i.e., if the theological reading no longer legitimated territorial maximalism—Israeli state expansion would face immediate legal and political reckoning: the secular national-liberation frame alone does not justify permanent territorial growth, and international law would reassert. Palestinian displacement and settlement policy would become clearly illegal under occupation law; regional conflict dynamics would shift fundamentally as the religious obligation frame no longer mobilizes settler behavior and state protection.
% FOUNDING_PROBLEM: Jewish historical trauma and diaspora vulnerability: the theological reading answers the existential question 'How do we restore Jewish sovereignty after centuries of exile and persecution?' by locating divine obligation to return to the land and rebuild the nation-state.
% FOUNDING_PROBLEM_CORROBORATION: Religious Zionist theology and Israeli state authorities affirm the founding problem and assert its ongoing urgency. Palestinian historians and international observers contest both the founding problem's framing and its status: they argue the problem was already substantially solved by the 1948/1967 establishment of Jewish political sovereignty, and that the theological reading now justifies continuous expansion rather than meeting a genuine founding need. Scholarly consensus outside religious Zionist circles (postcolonial studies, human rights bodies, international law experts) corroborates the 'contested' status: the founding problem has migrated into a legitimation narrative for territorial accumulation.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__religious_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__religious_restoration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__religious_restoration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__religious_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__religious_restoration_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__religious_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__religious_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__religious_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the religious restoration frame transforms territorial acquisition into moral obligation rather than optional political choice, enabling permanent land expropriation justified by divine mandate rather than negotiable power. Suppression is higher (0.72) because the theological authority structure suppresses alternative readings of the same scripture, Palestinian counter-narratives, and secular legal frameworks—suppression is structural, not merely coercive. Theater ratio is moderate-high (0.41) because the arrangement combines genuine coordination functions (diaspora mobilization, identity-binding) with extractive territorial accumulation: the theater tracks the ratio of mobilization rhetoric to actual justice/equity outcomes. Accessibility collapse is high (0.79) for the religious framework itself—once the divine mandate reading is understood, exit requires apostasy—but lower for secular and Palestinian alternatives that remain intellectually available, creating level-differentiated accessibility at structural (0.79 by 2024) vs. individual (0.64) levels. Resistance declined from 1967 to 2024 across organizational and class levels due to military suppression intensity and settler state entrenchment; individual resistance persists at 0.42, suggesting persistent micro-scale non-compliance despite macro-scale constraint consolidation. The measurements are authored at a single shared time grid so every metric appears at every interval checkpoint, avoiding OQ-105-style misalignment.
 *
 * PERSPECTIVAL GAP:
 *   From the religious Zionist agenda-setter seat, this is genuine coordination (binding diaspora, fulfilling divine purpose) with incidental territorial asymmetry. From the Palestinian victim seat, it is pure extraction masked by theological cover story. The secular Israeli beneficiary seat experiences both coordination and extraction together—nationalist mobilization is real, but costs (diplomatic isolation, military escalation, internal factionalization) are rising. The engine computes per-seat type divergence: what computes as tangled_rope from the settler/authority perspective (coordination + asymmetric extraction) may compute as snare from the Palestinian perspective (no coordination benefit, pure dispossession). The claim/metric gap is structural to the kernel contest: the religious reading CLAIMS coordination; the metrics describe substantially extractive operation with suppression rising faster than beneficiary coordination functions. That divergence is the measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish settlers occupy d ≈ 0.25 (low/beneficiary end despite identity-locked exit, because they collect land and institutional legitimacy without bearing core extraction costs). Religious Zionist authority d ≈ 0.1 (full beneficiary: they set the agenda, mobilize communities, collect institutional power, and face no real exit pressure). Palestinian displaced populations d ≈ 0.92 (near target end: trapped, structurally excluded from the theological framework, bearing land expropriation and settlement enclosure). Muslim/Christian minorities d ≈ 0.75 (high target end: constrained exit, subordinate legal status, displacement pressure). Secular Israeli political center d ≈ 0.55 (symmetric): they benefit from nationalist mobilization but bear escalating diplomatic and security costs; constrained exit because disowning the religious legitimacy frame would fracture state foundational claims. This directionality structure explains why the constraint computes as tangled_rope (coordination for settlers/authority + extraction from Palestinians/minorities) rather than pure snare: there is real coordination happening (diaspora mobilization, identity-binding), but it is asymmetrically paired with extraction from those who cannot participate in the coordination benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—Jewish vulnerability and diaspora survival—was substantially solved by the 1948/1967 establishment of Jewish political sovereignty and military security. The religious restoration reading persists not because the founding problem remains live, but because theological framing has migrated into justification for territorial maximalism and permanent occupation. This is textbook mandatrophy: the coordination function (diaspora mobilization) persists, but the founding mandate (security through return) has expired, replaced by expansion as the operative function. Theater ratio rising from 0.25 to 0.41 tracks this drift: more of the arrangement is now devoted to defending and expanding territorial claims (performance of sacred obligation) rather than meeting genuine coordination needs (shared identity, survival). The six-questions verdict 'founding_problem_status=contested' captures this: the religious Zionist leadership asserts the founding problem remains existentially live (security threats, Jewish survival at risk), while observers and secular critics argue the problem was solved and the arrangement now functions as territorial accumulation. The coercion grid shows rising suppression and stakes_inflation paired with declining resistance (organizational level dropping from 0.68 to 0.52), suggesting the suppression is effective—Palestinian organizational capacity to counter-narrate or resist has atrophied under settler state power. This is classic piton trajectory (atrophied coordination function, theaters maintaining the form), except the constraint retains enough beneficiary energy (settler mobilization, religious authority consolidation) and enough active enforcement (military suppression) to stay tangled_rope rather than fall to piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_reading_boundaries,
    'Is the religious restoration reading a transparent reading of Jewish scriptural sources, or a selective theological interpretation that emphasizes maximalist texts over reconciliation texts?',
    'Comparative textual analysis from non-Zionist Jewish scholars and Islamic/Christian scriptural traditions; historical documentation of which readings dominated at different periods (pre-1967 vs. post-1967); investigation of how rabbinical authority gatekeeps acceptable interpretations.',
    'If the reading is selective/constructed (rather than inevitable from scripture), it loses natural-law status and becomes a manufactured constraint whose extractiveness depends on authority monopoly over interpretation. If discovery of alternative readings within the tradition weakens the exclusive theological mandate, territorial claims become negotiable rather than sacred obligations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theological_reading_boundaries, conceptual, 'Whether the religious restoration reading is required by scripture or selected from competing Jewish traditions.').

omega_variable(
    messianic_temporality,
    'In the religious Zionist theology, is territorial maximalism a necessary precondition for messianic redemption, or a consequence/outcome that follows from it?',
    'Textual analysis of founding rabbinical sources; documentation of whether religious authority shifted the temporal relationship after 1967; comparison with pre-Zionist Jewish eschatology; analysis of which theological gates control whether expansion halts or continues indefinitely.',
    'If expansion is a precondition (must conquer to trigger redemption), the constraint is permanently extractive and becomes theologically harder to modify. If redemption is the goal and expansion is instrumental to it, then alternative means (spiritual return, restored sovereignty, religious authority without territorial growth) could substitute for physical expansion, opening exit paths.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_temporality, conceptual, 'Whether territorial maximalism is a necessary precondition or contingent outcome in religious Zionist eschatology.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) primarily structural (military control, legal barriers, resource denial) or internalized (Palestinians/minorities have absorbed the settler-state framing and no longer internally recognize alternative claims)?',
    'Post-constraint-removal thought experiment: if military suppression were removed, would organized Palestinian alternatives rapidly emerge, or has generations of displacement erased the organizational capacity to mount counter-narratives? Empirical tracking of Palestinian intellectual production, memory institutions, youth identity formation in diaspora vs. occupied territories.',
    'If suppression is primarily structural, removing military occupation would rapidly restore organized resistance and enable negotiation. If internalization is high, the constraint''s persistence depends less on coercive infrastructure than on identity fragmentation—a more durable extraction mechanism that persists even without active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression mechanism in the constraint''s operation.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the religious restoration reading''s core axiom (territorial possession as divine obligation) logically foreclose the settler-colonial reading''s analysis, or do both readings remain simultaneously available to different interpreters?',
    'Philosophical analysis: can an actor simultaneously hold (a) theological obligation to possess the land and (b) recognition that possession was achieved through colonial displacement of prior inhabitants? Do these claims logically exclude each other, or can one hold both—acknowledging the colonial mechanism while justifying it theologically?',
    'If they foreclose each other, then settlers cannot access settler-colonial critique without rejecting their theological foundation. If they coexist, then settlers could acknowledge the colonial structure while maintaining religious justification—a more complex contradictory consciousness that might enable negotiated exit rather than total identity/narrative collapse.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the religious and settler-colonial readings logically exclude each other or can coexist.').

omega_variable(
    false_summit_mountain_candidate,
    'Is the religious restoration reading grounded in timeless natural law (universal scripture, immutable divine will) or in a historical contingency (the 1967 intensification, specific rabbi-scholars'' choices to radicalize interpretation)?',
    'Historical documentation of when the religious maximalist reading became dominant (did it predate 1967, or was it selected/amplified post-1967 in response to territorial acquisition?); textual analysis of whether the reading was already present in pre-1948 Jewish theology or is a post-colonial construct; investigation of which authority figures explicitly shifted interpretation and when.',
    'If the reading is historically constructed (post-1967 intensification), then it is a false-summit mountain: it claims natural law status (divine obligation) but was actually constructed by beneficiaries to justify territorial gains. If the reading predates 1967 and is immutable, then it may be a genuine natural law (theological constants), though still highly contested. False-summit status would trigger mandatrophy resolution mechanisms and would reframe the constraint as tangled_rope masquerading as mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_mountain_candidate, empirical, 'Whether the religious restoration reading is a timeless theological constant or a historically constructed post-1967 interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__religious_restoration_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1967, 0.25).
narrative_ontology:measurement_basis(zion_tr_t1967, observed).
narrative_ontology:measurement(zion_tr_t1980, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1980, 0.29).
narrative_ontology:measurement_basis(zion_tr_t1980, observed).
narrative_ontology:measurement(zion_tr_t1995, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1995, 0.34).
narrative_ontology:measurement_basis(zion_tr_t1995, observed).
narrative_ontology:measurement(zion_tr_t2008, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2008, 0.38).
narrative_ontology:measurement_basis(zion_tr_t2008, observed).
narrative_ontology:measurement(zion_tr_t2018, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2018, 0.41).
narrative_ontology:measurement_basis(zion_tr_t2018, observed).
narrative_ontology:measurement(zion_tr_t2024, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2024, 0.41).
narrative_ontology:measurement_basis(zion_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1967, 0.52).
narrative_ontology:measurement_basis(zion_be_t1967, observed).
narrative_ontology:measurement(zion_be_t1980, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1980, 0.58).
narrative_ontology:measurement_basis(zion_be_t1980, observed).
narrative_ontology:measurement(zion_be_t1995, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1995, 0.63).
narrative_ontology:measurement_basis(zion_be_t1995, observed).
narrative_ontology:measurement(zion_be_t2008, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2008, 0.66).
narrative_ontology:measurement_basis(zion_be_t2008, observed).
narrative_ontology:measurement(zion_be_t2018, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2018, 0.68).
narrative_ontology:measurement_basis(zion_be_t2018, observed).
narrative_ontology:measurement(zion_be_t2024, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(zion_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1967, 0.55).
narrative_ontology:measurement_basis(zion_su_t1967, observed).
narrative_ontology:measurement(zion_su_t1980, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1980, 0.62).
narrative_ontology:measurement_basis(zion_su_t1980, observed).
narrative_ontology:measurement(zion_su_t1995, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1995, 0.68).
narrative_ontology:measurement_basis(zion_su_t1995, observed).
narrative_ontology:measurement(zion_su_t2008, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2008, 0.7).
narrative_ontology:measurement_basis(zion_su_t2008, observed).
narrative_ontology:measurement(zion_su_t2018, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2018, 0.72).
narrative_ontology:measurement_basis(zion_su_t2018, observed).
narrative_ontology:measurement(zion_su_t2024, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2024, 0.72).
narrative_ontology:measurement_basis(zion_su_t2024, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1967, tn=2024
narrative_ontology:measurement(zion_grid_01, zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse(class), 1967, 0.62).
narrative_ontology:measurement(zion_grid_02, zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse(class), 2024, 0.81).
narrative_ontology:measurement(zion_grid_03, zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse(individual), 1967, 0.48).
narrative_ontology:measurement(zion_grid_04, zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse(individual), 2024, 0.64).
narrative_ontology:measurement(zion_grid_05, zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse(organizational), 1967, 0.55).
narrative_ontology:measurement(zion_grid_06, zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse(organizational), 2024, 0.72).
narrative_ontology:measurement(zion_grid_07, zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse(structural), 1967, 0.68).
narrative_ontology:measurement(zion_grid_08, zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse(structural), 2024, 0.79).
narrative_ontology:measurement(zion_grid_09, zionist_legitimacy_basis__religious_restoration_reading, resistance(class), 1967, 0.71).
narrative_ontology:measurement(zion_grid_10, zionist_legitimacy_basis__religious_restoration_reading, resistance(class), 2024, 0.48).
narrative_ontology:measurement(zion_grid_11, zionist_legitimacy_basis__religious_restoration_reading, resistance(individual), 1967, 0.55).
narrative_ontology:measurement(zion_grid_12, zionist_legitimacy_basis__religious_restoration_reading, resistance(individual), 2024, 0.42).
narrative_ontology:measurement(zion_grid_13, zionist_legitimacy_basis__religious_restoration_reading, resistance(organizational), 1967, 0.68).
narrative_ontology:measurement(zion_grid_14, zionist_legitimacy_basis__religious_restoration_reading, resistance(organizational), 2024, 0.52).
narrative_ontology:measurement(zion_grid_15, zionist_legitimacy_basis__religious_restoration_reading, resistance(structural), 1967, 0.62).
narrative_ontology:measurement(zion_grid_16, zionist_legitimacy_basis__religious_restoration_reading, resistance(structural), 2024, 0.58).
narrative_ontology:measurement(zion_grid_17, zionist_legitimacy_basis__religious_restoration_reading, stakes_inflation(class), 1967, 0.65).
narrative_ontology:measurement(zion_grid_18, zionist_legitimacy_basis__religious_restoration_reading, stakes_inflation(class), 2024, 0.82).
narrative_ontology:measurement(zion_grid_19, zionist_legitimacy_basis__religious_restoration_reading, stakes_inflation(individual), 1967, 0.51).
narrative_ontology:measurement(zion_grid_20, zionist_legitimacy_basis__religious_restoration_reading, stakes_inflation(individual), 2024, 0.71).
narrative_ontology:measurement(zion_grid_21, zionist_legitimacy_basis__religious_restoration_reading, stakes_inflation(organizational), 1967, 0.52).
narrative_ontology:measurement(zion_grid_22, zionist_legitimacy_basis__religious_restoration_reading, stakes_inflation(organizational), 2024, 0.68).
narrative_ontology:measurement(zion_grid_23, zionist_legitimacy_basis__religious_restoration_reading, stakes_inflation(structural), 1967, 0.58).
narrative_ontology:measurement(zion_grid_24, zionist_legitimacy_basis__religious_restoration_reading, stakes_inflation(structural), 2024, 0.76).
narrative_ontology:measurement(zion_grid_25, zionist_legitimacy_basis__religious_restoration_reading, suppression(class), 1967, 0.58).
narrative_ontology:measurement(zion_grid_26, zionist_legitimacy_basis__religious_restoration_reading, suppression(class), 2024, 0.76).
narrative_ontology:measurement(zion_grid_27, zionist_legitimacy_basis__religious_restoration_reading, suppression(individual), 1967, 0.55).
narrative_ontology:measurement(zion_grid_28, zionist_legitimacy_basis__religious_restoration_reading, suppression(individual), 2024, 0.68).
narrative_ontology:measurement(zion_grid_29, zionist_legitimacy_basis__religious_restoration_reading, suppression(organizational), 1967, 0.52).
narrative_ontology:measurement(zion_grid_30, zionist_legitimacy_basis__religious_restoration_reading, suppression(organizational), 2024, 0.74).
narrative_ontology:measurement(zion_grid_31, zionist_legitimacy_basis__religious_restoration_reading, suppression(structural), 1967, 0.48).
narrative_ontology:measurement(zion_grid_32, zionist_legitimacy_basis__religious_restoration_reading, suppression(structural), 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__religious_restoration_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(zionist_legitimacy_basis__religious_restoration_reading, 0.12).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis__national_liberation_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis__settler_colonial_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, palestinian_nationalism__indigenous_return_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, israeli_settlement_expansion_legal_status).

% DUAL FORMULATION NOTE:
% The three constraint stories (religious_restoration_reading, national_liberation_reading, settler_colonial_reading) form a kernel family on the zionist_legitimacy_basis kernel. Each story specifies different ε values (religious: 0.68, national-liberation: lower due to genuine coordination, settler-colonial: higher due to pure extraction framing), different beneficiary/victim structures, and different structural mechanisms. The religious reading here makes territorial maximalism a religious obligation (accessibility collapse, identity lock); the national liberation reading treats territorial possession as decolonization right (reversible, negotiable); the settler-colonial reading treats it as ethno-state project enabled by colonial power (reframed as injustice). All three readings reference the same historical events but partition them into incompatible legitimacy structures. The network edges capture how each reading influences the others: the religious reading influences national liberation (radicalizing it toward territorial maximalism) and forecloses elements of settler-colonial framing (by claiming positive theological mandate rather than admitting to colonial power politics). The sibling readings are NOT part of this constraint—they are separate JSON files linked by the network edges here and in their own files' edges back to this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(zionist_legitimacy_basis__religious_restoration_reading, powerful, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
