% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__religious_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Religious Zionist Legitimacy: Divine Promise and Territorial Maximalism
 *   domain: political_history/nationalism/religious_authority
 *
 * SUMMARY:
 *   This constraint story models the religious Zionist reading of Zionism as
 *   instantiated by movement institutions and state structures post-1967. The
 *   reading holds that Jewish sovereignty over Eretz Yisrael (the Greater
 *   Land of Israel, including territories occupied in 1967) fulfills divine
 *   covenant and messianic obligation, making territorial compromise
 *   theologically impermissible. This reading became institutional force in
 *   Israeli politics through rabbinical authority (Gush Emunim movement,
 *   yeshiva networks), settlement policy, and state legitimacy claims. The
 *   constraint CLAIMS to be rope (genuine coordination of Jewish peoplehood
 *   and state security) while the authored metrics describe substantially
 *   extractive, actively enforced operation (suppression of Palestinian land
 *   claims, military occupation, resource transfer). The claim/metric
 *   divergence is deliberate: the religious Zionist movement's own framing
 *   positions the reading as coordinate; the measurement system records the
 *   displacement of Palestinian populations and the theological suppression
 *   of competing claims. The engine computes this divergence; the story does
 *   not reconcile it.
 *
 * KEY AGENTS:
 *   - Religious Zionist movement: agenda-setter, identity-locked, determines theological framework for territory
 *   - Israeli state apparatus: institutional beneficiary and agenda-setter coupling, enforces the reading through military and legal machinery
 *   - Palestinian Arabs in occupied territories: powerless, trapped targets, bear the primary cost of land dispossession and subordination
 *   - Palestinian Arab Israeli citizens: moderate power, constrained exit, subordinated within state claiming to act on behalf of exclusive Jewish peoplehood
 *   - Secular Jewish Israelis: powerful beneficiaries with mobile exit options, experience tension between state membership and disagreement with theological mandate
 *   - International law regime: structurally excluded from the reading's epistemic framework (theological obligation vs. universal rights)
 *   - Diaspora Jewish communities: observers with analytical distance but material investment in the reading's political success
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__religious_restoration_reading, 0.78).
domain_priors:suppression_score(zionist_legitimacy_basis__religious_restoration_reading, 0.71).
domain_priors:theater_ratio(zionist_legitimacy_basis__religious_restoration_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__religious_restoration_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__religious_restoration_reading, "Religious Zionist Legitimacy: Divine Promise and Territorial Maximalism").
narrative_ontology:topic_domain(zionist_legitimacy_basis__religious_restoration_reading, "political_history/nationalism/religious_authority").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__religious_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__religious_restoration_reading, '36d87557-229f-4d66-bcef-9a52c82034a0').
narrative_ontology:cs_kernel_codification('36d87557-229f-4d66-bcef-9a52c82034a0', fixed_text).
narrative_ontology:cs_authority_grounding('36d87557-229f-4d66-bcef-9a52c82034a0', lineage).
narrative_ontology:cs_interpretation_layer_present('36d87557-229f-4d66-bcef-9a52c82034a0').
narrative_ontology:cs_reading_relation('36d87557-229f-4d66-bcef-9a52c82034a0', zionist_legitimacy_basis__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('36d87557-229f-4d66-bcef-9a52c82034a0', zionist_legitimacy_basis__settler_colonial_reading, influences).
narrative_ontology:cs_axiom('36d87557-229f-4d66-bcef-9a52c82034a0', foundational, eretz_yisrael_jewish_divine_covenant).
narrative_ontology:cs_axiom_status(eretz_yisrael_jewish_divine_covenant, holdable).
narrative_ontology:cs_axiom_grounding('36d87557-229f-4d66-bcef-9a52c82034a0', eretz_yisrael_jewish_divine_covenant, deontological).
narrative_ontology:cs_axiom('36d87557-229f-4d66-bcef-9a52c82034a0', foundational, territorial_maximalism_theologically_nonnegotiable).
narrative_ontology:cs_axiom_status(territorial_maximalism_theologically_nonnegotiable, holdable).
narrative_ontology:cs_axiom_grounding('36d87557-229f-4d66-bcef-9a52c82034a0', territorial_maximalism_theologically_nonnegotiable, deontological).
narrative_ontology:cs_reference_frame('36d87557-229f-4d66-bcef-9a52c82034a0', torah_land_covenant_restoration).
narrative_ontology:cs_drift_state('36d87557-229f-4d66-bcef-9a52c82034a0', contemporary_post_1967_territorial_control, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('36d87557-229f-4d66-bcef-9a52c82034a0', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_movement).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, jewish_national_collective_identity).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, palestinian_arabs_in_occupied_territories).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, palestinian_arab_israeli_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, secular_jewish_israelis).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, secular_jewish_israelis).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__religious_restoration_reading, torah_land_covenant).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__religious_restoration_reading, messianic_return_doctrine).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__religious_restoration_reading, jewish_indigenous_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and promulgates the reading that Jewish sovereignty over Eretz Yisrael (Greater Israel) fulfills divine covenant and messianic obligation. Sets the theological framework through yeshiva education, rabbinical authority, and political mobilization. Their identity and institutional survival depend on territorial expansion as religious mandate, not negotiable political preference. After 1967, this movement becomes institutional force driving settlement policy by claiming every territory within 1967 borders and beyond is theologically non-negotiable.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_movement, agenda_setter,
    organized, generational, identity_locked, global).

% Adopts and institutionalizes the religious Zionist reading as state legitimacy claim, particularly post-1967. Uses the theological framework to justify territorial control, settlement policy, military operations, and resistance to international law constraints. State and religious movement become coupled: the state gains theological legitimacy; the movement gains state enforcement machinery.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, israeli_state_apparatus, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__religious_restoration_reading, israeli_state_apparatus, agenda_setter).

% A non-agent entity: the abstract collective identity narrative that a return to Zion fulfills Jewish peoplehood and historical destiny. This proposition benefits from the constraint by being vindicated and naturalized through state action and theological authority. No agent 'collects' from this identity, but the reading's operation sustains it.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, jewish_national_collective_identity, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(zionist_legitimacy_basis__religious_restoration_reading, jewish_national_collective_identity).

% Bear the primary cost of the reading's application: land dispossession, settlement expansion, military administration, economic subordination. Under the reading, their claims to the same land are theologically illegitimate—they are read as obstacles to Jewish restoration rather than agents with standing claims. They are trapped by military occupation; exit means abandonment of homeland or permanent displacement.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, palestinian_arabs_in_occupied_territories, payer,
    powerless, biographical, trapped, regional).

% Inhabit the Israeli state but are structurally subordinated by a reading that defines the state as exclusively Jewish and the land as Jewish by divine right. They are citizens without full membership in the collective the constraint serves. Their exit options are limited: they can leave (forfeiting property, social networks, national belonging) or remain in a subordinated status within a state that reads their presence as anomalous.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, palestinian_arab_israeli_citizens, payer,
    moderate, biographical, constrained, national).

% Benefit from the state's existence and the security framework the reading legitimizes, but do not endorse the reading itself. They experience tension between state membership (which claims to act on their behalf) and disagreement with the theological mandate. Their exit options are relatively open (emigration is possible); their secondary role as partial payer reflects conscription, taxation for military occupation, and moral complicity in state actions they contest.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, secular_jewish_israelis, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__religious_restoration_reading, secular_jewish_israelis, payer).

% Is structurally excluded from the reading's internal logic: international law operates on secular sovereignty and universal human rights; the religious restoration reading operates on theological obligation and Jewish peoplehood exclusivity. These frameworks cannot adjudicate each other from within. The regime's exclusion is not organizational (it attempts to intervene) but epistemic—the reading brackets international law as non-binding authority on matters of divine covenant.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, international_law_and_human_rights_regime, excluded,
    institutional, generational, analytical, universal).

% Observe and debate the reading from outside Israel. Some embrace it as validating Jewish peoplehood and historical return; others reject it as instrumentalizing religion for territorial expansion. They have analytical distance but material investment (political support, fundraising, identity narrative). Their seat is one of commentary, not enforcement, though their political mobilization affects state capacity.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, diaspora_jewish_communities, observer,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_movement).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__religious_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish collective identity and national belonging around a shared narrative of historical return and divine restoration; legitimizes the Jewish state as fulfillment of religious obligation rather than as a political arrangement subject to negotiation; integrates religious authority (rabbinical interpretation of Torah land covenant) with state power.
% TRANSFER_FUNCTION: Transfers land, political authority, security prerogatives, and territorial claims from Palestinian Arabs to Jewish Israelis under theological interpretation that the land is Jewish by divine covenant and that Jewish restoration is religiously mandated. Moves interpretive authority over territorial legitimacy from international law and secular political negotiation to religious authority (rabbinical interpretation of Torah). Transfers costs of occupation, dispossession, and political subordination to Palestinians; transfers validation and institutional authority to the religious Zionist movement.
% ABSENT_VOICES: Palestinian theological voices (Islamic and Christian interpretations of the same land as divinely promised to Muslim and Christian communities) are epistemically excluded from the reading's internal logic—the reading operates within a closed Jewish textual and theological framework where other religious claims do not have standing. Secular international law and human rights frameworks are bracketed as non-binding authority on questions of divine covenant. Diaspora Jewish communities that reject or reframe the reading are marginalized within the state's institutional hierarchy. Secular Jewish Israelis who deny the theological mandate are citizens without full membership in the collective the state claims to represent.
% DISAPPEARANCE_RATIONALE: If the religious restoration reading vanished and was replaced by an alternative legitimacy frame (secular national liberation, one-state solution, federalism, or alternative religious interpretation), the state would lose theological grounds for settlement expansion and territorial maximalism; would face pressure to justify territorial control through secular political negotiation or international law; would confront competing Palestinian land claims on equal epistemic footing rather than being able to dismiss them as theologically illegitimate; and would need to renegotiate territorial boundaries, property rights, and political arrangements through mechanisms not governed by religious interpretation.
% FOUNDING_PROBLEM: Jewish historical persecution, diaspora fragmentation, and the absence of Jewish political sovereignty and territorial rootedness—read theologically as exile (galut) from the land of covenant and solved by return (shiva) to Zion as restoration of the proper relationship between Jewish peoplehood and their ancestral land.
% FOUNDING_PROBLEM_CORROBORATION: Religious Zionist authorities attest the founding problem is perpetually live—Jewish continuity and redemption require territorial sovereignty over Eretz Yisrael (the whole land) as non-negotiable religious obligation; the state's existence is incomplete without full territorial realization. Secular Jewish Israelis and international observers attest the founding problem (Jewish homelessness and persecution) was substantially solved by the state's establishment in 1948; contemporary territorial expansion is pursued for strategic reasons disguised as religious mandate, not because the founding problem persists. Palestinian and Israeli human rights organizations attest the founding problem no longer justifies dispossession, and that invoking it to justify ongoing occupation is a distortion of its original meaning.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__religious_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__religious_restoration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__religious_restoration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__religious_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__religious_restoration_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high and rising (0.58→0.78 over the interval) because the reading's application transfers land, resources, political standing, and security burden from Palestinians to Jewish Israelis under theological authority that Palestinians cannot contest within the reading's own logic. Suppression is high (0.71) and rising because the constraint's persistence depends on actively enforcing the theological framework against competing Palestinian land claims and international law frameworks. Theater is moderate (0.42) and rising because a growing share of the reading's enforcement activity defends settlement expansion and territorial control rather than addressing the founding problem (Jewish homelessness post-Holocaust), which was substantially solved by the state's existence. The measurement series tracks extraction accumulation, suggesting the reading functions increasingly as rent-collection disguised as religious obligation. The coercion grid shows asymmetric level-resolved dynamics: structural-level suppression and stakes inflation rise (system-wide enforcement tightens), while individual-level resistance remains high but uncoordinated (Palestinians maintain resistance claims despite growing suppression). The grid models a constraint that intensifies enforcement across all levels while failing to suppress class-level and individual-level resistance—a diagnostic pattern for a structurally vulnerable extraction mechanism that depends on continued coercion.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the agenda-setter seat experience fundamentally different types and should compute differently. The agenda-setter (religious Zionist movement, Israeli state) experiences the reading as rope: genuine coordination solving a real problem (Jewish survivorship) with net benefit. The payer seat (Palestinians) experiences it as snare: the coordination story is justification for pure extraction (land dispossession, political subordination). These are not different observations of the same constraint; they are different constraints instantiated by different readings of the same kernel. The framework's job is to measure this divergence, not to adjudicate between readings.
 *
 * DIRECTIONALITY LOGIC:
 *   The religious Zionist movement has directionality near the full-beneficiary end (d approaching 0.0): the reading validates their identity, their institutional authority, their claim to land, and their political power. No cost accrues to them; the constraint sustains them. The Israeli state apparatus sits near moderate (d near 0.3-0.4): it benefits from the reading's legitimacy and the movement's mobilization, but secular state actors experience costs (conscription, taxation, moral complicity, international isolation). Palestinian Arabs in occupied territories have directionality at the full-target end (d approaching 1.0): they bear all costs (dispossession, subordination, military occupation) and collect no benefits; their exit is trapped. Palestinian Arab Israeli citizens sit near the target end (d near 0.7-0.8): they are subordinated by the reading's definition of Jewish exclusivity, but retain formal citizenship and some legal protections. Secular diaspora Jewish communities sit near the beneficiary end (d near 0.2): they benefit from Jewish state legitimacy but do not bear the occupation's costs directly. International law regime is analytical (d = 0.5): it has no direct interest in the outcome but is structurally excluded from adjudicating the dispute.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jewish homelessness and peoplehood continuity post-Holocaust) was substantially solved by the state's establishment in 1948. The reading's persistence post-1967 indicates mandatrophy: the original coordination function (ensuring Jewish peoplehood's survival and security) no longer requires territorial expansion into 1967-occupied territories—those territories were conquered in a war framed as defensive, not as necessary for Jewish survival. The reading's subsequent application to justify settlement expansion and territorial maximalism suggests the mechanism persists because it benefits a specific movement and state faction (the religious Zionist coalition) rather than because the founding problem remains live. The theater_ratio rising from 0.22 to 0.42 over the interval indicates increasing performative maintenance: the reading's enforcement machinery increasingly defends settlement expansion rather than addressing historical persecution or ensuring security. The resistance measurements show stable high resistance across levels, indicating the constraint is not solving an underlying coordination problem but is being enforced against persistent opposition. The mandatrophy pattern suggests the constraint should be reclassified as piton (degraded coordination maintained by inertia and faction interest) rather than rope or tangled rope—but the current classification as tangled rope reflects the reading's own internal logic: it coordinates Jewish collective identity AND extracts from Palestinians through the same theological mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_obligation_vs_political_strategy,
    'Is the reading''s theological mandate for territorial maximalism genuine religious belief held by the movement''s constituencies, or post-hoc theological justification for political-strategic land acquisition?',
    'Longitudinal analysis of rabbinical texts, settlement rhetoric, and movement internal debates: if the theological mandate predates 1967 territorial gains consistently across primary sources, the obligation is constitutive; if theological arguments intensify post-facto to justify realized territorial gains, the relationship is reversed (politics generating theology, not theology driving politics).',
    'If authentic obligation: the constraint''s persistence is driven by deeply held identity commitments, making negotiated territorial compromise structurally impossible for the movement. If post-hoc justification: the constraint could be abandoned if political strategy shifted, suggesting the extraction is instrumental rather than constitutive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_obligation_vs_political_strategy, empirical, 'Whether the religious mandate is foundational belief or strategic legitimation.').

omega_variable(
    reading_vs_sibling_foreclosure,
    'Does the religious restoration reading logically foreclose the national liberation reading and the settler colonial reading within a single coherent framework, or do all three coexist as competing interpretive frames held by different parties?',
    'Textual and institutional analysis: if the three readings operate in hermetically sealed discourse communities with no common adjudication criterion, they coexist; if one reading''s core claim directly contradicts another''s (e.g., the settler colonial reading asserts ''Jewish indigenous claim is historically false'' while the restoration reading asserts ''Jewish indigenous claim is theologically non-negotiable''), foreclosure obtains only if both claims enter the same framework, which would require a meta-framework to judge between them.',
    'If coexist_with: the constraint is one voice in an ongoing kernel contest; classification of competing seats depends on which reading they adopt, and the engine computes per-reading type. If forecloses: one reading has structural grounds to eliminate others, which would require institutional power to enforce (and would itself be extractive suppression of alternatives).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_vs_sibling_foreclosure, conceptual, 'Logical relationship between this reading and its siblings in the kernel.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of Palestinian resistance and alternative land claims structurally external (military occupation, legal prohibition, economic dependency) or substantially internalized through education, religious authority narratives, and identity fusion with the state?',
    'Post-exit trajectory analysis: if Palestinian populations removed from suppressive structures maintain their land claims and resist, suppression is primarily structural; if they internalize the reading''s premises that Palestinian claims are illegitimate and Jewish claims are non-negotiable, suppression is partially internalized and persists after structural constraints are removed.',
    'If structural: the constraint depends on active enforcement infrastructure; removing occupation machinery could enable alternative arrangements. If internalized: the constraint persists through belief systems and identity commitments; removal of structural suppression would not automatically enable negotiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression operates through external coercion or internalized belief.').

omega_variable(
    kernel_contest_reading_identity,
    'This constraint instantiates the religious restoration reading of the zionist_legitimacy_basis kernel. The sibling readings (national_liberation_reading, settler_colonial_reading) generate different constraint stories with different epsilon values and beneficiary/victim structures. Does the committer axis adequately specify what makes this reading distinct from its siblings?',
    'The reading_relations and axioms fields in cs_structure declare the structural distinctiveness: this reading''s foundational axiom is that Jewish sovereignty over Eretz Yisrael fulfills divine covenant and is theologically non-negotiable. The national_liberation reading asserts Jewish peoplehood''s right to self-determination on secular grounds; the settler_colonial reading asserts the arrangement is extractive displacement regardless of its justification. These are genuinely different structural claims with different epsilon values (restoration reading: high extraction + coordination = tangled rope; liberation reading: moderate extraction + strong coordination = rope; settler colonial reading: pure extraction = snare).',
    'The reading identity is established by the axiom differentiation. If the three readings were collapsed into one story with an observable-selection parameter, the constraint would be under-determined and the type classification would depend on measurement choice (Constraint Identity and the ε-Invariance Principle violation). Keeping them as three separate stories linked by network.affects_constraints preserves the measurement integrity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_reading_identity, conceptual, 'Confirmation that this reading is a coherent, ε-invariant constraint distinct from its kernel siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__religious_restoration_reading, 1967, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1967, 0.22).
narrative_ontology:measurement_basis(zion_tr_t1967, observed).
narrative_ontology:measurement(zion_tr_t1978, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1978, 0.28).
narrative_ontology:measurement_basis(zion_tr_t1978, observed).
narrative_ontology:measurement(zion_tr_t1987, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 1987, 0.33).
narrative_ontology:measurement_basis(zion_tr_t1987, observed).
narrative_ontology:measurement(zion_tr_t2000, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement_basis(zion_tr_t2000, observed).
narrative_ontology:measurement(zion_tr_t2013, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2013, 0.4).
narrative_ontology:measurement_basis(zion_tr_t2013, observed).
narrative_ontology:measurement(zion_tr_t2026, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 2026, 0.42).
narrative_ontology:measurement_basis(zion_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1967, 0.58).
narrative_ontology:measurement_basis(zion_be_t1967, observed).
narrative_ontology:measurement(zion_be_t1978, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1978, 0.64).
narrative_ontology:measurement_basis(zion_be_t1978, observed).
narrative_ontology:measurement(zion_be_t1987, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 1987, 0.68).
narrative_ontology:measurement_basis(zion_be_t1987, observed).
narrative_ontology:measurement(zion_be_t2000, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2000, 0.73).
narrative_ontology:measurement_basis(zion_be_t2000, observed).
narrative_ontology:measurement(zion_be_t2013, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2013, 0.76).
narrative_ontology:measurement_basis(zion_be_t2013, observed).
narrative_ontology:measurement(zion_be_t2026, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 2026, 0.78).
narrative_ontology:measurement_basis(zion_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1967, 0.54).
narrative_ontology:measurement_basis(zion_su_t1967, observed).
narrative_ontology:measurement(zion_su_t1978, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1978, 0.6).
narrative_ontology:measurement_basis(zion_su_t1978, observed).
narrative_ontology:measurement(zion_su_t1987, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 1987, 0.64).
narrative_ontology:measurement_basis(zion_su_t1987, observed).
narrative_ontology:measurement(zion_su_t2000, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement_basis(zion_su_t2000, observed).
narrative_ontology:measurement(zion_su_t2013, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2013, 0.7).
narrative_ontology:measurement_basis(zion_su_t2013, observed).
narrative_ontology:measurement(zion_su_t2026, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(zion_su_t2026, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1967, tn=2026
narrative_ontology:measurement(zion_grid_01, zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse(class), 1967, 0.38).
narrative_ontology:measurement(zion_grid_02, zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse(class), 2026, 0.71).
narrative_ontology:measurement(zion_grid_03, zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse(individual), 1967, 0.42).
narrative_ontology:measurement(zion_grid_04, zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse(individual), 2026, 0.75).
narrative_ontology:measurement(zion_grid_05, zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse(organizational), 1967, 0.45).
narrative_ontology:measurement(zion_grid_06, zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse(organizational), 2026, 0.62).
narrative_ontology:measurement(zion_grid_07, zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse(structural), 1967, 0.52).
narrative_ontology:measurement(zion_grid_08, zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse(structural), 2026, 0.68).
narrative_ontology:measurement(zion_grid_09, zionist_legitimacy_basis__religious_restoration_reading, resistance(class), 1967, 0.72).
narrative_ontology:measurement(zion_grid_10, zionist_legitimacy_basis__religious_restoration_reading, resistance(class), 2026, 0.74).
narrative_ontology:measurement(zion_grid_11, zionist_legitimacy_basis__religious_restoration_reading, resistance(individual), 1967, 0.78).
narrative_ontology:measurement(zion_grid_12, zionist_legitimacy_basis__religious_restoration_reading, resistance(individual), 2026, 0.73).
narrative_ontology:measurement(zion_grid_13, zionist_legitimacy_basis__religious_restoration_reading, resistance(organizational), 1967, 0.64).
narrative_ontology:measurement(zion_grid_14, zionist_legitimacy_basis__religious_restoration_reading, resistance(organizational), 2026, 0.71).
narrative_ontology:measurement(zion_grid_15, zionist_legitimacy_basis__religious_restoration_reading, resistance(structural), 1967, 0.68).
narrative_ontology:measurement(zion_grid_16, zionist_legitimacy_basis__religious_restoration_reading, resistance(structural), 2026, 0.72).
narrative_ontology:measurement(zion_grid_17, zionist_legitimacy_basis__religious_restoration_reading, stakes_inflation(class), 1967, 0.62).
narrative_ontology:measurement(zion_grid_18, zionist_legitimacy_basis__religious_restoration_reading, stakes_inflation(class), 2026, 0.81).
narrative_ontology:measurement(zion_grid_19, zionist_legitimacy_basis__religious_restoration_reading, stakes_inflation(individual), 1967, 0.58).
narrative_ontology:measurement(zion_grid_20, zionist_legitimacy_basis__religious_restoration_reading, stakes_inflation(individual), 2026, 0.79).
narrative_ontology:measurement(zion_grid_21, zionist_legitimacy_basis__religious_restoration_reading, stakes_inflation(organizational), 1967, 0.55).
narrative_ontology:measurement(zion_grid_22, zionist_legitimacy_basis__religious_restoration_reading, stakes_inflation(organizational), 2026, 0.78).
narrative_ontology:measurement(zion_grid_23, zionist_legitimacy_basis__religious_restoration_reading, stakes_inflation(structural), 1967, 0.48).
narrative_ontology:measurement(zion_grid_24, zionist_legitimacy_basis__religious_restoration_reading, stakes_inflation(structural), 2026, 0.72).
narrative_ontology:measurement(zion_grid_25, zionist_legitimacy_basis__religious_restoration_reading, suppression(class), 1967, 0.58).
narrative_ontology:measurement(zion_grid_26, zionist_legitimacy_basis__religious_restoration_reading, suppression(class), 2026, 0.77).
narrative_ontology:measurement(zion_grid_27, zionist_legitimacy_basis__religious_restoration_reading, suppression(individual), 1967, 0.62).
narrative_ontology:measurement(zion_grid_28, zionist_legitimacy_basis__religious_restoration_reading, suppression(individual), 2026, 0.82).
narrative_ontology:measurement(zion_grid_29, zionist_legitimacy_basis__religious_restoration_reading, suppression(organizational), 1967, 0.52).
narrative_ontology:measurement(zion_grid_30, zionist_legitimacy_basis__religious_restoration_reading, suppression(organizational), 2026, 0.71).
narrative_ontology:measurement(zion_grid_31, zionist_legitimacy_basis__religious_restoration_reading, suppression(structural), 1967, 0.48).
narrative_ontology:measurement(zion_grid_32, zionist_legitimacy_basis__religious_restoration_reading, suppression(structural), 2026, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__religious_restoration_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(zionist_legitimacy_basis__religious_restoration_reading, 0.12).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis__national_liberation_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis__settler_colonial_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, palestinian_national_legitimacy_basis__indigeneity_claim).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, israeli_settlement_policy_enforcement).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, international_law_framework__territorial_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the zionist_legitimacy_basis kernel, decomposed per the ε-invariance principle (OQ-26, DP-001). The three readings (national_liberation, religious_restoration, settler_colonial) are separate constraints with different epsilon values, different beneficiary/victim structures, and different epistemic grounds. They are linked via network.affects_constraints to model the kernel relationship: the religious restoration reading influences the settler colonial reading's visibility and analytical relevance; the national liberation reading provides an alternative legitimacy framework that the religious reading competes with. They do not foreclose each other (all three remain live in contemporary discourse) but coexist as competing interpretations held by different parties. Constraint stories for the sibling readings are separate files with their own structural data and metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
