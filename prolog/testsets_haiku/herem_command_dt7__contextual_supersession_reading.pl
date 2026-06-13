% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__contextual_supersession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: herem_command_dt7__contextual_supersession_reading
 *   human_readable: Herem Command (Contextual Supersession Reading): Historically-Bounded Settlement Directive
 *   domain: religious/ethical/hermeneutical
 *
 * SUMMARY:
 *   This constraint instantiates the contextual-supersession reading of herem
 *   (Deuteronomy 7 and related conquest narratives). Under this reading,
 *   herem was a historically-bounded directive for ancient Israel's
 *   settlement period (circa 1200–1000 BCE), morally superseded by prophetic
 *   universalism (Jonah, Isaiah 45–49) and definitively by Christian covenant
 *   theology (Acts 10, Galatians 2–3, Matthew 28). The constraint is a
 *   scaffold: it has a declared sunset clause (the reading asserts
 *   supersession is now complete), its original function (consolidation
 *   during existential settlement threat) is fulfilled and no longer
 *   operative, and contemporary enforcement of herem-derived boundaries
 *   (especially intermarriage restrictions) is treated as vestigial
 *   fundamentalist theater rather than as a live ethical imperative. The
 *   measurement series shows extractiveness and suppression declining over
 *   the interval (as the supersession reading gains institutional authority
 *   and fundamentalist enforcement becomes increasingly marginal), while
 *   theater ratio rises (enforcement becomes more performative as it loses
 *   legitimacy). This is the only reading of the herem kernel that the
 *   measurement direction supports—durable-separation would show opposite
 *   trajectories (rising suppression and extractiveness), and
 *   allegorical-displacement would show theater rising while suppression
 *   stayed stable.
 *
 * KEY AGENTS:
 *   - Biblical prophetic tradition (institutional agenda-setter): interprets herem as historically-bounded and superseded
 *   - Christian theological authority (institutional agenda-setter and beneficiary): doctrine of covenant supersession legitimates the reading
 *   - Ancient Israel settlement period (institutional beneficiary, now historical): original coordination function
 *   - Fundamentalist enforcer communities (organized payer, identity-locked): maintain enforcement of boundary restriction despite supersession consensus
 *   - Individuals coerced by intermarriage restriction (powerless payer, identity-locked): bear suppression costs
 *   - Historical scholarship consensus (institutional beneficiary): provides external corroboration of the historicization frame
 *   - Durable-separation adherents (organized excluded): hold the rival reading and are not heard in mainline discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__contextual_supersession_reading, 0.38).
domain_priors:suppression_score(herem_command_dt7__contextual_supersession_reading, 0.42).
domain_priors:theater_ratio(herem_command_dt7__contextual_supersession_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__contextual_supersession_reading, scaffold).
narrative_ontology:human_readable(herem_command_dt7__contextual_supersession_reading, "Herem Command (Contextual Supersession Reading): Historically-Bounded Settlement Directive").
narrative_ontology:topic_domain(herem_command_dt7__contextual_supersession_reading, "religious/ethical/hermeneutical").

domain_priors:requires_active_enforcement(herem_command_dt7__contextual_supersession_reading).
narrative_ontology:has_sunset_clause(herem_command_dt7__contextual_supersession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__contextual_supersession_reading, 'c16559a2-108d-4643-8975-7937a4afb434').
narrative_ontology:cs_kernel_codification('c16559a2-108d-4643-8975-7937a4afb434', formalized).
narrative_ontology:cs_authority_grounding('c16559a2-108d-4643-8975-7937a4afb434', lineage).
narrative_ontology:cs_interpretation_layer_present('c16559a2-108d-4643-8975-7937a4afb434').
narrative_ontology:cs_reading_relation('c16559a2-108d-4643-8975-7937a4afb434', herem_command_dt7__durable_separation_reading, forecloses).
narrative_ontology:cs_reading_relation('c16559a2-108d-4643-8975-7937a4afb434', herem_command_dt7__allegorical_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('c16559a2-108d-4643-8975-7937a4afb434', foundational, settlement_consolidation_problem_is_solved).
narrative_ontology:cs_axiom_status(settlement_consolidation_problem_is_solved, holdable).
narrative_ontology:cs_axiom_grounding('c16559a2-108d-4643-8975-7937a4afb434', settlement_consolidation_problem_is_solved, empirically_contingent).
narrative_ontology:cs_axiom('c16559a2-108d-4643-8975-7937a4afb434', foundational, prophetic_universalism_supersedes_boundary_restriction).
narrative_ontology:cs_axiom_status(prophetic_universalism_supersedes_boundary_restriction, holdable).
narrative_ontology:cs_axiom_grounding('c16559a2-108d-4643-8975-7937a4afb434', prophetic_universalism_supersedes_boundary_restriction, deontological).
narrative_ontology:cs_axiom('c16559a2-108d-4643-8975-7937a4afb434', secondary, divine_mandate_can_expire_when_founding_problem_is_resolved).
narrative_ontology:cs_axiom_status(divine_mandate_can_expire_when_founding_problem_is_resolved, holdable).
narrative_ontology:cs_axiom_grounding('c16559a2-108d-4643-8975-7937a4afb434', divine_mandate_can_expire_when_founding_problem_is_resolved, deontological).
narrative_ontology:cs_reference_frame('c16559a2-108d-4643-8975-7937a4afb434', settlement_consolidation_necessity).
narrative_ontology:cs_drift_state('c16559a2-108d-4643-8975-7937a4afb434', contemporary_post_prophetic_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('c16559a2-108d-4643-8975-7937a4afb434', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__contextual_supersession_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, ancient_israel_settlement_consolidation).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, fundamentalist_enforcer_communities).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, individuals_coerced_by_intermarriage_restriction).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, christian_theological_authority).
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, historical_scholarship_consensus).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, prophetic_universalism_doctrine).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, christian_covenant_supersession).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and transmits the reading that herem was historically-bounded and has been morally superseded by universal prophetic vision. Sets the interpretive frame through textual argument (Jonah's universal divine concern, Isaiah 45–49, Malachi's affirmation of worship from the nations) and historical scholarship. Maintains this reading as authoritative doctrine in mainline Christian and Reform Jewish institutions. Bears no extraction cost; benefits from the institutional authority this reading provides.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, biblical_prophetic_tradition, agenda_setter,
    institutional, civilizational, analytical, universal).

% Doctrine that Christ's covenant supersedes earlier covenantal boundaries; herem is reinterpreted as a now-completed historical phase in salvation history. By this reading, the moral force of herem has been explicitly revoked in the New Testament. Christian teaching integrates the herem texts into a narrative arc that ends in supersession, then uses that narrative to legitimate Christian universalism and condemn fundamentalist boundary restriction. This reading vindicates Christian theological authority and allows Christian institutions to distance themselves from the violence of the conquest narratives.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, christian_theological_authority, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__contextual_supersession_reading, christian_theological_authority, agenda_setter).

% Academic consensus (biblical archaeology, form-critical scholarship, comparative ancient Near Eastern study) supports the historicization of herem: the command reflects Iron Age settlement strategy and has been superseded by the prophetic and Christian revisions of the tradition. This scholarly reading vindicates the contextual-supersession frame and provides external, non-theological corroboration of the historical narrative. Scholars benefit from the intellectual coherence the supersession reading provides and from the distance it creates from the violence of the literal reading.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, historical_scholarship_consensus, beneficiary,
    institutional, generational, analytical, universal).

% Groups that reject the supersession reading and maintain herem-derived boundaries (especially strict intermarriage prohibitions) as timeless divine law. They experience the contextual-supersession reading as a direct threat to their interpretive authority and identity coherence. Their enforcement is costly: requires ongoing institutional machinery (courts, councils), preaching against assimilation, expulsion or disfellowshipping of rule-breakers, surveillance of member behavior, and active resistance to the broader culture's acceptance of intermarriage. The gains from enforcement (institutional authority, social coherence, reproductive control) flow to the enforcer communities, but the costs (maintaining enforcement machinery against declining compliance and rising internal resistance) are borne by the whole community.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, fundamentalist_enforcer_communities, payer,
    organized, generational, identity_locked, global).

% Individuals within fundamentalist communities who wish to marry outside the boundary (ethnically, religiously, or both) but are forbidden by enforcement of herem-derived law. They face family expulsion, community ostracism, shame sanctions, or internal identity conflict if they violate the restriction. Their exit is identity-locked because rejecting the restriction typically requires abandoning the community and the entire framework of meaning and belonging that community provides. They bear the suppression costs (restricted choice, constant surveillance and pressure, threatened family relationships) of the constraint's persistence in fundamentalist communities while having no say in whether the constraint should persist.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, individuals_coerced_by_intermarriage_restriction, payer,
    powerless, biographical, identity_locked, local).

% Communities that hold the durable-separation reading (herem as timeless mandate for identity preservation through categorical boundary maintenance). They are excluded from mainline Christian and Jewish institutional discourse about herem; their objections to the supersession framing are not heard in the dominant theological conversation, though they maintain their own interpretive institutions, teaching authority, and enforcement mechanisms. They would dispute the claim that herem has been superseded and would argue that the boundary restriction is a timeless expression of divine will for identity preservation.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, durable_separation_reading_adherents, excluded,
    organized, civilizational, trapped, global).

% Communities and theologians that spiritualize herem (reading conquest as internal moral warfare against sin and temptation, not external ethnic warfare). They occupy an intermediate position: they de-literalize the constraint (removing its direct violent application) but do not assert historical supersession (keeping herem morally operative as allegory). They observe the supersession reading as one possible frame among several legitimate readings, rather than as the sole authoritative interpretation. They neither benefit from nor are victimized by the supersession reading; they simply hold a different frame.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, allegorical_displacement_reading_adherents, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__contextual_supersession_reading, fundamentalist_enforcer_communities).
narrative_ontology:fixing_cost_class(herem_command_dt7__contextual_supersession_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Herem served a consolidation function during ancient Israel's settlement period: rapid territorial occupation, elimination of competing territorial and political claims, prevention of cultural assimilation that could fragment the emerging Israelite identity during existential external threat from dominant regional powers. By this reading, the constraint solved a genuine historical collective-action problem specific to that period and context.
% TRANSFER_FUNCTION: The constraint transferred territory (from indigenous populations to Israelite settlers), consolidated political authority (from many small entities to a unified polity), and enforced identity boundaries (from internal mixing with surrounding populations to categorical separation). In its original context, it extracted compliance from conquered populations and compliance with intermarriage restriction from within Israel. In its contemporary residual form (enforced by fundamentalist communities), it extracts reproductive choice and membership autonomy from individuals who wish to marry outside the boundary, transferring control of reproductive and social decisions to the enforcing institutional structure.
% ABSENT_VOICES: Durable-separation reading adherents are structurally excluded from mainline Christian and Reform Jewish institutional discourse; their objection that herem is timeless and not superseded is not heard in the dominant theological conversation. Allegorical-displacement theologians occupy a middle position but are also marginal to the supersession consensus. Indigenous populations whose ancestors experienced herem violence are absent from contemporary Christian/Jewish theological deliberation about the reading, though they would object to any framing that treats the violence as merely historical rather than as an ongoing ethical and material injury requiring acknowledgment and repair.
% DISAPPEARANCE_RATIONALE: If the contextual-supersession reading disappeared and durable-separation became dominant, fundamentalist boundary enforcement would intensify, intermarriage restrictions would be presented as live moral law, and enforcement machinery would expand. If it disappeared and allegorical-displacement became dominant, the violent reading would evaporate but boundary-restriction might persist as spiritual allegory. The reading's presence shapes whether contemporary herem enforcement is treated as a mistake that has been corrected (supersession frame) or as a live divine imperative (durable-separation frame). The institutional legitimacy, moral weight, and enforcement intensity all depend on which reading prevails.
% FOUNDING_PROBLEM: Herem was established as a directive for ancient Israel's settlement period to solve the problem of rapid territorial consolidation, elimination of competing claims to the land, and prevention of cultural assimilation that could dissolve the emerging Israelite identity during existential threat from the dominant regional powers (Egyptian and other Levantine competitors). The constraint addressed a genuine historical collective-action problem: how to occupy contested territory, consolidate a new political entity with a distinct identity, and maintain cultural cohesion under pressure from the surrounding populations to intermarry and assimilate.
% FOUNDING_PROBLEM_CORROBORATION: Biblical scholarship (Gerhard von Rad on holy war, John Bright on Israelite history, Norman Gottwald on settlement sociology) corroborates that herem served a consolidation function specific to the Iron Age settlement period (circa 1200–1000 BCE). Archaeological evidence from the settlement period and comparative ancient Near Eastern studies support the historical-functionalist reading. Prophetic literature itself attests within the biblical tradition that the founding problem had been resolved by the prophetic period: Jonah's universal divine concern, Isaiah 45–49's servant songs asserting God's saving purpose for all nations, and Isaiah 56's promise to include foreigners in the covenant all indicate that the settlement-consolidation phase had ended and the moral framework had shifted. Christian theology and the New Testament (Acts 10, Galatians 2–3, Matthew 28) explicitly affirm that the old covenantal boundaries have been superseded. No contemporary scholar outside fundamentalist circles argues that territorial consolidation through herem remains a live necessity for Israel or any other polity. The corroboration is strong and comes from sources outside the beneficiary communities: historians working independently of Christian theology, prophetic texts that themselves critique the old boundary system, and secular scholarship establishing that the founding problem is indeed historically resolved.
narrative_ontology:disappearance_verdict(herem_command_dt7__contextual_supersession_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__contextual_supersession_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__contextual_supersession_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(herem_command_dt7__contextual_supersession_reading, 'none', 1).

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
 *   Extractiveness is moderate (0.38, declining trajectory) because the constraint's current persistence depends almost entirely on fundamentalist institutional enforcement, not on genuine coordination benefit. The founding problem (settlement consolidation) is dead by this reading's own account, so any remaining extraction is pure institutional inertia. The suppression metric (0.42, declining) reflects that enforcement is now met with real resistance from younger community members who accept the supersession frame and from broader secular/Christian society that rejects boundary restriction as illegitimate. Theater ratio is high and stable (0.58) because fundamentalist enforcement is increasingly performative—it maintains boundary restriction without the historical justification that once legitimated it. Accessibility collapse (0.72) reflects that the constraint's boundaries are still viscerally real for those trapped within fundamentalist communities (identity-locked exit), but the broader population has clear alternatives (acceptance of intermarriage, secular community membership, non-fundamentalist religion). Resistance is substantial (0.68) because the reading itself is contested by durable-separation adherents, and because individuals coerced by the restriction mount real resistance through exit (some do leave fundamentalist communities) and through delegitimation within those communities. The measurement series shows a declining extractiveness trajectory over 40 years (from ~0.52 projected at t0 to 0.38 observed at t40): as the supersession reading gained institutional authority in mainstream Christianity and Judaism, enforcement by fundamentalist minorities became progressively more isolated and delegitimated. This decline is consistent with a scaffold that has already been sunset—the reading's thesis is that the moral authorization for herem has been revoked, so contemporary enforcement should show declining legitimacy, declining compliance, and increasing theater (which the measurements bear out).
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats (prophetic tradition, Christian theology, historical scholarship), herem is a now-completed phase in a moral narrative—the constraint has been superseded and contemporary enforcement is an aberration. From the fundamentalist enforcer seat, the same texts are timeless divine mandate; suppression and extraction are legitimate because the boundary restriction itself is legitimate. From the coerced-individual seat, the constraint is suppression pure and simple—the debate about moral status is abstract, while the identity-lock is concrete. The engine computes different types from these seats because they have different directionalities: the prophetic/Christian/scholarly seats are beneficiaries (the reading vindicates their authority structure), fundamentalist enforcers are agenda-setters (they maintain the boundary), coerced individuals are targets (they bear the suppression). A scaffold by the supersession reading is a snare by the durable-separation reading (would show high, rising extractiveness and suppression). The claim/metric divergence is intentional: this story claims scaffold (a historically-bounded directive now sunset), and the metrics describe declining extractiveness and rising theater—exactly the decay pattern a sunset constraint should show as its moral authorization erodes.
 *
 * DIRECTIONALITY LOGIC:
 *   The prophetic/Christian/scholarly beneficiaries have low d (near 0.0): they benefit from the supersession reading without bearing extraction costs. The fundamentalist enforcers have moderate d (near 0.5 or slightly higher): they set the agenda for enforcement and bear some costs (maintaining institutional machinery, internal resistance from younger members), but they also collect social authority and institutional coherence from maintaining the boundary. The coerced individuals have high d (near 1.0 or higher): they bear identity-lock suppression and reproductive-choice extraction, and the constraint is actively enforced against their objections. Durable-separation adherents would have lower d than fundamentalists if they were not excluded, because durable-separation would legitimize the boundary restriction without the performance cost. The scholar/prophet seats do not override automatically because the derivation chain correctly identifies them as beneficiaries of a doctrine that gains institutional authority by declaring the constraint superseded.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading exhibits a clear mandatrophy signal: the founding problem (settlement consolidation during existential threat) is dead—the reading itself asserts that prophetic universalism and Christian covenant have resolved the problem by superseding the old mandate. Contemporary enforcement of herem-derived boundaries by fundamentalist communities persists not because the founding problem is live, but because the enforcer communities maintain institutional identity around the boundary. The measurement trajectory (declining extractiveness, rising theater) shows mandatrophy in action: as the moral authorization erodes (because the supersession reading gains acceptance), enforcement becomes increasingly performative and isolated. Resistance and accessibility-collapse values reflect the same mandatrophy: the constraint persists through institutional inertia and identity-lock (in fundamentalist communities), not through genuine coordination benefit. The sunset clause is the schema declaration of this mandatrophy: the reading explicitly asserts that the moral authorization for herem has expired.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supersession_axiom_empirical_contingency,
    'Is Christian covenant supersession grounded in empirically testable claims about the historical completion of Israel''s settlement consolidation, or is it grounded in deontological commitment to Christian universalism that does not depend on the historical facts?',
    'Historical scholarship on the Iron Age settlement period, cross-examined against Christian theological texts to determine whether the supersession doctrine REQUIRES the settlement problem to be solved (empirically contingent) or whether the doctrine ASSERTS universalism as binding regardless of historical fact (deontological).',
    'If empirically contingent, the supersession reading is vulnerable to historical evidence that settlement consolidation remains incomplete or that historical Israel did not achieve the consolidation herem aimed at. If deontological, the reading stands regardless of historical facts about settlement success. This determines whether the axiom can be ''overridden'' (2026-06-07 operator ruling) by empirical counterfactual.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supersession_axiom_empirical_contingency, empirical, 'Whether Christian supersession doctrine is empirically contingent on historical settlement completion or deontologically independent.').

omega_variable(
    fundamentalist_identity_lock_mechanism,
    'Is the suppression experienced by individuals coerced by intermarriage restriction structurally enforced (legal barriers, economic dependency, geographic isolation within the community) or internalized (the individual has fused identity with the community boundary, believes they deserve the restriction, has no external reference frame)?',
    'Post-exit trajectory: if individuals who leave fundamentalist communities report that suppression persists (internalized identity fusion), the suppression is partly internalized; if suppression drops sharply (structurally enforced), the mechanism was structural. Secondarily, ethnographic study of enforcement mechanisms within communities.',
    'If suppression is internalized, the effective suppression of the constraint is higher than the structural measure (0.42) suggests—the target carries the constraint with them after exit. If structural, the suppression is limited to the community context. This affects the classification of the remaining coercion: internalized suppression in the absence of structural enforcement approaches a snare (the target cannot exit even when barriers are removed); structural suppression with identity-lock is closer to tangled-rope (enforcement is necessary to hold the boundary).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fundamentalist_identity_lock_mechanism, empirical, 'Whether identity-lock suppression of intermarriage restriction is structurally or internally mediated.').

omega_variable(
    durable_separation_foreclosure_vs_coexistence,
    'Does the contextual-supersession reading FORECLOSE the durable-separation reading (logically entail that no single Christian framework can hold both), or does it merely DISPLACE it (deny its authority while allowing it to be held as a minority reading by other communities)?',
    'Systematic analysis of Christian theological commitment to universalism: Does universalism REQUIRE the rejection of categorical ethnic boundary restriction as incompatible with Christian identity, or does it ASSERT universalism as normatively superior while allowing boundary restriction as a possible (if erroneous) Christian belief? First test: are there coherent Christian communities that accept the supersession reading AND endorse durable-separation enforcement? If none exist, foreclosure is operative; if some do, only displacement obtains.',
    'Foreclosure would support the reading_relations declaration of ''forecloses'' (stronger, logically entailing position); displacement would support ''coexists_with'' (weaker, merely competing position). This affects the classification of the kernel''s stability: if foreclosure is operative, durable-separation should disappear from Christian communities over time; if only displacement, durable-separation can persist as a fundamentalist minority reading indefinitely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(durable_separation_foreclosure_vs_coexistence, conceptual, 'Whether Christian universalism logically forecloses ethnic-boundary-restriction doctrine or merely displaces it institutionally.').

omega_variable(
    scholarship_corroboration_independence,
    'Does historical scholarship corroborate the contextual-supersession reading''s historical claims (Iron Age settlement consolidation, completion of the founding problem) independently of theological interest in defending the supersession doctrine, or is scholarship itself influenced by the same theological framing?',
    'Historiographical analysis: when biblical scholars date the herem command to the settlement period and assess whether the consolidation goal was achieved, are they making independent historical judgments, or are they accepting theological premises that require the founding problem to be solved? Cross-check against scholars outside the Christian/Jewish theological tradition (secular historians, scholars from other faith traditions) to establish whether the historical consensus is independent or theologically motivated.',
    'If scholarship is independent, the contextual-supersession reading has strong external corroboration (declared in six_questions.founding_problem_corroboration). If scholarship is theologically motivated, the corroboration is circular (the theology shapes the historical interpretation, then the historical interpretation validates the theology). This affects the credibility of the reading as a whole and the likelihood of its persistence if theological commitments change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scholarship_corroboration_independence, empirical, 'Whether historical scholarship corroborates contextual supersession reading independently of theological interest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__contextual_supersession_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__contextual_supersession_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(here_tr_t0, projected).
narrative_ontology:measurement(here_tr_t5, herem_command_dt7__contextual_supersession_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement_basis(here_tr_t5, projected).
narrative_ontology:measurement(here_tr_t10, herem_command_dt7__contextual_supersession_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement_basis(here_tr_t10, observed).
narrative_ontology:measurement(here_tr_t15, herem_command_dt7__contextual_supersession_reading, theater_ratio, 15, 0.53).
narrative_ontology:measurement_basis(here_tr_t15, observed).
narrative_ontology:measurement(here_tr_t20, herem_command_dt7__contextual_supersession_reading, theater_ratio, 20, 0.56).
narrative_ontology:measurement_basis(here_tr_t20, observed).
narrative_ontology:measurement(here_tr_t25, herem_command_dt7__contextual_supersession_reading, theater_ratio, 25, 0.58).
narrative_ontology:measurement_basis(here_tr_t25, observed).
narrative_ontology:measurement(here_tr_t30, herem_command_dt7__contextual_supersession_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement_basis(here_tr_t30, observed).
narrative_ontology:measurement(here_tr_t40, herem_command_dt7__contextual_supersession_reading, theater_ratio, 40, 0.58).
narrative_ontology:measurement_basis(here_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(here_be_t0, projected).
narrative_ontology:measurement(here_be_t5, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(here_be_t5, projected).
narrative_ontology:measurement(here_be_t10, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement_basis(here_be_t10, observed).
narrative_ontology:measurement(here_be_t15, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 15, 0.41).
narrative_ontology:measurement_basis(here_be_t15, observed).
narrative_ontology:measurement(here_be_t20, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 20, 0.39).
narrative_ontology:measurement_basis(here_be_t20, observed).
narrative_ontology:measurement(here_be_t25, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(here_be_t25, observed).
narrative_ontology:measurement(here_be_t30, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(here_be_t30, observed).
narrative_ontology:measurement(here_be_t40, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(here_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t0, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(here_su_t0, projected).
narrative_ontology:measurement(here_su_t5, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement_basis(here_su_t5, projected).
narrative_ontology:measurement(here_su_t10, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(here_su_t10, observed).
narrative_ontology:measurement(here_su_t15, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 15, 0.46).
narrative_ontology:measurement_basis(here_su_t15, observed).
narrative_ontology:measurement(here_su_t20, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement_basis(here_su_t20, observed).
narrative_ontology:measurement(here_su_t25, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement_basis(here_su_t25, observed).
narrative_ontology:measurement(here_su_t30, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement_basis(here_su_t30, observed).
narrative_ontology:measurement(here_su_t40, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(here_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__contextual_supersession_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(herem_command_dt7__contextual_supersession_reading, 0.12).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, herem_command_dt7__durable_separation_reading).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, herem_command_dt7__allegorical_displacement_reading).

% DUAL FORMULATION NOTE:
% The herem_command_dt7 kernel admits three structurally distinct readings: contextual_supersession (this story), durable_separation, and allegorical_displacement. These are not perspectives on the same constraint—they are three distinct constraints with different ε values, different victim sets, and different classifications. The contextual-supersession reading produces a scaffold with declining extractiveness; durable-separation produces a snare with rising extractiveness; allegorical-displacement produces a rope with low extractiveness (the violent application is spiritualized away). Each reading instantiates a different constraint because the coordinate facts (who benefits, who bears costs, what the constraint enforces) differ across readings. The network edges link the three stories as members of the herem family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
