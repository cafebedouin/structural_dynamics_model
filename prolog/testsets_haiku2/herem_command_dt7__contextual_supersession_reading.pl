% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__contextual_supersession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_contextual_supersession, []).

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
 *   constraint_id: herem_command_dt7__contextual_supersession_reading
 *   human_readable: Herem Command (Contextual Supersession Reading): Historically-Bounded Settlement Directive
 *   domain: religious_ethics/biblical_hermeneutics/commitment_systems
 *
 * SUMMARY:
 *   The contextual-supersession reading holds that herem (the ancient
 *   Israelite command to eliminate or severely restrict outsider populations
 *   in specific territorial contexts) was a historically-bounded directive
 *   tied to Iron Age settlement pressures and has been morally superseded by
 *   two developments: the prophetic universalization of moral standing
 *   (witnessed in Amos, Isaiah, Jeremiah) and, for Christian readings, the
 *   new covenant's dissolution of ethnic boundaries. Under this reading,
 *   herem has no contemporary binding force; modern fundamentalist attempts
 *   to apply it to intermarriage or territorial claims are misreadings that
 *   misplace a settlement-period expedient into a context where prophetic and
 *   covenantal authority have relocated the constraint's referent from ethnic
 *   boundary to internal spiritual struggle (in allegorical readings) or
 *   removed it entirely (in supersession readings). The measurement series
 *   tracks extractiveness decline as the contextual interpretation has gained
 *   institutional authority in academic theology, rabbinic Judaism, and
 *   mainline Christianity; the coercion grid shows asymmetric
 *   decay—structural and organizational suppression fell sharply as scholarly
 *   consensus shifted, but individual-level enforcement persists where
 *   fundamentalist communities remain identity-locked to the literal reading.
 *
 * KEY AGENTS:
 *   - ancient_israel_settlement_community: original beneficiaries of herem's coordination function (territorial consolidation, existential survival) — analytical position
 *   - prophetic_universalist_interpreters: institutional beneficiaries of the supersession narrative (authority over interpretation, moral legitimacy) — powerful, generational horizon
 *   - christian_covenant_interpreters: institutional beneficiaries through Christological reframing (authority to declare covenants abrogated) — institutional power, civilizational horizon
 *   - fundamentalist_literalist_interpreters: payers (bear burden of defending an interpretation increasingly indefensible in scholarly and prophetic context) and agenda-setters (attempt to enforce literal herem on contemporary adherents) — powerful at local scale, constrained by textual/prophetic challenge
 *   - targeted_outsider_groups_under_literalist_enforcement: victims (subject to exclusion, identity-locked to the constraint through relational bonds) — powerless, identity-locked exit
 *   - contextual_historians: external corroborators (attest the Iron Age founding problem and herem's settlement-period function)
 *   - intra_jewish_halakhic_interpreters: external corroborators and institutional rivals to fundamentalist reading (rabbinic precedent for contextual bounding)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__contextual_supersession_reading, 0.28).
domain_priors:suppression_score(herem_command_dt7__contextual_supersession_reading, 0.22).
domain_priors:theater_ratio(herem_command_dt7__contextual_supersession_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__contextual_supersession_reading, scaffold).
narrative_ontology:human_readable(herem_command_dt7__contextual_supersession_reading, "Herem Command (Contextual Supersession Reading): Historically-Bounded Settlement Directive").
narrative_ontology:topic_domain(herem_command_dt7__contextual_supersession_reading, "religious_ethics/biblical_hermeneutics/commitment_systems").

domain_priors:requires_active_enforcement(herem_command_dt7__contextual_supersession_reading).
narrative_ontology:has_sunset_clause(herem_command_dt7__contextual_supersession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__contextual_supersession_reading, '34bc16da-4d82-41dc-b81a-a2739c73da4d').
narrative_ontology:cs_kernel_codification('34bc16da-4d82-41dc-b81a-a2739c73da4d', fixed_text).
narrative_ontology:cs_authority_grounding('34bc16da-4d82-41dc-b81a-a2739c73da4d', lineage).
narrative_ontology:cs_interpretation_layer_present('34bc16da-4d82-41dc-b81a-a2739c73da4d').
narrative_ontology:cs_reading_relation('34bc16da-4d82-41dc-b81a-a2739c73da4d', herem_command_dt7__allegorical_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('34bc16da-4d82-41dc-b81a-a2739c73da4d', herem_command_dt7__durable_separation_reading, coexists_with).
narrative_ontology:cs_axiom('34bc16da-4d82-41dc-b81a-a2739c73da4d', foundational, prophetic_moral_universalism_supersedes_ethnic_boundary).
narrative_ontology:cs_axiom_status(prophetic_moral_universalism_supersedes_ethnic_boundary, holdable).
narrative_ontology:cs_axiom_grounding('34bc16da-4d82-41dc-b81a-a2739c73da4d', prophetic_moral_universalism_supersedes_ethnic_boundary, deontological).
narrative_ontology:cs_axiom('34bc16da-4d82-41dc-b81a-a2739c73da4d', foundational, covenant_succession_abrogates_settlement_period_directive).
narrative_ontology:cs_axiom_status(covenant_succession_abrogates_settlement_period_directive, holdable).
narrative_ontology:cs_axiom_grounding('34bc16da-4d82-41dc-b81a-a2739c73da4d', covenant_succession_abrogates_settlement_period_directive, deontological).
narrative_ontology:cs_reference_frame('34bc16da-4d82-41dc-b81a-a2739c73da4d', iron_age_tribal_consolidation_necessity).
narrative_ontology:cs_drift_state('34bc16da-4d82-41dc-b81a-a2739c73da4d', contemporary_institutional_consensus, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('34bc16da-4d82-41dc-b81a-a2739c73da4d', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__contextual_supersession_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, ancient_israel_settlement_community).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, fundamentalist_interpreters_coercing_modern_adherence).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, targeted_outsider_groups_under_literalist_enforcement).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, prophetic_universalist_interpreters).
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, christian_covenant_interpreters).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, fundamentalist_literalist_interpreters).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, prophetic_universal_moral_expansion).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, christian_covenant_supersession).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The original recipient community for whom herem served as a boundary-maintenance mechanism during territorial consolidation in Iron Age Levant. The directive coordinated identity preservation and territorial security in a specific historical moment of existential vulnerability. Beneficiaries of the military-religious coordination function herem provided.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, ancient_israel_settlement_community, beneficiary,
    organized, generational, analytical, regional).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__contextual_supersession_reading, ancient_israel_settlement_community, agenda_setter).

% Medieval and modern religious scholars and clergy who read the constraint as historically-bounded and morally superseded by the prophetic corpus (Amos, Isaiah, Jeremiah) which expanded moral standing to all humanity. They have reframed herem's referent from a timeless mandate to a settlement-period necessity, relocating authority to later, universalist revelation.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, prophetic_universalist_interpreters, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__contextual_supersession_reading, prophetic_universalist_interpreters, agenda_setter).

% Christian reading communities (both academic and ecclesiastical) who treat herem as part of the 'old covenant' and the old covenant as abrogated or fulfilled by the new covenant's inclusive logic. Their interpretive authority rests on the Christological premise that ethnic boundaries are now morally irrelevant.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, christian_covenant_interpreters, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__contextual_supersession_reading, christian_covenant_interpreters, agenda_setter).

% Modern interpreters who claim herem's directive is historically-independent and divinely timeless, who apply or attempt to apply its logic to contemporary intermarriage prohibitions, territorial claims, or separation doctrines. They bear the cost of defending an interpretation that faces sustained scholarly and prophetic-textual challenge, and they impose coercion on adherents seeking to marry outside designated boundaries or on outsiders targeted by the doctrine.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, fundamentalist_literalist_interpreters, payer,
    powerful, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__contextual_supersession_reading, fundamentalist_literalist_interpreters, agenda_setter).

% Modern individuals or communities (intermarriage partners, religious minorities, territorial neighbors) subject to contemporary fundamentalist herem enforcement—exclusion, shunning, denial of community participation, or territorial claims justified by literal herem application. Their exit from the constraint is blocked by relational identity fusion or institutional power asymmetries.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, targeted_outsider_groups_under_literalist_enforcement, payer,
    powerless, biographical, identity_locked, local).

% Academic historians and philologists who assess herem's function in ancient military-religious practice, its textual development across biblical strata, and its transformation in post-biblical Judaism and Christianity. They provide external corroboration of the founding problem (Iron Age territorial vulnerability) and the historical-context reading.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, contextual_historians_of_ancient_near_east, observer,
    institutional, civilizational, analytical, global).

% Talmudic and post-Talmudic Jewish interpreters who have historically bounded herem's contemporary application (restricting it to war/self-defense, not to ethnic intermarriage or territorial claims), establishing rabbinic precedent for contextual reading that this constraint's reading instantiates.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, intra_jewish_halakhic_interpreters, observer,
    institutional, civilizational, analytical, global).

% Non-member individuals in mixed-faith relationships whose voices would challenge the herem boundary as ethically unjustifiable, but who are structurally excluded from the interpretive conversation that determines whether herem applies to them. Their objection is foreclosed by the role boundary itself.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, excluded_intermarriage_partners, excluded,
    moderate, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__contextual_supersession_reading, diffuse).
narrative_ontology:fixing_cost_class(herem_command_dt7__contextual_supersession_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Herem, in its original Iron Age context, coordinated group survival and territorial consolidation by establishing clear in-group/out-group boundaries and authorizing unified military-religious response to existential territorial threats. The contextual-supersession reading relocates this coordination function to the ancient settlement period; it is no longer operative in the reading's present because prophetic and covenantal reinterpretation have dissolved the boundaries herem encoded.
% TRANSFER_FUNCTION: In the ancient context: redistributed social membership and ritual standing—those outside the boundary bore exclusion and potential displacement; those inside received corporate identity, security coordination, and collective action capacity. In the contemporary literalist enforcement context: redistributes social standing and relational access—those subject to herem enforcement bear exclusion, shunning, and denial of community participation; fundamentalist enforcement communities collect authority to police boundaries.
% ABSENT_VOICES: Intermarriage partners who would object to the boundary; religious minorities targeted by contemporary herem enforcement; outsider communities subjected to literalist territorial or separation claims. These voices are structurally excluded from the authority structure that interprets whether herem applies to them. The constraint's entire victim set is excluded from the interpretive conversation about its bindingness.
% DISAPPEARANCE_RATIONALE: If the supersession reading were universally adopted and enforced, the constraint would functionally disappear: literalist herem enforcement would cease, intermarriage prohibition based on herem would be morally delegitimated, territorial claims justified by herem would lose their scriptural warrant. However, the durable reading holds that herem cannot disappear—it is eternally binding—and some fundamentalist communities actively maintain it through enforcement. The contest over disappearance is the contest between readings.
% FOUNDING_PROBLEM: Iron Age Israel faced existential territorial vulnerability from surrounding militaries and cultural assimilation pressure from neighboring populations. Herem served as a boundary-maintenance and military-mobilization mechanism for tribal consolidation and survival during the settlement and early monarchic periods.
% FOUNDING_PROBLEM_CORROBORATION: Ancient Near Eastern historians confirm the Iron Age context and herem's military-religious coordination function in that setting. Rabbinic interpreters from the Second Temple period onward document historical contextual readings and their application restrictions, providing intra-tradition corroboration. The prophetic texts themselves (Amos, Isaiah, Jeremiah) document the expansion of moral standing universally, establishing precedent for supersession from within the scriptural canon itself. Secular academic biblical studies and histories of ancient Israel attest the founding problem and its settlement-period context from outside any benefiting community. The corroboration comes from historians, rabbinical authorities, and the prophetic canon—sources not positioned as beneficiaries of the contextual-supersession reading.
narrative_ontology:disappearance_verdict(herem_command_dt7__contextual_supersession_reading, contested).
narrative_ontology:founding_problem_status(herem_command_dt7__contextual_supersession_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__contextual_supersession_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(herem_command_dt7__contextual_supersession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__contextual_supersession_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is low (0.28 at interval end) under this reading because the constraint's primary function (territorial military coordination) is historically exhausted, and the only remaining extraction is fundamentalist enforcement of an obsolete boundary on modern adherents. Suppression is also low (0.22) because the reading has institutional backing in mainline theology and rabbinic interpretation, so enforcement of the supersession view requires minimal coercive overhead—it is the literal reading that now requires suppression maintenance. Theater is minimal (0.15) because the prophetic and covenantal reframings do substantive moral work (they are not theatrical cover stories; they genuinely relocate authority). The measurement trajectory shows extractiveness and suppression_requirement both declining from t=0 (when the literal reading had greater cultural authority) to t=50 (when the contextual reading has become institutional consensus in academic and liberal-religious contexts). This decline is NOT uniform across levels: organizational and structural suppression fell sharply (the interpretive authorities flipped), while individual-level suppression persisted longer (fundamentalist sub-communities maintain identity-locked resistance to the new reading). The coercion grid captures this asymmetry: at t0, structural/organizational/class stakeholders all experienced high stakes and suppression; at t50, individuals identity-locked to fundamentalism still face suppression and stakes, but the structural and organizational landscape has shifted away. This is the temporal signature of a reading that has won institutional authority but retains pockets of literalist enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The prophetic_universalist_interpreters and christian_covenant_interpreters should compute as low-extraction beneficiaries (they secured interpretive authority and moral legitimacy with minimal coercive cost; the shift to their reading was institutional/textual, not enforced). Fundamentalist_literalist_interpreters should compute as high-extraction payers (they must continually defend an interpretation against prophetic texts, rabbinic precedent, and scholarly consensus; the cost of maintaining herem's literal applicability has risen as alternative readings gained authority). Targeted_outsider_groups compute as victims with identity-locked exit (they are excluded by relational fusion and institutional power asymmetry; their exit costs are prohibitive). The engine's per-seat computation should reveal this asymmetry: from the institutional-authority seats, the supersession reading is low-friction Rope (genuine coordination of moral universalism, minimal enforcement cost). From the fundamentalist seat, it is high-extraction Snare (the burden of defense has grown; the reading persists by suppression of alternatives within the community). From the outsider seat, it is extractive victimhood—pure targets of a boundary that has been relocated from territorial necessity to identity policing. The structural data (roles, power atoms, exit options) encode this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Ancient_israel_settlement_community: analytical position (bounded in time, no contemporary directionality). Prophetic_universalist_interpreters: d ≈ 0.1 (full beneficiaries—they control interpretation, face no suppression, exit is impossible because they have already secured the interpretive authority they sought). Christian_covenant_interpreters: d ≈ 0.05 (likewise, beneficiaries of the reframing). Fundamentalist_literalist_interpreters: d ≈ 0.75 (near-targets—they must defend against prophetic texts and institutional pressure, their exit would mean abandoning their reading and accepting the prophetic/covenantal supersession, but they remain powerful at local scale through identity-fusion enforcement). Targeted_outsider_groups: d ≈ 0.92 (near-full targets—excluded, identity-locked exit through relational fusion, subject to enforcement, no structural pathway to alter the constraint without first exiting the relationship). Contextual_historians and intra_jewish_halakhic_interpreters: d ≈ 0.5 (symmetric analytical seats—they interpret, they do not collect or pay). These directionality values reflect the structural inequalities: the reading has succeeded in relocating authority, but the cost of that success is paid by fundamentalist communities and the individuals trapped within their enforcement boundaries.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is NOT present in the proper sense, but a related dynamic (founding problem death) is the reading's central claim. The founding problem (Iron Age territorial vulnerability) is genuinely dead—it solved a real collective-action problem in its historical moment. The constraint persists now only through two mechanisms: (1) institutional inertia (canonical texts transmit herem whether or not it applies), and (2) fundamentalist identity enforcement (a sub-community has fused herem-literalism with group identity and polices the boundary through exclusion/shunning). Under the contextual-supersession reading, these persistence mechanisms are explicitly NOT coordination—they are the residual extraction that happens when a settlement-period Rope has outlived its coordination function. The prophetic and covenantal frameworks explicitly dissolve herem's binding force, so herem persists as extracted constraint, not as coordination. This is structurally different from a true Piton: a Piton has no recognized victims (extraction is diffuse and no party profits enough to maintain it). Herem under literalist enforcement HAS identified victims (intermarriage partners, religious minorities) and has identified payers (the fundamentalist community itself, which bears the cost of defending an indefensible reading). Therefore, mandatrophy is not the right classification—Snare (at the fundamentalist seat) is more accurate. The reading's classification system is: scaffolding at the prophetic/covenantal level (temporary, sunset by reinterpretation), Snare at the literalist-enforcement level (extracted, identity-locked targets). The claim/metric independence is preserved: the reading CLAIMS the constraint is Scaffold (historically bounded, superseded), but the authored metrics on literalist enforcement show Snare-like extractiveness and suppression. The engine computes both per-seat; the divergence is the measurement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prophetic_canonical_status,
    'Do the prophetic texts (Amos, Isaiah, Jeremiah) represent a later, superseding canonical stratum with genuine hermeneutical authority over settlement-period directives, or are they one reading layer among many with no inherent priority?',
    'Textual criticism and redaction history: if the prophetic universalization is documentably later and explicit about relocating moral standing, the supersession reading gains canonical warrant. If the prophetic texts are contemporaneous or if herem texts claim independent divine authority unmediated by prophetic reinterpretation, the durable reading''s claim of non-subordination is strengthened.',
    'If prophetic texts are canonically authoritative supersessors, the contextual-supersession reading is structurally sound (extractiveness remains low because the reading has textual backing). If they are coordinate layers or if herem claims independent eternal authority, the reading collapses and extractiveness on literalist enforcement rises sharply—the constraint reverts to Snare without the institutional authority to maintain the supersession claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prophetic_canonical_status, empirical, 'Whether prophetic universalism represents canonical supersession or coordinate interpretation').

omega_variable(
    covenant_replacement_vs_expansion,
    'Does the Christian ''new covenant'' framework represent a replacement that abrogates the old covenant''s ethnic boundaries (as supersession reading holds), or an expansion that preserves the old covenant within a larger structure (as the durable reading could claim)?',
    'Theological tradition and textual exegesis: Paul''s treatment of Gentile inclusion (Romans 9-11, Galatians), early Christian identity claims, and subsequent theological consensus on the relationship between covenants. If the tradition consistently treats the new covenant as superseding ethnic requirements, the reading stands; if the tradition sees it as expanding without abrogating, the boundary remains live.',
    'Supersession through covenantal replacement strengthens the contextual reading''s low-extraction claim (Christian authority has relocated the boundary). Covenantal expansion without abrogation weakens it—the old covenant''s ethnic boundary would persist as binding for Jewish readers even if not for Gentile Christians, narrowing the reading''s scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenant_replacement_vs_expansion, conceptual, 'Whether Christian covenant theology abrogates or expands the ethnic boundary').

omega_variable(
    fundamentalist_identity_lock_mechanism,
    'Is the fundamentalist literalist reading persisting primarily through institutional coercion and enforcement (structural suppression), or primarily through identity fusion where adherents have fused their self-concept with the literal reading such that exiting the reading feels like exiting the group (internalized suppression)?',
    'Ethnographic study of fundamentalist communities: if enforcement is maintained through institutional sanctions (exclusion, loss of status, loss of relational access), the suppression is structural. If adherents report that exit feels impossible because the reading is inseparable from their identity, the suppression is internalized. Post-exit trajectories provide evidence: if individuals who leave the literalist reading report that suppression persists (internalized shame, identity confusion), the suppression was partly internalized.',
    'If suppression is structural, institutional counter-interpretation (the kind this reading represents) can erode it over time—institutional backing for the supersession reading reduces suppression costs. If suppression is internalized, the reading''s institutional success may not reduce enforcement burden—individuals remain self-policing even after institutional authority has shifted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fundamentalist_identity_lock_mechanism, empirical, 'Whether fundamentalist literalism persists through structural enforcement or identity-fusion internalization').

omega_variable(
    reading_containment_across_traditions,
    'Is the contextual-supersession reading genuinely held across Jewish, Christian, and secular-academic communities, or is it primarily a Christian/liberal-Protestant reading that Jewish literalism and Eastern Orthodox conservatism reject?',
    'Survey institutional positions: rabbinic academic consensus, halakhic case law, Christian systematic theology, academic biblical studies. If the reading is trans-traditional, extractiveness on literalist resistance is lower (isolated). If it is primarily Christian/liberal-academic with Orthodox Jewish and fundamentalist dissent, extractiveness on the constraint''s persistence is higher—the reading has not achieved consensus among all tradition-bearers.',
    'Trans-traditional acceptance strengthens the reading''s institutional authority and lowers extractiveness on enforcement (the literal reading becomes a minority position everywhere). Tradition-specific acceptance weakens it—fundamentalist Judaism and Orthodox Christianity would maintain the literal reading, meaning the constraint persists in those communities with high extractiveness and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_containment_across_traditions, empirical, 'Whether contextual-supersession reading achieves consensus across all tradition-bearers or remains tradition-specific').

omega_variable(
    allegorical_vs_supersession_boundary,
    'Is the allegorical-displacement reading a variant of this supersession reading (both remove literal application, just via different hermeneutical moves), or a genuinely distinct reading that preserves herem''s binding force in spiritualized form?',
    'Textual and theological analysis: if allegorical readings claim to preserve the spiritual force of herem (sin as the true enemy, temptation as the true outsider), they maintain extraction at the level of moral obligation—the constraint remains binding, just internally. If they treat allegory as dissolving the constraint''s binding force entirely (the literal text''s force was never intended for ethics, only metaphor), they align with supersession.',
    'If allegorical readings preserve obligation, they are a distinct constraint (herem_allegorical) with different ε and different victim set (adherents obligated to wage internal spiritual warfare). If they dissolve obligation, they are a variant of this reading. The network diagram would differ accordingly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(allegorical_vs_supersession_boundary, conceptual, 'Whether allegorical displacement preserves binding force or dissolves it').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__contextual_supersession_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__contextual_supersession_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(here_tr_t0, observed).
narrative_ontology:measurement(here_tr_t8, herem_command_dt7__contextual_supersession_reading, theater_ratio, 8, 0.1).
narrative_ontology:measurement_basis(here_tr_t8, observed).
narrative_ontology:measurement(here_tr_t16, herem_command_dt7__contextual_supersession_reading, theater_ratio, 16, 0.12).
narrative_ontology:measurement_basis(here_tr_t16, observed).
narrative_ontology:measurement(here_tr_t24, herem_command_dt7__contextual_supersession_reading, theater_ratio, 24, 0.14).
narrative_ontology:measurement_basis(here_tr_t24, observed).
narrative_ontology:measurement(here_tr_t32, herem_command_dt7__contextual_supersession_reading, theater_ratio, 32, 0.15).
narrative_ontology:measurement_basis(here_tr_t32, observed).
narrative_ontology:measurement(here_tr_t40, herem_command_dt7__contextual_supersession_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement_basis(here_tr_t40, observed).
narrative_ontology:measurement(here_tr_t50, herem_command_dt7__contextual_supersession_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement_basis(here_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement_basis(here_be_t0, observed).
narrative_ontology:measurement(here_be_t8, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(here_be_t8, observed).
narrative_ontology:measurement(here_be_t16, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement_basis(here_be_t16, observed).
narrative_ontology:measurement(here_be_t24, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 24, 0.35).
narrative_ontology:measurement_basis(here_be_t24, observed).
narrative_ontology:measurement(here_be_t32, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 32, 0.3).
narrative_ontology:measurement_basis(here_be_t32, observed).
narrative_ontology:measurement(here_be_t40, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement_basis(here_be_t40, observed).
narrative_ontology:measurement(here_be_t50, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement_basis(here_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t0, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(here_su_t0, observed).
narrative_ontology:measurement(here_su_t8, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement_basis(here_su_t8, observed).
narrative_ontology:measurement(here_su_t16, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 16, 0.42).
narrative_ontology:measurement_basis(here_su_t16, observed).
narrative_ontology:measurement(here_su_t24, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 24, 0.32).
narrative_ontology:measurement_basis(here_su_t24, observed).
narrative_ontology:measurement(here_su_t32, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 32, 0.26).
narrative_ontology:measurement_basis(here_su_t32, observed).
narrative_ontology:measurement(here_su_t40, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 40, 0.22).
narrative_ontology:measurement_basis(here_su_t40, observed).
narrative_ontology:measurement(here_su_t50, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 50, 0.22).
narrative_ontology:measurement_basis(here_su_t50, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=50
narrative_ontology:measurement(here_grid_01, herem_command_dt7__contextual_supersession_reading, accessibility_collapse(class), 0, 0.68).
narrative_ontology:measurement(here_grid_02, herem_command_dt7__contextual_supersession_reading, accessibility_collapse(class), 50, 0.35).
narrative_ontology:measurement(here_grid_03, herem_command_dt7__contextual_supersession_reading, accessibility_collapse(individual), 0, 0.55).
narrative_ontology:measurement(here_grid_04, herem_command_dt7__contextual_supersession_reading, accessibility_collapse(individual), 50, 0.38).
narrative_ontology:measurement(here_grid_05, herem_command_dt7__contextual_supersession_reading, accessibility_collapse(organizational), 0, 0.75).
narrative_ontology:measurement(here_grid_06, herem_command_dt7__contextual_supersession_reading, accessibility_collapse(organizational), 50, 0.32).
narrative_ontology:measurement(here_grid_07, herem_command_dt7__contextual_supersession_reading, accessibility_collapse(structural), 0, 0.82).
narrative_ontology:measurement(here_grid_08, herem_command_dt7__contextual_supersession_reading, accessibility_collapse(structural), 50, 0.28).
narrative_ontology:measurement(here_grid_09, herem_command_dt7__contextual_supersession_reading, resistance(class), 0, 0.55).
narrative_ontology:measurement(here_grid_10, herem_command_dt7__contextual_supersession_reading, resistance(class), 50, 0.72).
narrative_ontology:measurement(here_grid_11, herem_command_dt7__contextual_supersession_reading, resistance(individual), 0, 0.62).
narrative_ontology:measurement(here_grid_12, herem_command_dt7__contextual_supersession_reading, resistance(individual), 50, 0.68).
narrative_ontology:measurement(here_grid_13, herem_command_dt7__contextual_supersession_reading, resistance(organizational), 0, 0.42).
narrative_ontology:measurement(here_grid_14, herem_command_dt7__contextual_supersession_reading, resistance(organizational), 50, 0.75).
narrative_ontology:measurement(here_grid_15, herem_command_dt7__contextual_supersession_reading, resistance(structural), 0, 0.35).
narrative_ontology:measurement(here_grid_16, herem_command_dt7__contextual_supersession_reading, resistance(structural), 50, 0.78).
narrative_ontology:measurement(here_grid_17, herem_command_dt7__contextual_supersession_reading, stakes_inflation(class), 0, 0.72).
narrative_ontology:measurement(here_grid_18, herem_command_dt7__contextual_supersession_reading, stakes_inflation(class), 50, 0.2).
narrative_ontology:measurement(here_grid_19, herem_command_dt7__contextual_supersession_reading, stakes_inflation(individual), 0, 0.65).
narrative_ontology:measurement(here_grid_20, herem_command_dt7__contextual_supersession_reading, stakes_inflation(individual), 50, 0.25).
narrative_ontology:measurement(here_grid_21, herem_command_dt7__contextual_supersession_reading, stakes_inflation(organizational), 0, 0.8).
narrative_ontology:measurement(here_grid_22, herem_command_dt7__contextual_supersession_reading, stakes_inflation(organizational), 50, 0.18).
narrative_ontology:measurement(here_grid_23, herem_command_dt7__contextual_supersession_reading, stakes_inflation(structural), 0, 0.88).
narrative_ontology:measurement(here_grid_24, herem_command_dt7__contextual_supersession_reading, stakes_inflation(structural), 50, 0.15).
narrative_ontology:measurement(here_grid_25, herem_command_dt7__contextual_supersession_reading, suppression(class), 0, 0.58).
narrative_ontology:measurement(here_grid_26, herem_command_dt7__contextual_supersession_reading, suppression(class), 50, 0.2).
narrative_ontology:measurement(here_grid_27, herem_command_dt7__contextual_supersession_reading, suppression(individual), 0, 0.48).
narrative_ontology:measurement(here_grid_28, herem_command_dt7__contextual_supersession_reading, suppression(individual), 50, 0.25).
narrative_ontology:measurement(here_grid_29, herem_command_dt7__contextual_supersession_reading, suppression(organizational), 0, 0.68).
narrative_ontology:measurement(here_grid_30, herem_command_dt7__contextual_supersession_reading, suppression(organizational), 50, 0.22).
narrative_ontology:measurement(here_grid_31, herem_command_dt7__contextual_supersession_reading, suppression(structural), 0, 0.7).
narrative_ontology:measurement(here_grid_32, herem_command_dt7__contextual_supersession_reading, suppression(structural), 50, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__contextual_supersession_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(herem_command_dt7__contextual_supersession_reading, 0.12).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, herem_command_dt7__allegorical_displacement_reading).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, herem_command_dt7__durable_separation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the herem_command_dt7 kernel. The kernel is the stabilized biblical text (Deuteronomy 7 and related directives). Three structurally distinct readings contest its contemporary applicability: contextual_supersession_reading (this one), allegorical_displacement_reading, and durable_separation_reading. Each reading has different ε, different victim sets, and different beneficiary structures. The three are linked as siblings in the constraint family; they share the same referent (the biblical text) but instantiate different interpretive authorities (prophetic texts for supersession, internal spiritual struggle for allegory, eternal divine mandate for durable). See each story's cs_structure.reading_relations for the structural relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(herem_command_dt7__contextual_supersession_reading, organized, 0.08).
constraint_indexing:directionality_override(herem_command_dt7__contextual_supersession_reading, powerful, 0.72).
constraint_indexing:directionality_override(herem_command_dt7__contextual_supersession_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
