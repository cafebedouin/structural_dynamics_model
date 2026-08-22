% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__religious_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Zionist Legitimacy as Divine Covenant Fulfillment (Religious Restoration Reading, Post-1967)
 *   domain: political_history/nationalism/religion
 *
 * SUMMARY:
 *   This story instantiates the religious_restoration_reading: the framework,
 *   consolidated after the 1967 territorial outcome, under which Jewish
 *   sovereignty over the Land of Israel is held to fulfill a divine promise
 *   and the events of that year to mark a stage in a messianic process — with
 *   the structural consequences that religious obligation overrides secular
 *   political considerations and that territorial withdrawal is prohibited in
 *   advance rather than argued against. The framework operates through an
 *   interpretive apparatus (yeshivot, rabbinic rulings, religious parties,
 *   the settlement enterprise) and through state enforcement (settlement
 *   protection, military administration of the territories, coalition
 *   arithmetic). The epsilon referent is fixed by the kernel-reading rule:
 *   the standing arrangement under contest — the post-1967 settlement and
 *   sovereignty regime as actually practiced — never the reading's endorsed
 *   alternative. Metric values are reading-indexed: they record what this
 *   framework, by its own lights, acknowledges about the arrangement it
 *   legitimates, not an external audit. KEY AGENTS (by structural
 *   relationship): - religious_settler_movement: primary beneficiary
 *   (organized/identity_locked) — collects land, funding, and veto power;
 *   exit would require theological collapse - religious_zionist_institutions:
 *   interpretive agenda-setter and secondary beneficiary
 *   (organized/identity_locked) — issues the rulings that mandate settlement,
 *   collects state resources - israeli_state_apparatus: enforcing
 *   agenda-setter (institutional/constrained) — administers the regime and
 *   absorbs its strategic and diplomatic costs -
 *   palestinians_under_occupation: primary payer (powerless/trapped) — bear
 *   displacement, statelessness, and military administration -
 *   secular_israeli_citizens: payer (moderate/constrained) — bear foreclosed
 *   policy options, military burden, diplomatic isolation -
 *   palestinian_citizens_of_israel: payer (moderate/constrained) —
 *   equal-citizenship claims subordinated by the framework's identity
 *   structure - territorial_compromise_advocates: excluded
 *   (organized/constrained) — ruled out theologically rather than refuted -
 *   international_legal_institutions: analytical observer
 *   (institutional/analytical) — assesses the arrangement from outside its
 *   legitimacy structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__religious_restoration_reading, 0.22).
domain_priors:suppression_score(zionist_legitimacy_basis__religious_restoration_reading, 0.6).
domain_priors:theater_ratio(zionist_legitimacy_basis__religious_restoration_reading, 0.16).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 0.16).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse, 0.87).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__religious_restoration_reading, mountain).
narrative_ontology:human_readable(zionist_legitimacy_basis__religious_restoration_reading, "Zionist Legitimacy as Divine Covenant Fulfillment (Religious Restoration Reading, Post-1967)").
narrative_ontology:topic_domain(zionist_legitimacy_basis__religious_restoration_reading, "political_history/nationalism/religion").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__religious_restoration_reading).
domain_priors:emerges_naturally(zionist_legitimacy_basis__religious_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__religious_restoration_reading, 'e77f54d7-fdfd-429c-af06-c42d353b826a').
narrative_ontology:cs_kernel_codification('e77f54d7-fdfd-429c-af06-c42d353b826a', distributed).
narrative_ontology:cs_authority_grounding('e77f54d7-fdfd-429c-af06-c42d353b826a', lineage).
narrative_ontology:cs_interpretation_layer_present('e77f54d7-fdfd-429c-af06-c42d353b826a').
narrative_ontology:cs_reading_relation('e77f54d7-fdfd-429c-af06-c42d353b826a', zionist_legitimacy_basis__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('e77f54d7-fdfd-429c-af06-c42d353b826a', zionist_legitimacy_basis__settler_colonial_reading, coexists_with).
narrative_ontology:cs_axiom('e77f54d7-fdfd-429c-af06-c42d353b826a', foundational, covenantal_land_grant_binding).
narrative_ontology:cs_axiom_status(covenantal_land_grant_binding, holdable).
narrative_ontology:cs_axiom_grounding('e77f54d7-fdfd-429c-af06-c42d353b826a', covenantal_land_grant_binding, theological).
narrative_ontology:cs_axiom('e77f54d7-fdfd-429c-af06-c42d353b826a', foundational, messianic_redemption_in_progress).
narrative_ontology:cs_axiom_status(messianic_redemption_in_progress, holdable).
narrative_ontology:cs_axiom_grounding('e77f54d7-fdfd-429c-af06-c42d353b826a', messianic_redemption_in_progress, theological).
narrative_ontology:cs_axiom('e77f54d7-fdfd-429c-af06-c42d353b826a', secondary, religious_obligation_overrides_secular_politics).
narrative_ontology:cs_axiom_status(religious_obligation_overrides_secular_politics, holdable).
narrative_ontology:cs_axiom_grounding('e77f54d7-fdfd-429c-af06-c42d353b826a', religious_obligation_overrides_secular_politics, theological).
narrative_ontology:cs_reference_frame('e77f54d7-fdfd-429c-af06-c42d353b826a', covenantal_land_grant_supreme).
narrative_ontology:cs_drift_state('e77f54d7-fdfd-429c-af06-c42d353b826a', post_disengagement_contemporary, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('e77f54d7-fdfd-429c-af06-c42d353b826a', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, religious_settler_movement).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_institutions).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, palestinians_under_occupation).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_citizens).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__religious_restoration_reading, covenantal_land_grant_doctrine).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__religious_restoration_reading, messianic_redemption_process).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__religious_restoration_reading, settlement_as_redemptive_act).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizes and populates settlement in the territories, builds the yeshiva-and-community infrastructure of the blocs and outposts, and mobilizes youth and voters toward territorial goals. Receives state funding, legal protection, and de facto land allocation; its communities hold the territory the framework declares granted. Leaving would mean abandoning homes and dissolving the theological narrative that gives the enterprise its meaning — members experience the project as their religious identity itself, not a position they hold.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, religious_settler_movement, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__religious_restoration_reading, religious_settler_movement, agenda_setter).

% The network of yeshivot, rabbinic bodies, pre-military academies, and religious parties that produces the interpretive rulings — settlement as redemption, withdrawal as forbidden — and the personnel who staff the enterprise. Collects state budgets, political portfolios, and interpretive authority; issues the rulings that other seats treat as binding. Its authority depends on the framework remaining unchallenged from within.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_institutions, agenda_setter,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_institutions, beneficiary).

% The government, military, and civil administration that enforces the territorial regime: builds and protects settlements, administers the occupation's permit systems and courts, and absorbs the diplomatic costs. Governing coalitions depend on religious parties whose price is territorial policy; the state cannot adopt territorial compromise without coalition collapse and, since 2005, without confronting mass internal resistance. It both administers the framework and is bound by it.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Live under military administration they do not govern and cannot vote out: land expropriation for settlements, movement restrictions, permit regimes, home demolitions, and statelessness. Exit from the territory is possible only by emigration that abandons home, livelihood, and family; the framework that allocates their land grants them no seat in the decision.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, palestinians_under_occupation, payer,
    powerless, generational, trapped, regional).

% The majority population that votes, serves in the military, and pays taxes, but whose preferred territorial policies are blocked by the religious veto inside every governing coalition. They bear the military burden of administering the territories, the diplomatic costs of the settlement project, and the internal cultural conflict; exit means emigration at the cost of family, language, and livelihood.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_citizens, payer,
    moderate, biographical, constrained, national).

% Hold citizenship and vote, but live under a state whose legitimacy framework defines the territory as another people's divine-national patrimony; their equal-citizenship claims, land rights, and political representation are persistently subordinated by that identity structure. Exit would mean leaving their homeland; staying means contesting subordination from inside.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, palestinian_citizens_of_israel, payer,
    moderate, generational, constrained, national).

% Israeli left parties, Palestinian leadership factions, and international mediators who hold that the conflict ends in negotiated territorial division. Their position is not argued against within the framework — it is ruled out theologically in advance, which removes them from the conversation regardless of electoral support or diplomatic weight.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, territorial_compromise_advocates, excluded,
    organized, biographical, constrained, global).

% UN bodies, international courts, and foreign ministries that assess the settlement regime against international law, issue advisory opinions and resolutions, and condition relations on territorial policy. They observe the arrangement from outside its legitimacy structure and hold no seat inside it.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, international_legal_institutions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__religious_restoration_reading, religious_settler_movement).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__religious_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives the religious-national community a unified sacred project: aligns yeshivot, youth movements, settlement bodies, and political parties toward a shared territorial-religious goal, and binds individual life narrative — study, settlement, military service — to a collective redemption story that would otherwise fragment into ordinary congregational life.
% TRANSFER_FUNCTION: Moves land and administrative control from Palestinian inhabitants to Jewish national-religious institutions; moves state budget and legal protection to settlement bodies and religious institutions; moves veto power over territorial policy to religious parties; moves the costs of military administration and diplomatic isolation to the state's general population, and the costs of displacement and statelessness to Palestinians.
% ABSENT_VOICES: Palestinians under occupation have no seat in the framework that allocates their land and political status — within its structure they appear only as objects of the covenant, never as parties with claims. Territorial-compromise advocates are not refuted but ruled out in advance, which removes them from the conversation regardless of their support. Secular Israeli majorities that have supported compromise are overridden by coalition veto rather than persuaded.
% DISAPPEARANCE_RATIONALE: If the framework vanished overnight, the settlement enterprise would lose its theological motor and much of its mobilization capacity; religious parties would lose the veto that blocks territorial compromise; coalition politics would reorganize around ordinary policy disagreement; and the maximalist pole of the territorial conflict would collapse into a negotiable dispute. The state would persist, but its territorial politics would be rearranged.
% FOUNDING_PROBLEM: The reading was built to solve a crisis inside traditional Judaism: secular Zionism claimed Jewish sovereignty through human agency, which the rabbinic tradition had long treated as forbidden 'forcing of the end' — and, after 1967, to interpret an unexpected territorial windfall in a frame that made the state's existence and expansion theologically legible rather than accidental. The kabbalistic-humanist synthesis of the elder Rav Kook (the secular state as unwitting instrument of redemption) and its post-1967 radicalization (settlement as direct participation in redemption) is the arrangement's founding solution.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of religious Zionism working outside the movement (Ravitzky's study of messianism and Jewish religious radicalism; Luz's study of the confrontation between Zionism and the rabbinic tradition) attest both the founding problem's historical reality and its contested status. The contemporaneous anti-Zionist rabbinic leadership attested the problem was real while rejecting the covenantal-nationalist solution; Palestinian and international legal scholarship attests the arrangement's operative function from outside the beneficiary set. Within the movement the problem is attested as live. No seat disputes that the problem existed; the dispute is over whether it remains live and whether the solution is legitimate.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__religious_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__religious_restoration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__religious_restoration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__religious_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__religious_restoration_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__religious_restoration_reading_tests).

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
 *   The metric values are reading-indexed over a fixed referent: they record what the religious restoration framework, by its own lights, acknowledges about the standing post-1967 arrangement — not an external audit, and not the arrangement as a critic would measure it. The reading does not deny that the regime imposes costs: it acknowledges the military administration, the expropriations, and the growing enforcement apparatus, and assesses all of them as divinely sanctioned — the price of covenant fulfillment rather than a transfer unjustly taken. That legitimacy override is why extractiveness sits low (0.22 at interval end) while a critic of the same referent would author it several times higher; the discount IS the reading's normative frame, made numeric. Suppression is authored as the raw structural property it is — unscaled by power or scope — and the reading's own record acknowledges the coercive apparatus plainly (it calls it governance): 0.60 at interval end, up from 0.30 at 1967, dipping at Oslo and surging after 2000. Theater stays low from this seat because the reading experiences its apparatus — prayer, study, settlement, ruling — as intrinsically functional divine service; the slow rise (0.08 to 0.16) tracks the growing interpretive labor required to keep adverse events inside the redemptive narrative. Accessibility collapse is high (0.87) because from inside the frame the alternatives are not weak but void — compromise is ruled out before argument begins. Resistance (0.60) is the one metric on which the reading and its critics roughly agree: two intifadas, secular Israeli opposition, and international legal pressure are visible from every seat. The claimed_type is the reading's own claim, authored independently of the metrics: the divine grant is held to be as fixed as natural law, hence emerges_naturally from this seat. The divergence between that claim and the declared beneficiary structure is precisely what the false-summit machinery exists to measure; nothing here is reconciled to any predicted engine output. The identity_coordination declaration carries a known gaming risk — identity frames are the most common cover for extraction — and is declared because the identity function is genuine (the yeshiva system, the youth movements, the meaning-structure) while the transfer of land and sovereignty rides on it; the coupling pattern (costs concentrated on the powerless at regional scale while the identity frame operates nationally) is exactly the signature the guidance says to flag. Measurements run on one shared eight-point grid so no metric's series is silently backfilled from another's.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different arrangements from the same structure. From the beneficiary seats the arrangement is covenant fulfillment: the settler movement experiences state funding, legal protection, and land as the substance of redemption, and its identity-lock means exit is not costly but unthinkable — the enterprise is who they are. From the palestinians_under_occupation seat the same structure is pure imposition: no coordination benefit is received, no seat exists in the framework that allocates their land, and exit is trapped. Secular Israelis compute a constrained politics — a veto they did not consent to operating inside every coalition — while palestinian_citizens_of_israel compute subordinated citizenship. The israeli_state_apparatus seat is genuinely dual: it enforces the arrangement and absorbs its diplomatic and military costs, so its computed position should sit between an enforcer's and a payer's. The engine computes each of these from the structural data; the reading's own seat (this file) cannot see the divergence from inside, which is exactly why the per-seat computation exists. Coalition potential: the powerless seat's resistance has historically taken coalition form — the intifadas and boycott movements are the one lever that has visibly moved enforcement costs, and the dip-then-surge shape of the suppression series brackets that episode.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the low end: religious_settler_movement and religious_zionist_institutions collect land, budgets, and authority, so their d sits near the beneficiary end; their identity_lock does not push them toward the target end (lock keeps a beneficiary inside its benefit stream — it is a cage only in the sense that the benefit and the identity are fused) but it does mean their position cannot be bought out at any price the state can pay. Victim declarations drive the high end: palestinians_under_occupation (powerless, trapped) sit at or near full target — no exit, no seat; secular_israeli_citizens (moderate, constrained) and palestinian_citizens_of_israel (moderate, constrained) sit high but below full target because exit exists at real cost. The israeli_state_apparatus declares no beneficiary or victim position because it genuinely holds both sides of the relation — it enforces the arrangement and pays its strategic costs; the derivation's fallback plus its constrained exit should place it mid-range, and the commentary flags the dual position rather than forcing it with an override (an override keyed to the institutional power atom would also capture the analytical observer, whose exit options already differentiate the two seats). territorial_compromise_advocates are excluded rather than coordinated — the foreclosure of their position is the enforcement object itself, and they are authored on the excluded seat, not in the beneficiary or victim arrays.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Against pure-extraction mislabeling: the framework does solve a real coordination problem — it unifies a dispersed religious-national community around a shared project, funds and staffs institutions, and gives individual life narrative a collective meaning-structure; a pure-extraction reading would miss why the arrangement commands genuine sacrifice from its adherents. Against coordination mislabeling: the same structure transfers land and sovereignty from a population with no seat to a movement with an identity-lock, and rules out the compromise position in advance rather than defeating it politically — which is why the beneficiary and victim declarations are both required and why the mountain-claim is flagged for false-summit evaluation. On mandatrophy proper: the founding problem (making Jewish sovereignty theologically legible against the rabbinic tradition's prohibition on human-driven redemption, and interpreting 1967's territorial windfall) remains live inside the reading's community — no atrophy is declared; the framework's function has not outlived its mandate from its own seat, and the founding-problem status is authored 'contested' because the seats dispute exactly that.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_grant_naturality,
    'Is the covenantal land grant a genuine structural feature of reality — divine law as binding as physics, as the reading holds — or a constructed legitimacy arrangement whose costs fall on identifiable non-beneficiaries?',
    'Not resolvable by ordinary evidence; the structural test is counterfactual persistence — would the territorial arrangement hold its current form if no party defended or enforced it? — combined with the documented beneficiary structure: if identifiable parties collect from the arrangement''s operation, the naturality claim is performing legitimacy work rather than describing a limit.',
    'If constructed, the reading''s mountain-claim is a false summit: the arrangement reclassifies as a coordination/extraction hybrid — genuine community coordination fused with displacement costs borne by parties with no seat in the framework. If genuine, the arrangement''s costs are the price of a real limit and the beneficiary structure is incidental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_grant_naturality, conceptual, 'Whether the divine-grant naturality claim describes reality or performs legitimacy work for identifiable beneficiaries.').

omega_variable(
    kernel_reading_underdetermination,
    'This story instantiates one reading (religious_restoration_reading) of the zionist_legitimacy_basis kernel; the disagreement between readings is located in the source of legitimacy — divine grant (this reading) versus national self-determination versus colonial dispossession. What would the sibling readings change structurally?',
    'No empirical resolution: the readings are held by different parties over the same fixed referent arrangement. Each reading is authored as its own constraint file; cross-reading comparison is valid precisely because the referent is held constant.',
    'The national-liberation sibling would restore territorial flexibility (no theological foreclosure of compromise) and shrink the victim set''s policy-constraint component; the settler-colonial sibling would author high epsilon for the same referent, name Palestinians as primary rights-bearers, and classify the arrangement as extraction without coordination cover. Same referent, three epsilon values, three victim sets, three types.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer structure: which kernel, which reading this file instantiates, where the readings diverge, and what each sibling would change.').

omega_variable(
    messianic_falsifiability,
    'Is the reading''s messianic-process axiom empirically insulated? The reading indexes redemption to historical events; what event would its interpretive apparatus accept as counting against the process?',
    'Track the apparatus''s actual interpretive behavior across disconfirming events: the 2005 disengagement was reinterpreted as a trial rather than a falsification. If every adverse event is absorbed as confirmation, the axiom is unfalsifiable-in-practice.',
    'If insulated, the framework''s persistence does not depend on outcomes — enforcement capacity and identity-lock carry it alone, and the high accessibility collapse is structural rather than evidence-responsive. If falsifiable, territorial outcomes could in principle dissolve the framework''s mandate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_falsifiability, empirical, 'Whether the messianic axiom is empirically insulated or outcome-sensitive.').

omega_variable(
    suppression_internalization,
    'Is the framework''s hold on secular Israeli politics structural (coalition arithmetic, religious-party veto, state funding dependencies) or internalized (secular absorption of the restoration narrative — the sacred frame adopted by the very seats it constrains)?',
    'Policy behavior under secular-majority governments: if governments without religious coalition partners still avoid territorial compromise, the lock is internalized beyond structural veto points; supplement with survey and elite-discourse analysis of covenant language adoption among secular elites.',
    'If internalized, suppression persists even when structural veto points weaken — the framework travels inside its targets and removal requires generational cultural change. If structural, electoral or coalition reform could release the constraint quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Structural versus internalized suppression mechanism for the secular-Israeli seat.').

omega_variable(
    victim_set_boundary,
    'Are Palestinian citizens of Israel victims of THIS constraint (the legitimacy basis) or of adjacent legal instruments (nation-state legislation, land administration regimes)? The epsilon-invariance rule requires a stable victim set per story.',
    'Decomposition test: does the subordination of equal-citizenship claims follow from the legitimacy framework alone, or does it require the adjacent statutory machinery? If separable, author the statutory machinery as its own story and link with network edges.',
    'If separable, this story''s victim set shrinks to palestinians_under_occupation and secular_israeli_citizens, narrowing the measured extraction spread; if inseparable, the framework''s victim set includes Israel''s Palestinian citizenry and the extraction profile widens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_boundary, conceptual, 'Victim-set boundary between the legitimacy framework and adjacent statutory constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__religious_restoration_reading, 0, 58).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t0, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(zion_tr_t0, observed).
narrative_ontology:measurement(zion_tr_t10, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement_basis(zion_tr_t10, observed).
narrative_ontology:measurement(zion_tr_t19, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 19, 0.1).
narrative_ontology:measurement_basis(zion_tr_t19, observed).
narrative_ontology:measurement(zion_tr_t26, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 26, 0.11).
narrative_ontology:measurement_basis(zion_tr_t26, observed).
narrative_ontology:measurement(zion_tr_t33, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 33, 0.13).
narrative_ontology:measurement_basis(zion_tr_t33, observed).
narrative_ontology:measurement(zion_tr_t38, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 38, 0.15).
narrative_ontology:measurement_basis(zion_tr_t38, observed).
narrative_ontology:measurement(zion_tr_t48, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 48, 0.15).
narrative_ontology:measurement_basis(zion_tr_t48, observed).
narrative_ontology:measurement(zion_tr_t58, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 58, 0.16).
narrative_ontology:measurement_basis(zion_tr_t58, observed).

% Extraction over time
narrative_ontology:measurement(zion_be_t0, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(zion_be_t0, observed).
narrative_ontology:measurement(zion_be_t10, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 10, 0.16).
narrative_ontology:measurement_basis(zion_be_t10, observed).
narrative_ontology:measurement(zion_be_t19, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 19, 0.18).
narrative_ontology:measurement_basis(zion_be_t19, observed).
narrative_ontology:measurement(zion_be_t26, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 26, 0.19).
narrative_ontology:measurement_basis(zion_be_t26, observed).
narrative_ontology:measurement(zion_be_t33, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 33, 0.2).
narrative_ontology:measurement_basis(zion_be_t33, observed).
narrative_ontology:measurement(zion_be_t38, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 38, 0.2).
narrative_ontology:measurement_basis(zion_be_t38, observed).
narrative_ontology:measurement(zion_be_t48, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 48, 0.21).
narrative_ontology:measurement_basis(zion_be_t48, observed).
narrative_ontology:measurement(zion_be_t58, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 58, 0.22).
narrative_ontology:measurement_basis(zion_be_t58, observed).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t0, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(zion_su_t0, observed).
narrative_ontology:measurement(zion_su_t10, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement_basis(zion_su_t10, observed).
narrative_ontology:measurement(zion_su_t19, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 19, 0.4).
narrative_ontology:measurement_basis(zion_su_t19, observed).
narrative_ontology:measurement(zion_su_t26, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 26, 0.38).
narrative_ontology:measurement_basis(zion_su_t26, observed).
narrative_ontology:measurement(zion_su_t33, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 33, 0.5).
narrative_ontology:measurement_basis(zion_su_t33, observed).
narrative_ontology:measurement(zion_su_t38, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 38, 0.55).
narrative_ontology:measurement_basis(zion_su_t38, observed).
narrative_ontology:measurement(zion_su_t48, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 48, 0.58).
narrative_ontology:measurement_basis(zion_su_t48, observed).
narrative_ontology:measurement(zion_su_t58, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 58, 0.6).
narrative_ontology:measurement_basis(zion_su_t58, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__religious_restoration_reading, identity_coordination).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis__national_liberation_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis__settler_colonial_reading).

% DUAL FORMULATION NOTE:
% Constraint family: 'Zionism's legitimacy basis' is one contested kernel decomposed into three readings over a fixed referent (the standing post-1967 arrangement). This file is the religious_restoration_reading; the national-liberation and settler-colonial readings are sibling files. Per the epsilon-invariance principle, the colloquial label 'Zionism' covers structurally distinct claims — divine-grant legitimacy, national self-determination, colonial dispossession — with different epsilon values, different victim sets, and different types; forcing one story to carry all three would make epsilon observer-dependent. Influence structure: the national-liberation reading historically preceded this one and still resources it (shared institutions and mobilization), while the settler-colonial reading is downstream of this reading's post-1967 maximalism, which supplies its strongest evidence. Edges here run from this reading to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
