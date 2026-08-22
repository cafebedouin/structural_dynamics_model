% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__settler_colonial_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__settler_colonial_reading
 *   human_readable: Settler-Colonial Displacement Regime Reading of Jewish Sovereignty in Palestine
 *   domain: political philosophy/nationalism studies/postcolonial theory
 *
 * SUMMARY:
 *   Since the 1880s, an organized regime of Jewish immigration, land
 *   acquisition, and eventually state formation has operated in Palestine.
 *   This story authors that standing arrangement as the settler-colonial
 *   reading sees it: a displacement structure in which the transfer of land,
 *   dwellings, and political sovereignty from the country's Arab inhabitants
 *   to arriving Jewish immigrants and their imperial sponsors is constitutive
 *   of the arrangement, not an incidental cost of it. The epsilon referent is
 *   fixed per the kernel-reading rule: the existing sovereignty arrangement
 *   as this reading assesses it — never the binational or civic alternative
 *   this reading's allies or critics might prefer. KEY AGENTS (by structural
 *   relationship): colonial_metropole_sponsors — imperial beneficiary and
 *   initial agenda author (institutional/arbitrage), Balfour-Mandate through
 *   American patronage; jewish_settler_immigrants — resident beneficiary
 *   cohort (organized/identity_locked), positioned by this reading as
 *   settlers regardless of refugee motive; palestinian_displaced_refugees —
 *   primary target, the displaced of 1948 and 1967 and their descendants
 *   (powerless/trapped); palestinians_under_occupation — ongoing target under
 *   military administration and settlement expansion (moderate/constrained);
 *   palestinian_citizens_of_israel — absorbed remnant inside the Green Line
 *   (moderate/constrained); israeli_state_institutions — administering
 *   agenda-setter running land law, citizenship asymmetry, and settlement
 *   planning (institutional/arbitrage); un_and_international_legal_bodies —
 *   analytical observer documenting legality without enforcement power;
 *   binational_alternative_advocates — excluded voice whose alternatives were
 *   marginalized by both national movements.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__settler_colonial_reading, 0.88).
domain_priors:suppression_score(jewish_sovereignty_palestine__settler_colonial_reading, 0.84).
domain_priors:theater_ratio(jewish_sovereignty_palestine__settler_colonial_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 0.84).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, accessibility_collapse, 0.74).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__settler_colonial_reading, snare).
narrative_ontology:human_readable(jewish_sovereignty_palestine__settler_colonial_reading, "Settler-Colonial Displacement Regime Reading of Jewish Sovereignty in Palestine").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__settler_colonial_reading, "political philosophy/nationalism studies/postcolonial theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__settler_colonial_reading, '208ad854-11a5-4729-bf43-4c08279cfb81').
narrative_ontology:cs_kernel_codification('208ad854-11a5-4729-bf43-4c08279cfb81', formalized).
narrative_ontology:cs_authority_grounding('208ad854-11a5-4729-bf43-4c08279cfb81', extraction).
narrative_ontology:cs_interpretation_layer_present('208ad854-11a5-4729-bf43-4c08279cfb81').
narrative_ontology:cs_reading_relation('208ad854-11a5-4729-bf43-4c08279cfb81', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('208ad854-11a5-4729-bf43-4c08279cfb81', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('208ad854-11a5-4729-bf43-4c08279cfb81', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('208ad854-11a5-4729-bf43-4c08279cfb81', jewish_sovereignty_palestine__post_zionist_reading, influences).
narrative_ontology:cs_axiom('208ad854-11a5-4729-bf43-4c08279cfb81', foundational, territorial_realization_constitutively_displacive).
narrative_ontology:cs_axiom_status(territorial_realization_constitutively_displacive, holdable).
narrative_ontology:cs_axiom_grounding('208ad854-11a5-4729-bf43-4c08279cfb81', territorial_realization_constitutively_displacive, empirically_contingent).
narrative_ontology:cs_axiom('208ad854-11a5-4729-bf43-4c08279cfb81', foundational, intent_irrelevant_to_structural_classification).
narrative_ontology:cs_axiom_status(intent_irrelevant_to_structural_classification, holdable).
narrative_ontology:cs_axiom_grounding('208ad854-11a5-4729-bf43-4c08279cfb81', intent_irrelevant_to_structural_classification, conventional).
narrative_ontology:cs_reference_frame('208ad854-11a5-4729-bf43-4c08279cfb81', pre_zionist_palestinian_possession_order).
narrative_ontology:cs_drift_state('208ad854-11a5-4729-bf43-4c08279cfb81', contemporary_post_1948_order, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('208ad854-11a5-4729-bf43-4c08279cfb81', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, colonial_metropole_sponsors).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, jewish_settler_immigrants).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_displaced_refugees).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinians_under_occupation).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_citizens_of_israel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__settler_colonial_reading, wolfe_settler_colonial_structure_not_event_thesis).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__settler_colonial_reading, nakba_continuity_historiography).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Britain issued the Balfour Declaration and administered the Mandate, using the national-home policy to secure wartime alignment and Mediterranean positioning; after 1948 the patron role passed to the United States, which supplies military aid, diplomatic cover, and strategic cooperation. Both collect strategic rents from the arrangement without bearing its local costs, and both can reposition sponsorship — the arbitrage that lets them sit at the beneficiary end of the structure.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, colonial_metropole_sponsors, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__settler_colonial_reading, colonial_metropole_sponsors, agenda_setter).

% Arrived in waves — ideological pioneers purchasing land, refugees fleeing pogroms and Nazism, later Jews expelled from Arab states. Under this reading's frame they occupy the settler position regardless of motive: they received land, housing, and citizenship within a structure that removed the prior inhabitants, and their descendants are born into a polity whose territory and demography the regime maintains. Exit would mean renouncing the national project that constitutes collective identity and their only sovereign refuge, which is why the lock binds even those who privately dissent from expansion.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, jewish_settler_immigrants, beneficiary,
    organized, generational, identity_locked, regional).

% Roughly seven hundred thousand displaced in 1948 and further hundreds of thousands in 1967; property taken under absentee-custodian laws; return denied as a matter of state policy; statelessness inherited across generations in host countries that variously integrate, restrict, or instrumentalize them. Their exit is blocked twice over — by the denial of return and by host-state limits on local belonging — leaving the refugee registry itself as the intergenerational carrier of the claim.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_displaced_refugees, payer,
    powerless, generational, trapped, regional).

% West Bank and Gaza residents live under military administration, settlement expansion onto their land, movement restrictions, and recurring large-scale violence, with no sovereignty over the territory they inhabit. Historically some accessed wage labor and services inside the surrounding economy; exit means emigration, which the educated and mobile exercise at steady rates while the rest remain under an administration they do not govern.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinians_under_occupation, payer,
    moderate, biographical, constrained, regional).

% The remnant that stayed inside the 1949 armistice line: formal citizens with voting rights and state services, while land planning, resource allocation, and the state's symbolic constitution subordinate them as the non-Jewish minority. The reading classifies them as the absorbed natives of the settler polity — included enough to be taxed and conscripted-adjacent, excluded from the national ownership the state distributes.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_citizens_of_israel, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_citizens_of_israel, beneficiary).

% Administers the regime day to day: land registries and the absentee-custodian, the citizenship asymmetry between the Law of Return and the denial of refugee return, military government and settlement planning in the territories. Sets the agenda for expansion or retrenchment and can reposition internally — annex, freeze, withdraw, redefine — but cannot exit the structure without dissolving the founding identity of the state itself, so its arbitrage operates entirely inside the arrangement it runs.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% UNSCOP proposed partition; Resolution 194 affirmed return; the Fourth Geneva Convention's applicability, ICJ advisory opinions, and ICC inquiries document the arrangement's legality questions. They adjudicate and record without possessing enforcement power over the parties, which fixes their seat as analytical: their findings enter the structure as legitimacy pressure, not as command.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, un_and_international_legal_bodies, observer,
    institutional, generational, analytical, global).

% Brit Shalom, Ihud, and later one-state advocates proposed shared civic frameworks before and during the state-building period. Lacking mass constituencies on either side, they were marginalized by both national movements; their proposals survive in archives and academic debate while the arrangement they warned against consolidated. They would object that the zero-sum framing was chosen, not fated — and they were never in the room where it was chosen.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, binational_alternative_advocates, excluded,
    moderate, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__settler_colonial_reading, jewish_settler_immigrants).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organized the transfer, settlement, and absorption of persecuted Jewish populations onto land and within a polity: land assembly, institution-building, defense, and a sovereign umbrella solved collectively what scattered individual migration could not, and gave the sponsoring powers a fixed strategic position in the eastern Mediterranean.
% TRANSFER_FUNCTION: Moves land, dwellings, and political sovereignty from Palestine's Arab inhabitants to arriving Jewish immigrants and their successor state; moves strategic positioning rents to the sponsoring metropole; moves the costs of Europe's persecution of Jews onto the displaced and occupied.
% ABSENT_VOICES: The displaced of 1948 were absent from every arrangement that disposed of their property and return; the country's inhabitants had no seat when the Balfour Declaration allocated their homeland as a national home (the King-Crane Commission recorded their objection and was ignored); binational advocates were pushed out of both national conversations; neighboring Arab populations were consulted only after decisions were made.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, the citizenship, property titles, and security guarantees of millions would dissolve simultaneously; the refugee regime, the regional alliance system, the American strategic posture in the Mediterranean, and the nuclear balance would all reorganize; immediate contestation over territory and return would begin before any replacement order stabilized. Nothing about the current regional order survives the arrangement's removal intact.
% FOUNDING_PROBLEM: How could a persecuted, stateless European minority secure collective physical safety and a guaranteed refuge after centuries of expulsion culminating in the Holocaust — and, concurrently, how could the sponsoring empire secure wartime alignment and postwar positioning in the Middle East?
% FOUNDING_PROBLEM_CORROBORATION: Holocaust historiography and demographic records corroborate the founding problem's reality from well outside the beneficiary set; UN Resolution 194, ICJ proceedings, and the documentation of major human-rights organizations corroborate the displacement's continuity from outside it as well. No outside source attests that the founding problem still requires this particular arrangement: security studies are divided, and Palestinian scholarship attests that the problem the arrangement now chiefly solves is its own perpetuation. Corroboration splits cleanly along the beneficiary boundary, which is itself the signal.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__settler_colonial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__settler_colonial_reading, 0.88, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.88) because the reading holds the transfer to be constitutive: the land, homes, and sovereignty the beneficiary side receives ARE the assets removed from the victim side, a zero-sum structure with no separable coordination dividend. Suppression (0.84) is a raw structural scalar, unscaled by power or scope — it encodes denial of return, absentee-property confiscation, military administration, movement restriction, and blockade; the engine scales only extractiveness by directionality and scope. Theater_ratio (0.31) tracks the legitimating layer: early Zionism was candidly colonial in vocabulary (Jewish Colonial Trust, avowed transfer thinking), and the performative share rose as refuge, security, and democracy narratives replaced candid colonial language, peaking around the Oslo-era narrative before thinning as major human-rights organizations and international legal bodies shifted to apartheid and permanence framings. Accessibility_collapse (0.74) is snare-typical: return, binationalism, and power-sharing have been foreclosed in practice while remaining alive in discourse — collapsed far short of a natural law's near-total closure, far beyond a rope's open menu. Resistance (0.78) is high and continuous: revolts, intifadas, armed and civic struggle, boycott movements, litigation. Claimed type is snare, authored independently of the metrics: the reading's core move — displacement regardless of intent — is precisely the claim that the refuge and self-determination story functions as cover for a structure that persists by coercion and suppressed exits, with identifiable victims. The strongest rival classification, tangled_rope, is routed to the zero_sum_vs_positive_sum_intervals omega rather than reconciled here. All three metric series run on one shared eight-point grid (1882-2023) so no metric row borrows another's endpoints.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute radically different types from identical structural data. From the settler seat, the arrangement is homecoming and refuge — the constraint subsidizes them, and identity_lock binds them to it as constitutive of collective survival, not experienced as imposition. From the metropole seat, it is a strategic asset that paid for itself. From the payer seats, the same structure operates as elimination: the refugee seat experiences permanent exile enforced by denial of return; the occupied seat experiences administered dispossession; the citizen seat experiences subordinate inclusion. On coalition: the victim class is numerous but its coalition power is latent and unrealized — fragmented across diaspora jurisdictions with divergent host-state interests, split internally between competing representative institutions, and facing a beneficiary side whose identity_lock converts numerical disadvantage into total mobilization. The engine should see powerless/trapped atoms whose coalition potential exists on paper and has repeatedly failed to consolidate.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: the metropole sits nearest the beneficiary end (d near 0.0) with arbitrage-grade exit — it repositioned from Britain to the United States and can adjust sponsorship without bearing local costs. The settler cohort also derives low d as a declared beneficiary receiving land, housing, and citizenship; its identity_lock does not amplify chi because amplification applies to targets — for beneficiaries, lock sustains the subsidy. Victim declarations drive high d: displaced refugees, trapped and stateless, sit nearest the full-target end; occupied Palestinians, constrained but present, sit slightly below; citizens of Israel carry a dual declaration (payer with beneficiary secondary) placing them at intermediate d — formally included, structurally subordinated. Scope amplification is modest: the arrangement's operative scope is regional-national, with the metropole's global reach entering through its own seat rather than inflating the local extraction arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing collective safety for a persecuted, stateless minority after centuries of expulsion culminating in the Holocaust — was real and is corroborated from outside the beneficiary set. Statehood resolved the statelessness core of that problem; the arrangement nonetheless persisted and expanded past the resolution point. The reading locates the persistence driver in the structure's own expansionary logic (settlement as structure, not event) rather than in the founding need, which is why the R5 status is authored contested rather than dead: beneficiaries attest the security problem is live, while outside documentation attests that what the arrangement now chiefly produces is the conflict that justifies it. The classification discipline cuts both ways: refusing the rope rescue ('refuge was achieved, therefore coordination') keeps the victims structurally visible instead of letting a real historical good launder an ongoing extraction; conversely, the positive-sum omega prevents overclaiming pure snare if the economic record showed a durable coordination layer. Because status is contested and the verdict is world_rearranges, the mismatch consumer should register a contested genealogy, not a zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (settler_colonial_reading) of the kernel jewish_sovereignty_palestine; do the sibling readings (liberal_nationalist, religious_zionist, cultural_zionist, post_zionist) instantiate structurally different constraints with different victim sets, beneficiary attributions, and epsilon?',
    'Comparative compilation of the five sibling stories and cross-reading seat-classification diff: where sibling victim sets and beneficiary attributions diverge, the kernel is indexically contested and no single epsilon covers it.',
    'If a sibling reading better fits the operative structure, this story''s epsilon drops sharply (the liberal-nationalist reading would author the same referent as legitimate self-determination with contingent harms), the victim set re-scopes, and the snare claim collapses into rope-or-tangled-rope territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Kernel-level indexicality: which reading of the sovereignty kernel captures the operative constraint.').

omega_variable(
    refugee_status_vs_structural_position,
    'The reading classifies Jewish immigrants as settlers regardless of refugee status; is the structural position (receiver of transferred land within a displacement regime) or the agent''s own prior victimhood (pogrom survivors, Holocaust refugees, Jews expelled from Arab states) analytically decisive for directionality?',
    'Cohort disaggregation: measure differential land/property receipt and benefit accrual across immigration waves (ideological pioneers, Holocaust survivors, Mizrahi refugees) versus each cohort''s pre-arrival dispossession.',
    'If large cohorts were themselves displaced persons with no alternative destination and received proportionally little expropriated property, the uniform settler classification weakens toward a dual-victim structure, lowering effective extraction attribution to those cohorts and complicating the beneficiary declaration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refugee_status_vs_structural_position, empirical, 'Whether refugee origin defeats or survives the structural settler classification.').

omega_variable(
    zero_sum_vs_positive_sum_intervals,
    'Is the territorial logic strictly zero-sum (Palestinian loss equals settler gain, making the coordination story cover), or were there substantial positive-sum intervals (wage labor, health infrastructure, economic growth accruing to some Palestinians) that would constitute a genuine coordination layer?',
    'Economic-historical accounting of Palestinian welfare trajectories under the Mandate and Israeli periods, decomposed by region and class, net of displacement losses.',
    'Substantial durable positive-sum transfer would push the classification toward tangled_rope (genuine coordination plus asymmetric extraction) rather than snare; confirmation of zero-sum structure supports the snare claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zero_sum_vs_positive_sum_intervals, empirical, 'Snare-versus-tangled-rope boundary: whether any genuine coordination benefit survives the displacement accounting.').

omega_variable(
    metropole_principal_vs_agent,
    'After 1948, is the metropole (now the United States) the principal beneficiary collecting strategic rents, or has agency and receipt passed wholly to the settler state with the patron reduced to guarantor?',
    'Trace strategic rent flows (basin access, intelligence cooperation, regional deterrence value, arms-industry integration) against settler-state autonomy in initiating expansion the patron opposes.',
    'If the settler state is the principal, gain_flow attribution stabilizes on the resident seat and the metropole''s role decays to enabler; if the patron still captures the strategic surplus, the colonial-metropole beneficiary declaration remains load-bearing and agenda-setting authority is partly external.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metropole_principal_vs_agent, empirical, 'Who captures the strategic rents of the arrangement after the British-to-American patron shift.').

omega_variable(
    elimination_logic_vs_inertial_drift,
    'Does the regime''s persistence track an active eliminatory-expansionary logic (snare persists and intensifies) or increasingly inertial bureaucratic momentum (risk of piton drift if the frontier closes and maintenance turns theatrical)?',
    'Longitudinal comparison of settlement expansion rates, enforcement expenditure, and theater_ratio after any stabilization of the territorial frontier: active logic shows continued frontier movement; inertia shows flat frontier with rising performative legitimation.',
    'An inertial trajectory would forecast degradation toward piton (diffuse costs, no concentrated maintainer benefit) rather than indefinite snare persistence; an active trajectory confirms the snare reading''s core eliminatory-structure claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(elimination_logic_vs_inertial_drift, conceptual, 'Lifecycle question: active eliminatory logic versus eventual inertial decay.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__settler_colonial_reading, 1882, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jsp_scr_meas_tr_t1882, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1882, 0.08).
narrative_ontology:measurement_basis(jsp_scr_meas_tr_t1882, observed).
narrative_ontology:measurement(jsp_scr_meas_tr_t1917, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1917, 0.13).
narrative_ontology:measurement_basis(jsp_scr_meas_tr_t1917, observed).
narrative_ontology:measurement(jsp_scr_meas_tr_t1936, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1936, 0.19).
narrative_ontology:measurement_basis(jsp_scr_meas_tr_t1936, observed).
narrative_ontology:measurement(jsp_scr_meas_tr_t1948, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1948, 0.27).
narrative_ontology:measurement_basis(jsp_scr_meas_tr_t1948, observed).
narrative_ontology:measurement(jsp_scr_meas_tr_t1967, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1967, 0.33).
narrative_ontology:measurement_basis(jsp_scr_meas_tr_t1967, observed).
narrative_ontology:measurement(jsp_scr_meas_tr_t1987, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1987, 0.37).
narrative_ontology:measurement_basis(jsp_scr_meas_tr_t1987, observed).
narrative_ontology:measurement(jsp_scr_meas_tr_t2000, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 2000, 0.34).
narrative_ontology:measurement_basis(jsp_scr_meas_tr_t2000, observed).
narrative_ontology:measurement(jsp_scr_meas_tr_t2023, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 2023, 0.31).
narrative_ontology:measurement_basis(jsp_scr_meas_tr_t2023, observed).

% Extraction over time
narrative_ontology:measurement(jsp_scr_meas_be_t1882, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1882, 0.22).
narrative_ontology:measurement_basis(jsp_scr_meas_be_t1882, observed).
narrative_ontology:measurement(jsp_scr_meas_be_t1917, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1917, 0.38).
narrative_ontology:measurement_basis(jsp_scr_meas_be_t1917, observed).
narrative_ontology:measurement(jsp_scr_meas_be_t1936, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1936, 0.52).
narrative_ontology:measurement_basis(jsp_scr_meas_be_t1936, observed).
narrative_ontology:measurement(jsp_scr_meas_be_t1948, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1948, 0.78).
narrative_ontology:measurement_basis(jsp_scr_meas_be_t1948, observed).
narrative_ontology:measurement(jsp_scr_meas_be_t1967, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1967, 0.84).
narrative_ontology:measurement_basis(jsp_scr_meas_be_t1967, observed).
narrative_ontology:measurement(jsp_scr_meas_be_t1987, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1987, 0.85).
narrative_ontology:measurement_basis(jsp_scr_meas_be_t1987, observed).
narrative_ontology:measurement(jsp_scr_meas_be_t2000, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 2000, 0.86).
narrative_ontology:measurement_basis(jsp_scr_meas_be_t2000, observed).
narrative_ontology:measurement(jsp_scr_meas_be_t2023, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 2023, 0.88).
narrative_ontology:measurement_basis(jsp_scr_meas_be_t2023, observed).

% Suppression requirement over time
narrative_ontology:measurement(jsp_scr_meas_su_t1882, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1882, 0.14).
narrative_ontology:measurement_basis(jsp_scr_meas_su_t1882, observed).
narrative_ontology:measurement(jsp_scr_meas_su_t1917, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1917, 0.3).
narrative_ontology:measurement_basis(jsp_scr_meas_su_t1917, observed).
narrative_ontology:measurement(jsp_scr_meas_su_t1936, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1936, 0.56).
narrative_ontology:measurement_basis(jsp_scr_meas_su_t1936, observed).
narrative_ontology:measurement(jsp_scr_meas_su_t1948, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1948, 0.76).
narrative_ontology:measurement_basis(jsp_scr_meas_su_t1948, observed).
narrative_ontology:measurement(jsp_scr_meas_su_t1967, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1967, 0.71).
narrative_ontology:measurement_basis(jsp_scr_meas_su_t1967, observed).
narrative_ontology:measurement(jsp_scr_meas_su_t1987, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1987, 0.76).
narrative_ontology:measurement_basis(jsp_scr_meas_su_t1987, observed).
narrative_ontology:measurement(jsp_scr_meas_su_t2000, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 2000, 0.81).
narrative_ontology:measurement_basis(jsp_scr_meas_su_t2000, observed).
narrative_ontology:measurement(jsp_scr_meas_su_t2023, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 2023, 0.84).
narrative_ontology:measurement_basis(jsp_scr_meas_su_t2023, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__settler_colonial_reading, resource_allocation).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Zionism' conflates five structurally distinct claims about one kernel (jewish_sovereignty_palestine). Each sibling is a separate story with its own epsilon, victim set, and beneficiary attribution; they are linked here because the settler-colonial reading is the maximally extractive member and functions as the adversarial bound on the family — the liberal-nationalist reading's legitimacy claim is what this reading directly contests, and the post-Zionist reading inherits this reading's historiography while rejecting its terminal implications. Per the epsilon-invariance principle, no attempt is made to average across readings; the contest lives in the omega variables, not in a hedged epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
