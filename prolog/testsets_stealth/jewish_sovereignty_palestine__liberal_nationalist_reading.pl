% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__liberal_nationalist_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__liberal_nationalist_reading
 *   human_readable: Jewish Collective Self-Determination in the Ancestral Homeland (Liberal Nationalist Reading)
 *   domain: political philosophy/nationalism studies/postcolonial theory
 *
 * SUMMARY:
 *   This story instantiates the liberal nationalist reading of Jewish
 *   sovereignty in Palestine: the Jewish people hold a collective right of
 *   self-determination, and statehood in the ancestral homeland is its
 *   legitimate exercise. On this reading the standing arrangement — a Jewish
 *   nation-state exercising sovereignty over the territory, with military
 *   control since 1967 over the West Bank and a blockaded Gaza — is
 *   legitimate at its core but carries real obligations it has not
 *   discharged: Palestinians enter as co-equal self-determination claimants,
 *   and a partition or binational framework is the expected terminus. The
 *   arrangement therefore coordinates genuinely (protection and continuity
 *   for a historically stateless, persecuted minority) while simultaneously
 *   imposing asymmetric costs on Palestinians who lack matching sovereignty.
 *   This file is one member of a five-story constraint family decomposing the
 *   colloquial label 'Jewish sovereignty in Palestine'; the sibling readings
 *   are separate constraints with their own epsilon values, linked via
 *   network.affects_constraints (see network.dual_formulation_note). Per the
 *   kernel-reading rule, epsilon here refers to the standing arrangement as
 *   this reading assesses it — never to the binational or partitioned
 *   alternative this reading endorses.
 *
 * KEY AGENTS:
 *   - - israeli_jewish_citizenry: Primary beneficiary (organized/constrained) — collects sovereignty, security, land allocation, and immigration rights; bound by collective identity and conscription ties
 *   - - israeli_government: Agenda setter (institutional/arbitrage) — administers settlement policy, occupation apparatus, and final-status diplomacy; electorally accountable only to the Jewish citizenry
 *   - - west_bank_palestinians: Primary target (powerless/trapped) — governed by military administration they cannot vote out
 *   - - gaza_residents: Target (powerless/trapped) — lives under blockade administered without their consent
 *   - - palestinian_refugee_diaspora: Excluded claimant-payer (moderate/mobile) — bears intergenerational displacement costs while sitting outside the negotiating table
 *   - - palestinian_citizens_of_israel: Dual-positioned payer-beneficiary (moderate/constrained) — formal citizenship inside a state constituted by another people's nationality
 *   - - diaspora_jewish_communities: Secondary beneficiary (powerful/mobile) — draws insurance and identity value; exposure to costs is indirect and dialable
 *   - - palestinian_authority: Subordinate agenda-setter (moderate/trapped) — administers civil affairs on sufferance; neither delivers statehood nor can dissolve itself
 *   - - international_community: Analytical observer (institutional/analytical) — issues resolutions, rulings, and funding conditionality; bears none of the daily costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.55).
domain_priors:suppression_score(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.7).
domain_priors:theater_ratio(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__liberal_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__liberal_nationalist_reading, "Jewish Collective Self-Determination in the Ancestral Homeland (Liberal Nationalist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__liberal_nationalist_reading, "political philosophy/nationalism studies/postcolonial theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__liberal_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__liberal_nationalist_reading, 'c212c835-7497-4762-bb34-447abfbb44e2').
narrative_ontology:cs_kernel_codification('c212c835-7497-4762-bb34-447abfbb44e2', formalized).
narrative_ontology:cs_authority_grounding('c212c835-7497-4762-bb34-447abfbb44e2', lineage).
narrative_ontology:cs_interpretation_layer_present('c212c835-7497-4762-bb34-447abfbb44e2').
narrative_ontology:cs_reading_relation('c212c835-7497-4762-bb34-447abfbb44e2', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('c212c835-7497-4762-bb34-447abfbb44e2', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c212c835-7497-4762-bb34-447abfbb44e2', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c212c835-7497-4762-bb34-447abfbb44e2', jewish_sovereignty_palestine__post_zionist_reading, influences).
narrative_ontology:cs_axiom('c212c835-7497-4762-bb34-447abfbb44e2', foundational, jewish_nation_entitled_to_sovereign_expression).
narrative_ontology:cs_axiom_status(jewish_nation_entitled_to_sovereign_expression, holdable).
narrative_ontology:cs_axiom_grounding('c212c835-7497-4762-bb34-447abfbb44e2', jewish_nation_entitled_to_sovereign_expression, deontological).
narrative_ontology:cs_axiom('c212c835-7497-4762-bb34-447abfbb44e2', foundational, palestinian_equal_self_determination_required).
narrative_ontology:cs_axiom_status(palestinian_equal_self_determination_required, holdable).
narrative_ontology:cs_axiom_grounding('c212c835-7497-4762-bb34-447abfbb44e2', palestinian_equal_self_determination_required, deontological).
narrative_ontology:cs_reference_frame('c212c835-7497-4762-bb34-447abfbb44e2', partition_equal_self_determination_baseline).
narrative_ontology:cs_drift_state('c212c835-7497-4762-bb34-447abfbb44e2', post_oslo_collapse_contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c212c835-7497-4762-bb34-447abfbb44e2', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_jewish_citizenry).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, diaspora_jewish_communities).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, west_bank_palestinians).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, gaza_residents).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_refugee_diaspora).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_jewish_citizenry).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_authority).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_peoplehood_legal_recognition).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__liberal_nationalist_reading, ancestral_return_right_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elects the government that administers the sovereignty arrangement and draws from it security protection, land and housing allocation, immigration rights, Hebrew-language public institutions, and a state oriented to Jewish continuity. Pays for it through conscription, war casualties, taxation, and international isolation. Emigration is legally open, but family, military service, and linguistic ties make departure socially costly, and the community's sense of the state as insurance against persecution binds attachment beyond day-to-day benefit calculations.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_jewish_citizenry, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_jewish_citizenry, payer).

% Sets settlement policy, administers the military and civil apparatus governing the West Bank, co-administers the Gaza perimeter regime, controls permits, planning, water allocation, and movement between zones, and conducts all diplomacy over borders and final-status issues. Can reconfigure coalitions, settlement rates, and enforcement intensity at will; answers electorally to the Jewish citizenry and not at all to the populations its rules govern directly. Its negotiation activity doubles as coalition signaling, which is where the arrangement's performative layer concentrates.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Live under military administration that controls planning permission, water quotas, movement between zones, work permits, and residency rights; the overwhelming majority cannot vote for the government issuing the rules that govern their daily lives. Settlement expansion takes land and hilltop aquifers; the separation barrier and checkpoint network fragment economic life. Travel abroad or relocation is possible for a small minority with resources and foreign ties; most have no lawful path to citizenship in any state.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, west_bank_palestinians, payer,
    powerless, biographical, trapped, regional).

% Live under an air, sea, and land perimeter regime administered jointly with Egypt; entry and exit run through permit systems they do not control. The economy operates near humanitarian thresholds, with electricity, fuel, construction materials, and reconstruction all subject to external mediation. No emigration channel operates at scale; fishing limits, buffer zones, and recurring escalation cycles shape ordinary life.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, gaza_residents, payer,
    powerless, biographical, trapped, local).

% Descendants of those displaced in 1948 and 1967, holding registered refugee status and a return claim anchored in UN resolutions. They carry statelessness and lost-property costs across generations while sitting outside the negotiating table; host states and the Palestinian Authority speak for them intermittently and imperfectly. Many hold citizenship or residence elsewhere, giving them a mobility their West Bank and Gaza counterparts lack, but their central claim has no institutional seat anywhere.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_refugee_diaspora, excluded,
    moderate, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_refugee_diaspora, payer).

% Hold formal citizenship, vote, serve in the Knesset, and use state institutions, while documented gaps in land allocation, municipal funding, planning recognition, and admissibility decisions mark them as a minority inside a state constitutionally defined by another people's self-determination. Exit is legal but severs family, property, and communal ties; their political organizations operate inside the system whose national definition they contest.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_citizens_of_israel, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_citizens_of_israel, beneficiary).

% Draw insurance value and identity reinforcement from the existence of a Jewish sovereign state; philanthropy, lobbying, and advocacy flow toward it, and aliyah remains an option almost none take. Their exposure to the arrangement's costs is indirect — reputational, political, occasionally security-related — and their level of engagement can be raised or lowered at will, which distinguishes their position sharply from the resident citizenry's.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, diaspora_jewish_communities, beneficiary,
    powerful, biographical, mobile, global).

% Administers civil affairs in parts of the West Bank under the Oslo architecture, receives tax revenue cleared and remitted by Israel, and fields the security-coordination apparatus. Its jurisdiction, budget, and continued existence depend on Israeli and donor consent; it can neither deliver statehood nor dissolve itself without triggering a fiscal and security crisis. Its administrative role gives it a seat at the table while its dependence strips the seat of independent force.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_authority, agenda_setter,
    moderate, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_authority, payer).

% States, the UN system, and international courts issue resolutions, recognition decisions, and rulings on the legality of settlements and occupation practices; donors fund the Palestinian Authority and humanitarian relief in Gaza. Leverage is real but episodic — aid conditionality, court proceedings, arms-export reviews — and none of these seats bears the arrangement's daily costs, which shapes both the pace and the ceiling of their interventions.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, international_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_jewish_citizenry).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__liberal_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of protecting a historically stateless, repeatedly persecuted minority: concentrated territorial sovereignty provides defense, immigration refuge, language revival, and institutional continuity that no diaspora or minority-rights arrangement delivered. It also coordinates a shared civic infrastructure — courts, utilities, health systems — for everyone under its administration, unevenly.
% TRANSFER_FUNCTION: Moves land, water, movement freedom, and political voice asymmetrically: sovereignty, security guarantees, and development rights flow to the Jewish collective; land access, building permits, and full franchise are withheld from Palestinians under occupation; Palestinian labor and cleared tax revenue pass through Israeli control; refugee property remains unrestituted across generations.
% ABSENT_VOICES: The Palestinian refugee diaspora would object loudest and is furthest from the room — its return claim has no institutional seat in any negotiation. Future generations of both peoples are unrepresented in arrangements whose costs compound over generations. Regional neighbors absorbing spillover effects sit outside the bilateral frame. Inside the room, the seats with least power (occupied residents) attend only through intermediaries whose existence depends on the arrangement being negotiated.
% DISAPPEARANCE_RATIONALE: If the sovereignty arrangement vanished overnight, the region would reorganize violently and fundamentally: millions of people's citizenship, property, and physical security would be instantly indeterminate; armed actors would contest the vacuum; neighboring states would intervene; the diaspora's identity anchor and the refugee claim's addressee would both disappear simultaneously. Nothing about the current distribution of population, allegiance, or institution survives the removal — this is the opposite of a natural fact the world would regenerate.
% FOUNDING_PROBLEM: European antisemitism left the Jews a dispersed, stateless minority exposed to expulsion, expropriation, and ultimately genocide; Zionism was founded to solve this by concentrating the people behind defensible sovereignty in its historic homeland, converting an unprotectable diaspora into a self-governing collective.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: large-scale antisemitism survey data (EU Fundamental Rights Agency, ADL audits), Holocaust historiography, and the documented recurrence of persecution against stateless or vulnerable Jewish communities attest that the founding problem has not dissolved. Palestinian and Arab historiography acknowledges the European persecution as real while disputing that its costs should be allocated to Palestine's inhabitants — corroboration of the problem's liveness, joined to contestation of the solution's price assignment, from seats that gain nothing from the arrangement.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__liberal_nationalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__liberal_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claimed type is tangled_rope because both halves are structurally present and inseparable: a genuine coordination function (concentrated protection, immigration, language revival, and institutional continuity for a minority repeatedly exposed to expulsion and genocide — a real collective-action problem no diaspora arrangement solved) rides the same structure that imposes asymmetric costs (military rule without franchise, blockade, settlement land appropriation, refugee exclusion). Extractiveness is authored at 0.55: moderate-to-substantial, reflecting this reading's own assessment that the core is legitimate while the excess beyond co-equal self-determination (occupation permanence, settlement growth, refugee bar) is real and uncompensated. Suppression is authored at 0.70 as a raw structural property — the checkpoint, permit, planning, and administrative-detention machinery is the arrangement's active enforcement layer; per the framework, suppression is NOT scaled by power or scope, only extractiveness is scaled by directionality and scope in the engine's computation. Theater ratio 0.42: the state's civic and defensive functions are largely real, but the post-Oslo peace architecture acquired a large performative component — interim arrangements maintained as management theater while the underlying facts moved the other way. Accessibility collapse 0.45: alternatives (partition variants, confederation, binational models) remain discussable and periodically negotiated; nothing like natural-law closure obtains. Resistance 0.72: two intifadas, sustained civic mobilization, boycott movements, UN General Assembly majorities, and joint-list electoral participation — the arrangement meets continuous, organized opposition, which is itself evidence it is constructed rather than natural. The measurement series share one six-point grid (t in years since 1948: 0, 15, 30, 45, 60, 75) so every metric is authored at every examined point; the suppression series is included because enforcement capacity genuinely changed shape across the interval (military government over Arab citizens until 1966, post-1967 occupation build-out, Oslo-era partial devolution, second-intifada crackdown and barrier/checkpoint regime), not merely shifted level.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the israeli_jewish_citizenry seat the arrangement presents as hard-won coordination: the thing that ended two millennia of exposure to expulsion, and its costs (conscription, war, isolation) are paid by the same people who collect its benefits — near-symmetric from inside. From the west_bank_palestinians and gaza_residents seats the identical structure presents as rule without rights: every benefit the citizenry collects arrives as a cost they bear, with no exit and no vote. The israeli_government seat experiences neither — it experiences a management problem, a coalition-maintenance problem, and a legitimacy-management problem, which is precisely why the theater ratio concentrates in its activities (negotiation cycles, 'economic peace' initiatives, annexation debates that function as coalition signaling). The palestinian_authority seat is the sharpest divergence case: nominally an agenda-setter, structurally a captive administrator whose d should not be pulled to the beneficiary end by its administrative role. Coalition potential among the victim seats exists (Palestinian unity frameworks, joint electoral lists) but has been repeatedly fragmented — fragmentation that the enforcement architecture rewards, which the resistance series reflects.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. israeli_jewish_citizenry and diaspora_jewish_communities sit near the beneficiary end (d near 0.0) — the citizenry strongly (trapped by identity and conscription ties despite formal mobility), the diaspora weakly-but-clearly (mobile, dialable engagement, indirect costs). west_bank_palestinians and gaza_residents sit near the full-target end (d near 1.0): they bear the transfers (land, water, movement, revenue control) with trapped exit, and trapped targets amplify effective extraction. palestinian_refugee_diaspora sits high-d but attenuated by distance and mobility — bearing an intergenerational cost without daily contact with the enforcing apparatus. palestinian_citizens_of_israel straddle: formal membership pulls d down, documented allocation discrimination pushes it up. palestinian_authority is the case the automatic derivation gets wrong on role alone — its agenda_setter position would suggest beneficiary-side d, but its situation (jurisdiction, budget, and existence contingent on the very arrangement it administers) places it modestly target-side; no explicit override is filed because the situation text carries the correction and the power-atom-keyed override surface is too coarse to express a per-agent adjustment without mislabeling other moderate-power seats. No directionality_overrides are otherwise needed: the beneficiary/victim declarations plus exit options reproduce the qualitative structure without intervention.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Reading the arrangement as pure rope — 'a persecuted people solved its collective-action problem' — erases the identified victims whose costs ride the same structure; the tangled_rope gate forces naming them. Reading it as pure snare — 'extraction with a coordination cover story' — erases the fact that the coordination function is real, historically load-bearing, and corroborated from outside the beneficiary set (the founding problem it solves is independently attested); the snare gate would require the coordination story to be cover, and here it demonstrably is not. Mandatrophy status: the founding problem (stateless-minority exposure to persecution) is still live, so the arrangement has not outlived its mandate — but the Oslo interim architecture exhibits classic mandate-drift symptoms: a transitional framework whose temporary status hardened into indefinite administration, visible in the rising theater_ratio series after t=45. The mismatch consumer should read founding_problem_status=live against disappearance_verdict=world_rearranges as consistent (no zombie flag): the world genuinely depends on the arrangement, and the problem it was built for has not dissolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading (liberal_nationalist_reading) of the kernel jewish_sovereignty_palestine; would instantiating a sibling reading (settler_colonial, religious_zionist, cultural_zionist, post_zionist) change the victim set, the beneficiary structure, or epsilon enough to change the classification?',
    'Comparative analysis across the five sibling constraint files in the family: align their referents (the standing sovereignty arrangement), compare victim sets and epsilon, and test whether any pair of readings shares a stable epsilon over one referent.',
    'If the settler_colonial reading is adopted, the same arrangement computes with a larger victim set and higher epsilon (snare-leaning); if the cultural_zionist reading is adopted, the political-sovereignty component drops out and epsilon falls (rope/scaffold-leaning). The classification is indexical to the reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame uncertainty: which reading of the sovereignty kernel this arrangement instantiates.').

omega_variable(
    occupation_transitional_or_permanent,
    'Is the post-1967 control arrangement over the West Bank and Gaza a transitional regime awaiting a final-status settlement, or has it become a permanent feature of the sovereignty structure?',
    'Settlement population and built-area time series, annexation and regularization legislation, budget allocations to settlement authorities, and the absence or presence of credible final-status negotiation across successive governments.',
    'If transitional, part of the measured burden is the price of an unfinished compromise and the tangled_rope reading holds; if permanent, the effective burden on the occupied population rises sharply and the arrangement tilts toward pure extraction for that component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(occupation_transitional_or_permanent, empirical, 'Whether the occupation component is scaffold-like (transitional) or structurally permanent.').

omega_variable(
    refugee_bar_extraction_status,
    'Does the continuing bar on 1948 refugee return constitute an ongoing transfer (statelessness and lost property renewed each generation) or a settled consequence of a defensive war whose corrective claims were superseded by subsequent history?',
    'International-law scholarship on intergenerational refugee claims, comparative compensation precedents, and the position of the refugees themselves as expressed through representative bodies rather than host-state proxies.',
    'If ongoing transfer, epsilon rises materially and the refugee diaspora seat moves toward the full-target end; if settled consequence, epsilon falls and the seat reads as an excluded claimant rather than a current payer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refugee_bar_extraction_status, preference, 'Whether the refugee dimension is a live extraction channel or a closed historical account.').

omega_variable(
    homeland_grounding_basis,
    'Does the ancestral-homeland element ground the right empirically (historical continuity of presence, archaeology, documented ties) or normatively (peoplehood continuity sufficient regardless of empirical discontinuity)?',
    'Conceptual analysis of the reading''s own texts: if proponents treat empirical discontinuity (exile periods, demographic turnover) as irrelevant to the claim, the grounding is normative; if they cite continuity evidence as load-bearing, it is empirical and falsifiable.',
    'An empirical grounding exposes the right to revision by historical evidence; a normative grounding stabilizes epsilon but shifts the entire justification weight onto the equal-rights axiom, raising the cost of denying the Palestinian counterpart claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(homeland_grounding_basis, conceptual, 'Epistemic type of the ancestral-homeland premise within this reading.').

omega_variable(
    majority_identity_lock_degree,
    'To what degree is the Israeli-Jewish citizenry''s attachment to the arrangement identity-fused rather than merely interest-based — would meaningful numbers exit (emigrate, disengage) if the security and identity returns declined?',
    'Emigration (yerida) statistics correlated with security conditions, survey data on willingness to relocate, and longitudinal cohort analysis of diaspora ties among Israeli Jews.',
    'High identity lock places the primary beneficiary seat near the trapped end despite formal mobility, amplifying its computed stake in persistence; low lock makes the arrangement dependent on delivered benefits and more responsive to cost imposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majority_identity_lock_degree, empirical, 'Degree of identity fusion binding the principal beneficiary seat to the arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__liberal_nationalist_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(jewi_tr_t0, observed).
narrative_ontology:measurement(jewi_tr_t15, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement_basis(jewi_tr_t15, observed).
narrative_ontology:measurement(jewi_tr_t30, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(jewi_tr_t30, observed).
narrative_ontology:measurement(jewi_tr_t45, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 45, 0.3).
narrative_ontology:measurement_basis(jewi_tr_t45, observed).
narrative_ontology:measurement(jewi_tr_t60, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement_basis(jewi_tr_t60, observed).
narrative_ontology:measurement(jewi_tr_t75, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 75, 0.42).
narrative_ontology:measurement_basis(jewi_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(jewi_be_t0, observed).
narrative_ontology:measurement(jewi_be_t15, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 15, 0.44).
narrative_ontology:measurement_basis(jewi_be_t15, observed).
narrative_ontology:measurement(jewi_be_t30, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement_basis(jewi_be_t30, observed).
narrative_ontology:measurement(jewi_be_t45, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 45, 0.47).
narrative_ontology:measurement_basis(jewi_be_t45, observed).
narrative_ontology:measurement(jewi_be_t60, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement_basis(jewi_be_t60, observed).
narrative_ontology:measurement(jewi_be_t75, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 75, 0.55).
narrative_ontology:measurement_basis(jewi_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(jewi_su_t0, observed).
narrative_ontology:measurement(jewi_su_t15, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement_basis(jewi_su_t15, observed).
narrative_ontology:measurement(jewi_su_t30, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement_basis(jewi_su_t30, observed).
narrative_ontology:measurement(jewi_su_t45, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 45, 0.5).
narrative_ontology:measurement_basis(jewi_su_t45, observed).
narrative_ontology:measurement(jewi_su_t60, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 60, 0.66).
narrative_ontology:measurement_basis(jewi_su_t60, observed).
narrative_ontology:measurement(jewi_su_t75, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 75, 0.7).
narrative_ontology:measurement_basis(jewi_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__liberal_nationalist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'Jewish sovereignty in Palestine' conflates at least five structurally distinct claims that cannot share one epsilon. The liberal nationalist reading authors epsilon ~0.55 over the standing arrangement (legitimate core, uncompensated excess); the settler colonial reading authors a much higher epsilon over the same referent with a widened victim set (displacement regime regardless of intent); the religious zionist reading authors low epsilon but removes the Palestinian co-equality constraint entirely (inalienable claim); the cultural zionist reading shrinks the referent to cultural-institutional presence (political sovereignty optional), collapsing most of the measured extraction out of scope; the post zionist reading authors epsilon over the ethnic-national framework as an obstruction to civic equality. Upstream/downstream: the liberal reading's international-legitimacy strategy (Mandate lineage, UNGA 181, recognition) is cited by and pressures the religious reading's annexationism, and its very success in achieving statehood is the enabling condition for the post zionist critique. All five files cross-link via affects_constraints; no single file averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
