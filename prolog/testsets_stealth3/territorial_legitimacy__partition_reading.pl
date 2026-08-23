% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__partition_reading, []).

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
 *   constraint_id: territorial_legitimacy__partition_reading
 *   human_readable: Partition-Legitimacy Framework for the Former Mandate Territory (UN Resolution 181 / Recognized Borders)
 *   domain: political/international-law
 *
 * SUMMARY:
 *   The partition reading holds that territorial legitimacy in the former
 *   Mandate territory flows from international legal recognition of a
 *   division into two states: UN Resolution 181 as founding text, the
 *   recognized (Green Line) borders as its operative expression, and the
 *   recognition architecture — UN admission, bilateral treaties, ICJ
 *   engagement — as its maintenance machinery. The standing arrangement under
 *   contest, which is the referent for epsilon, is the actual territorial
 *   situation: one state controlling the whole territory, several hundred
 *   thousand settlers beyond the recognized lines, a Palestinian state
 *   recognized by a large majority of UN members but not existing in
 *   substance, and a negotiation process that has run for three decades
 *   without producing the division the framework promises. Assessed by this
 *   reading's own lights, that arrangement is substantially illegitimate: it
 *   takes land, water, and administrative control from the Palestinian
 *   population, and it erodes the anti-conquest norm the recognition
 *   architecture exists to uphold. The claimed type and the metrics are
 *   authored independently: the claim states what this reading believes is
 *   structurally true; the metrics state what is descriptively true of the
 *   arrangement's operation. KEY AGENTS (by structural relationship):
 *   israel_recognized_state (primary beneficiary and co-agenda-setter,
 *   institutional/constrained); west_bank_palestinians and
 *   east_jerusalem_palestinians (primary targets, powerless/trapped);
 *   palestinian_refugee_diaspora (residual target, powerless/trapped,
 *   generational horizon); settlement_regional_councils (secondary
 *   beneficiary, organized/constrained);
 *   palestinian_authority_institutional_class (dual-positioned
 *   beneficiary/payer, organized/constrained); un_security_council and
 *   us_peace_process_brokerage (agenda-setters, institutional/arbitrage);
 *   international_court_of_justice (analytical observer);
 *   normalization_partner_states (peripheral beneficiaries,
 *   institutional/mobile); one_state_advocates (excluded seat,
 *   moderate/constrained).
 *
 * KEY AGENTS:
 *   - israel_recognized_state: Primary beneficiary and co-agenda-setter (institutional/constrained) — collects recognition and normalization within the recognized lines while administering control beyond them
 *   - west_bank_palestinians: Primary target (powerless/trapped) — bear expropriation, movement restriction, and statelessness under the arrangement the framework condemns but does not stop
 *   - east_jerusalem_palestinians: Primary target (powerless/trapped) — bear revocable residency, planning restriction, and separation from the West Bank
 *   - palestinian_refugee_diaspora: Residual target (powerless/trapped, generational horizon) — their remedy is subordinated to the recognized-lines framework and deferred across three generations
 *   - settlement_regional_councils: Secondary beneficiary (organized/constrained) — expand under the framework's nominal condemnation, absorbing the gap between the norm and its enforcement
 *   - palestinian_authority_institutional_class: Dual-positioned beneficiary/payer (organized/constrained) — collects process rents while bearing administration without sovereignty
 *   - un_security_council: Formal agenda-setter (institutional/arbitrage) — holds the framework's enforcement authority; veto geometry converts enforcement into deferral
 *   - us_peace_process_brokerage: Operational agenda-setter with beneficiary secondary position (institutional/arbitrage) — brokers the process and collects leverage from its continuation
 *   - international_court_of_justice: Analytical observer (institutional/analytical) — has found the occupation unlawful and the settlements illegal without enforcement power
 *   - normalization_partner_states: Peripheral beneficiaries (institutional/mobile) — collect treaty and trade benefits under the framework's stability while deferring the Palestinian-state component
 *   - one_state_advocates: Excluded seat (moderate/constrained) — proponents of a single shared state on either side have no place in the sanctioned two-state conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, 0.76).
domain_priors:suppression_score(territorial_legitimacy__partition_reading, 0.78).
domain_priors:theater_ratio(territorial_legitimacy__partition_reading, 0.53).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, theater_ratio, 0.53).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__partition_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__partition_reading, "Partition-Legitimacy Framework for the Former Mandate Territory (UN Resolution 181 / Recognized Borders)").
narrative_ontology:topic_domain(territorial_legitimacy__partition_reading, "political/international-law").

domain_priors:requires_active_enforcement(territorial_legitimacy__partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__partition_reading, '6a67315d-655f-4b64-ab2d-51ddef6c4031').
narrative_ontology:cs_kernel_codification('6a67315d-655f-4b64-ab2d-51ddef6c4031', fixed_text).
narrative_ontology:cs_authority_grounding('6a67315d-655f-4b64-ab2d-51ddef6c4031', lineage).
narrative_ontology:cs_interpretation_layer_present('6a67315d-655f-4b64-ab2d-51ddef6c4031').
narrative_ontology:cs_reading_relation('6a67315d-655f-4b64-ab2d-51ddef6c4031', territorial_legitimacy__security_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('6a67315d-655f-4b64-ab2d-51ddef6c4031', territorial_legitimacy__indigenous_continuity_reading, forecloses).
narrative_ontology:cs_axiom('6a67315d-655f-4b64-ab2d-51ddef6c4031', foundational, recognition_constitutes_territorial_legitimacy).
narrative_ontology:cs_axiom_status(recognition_constitutes_territorial_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('6a67315d-655f-4b64-ab2d-51ddef6c4031', recognition_constitutes_territorial_legitimacy, conventional).
narrative_ontology:cs_axiom('6a67315d-655f-4b64-ab2d-51ddef6c4031', secondary, partition_realizes_mutual_self_determination).
narrative_ontology:cs_axiom_status(partition_realizes_mutual_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('6a67315d-655f-4b64-ab2d-51ddef6c4031', partition_realizes_mutual_self_determination, instrumental).
narrative_ontology:cs_reference_frame('6a67315d-655f-4b64-ab2d-51ddef6c4031', un181_recognized_two_state_baseline).
narrative_ontology:cs_drift_state('6a67315d-655f-4b64-ab2d-51ddef6c4031', contemporary_post_icj_2024, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('6a67315d-655f-4b64-ab2d-51ddef6c4031', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__partition_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, israel_recognized_state).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, palestinian_authority_institutional_class).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, settlement_regional_councils).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, west_bank_palestinians).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, east_jerusalem_palestinians).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, palestinian_refugee_diaspora).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, us_peace_process_brokerage).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, normalization_partner_states).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, palestinian_authority_institutional_class).
narrative_ontology:constraint_vindicates(territorial_legitimacy__partition_reading, recognition_constitutes_title_doctrine).
narrative_ontology:constraint_vindicates(territorial_legitimacy__partition_reading, ex_injuria_non_oritur_doctrine).
narrative_ontology:constraint_vindicates(territorial_legitimacy__partition_reading, un_charter_article_2_4_territorial_integrity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The state admitted to the UN within the recognized lines and in control of the territory beyond them. It collects recognition, treaties, security cooperation, and normalization for the state inside the lines while administering military government, settlement planning, and permit regimes beyond them. Its position in the framework is double: the framework legitimates the state and simultaneously condemns what the state builds across the lines. Exit would cost either the recognition or the territory, so it operates inside the framework while expanding the facts the framework condemns.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, israel_recognized_state, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, israel_recognized_state, agenda_setter).

% The administrative stratum created by the interim agreements: ministries, security services, and payroll networks funded substantially by foreign donors channeled through the framework. It collects salaries, aid flows, international standing, and a governing role without sovereignty, while bearing the costs of administering a population whose territory it does not control and whose principal grievance the process has not answered. Its institutional survival is tied to the process continuing; its constituency's patience is not.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, palestinian_authority_institutional_class, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, palestinian_authority_institutional_class, payer).

% The elected administrative bodies of the communities beyond the recognized lines. They receive planning authority, infrastructure, budgets, and security through the state apparatus, and have expanded continuously across the interval under a framework that formally declares their communities illegitimate. Their position depends on the gap between the framework's findings and their enforcement; a framework that enforced its own findings would require their relocation, as happened in Sinai in 1982 and Gaza in 2005.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, settlement_regional_councils, beneficiary,
    organized, biographical, constrained, regional).

% The roughly three million residents of the territory beyond the recognized lines, governed by military administration across most of the land area. They lose land and water to expropriation and settlement expansion, move through permit regimes and checkpoints, and hold residency rather than citizenship in the state that controls their lives. The framework promises them a state that has not arrived in three decades of process; leaving is blocked by borders, poverty, and neighboring states' refusal to absorb them.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, west_bank_palestinians, payer,
    powerless, biographical, trapped, regional).

% The Palestinian residents of the annexed sector of the city, holding permanent-resident status that can be revoked rather than citizenship. They pay municipal taxes, face planning regimes that restrict building, and are separated from the West Bank by the barrier's routing. The framework assigns their city to the Palestinian state that has not been established; their day-to-day status is administered by the state the framework recognizes.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, east_jerusalem_palestinians, payer,
    powerless, biographical, trapped, regional).

% The descendants of the 1948 displacement, several million people in Jordan, Lebanon, Syria, and beyond, many still stateless or holding precarious residency. The framework prices their claim as subordinate to the two-state division: their remedy is deferred to a future state most have never seen, while the recognized-lines principle treats their return as off the table. They bear the cost of a framework whose founding act produced their dispossession and whose remedy has been pending for three generations.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, palestinian_refugee_diaspora, payer,
    powerless, generational, trapped, global).

% The body holding the framework's formal enforcement authority: it adopted the resolutions defining the framework (242, 338, 2334) and can bind member states, but any permanent member can veto enforcement. The veto geometry converts enforcement findings into deferral; the body's agenda-setting is real, its enforcement is structurally hostage to five concurrences.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, un_security_council, agenda_setter,
    institutional, generational, arbitrage, global).

% The broker role held continuously by the United States across the interval: it convenes negotiations, funds the Palestinian institutions, arms and subsidizes the Israeli state, and shields the arrangement from Council enforcement through its veto. It collects diplomatic leverage, regional relationships, and domestic political returns from the broker position; its brokerage is the process's operational engine and its veto is the process's enforcement ceiling.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, us_peace_process_brokerage, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, us_peace_process_brokerage, beneficiary).

% The principal judicial organ of the UN. It found the separation barrier unlawful (2004) and the occupation unlawful and the settlements illegal (2024 advisory opinion), engaging the framework's own legal texts. It has no enforcement arm; its findings are addressed to the organs that do.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, international_court_of_justice, observer,
    institutional, generational, analytical, global).

% States — Egypt and Jordan by treaty, the Abraham Accords signatories more recently — that collect peace, trade, security, and technology benefits under the framework's stability. Their treaties recognize the state within its lines while deferring the Palestinian-state component; they can expand or freeze normalization at will, which gives them exit mobility the territorial parties lack.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, normalization_partner_states, beneficiary,
    institutional, biographical, mobile, regional).

% Israeli and Palestinian proponents of a single shared state with equal rights for all inhabitants. Their proposals are outside the sanctioned conversation: the framework recognizes only the two-state division as the legitimate outcome, so their organizing runs through civil society, academic debate, and marginal party lists with no seat in the process they propose to replace.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, one_state_advocates, excluded,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__partition_reading, israel_recognized_state).
narrative_ontology:fixing_cost_class(territorial_legitimacy__partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The framework coordinates mutual recognition between two national movements and gives third states a stable basis for relations with both: it defines which territorial control counts as legitimate (within the recognized lines), underwrites treaties and regional integration on that basis, and anchors the general norm that borders are not changed by force. It also coordinates the parties' own dealings by providing a single sanctioned outcome — two states — around which negotiations, aid, and diplomacy are organized.
% TRANSFER_FUNCTION: Moves land, water, and administrative control from the Palestinian population to the state and settlement apparatus as facts on the ground change during the process; moves diplomatic legitimacy and normalization to the recognized state; moves aid, salaries, and standing to the Palestinian institutional class; and moves brokerage leverage to the mediating power. Palestinian statehood is the consideration perpetually deferred.
% ABSENT_VOICES: The refugee diaspora would object that a framework founded on the act that displaced them prices their remedy out from the start; they are represented only indirectly, through an institutional class whose mandate excludes them. One-state advocates on both sides are outside the sanctioned conversation entirely. Within the process, the seats that speak are the broker, the recognized state, and the institutional class — the population bearing the arrangement's daily costs speaks through no seat of its own.
% DISAPPEARANCE_RATIONALE: If the partition-legitimacy framework vanished overnight, the recognition architecture collapses: the treaties and normalizations anchored in it lose their legal frame, both national movements' claims revert to contests of force and habitation with no shared arbiter, the Palestinian institutional class loses its funding and standing, and the anti-conquest norm loses its most-litigated application. The arrangements of a dozen states and several international organs are organized around this framework; they would not stay put.
% FOUNDING_PROBLEM: Terminate the British Mandate over a territory containing two irreconcilable national movements without an open war between them: the 1947 solution on paper was partition into two states — an Arab state and a Jewish state — with an international regime for Jerusalem and an economic union, adopted by the General Assembly as Resolution 181.
% FOUNDING_PROBLEM_CORROBORATION: Attested outside the benefiting parties by the UNSCOP majority report and the contemporaneous diplomatic record — the Arab League's rejection, the Great Power correspondence, the Ad Hoc Committee proceedings — and by international legal scholarship engaging the territory's legal regime, including the ICJ's advisory proceedings. The party institutions do not attest it neutrally: each retells the founding to fit its claim, and the framework's own beneficiaries have the strongest stake in the founding story.
narrative_ontology:disappearance_verdict(territorial_legitimacy__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__partition_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__partition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.76 at interval end) because the standing arrangement transfers land, water, and administrative control from the Palestinian population to the state and settlement apparatus at a rate that has not slowed across the interval, while the framework that condemns the transfer supplies the process structure that absorbs the cost of stopping it. Suppression is high (0.78) because the arrangement's persistence depends on active machinery — permit regimes, the separation barrier, administrative detention, settlement enforcement, and measures against boycott movements in third countries — not on participant preference. Theater is moderate and rising (0.53 at interval end): negotiation rounds, conference diplomacy, and recognition ceremonies perform the framework while the territorial facts move away from it, though the ICJ's opinions and the recognition architecture retain real function. Accessibility collapse is moderate-low (0.45): the sibling legitimacy readings remain live and one-state proposals circulate; accepting the partition principle does not close the alternative frameworks, it only prices them out of the sanctioned process. Resistance is high (0.70): the arrangement meets sustained Palestinian resistance, boycott movements, UN majorities, and advisory-opinion litigation. All three measurement series run on one shared nine-point grid (0-32, mapped to 1993-2025: Oslo Accords to present), so every tracked metric is authored at every examined time point; the enforcement-machinery series is included because the story specifically traces the hardening of enforcement capacity (barrier construction, permit-regime expansion, extraterritorial anti-boycott measures, wartime mass detention), not merely a static suppression level.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats should compute differently. From the recognized state's seat, the framework is the structure that legitimates the state's existence within recognized lines — genuine, valuable, worth defending; the territory beyond the lines is an administrable problem. From the West Bank Palestinian seat, the same framework is the process that has consumed a generation while the land available for the promised state shrinks — the framework's promises are experienced as the mechanism of their non-delivery. The Palestinian institutional class experiences both at once: it collects the process rents and pays the sovereignty costs, which is why its seat should classify differently from both the state's and the population's. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   israel_recognized_state is declared beneficiary (recognition, normalization, security cooperation) and agenda-setter (administers the arrangement), with constrained exit — it cannot abandon the framework without surrendering either recognition or territory — placing its directionality near the beneficiary end but not at it. settlement_regional_councils are beneficiaries of the gap between the framework's findings and their enforcement. The Palestinian institutional class is dual-positioned — beneficiary (aid, salaries, standing) with payer secondary (administration without sovereignty) — placing it near symmetric. west_bank_palestinians, east_jerusalem_palestinians, and palestinian_refugee_diaspora are the targets: they pay in land, water, movement, and statelessness, with trapped exit, placing them near the full-target end. The Council and the brokerage hold enforcement authority but collect no territorial gain; their directionality is governed by arbitrage exit (veto, brokerage leverage) rather than by benefit or payment. The ICJ holds the analytical seat. Scope amplification applies: the framework operates at global scale (Council, Court, recognition architecture), which raises verification costs and amplifies effective extraction on the trapped targets. No directionality overrides are authored: the beneficiary/victim declarations plus exit options differentiate the seats adequately, and the override mechanism keys on power atoms, which cannot separate the several institutional seats here.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification is what prevents both mislabelings. Reading the framework as pure coordination would absorb the Palestinian population's land, water, and time losses as coordination overhead — the anti-conquest norm and mutual recognition are real goods, but they are delivered to some seats through a structure that takes from others. Reading it as pure extraction would erase the genuine function: the recognition architecture does stabilize borders, does underwrite treaties (Egypt, Jordan, the Abraham Accords), and does give both national movements a legal vehicle for statehood that neither sibling reading provides. The R5 interview locates the genealogy: the founding problem (two irreconcilable national movements, one territory, Mandate terminating) was real and is attested by the UNSCOP record and contemporaneous diplomacy outside the benefiting parties; its status is contested rather than dead, because the problem the framework was built to solve has never been solved — the framework's mandate has not so much outlived its function as gone undischarged for three decades. That undischarged-mandate structure, with rising theater and rising extraction, is the signature the classification should register; if the two-state solution is foreclosed (see omega two_state_foreclosure_status), the profile migrates toward vestige and the founding-problem status shifts from contested to dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_location,
    'This story instantiates the partition_reading of the territorial_legitimacy kernel. The three readings locate legitimacy in different structural elements — international recognition of partition lines (this reading), defensive necessity (security_necessity_reading), continuous habitation and anti-colonial self-determination (indigenous_continuity_reading). Which legitimacy source does the classification machinery actually register, and where exactly do the readings'' victim and beneficiary sets diverge?',
    'Generate the sibling stories and compare their beneficiary/victim structures and epsilon values against this one. The disagreement is located in the status of the founding act (1947-48): this reading treats it as the legitimate source of title, the indigenous reading treats it as the injury, the security reading treats it as strategically settled.',
    'Under the indigenous reading''s structure the victim set expands to the 1948 refugees as primary rather than residual claimants and epsilon rises; under the security reading''s structure the victim set contracts to present-day occupation costs and epsilon falls. This story''s classification holds only under the partition reading''s structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Which legitimacy source the classification registers, and how sibling readings would restructure victims and epsilon.').

omega_variable(
    enforcement_deferral_mechanism,
    'Is the framework''s decades-long non-enforcement of its own findings (settlement illegitimacy, occupation unlawfulness) structural incapacity — P5 veto geometry no majority can overcome — or strategic choice by seats that prefer deferral?',
    'Counterfactual comparison of enforcement episodes: outcomes where enforcement ran without veto cover (General Assembly resolutions, ICJ advisory opinions) against episodes where a permanent member''s position shifted. If outcomes track veto geometry rather than seat preference, incapacity dominates.',
    'If incapacity, the framework''s losses are a coordination failure and the theater ratio measures honest impotence; if choice, the deferral is the transfer channel itself and the framework''s coordination function is substantially cover for it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_deferral_mechanism, empirical, 'Whether non-enforcement reflects incapacity or preference.').

omega_variable(
    two_state_foreclosure_status,
    'Does the standing arrangement leave the two-state solution structurally possible — the partition reading''s expected delta — or has settlement expansion and annexation practice foreclosed it, leaving the framework a vestige maintained by ritual?',
    'Territorial contiguity analysis: assess whether a sovereign Palestinian state on the 1967 lines with equivalent land swaps remains physically and institutionally constructible — settlement footprint, Area C control, infrastructure integration — using the demonstrated reversibility of withdrawal (Sinai 1982, Gaza 2005) as positive controls.',
    'If still possible, the framework remains a live coordination structure with rising extraction; if foreclosed, its function is dead and its persistence is performative maintenance — the classification migrates toward a vestige profile and the founding-problem status shifts from contested to dead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(two_state_foreclosure_status, empirical, 'Whether the reading''s expected structural delta remains achievable.').

omega_variable(
    recognition_without_realization,
    'Does the expanding diplomatic recognition of Palestine (140+ member states, the 2024-2025 recognition wave) function as progress toward the partition baseline, or as a substitute that relieves pressure for the territorial change the baseline requires?',
    'Compare settlement growth and Area C expropriation rates before and after recognition waves; if recognition waves coincide with accelerated facts-on-the-ground change, recognition functions as pressure-release rather than progress.',
    'If substitute, the recognition architecture inflates the theater ratio and the framework''s coordination function is weaker than its text suggests; if progress, theater is over-measured and the framework retains more function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recognition_without_realization, empirical, 'Whether recognition operates as progress toward the baseline or as a pressure-release substitute for it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__partition_reading, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy__partition_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(terr_tr_t0, observed).
narrative_ontology:measurement(terr_tr_t4, territorial_legitimacy__partition_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement_basis(terr_tr_t4, observed).
narrative_ontology:measurement(terr_tr_t8, territorial_legitimacy__partition_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement_basis(terr_tr_t8, observed).
narrative_ontology:measurement(terr_tr_t12, territorial_legitimacy__partition_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement_basis(terr_tr_t12, observed).
narrative_ontology:measurement(terr_tr_t16, territorial_legitimacy__partition_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement_basis(terr_tr_t16, observed).
narrative_ontology:measurement(terr_tr_t20, territorial_legitimacy__partition_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(terr_tr_t20, observed).
narrative_ontology:measurement(terr_tr_t24, territorial_legitimacy__partition_reading, theater_ratio, 24, 0.46).
narrative_ontology:measurement_basis(terr_tr_t24, observed).
narrative_ontology:measurement(terr_tr_t28, territorial_legitimacy__partition_reading, theater_ratio, 28, 0.5).
narrative_ontology:measurement_basis(terr_tr_t28, observed).
narrative_ontology:measurement(terr_tr_t32, territorial_legitimacy__partition_reading, theater_ratio, 32, 0.53).
narrative_ontology:measurement_basis(terr_tr_t32, observed).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy__partition_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(terr_be_t0, observed).
narrative_ontology:measurement(terr_be_t4, territorial_legitimacy__partition_reading, base_extractiveness, 4, 0.58).
narrative_ontology:measurement_basis(terr_be_t4, observed).
narrative_ontology:measurement(terr_be_t8, territorial_legitimacy__partition_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement_basis(terr_be_t8, observed).
narrative_ontology:measurement(terr_be_t12, territorial_legitimacy__partition_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement_basis(terr_be_t12, observed).
narrative_ontology:measurement(terr_be_t16, territorial_legitimacy__partition_reading, base_extractiveness, 16, 0.68).
narrative_ontology:measurement_basis(terr_be_t16, observed).
narrative_ontology:measurement(terr_be_t20, territorial_legitimacy__partition_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement_basis(terr_be_t20, observed).
narrative_ontology:measurement(terr_be_t24, territorial_legitimacy__partition_reading, base_extractiveness, 24, 0.72).
narrative_ontology:measurement_basis(terr_be_t24, observed).
narrative_ontology:measurement(terr_be_t28, territorial_legitimacy__partition_reading, base_extractiveness, 28, 0.74).
narrative_ontology:measurement_basis(terr_be_t28, observed).
narrative_ontology:measurement(terr_be_t32, territorial_legitimacy__partition_reading, base_extractiveness, 32, 0.76).
narrative_ontology:measurement_basis(terr_be_t32, observed).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy__partition_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(terr_su_t0, observed).
narrative_ontology:measurement(terr_su_t4, territorial_legitimacy__partition_reading, suppression_requirement, 4, 0.57).
narrative_ontology:measurement_basis(terr_su_t4, observed).
narrative_ontology:measurement(terr_su_t8, territorial_legitimacy__partition_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(terr_su_t8, observed).
narrative_ontology:measurement(terr_su_t12, territorial_legitimacy__partition_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement_basis(terr_su_t12, observed).
narrative_ontology:measurement(terr_su_t16, territorial_legitimacy__partition_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement_basis(terr_su_t16, observed).
narrative_ontology:measurement(terr_su_t20, territorial_legitimacy__partition_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(terr_su_t20, observed).
narrative_ontology:measurement(terr_su_t24, territorial_legitimacy__partition_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement_basis(terr_su_t24, observed).
narrative_ontology:measurement(terr_su_t28, territorial_legitimacy__partition_reading, suppression_requirement, 28, 0.74).
narrative_ontology:measurement_basis(terr_su_t28, observed).
narrative_ontology:measurement(terr_su_t32, territorial_legitimacy__partition_reading, suppression_requirement, 32, 0.78).
narrative_ontology:measurement_basis(terr_su_t32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__partition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__security_necessity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__indigenous_continuity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'legitimacy of Israel/Palestine' conflates three structurally distinct claims about the source of territorial title. This story authors only the partition reading: legitimacy constituted by international recognition of the 1947 division and its recognized-border expression. The security_necessity_reading (control legitimated by defensive necessity) and the indigenous_continuity_reading (title grounded in continuous habitation and anti-colonial self-determination, 1948 as injury) are separate constraints with their own epsilon, victim sets, and classification; each links back here. The partition reading is upstream in one direction — its recognition architecture is the legal vehicle both siblings must engage — and contested in the other, since the indigenous reading denies the legitimacy of the founding act this reading rests on.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
