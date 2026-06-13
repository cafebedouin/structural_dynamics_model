% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__advisory_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__advisory_coordination_reading, []).

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
 *   constraint_id: wto_dsb_authority__advisory_coordination_reading
 *   human_readable: WTO DSB Expert Advisory Opinion Coordination (Advisory Sovereignty Reading)
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   The WTO Dispute Settlement Body (DSB) operates as the primary
 *   institutional mechanism for resolving trade disputes between member
 *   states. This constraint story instantiates the
 *   ADVISORY_COORDINATION_READING of the contested kernel
 *   'wto_dsb_authority'—the reading that emphasizes the DSB's role as a
 *   provider of expert legal analysis to facilitate negotiated settlements
 *   while preserving member state sovereignty. Under this reading, DSB panels
 *   issue reports with conclusions and recommendations, but these are legally
 *   nonbinding; member states retain ultimate discretion to accept, reject,
 *   or selectively implement panel recommendations. Settlements emerge from
 *   bilateral negotiations informed by (but not determined by) panel
 *   expertise. The reading frames the DSB as coordination infrastructure—it
 *   solves the mutual problem of how disputing states can resolve claims
 *   without unilateral action—rather than as binding judicial authority. This
 *   reading is contested by two sibling readings: the binding_referee_reading
 *   (which asserts that panels issue binding interpretations backed by treaty
 *   law and compliance obligations) and the judicial_activism_reading (which
 *   claims panels exceed their mandate through interpretive drift). The
 *   measurement the corpus exists to take is the classification divergence
 *   across these readings: one kernel, three structurally distinct
 *   constraints with different types, different victim/beneficiary
 *   structures, and different directionality profiles. This story generates
 *   ONLY the advisory_coordination_reading; the siblings are separate
 *   constraint files linked by network.affects_constraints.
 *
 * KEY AGENTS:
 *   - WTO DSB Panels (institutional beneficiary): provide expert analysis and procedural legitimacy, maintain influence through technical competence rather than enforcement authority, their institutional survival depends on member state acceptance of the advisory-role framing
 *   - Complainant States (beneficiary + secondary payer): initiate disputes, receive expert analysis supporting their position, retain full discretion over settlement, bear litigation costs and reputational risk
 *   - Respondent States (payer + secondary beneficiary): defend allegations, receive expert analysis of complainant's case, retain full discretion to reject recommendations, pay litigation costs and face reputational exposure
 *   - Developed Economies (institutional beneficiary): possess superior legal resources and negotiating leverage, benefit from advisory framing that permits them to cite favorable analyses while rejecting unfavorable ones based on bilateral power, maintain policy discretion over core interests
 *   - Developing Economies (moderate-power payer): have weaker bilateral leverage and smaller legal budgets, face expert criticism but panel's nonbinding status means developed states can ignore unfavorable reports, asymmetric cost of negotiating under conditions where counterparties retain full discretion while possessing superior leverage
 *   - Excluded Non-Party Interests (excluded): non-party states, workers, environmental groups, consumers affected by disputes but without standing, would argue that binding authority with transparency is preferable to advisory status enabling states to ignore expert analysis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__advisory_coordination_reading, 0.32).
domain_priors:suppression_score(wto_dsb_authority__advisory_coordination_reading, 0.18).
domain_priors:theater_ratio(wto_dsb_authority__advisory_coordination_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__advisory_coordination_reading, rope).
narrative_ontology:human_readable(wto_dsb_authority__advisory_coordination_reading, "WTO DSB Expert Advisory Opinion Coordination (Advisory Sovereignty Reading)").
narrative_ontology:topic_domain(wto_dsb_authority__advisory_coordination_reading, "international_law/trade_governance/institutional_legitimacy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__advisory_coordination_reading, 'c7ae4b8e-6c4e-488f-884c-db871addb8c7').
narrative_ontology:cs_kernel_codification('c7ae4b8e-6c4e-488f-884c-db871addb8c7', fixed_text).
narrative_ontology:cs_authority_grounding('c7ae4b8e-6c4e-488f-884c-db871addb8c7', distributed).
narrative_ontology:cs_reading_relation('c7ae4b8e-6c4e-488f-884c-db871addb8c7', wto_dsb_authority__binding_referee_reading, coexists_with).
narrative_ontology:cs_reading_relation('c7ae4b8e-6c4e-488f-884c-db871addb8c7', wto_dsb_authority__judicial_activism_reading, coexists_with).
narrative_ontology:cs_axiom('c7ae4b8e-6c4e-488f-884c-db871addb8c7', foundational, state_sovereignty_discretion_preserved).
narrative_ontology:cs_axiom_status(state_sovereignty_discretion_preserved, holdable).
narrative_ontology:cs_axiom_grounding('c7ae4b8e-6c4e-488f-884c-db871addb8c7', state_sovereignty_discretion_preserved, deontological).
narrative_ontology:cs_axiom('c7ae4b8e-6c4e-488f-884c-db871addb8c7', foundational, expert_analysis_coordination_function).
narrative_ontology:cs_axiom_status(expert_analysis_coordination_function, holdable).
narrative_ontology:cs_axiom_grounding('c7ae4b8e-6c4e-488f-884c-db871addb8c7', expert_analysis_coordination_function, conventional).
narrative_ontology:cs_reference_frame('c7ae4b8e-6c4e-488f-884c-db871addb8c7', state_discretion_primacy).
narrative_ontology:cs_drift_state('c7ae4b8e-6c4e-488f-884c-db871addb8c7', contemporary_practice_accountability_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c7ae4b8e-6c4e-488f-884c-db871addb8c7', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__advisory_coordination_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, member_states_collectively).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, dispute_settlement_predictability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, complainant_states).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, respondent_states).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, developed_economies).
narrative_ontology:constraint_victim(wto_dsb_authority__advisory_coordination_reading, complainant_states).
narrative_ontology:constraint_victim(wto_dsb_authority__advisory_coordination_reading, respondent_states).
narrative_ontology:constraint_victim(wto_dsb_authority__advisory_coordination_reading, developing_economies).
narrative_ontology:constraint_vindicates(wto_dsb_authority__advisory_coordination_reading, state_sovereignty_preservation).
narrative_ontology:constraint_vindicates(wto_dsb_authority__advisory_coordination_reading, negotiation_facilitation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the dispute resolution process, issue panel reports with legal analysis and recommendations, maintain the procedural framework. They do not enforce compliance; instead, they frame recommendations as expert guidance to facilitate state negotiation. Their authority derives from procedural legitimacy and technical competence rather than institutional power to compel.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, wto_dsb_panels, agenda_setter,
    institutional, generational, analytical, universal).

% Created the DSB through treaty negotiation and retain collective authority to amend its mandate. They benefit from the dispute resolution infrastructure because it provides a procedural alternative to unilateral action and bilateral power contests. They also retain the option to exit or reform the system if it ceases to serve their interests.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, member_states_collectively, beneficiary,
    organized, generational, mobile, universal).

% Initiate disputes when they believe another member has violated trade obligations. They benefit from expert legal analysis supporting their claims and from the procedural legitimacy that DSB involvement provides to negotiations. They also bear litigation costs and face the risk that panels reject their arguments.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, complainant_states, beneficiary,
    organized, generational, mobile, universal).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__advisory_coordination_reading, complainant_states, payer).

% Defend against dispute allegations and have their policies subject to expert scrutiny in panel reports. They bear litigation costs and reputational costs if reports criticize their policies, but they retain discretion to reject panel recommendations and rely on their own legal arguments or negotiating leverage. They benefit from the procedural legitimacy that permits them to be heard and represented.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, respondent_states, payer,
    organized, generational, mobile, universal).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__advisory_coordination_reading, respondent_states, beneficiary).

% Possess superior legal resources to litigate cases effectively, superior economic leverage to negotiate favorable settlements, and the political influence to frame DSB decisions in their favor when they choose to comply and minimize losses when they choose to reject. They benefit from the advisory framing because it permits them to treat DSB recommendations as guidance they can accept or reject based on their bilateral interests.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, developed_economies, beneficiary,
    institutional, generational, arbitrage, universal).

% Have fewer legal and economic resources than developed states and face weaker negotiating leverage in bilateral settlements. When they are respondents, they face expert criticism and the pressure to settle on terms developed complainants prefer. When they are complainants, they often struggle to enforce settlements because developed respondents can selectively comply. They pay higher proportional costs for litigation and settlement.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, developing_economies, payer,
    moderate, generational, constrained, universal).

% Administers the DSB process, provides legal support to panels, maintains records, and facilitates negotiation. They observe the constraint as institutional infrastructure and have no enforcement authority. Their role is to enable member states to use the DSB for their disputes.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, dispute_settlement_secretariat, observer,
    institutional, generational, analytical, universal).

% Non-party states, workers, environmental groups, and consumers affected by trade disputes and their settlements but without formal standing in the DSB process. They are excluded from negotiations and have no input into which disputes are brought or how they are resolved. They experience the constraint's effects but cannot shape its operation.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, non_party_stakeholders, excluded,
    powerless, biographical, trapped, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_dsb_authority__advisory_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(wto_dsb_authority__advisory_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The DSB solves the mutual problem of how disputing member states can resolve trade claims through procedure and expert analysis rather than through unilateral action, retaliation, or bilateral power contests. Panels provide shared reference point—authoritative legal analysis of treaty obligations and precedent—that enables parties to negotiate from a common factual and legal baseline instead of from conflicting interpretations. This coordination function reduces uncertainty, lowers transaction costs of negotiation, and provides face-saving mechanism for states to change policy without appearing to capitulate to threats.
% TRANSFER_FUNCTION: Moves the authority to interpret treaty obligations from unilateral state action to shared institutional process, but retains ultimate authority to accept or reject institutional recommendations with the member states themselves. States provide legal arguments; panels analyze and recommend; states decide whether to settle and on what terms. The transfer is of analytical labor and procedural legitimacy, not of binding decision authority. Power flows from unilateral interpretation to negotiated understanding informed by expert guidance, but settlement outcomes remain determined by bilateral negotiating leverage rather than institutional authority.
% ABSENT_VOICES: Non-party states whose trade policies are discussed in dispute reports but who have no standing to participate in the DSB process or in settlement negotiations. Workers, environmental advocates, human rights groups, and consumers affected by trade settlements but without representation in WTO negotiations. They would object that the advisory framing leaves them entirely dependent on member state goodwill, and that binding authority grounded in transparent procedure would give them stronger voice in outcomes affecting their interests.
% DISAPPEARANCE_RATIONALE: If the DSB disappeared, member states would lose the institutional infrastructure for coordinated dispute resolution. Disputes would revert to unilateral interpretation, bilateral negotiation without procedural framework, and trade retaliation as the dispute-resolution mechanism. Settlement patterns would shift toward power-based outcomes with less input from expert legal analysis. The consequence would be a more fragmented trade system with higher frequency of escalated conflicts and less predictability in dispute outcomes. Some disputes that are now resolved through DSB negotiation would instead trigger trade wars or remain unresolved.
% FOUNDING_PROBLEM: After the Uruguay Round, the WTO membership had grown and trade disputes multiplied. Member states needed a dispute resolution mechanism that could handle large caseloads fairly, permit smaller states to assert their legal claims against larger trading partners without being overwhelmed by power disparity, and do so without appearing to override state sovereignty. The founding problem was: how can states resolve disputes through institutional process while preserving their ultimate authority over trade policy?
% FOUNDING_PROBLEM_CORROBORATION: The original WTO Agreement Establishing the DSU (Dispute Settlement Understanding) attests that the problem was real—member states wrote detailed procedural rules to enable dispute resolution without ceding policy discretion. Developed states and treaty scholars generally attest that the founding problem remains live: they argue the DSB continues to provide valuable coordination and that advisory status preserves the legitimacy of the system. Developing states and institutional-reform advocates increasingly argue the founding problem has been superseded: they contend that panel expertise has proven valuable enough to warrant binding authority, but states' selective compliance undermines predictability. The UN and regional organizations cite WTO disputes as reference points but do not independently corroborate the founding problem—all attestation comes from member states themselves, whose interest in portraying the founding problem as ongoing serves to legitimize their preferred reading of the constraint.
narrative_ontology:disappearance_verdict(wto_dsb_authority__advisory_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__advisory_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__advisory_coordination_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(wto_dsb_authority__advisory_coordination_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__advisory_coordination_reading_tests).
:- end_tests(wto_dsb_authority__advisory_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is CLAIMED as rope (coordination without asymmetric extraction) because the advisory reading emphasizes the DSB's role as neutral expert provider and member state sovereignty preservation. Under this reading, no party is structurally trapped, and the system solves a genuine collective-action problem: disputes resolved through mutual reference to expert analysis rather than unilateral retaliation. The authored metrics, however, show modest but real extractiveness (0.32) and theater ratio (0.41), which reflect the asymmetry that arises in practice even under the advisory framing. The extractiveness is low because the DSB has no formal enforcement mechanism and states genuinely retain discretion—but it is nonzero because developing states face higher reputational and economic costs for rejecting panel recommendations than developed states do. The theater ratio (0.41) reflects the performative effort states invest in maintaining the 'advisory' framing while treating reports as de facto binding guidance in many cases; the ratio indicates that institutional legitimacy and procedure account for a substantial share of the constraint's operation, not just functional coordination. Suppression is low (0.18) because the constraint operates through persuasion and expert authority rather than coercive enforcement—there is no WTO police force, no trade war imposed by the institution itself, no member state expelled for noncompliance. The measurement series (interval 0–30) show the constraint as stable over time: extractiveness rises modestly (0.28 to 0.32) as the DSB develops more precedent and states learn to navigate the system, theater ratio remains steady (~0.40) as the balance between procedural legitimacy and practical influence persists, and suppression stays flat (~0.18) as enforcement mechanisms never materialize. This profile is consistent with a constraint that genuinely coordinates without asymmetric extraction—a rope. The claim and metrics are aligned; the committer frame routes the contestation to omegas and cs_structure rather than collapsing the reading into a single classification.
 *
 * PERSPECTIVAL GAP:
 *   The advisory_coordination_reading produces different situational understandings at different power seats. From the developed-state perspective: the DSB is coordination infrastructure that enables negotiated settlement while preserving their policy discretion—they can cite favorable panel reports in negotiations and ignore unfavorable ones because their economic leverage permits selective compliance. They experience the constraint as genuine rope: coordination without extraction. From the developing-state perspective: the same DSB structure operates as soft enforcement of developed-state preferences—their limited bilateral leverage means they cannot ignore unfavorable reports without reputational cost, and the panel's nonbinding status means developed states have no obligation to reciprocate when reports favor developing-state complainants. They experience something closer to snare or tangled rope: asymmetric extraction disguised as neutral coordination. The engine computes per-seat types from the structural data (beneficiary/victim declarations, power atoms, exit options, spatial scope); this perspective gap should produce rope for institutional/powerful seats and tangled rope or snare for moderate/powerless seats. The committer structure routes this divergence to the omegas (especially 'developing_state_power_asymmetry_under_advisory_framing') rather than hiding it in a single claimed type.
 *
 * DIRECTIONALITY LOGIC:
 *   Under the advisory reading, the directionality derivation proceeds as follows. Member states collectively are beneficiaries (they collectively solved a coordination problem—disputes resolved through procedure rather than unilateral action). Developed states specifically are beneficiaries at the institutional level: they possess the legal resources to navigate the DSB effectively, their bilateral leverage means they can ignore unfavorable reports, and they benefit from the legitimacy the advisory framing provides (it permits them to frame selective compliance as respect for sovereignty rather than as exercise of power). Developing states are secondary payers: they bear litigation costs (proportionally higher relative to GDP), they face reputational costs for rejecting unfavorable reports (social pressure to comply with expert analysis), and their limited bilateral leverage constrains them to accept settlements that developed states can unilaterally reject. Complainants and respondents are symmetric in the structural sense—both have incentive to use the DSB for their disputes—but asymmetric in power: a developed-state complainant against a developing-state respondent asymmetrically favors the complainant because the developed state can afford prolonged litigation, can reject unfavorable settlements, and can retaliate economically if the developing state defects. The directionality override should NOT be necessary because the beneficiary/victim + power + exit derivation captures the structure: developed-state beneficiaries → low d; developing-state payers with constrained exit → high d; the system is symmetric at the institutional level but asymmetric at the power level. If the derivation produces oversimplified d values (all powerful states d=0.2, all moderate states d=0.8), overrides could refine (developing-state complainant in high-leverage dispute should get d lower than developing-state respondent in asymmetric dispute). For this story, no overrides are declared because the basic derivation should work: beneficiaries get low d, payers get high d, power modulates exit options, and exit options feed into d. The committer frame documents that the advisory_reading itself construes directionality differently than the binding_referee reading would—this is routed to the kernel_context note and the conceptual omega, not embedded in override values.
 *
 * MANDATROPHY ANALYSIS:
 *   The advisory_coordination_reading does NOT present a mandatrophy case—the founding problem (dispute resolution without unilateral action) remains live and the constraint continues to address it. Member states consistently use the DSB to file cases, request panel establishment, and negotiate settlements informed by panel reports, which indicates the founding problem is live and the system is performing its function. The founding_problem_status is marked as 'contested' because some actors (developing states, rule-of-law advocates) argue that the founding problem has been superseded by a new problem: the DSB's legitimacy has eroded because it cannot enforce compliance against powerful states that selectively ignore unfavorable rulings. The advisory framing was meant to preserve sovereignty; in practice, it permits powerful states to treat the DSB as a tool for leverage rather than as an authoritative institution. This is not mandatrophy in the sense of atrophied function—the DSB still produces reports, disputes still settle—but rather contestation about whether the constraint's legitimacy justifies its operation. The constraint persists because it solves enough of the coordination problem for enough parties (particularly developed states and the disputants who use it strategically). If mandatrophy were to occur, it would manifest as declining dispute filing, growing noncompliance without settlement, or states attempting to establish alternative dispute mechanisms outside the WTO. None of these signals have materialized substantially at the interval endpoint (2026), so mandatrophy is not triggered.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    advisory_vs_binding_reading_contest,
    'Is the DSB constraint structurally a pure coordination mechanism (advisory reading) or has it functionally evolved into binding authority despite the formal legal status (binding referee reading)?',
    'Analysis of compliance rates with panel recommendations across state power asymmetries: if developing states comply at >80% while developed states comply selectively (~60%), the constraint operates as binding authority enforced asymmetrically; if overall compliance is ~70% across all states, the advisory coordination reading holds. Examine settlement patterns post-report: do states treat panel reports as binding instructions or as negotiation inputs?',
    'If the constraint operates as binding authority in practice, the advisory reading mischaracterizes the constraint''s function and the engine should classify it differently (tangled rope or snare with developed-state beneficiaries, developing-state victims, rather than rope). If the advisory reading is accurate, the constraint genuinely coordinates without asymmetric extraction, and the modest theater ratio reflects the performative effort states invest in maintaining the ''advisory'' framing while treating reports as de facto binding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(advisory_vs_binding_reading_contest, empirical, 'Whether the DSB operates as pure coordination or binding authority with asymmetric leverage.').

omega_variable(
    sovereignty_preservation_authenticity,
    'When member states declare they ''retain ultimate policy discretion,'' are they preserving genuine agency, or is the discretion illusory—constrained by reputational cost, systemic exclusion, or bilateral power asymmetries to the point where meaningful alternatives do not exist?',
    'Natural experiment: track instances where a state rejects a DSB panel report that favors a developed-state complainant and successfully negotiates a different outcome. Count frequency by state power level. If developing states can reject unfavorable rulings without reputational sanctions or exclusion, sovereignty is authentic; if rejection triggers informal penalties (negotiating partner cooling, delayed cooperation on unrelated issues), the discretion is nominal.',
    'If sovereignty is illusory, the constraint functions as binding authority with soft enforcement (reputation, exclusion) rather than formal legal obligation. The advisory reading becomes a cover story for de facto institutional authority. Reclassification would shift from rope (coordination) toward snare (extraction with sovereignty narrative).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_preservation_authenticity, empirical, 'Whether the ''discretion'' member states retain is authentic or constrained by systemic pressure.').

omega_variable(
    committer_frame_alternative_reading_hypothesis,
    'This story instantiates ONE reading of a contested kernel (wto_dsb_authority). What would the BINDING_REFEREE reading claim structurally, and how does it change the constraint''s classification?',
    'This is a conceptual omega documenting the committer structure: the binding_referee_reading would assert that DSB panels issue binding interpretations grounded in treaty law, that member states surrendered policy discretion within WTO-covered domains, and that non-compliance triggers remedial action. Under that reading, the constraint would compute as tangled rope (coordination + asymmetric extraction, with developed-state beneficiaries and developing-state victims) or even snare (if the binding authority operates without genuine consent). The advisory_coordination_reading (this story) preserves state sovereignty framing and computes as rope (coordination without asymmetric extraction). The two readings cannot coexist in a single member state''s legal framework—a state either treats DSB rulings as binding or discretionary—but they persist as competing authoritative claims held by different coalitions (developed states preferring advisory framing for flexibility; some developing states and rule-of-law advocates preferring binding authority for predictability).',
    'The classification divergence (rope vs. tangled rope or snare) is the measurement the corpus exists to take. The two readings belong in separate constraint story files, linked by network.affects_constraints. This omega documents the kernel decomposition and routes the interpretive contest through the apparatus rather than burying it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_alternative_reading_hypothesis, conceptual, 'The committer-frame alternative reading and its structural implications.').

omega_variable(
    developing_state_power_asymmetry_under_advisory_framing,
    'Under the advisory reading, does the DSB''s nonbinding status amplify or dampen the power asymmetry between developed and developing states?',
    'Compare settlement outcomes in disputes between (1) developed complainant vs. developed respondent, (2) developed complainant vs. developing respondent, and (3) developing complainant vs. developed respondent. Measure: frequency of panel-report adoption, magnitude of policy changes demanded, correlation between report recommendation and actual settlement terms. If adoption and correlation are higher in (1) and lower in (2), the advisory framing amplifies asymmetry—developed states can reject unfavorable reports and rely on leverage, while developing states'' rejections trigger reputational or economic penalties.',
    'If the advisory framing amplifies asymmetry, the constraint functions as snare for developing states despite the rope classification from the developed-state seat. Reclassification would be per-seat: rope for developed states (coordination without asymmetric extraction), snare for developing states (enforced extraction disguised as coordination). The engine would compute different types per stakeholder power level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developing_state_power_asymmetry_under_advisory_framing, empirical, 'Whether advisory status mitigates or amplifies power asymmetry between developed and developing states.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__advisory_coordination_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t0, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(wto__tr_t0, observed).
narrative_ontology:measurement(wto__tr_t5, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 5, 0.39).
narrative_ontology:measurement_basis(wto__tr_t5, observed).
narrative_ontology:measurement(wto__tr_t10, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement_basis(wto__tr_t10, observed).
narrative_ontology:measurement(wto__tr_t15, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 15, 0.41).
narrative_ontology:measurement_basis(wto__tr_t15, observed).
narrative_ontology:measurement(wto__tr_t20, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(wto__tr_t20, observed).
narrative_ontology:measurement(wto__tr_t25, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(wto__tr_t25, observed).
narrative_ontology:measurement(wto__tr_t30, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(wto__tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(wto__be_t0, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(wto__be_t0, observed).
narrative_ontology:measurement(wto__be_t5, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 5, 0.29).
narrative_ontology:measurement_basis(wto__be_t5, observed).
narrative_ontology:measurement(wto__be_t10, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 10, 0.31).
narrative_ontology:measurement_basis(wto__be_t10, observed).
narrative_ontology:measurement(wto__be_t15, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 15, 0.33).
narrative_ontology:measurement_basis(wto__be_t15, observed).
narrative_ontology:measurement(wto__be_t20, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement_basis(wto__be_t20, observed).
narrative_ontology:measurement(wto__be_t25, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 25, 0.32).
narrative_ontology:measurement_basis(wto__be_t25, observed).
narrative_ontology:measurement(wto__be_t30, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 30, 0.32).
narrative_ontology:measurement_basis(wto__be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t0, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 0, 0.16).
narrative_ontology:measurement_basis(wto__su_t0, observed).
narrative_ontology:measurement(wto__su_t5, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 5, 0.17).
narrative_ontology:measurement_basis(wto__su_t5, observed).
narrative_ontology:measurement(wto__su_t10, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 10, 0.17).
narrative_ontology:measurement_basis(wto__su_t10, observed).
narrative_ontology:measurement(wto__su_t15, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 15, 0.18).
narrative_ontology:measurement_basis(wto__su_t15, observed).
narrative_ontology:measurement(wto__su_t20, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 20, 0.18).
narrative_ontology:measurement_basis(wto__su_t20, observed).
narrative_ontology:measurement(wto__su_t25, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 25, 0.18).
narrative_ontology:measurement_basis(wto__su_t25, observed).
narrative_ontology:measurement(wto__su_t30, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 30, 0.18).
narrative_ontology:measurement_basis(wto__su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__advisory_coordination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(wto_dsb_authority__advisory_coordination_reading, 0.12).
narrative_ontology:affects_constraint(wto_dsb_authority__advisory_coordination_reading, wto_dsb_authority__binding_referee_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__advisory_coordination_reading, wto_dsb_authority__judicial_activism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel: wto_dsb_authority. The kernel decomposes into three structurally distinct constraints, each instantiating a different interpretation of DSB authority. The advisory_coordination_reading emphasizes expert analysis and state sovereignty; the binding_referee_reading asserts treaty-grounded binding authority; the judicial_activism_reading treats binding claims as institutional overreach. Each reading has distinct ε, victim/beneficiary structure, and computed type. All three belong in the constraint family and should be cross-linked. The ε-invariance principle requires decomposition: measuring the constraint via compliance rates, institutional authority, or state agency produces different ε values depending on which reading is active. Rather than force one story to handle the measurement ambiguity, three stories disambiguate the claim. The upstream constraint is the kernel itself (wto_dsb_authority as an unspecified formalization); these three are downstream readings. Each reading's corroboration comes from the state or scholar coalition that holds it, not from neutral external observers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
