% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__market_libertarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__market_libertarian_reading, []).

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
 *   constraint_id: ai_governance_legitimacy__market_libertarian_reading
 *   human_readable: Market-Libertarian Reading of AI Governance Legitimacy
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel 'what
 *   legitimates AI governance': the market-libertarian reading, on which
 *   legitimacy flows only from voluntary exchange and pre-political property
 *   rights, and collective mandates are categorically illegitimate coercion.
 *   The standing arrangement under contest — the arrangement this story is
 *   ABOUT — is AI development governed by enforced property and contract with
 *   collective mandates ruled out of bounds. Per the kernel-reading rule,
 *   epsilon's referent is that standing arrangement assessed BY THIS
 *   READING'S OWN LIGHTS: the reading prices formal consent heavily, so it
 *   authors low epsilon (0.26) even while acknowledging victims, whose plight
 *   it attributes to market outcomes rather than to the frame. The
 *   claim/metric gap is deliberate and load-bearing: claimed_type is mountain
 *   because the reading asserts property rights as pre-political natural
 *   order, while the metrics are authored descriptively of the arrangement's
 *   actual operation. Beneficiaries are declared INTENTIONALLY to trigger
 *   false-summit evaluation — the reading presents as natural law a frame
 *   with identifiable, concentrated beneficiaries. Sibling readings
 *   (magisterial-subsidiarity, technocratic-optimization,
 *   democratic-pluralist) are separate constraint files with their own
 *   epsilon values and victim sets; this file authors only the
 *   market-libertarian instantiation and links them through the network.
 *
 * KEY AGENTS:
 *   - - incumbent_model_owners: Agenda-setter and principal recipient (institutional/arbitrage) — drafts and enforces access terms, collects what the frame protects
 *   - - venture_capitalists: Beneficiary (institutional/arbitrage) — supplies capital conditioned on frame persistence, captures appreciation
 *   - - ai_startup_founders: Beneficiary (powerful/mobile) — builds under light collective obligation
 *   - - high_autonomy_technologists: Beneficiary (powerful/mobile) — the group for whom dignity-through-exit actually delivers
 *   - - ai_end_users: Dual-positioned beneficiary/payer (moderate/constrained) — clickwrap consent, nominal exit
 *   - - monopsony_labor_market_workers: Payer (powerless/trapped) — formal consent under thin alternatives
 *   - - powerless_market_participants: Payer (powerless/constrained) — take-it-or-leave-it terms everywhere
 *   - - communities_facing_coordination_failures: Payer (moderate/constrained) — bears unpriced costs, collective bargaining recoded as coercion
 *   - - magisterium_and_csd_advocates: Excluded (institutional/analytical) — ruled out of bounds by definitional fiat
 *   - - democratic_mandate_advocates: Excluded (organized/analytical) — proposals recoded as usurpation
 *   - - private_arbitration_bodies: Agenda-setter (institutional/constrained) — administers the substitute-for-public-law layer
 *   - - political_theology_scholars: Observer (analytical/analytical) — maps the doctrinal contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__market_libertarian_reading, 0.26).
domain_priors:suppression_score(ai_governance_legitimacy__market_libertarian_reading, 0.5).
domain_priors:theater_ratio(ai_governance_legitimacy__market_libertarian_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, extractiveness, 0.26).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__market_libertarian_reading, mountain).
narrative_ontology:human_readable(ai_governance_legitimacy__market_libertarian_reading, "Market-Libertarian Reading of AI Governance Legitimacy").
narrative_ontology:topic_domain(ai_governance_legitimacy__market_libertarian_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__market_libertarian_reading).
domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__market_libertarian_reading, 'c8ce07c6-254b-491b-b0cd-e46c96b5d80c').
narrative_ontology:cs_kernel_codification('c8ce07c6-254b-491b-b0cd-e46c96b5d80c', distributed).
narrative_ontology:cs_authority_grounding('c8ce07c6-254b-491b-b0cd-e46c96b5d80c', self_enforcing).
narrative_ontology:cs_reading_relation('c8ce07c6-254b-491b-b0cd-e46c96b5d80c', ai_governance_legitimacy__magisterial_subsidiarity_reading, forecloses).
narrative_ontology:cs_reading_relation('c8ce07c6-254b-491b-b0cd-e46c96b5d80c', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c8ce07c6-254b-491b-b0cd-e46c96b5d80c', ai_governance_legitimacy__technocratic_optimization_reading, influences).
narrative_ontology:cs_axiom('c8ce07c6-254b-491b-b0cd-e46c96b5d80c', foundational, property_rights_pre_political).
narrative_ontology:cs_axiom_status(property_rights_pre_political, holdable).
narrative_ontology:cs_axiom_grounding('c8ce07c6-254b-491b-b0cd-e46c96b5d80c', property_rights_pre_political, deontological).
narrative_ontology:cs_axiom('c8ce07c6-254b-491b-b0cd-e46c96b5d80c', foundational, solidarity_mandates_illegitimate_coercion).
narrative_ontology:cs_axiom_status(solidarity_mandates_illegitimate_coercion, holdable).
narrative_ontology:cs_axiom_grounding('c8ce07c6-254b-491b-b0cd-e46c96b5d80c', solidarity_mandates_illegitimate_coercion, deontological).
narrative_ontology:cs_axiom('c8ce07c6-254b-491b-b0cd-e46c96b5d80c', secondary, unencumbered_innovation_maximizes_flourishing).
narrative_ontology:cs_axiom_status(unencumbered_innovation_maximizes_flourishing, holdable).
narrative_ontology:cs_axiom_grounding('c8ce07c6-254b-491b-b0cd-e46c96b5d80c', unencumbered_innovation_maximizes_flourishing, instrumental).
narrative_ontology:cs_reference_frame('c8ce07c6-254b-491b-b0cd-e46c96b5d80c', pre_political_property_rights_order).
narrative_ontology:cs_drift_state('c8ce07c6-254b-491b-b0cd-e46c96b5d80c', contemporary_ai_governance_debate, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c8ce07c6-254b-491b-b0cd-e46c96b5d80c', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, incumbent_model_owners).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, venture_capitalists).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, ai_startup_founders).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_technologists).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, monopsony_labor_market_workers).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, communities_facing_coordination_failures).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, powerless_market_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__market_libertarian_reading, ai_end_users).
narrative_ontology:constraint_victim(ai_governance_legitimacy__market_libertarian_reading, ai_end_users).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, pre_political_property_rights_doctrine).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, voluntary_exchange_legitimacy_principle).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__market_libertarian_reading, spontaneous_order_epistemology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own the model weights, training-corpus licenses, and serving infrastructure through which frontier AI capability reaches everyone else. Draft the terms under which developers, firms, and governments access that capability, and enforce them through contract, API gating, and license audit. Fund the policy institutes, litigation shops, and standard-setting presence that defend the frame against mandate proposals. Capital and corporate domicile can move across borders faster than any single jurisdiction can tighten terms.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, incumbent_model_owners, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__market_libertarian_reading, incumbent_model_owners, beneficiary).

% Allocate capital into AI ventures on the expectation that title in models, data, and patents will be enforced and that collective mandates will not strand portfolio companies. Capture returns through equity appreciation and exits. Can redeploy capital across jurisdictions and asset classes far faster than rules can be rewritten.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, venture_capitalists, beneficiary,
    institutional, biographical, arbitrage, global).

% Build products on the assumption that what they create is theirs to sell, license, or withhold, and that no duty to consult affected communities or share benefits attaches by default. Light collective obligation is the operating condition their business plans assume. Exit means reincorporating elsewhere or pivoting markets.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, ai_startup_founders, beneficiary,
    powerful, biographical, mobile, global).

% Scarce talent commanding premium compensation, contractual freedom, and credible threats to walk, renegotiate, or found competitors. For this group, dignity-through-exit genuinely delivers: their bargaining position is real, and their experience anchors the frame's plausibility for everyone else.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, high_autonomy_technologists, beneficiary,
    powerful, biographical, mobile, global).

% Receive capability cheaply and conveniently — the visible benefit side of voluntary exchange. Consent arrives by clicking terms they do not read; behavioral data, attention, and preference traces flow continuously to providers. Switching costs and ecosystem lock-in make exit nominal: every alternative counter offers substantially the same terms.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, ai_end_users, beneficiary,
    moderate, immediate, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__market_libertarian_reading, ai_end_users, payer).

% Work where one or a few employers dominate local demand for their skills. Visa sponsorship, non-compete clauses, credential lock-in, and thin local labor markets narrow alternatives to near zero. Wages and conditions settle at the employer's reservation position. They signed voluntarily, in a market where every door opens onto the same counter.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, monopsony_labor_market_workers, payer,
    powerless, biographical, trapped, regional).

% Transact under take-it-or-leave-it terms wherever they go — gig platforms, data brokers, algorithmic credit and pricing systems. The frame records their clicks as consent. They hold no seat where terms are drafted and no realistic refusal option, since declining means exiting the service economy altogether.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, powerless_market_participants, payer,
    powerless, immediate, constrained, national).

% Bear the unpriced costs of rapid deployment: displaced industries, strained housing, hollowed local institutions, degraded information environments. The frame classifies their attempts to bargain collectively — benefit-sharing agreements, local vetoes, levies — as illegitimate mandates, leaving exit from home as their only recourse.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, communities_facing_coordination_failures, payer,
    moderate, generational, constrained, local).

% Hold that technology must answer to common-good, subsidiarity, and solidarity principles as authoritatively interpreted, with accountability to the vulnerable built into governance. This reading defines their contribution in advance as coercion, so they enter the conversation only as defendants. Their solidarity demands are the specific object this frame rules out of bounds.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, magisterium_and_csd_advocates, excluded,
    institutional, civilizational, analytical, global).

% Legislators, regulators, and civic coalitions pursuing collective guardrails: audit duties, liability rules, incident reporting, benefit-sharing requirements. The frame recodes their proposals as usurpation rather than governance; several of their instruments are preempted, litigated into paralysis, or relocated into private arbitration where public accountability does not reach.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, democratic_mandate_advocates, excluded,
    organized, generational, analytical, continental).

% Administer the dispute-resolution layer that substitutes for public law: confidential proceedings, precedent-shaping awards, dockets dominated by repeat players who can afford the fees. Their procedural choices determine which grievances ever surface and which disappear into settlement.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, private_arbitration_bodies, agenda_setter,
    institutional, biographical, constrained, global).

% Trace how theological categories — common good, solidarity, subsidiarity — migrate into technology governance debates, and how rival economic theologies (spontaneous order, pre-political rights) function as competing doctrines with their own anthropologies and soteriologies of the market. Document the structure of the dispute without holding a position in it.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__market_libertarian_reading, political_theology_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_governance_legitimacy__market_libertarian_reading, incumbent_model_owners).
narrative_ontology:fixing_cost_class(ai_governance_legitimacy__market_libertarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real coordination problem: secure, enforceable property and contract let strangers invest, build, and transact over long horizons without first agreeing on ultimate values or submitting to a common interpreter. Capital formation, talent matching, and rapid product iteration proceed on the strength of title and agreement alone.
% TRANSFER_FUNCTION: Moves returns from AI deployment — subscription revenue, licensing fees, equity appreciation, data-derived advantage — toward holders of capital, intellectual property, and scarce skills; moves risk and unpriced cost (labor displacement, community disruption, behavioral-data exposure) onto participants without market power, under terms those participants formally accepted.
% ABSENT_VOICES: The Magisterium and Catholic Social Doctrine advocates are excluded by definitional fiat — this reading classes their solidarity demands as coercion before they speak. Democratic mandate advocates fare similarly: their proposals arrive pre-recoded as usurpation. Workers without exit options and communities bearing unpriced costs have no seat where terms are drafted; future generations exposed to uninternalized risk are present in no forum this frame recognizes.
% DISAPPEARANCE_RATIONALE: If the pre-political property floor vanished overnight, every jurisdiction would immediately confront the question this frame forecloses — who may govern AI, and by what warrant. Model weights, data rights, and platform terms would fall to whichever authority moved first: democratic legislatures, magisterial principles, or technocratic boards. Capital would reprice violently against jurisdictions perceived as expropriating; the licensing, arbitration, and reputational edifice would lose its object at once.
% FOUNDING_PROBLEM: Securing productive activity against arbitrary expropriation by rulers, and enabling strangers to transact without shared ultimate values — the classical liberal problem of making long-horizon investment safe from predation.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and development economists outside the benefiting parties corroborate the original problem's reality: expropriation demonstrably destroyed commerce, and secure title correlates with investment. But corroboration for the problem's continuing sufficiency in the AI case comes almost entirely from the frame's beneficiaries. Catholic Social Doctrine scholars, political theologians, and labor economists outside that set attest that AI-scale concentration and externality profiles pose a coordination problem the founding toolkit does not address. Both attestations are on the record; neither settles the matter.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__market_libertarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__market_libertarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__market_libertarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_governance_legitimacy__market_libertarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__market_libertarian_reading, 0.26, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__market_libertarian_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, ExtMetricName, E),
    domain_priors:suppression_score(ai_governance_legitimacy__market_libertarian_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ai_governance_legitimacy__market_libertarian_reading),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ai_governance_legitimacy__market_libertarian_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ai_governance_legitimacy__market_libertarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.26) BY THIS READING'S OWN LIGHTS over the fixed referent: most transfers occur under formally voluntary terms the frame counts as consent, and the residual epsilon reflects what even the reading concedes — enforcement-funded background coercion, monopsony wage-setting below competitive baselines, and unpriced externalities. Suppression (0.50) is a raw structural property, unscaled by power or scope: the arrangement actively suppresses collective mandates through litigation, preemption, doctrinal framing, and relocation of disputes into private arbitration — suppressing alternatives is its enforcement object, not a side effect. Theater ratio (0.30): the core functions (title, contract, arbitration) are real, but a growing share of maintenance is rhetorical — dignity-through-exit proclaimed where exit has thinned, innovation-unencumbered proclaimed by incumbents wielding patent thickets and data moats. Accessibility collapse (0.62): within the frame, alternatives collapse nearly completely (mandates are definitionally illegitimate); across frames, rivals remain live, so the collapse is strong but not total. Resistance (0.55): the encyclical, democratic movements, and regulatory campaigns meet the frame head-on — resistance at this level is itself diagnostic against genuine natural-law status, which meets near-zero resistance. The temporal series runs on ONE shared grid (t=0..24, seven points, every tracked metric at every point); all three metrics rise together as consolidation deepens under the frame's protection and its defensive apparatus matures. Endpoint values equal the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply different types from identical structural data. From the beneficiary seats — founders, investors, autonomous technologists — the arrangement reads as near-natural order: their consent is real, their exits are real, and mandates appear as pure imposition. From the payer seats — monopsony workers, clickwrap users, exposed communities — the same structure operates as enforced asymmetry: consent formally present, substantively unavailable. The excluded seats (magisterial, democratic) experience the frame as a prior restraint on speech itself. The engine computes this divergence from power, exit, and role data; the authored mountain claim does not adjudicate it — and the divergence between that claim and the payer-seat computation is precisely the false-summit signal this story is built to register.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (model owners, VCs, founders, autonomous technologists) derive low directionality — the arrangement subsidizes them, and their arbitrage-grade exits push them toward the beneficiary pole. Declared victims (monopsony workers, powerless participants, exposed communities) derive high directionality — they bear the transfer, and trapped or constrained exits pin them near the full-target pole. End users are structurally dual: genuine consumer surplus on one side, clickwrap data-and-lock-in terms on the other; their secondary payer role keeps them off the pure-beneficiary pole. Excluded voices contribute no directionality — they are outside the arrangement's operation — but their exclusion is the enforcement object and belongs in the record.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing commerce against predation — retains a live core, so this is not a zombie of a dead mandate and mandatrophy_resolved is NOT declared. The classification discipline cuts both ways here. Reading the arrangement as pure extraction would erase its genuine coordination function: enforceable title and contract really do enable strangers to invest without shared ultimate values, which is a real solved problem. Accepting the mountain claim at face value would instead immunize an actively enforced, beneficiary-bearing arrangement from all scrutiny — which is exactly the false-summit failure the FSM signature exists to catch. The honest structural reading sits between: real coordination function, real beneficiaries, real suppressed alternatives, real victims the frame's own consent-doctrine cannot see. The contested founding-problem status plus world-rearranging disappearance verdict marks the frame as load-bearing but no longer self-evidently sufficient — the condition under which mislabeling in EITHER direction is most likely and the per-seat computation most needed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_entitlement,
    'Is the pre-political property-rights frame a discovered natural order constraining all governance, or a constructed entitlement regime whose specific shape benefits identifiable agents?',
    'Comparative jurisprudence and economic history: property regimes vary radically across functioning societies while physical regularities do not; trace whose holdings each variant entrenches and who drafts the enforcement terms. Convergence across independent traditions would support naturality; divergence tracking beneficiary interests supports construction.',
    'If constructed, the mountain claim fails and the false-summit signature reclassifies toward a hybrid coordination/extraction profile; the frame loses its immunity-from-scrutiny and its beneficiaries become assessable as such. If natural, the declared beneficiaries are incidental and the low authored epsilon stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_entitlement, conceptual, 'Whether the frame is natural law or constructed entitlement — the FSM ambiguity this story''s beneficiary declarations are designed to expose.').

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the kernel ai_governance_legitimacy; how much of its classification is reading-indexed rather than topic-level?',
    'Family-level comparison: the magisterial reading authors high epsilon for THIS arrangement (reading unmandated externalities as extraction from the vulnerable) while this reading authors low epsilon for itself; the disagreement is located in the legitimacy-source slot — who or what confers authority to govern AI. Only cross-reading comparison over the shared referent resolves it.',
    'Classification is a property of the reading, not the topic; any verdict on ''AI governance legitimacy'' as such is ill-formed without specifying the reading. Sibling files carry the same referent with different authored values by design.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-structure omega: reading-indexed epsilon over a fixed referent; siblings are separate constraints, not hedges inside this one.').

omega_variable(
    consent_validity_under_monopsony,
    'Does formal voluntariness under monopsony concentration and thin alternatives constitute valid consent (supporting low extraction) or structurally coerced transfer (invalidating the consent foundation)?',
    'Labor economics on monopsony wage gaps, switching-cost measurement, and natural experiments from jurisdictions weakening non-competes and visa tying: if wages and terms move materially when alternatives widen, the prior terms were set by captured bargaining positions, not free consent.',
    'If consent is invalid under concentration, the reading''s epsilon foundation collapses — effective extraction for trapped seats rises sharply and the arrangement computes as enforced asymmetry despite the reading''s self-assessment; the mountain claim loses its consent-based warrant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_validity_under_monopsony, empirical, 'Whether click-level and contract-level consent survives concentration — the empirical hinge under the reading''s legitimacy claim.').

omega_variable(
    externality_pricing_gap,
    'Are the unpriced costs borne by communities and future generations (displacement, institutional erosion, data exposure, tail risk) extraction the frame conceals, or genuine costs of freedom the frame transparently allocates?',
    'Welfare-economic quantification of uninternalized costs in AI deployment against the frame''s own compensation channels (consumer surplus, growth dividends); test whether losers are compensated through any mechanism the frame itself recognizes.',
    'A material uncompensated wedge raises effective extraction for the payer seats and strengthens the hybrid-coordination/extraction reading of the arrangement; a negligible wedge supports the reading''s claim that costs are the visible price of dynamism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_pricing_gap, empirical, 'Size of the unpriced-cost wedge the frame leaves on outsiders.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__market_libertarian_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement(ai_g_tr_t4, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(ai_g_tr_t8, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(ai_g_tr_t12, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement(ai_g_tr_t16, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(ai_g_tr_t24, ai_governance_legitimacy__market_libertarian_reading, theater_ratio, 24, 0.3).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 0, 0.17).
narrative_ontology:measurement(ai_g_be_t4, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 4, 0.19).
narrative_ontology:measurement(ai_g_be_t8, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 8, 0.2).
narrative_ontology:measurement(ai_g_be_t12, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 12, 0.22).
narrative_ontology:measurement(ai_g_be_t16, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 16, 0.23).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 20, 0.25).
narrative_ontology:measurement(ai_g_be_t24, ai_governance_legitimacy__market_libertarian_reading, base_extractiveness, 24, 0.26).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(ai_g_su_t4, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 4, 0.37).
narrative_ontology:measurement(ai_g_su_t8, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(ai_g_su_t12, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 12, 0.43).
narrative_ontology:measurement(ai_g_su_t16, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(ai_g_su_t20, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(ai_g_su_t24, ai_governance_legitimacy__market_libertarian_reading, suppression_requirement, 24, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__market_libertarian_reading, resource_allocation).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__magisterial_subsidiarity_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__market_libertarian_reading, ai_governance_legitimacy__democratic_pluralist_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'AI governance legitimacy' conflates four structurally distinct claims, each instantiating a different constraint with its own epsilon, beneficiary structure, and victim set. This file is the market-libertarian member (low authored epsilon by its own lights, mountain claim, FSM-triggering beneficiaries). The magisterial-subsidiarity member authors high epsilon for this same arrangement and reverses the beneficiary/victim polarity; the technocratic and democratic-pluralist members occupy intermediate positions. The members are linked pairwise through affects_constraints so contamination and legitimacy-resource flows propagate across the family; no single file adjudicates the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
