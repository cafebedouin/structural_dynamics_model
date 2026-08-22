% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_dispute_settlement_authority, []).

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
 *   constraint_id: trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority
 *   human_readable: WTO Dispute Settlement Interpretive Authority over TRIPS
 *   domain: international_trade_law/intellectual_property
 *
 * SUMMARY:
 *   The WTO Dispute Settlement Body adjudicates TRIPS disputes through panels
 *   that render binding interpretations of the text. This reading
 *   instantiates the constraint as perceived by those who hold binding
 *   interpretive authority — the panels and the institutional framework that
 *   backs them with retaliation threat. The TRIPS Agreement text itself is
 *   the kernel: it can be read to permit broad compulsory licensing for
 *   public health (public_health_flexibility_reading) or to mandate high
 *   uniform patent protection (strong_exclusivity_reading) or, in this
 *   reading, to be subject to binding panel interpretation that locks in one
 *   reading over another through precedent and retaliation threat. The
 *   Appellate Body's collapse (2017–2020) is the structural pivot: before
 *   collapse, an appellate court could review and harmonize panel
 *   jurisprudence; after collapse, panel rulings stand unchallenged and
 *   accumulate as precedent, making this reading's interpretive lock
 *   absolute. The ε values rise sharply at t=10 (appellate collapse) and
 *   plateau thereafter, reflecting the new regime where panels have no
 *   judicial superior and extraction becomes irreversible through precedent.
 *
 * KEY AGENTS:
 *   - WTO dispute panels: institutional agenda-setter; render binding interpretations; operate without appellate review post-2020
 *   - Multinational pharmaceutical firms: primary beneficiaries; fund litigation, submit amici, secure narrow reading of flexibilities
 *   - Generic drug manufacturers: primary victims; constrained by narrow reading of compulsory licensing window
 *   - Least-developed countries: secondary victims; trapped by powerlessness, excluded from litigation, identity-locked to trade order
 *   - Public health ministries: secondary victims; dual mandate (TRIPS compliance + health access) increasingly irreconcilable
 *   - WTO member governments (strong IP position): secondary beneficiaries; bring cases, threaten retaliation, mobile exit
 *   - WTO member governments (public health focus): secondary victims; lose disputes, cannot threaten retaliation credibly, constrained exit
 *   - Appellate Body: structural absence; would have reviewed/harmonized; collapse is the mechanism that locks interpretive authority
 *   - Civil society health advocates: excluded; cannot bring cases, can submit briefs, lack retaliation threat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.68).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.72).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, extractiveness, 0.68).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "WTO Dispute Settlement Interpretive Authority over TRIPS").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "international_trade_law/intellectual_property").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, '683ccf90-ac3c-4604-8c27-8f5864112791').
narrative_ontology:cs_kernel_codification('683ccf90-ac3c-4604-8c27-8f5864112791', fixed_text).
narrative_ontology:cs_authority_grounding('683ccf90-ac3c-4604-8c27-8f5864112791', extraction).
narrative_ontology:cs_reading_relation('683ccf90-ac3c-4604-8c27-8f5864112791', trips_agreement_interpretive_kernel__trips_agreement_public_health_flexibility_reading, forecloses).
narrative_ontology:cs_reading_relation('683ccf90-ac3c-4604-8c27-8f5864112791', trips_agreement_interpretive_kernel__trips_agreement_strong_exclusivity_reading, coexists_with).
narrative_ontology:cs_axiom('683ccf90-ac3c-4604-8c27-8f5864112791', foundational, wto_panel_interpretive_authority_binding).
narrative_ontology:cs_axiom_status(wto_panel_interpretive_authority_binding, holdable).
narrative_ontology:cs_axiom_grounding('683ccf90-ac3c-4604-8c27-8f5864112791', wto_panel_interpretive_authority_binding, conventional).
narrative_ontology:cs_axiom('683ccf90-ac3c-4604-8c27-8f5864112791', foundational, panel_precedent_irreversible_post_appellate_collapse).
narrative_ontology:cs_axiom_status(panel_precedent_irreversible_post_appellate_collapse, holdable).
narrative_ontology:cs_axiom_grounding('683ccf90-ac3c-4604-8c27-8f5864112791', panel_precedent_irreversible_post_appellate_collapse, empirically_contingent).
narrative_ontology:cs_reference_frame('683ccf90-ac3c-4604-8c27-8f5864112791', multilateral_rule_based_dispute_settlement).
narrative_ontology:cs_drift_state('683ccf90-ac3c-4604-8c27-8f5864112791', post_appellate_body_collapse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('683ccf90-ac3c-4604-8c27-8f5864112791', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, multinational_pharmaceutical_firms).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, patent_holding_corporations).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, generic_drug_manufacturers).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, least_developed_countries).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, public_health_ministries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_member_governments_strong_ip_position).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_member_governments_public_health_focus).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, multilateral_trade_dispute_settlability_doctrine).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, interpretation_precedent_as_binding_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hear disputes over TRIPS compliance, render binding interpretations of the text, and issue rulings that lock in one reading against alternatives. Panel composition and reasoning are written; precedent accumulates. Panels operate under the Dispute Settlement Understanding and, when appellate review existed, faced appellate oversight. Since Appellate Body collapse (2020), panels' interpretations stand unchallenged by any superior tribunal within the WTO.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_dispute_panels, agenda_setter,
    institutional, generational, analytical, global).

% Secure high patent protection standards through panel rulings that narrow the public health flexibilities (compulsory licensing, parallel imports, emergency use). Each panel ruling that constrains flexibility is a strategic win; they fund litigation, submit amicus briefs, and coordinate with compliant national governments. Exit for them is lobbying the same panels through different cases or shifting to stronger bilateral agreements that bypass the TRIPS text entirely.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, multinational_pharmaceutical_firms, beneficiary,
    institutional, generational, arbitrage, global).

% Bear the cost of panel rulings that narrow compulsory licensing windows and parallel import routes. Each narrowing reduces their legal room to reverse-engineer drugs post-patent-life, produce copies for public health emergencies, or import cheaper versions from jurisdictions with legitimate manufacture. Their exit is limited: they can lobby their governments to bring complaints, but if the panels rule against them, they face trade retaliation on unrelated goods.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, generic_drug_manufacturers, payer,
    moderate, biographical, constrained, global).

% Cannot afford patented drug prices set by monopoly holders; are formally exempt from TRIPS for pharmaceuticals until 2033 (least-developed-country transition clause), but dispute panel rulings can weaken the exemptions' scope or create market pressure to comply early. They lack resources to hire counsel for WTO litigation, cannot credibly threaten trade retaliation, and are trapped by their need for medicines and their subordinate position in global trade. Many are excluded from the dispute settlement process entirely because they lack the legal and financial capacity to bring cases.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, least_developed_countries, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, least_developed_countries, excluded).

% Operate under dual mandates: comply with TRIPS and provide affordable healthcare. Panel rulings that narrow compulsory licensing force them to either violate trade rules (retaliation risk) or accept higher prices (public health failure). They are trapped by the constraints of WTO membership, low bargaining power, and institutional identity (they are bound by the legal order their governments signed). Exit for them is costly: withdrawing from TRIPS means losing market access and facing retaliation on unrelated trade.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, public_health_ministries, payer,
    powerless, immediate, identity_locked, national).

% Home to multinational pharmaceutical and tech firms; benefit from panel rulings that strengthen global patent protection. They bring disputes to the panels, defend strong readings of TRIPS text, and use the threat of retaliation against rivals to enforce compliance. Their exit is low-cost: if a panel rules against them, they can bring another case on a different issue, or negotiate bilaterally outside the TRIPS framework (Regional Trade Agreements). They have the legal and financial capacity to litigate and the economic power to threaten trade retaliation credibly.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_member_governments_strong_ip_position, beneficiary,
    powerful, generational, mobile, national).

% Want to use TRIPS flexibilities to provide affordable medicines but face panel rulings that narrow those flexibilities. They can bring disputes as complainants but lack the economic leverage to threaten retaliation and lose cases more often. Their exit is constrained: leaving TRIPS means abandoning market access; staying means accepting panel rulings that constrain their domestic health policy. Exit is also identity-locked for some — their institutional self-concept ties to participation in the multilateral order.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_member_governments_public_health_focus, payer,
    moderate, biographical, constrained, national).

% The Appellate Body (the WTO's supreme tribunal for trade disputes) has been dysfunctional since 2017 and effectively collapsed in 2020. An appellate function that could review panel interpretations for legal error and harmonize jurisprudence would be here; instead it is absent. This absence is precisely what makes this reading's interpretive authority so absolute — there is no higher tribunal to correct an over-reading, and panel decisions stack to form precedent. The exclusion of effective appellate review IS the mechanism that locks this reading in.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, appellate_body_substitute_or_absent, excluded,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, appellate_body_substitute_or_absent).

% Have standing to submit amicus briefs in some panels but cannot bring cases, fund litigation, or threaten retaliation. They advocate for public health flexibility readings but face institutional barriers to participation and are systematically excluded from the highest-leverage negotiation spaces where TRIPS is remade bilaterally.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, civil_society_health_advocates, excluded,
    powerless, immediate, trapped, global).

% International trade law scholars, policy analysts, and watchdog organizations track panel jurisprudence, document how precedent shifts, and analyze the cumulative impact on the TRIPS ecosystem. They produce the historical record and empirical analysis that would resolve the omegas, but have no direct authority to alter panel decisions.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_law_academy, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, multinational_pharmaceutical_firms).
narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a binding, rule-based framework for multilateral dispute resolution over intellectual property obligations, enabling WTO members to challenge each other's policies without resort to unilateral retaliation or arms-race escalation. The panels produce written reasoning that creates precedent, so disputes do not restart from zero each time.
% TRANSFER_FUNCTION: Moves policy space and interpretive authority from individual WTO members' regulatory discretion to panel rulings, and from broader textual flexibility readings to narrower ones favoring patent holder interests. The mechanism transfers constraint leverage from low-income health-focused governments to high-income IP-holder governments through the retaliation threat (countries that lose disputes face authorized trade retaliation on unrelated goods if they don't comply).
% ABSENT_VOICES: Least-developed countries and their public health constituencies are structurally excluded: they cannot afford to litigate, their governments lack legal resources, they face the largest health consequences of narrow TRIPS readings, and they are not represented in panel composition. Generic drug manufacturers are excluded from formal proceedings — they participate only through their governments' sponsorship. The Appellate Body is absent — it would have reviewed and harmonized panel rulings, constrained over-interpretation, and provided a higher court to correct error, but it has been non-functional since 2017 and collapsed entirely in 2020.
% DISAPPEARANCE_RATIONALE: If WTO dispute panels' binding interpretive authority over TRIPS vanished, member governments would revert to bilateral negotiation, regional agreements would proliferate, and the multilateral IP regime would fragment. Patent policy would be determined by bilateral bargaining power rather than rule-based adjudication. Generic drug access would expand in countries that negotiate favorable terms bilaterally but contract in those without bargaining leverage. Pharmaceutical innovation incentives would shift to markets where strong IP protection could be negotiated bilaterally.
% FOUNDING_PROBLEM: In the 1980s–1990s, member governments had conflicting readings of what TRIPS required: some interpreted it to allow broad compulsory licensing for public health; others read it to mandate high uniform patent protection. Disputes over compulsory licensing (India, Brazil) threatened unilateral retaliation. The founding problem was: how do we adjudicate these disputes without each member unilaterally imposing its reading through trade punishment?
% FOUNDING_PROBLEM_CORROBORATION: Pharmaceutical firms and IP-focused governments attest the founding problem is still live: they continue to bring disputes challenging public health flexibilities, arguing that without panels enforcing narrow readings, members would unilaterally adopt broad flexibilities and patent protection would collapse globally. Public health advocates and generic drug manufacturers attest the founding problem is SOLVED — the panels have generated predictable rulings, but the predictability has locked in a pro-patent reading that was not compelled by the text and now forecloses the flexibility the founding members (notably India, Brazil) read into TRIPS. Independent trade law scholars document that the 'problem' has been replaced by a different one: panel precedent now risks ossifying the interpretation and preventing evolutionary rereadings as public health crises (pandemics, antimicrobial resistance) emerge.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 endpoint) because the constraint's core function is to lock in a narrow reading of TRIPS that benefits patent holders and constrains public health flexibility. The reading-as-binding mechanism IS extractive: it takes policy discretion from individual governments and transfers it to panels, and then stacks panel rulings as precedent that forecloses reinterpretation. Suppression is very high (0.72) because the mechanism persists through retaliation threat (trade punishment on unrelated goods) rather than consensus. Theater_ratio rises sharply at t=10 (appellate collapse) and then plateaus (0.41), reflecting the moment when the interpretive authority becomes absolute and the 'dispute resolution as fair process' function gives way to 'precedent lock-in through institutional absence.' The measurement grid is aligned across all three metrics at each time point. The t=10 event (Appellate Body dysfunctionality, formalized by 2020) is the structural rupture: before it, panels' interpretations could be reviewed; after it, they stand unchallenged. This reading's binding authority is precisely what emerges from that absence.
 *
 * PERSPECTIVAL GAP:
 *   From the panel and strong-IP government perspective, this is genuine coordination and dispute resolution: plural readings exist, panels apply law, states accept rulings and maintain the rule of law. From the public health and victim perspective, this is enforced reading lock-in: the panels' rulings are not neutral legal interpretation but strategic determinations that narrow flexibility, and retaliation threat ensures compliance even for governments that disagree with the reading. The engine computes this divergence per seat: the strong-IP government and panel seats compute types based on low effective extraction (they benefit or are neutral); victim seats compute types based on high effective extraction (they pay, constrained exit, trapped). The claimed type (tangled_rope) reflects the fact that this constraint BOTH coordinates multilateral dispute resolution (genuine coordination function) AND asymmetrically extracts from victim seats (real extraction). But the engine's per-seat computation will show that victim seats perceive this as snare (extraction dominant) while beneficiary seats perceive it as rope (coordination dominant), and that divergence IS what this reading measures.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical firms and strong-IP governments sit at the beneficiary end (d near 0.0): they use the panels to enforce their reading, they face no retaliation (they win disputes or bring new ones), their exit is mobile (they can renegotiate bilaterally). Generic manufacturers, public health ministries, and LDCs sit at the victim end (d near 1.0): they lose disputes, face retaliation if they defy rulings, and have constrained exit (leaving TRIPS means losing market access). The panels themselves occupy an ambiguous middle: they are institutional agents that HAVE AUTHORITY but do NOT COLLECT from the arrangement (they do not profit from panels ruling for patent holders). In the sense of power/exit, they are 'analytical' — they implement a rule, do not extract. But structurally, they are the MECHANISM by which extraction persists: their binding authority is what makes this reading's transfer possible. This paradox (analytical seats that execute extractive power) is handled by directionality_overrides if needed: the panels' d should derive low from 'no extraction collected' but their structural role (locking in readings that benefit beneficiaries) argues for a higher override to mark that they are NOT neutral implementers of a neutral rule.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (adjudicating TRIPS disputes without unilateral retaliation; enabling multilateral rule-based trade) is CONTESTED as to whether it is live or dead. The panels' existence and rulings demonstrate the 'problem solved' narrative: disputes have been adjudicated, precedent has accumulated, the system has not collapsed into retaliation wars. BUT the public health community and some member governments attest that the problem was solved at the cost of locking in a substantive reading (narrow flexibility, high patent protection) that was NOT compelled by the text and now forecloses evolutionary rereadings. The mandatrophy signal is the mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges: if the panels vanished, the world WOULD rearrange (members would revert to bilateral bargaining, the multilateral regime would fragment), yet it is unclear whether the current arrangement is solving the original problem or displacing it. The measurement series shows extractiveness and suppression rise at t=10 (appellate collapse) and plateau, suggesting the 'solution' has stabilized into a permanent lock-in rather than an adaptive mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_ambiguity_of_trips_flexibilities,
    'Does the TRIPS text itself mandate narrow or broad compulsory licensing and parallel import rights, or is it genuinely ambiguous such that multiple substantively different readings are defensible from the text alone?',
    'Systematic textual analysis comparing the plain language, negotiating history (GATT records), and structural context of TRIPS Articles 30-31 (compulsory licensing), 6 (parallel imports), and 8 (public health carve-out) against the text''s architecture and the negotiators'' express statements in the record.',
    'If the text is genuinely ambiguous, panel rulings are interpretations layered atop irreducible textual openness, not determinations of what the text compels. If narrow readings are textually required, the panels are implementing law rather than making law. The first scenario supports this reading''s extractiveness (panels are locking one reading in, not discovering it); the second undermines it (the lock-in is merely enforcement of what the text requires).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_ambiguity_of_trips_flexibilities, conceptual, 'Whether the TRIPS text unambiguously requires one reading or leaves multiple substantive readings textually defensible.').

omega_variable(
    appellate_body_intentionality_of_collapse,
    'Is the Appellate Body''s dysfunctionality (2017–2020) a technical negotiation impasse on procedural appointments, or a structural choice by strong-IP governments to prevent appellate review of panel rulings they favor?',
    'Documentary analysis of negotiating positions, statements of intent, and strategic incentives of governments during the Appellate Body crisis. If strong-IP governments moved to unblock appointments, the collapse was incidental; if they blocked or delayed appointments while benefiting from the absence of appellate review, the collapse was deliberate.',
    'Intentional collapse strengthens the reading that this constraint is extractive (panels are locked in deliberately, the ''dispute resolution'' framing is theater); incidental collapse is consistent with genuine coordination that happened to break down. The extraction increases either way, but the mechanism changes from deliberate institutional capture to institutional dysfunction that was allowed to persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appellate_body_intentionality_of_collapse, empirical, 'Whether Appellate Body collapse was intentional institutional disabling by strong-IP governments or inadvertent procedural breakdown.').

omega_variable(
    retaliation_threat_credibility_and_asymmetry,
    'Is the WTO retaliation authorization (DSU Article 22) applied symmetrically across strong-IP and weak-bargaining-power members, or are weak members systematically deterred from retaliating because the economic costs are catastrophic while strong members'' retaliation threats are implicit and sufficient?',
    'Empirical analysis of retaliation authorization patterns: which members have requested and received retaliation authority, which have declined to retaliate despite authorization, and what they cite as the reason. Correlate with market size, trade dependence, and WTO voting power to test whether asymmetry correlates with structural position.',
    'If retaliation is asymmetric (strong members'' threats deter without action; weak members'' retaliation is self-damaging), the suppression metric understates the mechanism''s extractive force because the threat itself, without execution, sustains compliance. The constraint''s effective suppression is higher than the ''retaliation actually used'' count suggests; it is ''retaliation threat that deters defiance'' which is asymmetric by power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retaliation_threat_credibility_and_asymmetry, empirical, 'Whether WTO retaliation authorization and threat is applied symmetrically across member power positions or asymmetrically leverages the strong member''s deterrent without execution.').

omega_variable(
    panel_composition_and_expertise_capture,
    'Are WTO dispute panels selecting arbitrators with genuine independence, or are they systematically staffed with trade law experts who are trained in IP regimes favoring patent holders and who have professional incentives to maintain strong patent precedent?',
    'Systematic analysis of panel arbitrator selection, their prior affiliations, publications, and positions on IP and public health disputes. Correlate with their rulings: do arbitrators with pharmaceutical-industry backgrounds or pro-patent scholarship disproportionately rule for patent holders?',
    'If panels are systematically stacked with pro-patent expertise, the ''binding interpretive authority'' is not neutral arbitration but institutionalized capture disguised as adjudication. This would support high extraction (the panels are not discovering law, they are implementing one faction''s reading as law). If panels have diverse expertise, they are more plausibly neutral, though still locked in by precedent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(panel_composition_and_expertise_capture, empirical, 'Whether WTO dispute panels are composed with genuine independence or systematically staffed with arbitrators trained in and sympathetic to patent-holder interests.').

omega_variable(
    alternative_reading_suppression_through_precedent,
    'How complete is the precedent lock-in? Can member governments bring new disputes that argue for reinterpreting TRIPS flexibilities more broadly, or does prior precedent so constrain the panel''s argument space that new cases merely reinforce the narrow reading?',
    'Systematic review of dispute cases brought after major panels rulings on flexibility (India''s compulsory licensing case, Brazil''s parallel import challenges, etc.) to test whether new cases present new arguments or merely rehearse prior precedent. If new arguments are raised but rejected on precedent grounds, the precedent is suppressing reinterpretation.',
    'If precedent is absolutely constraining, the panels'' interpretive authority is irreversible in practice: once the narrow reading is locked in, no future dispute can dislodge it through new arguments. This would support very high theater_ratio (panels are performing dispute resolution, not performing open adjudication) and accessibility_collapse (alternatives are structurally foreclosed by precedent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_suppression_through_precedent, empirical, 'Whether precedent in WTO panels constrains future cases so completely that new arguments for flexible TRIPS readings are structurally excluded or merely overruled on precedent.').

omega_variable(
    committer_reading_contest_status,
    'Among WTO members and the TRIPS community, is the ''dispute_settlement_interpretive_authority'' reading held as THE CORRECT reading, or is it one contested reading among the three (strong_exclusivity, public_health_flexibility, dispute_settlement_authority) such that significant parties hold the sibling readings instead?',
    'Documentary evidence: legislative statements from member governments, positions taken in TRIPS Council, litigation briefs from member-state counsel, and expert consensus. Count how many members'' official positions endorse each of the three readings.',
    'If this reading is hegemonic (all/most members accept that panels hold binding authority), the contest is over in practice and this reading is functionally a mountain (irreversible, natural-seeming, no real alternatives). If it is contested (significant members hold sibling readings), then this reading is contingent and might be reversed if Appellate Body were restored or TRIPS were renegotiated. The reading''s classification as extractive vs. natural shifts with this.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_reading_contest_status, empirical, 'Whether the dispute-settlement-interpretive-authority reading is hegemonic or contested among WTO members and the expert community.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trips_theater_t0, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(trips_theater_t0, projected).
narrative_ontology:measurement(trips_theater_t5, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(trips_theater_t5, observed).
narrative_ontology:measurement(trips_theater_t10_appellate_crisis, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(trips_theater_t10_appellate_crisis, observed).
narrative_ontology:measurement(trips_theater_t15_appellate_absent, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 15, 0.41).
narrative_ontology:measurement_basis(trips_theater_t15_appellate_absent, observed).
narrative_ontology:measurement(trips_theater_t20, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(trips_theater_t20, observed).
narrative_ontology:measurement(trips_theater_t30_stabilized, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(trips_theater_t30_stabilized, observed).

% Extraction over time
narrative_ontology:measurement(trips_extract_t0_initial, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(trips_extract_t0_initial, projected).
narrative_ontology:measurement(trips_extract_t5, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(trips_extract_t5, observed).
narrative_ontology:measurement(trips_extract_t10_appellate_collapse, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(trips_extract_t10_appellate_collapse, observed).
narrative_ontology:measurement(trips_extract_t15_post_collapse, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(trips_extract_t15_post_collapse, observed).
narrative_ontology:measurement(trips_extract_t20, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(trips_extract_t20, observed).
narrative_ontology:measurement(trips_extract_t30_plateau, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(trips_extract_t30_plateau, observed).

% Suppression requirement over time
narrative_ontology:measurement(trips_suppress_t0, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(trips_suppress_t0, projected).
narrative_ontology:measurement(trips_suppress_t5, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(trips_suppress_t5, observed).
narrative_ontology:measurement(trips_suppress_t10_appellate_failure, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(trips_suppress_t10_appellate_failure, observed).
narrative_ontology:measurement(trips_suppress_t15_appellate_absent, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 15, 0.72).
narrative_ontology:measurement_basis(trips_suppress_t15_appellate_absent, observed).
narrative_ontology:measurement(trips_suppress_t20, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(trips_suppress_t20, observed).
narrative_ontology:measurement(trips_suppress_t30_plateaued, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(trips_suppress_t30_plateaued, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.12).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_public_health_flexibility_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_strong_exclusivity_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, compulsory_licensing_access_to_medicines_constraint).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_appellate_body_institutional_collapse).

% DUAL FORMULATION NOTE:
% This story is one reading of the contested TRIPS Agreement kernel. Sibling readings (public_health_flexibility_reading and strong_exclusivity_reading) are separate constraint stories with distinct ε values, beneficiary/victim structures, and classifications. This reading differs by being META (about how TRIPS is interpreted and locked in) rather than substantive (what TRIPS requires). The network links trace how each reading structurally influences the others: this reading's lock-in mechanism forecloses alternative readings from re-entering the legal space through new disputes, so this reading influences (but does not foreclose) the siblings — the siblings remain live in non-dispute forums (domestic legislation, bilateral negotiation) but are constrained within WTO dispute channels. The Appellate Body's collapse (a separate constraint story) is the structural condition that makes THIS reading's binding authority absolute; hence this story affects the appellate collapse constraint and vice versa.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, institutional, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
