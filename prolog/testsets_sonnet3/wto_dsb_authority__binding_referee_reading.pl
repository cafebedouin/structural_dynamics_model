% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__binding_referee_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__binding_referee_reading, []).

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
 *   constraint_id: wto_dsb_authority__binding_referee_reading
 *   human_readable: WTO Dispute Settlement Body — Binding Referee Reading
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   Under the binding referee reading, DSB rulings are treaty law, not
 *   advice: a losing member must bring its measure into compliance or face
 *   authorized retaliation, and the treaty text itself is read as the
 *   instrument through which member states surrendered unilateral policy
 *   discretion within WTO-covered domains in exchange for market access and
 *   enforceable reciprocity. This story is one of three readings of the same
 *   kernel (wto_dsb_authority). The advisory_coordination_reading treats
 *   panel output as expert facilitation preserving full policy discretion — a
 *   structurally different, much lower-extraction constraint. The
 *   judicial_activism_reading treats the same rulings as illegitimate
 *   interpretive overreach beyond treaty mandate — a structurally different,
 *   higher-suppression, legitimacy-contested constraint. This file authors
 *   ONLY the binding-referee reading's own ε, beneficiary/victim structure,
 *   and metrics, without averaging across or describing the sibling readings
 *   within the classification logic itself.
 *
 * KEY AGENTS:
 *   - wto_secretariat_and_appellate_apparatus: institutional agenda_setter administering binding panel process
 *   - export_oriented_member_states: primary structural beneficiary via enforceable market access
 *   - domestic_regulatory_constituencies: payer bearing compliance costs on domestically enacted policy
 *   - smaller_developing_members_facing_retaliation_asymmetry: payer with formally equal but practically weaker retaliation leverage
 *   - national_legislatures: excluded from proceedings that override their enactments
 *   - trade_law_scholars: analytical observers of interpretive drift and legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, 0.52).
domain_priors:suppression_score(wto_dsb_authority__binding_referee_reading, 0.58).
domain_priors:theater_ratio(wto_dsb_authority__binding_referee_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__binding_referee_reading, tangled_rope).
narrative_ontology:human_readable(wto_dsb_authority__binding_referee_reading, "WTO Dispute Settlement Body — Binding Referee Reading").
narrative_ontology:topic_domain(wto_dsb_authority__binding_referee_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__binding_referee_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__binding_referee_reading, '8528df04-9a15-4aba-8f86-937bcc17ede5').
narrative_ontology:cs_kernel_codification('8528df04-9a15-4aba-8f86-937bcc17ede5', formalized).
narrative_ontology:cs_authority_grounding('8528df04-9a15-4aba-8f86-937bcc17ede5', lineage).
narrative_ontology:cs_interpretation_layer_present('8528df04-9a15-4aba-8f86-937bcc17ede5').
narrative_ontology:cs_reading_relation('8528df04-9a15-4aba-8f86-937bcc17ede5', wto_dsb_authority__advisory_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('8528df04-9a15-4aba-8f86-937bcc17ede5', wto_dsb_authority__judicial_activism_reading, coexists_with).
narrative_ontology:cs_axiom('8528df04-9a15-4aba-8f86-937bcc17ede5', foundational, member_states_surrendered_discretion_via_ratification).
narrative_ontology:cs_axiom_status(member_states_surrendered_discretion_via_ratification, holdable).
narrative_ontology:cs_axiom_grounding('8528df04-9a15-4aba-8f86-937bcc17ede5', member_states_surrendered_discretion_via_ratification, conventional).
narrative_ontology:cs_axiom('8528df04-9a15-4aba-8f86-937bcc17ede5', foundational, panel_rulings_constitute_treaty_law_not_advice).
narrative_ontology:cs_axiom_status(panel_rulings_constitute_treaty_law_not_advice, holdable).
narrative_ontology:cs_axiom_grounding('8528df04-9a15-4aba-8f86-937bcc17ede5', panel_rulings_constitute_treaty_law_not_advice, conventional).
narrative_ontology:cs_axiom('8528df04-9a15-4aba-8f86-937bcc17ede5', secondary, retaliation_authorization_is_legitimate_enforcement_not_overreach).
narrative_ontology:cs_axiom_status(retaliation_authorization_is_legitimate_enforcement_not_overreach, holdable).
narrative_ontology:cs_axiom_grounding('8528df04-9a15-4aba-8f86-937bcc17ede5', retaliation_authorization_is_legitimate_enforcement_not_overreach, instrumental).
narrative_ontology:cs_reference_frame('8528df04-9a15-4aba-8f86-937bcc17ede5', uruguay_round_negotiated_bargain).
narrative_ontology:cs_drift_state('8528df04-9a15-4aba-8f86-937bcc17ede5', post_appellate_body_paralysis_2024, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('8528df04-9a15-4aba-8f86-937bcc17ede5', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__binding_referee_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, export_oriented_member_states).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, multinational_exporters).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, wto_secretariat_and_appellate_apparatus).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, domestic_regulatory_constituencies).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, smaller_developing_members_facing_retaliation_asymmetry).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, import_sensitive_domestic_industries).
narrative_ontology:constraint_vindicates(wto_dsb_authority__binding_referee_reading, treaty_supremacy_over_unilateral_policy_discretion).
narrative_ontology:constraint_vindicates(wto_dsb_authority__binding_referee_reading, rules_based_trading_system_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers panel composition, procedural timelines, and the legal reasoning apparatus that produces binding rulings. Has no direct revenue stake but its institutional authority and continued relevance depend on rulings being treated as binding rather than advisory. Cannot be exited by member states without withdrawing from the treaty system entirely.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, wto_secretariat_and_appellate_apparatus, agenda_setter,
    institutional, generational, analytical, global).

% Large exporting economies use the binding ruling mechanism to force open foreign markets and challenge protectionist measures against their goods. They have the legal capacity and retaliation leverage to make rulings bite against smaller trading partners, and can absorb the occasional adverse ruling against themselves without major domestic disruption.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, export_oriented_member_states, beneficiary,
    powerful, generational, mobile, global).

% Benefit from predictable, judicially enforceable market access rules that let them plan cross-border supply chains without depending on ad hoc political goodwill. Lobby governments to bring cases on their behalf and rarely bear the compliance costs directly.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, multinational_exporters, beneficiary,
    organized, biographical, mobile, global).

% Public health, environmental, and labor regulators whose domestic measures get challenged and struck down as disguised trade barriers. They must retool or repeal duly enacted domestic policy to avoid authorized retaliation, even where the policy reflects legitimate domestic democratic preference rather than protectionism.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, domestic_regulatory_constituencies, payer,
    moderate, biographical, trapped, national).

% Technically hold the same right to win rulings and authorize retaliation as large economies, but retaliating against a major trading partner by raising tariffs on its own imports is often economically self-destructive for a small economy. Winning a ruling frequently does not translate into real leverage; losing one means genuine binding compliance pressure with no comparable retaliatory counterweight.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, smaller_developing_members_facing_retaliation_asymmetry, payer,
    powerless, biographical, trapped, national).

% Domestic industries protected by tariffs or subsidies found treaty-inconsistent must absorb foreign competition once compliance is enforced, often with little transition support. Cannot appeal to national political process to preserve the measure once a binding ruling and authorized retaliation are in play.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, import_sensitive_domestic_industries, payer,
    moderate, biographical, constrained, national).

% Enacted the underlying domestic statutes now found treaty-inconsistent, but have no seat at the panel proceedings and only an after-the-fact choice between repeal, compensation, or accepting authorized retaliation. Their democratic mandate for the challenged policy is not itself a defense within the dispute settlement process.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, national_legislatures, excluded,
    institutional, generational, constrained, national).

% Study whether the binding-ruling structure functions as originally negotiated or has drifted into something the treaty text does not clearly authorize; produce competing analyses used by all other parties to justify their positions.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, trade_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_dsb_authority__binding_referee_reading, diffuse).
narrative_ontology:fixing_cost_class(wto_dsb_authority__binding_referee_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, rules-based forum where trading disputes are resolved through adjudication rather than unilateral retaliation or power politics, giving all members — especially smaller ones — a nominally equal legal avenue to challenge market-access violations.
% TRANSFER_FUNCTION: Moves policy discretion from individual member states' domestic political processes to a centralized treaty-interpreting body, and moves market access and compliance costs from the winning party's trading partners to whichever party is found treaty-inconsistent — disproportionately from states with weaker retaliation leverage to those with stronger export interests.
% ABSENT_VOICES: Domestic legislatures and the constituencies whose policies are challenged have no standing in panel proceedings; affected workers, patients, and communities whose protective regulations are struck down are represented, if at all, only through their government's litigation strategy, not directly.
% DISAPPEARANCE_RATIONALE: If binding DSB authority disappeared overnight, trade disputes would revert to unilateral retaliation, bilateral negotiation, and power-based settlement; large economies would regain unchecked leverage over smaller ones, multinational exporters would lose predictable market-access enforcement, and many currently-compliant domestic regulations would no longer face binding external review.
% FOUNDING_PROBLEM: Pre-WTO trade dispute resolution under GATT relied on consensus-blockable panel reports and unilateral retaliation (e.g., Section 301-style actions), which let powerful states act as judge and enforcer in their own disputes and left weaker states with no reliable recourse.
% FOUNDING_PROBLEM_CORROBORATION: Trade law scholars and WTO Secretariat officials attest the binding mechanism still solves the original enforcement-asymmetry problem for cases smaller states can afford to bring. Developing-country trade ministries and independent studies of retaliation-authorization outcomes attest that binding authority now often reproduces the power asymmetry it was built to fix, because retaliation capacity itself is unevenly distributed — corroboration exists on both sides, from outside the direct beneficiary set (export-oriented major economies).
narrative_ontology:disappearance_verdict(wto_dsb_authority__binding_referee_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__binding_referee_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__binding_referee_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(wto_dsb_authority__binding_referee_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__binding_referee_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__binding_referee_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_dsb_authority__binding_referee_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.52) reflects genuine, real transfer of policy discretion and compliance cost from the losing party to the treaty regime's collective enforcement logic, but is moderate rather than extreme because the mechanism does provide real coordination value (predictable rules over unilateral power politics) and the ruling process is reciprocal in form even where asymmetric in practice. Suppression (0.58) is higher than extraction because the binding character of the obligation — retaliation-authorized, not merely recommended — forecloses domestic policy alternatives once a ruling issues, regardless of domestic democratic preference. Accessibility collapse (0.62) is moderate-high: once a panel ruling issues, the losing state's practical alternatives (comply, compensate, or accept retaliation) collapse sharply, though withdrawal from the treaty system remains a theoretical (rarely exercised) exit. Resistance (0.55) reflects sustained pushback from developing-country delegations and domestic constituencies whose regulations are struck down, alongside the ongoing crisis in the Appellate Body's functioning (the US blocking appointments since 2019) as a direct resistance mechanism against binding authority.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat, binding authority is the treaty functioning exactly as negotiated — the mechanism that replaced unilateral trade war with adjudicated rules. From the payer seats, particularly smaller developing members, the same binding rulings can operate as one-way compliance pressure decoupled from real reciprocal leverage. The engine computes these divergent seat classifications from the declared power/exit/scope data; this story does not pre-resolve which seat's experience is 'correct.'
 *
 * DIRECTIONALITY LOGIC:
 *   Export-oriented member states and multinational exporters sit near the beneficiary end: they gain enforceable market access and rarely bear compliance costs directly. Domestic regulatory constituencies, import-sensitive industries, and especially smaller developing members sit near the target end: they bear compliance costs or retaliation exposure with comparatively less capacity to make retaliation authorization bite in return. The wto_secretariat_and_appellate_apparatus is coded as agenda_setter with analytical exit — it administers the binding mechanism but does not itself collect economic rents, which is why an override is not needed there; its incentive is institutional survival of the binding-authority framing itself, not direct extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unilateral retaliation and consensus-blockable panels under GATT — is genuinely addressed by binding referee authority for a real subset of disputes, particularly among comparably-resourced trading partners; this is not mere mandatrophy theater. But for smaller members whose formal right to retaliate rarely converts to real leverage, the binding mechanism risks functioning as compliance pressure without matching corrective capacity — a partial mandatrophy specific to the power-asymmetric tail of cases rather than the system as a whole. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (predictable dispute resolution favored by most members, including many developing ones, over pure power politics) while still naming the asymmetric extraction that the same binding structure produces for weaker payers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_surrender_scope_ambiguity,
    'Did member states, in ratifying the WTO agreements, actually surrender policy discretion within covered domains as a matter of treaty law, or did they retain discretion subject only to a compensation/retaliation cost — i.e., is compliance genuinely obligatory or merely priced?',
    'Close textual and travaux préparatoires analysis of the Dispute Settlement Understanding, compared against actual state practice patterns (compliance rates vs. compensation/retaliation acceptance rates across dispute history).',
    'If discretion was genuinely surrendered, binding_referee_reading is the structurally accurate account and judicial_activism_reading is largely a rhetorical objection to a correctly-functioning treaty mechanism. If discretion was retained subject to a price, the advisory_coordination_reading''s lower-extraction framing is closer to the treaty''s actual design and this story''s ε may be authored too high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_surrender_scope_ambiguity, conceptual, 'Whether WTO membership entails genuine surrender of policy discretion or merely a priced option to deviate.').

omega_variable(
    retaliation_leverage_asymmetry_naturalness,
    'Is the unevenness of retaliation leverage across member states an inherent, unavoidable feature of any reciprocity-based enforcement system, or a constructed asymmetry that could be corrected by design (e.g., collective retaliation authorization, monetary compensation defaults)?',
    'Comparative institutional analysis of proposed DSU reforms (collective retaliation, cross-retaliation rules, compensation-first remedies) and their modeled effects on smaller-member leverage.',
    'If the asymmetry is inherent to any bilateral-retaliation-based system, the extraction borne by smaller members is closer to an irreducible coordination cost. If it is a corrigible design choice not yet corrected, the same extraction is better read as avoidable and the tangled_rope classification''s extraction component should be weighted more heavily relative to its coordination component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retaliation_leverage_asymmetry_naturalness, empirical, 'Whether retaliation-leverage asymmetry is structurally inherent or a correctable design flaw.').

omega_variable(
    appellate_body_paralysis_effect_on_binding_character,
    'Since the US blocked Appellate Body appointments starting in 2019, rulings can be appealed ''into the void,'' leaving many panel reports unenforceable in practice — does this mean the binding_referee_reading no longer describes the system''s actual operation for a growing share of disputes, sliding it toward advisory_coordination_reading in practice even though the legal text is unchanged?',
    'Track the proportion of panel rulings appealed into the void versus those reaching final binding status and enforced compliance/retaliation, over the 2019–2024 period.',
    'A high and rising void-appeal rate would suggest the binding_referee_reading''s ε is becoming descriptively less accurate over time even as the treaty text remains unchanged — supporting the observed dip in suppression_requirement measured at 2020 in this story''s temporal series.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(appellate_body_paralysis_effect_on_binding_character, empirical, 'Whether Appellate Body paralysis is functionally converting binding rulings into non-binding recommendations for a subset of disputes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__binding_referee_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_dsb_authority__binding_referee_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(wto__tr_t2000, wto_dsb_authority__binding_referee_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(wto__tr_t2005, wto_dsb_authority__binding_referee_reading, theater_ratio, 2005, 0.14).
narrative_ontology:measurement(wto__tr_t2010, wto_dsb_authority__binding_referee_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(wto__tr_t2016, wto_dsb_authority__binding_referee_reading, theater_ratio, 2016, 0.22).
narrative_ontology:measurement(wto__tr_t2020, wto_dsb_authority__binding_referee_reading, theater_ratio, 2020, 0.3).
narrative_ontology:measurement(wto__tr_t2024, wto_dsb_authority__binding_referee_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_dsb_authority__binding_referee_reading, base_extractiveness, 1995, 0.32).
narrative_ontology:measurement(wto__be_t2000, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(wto__be_t2005, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2005, 0.43).
narrative_ontology:measurement(wto__be_t2010, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2010, 0.46).
narrative_ontology:measurement(wto__be_t2016, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2016, 0.49).
narrative_ontology:measurement(wto__be_t2020, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2020, 0.51).
narrative_ontology:measurement(wto__be_t2024, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2024, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_dsb_authority__binding_referee_reading, suppression_requirement, 1995, 0.4).
narrative_ontology:measurement(wto__su_t2000, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(wto__su_t2005, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2005, 0.5).
narrative_ontology:measurement(wto__su_t2010, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2010, 0.53).
narrative_ontology:measurement(wto__su_t2016, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2016, 0.56).
narrative_ontology:measurement(wto__su_t2020, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2020, 0.35).
narrative_ontology:measurement(wto__su_t2024, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__binding_referee_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_dsb_authority__advisory_coordination_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_dsb_authority__judicial_activism_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language label 'WTO DSB authority,' per the ε-invariance principle. advisory_coordination_reading authors a low-ε rope/scaffold-leaning account (panels as facilitators, discretion retained); binding_referee_reading (this file) authors a moderate-ε tangled_rope account (discretion surrendered, retaliation-backed compliance, real coordination value alongside real asymmetric extraction); judicial_activism_reading authors a higher-suppression, legitimacy-contested account (panels exceeding mandate). All three share the same underlying treaty text and dispute history but are structurally distinct constraints because they differ in what they claim actually happened to member state discretion — this satisfies the ε-invariance disambiguation rule rather than forcing one story to carry an observer-relative ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
