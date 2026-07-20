% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__binding_referee_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: WTO DSB Binding Referee Authority
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   WTO dispute settlement panels issue rulings that are formally binding on
 *   member states under the Dispute Settlement Understanding (DSU). This
 *   constraint instantiates the binding_referee_reading of the contested
 *   wto_dsb_authority kernel, which holds that member states voluntarily
 *   surrendered policy discretion in exchange for reciprocal market access
 *   guarantees, and that non-compliance constitutes a treaty violation
 *   subject to authorized retaliation. Sibling readings characterize the same
 *   institutional kernel as either non-binding advisory coordination or as
 *   illegitimate judicial activism. The binding reading treats the constraint
 *   as a tangled rope: it supplies genuine coordination (preventing trade
 *   wars, stabilizing expectations) while asymmetrically extracting
 *   regulatory autonomy from losing respondents through actively enforced
 *   compliance obligations.
 *
 * KEY AGENTS:
 *   - complainant_states: Beneficiary (powerful/constrained) â gain market access through binding compliance
 *   - respondent_states: Primary target (powerful/constrained) â lose policy discretion and must reform domestic measures
 *   - domestic_regulated_industries: Secondary target (organized/trapped) â bear the domestic economic costs of compliance
 *   - wto_dsb_panels: Agenda-setter (institutional/analytical) â administer proceedings and authorize retaliation
 *   - civil_society_actors: Excluded voice (moderate/trapped) â affected by rulings but absent from proceedings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, 0.68).
domain_priors:suppression_score(wto_dsb_authority__binding_referee_reading, 0.72).
domain_priors:theater_ratio(wto_dsb_authority__binding_referee_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__binding_referee_reading, tangled_rope).
narrative_ontology:human_readable(wto_dsb_authority__binding_referee_reading, "WTO DSB Binding Referee Authority").
narrative_ontology:topic_domain(wto_dsb_authority__binding_referee_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__binding_referee_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__binding_referee_reading, '75db61e9-1bae-4698-9828-14fc0fd84025').
narrative_ontology:cs_kernel_codification('75db61e9-1bae-4698-9828-14fc0fd84025', formalized).
narrative_ontology:cs_authority_grounding('75db61e9-1bae-4698-9828-14fc0fd84025', lineage).
narrative_ontology:cs_interpretation_layer_present('75db61e9-1bae-4698-9828-14fc0fd84025').
narrative_ontology:cs_reading_relation('75db61e9-1bae-4698-9828-14fc0fd84025', wto_dsb_authority__advisory_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('75db61e9-1bae-4698-9828-14fc0fd84025', wto_dsb_authority__judicial_activism_reading, coexists_with).
narrative_ontology:cs_axiom('75db61e9-1bae-4698-9828-14fc0fd84025', foundational, panel_authority_derives_from_member_consent).
narrative_ontology:cs_axiom_status(panel_authority_derives_from_member_consent, holdable).
narrative_ontology:cs_axiom_grounding('75db61e9-1bae-4698-9828-14fc0fd84025', panel_authority_derives_from_member_consent, conventional).
narrative_ontology:cs_axiom('75db61e9-1bae-4698-9828-14fc0fd84025', foundational, binding_compliance_is_reciprocal_sovereignty_exchange).
narrative_ontology:cs_axiom_status(binding_compliance_is_reciprocal_sovereignty_exchange, holdable).
narrative_ontology:cs_axiom_grounding('75db61e9-1bae-4698-9828-14fc0fd84025', binding_compliance_is_reciprocal_sovereignty_exchange, instrumental).
narrative_ontology:cs_reference_frame('75db61e9-1bae-4698-9828-14fc0fd84025', reciprocal_sovereignty_exchange_framework).
narrative_ontology:cs_drift_state('75db61e9-1bae-4698-9828-14fc0fd84025', post_appellate_body_paralysis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('75db61e9-1bae-4698-9828-14fc0fd84025', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__binding_referee_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, complainant_states).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, exporters_in_complainant_states).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, respondent_states).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, domestic_regulated_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiate disputes to restore market access guaranteed by WTO agreements. When they prevail, they receive binding rulings that force respondent states to remove trade barriers or face authorized retaliation. They benefit from a rules-based mechanism that substitutes for unilateral economic pressure.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, complainant_states, beneficiary,
    powerful, generational, constrained, global).

% Defend domestic measures such as subsidies, health regulations, and trade remedies before DSB panels. When they lose, they must either modify domestic laws and policies or accept retaliatory tariffs. They bear the direct cost of surrendered policy discretion and domestic regulatory autonomy.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, respondent_states, payer,
    powerful, generational, constrained, global).

% Ad hoc adjudicatory bodies composed of trade law experts who interpret WTO covered agreements and issue reports adopted through the DSB. They administer proceedings, assess treaty compliance, and authorize retaliation levels. Their authority derives directly from the Dispute Settlement Understanding.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, wto_dsb_panels, agenda_setter,
    institutional, generational, analytical, global).

% Industries and domestic producers protected by respondent state measures such as anti-dumping duties, subsidies, or sanitary regulations. When their government loses a dispute, these industries face removal of protections or the imposition of retaliatory tariffs on their exports. They have no standing in state-to-state proceedings and cannot independently challenge or exit the ruling.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, domestic_regulated_industries, payer,
    organized, biographical, trapped, national).

% Exporting firms and sectors that gain improved market access when respondent states are compelled to remove trade barriers. They benefit indirectly from their government's successful litigation but do not control the dispute settlement process or its timing.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, exporters_in_complainant_states, beneficiary,
    organized, biographical, constrained, global).

% Labor unions, environmental NGOs, public health advocates, and indigenous groups whose interests are affected by trade rulings on pharmaceutical patents, environmental measures, and labor standards. They are structurally excluded from WTO dispute settlement, which is limited to member state governments, and their concerns are mediated or overridden by trade officials.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, civil_society_actors, excluded,
    moderate, biographical, trapped, global).

% Academic and practicing international trade lawyers who analyze panel reports, track compliance patterns, and debate the legitimacy of interpretations. They provide external assessment of whether rulings are treaty-grounded or represent judicial overreach.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, trade_law_scholars, observer,
    analytical, generational, analytical, global).

narrative_ontology:fixing_cost_class(wto_dsb_authority__binding_referee_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a rules-based mechanism to resolve international trade disputes without unilateral retaliation, creating predictable expectations about market access commitments and stabilizing the multilateral trading system.
% TRANSFER_FUNCTION: Moves policy discretion and regulatory autonomy from respondent states to the collective enforcement mechanism; transfers market access gains to complainant states and their export sectors when respondents are compelled to comply.
% ABSENT_VOICES: Domestic civil society organizations, labor unions, environmental and public health advocates, and indigenous communities within both complainant and respondent states are structurally excluded from the state-to-state proceedings; their interests are mediated by trade ministries or ignored entirely.
% DISAPPEARANCE_RATIONALE: Without binding DSB rulings, the WTO would revert to the pre-1995 GATT model of diplomatic bargaining and unilateral retaliation. Trade disputes would be resolved by economic power rather than legal rules, bilateral deals would replace multilateral discipline, and the institutional architecture of global trade governance would reorganize fundamentally.
% FOUNDING_PROBLEM: The pre-1995 GATT dispute settlement system lacked effective enforcement, allowing powerful trading nations to block adoption of panel reports and ignore rulings with impunity, leading to unilateral trade measures and protectionist retaliation.
% FOUNDING_PROBLEM_CORROBORATION: Trade historians and economists outside the WTO Secretariat attest to the pre-1995 enforcement gap. However, developing country negotiators, sovereignty advocates, and critical legal scholars contest whether the binding dispute settlement solution was a necessary fix or a mechanism that privileges export interests over regulatory autonomy; no consensus exists outside the beneficiary set.
narrative_ontology:disappearance_verdict(wto_dsb_authority__binding_referee_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__binding_referee_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__binding_referee_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(wto_dsb_authority__binding_referee_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__binding_referee_reading, 0.68, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.68) reflects the substantial cost to respondent states of surrendering regulatory autonomy, though the system is reciprocal over time. Suppression (0.72) captures the active enforcement mechanism: retaliation authorization and the prohibition on unilateral measures collapse alternatives for respondents. Accessibility_collapse (0.75) is high because legally permissible alternatives to compliance (unilateral retaliation, treaty withdrawal) are extremely costly. Resistance (0.60) reflects the Appellate Body crisis, persistent non-compliance by major economies, and ongoing sovereignty critiques. Theater_ratio (0.30) is moderate: the legal proceedings are functional but involve extensive procedural performance that partially masks power-political bargaining. The measurement series show rising extraction and theater through the system's maturation, with suppression requirement peaking around 2015 and moderating as the Appellate Body crisis eroded enforcement capacity after 2019.
 *
 * PERSPECTIVAL GAP:
 *   From the complainant seat, the constraint appears as legitimate rope â a necessary enforcement mechanism that prevents cheating on trade commitments. From the respondent seat, the same constraint appears as extractive â an external tribunal overriding democratic regulatory choices. The engine computes this divergence from the structural data: identical power atoms (powerful states) experience opposite directionalities depending on whether they initiate or defend disputes. Domestic industries experience higher effective extraction than their governments because they are trapped (no exit) while states are merely constrained (can withdraw from WTO, though at high cost).
 *
 * DIRECTIONALITY LOGIC:
 *   Complainant states and their exporters are structural beneficiaries (low d): they activate the constraint to gain market access. Respondent states and domestic industries are structural targets (high d): they bear the compliance costs and loss of autonomy. The panels themselves sit near symmetric (moderate d): they neither collect the gains nor pay the costs, but administer the extraction. Civil society is excluded entirely (no d computation).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the system as pure extraction (snare) because the constraint is actively used by all members, including developing countries, to challenge powerful trading partners â the coordination function is not merely cover. It prevents mislabeling as pure rope because the compliance obligation is coercively enforced through retaliation, and respondents demonstrably lose policy autonomy. The R5 genealogy shows the founding problem (weak pre-1995 enforcement) is contested, with corroboration from outside the beneficiary set, preventing a flattering origin myth from masking current extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binding_vs_advisory_kernel_ambiguity,
    'Does the DSB''s authority to issue binding rulings reflect the original political bargain of the Uruguay Round, or has the binding character exceeded the consensual foundation?',
    'Archival analysis of Uruguay Round negotiating records on the DSU, combined with subsequent state practice, reservations, and ministerial statements.',
    'If the binding character exceeded the original bargain, the constraint shifts toward snare as an illegitimate transfer of sovereignty; if it reflects genuine consent, tangled_rope remains appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binding_vs_advisory_kernel_ambiguity, conceptual, 'Ambiguity about whether binding DSB authority was originally consented to').

omega_variable(
    power_asymmetry_in_compliance,
    'Is compliance with DSB rulings driven by the normative legitimacy of the dispute settlement process, or by the coercive threat of retaliation from economically powerful complainants?',
    'Statistical analysis of compliance rates across asymmetric power dyads, controlling for legal merit and domestic political economy.',
    'If compliance is primarily coerced by power asymmetry, the coordination function is weaker and extraction dominates for weak respondents; if legitimacy-driven, the reciprocal coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(power_asymmetry_in_compliance, empirical, 'Whether compliance is legitimacy-based or coercion-based').

omega_variable(
    appellate_body_crisis_significance,
    'Does the paralysis of the Appellate Body since 2019 represent a temporary institutional malfunction or a terminal rejection of the binding referee framework by major powers?',
    'Observation of whether the Appellate Body is restored, whether the MPIA becomes permanent, or whether the system reverts to panel-only or diplomatic settlement.',
    'If terminal, the constraint is drifting toward piton (theatrical maintenance of a defunct binding ideal) or scaffold (transitional workaround); if restored, the tangled_rope classification persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appellate_body_crisis_significance, empirical, 'Whether the AB crisis is temporary or terminal for the binding model').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__binding_referee_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t0, wto_dsb_authority__binding_referee_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(wto__tr_t5, wto_dsb_authority__binding_referee_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(wto__tr_t10, wto_dsb_authority__binding_referee_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(wto__tr_t15, wto_dsb_authority__binding_referee_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(wto__tr_t20, wto_dsb_authority__binding_referee_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(wto__tr_t25, wto_dsb_authority__binding_referee_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement(wto__tr_t30, wto_dsb_authority__binding_referee_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(wto__be_t0, wto_dsb_authority__binding_referee_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(wto__be_t5, wto_dsb_authority__binding_referee_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(wto__be_t10, wto_dsb_authority__binding_referee_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(wto__be_t15, wto_dsb_authority__binding_referee_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(wto__be_t20, wto_dsb_authority__binding_referee_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(wto__be_t25, wto_dsb_authority__binding_referee_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(wto__be_t30, wto_dsb_authority__binding_referee_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t0, wto_dsb_authority__binding_referee_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(wto__su_t5, wto_dsb_authority__binding_referee_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(wto__su_t10, wto_dsb_authority__binding_referee_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(wto__su_t15, wto_dsb_authority__binding_referee_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(wto__su_t20, wto_dsb_authority__binding_referee_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(wto__su_t25, wto_dsb_authority__binding_referee_reading, suppression_requirement, 25, 0.68).
narrative_ontology:measurement(wto__su_t30, wto_dsb_authority__binding_referee_reading, suppression_requirement, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__binding_referee_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
