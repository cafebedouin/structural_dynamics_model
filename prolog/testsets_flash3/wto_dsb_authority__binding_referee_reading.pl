% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__binding_referee_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   human_readable: WTO DSB Authority (Binding Referee Reading)
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   This constraint describes the WTO Dispute Settlement Body's (DSB)
 *   authority as a binding referee, where member states have explicitly
 *   surrendered policy discretion in WTO-covered domains in exchange for a
 *   rules-based trading system. This reading emphasizes the legal
 *   enforceability of DSB rulings and the compliance obligations they impose,
 *   backed by the threat of authorized retaliation. It is one reading of the
 *   broader 'wto_dsb_authority' kernel, distinct from readings that view the
 *   DSB as merely advisory or as an overreaching judicial body.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, 0.65).
domain_priors:suppression_score(wto_dsb_authority__binding_referee_reading, 0.75).
domain_priors:theater_ratio(wto_dsb_authority__binding_referee_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__binding_referee_reading, tangled_rope).
narrative_ontology:human_readable(wto_dsb_authority__binding_referee_reading, "WTO DSB Authority (Binding Referee Reading)").
narrative_ontology:topic_domain(wto_dsb_authority__binding_referee_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__binding_referee_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__binding_referee_reading, 'c1eb3512-6eda-40ca-a745-f00b75004579').
narrative_ontology:cs_kernel_codification('c1eb3512-6eda-40ca-a745-f00b75004579', formalized).
narrative_ontology:cs_authority_grounding('c1eb3512-6eda-40ca-a745-f00b75004579', lineage).
narrative_ontology:cs_interpretation_layer_present('c1eb3512-6eda-40ca-a745-f00b75004579').
narrative_ontology:cs_reading_relation('c1eb3512-6eda-40ca-a745-f00b75004579', wto_dsb_authority__advisory_coordination_reading, influences).
narrative_ontology:cs_reading_relation('c1eb3512-6eda-40ca-a745-f00b75004579', wto_dsb_authority__judicial_activism_reading, coexists_with).
narrative_ontology:cs_axiom('c1eb3512-6eda-40ca-a745-f00b75004579', foundational, dsr_binding_force).
narrative_ontology:cs_axiom_status(dsr_binding_force, holdable).
narrative_ontology:cs_axiom_grounding('c1eb3512-6eda-40ca-a745-f00b75004579', dsr_binding_force, conventional).
narrative_ontology:cs_axiom('c1eb3512-6eda-40ca-a745-f00b75004579', foundational, sovereignty_ceded_for_market_access).
narrative_ontology:cs_axiom_status(sovereignty_ceded_for_market_access, holdable).
narrative_ontology:cs_axiom_grounding('c1eb3512-6eda-40ca-a745-f00b75004579', sovereignty_ceded_for_market_access, conventional).
narrative_ontology:cs_reference_frame('c1eb3512-6eda-40ca-a745-f00b75004579', rules_based_multilateralism).
narrative_ontology:cs_drift_state('c1eb3512-6eda-40ca-a745-f00b75004579', contemporary_us_appellate_body_blockage, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('c1eb3512-6eda-40ca-a745-f00b75004579', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__binding_referee_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, wto_member_states_exporting).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, wto_dispute_settlement_body).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, wto_member_states_importing).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, domestic_industries_targeted_by_rulings).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The institutional body that establishes panels, adopts reports, and authorizes retaliation. It interprets treaty law to issue binding rulings, acting as the ultimate arbiter of trade disputes. Its legitimacy is derived from member states' initial agreement to the DSU.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, wto_dispute_settlement_body, agenda_setter,
    institutional, generational, constrained, global).

% States that benefit from the enforcement of trade rules, ensuring market access for their goods and services. They rely on DSB rulings to challenge protectionist measures by other states and can seek authorization for retaliatory tariffs if rulings are not complied with.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, wto_member_states_exporting, beneficiary,
    institutional, biographical, mobile, global).

% States whose domestic policies are challenged and found inconsistent with WTO agreements. They face compliance obligations, which may require altering domestic laws or regulations, or face authorized retaliation from the complaining party. This entails a surrender of policy discretion.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, wto_member_states_importing, payer,
    institutional, biographical, constrained, global).

% Industries within member states that are directly affected by DSB rulings, often requiring them to adapt to new trade rules, lose subsidies, or face increased competition. They have little direct recourse within the WTO system and rely on their national governments to represent their interests.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, domestic_industries_targeted_by_rulings, payer,
    organized, immediate, trapped, national).

% Analyze the legal implications of DSB rulings, their consistency with international law, and their impact on state sovereignty. They provide critical commentary on the evolution of the WTO's judicial function.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a rules-based mechanism for resolving trade disputes between sovereign states, preventing unilateral trade actions and ensuring a predictable global trading environment based on agreed-upon treaty law.
% TRANSFER_FUNCTION: Transfers policy discretion from individual member states to the WTO's dispute settlement mechanism in covered areas, in exchange for a stable and enforceable global trading system. Non-compliance can lead to authorized economic retaliation.
% ABSENT_VOICES: Domestic political constituencies within member states (e.g., labor unions, environmental groups, specific industries) whose policy preferences might be overridden by WTO rulings. Their voices are mediated through national governments, which may prioritize broader trade interests.
% DISAPPEARANCE_RATIONALE: If the DSB's binding authority vanished, trade disputes would likely revert to power-based negotiations or unilateral retaliatory measures, leading to increased trade friction, uncertainty, and a potential breakdown of the multilateral trading system. Member states would regain full policy discretion but lose the predictability and enforcement of global trade rules.
% FOUNDING_PROBLEM: The lack of an effective, enforceable dispute settlement mechanism in the GATT era, leading to unresolved trade conflicts and a reliance on power politics rather than rules.
% FOUNDING_PROBLEM_CORROBORATION: Most WTO member states, particularly those with smaller economies, continue to attest that a binding dispute settlement mechanism is crucial for a stable, rules-based trading system. International legal bodies and trade organizations also corroborate the ongoing need for such a mechanism to prevent trade wars.
narrative_ontology:disappearance_verdict(wto_dsb_authority__binding_referee_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__binding_referee_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__binding_referee_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(wto_dsb_authority__binding_referee_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__binding_referee_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.65) because member states must alter domestic policies to comply with rulings, representing a significant transfer of policy autonomy. Suppression is also high (0.75) due to the binding nature of rulings and the credible threat of authorized retaliation, which makes non-compliance costly. Theater ratio is low (0.1) as the DSB's function is genuinely to resolve disputes and enforce rules, not primarily for show. Accessibility collapse is high (0.7) because once a ruling is made, the options for the losing party are severely constrained to compliance or retaliation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of exporting states, the DSB is a crucial mechanism for fair trade and market access, a net benefit. From the perspective of importing states facing adverse rulings, it is an extractive mechanism that curtails their sovereign policy space. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The WTO DSB itself, as the institutional arbiter, and exporting member states (who benefit from market access and enforcement) are beneficiaries. Importing member states (whose policies are challenged) and their domestic industries (directly impacted by compliance) are the primary payers. The surrender of sovereignty is a core feature of this reading, leading to high directionality for the targeted states.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extent_of_sovereignty_surrender,
    'To what extent do member states genuinely perceive DSB rulings as a binding surrender of sovereignty versus a pragmatic, reversible policy choice?',
    'Analysis of state behavior post-ruling (e.g., speed and completeness of compliance, frequency of retaliation vs. negotiation), and official statements from national governments and legal scholars.',
    'If perceived as less binding, the effective suppression and extractiveness would be lower, pushing the classification towards a more ''rope-like'' or ''advisory'' function. If perceived as a full surrender, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extent_of_sovereignty_surrender, empirical, 'Ambiguity in member states'' perception of DSB authority.').

omega_variable(
    legitimacy_of_interpretive_drift,
    'Is the DSB''s interpretation of treaty law a legitimate evolution of international jurisprudence or an overreach that creates new obligations not explicitly agreed upon by member states?',
    'Comparative legal analysis of DSB jurisprudence against the text of the DSU and other WTO agreements, and assessment of member state reactions (e.g., attempts to amend the DSU, non-compliance based on claims of overreach).',
    'If deemed an overreach, the ''judicial_activism_reading'' gains strength, potentially reclassifying this constraint as more extractive due to illegitimate authority. If legitimate, the ''binding_referee_reading'' is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_interpretive_drift, conceptual, 'Contestation over the DSB''s interpretive authority and potential judicial activism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__binding_referee_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_dsb_authority__binding_referee_reading, theater_ratio, 1995, 0.05).
narrative_ontology:measurement(wto__tr_t2005, wto_dsb_authority__binding_referee_reading, theater_ratio, 2005, 0.08).
narrative_ontology:measurement(wto__tr_t2015, wto_dsb_authority__binding_referee_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(wto__tr_t2024, wto_dsb_authority__binding_referee_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_dsb_authority__binding_referee_reading, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(wto__be_t2005, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(wto__be_t2015, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement(wto__be_t2024, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_dsb_authority__binding_referee_reading, suppression_requirement, 1995, 0.65).
narrative_ontology:measurement(wto__su_t2005, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(wto__su_t2015, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2015, 0.75).
narrative_ontology:measurement(wto__su_t2024, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2024, 0.75).


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
