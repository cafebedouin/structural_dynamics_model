% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__judicial_activism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__judicial_activism_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: wto_dsb_authority__judicial_activism_reading
 *   human_readable: WTO DSB Authority: Judicial Activism Reading
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   The WTO Dispute Settlement Body (DSB) and its panels interpret trade
 *   agreements and issue binding rulings with compliance obligations. This
 *   constraint story instantiates the judicial activism reading of the
 *   wto_dsb_authority kernel: the claim that panels have exceeded their
 *   treaty mandate through interpretive drift, creating new obligations that
 *   member states never negotiated. The reading views the mechanism not as a
 *   neutral referee but as an illegitimate legislator that extracts policy
 *   autonomy from losing states and domestic regulators, concentrating
 *   interpretive authority in the DSB apparatus. The constraint carries a
 *   genuine coordination functionâpreventing trade warsâbut
 *   asymmetrically extracts sovereignty costs from defendants while the legal
 *   bureaucracy and winning states benefit from enforceable market access.
 *
 * KEY AGENTS:
 *   - dsb_adjudicators: Primary agenda-setter (institutional/constrained) â expands authority through interpretive rulings
 *   - wto_secretariat: Secondary institutional actor (institutional/constrained) â administers and benefits from expanded apparatus
 *   - enforcing_member_states: Primary beneficiary (powerful/constrained) â obtains binding rulings and retaliation rights against trading partners
 *   - losing_member_states: Primary payer (powerful/constrained) â loses policy autonomy and faces retaliation for non-compliance
 *   - domestic_regulators: Secondary payer (organized/constrained) â must implement externally mandated regulatory rollback
 *   - small_member_states: Excluded voice (moderate/constrained) â underrepresented in complex litigation that generates precedential obligations
 *   - trade_law_scholars: Analytical observer (analytical/analytical) â documents textual gaps and interpretive drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, 0.68).
domain_priors:suppression_score(wto_dsb_authority__judicial_activism_reading, 0.72).
domain_priors:theater_ratio(wto_dsb_authority__judicial_activism_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__judicial_activism_reading, tangled_rope).
narrative_ontology:human_readable(wto_dsb_authority__judicial_activism_reading, "WTO DSB Authority: Judicial Activism Reading").
narrative_ontology:topic_domain(wto_dsb_authority__judicial_activism_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__judicial_activism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__judicial_activism_reading, 'c152d33a-11a7-4374-b538-69969b503e4f').
narrative_ontology:cs_kernel_codification('c152d33a-11a7-4374-b538-69969b503e4f', formalized).
narrative_ontology:cs_authority_grounding('c152d33a-11a7-4374-b538-69969b503e4f', extraction).
narrative_ontology:cs_interpretation_layer_present('c152d33a-11a7-4374-b538-69969b503e4f').
narrative_ontology:cs_reading_relation('c152d33a-11a7-4374-b538-69969b503e4f', wto_dsb_authority__binding_referee_reading, influences).
narrative_ontology:cs_reading_relation('c152d33a-11a7-4374-b538-69969b503e4f', wto_dsb_authority__advisory_coordination_reading, coexists_with).
narrative_ontology:cs_axiom('c152d33a-11a7-4374-b538-69969b503e4f', foundational, panel_rulings_exceed_negotiated_text).
narrative_ontology:cs_axiom_status(panel_rulings_exceed_negotiated_text, holdable).
narrative_ontology:cs_axiom_grounding('c152d33a-11a7-4374-b538-69969b503e4f', panel_rulings_exceed_negotiated_text, conventional).
narrative_ontology:cs_axiom('c152d33a-11a7-4374-b538-69969b503e4f', foundational, judicial_legislation_violates_state_sovereignty).
narrative_ontology:cs_axiom_status(judicial_legislation_violates_state_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('c152d33a-11a7-4374-b538-69969b503e4f', judicial_legislation_violates_state_sovereignty, deontological).
narrative_ontology:cs_reference_frame('c152d33a-11a7-4374-b538-69969b503e4f', state_consent_derived_jurisdiction).
narrative_ontology:cs_drift_state('c152d33a-11a7-4374-b538-69969b503e4f', contemporary_appellate_body_crisis, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('c152d33a-11a7-4374-b538-69969b503e4f', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, dsb_adjudicators).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, enforcing_member_states).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, wto_secretariat).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, losing_member_states).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, domestic_regulators).
narrative_ontology:constraint_vindicates(wto_dsb_authority__judicial_activism_reading, dynamic_treaty_interpretation_doctrine).
narrative_ontology:constraint_vindicates(wto_dsb_authority__judicial_activism_reading, judicial_gap_filling_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issue rulings and interpretations of WTO agreements, often filling textual gaps or extending obligations beyond negotiated language. They maintain that effective dispute resolution requires evolutionary interpretation, but are accused of legislating new obligations. Their authority and institutional role expand with each broad interpretation.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, dsb_adjudicators, agenda_setter,
    institutional, generational, constrained, global).

% Provides legal support and logistics for panel proceedings. Administers the dispute settlement machinery and benefits from an expanded apparatus through larger budgets, staff, and institutional prominence as case volumes and interpretive complexity grow.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, wto_secretariat, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__judicial_activism_reading, wto_secretariat, beneficiary).

% Win disputes and obtain authorization to impose retaliatory tariffs on trading partners. They benefit from enforced market access and the leverage of binding rulings, though they remain vulnerable to activist rulings against them in future cases.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, enforcing_member_states, beneficiary,
    powerful, generational, constrained, global).

% Must alter domestic laws and policies to comply with panel rulings that interpret treaty obligations broadly. They bear sovereignty costs and domestic political costs from regulatory rollback, and face authorized retaliation if they do not comply.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, losing_member_states, payer,
    powerful, generational, constrained, global).

% Implement regulatory changes demanded by DSB rulings that go beyond the text of the underlying trade agreements. They experience the constraint as an external override of domestic policy autonomy and statutory mandates.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, domestic_regulators, payer,
    organized, biographical, constrained, national).

% Lack the legal capacity and resources to participate fully in complex dispute settlement proceedings. Their interests are underrepresented when panels create new obligations through expansive interpretation, as they cannot litigate effectively to resist doctrinal expansion.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, small_member_states, excluded,
    moderate, generational, constrained, global).

% Document and analyze the gap between negotiated treaty text and panel rulings. They provide the empirical and textual evidence that the judicial activism reading relies upon to claim interpretive drift.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, trade_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves trade disputes between member states through formalized adjudication, preventing unilateral retaliation and providing a predictable rule-based forum for addressing trade grievances.
% TRANSFER_FUNCTION: Transfers policy autonomy from losing member states and domestic regulators to the dispute settlement mechanism; also transfers interpretive authority from treaty negotiators and member state consent to panel adjudicators who expand obligations through evolutionary interpretation.
% ABSENT_VOICES: Small developing member states with limited litigation capacity are structurally excluded from fully shaping interpretive outcomes; domestic constituencies affected by regulatory rollback are not party to the proceedings and lack standing.
% DISAPPEARANCE_RATIONALE: If the DSB's authority to issue binding rulings with compliance obligations disappeared, member states would revert to unilateral trade measures and bilateral power-based negotiations. The multilateral trade order would fragment, as the enforcement mechanism that holds liberalization commitments in place would collapse.
% FOUNDING_PROBLEM: The GATT era lacked an effective multilateral mechanism to resolve trade disputes, leading to unilateral retaliation, power-based trade conflicts, and erosion of negotiated tariff bindings.
% FOUNDING_PROBLEM_CORROBORATION: Trade historians and GATT-era negotiators corroborate the pre-WTO problem of unilateralism. However, contemporary member state delegations and critical legal scholars outside the benefiting parties attest that the current mechanism has exceeded the mandate required to solve that problem, citing the Appellate Body appointment crisis and explicit member state reservations as evidence.
narrative_ontology:disappearance_verdict(wto_dsb_authority__judicial_activism_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__judicial_activism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__judicial_activism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(wto_dsb_authority__judicial_activism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__judicial_activism_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__judicial_activism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_dsb_authority__judicial_activism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because panel rulings regularly extend obligations beyond negotiated text, transferring legislative authority to adjudicators. Suppression (0.72) is high because the DSU retaliation mechanism actively suppresses non-compliance and unilateral exit. Theater ratio (0.48) reflects the elaborate legal reasoning panels deploy to present obligation-creation as mere interpretation. Accessibility collapse (0.65) is moderate-high: while states can theoretically block appointments or withdraw from WTO, the institutional and economic costs are severe, and alternatives like unilateral retaliation are delegitimized. Resistance (0.78) is high and rising, evidenced by the Appellate Body appointment crisis, US non-compliance in specific cases, and developing-country critiques. The measurement series tracks the progressive hardening of the mechanism from 1995 to 2024 on a single shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (DSB adjudicators and secretariat) experiences the constraint as legitimate evolutionary interpretation necessary for an effective trading system. The payer seats (losing member states and domestic regulators) experience it as an external seizure of legislative authority. The beneficiary seat (enforcing member states) experiences it as a valuable enforcement tool until the moment an activist ruling turns against them. The engine will compute divergent per-seat classifications from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   DSB adjudicators and the WTO secretariat are structural beneficiaries of expanded institutional authority (low d). Enforcing member states are incidentally beneficiaries of specific rulings but remain vulnerable to future activism (moderate-low d). Losing member states and domestic regulators are structural targets: they bear the costs of compliance and sovereignty loss, with constrained exit (high d). Small member states are excluded from the interpretive process entirely, sitting at high d through absence of voice. The high suppression and moderate scope amplify effective extraction for the target seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâGATT-era unilateralismâwas real, and the coordination function of dispute settlement remains live. The mandatrophy risk would be misclassifying the entire mechanism as a snare because of the activism critique. By declaring tangled_rope, the story captures both the genuine coordination (dispute prevention) and the asymmetric extraction (sovereignty costs, judicial legislation). The R5 genealogy interview records that the founding problem status is contested, corroborated by outside scholars and state delegations, which supports the tangled_rope classification over a pure coordination (rope) or pure extraction (snare) reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_drift_vs_entrepreneurship,
    'Does the DSB''s expansion reflect accidental interpretive drift by adjudicators, or intentional institutional entrepreneurship to compensate for legislative gridlock?',
    'Archival analysis of panel deliberations, internal secretariat memoranda, and adjudicator correspondence to distinguish drift from strategic expansion.',
    'If intentional, the constraint is more snare-like (coordination story as cover); if drift, it trends toward piton (atrophied legitimacy, performative maintenance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_drift_vs_entrepreneurship, empirical, 'Ambiguity about whether interpretive expansion is deliberate or drift-driven.').

omega_variable(
    retaliation_authorization_legitimacy,
    'Is DSU Article 22 retaliation authorization a necessary enforcement tool or an illegitimate transfer of coercive power from member states to a judicial body?',
    'Comparative compliance-rate analysis across dispute settlement systems with and without centralized retaliation authorization.',
    'If separable from coordination, the enforcement component is extractive overhead; if inseparable, part of the extraction is the genuine price of dispute resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retaliation_authorization_legitimacy, conceptual, 'Whether retaliation authority is structurally separable from coordination function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__judicial_activism_reading, 0, 29).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto_dsb_ja_tr_t0, wto_dsb_authority__judicial_activism_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(wto_dsb_ja_tr_t5, wto_dsb_authority__judicial_activism_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(wto_dsb_ja_tr_t10, wto_dsb_authority__judicial_activism_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(wto_dsb_ja_tr_t15, wto_dsb_authority__judicial_activism_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(wto_dsb_ja_tr_t20, wto_dsb_authority__judicial_activism_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement(wto_dsb_ja_tr_t29, wto_dsb_authority__judicial_activism_reading, theater_ratio, 29, 0.48).

% Extraction over time
narrative_ontology:measurement(wto_dsb_ja_be_t0, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(wto_dsb_ja_be_t5, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(wto_dsb_ja_be_t10, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(wto_dsb_ja_be_t15, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(wto_dsb_ja_be_t20, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(wto_dsb_ja_be_t29, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 29, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(wto_dsb_ja_su_t0, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(wto_dsb_ja_su_t5, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(wto_dsb_ja_su_t10, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(wto_dsb_ja_su_t15, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(wto_dsb_ja_su_t20, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(wto_dsb_ja_su_t29, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 29, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, binding_referee_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, advisory_coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the wto_dsb_authority kernel, specifically the judicial activism reading. The kernel conflates three structurally distinct claims: the advisory coordination reading (panels as facilitators), the binding referee reading (panels as treaty-bound adjudicators), and this reading (panels as illegitimate legislators). Decomposed per the Îµ-invariance principle because each reading carries a different Îµ, beneficiary structure, and empirical status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
