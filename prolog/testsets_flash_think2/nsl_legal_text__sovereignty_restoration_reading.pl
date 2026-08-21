% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__sovereignty_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__sovereignty_restoration_reading, []).

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
 *   constraint_id: nsl_legal_text__sovereignty_restoration_reading
 *   human_readable: NSL as Sovereign Security Instrument (Sovereignty Restoration Reading)
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   This constraint story instantiates the 'sovereignty_restoration_reading'
 *   of the National Security Law (NSL) kernel. From this perspective, the NSL
 *   is a legitimate and necessary instrument for the central government to
 *   restore constitutional order and national security following the 2019
 *   unrest. It is framed as a response to destabilizing forces, aiming to
 *   bring stability and ensure the region's integration within the national
 *   framework. The claimed type is 'rope' because this reading asserts a
 *   genuine coordination function (restoring order) and benefits for loyalist
 *   citizens, despite the metrics indicating substantial extraction and
 *   suppression for political opposition.
 *
 * KEY AGENTS:
 *   - central_government_authorities: Primary agenda_setter (institutional/arbitrage) — enforces NSL, claims legitimacy.
 *   - loyalist_citizens: Primary beneficiary (moderate/mobile) — perceives increased stability.
 *   - pro_democracy_activists: Primary target/payer (powerless/trapped) — faces criminalization and suppression.
 *   - political_opposition: Payer (organized/constrained) — faces legal restrictions and reduced political space.
 *   - common_law_judiciary: Payer (institutional/constrained) — must apply NSL, potentially eroding autonomy.
 *   - international_observers: Analytical observer (analytical/analytical) — monitors human rights and rule of law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, 0.52).
domain_priors:suppression_score(nsl_legal_text__sovereignty_restoration_reading, 0.8).
domain_priors:theater_ratio(nsl_legal_text__sovereignty_restoration_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__sovereignty_restoration_reading, rope).
narrative_ontology:human_readable(nsl_legal_text__sovereignty_restoration_reading, "NSL as Sovereign Security Instrument (Sovereignty Restoration Reading)").
narrative_ontology:topic_domain(nsl_legal_text__sovereignty_restoration_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__sovereignty_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__sovereignty_restoration_reading, 'c22adc65-d52e-4ce4-888c-ac1fa260f756').
narrative_ontology:cs_kernel_codification('c22adc65-d52e-4ce4-888c-ac1fa260f756', fixed_text).
narrative_ontology:cs_authority_grounding('c22adc65-d52e-4ce4-888c-ac1fa260f756', lineage).
narrative_ontology:cs_interpretation_layer_present('c22adc65-d52e-4ce4-888c-ac1fa260f756').
narrative_ontology:cs_reading_relation('c22adc65-d52e-4ce4-888c-ac1fa260f756', nsl_legal_text__democratic_enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('c22adc65-d52e-4ce4-888c-ac1fa260f756', nsl_legal_text__jurisdictional_capture_reading, coexists_with).
narrative_ontology:cs_axiom('c22adc65-d52e-4ce4-888c-ac1fa260f756', foundational, national_sovereignty_is_paramount).
narrative_ontology:cs_axiom_status(national_sovereignty_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('c22adc65-d52e-4ce4-888c-ac1fa260f756', national_sovereignty_is_paramount, deontological).
narrative_ontology:cs_axiom('c22adc65-d52e-4ce4-888c-ac1fa260f756', secondary, security_trumps_local_autonomy).
narrative_ontology:cs_axiom_status(security_trumps_local_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('c22adc65-d52e-4ce4-888c-ac1fa260f756', security_trumps_local_autonomy, conventional).
narrative_ontology:cs_reference_frame('c22adc65-d52e-4ce4-888c-ac1fa260f756', post_unrest_constitutional_order).
narrative_ontology:cs_drift_state('c22adc65-d52e-4ce4-888c-ac1fa260f756', contemporary_implementation, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c22adc65-d52e-4ce4-888c-ac1fa260f756', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, central_government_authorities).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, loyalist_citizens).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, pro_democracy_activists).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, political_opposition).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, common_law_judiciary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces the National Security Law (NSL), claiming it restores constitutional order and national security. Benefits from enhanced control and perceived legitimacy.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, central_government_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Perceive increased stability, security, and a return to order, aligning with the government's narrative. They benefit from a reduction in visible unrest.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, loyalist_citizens, beneficiary,
    moderate, biographical, mobile, national).

% Directly targeted by the NSL, facing arrest, prosecution, and severe restrictions on freedom of expression and assembly. Their dissent is criminalized as a security threat.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, pro_democracy_activists, payer,
    powerless, immediate, trapped, local).

% Face legal restrictions on their activities, reduced political space, and the risk of being labeled as threats to national security. Their ability to challenge the government is severely curtailed.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, political_opposition, payer,
    organized, biographical, constrained, national).

% Must interpret and apply the NSL, which introduces mainland legal principles and potentially erodes the autonomy and distinctiveness of the common law system. They bear the burden of reconciling conflicting legal traditions.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, common_law_judiciary, payer,
    institutional, generational, constrained, local).

% Monitor the implementation of the NSL, assessing its impact on human rights, rule of law, and international agreements. Their analysis often contrasts with the official narrative.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, international_observers, observer,
    analytical, immediate, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__sovereignty_restoration_reading, central_government_authorities).
narrative_ontology:fixing_cost_class(nsl_legal_text__sovereignty_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To re-establish public order, national security, and constitutional governance after periods of perceived unrest and threats to sovereignty, ensuring stability and central authority.
% TRANSFER_FUNCTION: Transfers legal authority and enforcement power from local institutions to central government agencies, and transfers the burden of maintaining 'order' onto political opposition and activists through legal and coercive means.
% ABSENT_VOICES: Those advocating for greater autonomy, democratic freedoms, and the preservation of distinct legal traditions are structurally silenced or criminalized. They would object to the NSL's broad powers and its impact on civil liberties.
% DISAPPEARANCE_RATIONALE: If the NSL vanished overnight, the political landscape would immediately shift. Calls for democratic reform and potentially a resurgence of public protests would likely occur, and the central government's control over the region would be significantly challenged, leading to a reorganization of power dynamics.
% FOUNDING_PROBLEM: Widespread civil unrest, protests, and perceived threats to national sovereignty and constitutional order following the 2019 events, which were framed as destabilizing and secessionist.
% FOUNDING_PROBLEM_CORROBORATION: Central government statements, state media, and loyalist political figures consistently attest to the ongoing necessity of the NSL to maintain stability and counter residual threats. Independent international legal bodies and human rights organizations dispute this, citing a decline in civil liberties and political freedoms, but this reading prioritizes the government's perspective on the problem's status.
narrative_ontology:disappearance_verdict(nsl_legal_text__sovereignty_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__sovereignty_restoration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__sovereignty_restoration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(nsl_legal_text__sovereignty_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__sovereignty_restoration_reading, 0.52, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.52) is moderate because, from this reading's perspective, the law primarily targets specific political opposition and activities deemed subversive, rather than imposing broad economic extraction on the general population. Suppression (0.80) is high due to the active enforcement mechanisms, including arrests, prosecutions, and restrictions on civil liberties, which are seen as necessary to maintain order. The theater ratio (0.15) is low, reflecting the view that the NSL is a genuine and functional security instrument, not merely performative. Accessibility collapse (0.60) is moderate as political alternatives are significantly curtailed, and resistance (0.50) is moderate, indicating ongoing but suppressed opposition.
 *
 * PERSPECTIVAL GAP:
 *   The central government and loyalist citizens experience the NSL as a beneficial coordination mechanism that restores stability. In contrast, pro-democracy activists, political opposition, and the common law judiciary experience it as a highly extractive and suppressive force. The engine's classification will highlight this divergence, showing how a claimed 'rope' operates with high extraction and suppression from the perspective of those targeted.
 *
 * DIRECTIONALITY LOGIC:
 *   Central government authorities are clear beneficiaries (d=0.0-0.1) as they gain control and legitimacy. Loyalist citizens also benefit from perceived stability (d=0.2-0.3). Pro-democracy activists and political opposition are clear targets (d=0.8-1.0) as they bear the direct costs of suppression and criminalization. The common law judiciary is also a target (d=0.7-0.8) as its autonomy is constrained. International observers are analytical (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading asserts the NSL's mandate is live, directly addressing the 'founding problem' of unrest. The classification prevents mislabeling it as a 'piton' by emphasizing its active enforcement and perceived functional role in restoring order, even if other readings would classify it differently. The 'live' status of the founding problem, combined with the 'world_rearranges' disappearance verdict, supports the claim of ongoing necessity from this perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_order_definition_ambiguity,
    'Does the NSL genuinely restore a pre-existing constitutional order, or does it impose a new interpretation of ''order'' that fundamentally alters the constitutional framework?',
    'Comparative legal analysis of pre- and post-NSL constitutional practice, focusing on judicial independence, legislative powers, and civil liberties, assessed by independent constitutional scholars.',
    'If it imposes a new order, the ''restoration'' claim is a cover for structural change, potentially shifting the constraint''s classification towards a ''snare'' or ''tangled_rope'' by revealing a deeper extractive function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_order_definition_ambiguity, conceptual, 'Ambiguity in the definition of ''constitutional order'' being restored.').

omega_variable(
    nsl_effectiveness_vs_suppression,
    'Is the NSL''s primary effect the restoration of genuine stability and security, or is it primarily the suppression of legitimate political dissent, with stability as a side effect of coercion?',
    'Longitudinal study of public sentiment, economic indicators, and independent assessments of human rights conditions, distinguishing between genuine public safety improvements and the silencing of opposition.',
    'If primarily suppression, the ''security instrument'' framing is a cover, increasing the effective extractiveness and suppression, pushing the classification towards ''snare'' by revealing victims whose ''security'' is being extracted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nsl_effectiveness_vs_suppression, empirical, 'Distinguishing genuine security restoration from mere suppression of dissent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__sovereignty_restoration_reading, 2019, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t2019, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2019, 0.1).
narrative_ontology:measurement(nsl__tr_t2020, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2020, 0.12).
narrative_ontology:measurement(nsl__tr_t2021, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2021, 0.13).
narrative_ontology:measurement(nsl__tr_t2022, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2022, 0.14).
narrative_ontology:measurement(nsl__tr_t2023, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2023, 0.15).
narrative_ontology:measurement(nsl__tr_t2024, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(nsl__be_t2019, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2019, 0.35).
narrative_ontology:measurement(nsl__be_t2020, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2020, 0.4).
narrative_ontology:measurement(nsl__be_t2021, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2021, 0.45).
narrative_ontology:measurement(nsl__be_t2022, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2022, 0.48).
narrative_ontology:measurement(nsl__be_t2023, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2023, 0.5).
narrative_ontology:measurement(nsl__be_t2024, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2024, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t2019, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2019, 0.6).
narrative_ontology:measurement(nsl__su_t2020, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2020, 0.65).
narrative_ontology:measurement(nsl__su_t2021, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2021, 0.7).
narrative_ontology:measurement(nsl__su_t2022, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2022, 0.75).
narrative_ontology:measurement(nsl__su_t2023, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2023, 0.78).
narrative_ontology:measurement(nsl__su_t2024, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__sovereignty_restoration_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, local_autonomy_erosion).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, international_relations_tension).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'nsl_legal_text' kernel, each representing a distinct structural interpretation of the National Security Law. This reading focuses on the law's role in restoring sovereign order.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
