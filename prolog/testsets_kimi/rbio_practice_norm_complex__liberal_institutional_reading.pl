% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__liberal_institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__liberal_institutional_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__liberal_institutional_reading
 *   human_readable: RBIO Liberal Institutional Reading
 *   domain: international_relations/law/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the liberal institutional reading of the
 *   Rules-Based International Order (RBIO) practice norm complex. Under this
 *   reading, the UN Charter-based order is universal, consent-based, and
 *   genuinely revisable through multilateral amendment and institutional
 *   practice; enforcement selectivity reflects material capacity constraints
 *   and geopolitical friction rather than a structural legitimacy deficit.
 *   The reading acknowledges that intervening states and their contractors
 *   benefit from authorized interventions and sanctions regimes, while
 *   targeted states and their civilian populations bear the costs, but frames
 *   these asymmetries as incidental to the coordination function of
 *   collective security rather than constitutive of extraction. It is one of
 *   three contested readings of the same kernel, alongside a hegemonic
 *   extraction reading and a sovereignty maximalist reading.
 *
 * KEY AGENTS:
 *   - unsc_p5_bloc: Primary agenda-setter (institutional/constrained) â administers authorization and veto
 *   - intervening_states: Primary beneficiary (powerful/mobile) â gains legitimacy and strategic rents
 *   - defense_contractors: Secondary beneficiary (powerful/mobile) â captures intervention rents
 *   - targeted_states: Primary target (powerless/trapped) â loses sovereignty and bears sanctions
 *   - civilian_populations_in_target_states: Secondary target (powerless/trapped) â bears humanitarian costs
 *   - global_south_dissent_bloc: Excluded voice (moderate/constrained) â contests selectivity without agenda power
 *   - international_legal_scholars: Analytical observer (analytical) â provides legitimizing/delegitimizing discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__liberal_institutional_reading, 0.63).
domain_priors:suppression_score(rbio_practice_norm_complex__liberal_institutional_reading, 0.76).
domain_priors:theater_ratio(rbio_practice_norm_complex__liberal_institutional_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__liberal_institutional_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__liberal_institutional_reading, "RBIO Liberal Institutional Reading").
narrative_ontology:topic_domain(rbio_practice_norm_complex__liberal_institutional_reading, "international_relations/law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__liberal_institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__liberal_institutional_reading, '22956477-4712-4769-804a-2aaaf3d165c3').
narrative_ontology:cs_kernel_codification('22956477-4712-4769-804a-2aaaf3d165c3', formalized).
narrative_ontology:cs_authority_grounding('22956477-4712-4769-804a-2aaaf3d165c3', lineage).
narrative_ontology:cs_interpretation_layer_present('22956477-4712-4769-804a-2aaaf3d165c3').
narrative_ontology:cs_reading_relation('22956477-4712-4769-804a-2aaaf3d165c3', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('22956477-4712-4769-804a-2aaaf3d165c3', rbio_practice_norm_complex__sovereignty_maximalist_reading, forecloses).
narrative_ontology:cs_axiom('22956477-4712-4769-804a-2aaaf3d165c3', foundational, unsc_authorization_legitimizes_intervention).
narrative_ontology:cs_axiom_status(unsc_authorization_legitimizes_intervention, holdable).
narrative_ontology:cs_axiom_grounding('22956477-4712-4769-804a-2aaaf3d165c3', unsc_authorization_legitimizes_intervention, conventional).
narrative_ontology:cs_axiom('22956477-4712-4769-804a-2aaaf3d165c3', foundational, multilateral_consent_genuine_revisability).
narrative_ontology:cs_axiom_status(multilateral_consent_genuine_revisability, holdable).
narrative_ontology:cs_axiom_grounding('22956477-4712-4769-804a-2aaaf3d165c3', multilateral_consent_genuine_revisability, conventional).
narrative_ontology:cs_reference_frame('22956477-4712-4769-804a-2aaaf3d165c3', multilateral_consent_based_order).
narrative_ontology:cs_drift_state('22956477-4712-4769-804a-2aaaf3d165c3', contemporary_multipolar_contestation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('22956477-4712-4769-804a-2aaaf3d165c3', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, defense_contractors).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, targeted_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, civilian_populations_in_target_states).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__liberal_institutional_reading, un_charter_supremacy).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__liberal_institutional_reading, responsibility_to_protect).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the UN Charter framework, controls veto power over authorization of force and sanctions, and sets the institutional agenda for RBIO enforcement. While formally constrained by Charter rules, the P5 bloc effectively determines which crises receive multilateral legitimacy and which do not.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, unsc_p5_bloc, agenda_setter,
    institutional, generational, constrained, global).

% Receive UN-sanctioned legitimacy for military intervention and economic sanctions, gain strategic outcomes and regime-change objectives, and capture reconstruction and security contracts in targeted territories. They benefit from the selective enforcement patterns that align with their strategic interests.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states, beneficiary,
    powerful, generational, mobile, global).

% Receive lucrative contracts for military logistics, private security, and post-intervention reconstruction services in theaters authorized or tacitly permitted by the RBIO framework. Their revenue flows are directly tied to the volume and geographic distribution of sanctioned interventions.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, defense_contractors, beneficiary,
    powerful, biographical, mobile, global).

% Lose effective sovereignty through UNSC-authorized sanctions and military intervention, bear the economic and territorial costs of enforcement actions, and are excluded from the agenda-setting that determines when and where norms are enforced against them.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, targeted_states, payer,
    powerless, generational, trapped, national).

% Bear the humanitarian costs of sanctions regimes and military interventions authorized under the RBIO framework, including food insecurity, infrastructure destruction, and displacement. They have no exit from the territory of the targeted state and no voice in the authorization decisions.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, civilian_populations_in_target_states, payer,
    powerless, immediate, trapped, national).

% Comprises states and diplomatic coalitions from the Global South that contest the selectivity of RBIO enforcement and the double standards of humanitarian intervention. They are formally present in UN General Assembly debates but structurally excluded from UNSC agenda-setting and P5 decision-making on authorization.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, global_south_dissent_bloc, excluded,
    moderate, generational, constrained, global).

% Analyze the consistency of RBIO enforcement with Charter text and customary international law, document patterns of selectivity, and provide the interpretive framework that either legitimizes or delegitimizes specific interventions.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__liberal_institutional_reading, diffuse).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__liberal_institutional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal institutional framework for collective security, interstate conflict management, and peaceful revision of international legal obligations through multilateral consent, replacing ad hoc power politics with rules-based procedures.
% TRANSFER_FUNCTION: Moves military intervention legitimacy, strategic territorial access, and economic rents from targeted states and their populations to intervening states and global security contractors, mediated through UNSC authorization and multilateral sanctions architecture.
% ABSENT_VOICES: Civilian populations in targeted states bear the costs of sanctions and intervention but are not represented in UNSC deliberations; targeted state governments are heard but overruled; Global South states contest selectivity in the General Assembly but lack veto power to alter enforcement patterns.
% DISAPPEARANCE_RATIONALE: Without the RBIO multilateral framework, collective security authorization collapses into ad hoc coalitions, sanctions lose normative boundaries and proliferate arbitrarily, small states lose the formal shield of sovereign equality, and the global economy fragments into competing regional blocs.
% FOUNDING_PROBLEM: The absence of a universal mechanism to prevent interstate aggression, manage collective security, and enable peaceful revision of international order following the collapse of the League of Nations and the experience of World War II.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars outside the P5 beneficiary coalition attest the founding problem was genuine in 1945; Global South diplomats and critical IR scholars attest the arrangement has drifted toward P5 prerogative, while P5 foreign ministries and liberal institutionalist scholars assert the founding problem remains live. Corroboration is split across seats with no neutral consensus.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__liberal_institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__liberal_institutional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__liberal_institutional_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__liberal_institutional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__liberal_institutional_reading, 0.63, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__liberal_institutional_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.63 reflects substantial but partial extraction: the framework delivers genuine coordination (collective security, dispute resolution, trade predictability) while asymmetrically transferring costs to weaker targets. Suppression at 0.76 reflects the active delegitimization of unilateral alternatives and the institutional barriers to Charter revision. Theater_ratio at 0.47 reflects significant performative maintenance of multilateral legitimacy even when enforcement tracks P5 interests. Accessibility_collapse at 0.52: alternatives such as unilateralism, regional blocs, and non-Westphalian orders persist but are heavily delegitimized by the RBIO discourse. Resistance at 0.58: substantial and growing pushback from targeted states, Russia and China, and the broader Global South against selective enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the UNSC P5 and intervening-state seats, the constraint appears as a Rope or Scaffold â a necessary coordination mechanism for global order that extracts only to the extent required by capacity limits. From the targeted-state and civilian-population seats, the same structure reads as a Snare or Tangled Rope â active enforcement that coincidentally aligns with P5 strategic interests and systematically falls on weaker states. The engine computes this divergence from the structural data; the authored claim (tangled_rope) captures the hybrid reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (intervening_states, defense_contractors) have low directionality because the constraint subsidizes their strategic and economic interests through authorized intervention and contract flows. Victims (targeted_states, civilian_populations_in_target_states) have high directionality because the constraint extracts sovereignty and welfare from them via sanctions and military action. The UNSC P5 bloc sits near symmetric directionality: it administrates the constraint and derives structural power from it, but is also formally bound by its procedural rules. Global South dissent and legal scholars occupy intermediate positions based on their exit options and institutional distance from extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by mandating declared victims for any extractive classification. This reading could be mistaken for a Rope if one looked only at the coordination function (collective security, dispute resolution, consent-based revision). By declaring targeted states and civilian populations as victims and requiring active enforcement, the story forces the engine to register the asymmetric extraction that rides on the coordination mechanism, producing Tangled Rope rather than pure Rope. Conversely, the genuine coordination function (universal membership, consent-based procedures, humanitarian exceptions) prevents collapse into pure Snare â the extraction is hybrid, not cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_vs_intent_selectivity,
    'Is enforcement selectivity a genuine capacity constraint in a multilateral system, or does the structural alignment of selective enforcement with P5 strategic interests indicate extractive intent?',
    'Comparative case analysis of enforcement patterns: if selectivity correlates more strongly with P5 strategic interest than with objective capacity constraints (geographic proximity, logistical feasibility), the capacity explanation weakens and the extractive explanation strengthens.',
    'Resolution would shift classification toward hegemonic extraction (if intent) or toward rope/scaffold (if genuine capacity); keeps the current tangled_rope classification in the intermediate zone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_vs_intent_selectivity, empirical, 'Ambiguity between capacity and intent explanations for selective RBIO enforcement').

omega_variable(
    genuine_revisability_of_charter,
    'Are UN Charter amendment procedures and RBIO norms genuinely revisable through multilateral consent, or does the P5 veto make revision practically impossible?',
    'Historical success rate of Charter amendment proposals and comparative analysis of institutional change pathways; if amendment is blocked while practice drifts via interpretation, the formal revisability is theatrical.',
    'If revision is blocked, the consent-based claim is performative and theater_ratio rises; if genuinely revisable, the coordination function is stronger and extraction may be lower.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(genuine_revisability_of_charter, empirical, 'Whether multilateral revisability is structural or performative').

omega_variable(
    committer_reading_boundary,
    'Does the liberal institutional reading of RBIO share enough structural premises with the hegemonic extraction reading that they are variant framings of the same constraint, or are they structurally distinct constraints with different epsilon values?',
    'Measure epsilon under both framings for the same empirical cases; if epsilon differs by more than 0.2 across the same observations, they are distinct constraints per the epsilon-invariance principle.',
    'If they are variant framings, the kernel context should be revised; if distinct, the current decomposition is validated and network edges are warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_reading_boundary, conceptual, 'Whether sibling readings are distinct constraints or observer-relative framings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__liberal_institutional_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t0, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(rbio_tr_t20, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(rbio_tr_t40, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(rbio_tr_t60, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement(rbio_tr_t75, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 75, 0.45).
narrative_ontology:measurement(rbio_tr_t80, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 80, 0.47).

% Extraction over time
narrative_ontology:measurement(rbio_be_t0, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(rbio_be_t20, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(rbio_be_t40, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement(rbio_be_t60, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(rbio_be_t75, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 75, 0.6).
narrative_ontology:measurement(rbio_be_t80, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 80, 0.63).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t0, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(rbio_su_t20, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(rbio_su_t40, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(rbio_su_t60, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(rbio_su_t75, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 75, 0.72).
narrative_ontology:measurement(rbio_su_t80, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 80, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__liberal_institutional_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, hegemonic_extraction_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, sovereignty_maximalist_reading).

% DUAL FORMULATION NOTE:
% The RBIO practice norm complex decomposes into at least three structurally distinct constraints under the epsilon-invariance principle: the liberal institutional reading (this file), the hegemonic extraction reading, and the sovereignty maximalist reading. They share the same kernel (UN Charter / RBIO norms) but instantiate different epsilon values and beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
