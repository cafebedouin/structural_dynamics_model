% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__universality_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__universality_paradox_reading, []).

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
 *   constraint_id: unconditional_income_support__universality_paradox_reading
 *   human_readable: Unconditional Income Support: Universality Paradox Reading
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint describes the political ambiguity surrounding
 *   unconditional income support (UIS), where its cross-ideological appeal
 *   functions as a 'Trojan horse,' masking incompatible implementation paths
 *   that often converge on similar fiscal outcomes due to 'taxing-back'
 *   mechanisms. This story is one reading of the
 *   'unconditional_income_support' kernel, focusing on the political dynamics
 *   of ambiguity rather than its direct economic or social effects. Sibling
 *   readings include 'freedom_floor_reading' and 'dependency_trap_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__universality_paradox_reading, 0.25).
domain_priors:suppression_score(unconditional_income_support__universality_paradox_reading, 0.45).
domain_priors:theater_ratio(unconditional_income_support__universality_paradox_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__universality_paradox_reading, tangled_rope).
narrative_ontology:human_readable(unconditional_income_support__universality_paradox_reading, "Unconditional Income Support: Universality Paradox Reading").
narrative_ontology:topic_domain(unconditional_income_support__universality_paradox_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(unconditional_income_support__universality_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__universality_paradox_reading, '8133fd76-3d73-4101-8bfe-caf9132b1ef0').
narrative_ontology:cs_kernel_codification('8133fd76-3d73-4101-8bfe-caf9132b1ef0', distributed).
narrative_ontology:cs_authority_grounding('8133fd76-3d73-4101-8bfe-caf9132b1ef0', practice).
narrative_ontology:cs_interpretation_layer_present('8133fd76-3d73-4101-8bfe-caf9132b1ef0').
narrative_ontology:cs_reading_relation('8133fd76-3d73-4101-8bfe-caf9132b1ef0', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('8133fd76-3d73-4101-8bfe-caf9132b1ef0', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('8133fd76-3d73-4101-8bfe-caf9132b1ef0', foundational, political_ambiguity_is_strategic).
narrative_ontology:cs_axiom_status(political_ambiguity_is_strategic, holdable).
narrative_ontology:cs_axiom_grounding('8133fd76-3d73-4101-8bfe-caf9132b1ef0', political_ambiguity_is_strategic, instrumental).
narrative_ontology:cs_axiom('8133fd76-3d73-4101-8bfe-caf9132b1ef0', foundational, fiscal_outcomes_converge_despite_ideology).
narrative_ontology:cs_axiom_status(fiscal_outcomes_converge_despite_ideology, holdable).
narrative_ontology:cs_axiom_grounding('8133fd76-3d73-4101-8bfe-caf9132b1ef0', fiscal_outcomes_converge_despite_ideology, empirically_contingent).
narrative_ontology:cs_reference_frame('8133fd76-3d73-4101-8bfe-caf9132b1ef0', strategic_coalition_building).
narrative_ontology:cs_drift_state('8133fd76-3d73-4101-8bfe-caf9132b1ef0', contemporary_policy_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8133fd76-3d73-4101-8bfe-caf9132b1ef0', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__universality_paradox_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, political_entrepreneurs).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, policy_designers).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, ideological_clarity_advocates).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, targeted_program_recipients).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exploit the cross-ideological appeal and ambiguity of unconditional income support to build broad, otherwise incompatible political coalitions, gaining influence and advancing their careers.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, political_entrepreneurs, agenda_setter,
    powerful, biographical, mobile, national).

% Develop and implement 'taxing-back' mechanisms that allow the policy to be rhetorically framed as universal while achieving similar fiscal outcomes to targeted programs, maintaining their professional relevance and institutional power.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, policy_designers, agenda_setter,
    institutional, generational, constrained, national).

% Bear the cost of ideological confusion and lack of coherent policy evaluation, as the ambiguity prevents clear debate and consistent implementation of social policy goals, regardless of their stance on UBI itself.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, ideological_clarity_advocates, payer,
    moderate, biographical, constrained, national).

% Risk losing existing, often more generous, targeted social support programs as universality is used to justify cuts or consolidation, potentially leaving them worse off despite the rhetorical promise of UBI.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, targeted_program_recipients, payer,
    powerless, immediate, trapped, local).

% Advocate for UBI as an autonomy-enabling floor, but their specific vision and normative commitments are often obscured or co-opted by the broader political ambiguity, preventing a clear articulation of their goals.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, freedom_floor_advocates, excluded,
    organized, biographical, mobile, national).

% Criticize UBI as an incentive-distorting subsidy, but their concerns about idleness and fiscal responsibility are often masked or dismissed by the cross-ideological appeal, hindering effective policy critique.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, dependency_trap_critics, excluded,
    organized, biographical, mobile, national).

% Conduct independent research on the fiscal and distributional outcomes of various UBI designs, often finding that different ideological justifications converge on similar practical results due to taxing-back mechanisms.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, fiscal_analysts, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unconditional_income_support__universality_paradox_reading, political_entrepreneurs).
narrative_ontology:fixing_cost_class(unconditional_income_support__universality_paradox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate diverse political factions and ideological camps around a common policy label ('unconditional income support') despite their differing and often incompatible underlying goals and implementation preferences.
% TRANSFER_FUNCTION: Transfers political capital, rhetorical flexibility, and coalition-building capacity to political entrepreneurs and policy designers, by obscuring the actual fiscal and social impacts and allowing multiple, conflicting interpretations of the policy's purpose.
% ABSENT_VOICES: Advocates for clear, ideologically consistent policy design (both those strongly for and strongly against UBI) are often sidelined or drowned out by the strategic ambiguity, preventing a transparent and coherent public debate.
% DISAPPEARANCE_RATIONALE: If the political ambiguity surrounding unconditional income support vanished overnight, the existing cross-ideological coalitions would likely fracture. Proponents would be forced to articulate specific, ideologically coherent proposals, leading to a reorganization of political alliances and policy debates around social welfare.
% FOUNDING_PROBLEM: The persistent difficulty of building broad political consensus and coalitions for significant social policy reform in ideologically polarized political environments.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists, policy analysts, and historians (outside the direct beneficiaries of the ambiguity) corroborate the ongoing challenge of coalition building in social policy and the strategic use of ambiguous policy framing as a common political tactic.
narrative_ontology:disappearance_verdict(unconditional_income_support__universality_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__universality_paradox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__universality_paradox_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(unconditional_income_support__universality_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__universality_paradox_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__universality_paradox_reading_tests).
:- end_tests(unconditional_income_support__universality_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates diverse political factions (beneficiaries) around a policy label, but simultaneously extracts from ideological clarity and potentially from targeted program recipients (victims) through its inherent ambiguity. Extractiveness is low (0.25) because the 'extraction' is primarily political capital and rhetorical space, and fiscal outcomes often converge across designs. Suppression (0.45) reflects the active masking of incompatible paths and the sidelining of clear ideological debate. Theater ratio is high (0.6) due to the significant performative aspect of maintaining cross-ideological appeal despite underlying divergences.
 *
 * PERSPECTIVAL GAP:
 *   Political entrepreneurs and policy designers perceive this ambiguity as a strategic tool for coalition building and policy innovation, enabling progress where clear ideological stances would lead to gridlock. Conversely, advocates for ideological clarity and targeted program recipients experience it as a source of confusion, misdirection, and potential harm, as their specific concerns are obscured or undermined by the universalist rhetoric.
 *
 * DIRECTIONALITY LOGIC:
 *   Political entrepreneurs and policy designers are beneficiaries (low d) as they gain political leverage and flexibility from the ambiguity. Ideological clarity advocates and targeted program recipients are victims (high d) as they bear the costs of obscured policy debate and potential loss of specific benefits. The 'excluded' stakeholders (freedom_floor_advocates, dependency_trap_critics) are those whose specific, ideologically consistent readings are marginalized by the dominant ambiguous framing.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to build broad political coalitions for social policy reform. The ambiguity serves this function, but it also risks becoming a self-perpetuating political strategy that prevents genuine assessment of whether the policy truly addresses social needs or merely serves political expediency. The high theater ratio suggests a significant portion of the constraint's operation is performative maintenance of this ambiguity, rather than functional problem-solving.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ideological_convergence_vs_divergence,
    'Does the observed fiscal convergence of UIS designs truly reflect a deeper ideological convergence, or does it merely mask persistent, irreconcilable ideological divergences that will eventually lead to policy instability?',
    'Longitudinal studies of political discourse and voting patterns post-implementation, observing whether initial coalitions hold or fracture as specific impacts become clearer.',
    'If divergence persists, the constraint is more extractive (from clarity) and less coordinative than currently assessed, potentially shifting its classification towards a Snare of political manipulation. If convergence is genuine, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ideological_convergence_vs_divergence, empirical, 'Whether fiscal convergence reflects genuine ideological alignment or merely strategic masking.').

omega_variable(
    ambiguity_as_feature_or_bug,
    'Is the political ambiguity of unconditional income support a necessary ''feature'' for achieving policy innovation and overcoming political gridlock, or is it a ''bug'' that prevents transparent governance and effective public accountability?',
    'Comparative policy analysis across jurisdictions with varying levels of ideological clarity in their UIS debates, assessing the long-term stability and public trust in the implemented policies.',
    'If ambiguity is a necessary feature, the measured extraction from clarity is a justifiable cost of coordination. If it''s a bug, the extraction is unwarranted, and the constraint''s legitimacy is undermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_as_feature_or_bug, conceptual, 'The normative status of political ambiguity in policy design.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__universality_paradox_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__universality_paradox_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(unco_tr_t5, unconditional_income_support__universality_paradox_reading, theater_ratio, 5, 0.54).
narrative_ontology:measurement(unco_tr_t10, unconditional_income_support__universality_paradox_reading, theater_ratio, 10, 0.57).
narrative_ontology:measurement(unco_tr_t15, unconditional_income_support__universality_paradox_reading, theater_ratio, 15, 0.59).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__universality_paradox_reading, theater_ratio, 20, 0.6).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__universality_paradox_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(unco_be_t5, unconditional_income_support__universality_paradox_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(unco_be_t10, unconditional_income_support__universality_paradox_reading, base_extractiveness, 10, 0.23).
narrative_ontology:measurement(unco_be_t15, unconditional_income_support__universality_paradox_reading, base_extractiveness, 15, 0.24).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__universality_paradox_reading, base_extractiveness, 20, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__universality_paradox_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(unco_su_t5, unconditional_income_support__universality_paradox_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(unco_su_t10, unconditional_income_support__universality_paradox_reading, suppression_requirement, 10, 0.43).
narrative_ontology:measurement(unco_su_t15, unconditional_income_support__universality_paradox_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement(unco_su_t20, unconditional_income_support__universality_paradox_reading, suppression_requirement, 20, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__universality_paradox_reading, identity_coordination).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__dependency_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'unconditional_income_support' kernel, focusing on the political ambiguity and 'Trojan horse' aspect. It is linked to sibling readings that emphasize the 'freedom floor' and 'dependency trap' perspectives, as these are all interpretations of the same underlying policy concept.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
