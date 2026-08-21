% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__cohabitation_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__cohabitation_equilibrium_reading, []).

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
 *   constraint_id: fifth_republic_constitution__cohabitation_equilibrium_reading
 *   human_readable: Fifth Republic Constitution: Cohabitation Equilibrium Reading
 *   domain: constitutional_law/political_systems/comparative_government
 *
 * SUMMARY:
 *   This constraint describes the 'cohabitation equilibrium' reading of the
 *   Fifth Republic Constitution, where executive authority is negotiated
 *   between a directly elected president and a prime minister accountable to
 *   the National Assembly. This reading emphasizes mutual constraint and
 *   power-sharing, particularly when the president and parliamentary majority
 *   come from different political parties. The system coordinates governance
 *   but incurs costs in policy coherence and stability, leading to moderate
 *   extractiveness. This reading contrasts with the
 *   'hyper_presidential_reading' (president as sovereign) and the
 *   'parliamentary_constraint_reading' (president requiring legislative
 *   authorization).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.55).
domain_priors:suppression_score(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.45).
domain_priors:theater_ratio(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__cohabitation_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__cohabitation_equilibrium_reading, "Fifth Republic Constitution: Cohabitation Equilibrium Reading").
narrative_ontology:topic_domain(fifth_republic_constitution__cohabitation_equilibrium_reading, "constitutional_law/political_systems/comparative_government").

domain_priors:requires_active_enforcement(fifth_republic_constitution__cohabitation_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__cohabitation_equilibrium_reading, '2c659129-6021-4718-b682-05d2dff1a217').
narrative_ontology:cs_kernel_codification('2c659129-6021-4718-b682-05d2dff1a217', fixed_text).
narrative_ontology:cs_authority_grounding('2c659129-6021-4718-b682-05d2dff1a217', lineage).
narrative_ontology:cs_interpretation_layer_present('2c659129-6021-4718-b682-05d2dff1a217').
narrative_ontology:cs_reading_relation('2c659129-6021-4718-b682-05d2dff1a217', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('2c659129-6021-4718-b682-05d2dff1a217', fifth_republic_constitution__parliamentary_constraint_reading, coexists_with).
narrative_ontology:cs_axiom('2c659129-6021-4718-b682-05d2dff1a217', foundational, executive_power_shared).
narrative_ontology:cs_axiom_status(executive_power_shared, holdable).
narrative_ontology:cs_axiom_grounding('2c659129-6021-4718-b682-05d2dff1a217', executive_power_shared, conventional).
narrative_ontology:cs_axiom('2c659129-6021-4718-b682-05d2dff1a217', foundational, mutual_veto_legitimate).
narrative_ontology:cs_axiom_status(mutual_veto_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('2c659129-6021-4718-b682-05d2dff1a217', mutual_veto_legitimate, conventional).
narrative_ontology:cs_reference_frame('2c659129-6021-4718-b682-05d2dff1a217', balanced_executive_power).
narrative_ontology:cs_drift_state('2c659129-6021-4718-b682-05d2dff1a217', contemporary_quinquennat_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2c659129-6021-4718-b682-05d2dff1a217', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, president).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, political_parties).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, electorate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directly elected head of state, sets broad national direction, especially foreign policy. During cohabitation, the president must negotiate authority allocation with the prime minister and National Assembly, constraining unilateral action.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, president, agenda_setter,
    powerful, generational, constrained, national).

% Head of government, leads domestic policy, and is accountable to the National Assembly. During cohabitation, the prime minister's power is significantly amplified, requiring the president to cede control over certain policy domains.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister, agenda_setter,
    powerful, biographical, constrained, national).

% The legislative body, which can pass no-confidence motions against the prime minister. Its power to influence policy and constrain the president is significantly enhanced during periods of cohabitation.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly, beneficiary,
    organized, biographical, constrained, national).

% Adjudicates the constitutionality of laws and elections, acting as a crucial arbiter in disputes over authority allocation between the executive branches, ensuring the constitutional framework is upheld.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, constitutional_council, observer,
    institutional, civilizational, analytical, national).

% Bears the costs of potential policy instability or incoherence that can arise from the negotiated authority allocation during cohabitation, but also has the power to shift the balance of power through elections.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, electorate, payer,
    organized, biographical, constrained, national).

% Organize political action, contest elections, and form governments. Their alignment or opposition is central to whether cohabitation occurs and how the negotiated authority allocation functions.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, political_parties, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, political_parties, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__cohabitation_equilibrium_reading, diffuse).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__cohabitation_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To ensure stable governance by distributing executive power between a directly elected president and a prime minister accountable to the legislature, preventing unilateral rule and balancing different mandates.
% TRANSFER_FUNCTION: Transfers authority over specific policy domains (e.g., foreign policy to the president, domestic to the prime minister) based on political alignment, and transfers the costs of policy incoherence or deadlock to the electorate.
% ABSENT_VOICES: Advocates for a purely presidential or purely parliamentary system, who would argue for clearer lines of authority and less potential for deadlock, are structurally marginalized by the existing constitutional design.
% DISAPPEARANCE_RATIONALE: If the constitutional framework for cohabitation and its negotiated authority allocation vanished overnight, the entire French political system would collapse into a constitutional crisis, leading to a fundamental reorganization of executive and legislative power.
% FOUNDING_PROBLEM: To create a strong, stable executive after the instability of the Fourth Republic, balancing presidential authority with parliamentary accountability to prevent both executive overreach and legislative paralysis.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars, political scientists, and historical analyses from outside the directly benefiting political parties corroborate this founding problem and its ongoing relevance in managing the inherent tensions of the dual executive system.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__cohabitation_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__cohabitation_equilibrium_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(fifth_republic_constitution__cohabitation_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__cohabitation_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fifth_republic_constitution__cohabitation_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fifth_republic_constitution__cohabitation_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.55) reflects the inherent costs of negotiation, potential policy incoherence, and the political maneuvering required to maintain the equilibrium. Suppression (0.45) is moderate, as neither executive branch can fully suppress the other, but both possess significant blocking power. The low theater ratio (0.15) indicates that the negotiations and power struggles are genuine, not merely performative. The temporal measurements show fluctuations in extractiveness, reflecting periods of more or less intense cohabitation, but generally remain in the moderate range, consistent with an unstable equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the executive branches and political parties, the cohabitation equilibrium is a necessary and legitimate mechanism for power-sharing and stable governance. From the perspective of the electorate, it can be seen as a source of policy friction and inefficiency, leading to a different experience of the constraint's extractiveness. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The president, prime minister, National Assembly, and political parties are all beneficiaries in this reading, as they gain influence and power within their respective domains through the negotiated allocation of authority. The electorate, however, acts as a payer, bearing the costs of potential policy instability or incoherence. The Constitutional Council acts as an observer, ensuring adherence to the constitutional framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cohabitation_stability_ambiguity,
    'Is the observed stability of cohabitation a feature of the constitutional design itself, or primarily a result of political actors'' pragmatism and willingness to compromise?',
    'Comparative analysis with other dual-executive systems lacking similar constitutional provisions, or historical analysis of periods where political actors were less pragmatic.',
    'If primarily due to pragmatism, the constraint''s inherent stability is lower than perceived, and its classification might drift towards a more extractive type if political will erodes. If constitutional, its stability is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohabitation_stability_ambiguity, empirical, 'Whether cohabitation stability is constitutional or political.').

omega_variable(
    policy_coherence_cost_justification,
    'Is the cost to policy coherence and efficiency, inherent in negotiated authority allocation, an acceptable trade-off for the democratic legitimacy and power-sharing it provides, or an unnecessary inefficiency?',
    'Public opinion surveys on satisfaction with governance during cohabitation, and expert analysis comparing policy outcomes under cohabitation versus unified government.',
    'If deemed an acceptable trade-off, the measured extractiveness is viewed as a legitimate cost of coordination. If deemed an unnecessary inefficiency, it strengthens the argument for constitutional reform to reduce extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_coherence_cost_justification, preference, 'Normative evaluation of policy coherence costs.').

omega_variable(
    electoral_cycle_impact_on_equilibrium,
    'How do changes in electoral cycles, such as the alignment of presidential and legislative terms (quinquennat), alter the cohabitation equilibrium and its inherent extractiveness?',
    'Longitudinal study comparing periods before and after the quinquennat reform, analyzing frequency and duration of cohabitation, and associated policy outcomes.',
    'If electoral alignment significantly reduces cohabitation, the constraint''s ''equilibrium'' aspect becomes less relevant, and the system might drift towards a more ''hyper-presidential'' classification. If cohabitation persists, the equilibrium remains a core feature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(electoral_cycle_impact_on_equilibrium, empirical, 'Impact of electoral reforms on cohabitation dynamics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__cohabitation_equilibrium_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t0, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fift_tr_t5, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(fift_tr_t10, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(fift_tr_t15, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement(fift_tr_t20, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(fift_tr_t25, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 25, 0.13).
narrative_ontology:measurement(fift_tr_t30, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(fift_be_t0, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(fift_be_t5, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(fift_be_t10, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(fift_be_t15, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(fift_be_t20, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(fift_be_t25, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 25, 0.53).
narrative_ontology:measurement(fift_be_t30, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 30, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t0, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(fift_su_t5, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(fift_su_t10, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(fift_su_t15, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(fift_su_t20, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 20, 0.43).
narrative_ontology:measurement(fift_su_t25, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement(fift_su_t30, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 30, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
