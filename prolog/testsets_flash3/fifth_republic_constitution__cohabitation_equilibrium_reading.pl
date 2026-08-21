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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: fifth_republic_constitution__cohabitation_equilibrium_reading
 *   human_readable: Fifth Republic Constitution: Cohabitation Equilibrium Reading
 *   domain: constitutional_law/political_systems/comparative_government
 *
 * SUMMARY:
 *   This constraint describes the 'cohabitation equilibrium' reading of the
 *   French Fifth Republic Constitution, where a President and Prime Minister
 *   from opposing political parties are forced to negotiate and share
 *   executive authority. This reading emphasizes the mutual constraints and
 *   negotiated allocation of power, particularly between foreign policy
 *   (Presidential domain) and domestic policy (Prime Ministerial domain). It
 *   is one of several interpretations of the Fifth Republic's dual executive
 *   structure.
 *
 * KEY AGENTS:
 *   - president_in_cohabitation: Agenda setter (institutional/constrained)
 *   - prime_minister_in_cohabitation: Agenda setter (institutional/constrained)
 *   - national_assembly: Beneficiary (institutional/constrained)
 *   - electorate: Payer (organized/mobile)
 *   - policy_coherence: Victim (powerless/trapped)
 *   - political_stability: Victim (powerless/trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.45).
domain_priors:suppression_score(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.6).
domain_priors:theater_ratio(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__cohabitation_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__cohabitation_equilibrium_reading, "Fifth Republic Constitution: Cohabitation Equilibrium Reading").
narrative_ontology:topic_domain(fifth_republic_constitution__cohabitation_equilibrium_reading, "constitutional_law/political_systems/comparative_government").

domain_priors:requires_active_enforcement(fifth_republic_constitution__cohabitation_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__cohabitation_equilibrium_reading, 'd25e07a2-49f0-4c2a-9cab-49f75fd3662f').
narrative_ontology:cs_kernel_codification('d25e07a2-49f0-4c2a-9cab-49f75fd3662f', formalized).
narrative_ontology:cs_authority_grounding('d25e07a2-49f0-4c2a-9cab-49f75fd3662f', lineage).
narrative_ontology:cs_interpretation_layer_present('d25e07a2-49f0-4c2a-9cab-49f75fd3662f').
narrative_ontology:cs_reading_relation('d25e07a2-49f0-4c2a-9cab-49f75fd3662f', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('d25e07a2-49f0-4c2a-9cab-49f75fd3662f', fifth_republic_constitution__parliamentary_constraint_reading, coexists_with).
narrative_ontology:cs_axiom('d25e07a2-49f0-4c2a-9cab-49f75fd3662f', foundational, executive_power_shared_by_mandate).
narrative_ontology:cs_axiom_status(executive_power_shared_by_mandate, holdable).
narrative_ontology:cs_axiom_grounding('d25e07a2-49f0-4c2a-9cab-49f75fd3662f', executive_power_shared_by_mandate, conventional).
narrative_ontology:cs_axiom('d25e07a2-49f0-4c2a-9cab-49f75fd3662f', foundational, mutual_constraint_ensures_balance).
narrative_ontology:cs_axiom_status(mutual_constraint_ensures_balance, holdable).
narrative_ontology:cs_axiom_grounding('d25e07a2-49f0-4c2a-9cab-49f75fd3662f', mutual_constraint_ensures_balance, instrumental).
narrative_ontology:cs_reference_frame('d25e07a2-49f0-4c2a-9cab-49f75fd3662f', constitutional_balance_of_powers).
narrative_ontology:cs_drift_state('d25e07a2-49f0-4c2a-9cab-49f75fd3662f', contemporary_political_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d25e07a2-49f0-4c2a-9cab-49f75fd3662f', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, president_in_cohabitation).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister_in_cohabitation).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, political_stability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, electorate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The President, whose party does not control the National Assembly, must negotiate policy with the Prime Minister. Benefits from retaining control over foreign policy and defense, but faces significant constraints on domestic agenda. The President's authority is derived from direct popular election, but its exercise is limited by the need for parliamentary support.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, president_in_cohabitation, agenda_setter,
    institutional, biographical, constrained, national).

% The Prime Minister, whose party controls the National Assembly, holds significant domestic policy power but must navigate the President's constitutional prerogatives and international role. Benefits from leading the government and implementing the parliamentary majority's agenda, but is constrained by the President's ability to dissolve the Assembly or appeal directly to the public.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister_in_cohabitation, agenda_setter,
    institutional, biographical, constrained, national).

% The legislative body, whose majority supports the Prime Minister. Benefits from increased influence over policy during cohabitation, as the President's power is diluted. Constrained by the President's power of dissolution and the need to maintain a stable majority.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly, beneficiary,
    institutional, biographical, constrained, national).

% Experiences policy shifts and potential gridlock due to the divided executive. Pays the cost of reduced governmental efficiency and clarity, but also benefits from checks and balances. Can express dissatisfaction through elections.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, electorate, payer,
    organized, immediate, mobile, national).

% Suffers from the need for constant negotiation and compromise between the President and Prime Minister, leading to fragmented or inconsistent policy implementation, especially in areas of overlapping jurisdiction. This is an abstract victim, representing the quality of governance.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_non_agent(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence).

% Can be undermined by the inherent tensions and potential for deadlock in a cohabitation scenario, leading to governmental crises or early elections. This is an abstract victim, representing the smooth functioning of the political system.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, political_stability, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_non_agent(fifth_republic_constitution__cohabitation_equilibrium_reading, political_stability).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the exercise of executive power between a President and a Prime Minister from opposing political parties, ensuring that government can continue to function despite divided mandates.
% TRANSFER_FUNCTION: Transfers authority and policy influence between the President and Prime Minister based on their respective constitutional powers and political strength, often resulting in a de facto division of labor (e.g., President for foreign affairs, PM for domestic).
% ABSENT_VOICES: A unified, efficient executive, unburdened by internal political divisions, is absent. Such an executive would argue for clearer lines of authority or a purely parliamentary system to avoid the inefficiencies of cohabitation.
% DISAPPEARANCE_RATIONALE: If the constitutional requirement for negotiated authority allocation vanished, the Fifth Republic's political system would fundamentally change. Either the President would become fully dominant (hyper-presidentialism) or the Prime Minister would become fully parliamentary (parliamentary constraint), leading to a different distribution of power and policy outcomes.
% FOUNDING_PROBLEM: The Fifth Republic Constitution was designed to overcome the governmental instability of the Fourth Republic by creating a strong, directly elected President, while retaining a parliamentary government.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and political historians widely corroborate that the founding problem of governmental instability was addressed by the Fifth Republic's design. The 'live' status of the problem is attested by ongoing debates about executive power and parliamentary accountability, even during periods of cohabitation.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__cohabitation_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__cohabitation_equilibrium_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fifth_republic_constitution__cohabitation_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__cohabitation_equilibrium_reading_tests).
:- end_tests(fifth_republic_constitution__cohabitation_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate and unstable, reflecting the costs of negotiation and potential policy gridlock, which are borne by policy coherence and political stability. Suppression (0.6) is present as the constitutional framework actively limits the full exercise of power by either executive, forcing compromise. Theater ratio (0.2) is low, as the negotiations are genuine and consequential, not merely performative. The claimed type is Tangled Rope because it genuinely coordinates the two executives but also extracts costs (policy coherence, stability) through the same structure, requiring active enforcement (constitutional norms, political pressure) to hold.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the President or Prime Minister during cohabitation, the constraint is a necessary framework for governance, albeit one that limits their full agenda. From the perspective of policy coherence or political stability, it is an extractive mechanism that introduces friction and potential instability. The electorate's view is mixed, valuing checks and balances but lamenting inefficiency.
 *
 * DIRECTIONALITY LOGIC:
 *   The President and Prime Minister, while constrained, are also beneficiaries in that they retain significant power and can implement parts of their agenda. The National Assembly benefits from increased influence. The electorate bears the costs of potential inefficiency. Policy coherence and political stability are abstract victims, bearing the systemic costs of divided authority. The 'requires_active_enforcement' is true because the constitutional norms and political pressures that compel negotiation are actively maintained.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the cohabitation dynamic as pure extraction (Snare) by recognizing its genuine coordination function in allowing government to operate under divided mandates. It also avoids mislabeling it as a pure coordination (Rope) by acknowledging the significant costs it imposes on policy coherence and stability. The 'contested' status of the founding problem highlights the ongoing debate about whether the system's original intent is still being served or if it has drifted into a less functional state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cohabitation_efficiency_tradeoff,
    'Is the policy inefficiency and potential instability during cohabitation an acceptable cost for the constitutional checks and balances it provides, or does it represent an unacceptable systemic extraction?',
    'Comparative analysis of policy outcomes and governmental stability during cohabitation periods versus periods of unified executive control, assessed against democratic values and governance effectiveness metrics.',
    'If deemed an acceptable cost, the extractiveness might be re-evaluated as a necessary coordination overhead. If unacceptable, it would reinforce the Snare-like aspects of the constraint, highlighting its systemic victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohabitation_efficiency_tradeoff, preference, 'Evaluates the normative trade-off between executive division and governmental efficiency.').

omega_variable(
    cohabitation_vs_hyperpresidentialism_framing,
    'Is this constraint a genuine ''cohabitation equilibrium'' or merely a temporary deviation from a ''hyper-presidential'' default, which reasserts itself whenever political conditions allow?',
    'Longitudinal analysis of constitutional practice and political culture, examining whether the norms of negotiated authority persist even when one party holds a strong majority, or if they erode in favor of presidential dominance.',
    'If it''s a temporary deviation, the ''hyper_presidential_reading'' would be seen as the more fundamental constraint, and this reading would be reclassified as a Scaffold or Piton, representing a transient or atrophied state of balance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohabitation_vs_hyperpresidentialism_framing, conceptual, 'Distinguishes between a stable equilibrium and a transient state within the Fifth Republic''s constitutional practice.').

omega_variable(
    coordination_extraction_boundary_cohabitation,
    'To what extent does the ''negotiated authority allocation'' genuinely coordinate the dual executive, versus merely providing a constitutional cover for political infighting and rent-seeking by the dominant party in each branch?',
    'Detailed case studies of policy formation during cohabitation, identifying instances of genuine compromise versus strategic obstruction or leveraging of constitutional powers for partisan gain. Analysis of resource allocation and patronage during these periods.',
    'If the ''coordination'' is primarily cover for extraction, the constraint''s extractiveness would be higher, and its classification would shift closer to a Snare. If genuine coordination predominates, it would reinforce the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary_cohabitation, empirical, 'Assesses the balance between genuine coordination and disguised extraction within the cohabitation framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__cohabitation_equilibrium_reading, 1986, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t1986, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1986, 0.15).
narrative_ontology:measurement(fift_tr_t1993, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1993, 0.25).
narrative_ontology:measurement(fift_tr_t1997, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1997, 0.22).
narrative_ontology:measurement(fift_tr_t2002, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2002, 0.18).
narrative_ontology:measurement(fift_tr_t2012, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2012, 0.2).
narrative_ontology:measurement(fift_tr_t2024, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(fift_be_t1986, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1986, 0.4).
narrative_ontology:measurement(fift_be_t1993, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1993, 0.5).
narrative_ontology:measurement(fift_be_t1997, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1997, 0.48).
narrative_ontology:measurement(fift_be_t2002, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2002, 0.42).
narrative_ontology:measurement(fift_be_t2012, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2012, 0.45).
narrative_ontology:measurement(fift_be_t2024, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t1986, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1986, 0.55).
narrative_ontology:measurement(fift_su_t1993, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1993, 0.65).
narrative_ontology:measurement(fift_su_t1997, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1997, 0.6).
narrative_ontology:measurement(fift_su_t2002, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2002, 0.58).
narrative_ontology:measurement(fift_su_t2012, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2012, 0.6).
narrative_ontology:measurement(fift_su_t2024, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__cohabitation_equilibrium_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution__hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution__parliamentary_constraint_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the French Fifth Republic Constitution's dual executive. Each reading represents a distinct structural claim about how executive power is allocated and constrained.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
