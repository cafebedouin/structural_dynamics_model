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
 *   domain: constitutional_law/political_systems
 *
 * SUMMARY:
 *   This constraint describes the 'cohabitation equilibrium' reading of the
 *   French Fifth Republic Constitution, where the dual executive (President
 *   and Prime Minister) must negotiate authority allocation, particularly
 *   when they come from different political majorities. This reading
 *   emphasizes the checks and balances inherent in the system, with both
 *   executive branches constraining each other. The extractiveness is
 *   moderate, reflecting the costs of policy incoherence and potential
 *   gridlock, while the beneficiaries are whichever actor controls key policy
 *   domains (e.g., President for foreign affairs, Prime Minister for
 *   domestic).
 *
 * KEY AGENTS:
 *   - president_of_france: Primary agenda_setter (institutional/constrained)
 *   - prime_minister_of_france: Primary agenda_setter (institutional/constrained)
 *   - national_assembly_majority: Beneficiary (organized/mobile)
 *   - policy_coherence: Victim (powerless/trapped)
 *   - political_stability: Victim (powerless/trapped)
 *   - french_electorate: Observer (organized/mobile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.45).
domain_priors:suppression_score(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.3).
domain_priors:theater_ratio(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__cohabitation_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__cohabitation_equilibrium_reading, "Fifth Republic Constitution: Cohabitation Equilibrium Reading").
narrative_ontology:topic_domain(fifth_republic_constitution__cohabitation_equilibrium_reading, "constitutional_law/political_systems").

domain_priors:requires_active_enforcement(fifth_republic_constitution__cohabitation_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__cohabitation_equilibrium_reading, '8677d927-9b7c-4d32-8077-4adb8f53012d').
narrative_ontology:cs_kernel_codification('8677d927-9b7c-4d32-8077-4adb8f53012d', fixed_text).
narrative_ontology:cs_authority_grounding('8677d927-9b7c-4d32-8077-4adb8f53012d', lineage).
narrative_ontology:cs_interpretation_layer_present('8677d927-9b7c-4d32-8077-4adb8f53012d').
narrative_ontology:cs_reading_relation('8677d927-9b7c-4d32-8077-4adb8f53012d', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('8677d927-9b7c-4d32-8077-4adb8f53012d', fifth_republic_constitution__parliamentary_constraint_reading, coexists_with).
narrative_ontology:cs_axiom('8677d927-9b7c-4d32-8077-4adb8f53012d', foundational, dual_executive_negotiated_authority).
narrative_ontology:cs_axiom_status(dual_executive_negotiated_authority, holdable).
narrative_ontology:cs_axiom_grounding('8677d927-9b7c-4d32-8077-4adb8f53012d', dual_executive_negotiated_authority, conventional).
narrative_ontology:cs_axiom('8677d927-9b7c-4d32-8077-4adb8f53012d', foundational, presidential_popular_legitimacy_balanced_by_assembly_confidence).
narrative_ontology:cs_axiom_status(presidential_popular_legitimacy_balanced_by_assembly_confidence, holdable).
narrative_ontology:cs_axiom_grounding('8677d927-9b7c-4d32-8077-4adb8f53012d', presidential_popular_legitimacy_balanced_by_assembly_confidence, conventional).
narrative_ontology:cs_reference_frame('8677d927-9b7c-4d32-8077-4adb8f53012d', constitutional_balance_of_powers).
narrative_ontology:cs_drift_state('8677d927-9b7c-4d32-8077-4adb8f53012d', contemporary_political_practice, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('8677d927-9b7c-4d32-8077-4adb8f53012d', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, president_of_france).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister_of_france).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly_majority).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, political_stability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds significant powers in foreign policy and defense, but must negotiate with the Prime Minister and National Assembly for domestic policy when cohabitation occurs. Benefits from the constitutional design that grants direct popular legitimacy.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, president_of_france, agenda_setter,
    institutional, biographical, constrained, national).

% Leads the government and is responsible for domestic policy, requiring the confidence of the National Assembly. During cohabitation, the Prime Minister's power is enhanced in domestic affairs, often at the expense of presidential influence.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister_of_france, agenda_setter,
    institutional, biographical, constrained, national).

% The parliamentary majority that supports the Prime Minister. Benefits from the ability to shape domestic policy and hold the government accountable, especially during periods of cohabitation where its influence over the executive is maximized.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly_majority, beneficiary,
    organized, biographical, mobile, national).

% Suffers from the divided executive authority during cohabitation, leading to potential gridlock, conflicting policy signals, and slower decision-making, particularly in areas where presidential and prime ministerial competencies overlap.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_non_agent(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence).

% Can be undermined by the inherent tensions and power struggles of cohabitation, leading to increased political maneuvering, public confusion, and a perception of governmental weakness or inefficiency.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, political_stability, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_non_agent(fifth_republic_constitution__cohabitation_equilibrium_reading, political_stability).

% Participates in elections that determine both the President and the National Assembly, thereby indirectly shaping the likelihood and dynamics of cohabitation. Experiences the outcomes of negotiated governance, sometimes valuing the checks and balances, other times frustrated by perceived inefficiency.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, french_electorate, observer,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the exercise of executive power between a directly elected President and a Prime Minister accountable to the National Assembly, particularly when they come from different political majorities, ensuring continuity of government despite divided mandates.
% TRANSFER_FUNCTION: Transfers authority and policy initiative between the President and Prime Minister depending on the political context, with the President typically retaining foreign policy and defense, and the Prime Minister leading domestic policy. This transfer is negotiated and often contested.
% ABSENT_VOICES: A purely parliamentary system advocate would argue for a single, unified executive accountable solely to the legislature, eliminating the inherent tensions of dual executive power. A purely presidential system advocate would argue for a single, strong executive unconstrained by legislative majorities.
% DISAPPEARANCE_RATIONALE: If the constitutional requirement for negotiated authority allocation vanished, the Fifth Republic's political system would fundamentally change. Either the President would become supreme, or the Prime Minister and National Assembly would dominate, leading to a different balance of power and governance outcomes.
% FOUNDING_PROBLEM: The instability and governmental paralysis of the Fourth Republic, characterized by frequent changes of government and weak executive authority, which led to a desire for a stronger, more stable executive.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political scientists widely corroborate the founding problem, citing the chronic instability of the Fourth Republic. The French electorate's continued preference for a strong executive, even during cohabitation, also supports the ongoing relevance of stability concerns.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__cohabitation_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__cohabitation_equilibrium_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.45) is moderate, reflecting the friction and occasional paralysis that can arise from divided executive power, leading to costs in policy coherence and efficiency. Suppression (0.30) is relatively low, as the system is designed to allow for contestation and negotiation rather than outright suppression of one branch by another. Theater ratio (0.10) is also low, as the negotiations and power struggles are genuine and functional, not merely performative. The claimed type is 'tangled_rope' because it genuinely coordinates executive power but also generates asymmetric costs (policy incoherence) that require active enforcement (constitutional mechanisms, political maneuvering) to manage.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the President or Prime Minister during cohabitation, the constraint is a necessary framework for governance, albeit one that requires significant political skill. From the perspective of policy coherence or political stability, it can appear as an extractive mechanism that imposes costs on the nation for the sake of maintaining a complex power-sharing arrangement. The French electorate's view oscillates between appreciating the checks and balances and being frustrated by perceived inefficiency.
 *
 * DIRECTIONALITY LOGIC:
 *   The President and Prime Minister are both agenda-setters and beneficiaries, as they each gain significant power within their respective domains, but also bear costs through the need for negotiation. The National Assembly majority benefits from its enhanced role in domestic policy during cohabitation. Policy coherence and political stability are victims, as they are directly undermined by the inherent tensions of the system. The French electorate is an observer, experiencing the outcomes.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to ensure stable governance after the Fourth Republic's instability) is still live, but its operation during cohabitation introduces new forms of instability (policy incoherence). The classification as a 'tangled_rope' prevents mislabeling it as a 'rope' (ignoring the extraction from policy coherence) or a 'snare' (ignoring the genuine coordination function). The system's persistence is due to the constitutional design and the political actors' ability to navigate its complexities, rather than pure inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cohabitation_efficiency_tradeoff,
    'Is the policy incoherence and reduced efficiency during cohabitation an acceptable cost for the enhanced checks and balances and democratic representation it provides?',
    'Comparative analysis of policy outcomes and public satisfaction in periods of cohabitation versus unified government, alongside normative evaluation of democratic values.',
    'If the costs are deemed unacceptable, the constraint would lean more towards a Snare for policy coherence; if acceptable, it would reinforce the Tangled Rope classification as a necessary, albeit costly, coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohabitation_efficiency_tradeoff, preference, 'Trade-off between efficiency and democratic checks during cohabitation.').

omega_variable(
    constitutional_interpretation_drift,
    'To what extent has the actual practice of cohabitation shifted the constitutional interpretation away from its original intent, either towards a more presidential or more parliamentary system?',
    'Legal and political science analysis of constitutional court rulings, executive decrees, and legislative practices over time, comparing them to the original constitutional debates.',
    'If practice has significantly drifted towards one extreme, the ''cohabitation equilibrium'' reading might become less empirically grounded, potentially leading to a reclassification towards a ''hyper-presidential'' or ''parliamentary constraint'' type, or highlighting a growing ''theater_ratio'' if the equilibrium is merely performative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_interpretation_drift, empirical, 'Drift in constitutional interpretation due to cohabitation practice.').

omega_variable(
    reading_naturalness_vs_construction,
    'Is this ''cohabitation equilibrium'' reading a natural outcome of the constitutional text, or a political construction that has been actively maintained by actors to manage divided government?',
    'Analysis of political actors'' statements, constitutional debates, and historical precedents to determine the degree of intentionality and active maintenance versus emergent property.',
    'If it''s primarily a political construction, its persistence is more dependent on active enforcement and less on inherent constitutional design, potentially increasing its ''suppression'' metric and reinforcing its ''tangled_rope'' nature. If it''s a natural outcome, its ''extractiveness'' might be seen as an unavoidable feature of the system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_naturalness_vs_construction, conceptual, 'Naturalness vs. constructedness of the cohabitation equilibrium.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__cohabitation_equilibrium_reading, 1986, 2022).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t1986, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1986, 0.08).
narrative_ontology:measurement(fift_tr_t1993, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1993, 0.12).
narrative_ontology:measurement(fift_tr_t1997, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1997, 0.1).
narrative_ontology:measurement(fift_tr_t2002, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2002, 0.07).
narrative_ontology:measurement(fift_tr_t2012, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2012, 0.09).
narrative_ontology:measurement(fift_tr_t2022, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2022, 0.1).

% Extraction over time
narrative_ontology:measurement(fift_be_t1986, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1986, 0.4).
narrative_ontology:measurement(fift_be_t1993, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1993, 0.5).
narrative_ontology:measurement(fift_be_t1997, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1997, 0.48).
narrative_ontology:measurement(fift_be_t2002, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2002, 0.42).
narrative_ontology:measurement(fift_be_t2012, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2012, 0.45).
narrative_ontology:measurement(fift_be_t2022, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2022, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t1986, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1986, 0.3).
narrative_ontology:measurement(fift_su_t1993, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1993, 0.35).
narrative_ontology:measurement(fift_su_t1997, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1997, 0.32).
narrative_ontology:measurement(fift_su_t2002, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2002, 0.28).
narrative_ontology:measurement(fift_su_t2012, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2012, 0.3).
narrative_ontology:measurement(fift_su_t2022, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2022, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__cohabitation_equilibrium_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Fifth Republic Constitution kernel. It focuses on the cohabitation equilibrium, distinct from hyper-presidential or parliamentary constraint readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
