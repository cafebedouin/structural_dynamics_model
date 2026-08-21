% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__nuclear_taboo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__nuclear_taboo_reading, []).

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
 *   constraint_id: total_war_possibility_space__nuclear_taboo_reading
 *   human_readable: Normative Prohibition of Total War (Nuclear Taboo Reading)
 *   domain: international_relations/strategic_studies/institutional_history
 *
 * SUMMARY:
 *   This constraint describes the normative prohibition against total war,
 *   specifically nuclear war, as a constructed taboo. It argues that this
 *   taboo, rather than purely material deterrence, significantly limits the
 *   possibility space for major power conflict. The constraint is actively
 *   maintained through international norms, institutions, and diplomatic
 *   pressure, imposing costs on those who might violate it or seek to acquire
 *   nuclear weapons. This is one reading of the 'total_war_possibility_space'
 *   kernel, focusing on the normative dimension.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, 0.7).
domain_priors:suppression_score(total_war_possibility_space__nuclear_taboo_reading, 0.85).
domain_priors:theater_ratio(total_war_possibility_space__nuclear_taboo_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__nuclear_taboo_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__nuclear_taboo_reading, "Normative Prohibition of Total War (Nuclear Taboo Reading)").
narrative_ontology:topic_domain(total_war_possibility_space__nuclear_taboo_reading, "international_relations/strategic_studies/institutional_history").

domain_priors:requires_active_enforcement(total_war_possibility_space__nuclear_taboo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__nuclear_taboo_reading, '5f399f2a-a01c-4957-8b5a-042776b167aa').
narrative_ontology:cs_kernel_codification('5f399f2a-a01c-4957-8b5a-042776b167aa', implicit).
narrative_ontology:cs_authority_grounding('5f399f2a-a01c-4957-8b5a-042776b167aa', practice).
narrative_ontology:cs_interpretation_layer_present('5f399f2a-a01c-4957-8b5a-042776b167aa').
narrative_ontology:cs_reading_relation('5f399f2a-a01c-4957-8b5a-042776b167aa', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('5f399f2a-a01c-4957-8b5a-042776b167aa', total_war_possibility_space__space_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('5f399f2a-a01c-4957-8b5a-042776b167aa', foundational, nuclear_use_is_unthinkable).
narrative_ontology:cs_axiom_status(nuclear_use_is_unthinkable, holdable).
narrative_ontology:cs_axiom_grounding('5f399f2a-a01c-4957-8b5a-042776b167aa', nuclear_use_is_unthinkable, deontological).
narrative_ontology:cs_axiom('5f399f2a-a01c-4957-8b5a-042776b167aa', secondary, total_war_is_obsolete).
narrative_ontology:cs_axiom_status(total_war_is_obsolete, holdable).
narrative_ontology:cs_axiom_grounding('5f399f2a-a01c-4957-8b5a-042776b167aa', total_war_is_obsolete, conventional).
narrative_ontology:cs_reference_frame('5f399f2a-a01c-4957-8b5a-042776b167aa', post_hiroshima_normative_shift).
narrative_ontology:cs_drift_state('5f399f2a-a01c-4957-8b5a-042776b167aa', contemporary_geopolitical_challenges, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5f399f2a-a01c-4957-8b5a-042776b167aa', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, global_stability_advocates).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, nuclear_powers_strategic_planners).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, aspiring_nuclear_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the reduced risk of existential conflict and the maintenance of a stable international order. They actively promote and reinforce the nuclear taboo through diplomacy, advocacy, and international law.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, global_stability_advocates, beneficiary,
    institutional, civilizational, analytical, global).

% Bear the cost of foregone strategic options, as the taboo limits the use of their most powerful weapons. While they benefit from global stability, they must constantly navigate the tension between deterrence and the normative prohibition against use, often maintaining capabilities while publicly adhering to non-use norms.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, nuclear_powers_strategic_planners, payer,
    institutional, biographical, constrained, global).

% Benefit from the reduced threat of nuclear attack and the implicit security guarantee provided by the taboo. Their strategic autonomy is preserved by not needing to develop nuclear weapons, but they remain vulnerable to violations of the taboo by others.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states, beneficiary,
    organized, generational, constrained, global).

% Are targets of the taboo's enforcement, facing diplomatic isolation, sanctions, and potential military action if they pursue nuclear weapons. Their strategic options are severely curtailed by the international norm, making their pursuit of nuclear capabilities costly and risky.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, aspiring_nuclear_states, payer,
    powerful, immediate, trapped, regional).

% Actively administer and enforce the non-proliferation regime and other mechanisms that reinforce the nuclear taboo. They coordinate diplomatic efforts, monitor compliance, and provide platforms for norm articulation and reinforcement.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, international_organizations, agenda_setter,
    institutional, generational, analytical, global).

% Are individuals and groups (e.g., NGOs, academics) who actively work to strengthen and propagate the nuclear taboo. They shape discourse, conduct research, and lobby governments to uphold and extend the norm against nuclear weapons.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneurs, agenda_setter,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared normative boundary against the use of nuclear weapons, preventing the escalation of conflicts to total war and fostering a degree of global stability among nuclear-armed states.
% TRANSFER_FUNCTION: Transfers the strategic option of total war from the realm of thinkable policy for nuclear powers to a normatively prohibited category, imposing costs on those who might consider violating the taboo and benefiting global security.
% ABSENT_VOICES: States or non-state actors who view nuclear weapons as an indispensable deterrent for their survival, or who reject the premise of a global normative order that curtails their sovereign strategic choices. Their perspectives are often marginalized or actively suppressed by the non-proliferation regime.
% DISAPPEARANCE_RATIONALE: If the nuclear taboo vanished overnight, the strategic landscape would fundamentally shift. Nuclear proliferation would accelerate, the threshold for nuclear use would lower dramatically, and existing international security architectures (like the NPT) would collapse, leading to a highly unstable and dangerous world.
% FOUNDING_PROBLEM: The existential threat posed by nuclear weapons after their use in Hiroshima and Nagasaki, and the realization that traditional warfare concepts were inadequate for managing the risks of the nuclear age.
% FOUNDING_PROBLEM_CORROBORATION: International treaties (e.g., NPT, CTBT), UN resolutions, diplomatic statements from numerous states, and extensive academic literature across international relations, history, and ethics consistently corroborate the ongoing relevance of preventing nuclear war and the importance of the taboo.
narrative_ontology:disappearance_verdict(total_war_possibility_space__nuclear_taboo_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__nuclear_taboo_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__nuclear_taboo_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(total_war_possibility_space__nuclear_taboo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__nuclear_taboo_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.7) because the taboo forecloses a significant strategic option for nuclear powers and imposes substantial costs on aspiring nuclear states. Suppression is very high (0.85) due to the robust non-proliferation regime and the strong diplomatic and social stigma associated with nuclear use. Theater ratio is moderate (0.35) as there are genuine efforts to maintain the taboo, but also performative aspects in rhetoric that may not fully align with underlying strategic planning. Accessibility collapse is high (0.8) for major powers, as the taboo makes total war largely unthinkable, though less so for states outside the established nuclear order. Resistance is moderate (0.5) from states seeking nuclear weapons or those who challenge the norm's universality.
 *
 * PERSPECTIVAL GAP:
 *   Nuclear powers' strategic planners experience this as a necessary but costly limitation on their options, balancing deterrence with non-use. Non-nuclear states largely perceive it as a beneficial protective shield. Aspiring nuclear states experience it as a highly extractive and suppressive barrier to their perceived security needs. Global stability advocates see it as a foundational element of international order.
 *
 * DIRECTIONALITY LOGIC:
 *   Global stability advocates and non-nuclear states are beneficiaries, as the taboo reduces existential threats. Nuclear powers' strategic planners are payers, as they bear the cost of foregone options and the burden of maintaining the taboo. Aspiring nuclear states are clear victims, facing severe consequences for challenging the norm. International organizations and norm entrepreneurs act as agenda-setters, actively shaping and enforcing the taboo.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    taboo_strength_persistence,
    'How robust is the nuclear taboo in the face of renewed great power competition and technological advancements (e.g., AI in command and control)?',
    'Analysis of state rhetoric, military doctrine shifts, and actual crisis behavior over the next decade. A weakening of no-first-use pledges or increased ''saber-rattling'' would indicate erosion.',
    'If the taboo weakens, the constraint''s effective suppression and extractiveness would decrease for nuclear powers, potentially leading to a reclassification towards a ''rope'' (if coordination remains) or ''piton'' (if maintenance becomes purely theatrical).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taboo_strength_persistence, empirical, 'The resilience of the nuclear taboo against contemporary challenges.').

omega_variable(
    taboo_vs_deterrence_causality,
    'To what extent does the absence of total war stem from the nuclear taboo (normative prohibition) versus material deterrence (fear of retaliation)?',
    'Comparative historical analysis of crises where deterrence was ambiguous but the taboo held, or scenarios where the taboo was challenged but deterrence remained strong. Expert elicitation from strategic planners on their decision-making calculus.',
    'If deterrence is the primary driver, this constraint''s extractiveness (as a normative force) would be lower, and the ''deterrence_equilibrium_reading'' would be more accurate. If the taboo is primary, this reading''s classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(taboo_vs_deterrence_causality, conceptual, 'Disentangling the causal weight of normative taboo versus material deterrence in preventing total war.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of nuclear proliferation primarily structural (sanctions, military threats) or internalized (states'' self-restraint due to the norm)?',
    'Post-sanction trajectory of aspiring nuclear states: if proliferation efforts persist after structural pressures are removed, reclassify as partially internalized. Analysis of domestic political discourse in non-nuclear states regarding proliferation.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit. If purely structural, the constraint is more vulnerable to shifts in enforcement capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for nuclear proliferation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__nuclear_taboo_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1945, 0.4).
narrative_ontology:measurement(tota_tr_t1965, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1965, 0.3).
narrative_ontology:measurement(tota_tr_t1985, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement(tota_tr_t2005, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(tota_tr_t2025, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2025, 0.35).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1945, 0.5).
narrative_ontology:measurement(tota_be_t1965, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1965, 0.6).
narrative_ontology:measurement(tota_be_t1985, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1985, 0.7).
narrative_ontology:measurement(tota_be_t2005, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(tota_be_t2025, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2025, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1945, 0.6).
narrative_ontology:measurement(tota_su_t1965, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1965, 0.75).
narrative_ontology:measurement(tota_su_t1985, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1985, 0.85).
narrative_ontology:measurement(tota_su_t2005, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2005, 0.8).
narrative_ontology:measurement(tota_su_t2025, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__nuclear_taboo_reading, identity_coordination).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space__deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space__space_contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'total_war_possibility_space' kernel, focusing on the normative prohibition (taboo) as the primary mechanism. It is linked to sibling readings that emphasize material deterrence or the physical impossibility of total war.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
