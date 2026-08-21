% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__structural_contraction_reading, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: nuclear_impossibility_kernel__structural_contraction_reading
 *   human_readable: Nuclear Impossibility: Structural Contraction of War
 *   domain: strategic_studies/international_relations
 *
 * SUMMARY:
 *   This constraint describes the physical impossibility of rational military
 *   victory in a nuclear exchange, leading to a structural contraction of the
 *   set of reachable strategic outcomes. It is a reading of the
 *   'nuclear_impossibility_kernel', focusing on the objective, physical
 *   limits imposed by mutual annihilation. This reading asserts that war, as
 *   a means to achieve political objectives, has fundamentally exited the
 *   reachable set for nuclear-armed states, not merely become too costly.
 *   Proxy wars are seen as a substitution for direct conflict, not a
 *   continuation of it by other means.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__structural_contraction_reading, 0.05).
domain_priors:suppression_score(nuclear_impossibility_kernel__structural_contraction_reading, 0.98).
domain_priors:theater_ratio(nuclear_impossibility_kernel__structural_contraction_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, accessibility_collapse, 0.99).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__structural_contraction_reading, mountain).
narrative_ontology:human_readable(nuclear_impossibility_kernel__structural_contraction_reading, "Nuclear Impossibility: Structural Contraction of War").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__structural_contraction_reading, "strategic_studies/international_relations").

domain_priors:emerges_naturally(nuclear_impossibility_kernel__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__structural_contraction_reading, 'c0cbc765-6181-454e-9724-b6dad51f8f59').
narrative_ontology:cs_kernel_codification('c0cbc765-6181-454e-9724-b6dad51f8f59', implicit).
narrative_ontology:cs_authority_grounding('c0cbc765-6181-454e-9724-b6dad51f8f59', self_enforcing).
narrative_ontology:cs_reading_relation('c0cbc765-6181-454e-9724-b6dad51f8f59', nuclear_impossibility_kernel__rational_dropout_reading, influences).
narrative_ontology:cs_reading_relation('c0cbc765-6181-454e-9724-b6dad51f8f59', nuclear_impossibility_kernel__credibility_paradox_reading, influences).
narrative_ontology:cs_axiom('c0cbc765-6181-454e-9724-b6dad51f8f59', foundational, mutual_annihilation_is_guaranteed).
narrative_ontology:cs_axiom_status(mutual_annihilation_is_guaranteed, holdable).
narrative_ontology:cs_axiom_grounding('c0cbc765-6181-454e-9724-b6dad51f8f59', mutual_annihilation_is_guaranteed, empirically_contingent).
narrative_ontology:cs_axiom('c0cbc765-6181-454e-9724-b6dad51f8f59', foundational, war_as_rational_policy_is_foreclosed).
narrative_ontology:cs_axiom_status(war_as_rational_policy_is_foreclosed, holdable).
narrative_ontology:cs_axiom_grounding('c0cbc765-6181-454e-9724-b6dad51f8f59', war_as_rational_policy_is_foreclosed, deontological).
narrative_ontology:cs_reference_frame('c0cbc765-6181-454e-9724-b6dad51f8f59', pre_nuclear_warfare).
narrative_ontology:cs_drift_state('c0cbc765-6181-454e-9724-b6dad51f8f59', post_nuclear_proliferation, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('c0cbc765-6181-454e-9724-b6dad51f8f59', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, humanity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, non_nuclear_states).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_powers).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__structural_contraction_reading, mutual_assured_destruction_doctrine).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_winter_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the existential cost of maintaining nuclear arsenals and the constant risk of accidental war, but are also locked into the system by the perceived need for deterrence. Their strategic options are severely constrained by the impossibility of victory.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_powers, payer,
    institutional, generational, identity_locked, global).

% Benefit from the absence of large-scale conventional wars between major powers, but live under the constant threat of nuclear escalation. Their security is indirectly tied to the stability of nuclear deterrence.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, non_nuclear_states, beneficiary,
    moderate, generational, constrained, global).

% Benefits from the structural impossibility of total war, which prevents species-level extinction. However, it is also trapped by the existential risk posed by the mere existence of nuclear weapons, with no collective exit option.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, humanity, beneficiary,
    powerless, civilizational, trapped, universal).

% Analyze the implications of nuclear weapons for international relations, developing theories of deterrence and arms control. They observe the constraint's effects and attempt to model its dynamics.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, strategic_theorists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Forces a de facto coordination among nuclear powers to avoid direct military confrontation, as any such conflict risks escalation to mutual annihilation.
% TRANSFER_FUNCTION: Transfers the possibility of large-scale conventional war between major powers out of the realm of rational action, effectively 'costing' the option of military victory.
% ABSENT_VOICES: Historical military strategists who believed in the possibility of decisive victory through conventional means; they would argue that the 'impossibility' is a temporary political construct, not a physical limit.
% DISAPPEARANCE_RATIONALE: If the physical impossibility of victory vanished (e.g., through a perfect defense or a weapon that only affected one side), the global strategic landscape would fundamentally rearrange, likely leading to a new era of great power conflict and potentially large-scale conventional warfare.
% FOUNDING_PROBLEM: The problem of preventing total war and ensuring the survival of humanity in an era of weapons of mass destruction.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing existence of nuclear arsenals, the continued investment in deterrence strategies by nuclear powers, and the persistent global concern about nuclear proliferation all corroborate that the founding problem remains live. International bodies and non-proliferation advocates consistently attest to this from outside the benefiting parties.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__structural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__structural_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__structural_contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(nuclear_impossibility_kernel__structural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__structural_contraction_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__structural_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(nuclear_impossibility_kernel__structural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(nuclear_impossibility_kernel__structural_contraction_reading),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(nuclear_impossibility_kernel__structural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.05) because the constraint primarily extracts the 'option' of total war, which is a net benefit for humanity. Suppression is extremely high (0.98) because the physical reality of nuclear weapons fundamentally suppresses any rational path to victory. Theater ratio is negligible (0.01) as the impossibility is a physical fact, not a performance. Accessibility collapse is near total (0.99) as the alternative of 'winning a nuclear war' is physically foreclosed. Resistance is minimal (0.02) because the physical reality is largely uncontested, though some actors may resist the implications.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of nuclear powers, the constraint imposes a profound strategic dilemma and a constant burden of risk management. From the perspective of non-nuclear states and humanity, it is a terrifying but ultimately life-preserving physical limit. The engine's classification will reflect this divergence, with nuclear powers experiencing a form of 'payer' status despite the overall 'mountain' nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear powers are 'payers' in the sense that they bear the immense costs and risks of maintaining arsenals without the possibility of achieving traditional military victory. Non-nuclear states and humanity are 'beneficiaries' as they are spared from large-scale conventional wars between major powers and potential extinction, respectively. The constraint subsidizes the survival of all, even as it imposes existential risk.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_vs_rational_impossibility,
    'Is the impossibility of victory a purely physical constraint (mutual annihilation), or is it primarily a rational-choice constraint (costs outweigh benefits, but victory is still physically possible)?',
    'Analysis of post-nuclear exchange scenarios: if any scenario allows for a ''winner'' with acceptable costs, it leans towards rational-choice; if all scenarios lead to unacceptable, irreversible destruction for all, it''s physical.',
    'If purely physical, the constraint is a stronger Mountain. If primarily rational-choice, it might lean towards a Rope or even a Snare if the ''costs'' are manipulated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(physical_vs_rational_impossibility, conceptual, 'Distinguishing between physical and rational impossibility of victory.').

omega_variable(
    proxy_war_substitution_vs_continuation,
    'Are proxy wars a genuine substitution for direct great power conflict (implying direct war is foreclosed), or merely a continuation of great power competition by other means (implying direct war is merely too costly)?',
    'Historical analysis of conflict patterns before and after nuclear proliferation, focusing on the strategic objectives and risk tolerance of great powers in proxy conflicts.',
    'If substitution, it reinforces the ''structural contraction'' reading. If continuation, it weakens the claim of physical impossibility, suggesting a rational-dropout scenario.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_war_substitution_vs_continuation, empirical, 'Nature of proxy wars in the nuclear age.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__structural_contraction_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1945, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1945, 0.01).
narrative_ontology:measurement(nucl_tr_t1962, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1962, 0.01).
narrative_ontology:measurement(nucl_tr_t1989, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1989, 0.01).
narrative_ontology:measurement(nucl_tr_t2024, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 2024, 0.01).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1945, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1945, 0.01).
narrative_ontology:measurement(nucl_be_t1962, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1962, 0.03).
narrative_ontology:measurement(nucl_be_t1989, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1989, 0.02).
narrative_ontology:measurement(nucl_be_t2024, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1945, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1945, 0.9).
narrative_ontology:measurement(nucl_su_t1962, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1962, 0.95).
narrative_ontology:measurement(nucl_su_t1989, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1989, 0.92).
narrative_ontology:measurement(nucl_su_t2024, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 2024, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__structural_contraction_reading, global_infrastructure).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel__rational_dropout_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel__credibility_paradox_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'nuclear_impossibility_kernel'. This reading emphasizes the physical impossibility of victory, while the 'rational_dropout_reading' focuses on prohibitive costs, and the 'credibility_paradox_reading' on the inherent incredibility of nuclear threats. All three are distinct but related constraints stemming from the same core phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
