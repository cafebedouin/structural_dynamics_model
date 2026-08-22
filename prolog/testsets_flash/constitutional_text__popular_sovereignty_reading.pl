% ============================================================================
% CONSTRAINT STORY: constitutional_text__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__popular_sovereignty_reading, []).

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
 *   constraint_id: constitutional_text__popular_sovereignty_reading
 *   human_readable: Constitutional Text (Popular Sovereignty Reading)
 *   domain: constitutional_theory/political_philosophy/comparative_law
 *
 * SUMMARY:
 *   This constraint represents the 'popular sovereignty' reading of a
 *   constitutional text, where the ultimate authority for constitutional
 *   interpretation and change resides with the people (the demos), rather
 *   than with courts or legislatures. It views the constitution as a living
 *   document whose meaning is ultimately determined by popular will,
 *   expressed through various means including amendment, convention, or even
 *   revolutionary action. The constraint is claimed as a Rope, as it aims to
 *   coordinate collective self-governance, but its metrics reflect a degree
 *   of extraction from institutional stability and a need for active popular
 *   resistance to maintain its principles.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__popular_sovereignty_reading, 0.3).
domain_priors:suppression_score(constitutional_text__popular_sovereignty_reading, 0.2).
domain_priors:theater_ratio(constitutional_text__popular_sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__popular_sovereignty_reading, rope).
narrative_ontology:human_readable(constitutional_text__popular_sovereignty_reading, "Constitutional Text (Popular Sovereignty Reading)").
narrative_ontology:topic_domain(constitutional_text__popular_sovereignty_reading, "constitutional_theory/political_philosophy/comparative_law").

domain_priors:requires_active_enforcement(constitutional_text__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__popular_sovereignty_reading, '99a4680c-e0d2-4890-a171-7786130f6eb6').
narrative_ontology:cs_kernel_codification('99a4680c-e0d2-4890-a171-7786130f6eb6', fixed_text).
narrative_ontology:cs_authority_grounding('99a4680c-e0d2-4890-a171-7786130f6eb6', lineage).
narrative_ontology:cs_interpretation_layer_present('99a4680c-e0d2-4890-a171-7786130f6eb6').
narrative_ontology:cs_reading_relation('99a4680c-e0d2-4890-a171-7786130f6eb6', constitutional_text__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('99a4680c-e0d2-4890-a171-7786130f6eb6', constitutional_text__legislative_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('99a4680c-e0d2-4890-a171-7786130f6eb6', foundational, ultimate_authority_resides_with_the_people).
narrative_ontology:cs_axiom_status(ultimate_authority_resides_with_the_people, holdable).
narrative_ontology:cs_axiom_grounding('99a4680c-e0d2-4890-a171-7786130f6eb6', ultimate_authority_resides_with_the_people, deontological).
narrative_ontology:cs_axiom('99a4680c-e0d2-4890-a171-7786130f6eb6', secondary, constitutional_meaning_is_dynamic).
narrative_ontology:cs_axiom_status(constitutional_meaning_is_dynamic, holdable).
narrative_ontology:cs_axiom_grounding('99a4680c-e0d2-4890-a171-7786130f6eb6', constitutional_meaning_is_dynamic, conventional).
narrative_ontology:cs_reference_frame('99a4680c-e0d2-4890-a171-7786130f6eb6', constituent_power_of_the_demos).
narrative_ontology:cs_drift_state('99a4680c-e0d2-4890-a171-7786130f6eb6', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('99a4680c-e0d2-4890-a171-7786130f6eb6', '').
narrative_ontology:cs_kernel_id(constitutional_text__popular_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, democratic_participation).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, the_demos).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, institutional_stability).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, judicial_expertise).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, legislative_efficiency).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, constitutional_courts).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, legislature).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate source of constitutional authority, capable of amending, re-interpreting, or even revolutionizing the constitutional order. Benefits from the flexibility and responsiveness of the constitution to popular will.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, the_demos, agenda_setter,
    institutional, generational, mobile, national).

% The active engagement of citizens in shaping constitutional meaning, through various forms of political action, referenda, or social movements. Directly benefits from the recognition of popular sovereignty.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, democratic_participation, beneficiary,
    organized, biographical, mobile, national).

% Interpret the constitution but are ultimately subordinate to the people's will. Their interpretations are provisional and subject to popular override. Bears the cost of potentially having their rulings overturned or ignored by popular movements.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, constitutional_courts, payer,
    institutional, generational, constrained, national).

% Enacts laws but is also subordinate to the people's ultimate interpretive authority. Its legislative power is bounded by the popular understanding of the constitution. Bears the cost of potential popular resistance or constitutional amendments that limit its power.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, legislature, payer,
    institutional, biographical, constrained, national).

% The predictability and endurance of constitutional structures. This reading, by emphasizing popular override, introduces a degree of instability compared to readings that prioritize institutional finality.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, institutional_stability, payer,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(constitutional_text__popular_sovereignty_reading, institutional_stability).

% The specialized knowledge and experience of judges in constitutional interpretation. This reading de-emphasizes the finality of judicial pronouncements, potentially reducing the perceived value of this expertise.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, judicial_expertise, payer,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(constitutional_text__popular_sovereignty_reading, judicial_expertise).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for collective self-governance where the ultimate authority rests with the people, ensuring that constitutional meaning can evolve with societal values and prevent institutional capture.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority from institutional actors (courts, legislature) to the broader populace, enabling popular will to shape constitutional meaning, while imposing costs on institutional stability and expert authority.
% ABSENT_VOICES: Those who advocate for strict judicial supremacy or parliamentary sovereignty would argue that this reading introduces dangerous instability and undermines the rule of law by politicizing constitutional interpretation. They are often present in academic discourse but structurally excluded from the 'ultimate authority' role in this reading.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the constitutional order would likely default to either judicial or legislative supremacy, fundamentally altering the balance of power and the mechanisms for constitutional change. Popular movements would lose a key legitimating narrative for extra-institutional action.
% FOUNDING_PROBLEM: To prevent tyranny and ensure that government remains accountable to the governed, by establishing the people as the ultimate source of legitimate power, even over the constitutional text itself.
% FOUNDING_PROBLEM_CORROBORATION: Historians of democratic revolutions and political theorists attest to the enduring problem of institutional overreach and the need for popular checks. Contemporary social movements and constitutional reform advocates also corroborate the ongoing relevance of popular sovereignty against entrenched elites.
narrative_ontology:disappearance_verdict(constitutional_text__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__popular_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__popular_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_text__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__popular_sovereignty_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__popular_sovereignty_reading_tests).
:- end_tests(constitutional_text__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.3) because while it empowers the people, it can impose costs on institutional actors by challenging their authority. Suppression is low (0.2) as this reading inherently resists top-down enforcement and relies on popular agency. Theater ratio is low (0.1) because the emphasis is on genuine popular engagement, not mere performance. Accessibility collapse is moderate (0.4) as institutional alternatives (judicial or legislative supremacy) are always present but are structurally subordinated. Resistance is high (0.7) because this reading often requires active popular mobilization to assert its principles against institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Demos, this is a pure Rope, ensuring democratic control. From the perspective of institutional actors (courts, legislature), it can feel like a Snare, as their established authority is constantly under potential threat of popular challenge. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Demos and democratic participation are clear beneficiaries, as the constraint directly empowers them. Constitutional courts and the legislature are payers, as their authority is constrained and their interpretations are subject to popular override. Institutional stability and judicial expertise are also 'victims' (payers) in this reading, as their claims to finality are challenged.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    popular_will_definition,
    'How is ''the will of the people'' genuinely ascertained and distinguished from transient majorities or mobilized factions?',
    'Empirical study of constitutional conventions, referenda, and sustained social movements, alongside theoretical work on deliberative democracy and legitimate popular expression.',
    'If ''popular will'' is easily manipulated or indistinguishable from factional interests, this reading''s coordination function collapses into a Snare, as it becomes a tool for majoritarian oppression rather than genuine self-governance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(popular_will_definition, conceptual, 'Ambiguity in defining and identifying legitimate ''popular will''.').

omega_variable(
    institutional_stability_threshold,
    'At what point does the emphasis on popular override undermine necessary institutional stability and the rule of law?',
    'Comparative legal analysis of constitutional systems with strong popular sovereignty mechanisms, examining their resilience during crises and their capacity for consistent governance.',
    'If popular overrides lead to chronic instability or a breakdown of legal predictability, the constraint''s classification shifts towards a Tangled Rope or Snare, as the costs to institutional function outweigh the benefits of popular control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_stability_threshold, empirical, 'The trade-off between popular sovereignty and institutional stability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__popular_sovereignty_reading, 1787, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1787, constitutional_text__popular_sovereignty_reading, theater_ratio, 1787, 0.05).
narrative_ontology:measurement(cons_tr_t1865, constitutional_text__popular_sovereignty_reading, theater_ratio, 1865, 0.08).
narrative_ontology:measurement(cons_tr_t1930, constitutional_text__popular_sovereignty_reading, theater_ratio, 1930, 0.1).
narrative_ontology:measurement(cons_tr_t1960, constitutional_text__popular_sovereignty_reading, theater_ratio, 1960, 0.08).
narrative_ontology:measurement(cons_tr_t2000, constitutional_text__popular_sovereignty_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(cons_tr_t2024, constitutional_text__popular_sovereignty_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(cons_be_t1787, constitutional_text__popular_sovereignty_reading, base_extractiveness, 1787, 0.2).
narrative_ontology:measurement(cons_be_t1865, constitutional_text__popular_sovereignty_reading, base_extractiveness, 1865, 0.25).
narrative_ontology:measurement(cons_be_t1930, constitutional_text__popular_sovereignty_reading, base_extractiveness, 1930, 0.3).
narrative_ontology:measurement(cons_be_t1960, constitutional_text__popular_sovereignty_reading, base_extractiveness, 1960, 0.28).
narrative_ontology:measurement(cons_be_t2000, constitutional_text__popular_sovereignty_reading, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement(cons_be_t2024, constitutional_text__popular_sovereignty_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1787, constitutional_text__popular_sovereignty_reading, suppression_requirement, 1787, 0.15).
narrative_ontology:measurement(cons_su_t1865, constitutional_text__popular_sovereignty_reading, suppression_requirement, 1865, 0.2).
narrative_ontology:measurement(cons_su_t1930, constitutional_text__popular_sovereignty_reading, suppression_requirement, 1930, 0.25).
narrative_ontology:measurement(cons_su_t1960, constitutional_text__popular_sovereignty_reading, suppression_requirement, 1960, 0.2).
narrative_ontology:measurement(cons_su_t2000, constitutional_text__popular_sovereignty_reading, suppression_requirement, 2000, 0.2).
narrative_ontology:measurement(cons_su_t2024, constitutional_text__popular_sovereignty_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__popular_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, constitutional_text__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, constitutional_text__legislative_sovereignty_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
