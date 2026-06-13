% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__contraction_reading, []).

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
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: honor_violence_legitimacy__contraction_reading
 *   human_readable: Honor Redefined to Exclude Violence (Contraction Reading)
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes the historical process by which the concept of
 *   'honor' was redefined in Western societies, specifically to exclude
 *   dueling and other forms of private violence as legitimate means of
 *   defense. This 'contraction reading' emphasizes the internal conceptual
 *   shift, rather than external costs, as the primary driver for dueling's
 *   decline. The constraint is claimed as a Mountain because the redefinition
 *   of honor became an internalized, self-enforcing social fact, making
 *   dueling structurally unthinkable for those operating within the new honor
 *   code. While there are beneficiaries (social order, state), the constraint
 *   itself is presented as a fundamental shift in social ontology, not an
 *   extractive mechanism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, 0.1).
domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, 0.05).
domain_priors:theater_ratio(honor_violence_legitimacy__contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__contraction_reading, mountain).
narrative_ontology:human_readable(honor_violence_legitimacy__contraction_reading, "Honor Redefined to Exclude Violence (Contraction Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__contraction_reading, "historical_sociology/legal_anthropology/commitment_systems").

domain_priors:emerges_naturally(honor_violence_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__contraction_reading, 'ca0adb93-355e-47b7-9c4b-a3217b925b7c').
narrative_ontology:cs_kernel_codification('ca0adb93-355e-47b7-9c4b-a3217b925b7c', implicit).
narrative_ontology:cs_authority_grounding('ca0adb93-355e-47b7-9c4b-a3217b925b7c', practice).
narrative_ontology:cs_interpretation_layer_present('ca0adb93-355e-47b7-9c4b-a3217b925b7c').
narrative_ontology:cs_reading_relation('ca0adb93-355e-47b7-9c4b-a3217b925b7c', honor_violence_legitimacy__drop_reading, influences).
narrative_ontology:cs_reading_relation('ca0adb93-355e-47b7-9c4b-a3217b925b7c', honor_violence_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('ca0adb93-355e-47b7-9c4b-a3217b925b7c', foundational, honor_excludes_private_violence).
narrative_ontology:cs_axiom_status(honor_excludes_private_violence, holdable).
narrative_ontology:cs_axiom_grounding('ca0adb93-355e-47b7-9c4b-a3217b925b7c', honor_excludes_private_violence, deontological).
narrative_ontology:cs_axiom('ca0adb93-355e-47b7-9c4b-a3217b925b7c', secondary, state_monopoly_on_violence_is_legitimate).
narrative_ontology:cs_axiom_status(state_monopoly_on_violence_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('ca0adb93-355e-47b7-9c4b-a3217b925b7c', state_monopoly_on_violence_is_legitimate, conventional).
narrative_ontology:cs_reference_frame('ca0adb93-355e-47b7-9c4b-a3217b925b7c', honor_as_non_violent_resolution).
narrative_ontology:cs_drift_state('ca0adb93-355e-47b7-9c4b-a3217b925b7c', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ca0adb93-355e-47b7-9c4b-a3217b925b7c', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, social_order).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, state_monopoly_on_violence).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, gentlemen_of_honor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically, these individuals were bound by a code that often necessitated dueling to defend honor. As the definition of honor shifted, they were compelled to internalize new norms, making dueling unthinkable without losing social standing. Their identity as 'men of honor' became fused with non-violent resolution.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, gentlemen_of_honor, payer,
    powerful, biographical, identity_locked, national).

% The state actively promoted a redefinition of honor that aligned with its monopoly on violence, gradually criminalizing dueling and providing alternative legal avenues for dispute resolution. It benefited from increased social stability and consolidated its authority.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, state_legal_system, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefited from the reduction of private violence and the increased predictability of social interactions. The redefinition of honor contributed to a more stable and less violent public sphere.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, social_order, beneficiary,
    organized, generational, analytical, national).

% Intellectuals, writers, and moral reformers who actively championed new ideals of honor, emphasizing restraint, civility, and legal recourse over personal combat. They shaped public discourse and influenced the redefinition of social norms.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, cultural_elites, agenda_setter,
    powerful, generational, mobile, national).

% Individuals or small groups who continued to uphold the older, violent code of honor, finding themselves increasingly marginalized, ridiculed, or legally persecuted. Their voices were systematically excluded from the dominant discourse on honor.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, dueling_tradition_adherents, excluded,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social expectations around honor, providing a shared understanding of what constitutes honorable conduct and how grievances are legitimately resolved, thereby reducing arbitrary violence.
% TRANSFER_FUNCTION: Transfers the right to adjudicate honor disputes from individuals to the state and other social institutions, and shifts the social cost of violence from private individuals to the collective legal system.
% ABSENT_VOICES: Adherents to the older dueling tradition, who would argue that true honor *requires* personal defense through combat, were increasingly silenced and excluded from the public discourse that redefined honor.
% DISAPPEARANCE_RATIONALE: The redefinition of honor has become deeply embedded in social norms and legal structures. If this constraint vanished, the underlying conceptual shift would remain, and dueling would not spontaneously re-emerge as a legitimate practice. The world would not revert to a pre-redefinition state.
% FOUNDING_PROBLEM: The problem of endemic private violence and challenges to state authority posed by the practice of dueling among elites.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars widely corroborate that the problem of dueling as a widespread social practice is dead. While individual acts of violence persist, the cultural legitimacy of dueling for honor has been extinguished, as evidenced by legal codes and social histories from outside the direct beneficiaries.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__contraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_violence_legitimacy__contraction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__contraction_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_violence_legitimacy__contraction_reading),
    narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_violence_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very low (0.1 declining to 0.01) because the constraint primarily represents a conceptual shift that became self-enforcing, rather than an ongoing extraction. Suppression is also low (0.05 declining to 0.01) as the constraint's persistence relies on internalized norms, not active coercion. Theater ratio is 0.0, as there is no performative maintenance; the shift is genuine. Accessibility collapse is high (0.9) because dueling became conceptually unavailable as an option for honorable men. Resistance is low (0.05) because the conceptual shift was largely successful, with only marginalized groups resisting.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state and cultural elites, this was a necessary and beneficial evolution of social norms. From the perspective of those bound by the old honor code, it represented a loss of a traditional means of redress, though eventually internalized. The engine's classification will reflect the low extraction and high accessibility collapse, consistent with a Mountain, even with identifiable beneficiaries, because the core mechanism is a conceptual shift, not an extractive structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'gentlemen of honor' are payers because they bear the cost of adapting their identity and behavior to the new honor code, losing the option of dueling. The 'state legal system' and 'social order' are beneficiaries, gaining stability and authority. 'Cultural elites' are agenda-setters, actively shaping the new definition. 'Dueling tradition adherents' are excluded, as their worldview is no longer recognized as legitimate.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_vs_material_causation,
    'To what extent was the decline of dueling primarily driven by the conceptual redefinition of honor (internal), versus the rising external costs and legal penalties (material)?',
    'Comparative historical analysis across different regions and legal systems, examining the timing and sequence of conceptual shifts versus legal/economic disincentives.',
    'If material costs were dominant, the constraint would be less of a Mountain and more of a Snare (due to external suppression), or a Rope (if the costs were genuinely coordinating). If conceptual redefinition was dominant, the Mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_vs_material_causation, empirical, 'Distinguishing internal conceptual change from external material pressures in the decline of dueling.').

omega_variable(
    identity_lock_mechanism,
    'Was the ''identity_locked'' exit option for gentlemen of honor primarily due to internal psychological shifts, or external social pressure to conform to the new honor code?',
    'Analysis of personal diaries, letters, and contemporary psychological treatises to discern the internal experience of honor and shame, alongside sociological studies of social conformity mechanisms.',
    'If primarily internal, the constraint''s ''naturalness'' is stronger. If primarily external social pressure, the ''suppression'' metric might be understated, and the constraint leans more towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Understanding the mechanism of identity lock-in for the new honor code.').

omega_variable(
    false_summit_ambiguity,
    'Is this constraint a genuine natural law (a fundamental shift in social ontology), or a constructed constraint that benefits identifiable agents (state, social order) and is merely presented as natural?',
    'Examination of the historical agency of the beneficiaries: did they actively ''construct'' the redefinition for their benefit, or did they merely align with an emergent social fact? This requires deep historical and sociological analysis of power dynamics.',
    'If primarily constructed for benefit, the classification would shift from Mountain to Tangled Rope or Snare, as the ''naturalness'' claim would be a cover for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_ambiguity, conceptual, 'Ambiguity between genuine social evolution and constructed benefit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__contraction_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_violence_legitimacy__contraction_reading, theater_ratio, 1700, 0.0).
narrative_ontology:measurement(hono_tr_t1750, honor_violence_legitimacy__contraction_reading, theater_ratio, 1750, 0.0).
narrative_ontology:measurement(hono_tr_t1800, honor_violence_legitimacy__contraction_reading, theater_ratio, 1800, 0.0).
narrative_ontology:measurement(hono_tr_t1850, honor_violence_legitimacy__contraction_reading, theater_ratio, 1850, 0.0).
narrative_ontology:measurement(hono_tr_t1900, honor_violence_legitimacy__contraction_reading, theater_ratio, 1900, 0.0).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1700, 0.1).
narrative_ontology:measurement(hono_be_t1750, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1750, 0.08).
narrative_ontology:measurement(hono_be_t1800, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1800, 0.05).
narrative_ontology:measurement(hono_be_t1850, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1850, 0.02).
narrative_ontology:measurement(hono_be_t1900, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1900, 0.01).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1700, 0.05).
narrative_ontology:measurement(hono_su_t1750, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1750, 0.04).
narrative_ontology:measurement(hono_su_t1800, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1800, 0.03).
narrative_ontology:measurement(hono_su_t1850, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1850, 0.02).
narrative_ontology:measurement(hono_su_t1900, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1900, 0.01).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'honor_violence_legitimacy' kernel. This 'contraction reading' emphasizes the internal conceptual redefinition of honor as the primary driver for dueling's decline, distinct from the 'drop reading' (external costs) and 'composite reading' (both factors).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
