% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__decline_reading, []).

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
 *   constraint_id: honor_satisfaction_mechanism__decline_reading
 *   human_readable: Honor Satisfaction Mechanism (Decline Reading)
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   This constraint story, 'honor_satisfaction_mechanism__decline_reading',
 *   is one interpretation of the broader 'honor_satisfaction_mechanism'
 *   kernel. It posits that dueling, as a mechanism for honor satisfaction,
 *   experienced a gradual decline in frequency and social acceptance, but
 *   never fully disappeared or became unthinkable. Instead, it persisted as a
 *   fringe practice, conceptually available but increasingly costly and
 *   stigmatized. This reading emphasizes the continued, albeit weakened,
 *   presence of the practice, rather than its complete disappearance or
 *   transformation. Sibling readings include 'contraction_reading' (dueling
 *   became cognitively unthinkable) and 'composite_reading' (multiple
 *   mechanisms drove its decline).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__decline_reading, 0.45).
domain_priors:suppression_score(honor_satisfaction_mechanism__decline_reading, 0.6).
domain_priors:theater_ratio(honor_satisfaction_mechanism__decline_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__decline_reading, piton).
narrative_ontology:human_readable(honor_satisfaction_mechanism__decline_reading, "Honor Satisfaction Mechanism (Decline Reading)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__decline_reading, "historical_sociology/legal_history/normative_systems").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__decline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__decline_reading, '588a2831-0669-4155-bec7-481bfd026370').
narrative_ontology:cs_kernel_codification('588a2831-0669-4155-bec7-481bfd026370', implicit).
narrative_ontology:cs_authority_grounding('588a2831-0669-4155-bec7-481bfd026370', practice).
narrative_ontology:cs_interpretation_layer_present('588a2831-0669-4155-bec7-481bfd026370').
narrative_ontology:cs_reading_relation('588a2831-0669-4155-bec7-481bfd026370', honor_satisfaction_mechanism__composite_reading, coexists_with).
narrative_ontology:cs_reading_relation('588a2831-0669-4155-bec7-481bfd026370', honor_satisfaction_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_axiom('588a2831-0669-4155-bec7-481bfd026370', foundational, honor_requires_physical_satisfaction).
narrative_ontology:cs_axiom_status(honor_requires_physical_satisfaction, holdable).
narrative_ontology:cs_axiom_grounding('588a2831-0669-4155-bec7-481bfd026370', honor_requires_physical_satisfaction, conventional).
narrative_ontology:cs_axiom('588a2831-0669-4155-bec7-481bfd026370', secondary, state_law_is_circumventable_for_honor).
narrative_ontology:cs_axiom_status(state_law_is_circumventable_for_honor, holdable).
narrative_ontology:cs_axiom_grounding('588a2831-0669-4155-bec7-481bfd026370', state_law_is_circumventable_for_honor, conventional).
narrative_ontology:cs_reference_frame('588a2831-0669-4155-bec7-481bfd026370', gentlemanly_honor_code_pre_decline).
narrative_ontology:cs_drift_state('588a2831-0669-4155-bec7-481bfd026370', late_19th_century, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('588a2831-0669-4155-bec7-481bfd026370', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, gentlemanly_class_late_period).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, duelists_late_period).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, state_legal_system).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, bourgeois_public_opinion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintained the social code of honor, which, though increasingly challenged, still provided a framework for resolving certain disputes. They benefited from the symbolic capital of adhering to tradition, even as the practice became more dangerous and less socially acceptable. Their identity was tied to this code.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, gentlemanly_class_late_period, agenda_setter,
    organized, biographical, identity_locked, national).

% Individuals who, when challenged, felt compelled to participate to maintain their social standing, despite the increasing legal and social penalties. They bore the direct costs of injury, death, and legal prosecution.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, duelists_late_period, payer,
    moderate, immediate, constrained, local).

% Actively enforced anti-dueling laws, but faced challenges due to social inertia and the difficulty of prosecuting high-status individuals. It bore the costs of enforcement and the erosion of its authority when duels occurred with impunity.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, state_legal_system, payer,
    institutional, generational, constrained, national).

% Benefited from the decline of dueling as a sign of societal progress and the triumph of rational legal order over aristocratic violence. Their norms increasingly stigmatized dueling, providing a new social framework for conflict resolution.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, bourgeois_public_opinion, beneficiary,
    organized, biographical, mobile, regional).

% Would have offered alternative mechanisms for risk management and dispute resolution, but their influence was indirect and often undermined by the persistence of honor codes. They were excluded from directly mediating honor disputes.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, insurance_companies, excluded,
    powerful, biographical, arbitrage, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a formalized, if dangerous, mechanism for gentlemen to resolve disputes and restore perceived honor, preventing open-ended feuds and maintaining social hierarchy within a specific class.
% TRANSFER_FUNCTION: Transferred social standing and perceived honor (or its loss) between individuals, often at the cost of physical harm or death, and transferred legal authority from traditional honor codes to the state.
% ABSENT_VOICES: Women, lower classes, and those who viewed dueling as barbaric violence were largely excluded from the formal discourse surrounding its legitimacy, though their moral condemnation contributed to its decline. Insurance companies would have offered alternative risk management.
% DISAPPEARANCE_RATIONALE: If the honor satisfaction mechanism vanished overnight in its late period, the gentlemanly class would have faced a crisis in how to manage perceived insults and maintain social standing, leading to a scramble for new, less violent, but equally binding social codes. The state's legal authority would have been strengthened, but the social fabric of the elite would have been disrupted.
% FOUNDING_PROBLEM: Unregulated violence and endless feuds among the elite, where personal insults could escalate without a clear, formalized path to resolution, threatening social order.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and sociologists attest that the problem of unregulated elite violence was largely superseded by the rise of state legal monopolies and changing social norms. While personal insults still occur, the specific problem dueling solved (formalized honor satisfaction) is no longer 'live' in the same way. The gentlemanly class, however, might still claim its 'live' status to justify its persistence.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__decline_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__decline_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__decline_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_satisfaction_mechanism__decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__decline_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__decline_reading_tests).
:- end_tests(honor_satisfaction_mechanism__decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the declining but still present costs borne by duelists and the state, while suppression (0.60) indicates the increasing legal and social pressure against the practice. Theater ratio (0.20) is low, as the act of dueling, though less frequent, remained a serious, high-stakes affair rather than pure performance. The decline in extractiveness over time reflects the decreasing frequency and social utility of dueling, while rising suppression reflects the state's increasing efforts to outlaw and punish it. The claimed type is 'piton' because the primary function (honor satisfaction) atrophied, but the constraint persisted due to institutional inertia and the identity-locked nature of the gentlemanly class, even as no party benefited enough to actively maintain it at its former scale.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the gentlemanly class, the mechanism, though less frequently invoked, remained a vital, if dangerous, component of their social identity. From the state's perspective, it was a persistent challenge to its legal monopoly on violence. The engine's per-seat classification would reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The gentlemanly_class_late_period, though increasingly constrained, still derived identity and social capital from the honor code, making them beneficiaries (d near 0.0-0.2). Duelists_late_period were direct targets (d near 0.8-1.0), bearing the physical and legal costs. The state_legal_system was a payer (d near 0.6-0.8), expending resources on enforcement against a declining but persistent practice. Bourgeois_public_opinion benefited from the decline of dueling, aligning with the state's efforts.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'decline_reading' suggests a mandatrophy in progress: the original problem of unregulated elite violence was largely addressed by the state, but the honor satisfaction mechanism persisted due to social inertia and identity-lock, even as its utility diminished. The classification as 'piton' captures this atrophy, preventing mislabeling it as a 'snare' (which would imply active, concentrated beneficiaries) or a 'rope' (which would imply a live coordination function). The 'dead' status of the founding problem, coupled with the 'world_rearranges' verdict, points to the constraint's inertial persistence beyond its original mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decline_vs_contraction_mechanism,
    'Did dueling merely decline in frequency and social acceptance (decline_reading), or did it become cognitively unthinkable, a category-level impossibility (contraction_reading)?',
    'Analysis of historical records for explicit moral condemnation vs. continued, albeit rare, instances of dueling among elites. Examination of literary and legal texts for shifts in the conceptual framing of dueling.',
    'If ''contraction_reading'' is correct, the constraint''s accessibility_collapse would be much higher (near 1.0), and its extractiveness would be near 0.0, reclassifying it as a ''mountain'' of social cognition. If ''decline_reading'' is correct, the current ''piton'' classification holds, reflecting its lingering presence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decline_vs_contraction_mechanism, conceptual, 'Distinguishing between a decline in practice and a cognitive contraction of the category itself.').

omega_variable(
    identity_lock_strength,
    'How strong was the identity-lock for the gentlemanly class in the late period? Was adherence to dueling truly a non-negotiable aspect of their self-concept, or merely a strong social convention?',
    'Detailed biographical studies of individuals who refused duels and their subsequent social standing. Analysis of internal class debates and memoirs regarding the changing nature of honor.',
    'If identity-lock was weaker, the exit_options for the gentlemanly class would shift from ''identity_locked'' to ''constrained'' or ''mobile'', potentially lowering their directionality and thus the effective extraction they experienced, making the constraint less ''piton''-like and more ''snare''-like if other beneficiaries emerged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_strength, empirical, 'Assessing the degree to which elite identity was fused with the practice of dueling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__decline_reading, 1750, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1750, 0.1).
narrative_ontology:measurement(hono_tr_t1780, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1780, 0.12).
narrative_ontology:measurement(hono_tr_t1810, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1810, 0.15).
narrative_ontology:measurement(hono_tr_t1840, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1840, 0.17).
narrative_ontology:measurement(hono_tr_t1870, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1870, 0.19).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1900, 0.2).

% Extraction over time
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1750, 0.65).
narrative_ontology:measurement(hono_be_t1780, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1780, 0.6).
narrative_ontology:measurement(hono_be_t1810, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1810, 0.55).
narrative_ontology:measurement(hono_be_t1840, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1840, 0.5).
narrative_ontology:measurement(hono_be_t1870, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1870, 0.48).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1900, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1750, 0.4).
narrative_ontology:measurement(hono_su_t1780, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1780, 0.45).
narrative_ontology:measurement(hono_su_t1810, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1810, 0.5).
narrative_ontology:measurement(hono_su_t1840, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1840, 0.55).
narrative_ontology:measurement(hono_su_t1870, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1870, 0.58).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1900, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__decline_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, state_monopoly_on_violence).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, bourgeois_social_norms).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'honor_satisfaction_mechanism' kernel. The 'decline_reading' emphasizes the gradual weakening and persistence of dueling as a fringe practice, distinct from the 'contraction_reading' (dueling became unthinkable) and 'composite_reading' (multiple, distinct mechanisms drove its decline). Each reading represents a different structural claim about the constraint's operation and persistence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
