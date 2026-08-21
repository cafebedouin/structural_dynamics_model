% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__cultural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__cultural_contraction_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: honor_satisfaction_substrate__cultural_contraction_reading
 *   human_readable: Honor Satisfaction Substrate (Cultural Contraction Reading)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint story represents the 'cultural contraction' reading of
 *   the honor satisfaction substrate. It argues that dueling became
 *   unthinkable not primarily due to external suppression, but because the
 *   underlying cultural framework of 'honor' itself transformed into a
 *   'culture of dignity.' This shift rendered the very concept of dueling as
 *   a legitimate means of satisfaction obsolete, effectively eroding the
 *   'mountain' of cultural necessity that once supported it. The constraint
 *   is classified as a Mountain because its persistence (or, in this case,
 *   its disappearance) is seen as an emergent property of deep cultural
 *   structures, not a human-enforced choice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__cultural_contraction_reading, 0.05).
domain_priors:suppression_score(honor_satisfaction_substrate__cultural_contraction_reading, 0.02).
domain_priors:theater_ratio(honor_satisfaction_substrate__cultural_contraction_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__cultural_contraction_reading, mountain).
narrative_ontology:human_readable(honor_satisfaction_substrate__cultural_contraction_reading, "Honor Satisfaction Substrate (Cultural Contraction Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__cultural_contraction_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:emerges_naturally(honor_satisfaction_substrate__cultural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__cultural_contraction_reading, '78f14fbc-518c-4b03-a945-65fca52bacba').
narrative_ontology:cs_kernel_codification('78f14fbc-518c-4b03-a945-65fca52bacba', implicit).
narrative_ontology:cs_authority_grounding('78f14fbc-518c-4b03-a945-65fca52bacba', practice).
narrative_ontology:cs_interpretation_layer_present('78f14fbc-518c-4b03-a945-65fca52bacba').
narrative_ontology:cs_reading_relation('78f14fbc-518c-4b03-a945-65fca52bacba', honor_satisfaction_substrate__practice_decline_reading, influences).
narrative_ontology:cs_reading_relation('78f14fbc-518c-4b03-a945-65fca52bacba', honor_satisfaction_substrate__composite_overdetermined_reading, coexists_with).
narrative_ontology:cs_axiom('78f14fbc-518c-4b03-a945-65fca52bacba', foundational, honor_code_is_cultural_substrate).
narrative_ontology:cs_axiom_status(honor_code_is_cultural_substrate, holdable).
narrative_ontology:cs_axiom_grounding('78f14fbc-518c-4b03-a945-65fca52bacba', honor_code_is_cultural_substrate, conventional).
narrative_ontology:cs_axiom('78f14fbc-518c-4b03-a945-65fca52bacba', foundational, dueling_unthinkable_in_dignity_culture).
narrative_ontology:cs_axiom_status(dueling_unthinkable_in_dignity_culture, holdable).
narrative_ontology:cs_axiom_grounding('78f14fbc-518c-4b03-a945-65fca52bacba', dueling_unthinkable_in_dignity_culture, empirically_contingent).
narrative_ontology:cs_reference_frame('78f14fbc-518c-4b03-a945-65fca52bacba', culture_of_honor_necessity).
narrative_ontology:cs_drift_state('78f14fbc-518c-4b03-a945-65fca52bacba', culture_of_dignity_ascendance, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('78f14fbc-518c-4b03-a945-65fca52bacba', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the primary participants in dueling, their cultural framework shifted from honor to dignity, making dueling unthinkable. They are now observers of a past cultural substrate.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, historical_elites, observer,
    analytical, generational, analytical, regional).

% Analyze the historical shift in cultural norms that rendered dueling obsolete. They observe the constraint's disappearance as a cultural phenomenon.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, cultural_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Historically, it provided a framework for resolving disputes among elites and maintaining social status through ritualized violence, ensuring a predictable (if deadly) form of satisfaction.
% TRANSFER_FUNCTION: Transferred social standing and reputation, or physical harm/death, based on adherence to a specific code of conduct. Its disappearance means this transfer mechanism is no longer operative.
% ABSENT_VOICES: The victims of dueling (those killed or injured) and those who found the practice barbaric were historically marginalized. Their voices would have condemned the practice, but the cultural shift made it unthinkable rather than merely suppressed.
% DISAPPEARANCE_RATIONALE: From this reading, the 'honor satisfaction substrate' itself contracted and became culturally unthinkable. Its disappearance means the world is already 'unchanged' in the sense that the cultural conditions for its existence no longer obtain. The shift from honor to dignity is the disappearance.
% FOUNDING_PROBLEM: To provide a formalized, ritualized mechanism for elites to defend their honor and resolve grievances, preventing unconstrained violence and maintaining social order within a specific cultural framework.
% FOUNDING_PROBLEM_CORROBORATION: Cultural historians and sociologists widely corroborate that the 'culture of honor' that necessitated dueling has largely given way to a 'culture of dignity,' rendering the original problem obsolete. This is attested by shifts in legal codes, social etiquette, and personal values across historical periods, from outside the original benefiting parties.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__cultural_contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__cultural_contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__cultural_contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_satisfaction_substrate__cultural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__cultural_contraction_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__cultural_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_satisfaction_substrate__cultural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_satisfaction_substrate__cultural_contraction_reading),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_satisfaction_substrate__cultural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics reflect a 'mountain erosion' scenario. Extractiveness, suppression, and theater ratio are all very low because the constraint's operation is not about active enforcement or extraction, but about the presence or absence of a cultural substrate. As the culture of honor contracted, the 'mountain' supporting dueling simply ceased to exist as a viable social reality. Accessibility collapse is high because, once the cultural shift occurred, dueling exited the realm of thinkable actions, collapsing all alternatives for 'satisfaction' through that mechanism. Resistance is low because the change was a fundamental cultural transformation, not a contested policy.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between this reading and those that emphasize exogenous enforcement. This reading posits a fundamental cultural shift, making dueling unthinkable, whereas other readings might see it as merely suppressed. The engine's classification as a Mountain (due to low extraction/suppression and high accessibility collapse) supports this reading's emphasis on deep cultural structure over active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   There are no direct beneficiaries or victims in the traditional sense, as the constraint describes a cultural substrate. Historical elites, who once participated in dueling, are now 'observers' of a past cultural reality. Cultural historians are also 'observers,' analyzing the shift. No agent actively benefits from or is harmed by the 'existence' of this cultural substrate in the present, as it has largely vanished.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_vs_legal_causation,
    'Was the decline of dueling primarily due to a fundamental cultural shift (as this reading claims), or to increasing legal and institutional suppression?',
    'Comparative historical analysis of regions with differing legal enforcement timelines and cultural shifts; detailed examination of primary sources (diaries, legal records, etiquette manuals) to trace the sequence of changes in social thought and practice.',
    'If legal suppression was primary, the constraint would be reclassified as a Snare or Tangled Rope, reflecting active enforcement and extraction. If cultural shift was primary, the Mountain classification holds, emphasizing the substrate''s erosion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cultural_vs_legal_causation, empirical, 'Distinguishing between endogenous cultural change and exogenous legal enforcement as the primary cause of dueling''s decline.').

omega_variable(
    substrate_vs_practice_persistence,
    'Did the ''honor code'' as a normative substrate truly disappear, or did it merely cease to manifest in dueling while persisting in other forms of social interaction?',
    'Anthropological and sociological studies of contemporary ''honor cultures'' in different contexts, examining whether the underlying logic of honor persists in non-dueling forms (e.g., reputation management, conflict resolution in specific subcultures).',
    'If the honor code persists, the ''mountain erosion'' claim is weakened, and the constraint might be reclassified as a Piton (atrophied practice) or a Rope (repurposed coordination) if the substrate still coordinates other behaviors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_vs_practice_persistence, conceptual, 'Whether the cultural substrate itself contracted or only the specific practice of dueling declined.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__cultural_contraction_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1700, 0.01).
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1750, 0.01).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1800, 0.01).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1850, 0.01).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1900, 0.01).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1700, 0.05).
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1750, 0.04).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1800, 0.03).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1850, 0.02).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1900, 0.01).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1700, 0.02).
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1750, 0.02).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1800, 0.01).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1850, 0.01).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1900, 0.01).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__cultural_contraction_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'honor_satisfaction_substrate' kernel. This 'cultural_contraction_reading' emphasizes the internal cultural shift, distinct from 'practice_decline_reading' (exogenous enforcement) and 'composite_overdetermined_reading' (both factors).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
