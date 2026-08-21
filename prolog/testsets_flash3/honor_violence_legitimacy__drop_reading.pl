% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__drop_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__drop_reading
 *   human_readable: Honor Violence Legitimacy (Drop Reading)
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the 'drop reading' of the
 *   honor_violence_legitimacy kernel. It posits that dueling, as a form of
 *   honor violence, remained conceptually legitimate for a segment of society
 *   but became practically rare due to increasing external costs (legal
 *   prohibition, social stigma, economic penalties). The constraint's
 *   persistence is largely inertial, as its functional role has atrophied,
 *   but its conceptual availability as a means of honor defense persists for
 *   some, making it a Piton. The metrics reflect a decline in actual
 *   extraction (as fewer people duel) but a rise in theatricality (as the
 *   idea of dueling persists without the practice).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__drop_reading, 0.2).
domain_priors:suppression_score(honor_violence_legitimacy__drop_reading, 0.1).
domain_priors:theater_ratio(honor_violence_legitimacy__drop_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__drop_reading, piton).
narrative_ontology:human_readable(honor_violence_legitimacy__drop_reading, "Honor Violence Legitimacy (Drop Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__drop_reading, "historical_sociology/legal_anthropology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__drop_reading, '8420ce00-6cdf-42bb-ae7a-8fa2f0af464a').
narrative_ontology:cs_kernel_codification('8420ce00-6cdf-42bb-ae7a-8fa2f0af464a', implicit).
narrative_ontology:cs_authority_grounding('8420ce00-6cdf-42bb-ae7a-8fa2f0af464a', practice).
narrative_ontology:cs_interpretation_layer_present('8420ce00-6cdf-42bb-ae7a-8fa2f0af464a').
narrative_ontology:cs_reading_relation('8420ce00-6cdf-42bb-ae7a-8fa2f0af464a', honor_violence_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('8420ce00-6cdf-42bb-ae7a-8fa2f0af464a', honor_violence_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('8420ce00-6cdf-42bb-ae7a-8fa2f0af464a', foundational, honor_requires_violent_defense_conceptually).
narrative_ontology:cs_axiom_status(honor_requires_violent_defense_conceptually, holdable).
narrative_ontology:cs_axiom_grounding('8420ce00-6cdf-42bb-ae7a-8fa2f0af464a', honor_requires_violent_defense_conceptually, conventional).
narrative_ontology:cs_reference_frame('8420ce00-6cdf-42bb-ae7a-8fa2f0af464a', dueling_as_legitimate_honor_defense).
narrative_ontology:cs_drift_state('8420ce00-6cdf-42bb-ae7a-8fa2f0af464a', late_19th_century_legal_prohibition, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('8420ce00-6cdf-42bb-ae7a-8fa2f0af464a', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, honor_seekers).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, social_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who, in earlier eras, would have engaged in dueling to defend their honor. They now face prohibitive legal and social costs for such actions, making dueling practically impossible, though the underlying concept of violent honor defense remains conceptually available to them.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, honor_seekers, payer,
    moderate, biographical, constrained, local).

% The broader society and its legal institutions that increasingly criminalized dueling and imposed severe penalties. While dueling's legitimacy at a conceptual level might persist for some, the practical enforcement of laws against it makes its actual occurrence rare, reducing social disruption.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, social_order, payer,
    institutional, generational, mobile, national).

% Analysts who study the historical evolution of honor codes and violence. They observe that dueling's decline was primarily due to external costs and legal suppression, not a fundamental redefinition of honor itself, maintaining that the structural legitimacy of dueling for honor persisted.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, historical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a formalized, if violent, mechanism for resolving disputes over honor and maintaining social status hierarchies among certain classes.
% TRANSFER_FUNCTION: Transferred the burden of maintaining honor from individual reputation to the willingness to engage in high-stakes violence, with the costs of this violence (injury, death, legal penalties) borne by participants and the broader social order.
% ABSENT_VOICES: Those who might have sought to redefine honor to exclude violence were not structurally excluded, but their arguments were not the primary driver of dueling's decline in this reading; rather, external costs made the practice untenable.
% DISAPPEARANCE_RATIONALE: If the conceptual legitimacy of dueling for honor (as understood by this reading) vanished overnight, the practical landscape would remain largely unchanged, as dueling is already rare due to external costs. The world has already rearranged itself around its practical absence.
% FOUNDING_PROBLEM: The need for a formal mechanism to resolve challenges to personal and family honor, particularly among aristocratic and military classes, to prevent endless feuds and maintain social standing.
% FOUNDING_PROBLEM_CORROBORATION: Historical legal records and sociological analyses corroborate that dueling's original function as a legitimate honor-defense mechanism is no longer active due to legal prohibition and social stigma, even if the underlying concept of honor-violence retains some conceptual availability for some individuals.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__drop_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__drop_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__drop_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_violence_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__drop_reading, 0.2, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__drop_reading_tests).
:- end_tests(honor_violence_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.2) because the actual practice of dueling, and thus its direct costs, became rare. Suppression is also low (0.1) because the constraint's persistence is not due to active enforcement of dueling, but rather the enforcement of laws AGAINST it, which makes the practice costly. The high theater_ratio (0.6) reflects that the idea of dueling for honor persists in cultural narratives and individual thought, even as its practical manifestation is almost nil. Accessibility collapse is moderate (0.3) because while dueling is practically inaccessible, the conceptual path to it remains open. Resistance is low (0.05) because there is little active resistance to the *idea* of dueling, as its practice has largely ceased.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 'honor seekers' who still conceptually value dueling, the constraint is a 'constrained' option, available in theory but too costly in practice. From the perspective of the 'social order', it's a 'piton' – a vestigial concept with little practical impact but requiring some maintenance of legal prohibitions. The engine's classification as Piton reflects the overall societal view of its atrophied function.
 *
 * DIRECTIONALITY LOGIC:
 *   Honor seekers are targets (payer) because they bear the costs of engaging in dueling, even if they conceptually value its legitimacy. The social order is also a target (payer) as it bears the diffuse costs of maintaining laws against dueling and dealing with its residual cultural influence. Historical observers are analytical, neither benefiting nor paying.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_vs_practical_legitimacy,
    'To what extent did the conceptual legitimacy of dueling for honor truly persist, versus being merely a residual cultural artifact without active endorsement?',
    'Analysis of personal correspondence, diaries, and literary works from the period for explicit endorsements or rejections of dueling''s underlying principles, rather than just its practice.',
    'If conceptual legitimacy was truly absent, the constraint would be closer to a pure Piton, with even less ''live'' function. If it was actively endorsed, the ''theater_ratio'' might be lower, indicating a more active, albeit suppressed, conceptual constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_vs_practical_legitimacy, empirical, 'Distinguishing between passive cultural residue and active conceptual legitimacy.').

omega_variable(
    causal_primacy_of_external_costs,
    'Were external costs (legal, social) the sole or primary driver of dueling''s decline, or did a redefinition of honor (as argued by the ''contraction_reading'') also play a significant, independent role?',
    'Comparative historical analysis of societies with similar legal prohibitions but different cultural evolutions of honor, or detailed micro-historical studies of individual decisions to avoid dueling.',
    'If honor was indeed redefined, this reading''s claim of persistent structural legitimacy would be weakened, potentially shifting the classification towards a more ''dead'' or ''contracted'' form of the constraint, aligning more with the ''contraction_reading'' or ''composite_reading''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causal_primacy_of_external_costs, conceptual, 'The relative causal weight of external costs versus internal conceptual shifts in dueling''s decline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__drop_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_violence_legitimacy__drop_reading, theater_ratio, 1700, 0.1).
narrative_ontology:measurement(hono_tr_t1750, honor_violence_legitimacy__drop_reading, theater_ratio, 1750, 0.25).
narrative_ontology:measurement(hono_tr_t1800, honor_violence_legitimacy__drop_reading, theater_ratio, 1800, 0.4).
narrative_ontology:measurement(hono_tr_t1850, honor_violence_legitimacy__drop_reading, theater_ratio, 1850, 0.5).
narrative_ontology:measurement(hono_tr_t1900, honor_violence_legitimacy__drop_reading, theater_ratio, 1900, 0.6).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_violence_legitimacy__drop_reading, base_extractiveness, 1700, 0.4).
narrative_ontology:measurement(hono_be_t1750, honor_violence_legitimacy__drop_reading, base_extractiveness, 1750, 0.3).
narrative_ontology:measurement(hono_be_t1800, honor_violence_legitimacy__drop_reading, base_extractiveness, 1800, 0.25).
narrative_ontology:measurement(hono_be_t1850, honor_violence_legitimacy__drop_reading, base_extractiveness, 1850, 0.22).
narrative_ontology:measurement(hono_be_t1900, honor_violence_legitimacy__drop_reading, base_extractiveness, 1900, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_violence_legitimacy__drop_reading, suppression_requirement, 1700, 0.05).
narrative_ontology:measurement(hono_su_t1750, honor_violence_legitimacy__drop_reading, suppression_requirement, 1750, 0.07).
narrative_ontology:measurement(hono_su_t1800, honor_violence_legitimacy__drop_reading, suppression_requirement, 1800, 0.08).
narrative_ontology:measurement(hono_su_t1850, honor_violence_legitimacy__drop_reading, suppression_requirement, 1850, 0.09).
narrative_ontology:measurement(hono_su_t1900, honor_violence_legitimacy__drop_reading, suppression_requirement, 1900, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__drop_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'honor_violence_legitimacy' kernel, focusing on the decline of dueling due to external costs while its conceptual legitimacy persists. It is linked to sibling readings that emphasize conceptual redefinition ('contraction_reading') or a combination of factors ('composite_reading').

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
