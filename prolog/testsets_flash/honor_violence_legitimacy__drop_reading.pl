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
 *   This constraint represents the 'drop_reading' of the
 *   'honor_violence_legitimacy' kernel. In this reading, dueling, as a
 *   mechanism for resolving honor disputes, remained conceptually legitimate
 *   within certain elite circles during the 18th and 19th centuries. However,
 *   its practical frequency declined significantly not because honor itself
 *   was redefined to exclude violence, but because the external costs
 *   associated with dueling (e.g., legal penalties, social ostracism,
 *   economic ruin) became prohibitively high. The constraint thus describes a
 *   situation where the 'right' to duel was still acknowledged, but the
 *   'cost' of exercising that right became too great for most, leading to a
 *   de facto, rather than de jure, decline in practice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__drop_reading, 0.3).
domain_priors:suppression_score(honor_violence_legitimacy__drop_reading, 0.2).
domain_priors:theater_ratio(honor_violence_legitimacy__drop_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__drop_reading, rope).
narrative_ontology:human_readable(honor_violence_legitimacy__drop_reading, "Honor Violence Legitimacy (Drop Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__drop_reading, "historical_sociology/legal_anthropology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__drop_reading, '4c33fc82-a7f3-402f-85ec-4bcf95689351').
narrative_ontology:cs_kernel_codification('4c33fc82-a7f3-402f-85ec-4bcf95689351', implicit).
narrative_ontology:cs_authority_grounding('4c33fc82-a7f3-402f-85ec-4bcf95689351', practice).
narrative_ontology:cs_interpretation_layer_present('4c33fc82-a7f3-402f-85ec-4bcf95689351').
narrative_ontology:cs_reading_relation('4c33fc82-a7f3-402f-85ec-4bcf95689351', honor_violence_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('4c33fc82-a7f3-402f-85ec-4bcf95689351', honor_violence_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('4c33fc82-a7f3-402f-85ec-4bcf95689351', foundational, honor_requires_ultimate_violent_redress_option).
narrative_ontology:cs_axiom_status(honor_requires_ultimate_violent_redress_option, holdable).
narrative_ontology:cs_axiom_grounding('4c33fc82-a7f3-402f-85ec-4bcf95689351', honor_requires_ultimate_violent_redress_option, conventional).
narrative_ontology:cs_reference_frame('4c33fc82-a7f3-402f-85ec-4bcf95689351', honor_code_with_duel_option).
narrative_ontology:cs_drift_state('4c33fc82-a7f3-402f-85ec-4bcf95689351', late_19th_century_social_context, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4c33fc82-a7f3-402f-85ec-4bcf95689351', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, honor_bound_elites).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, potential_duelists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals maintained a social code where dueling was a legitimate, if increasingly rare, means of defending honor. They benefited from the conceptual availability of dueling as a deterrent and a final arbiter, even if they rarely engaged in it due to external costs. Their identity was tied to this honor system.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, honor_bound_elites, beneficiary,
    powerful, generational, identity_locked, national).

% Individuals who might have engaged in dueling to defend their honor. They faced the high external costs (legal, social, economic) that made the practice practically prohibitive, even if they still believed in its legitimacy. Their options were to bear the costs, find alternative (less satisfying) means of redress, or suffer perceived dishonor.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, potential_duelists, payer,
    moderate, biographical, constrained, local).

% While not directly redefining honor, these authorities increasingly imposed legal penalties (fines, imprisonment, social ostracism) on duelists, raising the external costs of the practice. They did not necessarily challenge the underlying honor code but made its violent expression impractical.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, state_legal_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Groups advocating for the abolition of dueling and a redefinition of honor to exclude violence. In this 'drop' reading, their efforts contributed to the rising external costs but did not fundamentally alter the conceptual legitimacy of dueling within the honor code itself.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, social_reformers, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a framework for resolving extreme honor disputes among elites, maintaining social order and status hierarchies by offering a recognized, albeit costly, mechanism for redress.
% TRANSFER_FUNCTION: Transferred the burden of maintaining honor from direct violence to bearing significant external costs (legal, social, economic) for those who chose to duel.
% ABSENT_VOICES: Social reformers and anti-dueling leagues, who would have argued for a complete redefinition of honor to exclude violence, were present but, in this reading, their arguments primarily influenced the external costs rather than the internal legitimacy of dueling.
% DISAPPEARANCE_RATIONALE: If the conceptual legitimacy of dueling (even with high costs) vanished, the entire system of honor among elites would need to find new, non-violent mechanisms for ultimate redress, potentially altering social hierarchies and norms of masculine conduct.
% FOUNDING_PROBLEM: The need for a definitive, public mechanism to resolve grave insults to honor among elites, preventing endless feuds and maintaining social standing.
% FOUNDING_PROBLEM_CORROBORATION: The problem of honor redress remained live for elites, even if the preferred mechanism became too costly. Historical accounts and literary works from the period, including those by non-dueling parties, attest to the continued importance of honor and the conceptual availability of dueling, despite its rarity.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__drop_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__drop_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__drop_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_violence_legitimacy__drop_reading, 'none', 1).

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
 *   The extractiveness (0.3) is moderate, reflecting the high but not absolute costs of dueling. Suppression (0.2) is low because the constraint isn't actively enforced to prevent dueling, but rather the external costs act as a deterrent. Theater ratio (0.1) is low as the practice, while rare, was still a serious affair when it occurred, not merely performative. Accessibility collapse (0.4) is moderate; alternatives to dueling existed but were not always seen as fully satisfying honor. Resistance (0.1) is low because the decline was driven by external factors, not active opposition to the concept of dueling itself.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of honor-bound elites, the system remained a legitimate, if costly, means of redress. From the perspective of those considering a duel, the external costs made it a highly constrained option. The engine's classification as a Rope reflects the coordination function of maintaining an honor code, with the 'extraction' being the external costs that made the practice rare.
 *
 * DIRECTIONALITY LOGIC:
 *   Honor-bound elites are the primary beneficiaries, as the system of honor still conceptually allowed for dueling, even if rarely practiced. The costs were borne by those who might have otherwise engaged in dueling, but these costs were external to the honor system itself, making them more like a 'tax' on the practice rather than direct extraction by a specific agent within the honor system. Thus, no clear 'victims' are declared in the sense of a party directly extracting from duelists through the honor code itself.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''drop'' in practice frequency due to external costs, or is it a ''contraction'' of honor''s definition, or a ''composite'' of both?',
    'Historical analysis of legal codes and social norms: if dueling laws remained permissive but other costs (e.g., social ostracism, economic ruin) rose, it supports the ''drop'' reading. If honor itself was redefined to exclude violence, it supports ''contraction''.',
    'If ''contraction'', the constraint would be a Mountain (honor redefined as natural law); if ''composite'', it would be a Tangled Rope (coordination with extraction from those who couldn''t afford the costs). This ''drop'' reading implies a Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''drop_reading'' of the ''honor_violence_legitimacy'' kernel, where dueling remained legitimate but became rare due to external costs, not a redefinition of honor.').

omega_variable(
    external_cost_quantification,
    'What was the precise magnitude and nature of the external costs that made dueling practically rare?',
    'Quantitative historical sociology: analyze court records, insurance policies, and social commentary to measure the rising economic, legal, and social penalties associated with dueling.',
    'Higher, more consistent external costs would strengthen the ''drop'' reading''s explanation for reduced frequency. If costs were low or inconsistent, it would weaken this reading and lend credence to ''contraction'' or ''composite'' readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_cost_quantification, empirical, 'Quantification of external costs driving the decline in dueling practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__drop_reading, 1750, 1850).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_violence_legitimacy__drop_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hono_tr_t10, honor_violence_legitimacy__drop_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(hono_tr_t20, honor_violence_legitimacy__drop_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_violence_legitimacy__drop_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(hono_be_t10, honor_violence_legitimacy__drop_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(hono_be_t20, honor_violence_legitimacy__drop_reading, base_extractiveness, 20, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_violence_legitimacy__drop_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(hono_su_t10, honor_violence_legitimacy__drop_reading, suppression_requirement, 10, 0.25).
narrative_ontology:measurement(hono_su_t20, honor_violence_legitimacy__drop_reading, suppression_requirement, 20, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__drop_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'honor_violence_legitimacy' kernel. This 'drop_reading' focuses on the decline of dueling due to external costs, while the 'contraction_reading' focuses on the redefinition of honor, and the 'composite_reading' considers both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
