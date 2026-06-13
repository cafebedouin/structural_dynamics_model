% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__composite_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: honor_violence_legitimacy__composite_reading
 *   human_readable: Honor Violence Legitimacy (Composite Reading)
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This constraint models the legitimacy of honor violence (e.g., dueling,
 *   blood feuds) as a 'composite reading' of its historical decline. It
 *   posits that the decline was 'overdetermined' by two simultaneous
 *   mechanisms: 'drop' (external costs, such as legal prohibitions and state
 *   enforcement, making it practically unfeasible or too costly) and
 *   'contraction' (an internal redefinition of 'honor' itself, making
 *   violence conceptually incompatible with true honor). This reading
 *   acknowledges that both structural changes and conceptual shifts
 *   contributed to the erosion of its legitimacy, affecting different victim
 *   sets and extractiveness profiles.
 *
 * KEY AGENTS:
 *   - social_elites_maintaining_honor_codes: Primary beneficiary (institutional/generational) — benefits from the social order maintained by honor codes, even as they adapt.
 *   - legal_authorities_preserving_social_order: Agenda setter (institutional/generational) — enforces laws that impose external costs on honor violence, gradually redefining its legitimacy.
 *   - individuals_subject_to_honor_violence: Primary victim (powerless/biographical) — bears the direct costs of honor violence, trapped by social expectations.
 *   - families_bound_by_honor_codes: Payer (organized/generational) — bears the social and economic costs of maintaining honor, even as its definition shifts.
 *   - moral_philosophers_and_reformers: Observer (analytical/generational) — articulate new conceptualizations of honor that exclude violence, influencing the 'contraction' mechanism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, 0.65).
domain_priors:suppression_score(honor_violence_legitimacy__composite_reading, 0.4).
domain_priors:theater_ratio(honor_violence_legitimacy__composite_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__composite_reading, "Honor Violence Legitimacy (Composite Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__composite_reading, "historical_sociology/legal_anthropology/commitment_systems").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__composite_reading, '52abc8e5-ae76-4e2b-9683-708d936f3269').
narrative_ontology:cs_kernel_codification('52abc8e5-ae76-4e2b-9683-708d936f3269', implicit).
narrative_ontology:cs_authority_grounding('52abc8e5-ae76-4e2b-9683-708d936f3269', practice).
narrative_ontology:cs_interpretation_layer_present('52abc8e5-ae76-4e2b-9683-708d936f3269').
narrative_ontology:cs_reading_relation('52abc8e5-ae76-4e2b-9683-708d936f3269', honor_violence_legitimacy__drop_reading, influences).
narrative_ontology:cs_reading_relation('52abc8e5-ae76-4e2b-9683-708d936f3269', honor_violence_legitimacy__contraction_reading, influences).
narrative_ontology:cs_axiom('52abc8e5-ae76-4e2b-9683-708d936f3269', foundational, honor_is_socially_constructed_and_mutable).
narrative_ontology:cs_axiom_status(honor_is_socially_constructed_and_mutable, holdable).
narrative_ontology:cs_axiom_grounding('52abc8e5-ae76-4e2b-9683-708d936f3269', honor_is_socially_constructed_and_mutable, conventional).
narrative_ontology:cs_axiom('52abc8e5-ae76-4e2b-9683-708d936f3269', foundational, decline_is_multicausal).
narrative_ontology:cs_axiom_status(decline_is_multicausal, holdable).
narrative_ontology:cs_axiom_grounding('52abc8e5-ae76-4e2b-9683-708d936f3269', decline_is_multicausal, empirically_contingent).
narrative_ontology:cs_reference_frame('52abc8e5-ae76-4e2b-9683-708d936f3269', honor_violence_as_legitimate_social_practice).
narrative_ontology:cs_drift_state('52abc8e5-ae76-4e2b-9683-708d936f3269', contemporary_human_rights_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('52abc8e5-ae76-4e2b-9683-708d936f3269', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, social_elites_maintaining_honor_codes).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, legal_authorities_preserving_social_order).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, individuals_subject_to_honor_violence).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, families_bound_by_honor_codes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups historically benefited from honor codes as a means of social control and status differentiation. As honor violence declines, they adapt by redefining honor to exclude violence, maintaining their social standing through other means, and benefiting from a more stable social order.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, social_elites_maintaining_honor_codes, beneficiary,
    institutional, generational, mobile, national).

% State and legal institutions actively enforce laws against honor violence, imposing fines, imprisonment, and other external costs. They benefit from the reduction of private violence and the consolidation of state authority over justice, contributing to the 'drop' mechanism.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, legal_authorities_preserving_social_order, agenda_setter,
    institutional, generational, analytical, national).

% These individuals are directly compelled to participate in or suffer the consequences of honor violence due to intense social pressure and the threat of ostracism. Their options are severely limited, making them primary victims of the constraint's extractive force, even as it declines.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, individuals_subject_to_honor_violence, payer,
    powerless, biographical, trapped, local).

% Families are the primary units through which honor codes are transmitted and enforced. They bear the collective social and economic costs of maintaining honor, including potential feuds or social isolation. Their identity is often fused with the honor system, making exit difficult even as norms shift, contributing to the 'contraction' mechanism's lingering effects.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, families_bound_by_honor_codes, payer,
    organized, generational, identity_locked, regional).

% Intellectuals and social movements that advocate for new ethical frameworks where violence is incompatible with true honor. They influence the 'contraction' mechanism by providing conceptual tools for redefinition, but do not directly enforce the constraint.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, moral_philosophers_and_reformers, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__composite_reading, social_elites_maintaining_honor_codes).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social behavior around a shared understanding of 'honor' and its defense, providing a framework for resolving disputes and maintaining social hierarchy, even as the definition of honor evolves.
% TRANSFER_FUNCTION: Transfers social capital, status, and sometimes physical safety from individuals and families to the collective maintenance of honor codes and the authority of social elites and legal institutions.
% ABSENT_VOICES: Victims of honor violence, particularly women and marginalized individuals, whose perspectives were historically suppressed and whose suffering was often justified by the very honor codes in question. Their voices would challenge the legitimacy of the entire system.
% DISAPPEARANCE_RATIONALE: If the legitimacy of honor violence vanished overnight, the social structures and power dynamics that sustained it would undergo significant rearrangement. Families and communities would need to find new ways to manage reputation and resolve disputes, and legal systems would face less resistance in enforcing prohibitions against violence. The very concept of 'honor' would likely be decoupled from violence entirely.
% FOUNDING_PROBLEM: The founding problem was the need for a mechanism to regulate social status, resolve perceived insults, and maintain social order in contexts where state authority was weak or non-existent, or where personal reputation was paramount.
% FOUNDING_PROBLEM_CORROBORATION: While some traditionalists might argue the problem is still live, most legal authorities, human rights organizations, and contemporary social commentators attest that the original problem of regulating status through violence is largely obsolete or has been superseded by state legal systems. The persistence of honor violence is now seen as a problem of social control and human rights, not a legitimate means of dispute resolution. This is corroborated by international legal frameworks and sociological studies from outside the benefiting parties.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__composite_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_violence_legitimacy__composite_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_violence_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the residual social pressure and potential for violence, even as its overt practice declines. Suppression (0.40) is moderate, as it relies on both active legal enforcement (drop) and internalized social norms (contraction). The low theater ratio (0.10) indicates that while the overt practice of honor violence may become performative, the underlying social pressures and conceptual debates are genuine. The decline in extractiveness over the interval reflects the combined effect of both mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individuals subject to honor violence, the constraint is highly extractive and suppressive, regardless of whether the decline is due to external costs or conceptual shifts. For social elites, the constraint shifts from a direct enforcement mechanism to a more subtle form of social coordination around redefined norms, still benefiting them by maintaining social order. Legal authorities experience it as a successful application of state power to reduce violence.
 *
 * DIRECTIONALITY LOGIC:
 *   Social elites and legal authorities are beneficiaries (d near 0.0) as they either maintain social order or enforce new norms. Individuals and families are victims (d near 1.0) as they bear the direct and indirect costs of honor codes, even in decline. The composite nature means that the 'drop' mechanism (external costs) targets individuals more directly, while the 'contraction' mechanism (redefinition of honor) targets the broader social fabric and families.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (maintaining honor) has not atrophied, but its *means* have been redefined. The composite reading prevents mislabeling it as a pure 'snare' (if only external costs were considered) or a pure 'rope' (if only conceptual redefinition were considered as a voluntary shift). It acknowledges the coercive elements of both external enforcement and the social pressure of evolving norms, classifying it as a tangled_rope where coordination around new norms still carries asymmetric extraction from those bound by older interpretations of honor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine composite of ''drop'' and ''contraction'' mechanisms, or is one mechanism dominant?',
    'Detailed historical analysis of specific cases where both external costs and conceptual redefinition were simultaneously active and demonstrably contributed to the decline of honor violence.',
    'If one mechanism is dominant, the constraint should be reclassified as either ''drop_reading'' (if external costs are primary) or ''contraction_reading'' (if conceptual redefinition is primary), leading to different extractiveness and victim profiles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''composite_reading'' of the ''honor_violence_legitimacy'' kernel, asserting that both external costs (drop) and conceptual redefinition (contraction) operated simultaneously in its decline. Sibling readings are ''drop_reading'' and ''contraction_reading''.').

omega_variable(
    relative_contribution_of_mechanisms,
    'What was the relative contribution of external costs versus conceptual redefinition to the decline of honor violence in specific historical contexts?',
    'Quantitative historical sociology comparing the impact of legal prohibitions and economic changes (external costs) against shifts in moral philosophy and social norms (conceptual redefinition) on the incidence of honor violence.',
    'A higher contribution from external costs would shift the constraint''s extractiveness profile towards a ''snare'' (more direct coercion), while a higher contribution from conceptual redefinition would emphasize the ''tangled_rope'' aspect (coordination around new norms with residual extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(relative_contribution_of_mechanisms, empirical, 'Assessing the proportional impact of ''drop'' vs. ''contraction'' mechanisms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__composite_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_violence_legitimacy__composite_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hono_tr_t50, honor_violence_legitimacy__composite_reading, theater_ratio, 50, 0.08).
narrative_ontology:measurement(hono_tr_t100, honor_violence_legitimacy__composite_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_violence_legitimacy__composite_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(hono_be_t50, honor_violence_legitimacy__composite_reading, base_extractiveness, 50, 0.75).
narrative_ontology:measurement(hono_be_t100, honor_violence_legitimacy__composite_reading, base_extractiveness, 100, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_violence_legitimacy__composite_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(hono_su_t50, honor_violence_legitimacy__composite_reading, suppression_requirement, 50, 0.45).
narrative_ontology:measurement(hono_su_t100, honor_violence_legitimacy__composite_reading, suppression_requirement, 100, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__composite_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy__contraction_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
