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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: honor_violence_legitimacy__composite_reading
 *   human_readable: Honor Violence Legitimacy (Composite Reading)
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents a 'composite reading' of the decline in honor
 *   violence legitimacy, arguing that its reduction was overdetermined by
 *   both external costs (the 'drop' mechanism) and an internal conceptual
 *   redefinition of honor itself (the 'contraction' mechanism). This reading
 *   posits that while external pressures made honor violence more costly and
 *   risky, the more fundamental shift was a redefinition of 'honor' that
 *   increasingly excluded violence as a legitimate expression, making it
 *   structurally unthinkable rather than merely impractical. This composite
 *   view acknowledges both mechanisms operating simultaneously, with the
 *   conceptual redefinition providing a deeper, more resilient form of
 *   delegitimization.
 *
 * KEY AGENTS:
 *   - male_kin_group_heads: Agenda setter (institutional/identity_locked) — maintains and benefits from the system.
 *   - community_elders: Beneficiary (organized/constrained) — sanctions and benefits from social order.
 *   - women_accused_of_dishonor: Payer (powerless/trapped) — primary victims of the violence.
 *   - younger_male_kin: Payer (moderate/identity_locked) — coerced enforcers, bear costs.
 *   - state_legal_authorities: Observer (institutional/analytical) — external legal challenge.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, 0.65).
domain_priors:suppression_score(honor_violence_legitimacy__composite_reading, 0.7).
domain_priors:theater_ratio(honor_violence_legitimacy__composite_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__composite_reading, "Honor Violence Legitimacy (Composite Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__composite_reading, "historical_sociology/legal_anthropology/commitment_systems").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__composite_reading, 'bf763aab-8486-4b74-b141-7a3a4c56c5bc').
narrative_ontology:cs_kernel_codification('bf763aab-8486-4b74-b141-7a3a4c56c5bc', implicit).
narrative_ontology:cs_authority_grounding('bf763aab-8486-4b74-b141-7a3a4c56c5bc', practice).
narrative_ontology:cs_interpretation_layer_present('bf763aab-8486-4b74-b141-7a3a4c56c5bc').
narrative_ontology:cs_reading_relation('bf763aab-8486-4b74-b141-7a3a4c56c5bc', honor_violence_legitimacy__drop_reading, influences).
narrative_ontology:cs_reading_relation('bf763aab-8486-4b74-b141-7a3a4c56c5bc', honor_violence_legitimacy__contraction_reading, influences).
narrative_ontology:cs_axiom('bf763aab-8486-4b74-b141-7a3a4c56c5bc', foundational, honor_is_reputation_and_violence_is_a_legitimate_defense).
narrative_ontology:cs_axiom_status(honor_is_reputation_and_violence_is_a_legitimate_defense, holdable).
narrative_ontology:cs_axiom_grounding('bf763aab-8486-4b74-b141-7a3a4c56c5bc', honor_is_reputation_and_violence_is_a_legitimate_defense, conventional).
narrative_ontology:cs_axiom('bf763aab-8486-4b74-b141-7a3a4c56c5bc', foundational, external_costs_and_internal_redefinition_drive_decline).
narrative_ontology:cs_axiom_status(external_costs_and_internal_redefinition_drive_decline, holdable).
narrative_ontology:cs_axiom_grounding('bf763aab-8486-4b74-b141-7a3a4c56c5bc', external_costs_and_internal_redefinition_drive_decline, empirically_contingent).
narrative_ontology:cs_reference_frame('bf763aab-8486-4b74-b141-7a3a4c56c5bc', traditional_patriarchal_honor_system).
narrative_ontology:cs_drift_state('bf763aab-8486-4b74-b141-7a3a4c56c5bc', contemporary_human_rights_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('bf763aab-8486-4b74-b141-7a3a4c56c5bc', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, male_kin_group_heads).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, community_elders).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, women_accused_of_dishonor).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, younger_male_kin).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the primary enforcers and beneficiaries of honor violence, they maintain social order and family reputation through the threat and execution of violence. Their authority is deeply intertwined with the system, making exit unthinkable without losing status and power.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, male_kin_group_heads, agenda_setter,
    institutional, generational, identity_locked, local).

% Benefit from the social stability and adherence to traditional norms that honor violence is perceived to uphold. They may not directly initiate violence but sanction its practice and benefit from the deference it enforces.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, community_elders, beneficiary,
    organized, generational, constrained, local).

% Are the primary targets and victims of honor violence, facing severe consequences including death for perceived transgressions. They have no legal recourse or social support, making their situation one of extreme vulnerability and lack of exit.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, women_accused_of_dishonor, payer,
    powerless, immediate, trapped, local).

% Are often coerced into carrying out honor violence to uphold family reputation, even if they personally disagree. Their identity and social standing are tied to their kin group, making refusal a form of social suicide. They bear the psychological and legal costs of enforcement.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, younger_male_kin, payer,
    moderate, biographical, identity_locked, local).

% Represent a competing legal framework that often criminalizes honor violence. Their ability to enforce state law is constrained by local social norms, political will, and the difficulty of intervention in private family matters. They observe and occasionally prosecute.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, state_legal_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social behavior and maintains a specific patriarchal order by defining and enforcing 'honor' through violence, ensuring adherence to traditional gender roles and family reputation within a community.
% TRANSFER_FUNCTION: Transfers social control, deference, and reputational capital to male kin group heads and community elders, at the cost of the autonomy, safety, and lives of women and dissenting younger male kin.
% ABSENT_VOICES: Victims of honor violence and those who internally dissent but are identity-locked are structurally excluded. They would articulate the profound costs and lack of justice, but their voices are suppressed by the very system that defines honor.
% DISAPPEARANCE_RATIONALE: If the legitimacy of honor violence vanished overnight, the social order, power structures, and gender relations within affected communities would undergo a profound and rapid rearrangement. Traditional authority would collapse, and new forms of social control and justice would emerge, likely with significant initial instability.
% FOUNDING_PROBLEM: The constraint was established to maintain patriarchal social order, control female sexuality, and preserve family and community reputation in contexts where state law was weak or absent, and social standing was paramount.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies and historical accounts from outside the benefiting parties corroborate that the founding problem of maintaining social order and reputation through these means was historically live. However, contemporary human rights organizations and legal scholars contest its continued legitimacy and necessity, arguing that the 'problem' is now a pretext for control.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__composite_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__composite_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_violence_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__composite_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) and suppression (0.70) are high, reflecting the severe costs borne by victims and the coercive nature of the system. The decline in these metrics over time reflects the weakening of the constraint's legitimacy and enforcement capacity due to both external pressures and internal conceptual shifts. Theater ratio is low (0.10) because honor violence, when it occurs, is a direct and brutal enforcement, not a performance. The composite reading emphasizes that the decline is not solely due to external factors but also to a fundamental redefinition of the underlying concept of honor, which makes the violence increasingly unthinkable.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of male kin group heads, the constraint is a necessary mechanism for maintaining social order and family honor, a 'tangled rope' that coordinates community values. From the perspective of women and younger male kin, it is a 'snare' of pure extraction and coercion. State legal authorities view it as a criminal act, a 'snare' that violates fundamental rights. The composite reading attempts to explain the historical trajectory of these diverging perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Male kin group heads are full beneficiaries (d=0.0) as they control and benefit from the system. Community elders are also beneficiaries (d=0.15) due to the social stability it provides. Women accused of dishonor are full targets (d=1.0) as they bear the ultimate cost. Younger male kin are also targets (d=0.8) due to coercion and identity-lock. State legal authorities are analytical observers (d=0.5) with the potential to shift the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The composite reading prevents mislabeling the decline as solely a 'drop' (external costs) or 'contraction' (internal redefinition). By acknowledging both, it captures the overdetermined nature of the delegitimization. The 'contraction' aspect, where honor itself is redefined to exclude violence, suggests a deeper, more fundamental shift than mere cost-benefit calculation, making the constraint's mandate not just obsolete but conceptually incoherent within a new understanding of honor. This makes the constraint's persistence increasingly a matter of inertia and raw power, rather than a live coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relative_weight_of_decline_mechanisms,
    'What was the relative contribution of external costs (drop) versus conceptual redefinition (contraction) to the decline in honor violence legitimacy?',
    'Comparative historical analysis across different societies with varying exposure to external costs and internal intellectual movements, using quantitative measures of legal enforcement, social discourse, and actual incidence of violence.',
    'If external costs were dominant, the constraint''s decline is primarily a function of changing material conditions. If conceptual redefinition was dominant, the decline is a deeper shift in normative frameworks, suggesting a more robust and less reversible delegitimization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relative_weight_of_decline_mechanisms, empirical, 'Determining the primary driver of the decline in honor violence.').

omega_variable(
    causal_direction_between_mechanisms,
    'Did external costs primarily drive the conceptual redefinition of honor, or did an evolving concept of honor make external costs more salient and effective?',
    'Detailed historical case studies tracing the temporal sequence of legal reforms, economic changes, and intellectual debates about honor in specific communities.',
    'Understanding the causal direction clarifies whether material conditions or ideational shifts are the more fundamental levers for change in such commitment systems.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(causal_direction_between_mechanisms, empirical, 'Clarifying the interplay between material and ideational factors in the decline of honor violence.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as a ''composite_reading'' of the honor_violence_legitimacy kernel, or does it lean too heavily towards one of the sibling readings (drop_reading or contraction_reading)?',
    'Expert review by legal anthropologists and historical sociologists, comparing the narrative and metrics against the established definitions of the ''drop'' and ''contraction'' mechanisms and their observed historical patterns.',
    'If the reading is misidentified, the analysis of the constraint''s decline and its implications for social change would be skewed, potentially overemphasizing one causal pathway over another.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Verifying the accurate classification of this reading within the honor_violence_legitimacy kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__composite_reading, 1800, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1800, honor_violence_legitimacy__composite_reading, theater_ratio, 1800, 0.05).
narrative_ontology:measurement(hono_tr_t1850, honor_violence_legitimacy__composite_reading, theater_ratio, 1850, 0.05).
narrative_ontology:measurement(hono_tr_t1900, honor_violence_legitimacy__composite_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(hono_tr_t1950, honor_violence_legitimacy__composite_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(hono_tr_t2000, honor_violence_legitimacy__composite_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(hono_tr_t2020, honor_violence_legitimacy__composite_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(hono_be_t1800, honor_violence_legitimacy__composite_reading, base_extractiveness, 1800, 0.8).
narrative_ontology:measurement(hono_be_t1850, honor_violence_legitimacy__composite_reading, base_extractiveness, 1850, 0.75).
narrative_ontology:measurement(hono_be_t1900, honor_violence_legitimacy__composite_reading, base_extractiveness, 1900, 0.7).
narrative_ontology:measurement(hono_be_t1950, honor_violence_legitimacy__composite_reading, base_extractiveness, 1950, 0.68).
narrative_ontology:measurement(hono_be_t2000, honor_violence_legitimacy__composite_reading, base_extractiveness, 2000, 0.66).
narrative_ontology:measurement(hono_be_t2020, honor_violence_legitimacy__composite_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1800, honor_violence_legitimacy__composite_reading, suppression_requirement, 1800, 0.9).
narrative_ontology:measurement(hono_su_t1850, honor_violence_legitimacy__composite_reading, suppression_requirement, 1850, 0.85).
narrative_ontology:measurement(hono_su_t1900, honor_violence_legitimacy__composite_reading, suppression_requirement, 1900, 0.8).
narrative_ontology:measurement(hono_su_t1950, honor_violence_legitimacy__composite_reading, suppression_requirement, 1950, 0.75).
narrative_ontology:measurement(hono_su_t2000, honor_violence_legitimacy__composite_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(hono_su_t2020, honor_violence_legitimacy__composite_reading, suppression_requirement, 2020, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__composite_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_violence_legitimacy__composite_reading, 0.08).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy__contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'honor_violence_legitimacy' kernel. The 'composite_reading' posits that both external costs (drop) and internal conceptual redefinition (contraction) operated simultaneously to delegitimize honor violence. The 'drop_reading' focuses solely on external costs, while the 'contraction_reading' focuses on the redefinition of honor itself. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
