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
 *   constraint_id: honor_violence_legitimacy__contraction_reading
 *   human_readable: Honor Redefined to Exclude Violence (Contraction Reading)
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes a historical shift in the definition of honor,
 *   where violence, particularly dueling, became conceptually incompatible
 *   with true honor. This 'contraction reading' posits that the very
 *   framework of honor changed, making dueling structurally unthinkable
 *   rather than merely costly or illegal. It is presented as a Mountain
 *   because the conceptual shift, once established, became an unchangeable
 *   feature of the social landscape, persisting regardless of individual
 *   enforcement. The beneficiaries are civil society and the state, which
 *   gain from reduced private violence and strengthened legal authority.
 *   Former duelists are 'payers' in the sense that their previous mode of
 *   honor defense became illegitimate, forcing a re-evaluation of their
 *   identity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, 0.15).
domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, 0.05).
domain_priors:theater_ratio(honor_violence_legitimacy__contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, extractiveness, 0.15).
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
narrative_ontology:cs_story_uid(honor_violence_legitimacy__contraction_reading, '06cdd707-7462-49d2-bade-f6784d1cc75d').
narrative_ontology:cs_kernel_codification('06cdd707-7462-49d2-bade-f6784d1cc75d', implicit).
narrative_ontology:cs_authority_grounding('06cdd707-7462-49d2-bade-f6784d1cc75d', practice).
narrative_ontology:cs_interpretation_layer_present('06cdd707-7462-49d2-bade-f6784d1cc75d').
narrative_ontology:cs_reading_relation('06cdd707-7462-49d2-bade-f6784d1cc75d', honor_violence_legitimacy__drop_reading, influences).
narrative_ontology:cs_reading_relation('06cdd707-7462-49d2-bade-f6784d1cc75d', honor_violence_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('06cdd707-7462-49d2-bade-f6784d1cc75d', foundational, honor_is_non_violent_virtue).
narrative_ontology:cs_axiom_status(honor_is_non_violent_virtue, holdable).
narrative_ontology:cs_axiom_grounding('06cdd707-7462-49d2-bade-f6784d1cc75d', honor_is_non_violent_virtue, deontological).
narrative_ontology:cs_axiom('06cdd707-7462-49d2-bade-f6784d1cc75d', secondary, private_violence_is_dishonorable).
narrative_ontology:cs_axiom_status(private_violence_is_dishonorable, holdable).
narrative_ontology:cs_axiom_grounding('06cdd707-7462-49d2-bade-f6784d1cc75d', private_violence_is_dishonorable, conventional).
narrative_ontology:cs_reference_frame('06cdd707-7462-49d2-bade-f6784d1cc75d', honor_as_civic_virtue).
narrative_ontology:cs_drift_state('06cdd707-7462-49d2-bade-f6784d1cc75d', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('06cdd707-7462-49d2-bade-f6784d1cc75d', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, civil_society).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, state_legal_apparatus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, former_duelists).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, non_violence_as_honor).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, state_monopoly_on_violence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the reduction of private violence and the redefinition of honor towards civic virtues. Actively promotes and reinforces the new understanding of honor.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, civil_society, beneficiary,
    organized, generational, analytical, national).

% Benefits from the state's strengthened monopoly on violence and the reduced need to prosecute dueling. Its legitimacy is enhanced by the alignment of social norms with legal prohibitions.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, state_legal_apparatus, beneficiary,
    institutional, generational, analytical, national).

% Individuals who previously saw dueling as a legitimate expression of honor now face social opprobrium and legal consequences for engaging in it. Their identity as honorable men is now tied to non-violence, making dueling unthinkable.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, former_duelists, payer,
    moderate, biographical, identity_locked, local).

% Academics and intellectuals who study the evolution of honor codes and their relationship to violence. They analyze the conceptual shifts and their societal impacts.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, honor_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social expectations around honor, shifting the definition to align with non-violent resolution of disputes and civic responsibility, thereby reducing social friction and private violence.
% TRANSFER_FUNCTION: Transfers the social cost of private violence (deaths, injuries, feuds) from individuals and society to a conceptual framework where such violence is no longer legitimate. It also transfers the burden of dispute resolution to formal legal systems.
% ABSENT_VOICES: Those who clung to older, more violent conceptions of honor were marginalized and eventually silenced by the dominant cultural shift. Their voices would argue for the 'right' to defend honor through personal combat, but their framework became socially unintelligible.
% DISAPPEARANCE_RATIONALE: If this redefinition of honor vanished, it would mean a return to a conceptual space where dueling is thinkable, but the societal structures and legal norms that replaced it are now deeply entrenched. The world would not immediately revert to widespread dueling, but the conceptual barrier would be gone.
% FOUNDING_PROBLEM: The problem of pervasive private violence and challenges to state authority stemming from a cultural code that legitimized dueling as a means of dispute resolution and honor defense.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, legal reforms, and sociological analyses from the period corroborate that the problem of dueling was a significant societal concern. The widespread acceptance of the new honor code by civil society and state institutions confirms the problem's resolution, as attested by historians and legal scholars.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_violence_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__contraction_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
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
 *   Extractiveness is low and decreasing because the constraint primarily operates by redefining a conceptual space, not by active extraction. Suppression is also low and decreasing as the new definition of honor becomes internalized and self-enforcing, requiring less external coercion. Accessibility collapse is high (0.9) because the conceptual space for dueling as an honorable act largely vanished. Resistance is low (0.05) because the shift was a broad cultural redefinition, not a contested policy. The temporal measurements reflect the gradual but decisive shift in social norms, with extractiveness and suppression declining as the new definition of honor solidified.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of civil society and the state, this was a beneficial evolution towards a more civilized order. From the perspective of those who adhered to older honor codes, it was a loss of a fundamental right or a degradation of true honor. However, the 'contraction reading' emphasizes that the conceptual space itself changed, making the old perspective increasingly unintelligible within the new framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil society and the state are beneficiaries as the redefinition of honor aligns with their interests in peace and order. Former duelists are payers because their previous identity and means of dispute resolution are invalidated, forcing them to adapt to a new social code. Honor theorists are observers, analyzing the phenomenon without direct participation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to reduce private violence by redefining honor) was successfully resolved, and the constraint persists as a 'dead' founding problem. However, because it operates as a conceptual Mountain, its persistence is not due to inertia or theatrical maintenance (low theater_ratio), but because the underlying conceptual framework has genuinely shifted. It is not a Piton because it is not a degraded function, but a fundamental redefinition that became self-sustaining.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_vs_material_causation,
    'To what extent was the decline of dueling primarily driven by conceptual redefinition (this reading) versus material costs and state suppression (the ''drop_reading'' sibling)?',
    'Comparative historical analysis across different regions and time periods, examining the sequence and relative timing of conceptual shifts, legal prohibitions, and economic changes.',
    'If material costs were dominant, this constraint''s ''mountain'' classification would be less accurate, and the ''drop_reading'' (more extractive, snare-like) would gain explanatory power. If conceptual redefinition was primary, this reading''s ''mountain'' classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conceptual_vs_material_causation, empirical, 'Distinguishing conceptual vs. material drivers of dueling''s decline.').

omega_variable(
    honor_redefinition_naturalness,
    'Was the redefinition of honor a ''natural'' evolution of social norms, or was it actively engineered and enforced by specific social and political actors?',
    'Detailed historical sociological study of the agents and institutions promoting the new honor code, and the mechanisms they used to achieve its widespread adoption.',
    'If actively engineered, the ''emerges_naturally: true'' claim would be challenged, potentially reclassifying this as a ''tangled_rope'' or ''snare'' that benefited specific agenda-setters, even if the conceptual shift became self-sustaining.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_redefinition_naturalness, conceptual, 'Assessing the ''naturalness'' of the honor redefinition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__contraction_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1700, 0.25).
narrative_ontology:measurement(hono_be_t1750, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1750, 0.2).
narrative_ontology:measurement(hono_be_t1800, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1800, 0.15).
narrative_ontology:measurement(hono_be_t1850, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1850, 0.1).
narrative_ontology:measurement(hono_be_t1900, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1900, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1700, 0.3).
narrative_ontology:measurement(hono_su_t1750, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1750, 0.2).
narrative_ontology:measurement(hono_su_t1800, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1800, 0.1).
narrative_ontology:measurement(hono_su_t1850, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1850, 0.05).
narrative_ontology:measurement(hono_su_t1900, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1900, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__composite_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, state_monopoly_on_violence).

% DUAL FORMULATION NOTE:
% This constraint is the 'contraction reading' of the 'honor_violence_legitimacy' kernel, focusing on the conceptual redefinition of honor. It is linked to the 'drop_reading' (external costs) and 'composite_reading' (both factors) as part of a constraint family exploring the decline of dueling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
