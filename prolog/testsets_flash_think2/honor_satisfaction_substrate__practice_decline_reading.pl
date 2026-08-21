% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__practice_decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__practice_decline_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: honor_satisfaction_substrate__practice_decline_reading
 *   human_readable: Decline of Dueling due to Exogenous Enforcement (Honor Code Reading)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint is the 'practice_decline_reading' of the
 *   'honor_satisfaction_substrate' kernel. It posits that the honor code
 *   itself persists as a normative substrate, but the practice of dueling
 *   declined primarily due to external legal and institutional enforcement,
 *   rather than an internal transformation of the honor code or a complete
 *   societal shift away from honor culture. The constraint's claimed type is
 *   'rope' as per prompt guidance, reflecting an underlying coordination
 *   function (moving away from private violence), but the authored metrics
 *   (high suppression, high accessibility collapse for dueling) reflect the
 *   significant coercive overhead required to achieve this coordination,
 *   which the engine will measure as a divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__practice_decline_reading, 0.45).
domain_priors:suppression_score(honor_satisfaction_substrate__practice_decline_reading, 0.8).
domain_priors:theater_ratio(honor_satisfaction_substrate__practice_decline_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__practice_decline_reading, rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__practice_decline_reading, "Decline of Dueling due to Exogenous Enforcement (Honor Code Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__practice_decline_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__practice_decline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__practice_decline_reading, '195047f5-8b21-486f-ae4b-8a17c97ae7ef').
narrative_ontology:cs_kernel_codification('195047f5-8b21-486f-ae4b-8a17c97ae7ef', implicit).
narrative_ontology:cs_authority_grounding('195047f5-8b21-486f-ae4b-8a17c97ae7ef', practice).
narrative_ontology:cs_interpretation_layer_present('195047f5-8b21-486f-ae4b-8a17c97ae7ef').
narrative_ontology:cs_reading_relation('195047f5-8b21-486f-ae4b-8a17c97ae7ef', honor_satisfaction_substrate__composite_overdetermined_reading, coexists_with).
narrative_ontology:cs_reading_relation('195047f5-8b21-486f-ae4b-8a17c97ae7ef', honor_satisfaction_substrate__cultural_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('195047f5-8b21-486f-ae4b-8a17c97ae7ef', foundational, honor_code_endures_as_social_force).
narrative_ontology:cs_axiom_status(honor_code_endures_as_social_force, holdable).
narrative_ontology:cs_axiom_grounding('195047f5-8b21-486f-ae4b-8a17c97ae7ef', honor_code_endures_as_social_force, conventional).
narrative_ontology:cs_axiom('195047f5-8b21-486f-ae4b-8a17c97ae7ef', foundational, dueling_decline_exogenous_cause).
narrative_ontology:cs_axiom_status(dueling_decline_exogenous_cause, holdable).
narrative_ontology:cs_axiom_grounding('195047f5-8b21-486f-ae4b-8a17c97ae7ef', dueling_decline_exogenous_cause, empirically_contingent).
narrative_ontology:cs_reference_frame('195047f5-8b21-486f-ae4b-8a17c97ae7ef', honor_code_governed_society).
narrative_ontology:cs_drift_state('195047f5-8b21-486f-ae4b-8a17c97ae7ef', post_legal_prohibition_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('195047f5-8b21-486f-ae4b-8a17c97ae7ef', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, society_at_large).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, legal_authorities).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, individuals_avoiding_dueling).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, individuals_seeking_honor_satisfaction_via_dueling).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, individuals_who_valued_dueling_ritual).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, military_honor_cultures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals, often from aristocratic or military backgrounds, felt honor-bound to duel to resolve certain affronts. The decline of dueling meant they lost a primary, culturally sanctioned method of satisfying honor, forcing them to accept legal recourse or perceived dishonor.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, individuals_seeking_honor_satisfaction_via_dueling, payer,
    powerless, biographical, constrained, local).

% Representing the state's monopoly on violence, legal authorities actively enforced prohibitions against dueling, viewing it as a challenge to public order and their judicial authority. They benefited from increased social stability and the consolidation of state power.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, legal_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Benefited from the reduction in private violence, arbitrary deaths, and social instability associated with dueling. The shift towards legal resolution of disputes contributed to a more predictable and less violent social order.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, society_at_large, beneficiary,
    organized, generational, mobile, national).

% These individuals saw dueling not just as a means of satisfaction but as a vital social ritual, a test of courage, and a marker of elite status. Their voices were increasingly marginalized as legal and social pressures mounted, leading to the loss of a valued cultural practice.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, individuals_who_valued_dueling_ritual, excluded,
    moderate, biographical, identity_locked, local).

% While dueling declined in civilian life, attenuated forms of honor codes persisted in military contexts and certain regional subcultures (e.g., the American South). These groups adapted the honor code to new forms of satisfaction, benefiting from its continued role in maintaining internal discipline and social cohesion, albeit without formal dueling.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, military_honor_cultures, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__practice_decline_reading, military_honor_cultures, beneficiary).

% Individuals who, for moral, religious, or pragmatic reasons, wished to avoid dueling benefited directly from its legal prohibition and social stigmatization, as it removed the pressure to participate in a potentially lethal practice.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, individuals_avoiding_dueling, beneficiary,
    powerless, immediate, mobile, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinated society away from private violence as a means of honor satisfaction, channeling disputes towards legal and institutional mechanisms, thereby establishing the state's monopoly on legitimate force.
% TRANSFER_FUNCTION: Transfers the right to adjudicate honor disputes and apply violence from individuals to the state, and transfers the social costs of dueling (death, injury, instability) into a more stable, legally governed social order.
% ABSENT_VOICES: Those who believed dueling was a necessary, honorable practice for maintaining social order and personal reputation, or who felt their honor could not be adequately satisfied by legal means, were increasingly excluded from the public discourse and legal frameworks.
% DISAPPEARANCE_RATIONALE: If the exogenous enforcement against dueling vanished overnight, the social dynamics around honor satisfaction would significantly shift. While dueling might not immediately return to its historical prevalence, the vacuum would create pressure for new forms of private dispute resolution, potentially leading to increased violence or the re-emergence of informal honor-based justice systems, challenging the state's authority.
% FOUNDING_PROBLEM: The founding problem was widespread private violence and social instability arising from honor disputes, which undermined state authority and public safety.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians, sociologists, and criminologists (outside the direct beneficiaries of the legal system) corroborate that the problem of private violence and social instability from honor disputes was a genuine concern, and its management by the state was a significant historical development.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__practice_decline_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__practice_decline_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__practice_decline_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(honor_satisfaction_substrate__practice_decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__practice_decline_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__practice_decline_reading_tests).
:- end_tests(honor_satisfaction_substrate__practice_decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.45) reflects the ongoing social costs and pressures of maintaining honor within the attenuated code, even without dueling, and the loss of a culturally significant means of satisfaction for some. Suppression (0.8) is high due to the active legal prohibitions and institutional barriers against dueling. Accessibility collapse (0.85) is high for dueling as a method of honor satisfaction, as it became largely impossible. Resistance (0.2) is low because the exogenous enforcement was largely effective. The theater ratio is low (0.1) as the enforcement against dueling was genuine, not performative. The temporal measurements show a clear increase in suppression over the century, corresponding to the decline of dueling, while extractiveness slightly decreases as the most extreme form of extraction (death in a duel) is removed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of legal authorities and society, the decline of dueling was a successful coordination effort, leading to greater stability. From the perspective of those who felt their honor could only be satisfied by dueling, or who valued the ritual, it was a loss of agency and cultural meaning, enforced by external power. The engine will compute these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Legal authorities and society at large are beneficiaries, gaining from reduced violence and consolidated state power. Individuals who felt honor-bound to duel, or who valued the ritual, are victims, losing a means of satisfaction or a cultural practice. Military honor cultures represent a mixed seat, adapting the honor code while benefiting from its continued role in discipline.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the decline of dueling as a 'mountain' (natural cultural evolution) or a 'snare' (pure extraction without coordination). By claiming 'rope' but showing high suppression, it highlights that while the underlying honor code (a coordination mechanism) persists, its most direct and extractive manifestation (dueling) was suppressed by external forces, leading to a 'coordination failure under legal pressure' rather than a natural atrophy or a purely extractive imposition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    honor_code_transformation_vs_persistence,
    'Did the honor code truly persist as a normative substrate, or did it undergo a foundational transformation (as suggested by the ''cultural_contraction_reading'')?',
    'Detailed historical-sociological analysis of primary sources (diaries, letters, legal records) to trace the evolution of honor concepts and their social functions, comparing regions where dueling declined with those where it persisted longer.',
    'If the honor code fundamentally transformed, the constraint might be reclassified towards a ''mountain'' (natural cultural shift) or a ''piton'' (vestigial social norm), rather than a ''rope'' whose function was externally suppressed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_code_transformation_vs_persistence, conceptual, 'Ambiguity regarding the persistence vs. transformation of the honor code itself.').

omega_variable(
    exogenous_vs_endogenous_decline_factors,
    'Was the decline of dueling solely due to exogenous enforcement, or were there significant endogenous factors (e.g., changing social values, internal delegitimation of dueling) that contributed (as suggested by the ''composite_overdetermined_reading'')?',
    'Comparative historical analysis across different legal and cultural contexts, examining the timing and mechanisms of dueling''s decline in relation to the introduction of legal prohibitions versus shifts in public opinion or elite behavior.',
    'If endogenous factors played a dominant role, the constraint''s suppression metric might be lower, and its classification might shift towards a ''piton'' (atrophied practice) or a ''tangled_rope'' (hybrid of internal and external pressures).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exogenous_vs_endogenous_decline_factors, empirical, 'Ambiguity regarding the primary drivers of dueling''s decline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__practice_decline_reading, 1800, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(hono_tr_t1820, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1820, 0.1).
narrative_ontology:measurement(hono_tr_t1840, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1840, 0.1).
narrative_ontology:measurement(hono_tr_t1860, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1860, 0.1).
narrative_ontology:measurement(hono_tr_t1880, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1880, 0.1).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1900, 0.1).

% Extraction over time
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1800, 0.55).
narrative_ontology:measurement(hono_be_t1820, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1820, 0.5).
narrative_ontology:measurement(hono_be_t1840, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1840, 0.48).
narrative_ontology:measurement(hono_be_t1860, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1860, 0.46).
narrative_ontology:measurement(hono_be_t1880, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1880, 0.45).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1900, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1800, 0.4).
narrative_ontology:measurement(hono_su_t1820, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1820, 0.55).
narrative_ontology:measurement(hono_su_t1840, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1840, 0.68).
narrative_ontology:measurement(hono_su_t1860, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1860, 0.75).
narrative_ontology:measurement(hono_su_t1880, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1880, 0.78).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1900, 0.8).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1800, tn=1900
narrative_ontology:measurement(hono_grid_01, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(class), 1800, 0.25).
narrative_ontology:measurement(hono_grid_02, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(class), 1900, 0.75).
narrative_ontology:measurement(hono_grid_03, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(individual), 1800, 0.2).
narrative_ontology:measurement(hono_grid_04, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(individual), 1900, 0.85).
narrative_ontology:measurement(hono_grid_05, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(organizational), 1800, 0.3).
narrative_ontology:measurement(hono_grid_06, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(organizational), 1900, 0.8).
narrative_ontology:measurement(hono_grid_07, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(structural), 1800, 0.15).
narrative_ontology:measurement(hono_grid_08, honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse(structural), 1900, 0.9).
narrative_ontology:measurement(hono_grid_09, honor_satisfaction_substrate__practice_decline_reading, resistance(class), 1800, 0.55).
narrative_ontology:measurement(hono_grid_10, honor_satisfaction_substrate__practice_decline_reading, resistance(class), 1900, 0.25).
narrative_ontology:measurement(hono_grid_11, honor_satisfaction_substrate__practice_decline_reading, resistance(individual), 1800, 0.6).
narrative_ontology:measurement(hono_grid_12, honor_satisfaction_substrate__practice_decline_reading, resistance(individual), 1900, 0.2).
narrative_ontology:measurement(hono_grid_13, honor_satisfaction_substrate__practice_decline_reading, resistance(organizational), 1800, 0.5).
narrative_ontology:measurement(hono_grid_14, honor_satisfaction_substrate__practice_decline_reading, resistance(organizational), 1900, 0.15).
narrative_ontology:measurement(hono_grid_15, honor_satisfaction_substrate__practice_decline_reading, resistance(structural), 1800, 0.4).
narrative_ontology:measurement(hono_grid_16, honor_satisfaction_substrate__practice_decline_reading, resistance(structural), 1900, 0.1).
narrative_ontology:measurement(hono_grid_17, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(class), 1800, 0.5).
narrative_ontology:measurement(hono_grid_18, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(class), 1900, 0.8).
narrative_ontology:measurement(hono_grid_19, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(individual), 1800, 0.7).
narrative_ontology:measurement(hono_grid_20, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(individual), 1900, 0.9).
narrative_ontology:measurement(hono_grid_21, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(organizational), 1800, 0.4).
narrative_ontology:measurement(hono_grid_22, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(organizational), 1900, 0.7).
narrative_ontology:measurement(hono_grid_23, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(structural), 1800, 0.3).
narrative_ontology:measurement(hono_grid_24, honor_satisfaction_substrate__practice_decline_reading, stakes_inflation(structural), 1900, 0.85).
narrative_ontology:measurement(hono_grid_25, honor_satisfaction_substrate__practice_decline_reading, suppression(class), 1800, 0.25).
narrative_ontology:measurement(hono_grid_26, honor_satisfaction_substrate__practice_decline_reading, suppression(class), 1900, 0.7).
narrative_ontology:measurement(hono_grid_27, honor_satisfaction_substrate__practice_decline_reading, suppression(individual), 1800, 0.3).
narrative_ontology:measurement(hono_grid_28, honor_satisfaction_substrate__practice_decline_reading, suppression(individual), 1900, 0.8).
narrative_ontology:measurement(hono_grid_29, honor_satisfaction_substrate__practice_decline_reading, suppression(organizational), 1800, 0.2).
narrative_ontology:measurement(hono_grid_30, honor_satisfaction_substrate__practice_decline_reading, suppression(organizational), 1900, 0.75).
narrative_ontology:measurement(hono_grid_31, honor_satisfaction_substrate__practice_decline_reading, suppression(structural), 1800, 0.1).
narrative_ontology:measurement(hono_grid_32, honor_satisfaction_substrate__practice_decline_reading, suppression(structural), 1900, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
