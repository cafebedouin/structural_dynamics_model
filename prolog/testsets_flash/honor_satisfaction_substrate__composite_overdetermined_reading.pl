% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__composite_overdetermined_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__composite_overdetermined_reading, []).

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
 *   constraint_id: honor_satisfaction_substrate__composite_overdetermined_reading
 *   human_readable: Overdetermined Decline of Dueling (Composite Reading)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint models the decline of dueling as an overdetermined
 *   process, where both external legal/institutional suppression and internal
 *   transformation of the honor code (endogenous delegitimation) operated
 *   simultaneously and with causally entangled pathways. It is a 'composite'
 *   reading because it integrates elements often treated as separate causes
 *   of decline. The constraint is claimed as a Tangled Rope, reflecting its
 *   initial coordination function for honor-bound gentlemen alongside its
 *   inherent risks and the eventual external enforcement that suppressed it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, 0.4).
domain_priors:suppression_score(honor_satisfaction_substrate__composite_overdetermined_reading, 0.7).
domain_priors:theater_ratio(honor_satisfaction_substrate__composite_overdetermined_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__composite_overdetermined_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__composite_overdetermined_reading, "Overdetermined Decline of Dueling (Composite Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__composite_overdetermined_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__composite_overdetermined_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__composite_overdetermined_reading, 'c69ec268-0f4e-45ed-b8d7-732f096bf149').
narrative_ontology:cs_kernel_codification('c69ec268-0f4e-45ed-b8d7-732f096bf149', implicit).
narrative_ontology:cs_authority_grounding('c69ec268-0f4e-45ed-b8d7-732f096bf149', practice).
narrative_ontology:cs_interpretation_layer_present('c69ec268-0f4e-45ed-b8d7-732f096bf149').
narrative_ontology:cs_reading_relation('c69ec268-0f4e-45ed-b8d7-732f096bf149', honor_satisfaction_substrate__practice_decline_reading, influences).
narrative_ontology:cs_reading_relation('c69ec268-0f4e-45ed-b8d7-732f096bf149', honor_satisfaction_substrate__cultural_contraction_reading, influences).
narrative_ontology:cs_axiom('c69ec268-0f4e-45ed-b8d7-732f096bf149', foundational, decline_is_multifactorial).
narrative_ontology:cs_axiom_status(decline_is_multifactorial, holdable).
narrative_ontology:cs_axiom_grounding('c69ec268-0f4e-45ed-b8d7-732f096bf149', decline_is_multifactorial, empirically_contingent).
narrative_ontology:cs_axiom('c69ec268-0f4e-45ed-b8d7-732f096bf149', foundational, exogenous_endogenous_causally_entangled).
narrative_ontology:cs_axiom_status(exogenous_endogenous_causally_entangled, holdable).
narrative_ontology:cs_axiom_grounding('c69ec268-0f4e-45ed-b8d7-732f096bf149', exogenous_endogenous_causally_entangled, empirically_contingent).
narrative_ontology:cs_reference_frame('c69ec268-0f4e-45ed-b8d7-732f096bf149', honor_code_as_social_regulator).
narrative_ontology:cs_drift_state('c69ec268-0f4e-45ed-b8d7-732f096bf149', late_19th_century, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('c69ec268-0f4e-45ed-b8d7-732f096bf149', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, social_order_maintainers).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, state_legal_apparatus).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, honor_bound_gentlemen).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, dueling_seconds).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals whose social standing and self-worth were tied to the honor code, making them susceptible to dueling. They bore the direct risks of dueling and the social costs of non-participation, but also gained status from adherence. As the substrate transformed, their identity lock weakened.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, honor_bound_gentlemen, payer,
    moderate, biographical, identity_locked, local).

% Facilitators and witnesses of duels, often friends or associates of the principals. They bore social and legal risks for their involvement, but also gained status and influence within the honor system. Their role became increasingly untenable as legal suppression intensified.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, dueling_seconds, payer,
    moderate, biographical, constrained, local).

% Elites and institutions (e.g., church, moral reformers) who sought to replace dueling with other forms of conflict resolution, viewing it as disruptive to public order and Christian morality. They benefited from the decline of dueling as it reduced social instability and violence.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, social_order_maintainers, beneficiary,
    institutional, generational, mobile, national).

% The legal and judicial system that actively criminalized dueling, enforced prohibitions, and sought to establish its monopoly on legitimate violence. It benefited from the expansion of its authority and the reduction of extra-legal violence.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, state_legal_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Scholars who analyze the historical and cultural forces that led to the decline of dueling, examining both external pressures and internal transformations of honor codes. They seek to understand the complex interplay of factors without direct involvement.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, cultural_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a formalized, if violent, mechanism for resolving disputes among gentlemen, maintaining social hierarchy, and validating personal honor within a specific social stratum.
% TRANSFER_FUNCTION: Transferred social status and perceived honor to participants who adhered to its rituals, while transferring risk of injury or death to the combatants and legal liability to all involved. Over time, it transferred authority over conflict resolution from individuals to the state.
% ABSENT_VOICES: Women, lower classes, and non-elites were largely excluded from the formal dueling code, though their honor could be implicated. They would have argued for a more equitable and less violent system of justice, but their voices were not central to the code's operation or decline.
% DISAPPEARANCE_RATIONALE: The disappearance of dueling fundamentally altered the social landscape for gentlemen, shifting how honor was defended and disputes resolved. It empowered the state's monopoly on violence and contributed to the rise of 'cultures of dignity' where individual worth was intrinsic rather than externally validated.
% FOUNDING_PROBLEM: To provide a structured means for gentlemen to defend their honor against perceived insults, thereby maintaining social order and personal reputation in a context where state legal systems were often insufficient or too slow for matters of personal affront.
% FOUNDING_PROBLEM_CORROBORATION: Historical legal records and sociological analyses from outside the direct participants corroborate that the problem of honor defense shifted from personal combat to legal and social mechanisms, rendering dueling's original function obsolete. Contemporary legal scholars and cultural anthropologists attest to the problem's death.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__composite_overdetermined_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__composite_overdetermined_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_satisfaction_substrate__composite_overdetermined_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).
:- end_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.4) reflects the high personal cost (risk of death/injury) inherent in dueling, even as it provided social benefits. Suppression (0.7) is high and rising, reflecting the increasing legal and institutional pressure against dueling. Accessibility collapse (0.75) is high because the combination of legal prohibition and changing social norms made dueling increasingly unthinkable and impractical. Resistance (0.2) is low, indicating that active defense of dueling as a practice waned over time. The claimed type is Tangled Rope, as it initially served a coordination function for honor, but its persistence became increasingly dependent on active enforcement and the suppression of alternatives (legal means of dispute resolution). The temporal measurements show rising suppression and declining extractiveness, as the state's power grew and the honor code's internal logic eroded.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the honor-bound gentlemen, the constraint was a necessary, if dangerous, means of maintaining social standing. From the state's perspective, it was an illegitimate challenge to its authority that needed to be suppressed. The composite reading acknowledges both internal and external dynamics, showing how the constraint's nature shifted from a self-regulated (though violent) coordination mechanism to a practice actively suppressed by a rising state.
 *
 * DIRECTIONALITY LOGIC:
 *   Honor-bound gentlemen and dueling seconds are targets (payers) as they bore the direct costs and risks. Social order maintainers and the state legal apparatus are beneficiaries/agenda-setters, as they gained authority and reduced social instability from dueling's decline. The identity_locked exit option for gentlemen reflects the powerful internal commitment to the honor code, which was a key factor in the constraint's initial persistence and later transformation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_pathway_disambiguation,
    'What was the precise causal weighting and interaction between exogenous legal suppression and endogenous honor code transformation in the decline of dueling?',
    'Detailed historical case studies comparing regions with differing legal enforcement intensities and cultural honor code resilience, using counterfactual analysis.',
    'A stronger weighting towards exogenous suppression would push the constraint closer to a Snare (pure extraction by the state); a stronger weighting towards endogenous transformation would emphasize the Mountain-like erosion of the honor substrate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_pathway_disambiguation, empirical, 'Disambiguating the relative causal contributions of external and internal factors to dueling''s decline.').

omega_variable(
    identity_lock_transformation,
    'How did the ''identity_locked'' status of honor-bound gentlemen transform as the honor code itself changed, and at what point did the internal commitment become ''constrained'' or ''mobile''?',
    'Analysis of personal diaries, correspondence, and literary works from the period to trace shifts in individual self-perception and social expectations regarding honor.',
    'Understanding the precise timing and mechanism of identity lock transformation would refine the temporal measurements of extractiveness and suppression, showing how internal delegitimation reduced the ''cost'' of non-participation even as external suppression increased.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_transformation, empirical, 'Tracing the evolution of identity lock as the honor code changed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__composite_overdetermined_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1700, 0.05).
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1750, 0.07).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1800, 0.09).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1850, 0.1).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1900, 0.1).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1700, 0.6).
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1750, 0.55).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1800, 0.48).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1850, 0.42).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1900, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1700, 0.3).
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1750, 0.45).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1800, 0.6).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1850, 0.68).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1900, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__composite_overdetermined_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate__practice_decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate__cultural_contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'honor_satisfaction_substrate' kernel, focusing on the composite, overdetermined nature of dueling's decline, integrating both external suppression and internal cultural transformation. It is linked to sibling readings that emphasize one factor over the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
