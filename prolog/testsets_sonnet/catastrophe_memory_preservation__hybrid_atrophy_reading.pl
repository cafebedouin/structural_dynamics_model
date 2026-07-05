% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__hybrid_atrophy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__hybrid_atrophy_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_preservation__hybrid_atrophy_reading
 *   human_readable: Catastrophe-Memory Ritual (Hybrid Atrophy Reading): Survival Practice Decayed to Mourning Theater
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This story is the HYBRID reading of the catastrophe-memory-preservation
 *   kernel: it claims the ritual once did real coordination work
 *   (transmitting operational hazard-recognition competence) and that this
 *   function has genuinely atrophied under modernity, leaving a commemorative
 *   shell that still extracts costs from present practitioners without
 *   returning the adaptive payoff the original exchange offered. This is
 *   distinct from the survival_competence_reading (which claims the
 *   operational transfer is still substantially intact) and the
 *   mourning_practice_reading (which claims the ritual never needed
 *   operational content — symbolic continuity was always the real function
 *   and remains fully served). The three readings are not three observations
 *   of one constraint; they are three different structural claims with
 *   different ε trajectories, different beneficiary/victim sets, and
 *   different classifications, linked as siblings of one kernel.
 *
 * KEY AGENTS:
 *   - communal_identity_custodians: administer the observance, benefit from continuity of communal identity, no longer transmit operational competence
 *   - present_generation_practitioners: bear the time/social cost of compliance without adaptive payoff
 *   - historical_ancestors_original_beneficiaries: non-agent marker of the rite's original, now-vacated, coordination function
 *   - secular_youth_defectors: excluded voice already exited, evidence largely unconsulted
 *   - folklorists_and_ethnographers: outside observers who corroborate the atrophy claim against archival hazard records
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.42).
domain_priors:suppression_score(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.38).
domain_priors:theater_ratio(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__hybrid_atrophy_reading, piton).
narrative_ontology:human_readable(catastrophe_memory_preservation__hybrid_atrophy_reading, "Catastrophe-Memory Ritual (Hybrid Atrophy Reading): Survival Practice Decayed to Mourning Theater").
narrative_ontology:topic_domain(catastrophe_memory_preservation__hybrid_atrophy_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__hybrid_atrophy_reading, 'bd9af773-30bd-41b2-9823-f45cf9dcd4c0').
narrative_ontology:cs_kernel_codification('bd9af773-30bd-41b2-9823-f45cf9dcd4c0', distributed).
narrative_ontology:cs_authority_grounding('bd9af773-30bd-41b2-9823-f45cf9dcd4c0', practice).
narrative_ontology:cs_interpretation_layer_present('bd9af773-30bd-41b2-9823-f45cf9dcd4c0').
narrative_ontology:cs_reading_relation('bd9af773-30bd-41b2-9823-f45cf9dcd4c0', catastrophe_memory_preservation__survival_competence_reading, influences).
narrative_ontology:cs_reading_relation('bd9af773-30bd-41b2-9823-f45cf9dcd4c0', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_axiom('bd9af773-30bd-41b2-9823-f45cf9dcd4c0', foundational, operational_function_has_genuinely_atrophied).
narrative_ontology:cs_axiom_status(operational_function_has_genuinely_atrophied, holdable).
narrative_ontology:cs_axiom_grounding('bd9af773-30bd-41b2-9823-f45cf9dcd4c0', operational_function_has_genuinely_atrophied, empirically_contingent).
narrative_ontology:cs_axiom('bd9af773-30bd-41b2-9823-f45cf9dcd4c0', secondary, residual_identity_function_justifies_continued_cost).
narrative_ontology:cs_axiom_status(residual_identity_function_justifies_continued_cost, holdable).
narrative_ontology:cs_axiom_grounding('bd9af773-30bd-41b2-9823-f45cf9dcd4c0', residual_identity_function_justifies_continued_cost, conventional).
narrative_ontology:cs_reference_frame('bd9af773-30bd-41b2-9823-f45cf9dcd4c0', operational_hazard_transmission_framework).
narrative_ontology:cs_drift_state('bd9af773-30bd-41b2-9823-f45cf9dcd4c0', post_industrial_settlement_change, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bd9af773-30bd-41b2-9823-f45cf9dcd4c0', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, communal_identity_custodians).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_practitioners).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__hybrid_atrophy_reading, continuity_of_ancestral_observance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elders, clergy, and cultural associations administer the annual observance calendar, decide which elements of the old catastrophe-avoidance rite are retained, and derive social standing and continuity of communal identity from keeping the practice alive. They no longer possess or transmit the operational knowledge (flood-timing signs, famine-precursor readings, terrain hazard markers) the rite once encoded — what they administer now is the commemorative shell. They benefit from the rite's persistence as a marker of belonging, not from any surviving competence it confers.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, communal_identity_custodians, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__hybrid_atrophy_reading, communal_identity_custodians, agenda_setter).

% Younger community members are expected to fast, travel, take time off work, and perform the full ritual sequence annually. They inherit the time cost, opportunity cost, and social pressure of compliance without inheriting any working survival-relevant skill the ritual once transmitted alongside its symbolism. Non-participation carries reputational cost inside the community; participation carries no adaptive payoff outside it. Their exit is blocked less by external coercion than by identity fusion — leaving the practice reads as leaving the community itself.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_practitioners, payer,
    moderate, biographical, identity_locked, local).

% Named for completeness: the rite's original function (pre-modern generations who used the embedded warning-signs and seasonal cues to avoid recurring famine, flood, or predator hazard) is not an active agent today. It is retained here to mark that the constraint's coordination rationale is historical, not present — the beneficiary of the ORIGINAL function no longer exists as a party to evaluate.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, historical_ancestors_original_beneficiaries, beneficiary,
    analytical, civilizational, analytical, regional).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_preservation__hybrid_atrophy_reading, historical_ancestors_original_beneficiaries).

% Community members who have already stopped participating, citing the rite's lack of practical relevance. They are rarely consulted in decisions about whether or how the observance should adapt; the custodial layer treats their absence as attrition to manage rather than as evidence to weigh.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, secular_youth_defectors, excluded,
    powerless, biographical, mobile, local).

% Academic observers who document the rite's historical function, compare surviving fragments against archival hazard records, and can trace the specific point at which operational content was lost from the transmitted sequence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, folklorists_and_ethnographers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_preservation__hybrid_atrophy_reading, communal_identity_custodians).
narrative_ontology:fixing_cost_class(catastrophe_memory_preservation__hybrid_atrophy_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally: synchronized seasonal vigilance and transmitted embedded hazard-recognition cues (timing of floods, famine precursors, terrain dangers) across generations through memorable, repeatable ritual form. Presently: synchronizes communal identity and provides a shared occasion for in-group affirmation — a real but different coordination function than the one that justified the rite's original form.
% TRANSFER_FUNCTION: Moves time, discretionary income, and social-compliance labor from present-generation practitioners to the maintenance of an observance calendar administered by identity custodians, in exchange for continued communal standing. No operational competence is transferred in return, unlike the original exchange.
% ABSENT_VOICES: Secular youth who have already exited are rarely part of the deliberation over whether the ritual's form should be revised or its costs reduced; their disengagement is read by custodians as decline to be resisted rather than as data about the practice's current cost-benefit balance for the paying generation.
% DISAPPEARANCE_RATIONALE: Custodians and many practitioners would say the world rearranges sharply — communal identity and intergenerational continuity would visibly fray. Ethnographic observers and the excluded defector population would say the operationally relevant world is already unchanged, since the ritual's original hazard-avoidance function is already gone; what would end is a costly performance, not a functioning safeguard. The verdict genuinely depends on which function ('identity maintenance' vs. 'survival competence') is taken as the live one.
% FOUNDING_PROBLEM: Recurring, high-cost environmental catastrophe (flood, famine, predation) that early communities needed a reliable, memorable, cross-generational transmission method to anticipate and avoid, in the absence of writing or centralized record-keeping.
% FOUNDING_PROBLEM_CORROBORATION: Ethnographic and archival hazard-record comparison by outside academic observers (folklorists_and_ethnographers) corroborates that the specific environmental hazards the original ritual sequence encoded no longer track present conditions (altered hydrology, modern famine early-warning systems, settlement pattern change) and that the operational content has been demonstrably lost from the transmitted sequence within living memory of at least two generations. This corroboration comes from outside the custodial beneficiary group; the custodians themselves report the founding problem as still 'symbolically live,' which is a different and non-corroborating claim about the identity function, not the survival function.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__hybrid_atrophy_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__hybrid_atrophy_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__hybrid_atrophy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).
:- end_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate-high (0.58) and declines to moderate (0.42) over the interval — this reading claims the constraint is becoming LESS extractive over time not because the underlying practice becomes fairer but because participation itself is eroding (secular defection, shrinking compliance base), which naturally reduces aggregate extraction even as theater ratio rises. Theater ratio rises sharply (0.22 to 0.71) as the operational content visibly hollows out and is replaced by performative, symbolic-only observance — this is the atrophy signature the hybrid reading is built to register. Suppression (0.38) and resistance (0.4) are moderate: there is no active enforcement apparatus, but identity-lock and reputational cost create real friction against exit, and there is real (if incomplete) resistance from the defector population.
 *
 * PERSPECTIVAL GAP:
 *   Custodians experience the constraint as continuous, functioning coordination (they administer it, and from their seat the rite still 'protects' the community, now understood as protecting identity rather than lives). Present-generation practitioners paying the cost without adaptive return experience something closer to imposed inherited obligation. The engine should compute these as structurally different seats given the same authored data — that divergence is the point of the hybrid reading, distinguishing it from a reading where all parties would agree on function.
 *
 * DIRECTIONALITY LOGIC:
 *   Communal identity custodians sit near the beneficiary end: they administer the practice, collect social capital and continuity value from its persistence, and bear little of its opportunity cost. Present-generation practitioners sit near the target end: identity-locked exit, real time and money cost, no adaptive return. The vacated original beneficiary (historical ancestors) is marked non-agent and excluded from directionality computation — it cannot collect anything now, which is precisely the atrophy this reading asserts.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid_atrophy_reading is a direct mandatrophy case: the founding problem (recurring environmental catastrophe requiring cross-generational hazard-transmission) is corroborated dead by outside observers, yet the arrangement persists — reclassified onto a different, real-but-different function (identity maintenance) that the custodial layer now uses to justify continued cost extraction from practitioners who receive none of the original payoff. Classifying this as piton (rather than snare) matters: no party is concentratedly profiteering from active coercion — custodians gain diffuse status rather than concentrated rent, and enforcement is social/reputational rather than institutional. The rising theater_ratio and declining extractiveness together are the piton signature: hollowing function, persistent but weakening structure, no concentrated capturer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    atrophy_versus_original_symbolism,
    'Did the ritual ever actually encode transmissible operational hazard-recognition content, or has the hybrid_atrophy_reading retrojected a ''lost survival function'' onto a practice that was always primarily symbolic (as the mourning_practice_reading claims)?',
    'Comparative ethnographic and archival analysis: cross-reference the ritual''s specific timing, gestures, and taboos against documented historical hazard patterns (flood cycles, famine years, predator migration) in the region. If the ritual''s structural elements correlate tightly with historically documented hazard timing, the hybrid/atrophy reading is supported; if no correlation is found, the mourning_practice_reading''s claim that there was never operational content is supported instead.',
    'If no correlation is found, this constraint story should not exist in its current form — the kernel would collapse to two readings (survival_competence_reading would also be undermined) rather than three, and the entire hybrid-atrophy framing would be a retrojection rather than a genealogical fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrophy_versus_original_symbolism, empirical, 'Whether the ritual ever encoded real operational content, which the hybrid reading''s entire premise depends on.').

omega_variable(
    custodian_awareness_of_atrophy,
    'Do the communal_identity_custodians privately recognize that the operational function is gone and are consciously repurposing the ritual as identity maintenance, or do they sincerely believe the original hazard-avoidance function is still active (making their claim continuous with survival_competence_reading from their own seat)?',
    'Interview-based fieldwork distinguishing sincere belief from strategic reframing among custodial leadership; look for internal doctrinal statements that explicitly justify retained elements on identity grounds versus continued claims of literal protective efficacy.',
    'If custodians sincerely believe the survival function persists, the constraint is better modeled from their seat as an honest false-summit case (claimed mountain/rope with FSM-relevant beneficiary structure) rather than a knowing piton-style repurposing; this would sharpen rather than resolve the seat divergence already noted in commentary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(custodian_awareness_of_atrophy, conceptual, 'Whether the custodial belief in continued protective function is sincere or strategic, which shapes whether this is deception or genuine institutional drift.').

omega_variable(
    sibling_reading_boundary_location,
    'Where exactly does the disagreement between the three sibling readings live — is it a factual dispute about what the ritual once did, or a values dispute about which function (survival vs. identity) counts as the ''real'' one even if both are simultaneously present in degraded form?',
    'Structural analysis of whether the three readings could in principle be reconciled by a fourth reading that assigns partial weight to both functions across the interval, versus whether the readings are genuinely mutually exclusive claims about a single historical trajectory.',
    'If reconcilable, the three-way kernel split may be an artifact of forcing a continuous historical process into discrete reading buckets; if genuinely exclusive, the kernel split is structurally warranted and each reading should retain a stable, distinct ε.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_boundary_location, conceptual, 'Whether the kernel''s three-reading structure reflects genuine mutual exclusivity or an artifact of discretizing a continuous drift process.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__hybrid_atrophy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(cata_tr_t8, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(cata_tr_t16, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 16, 0.48).
narrative_ontology:measurement(cata_tr_t24, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 24, 0.58).
narrative_ontology:measurement(cata_tr_t32, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 32, 0.65).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 40, 0.71).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(cata_be_t8, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(cata_be_t16, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(cata_be_t24, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 24, 0.46).
narrative_ontology:measurement(cata_be_t32, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 32, 0.44).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 40, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_preservation__hybrid_atrophy_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__hybrid_atrophy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, mourning_practice_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings decomposed from the natural-language label 'catastrophe memory preservation ritual.' The three readings share a kernel (the ritual's persisting form) but diverge sharply on ε trajectory, beneficiary/victim structure, and classification: survival_competence_reading claims stable low extractiveness (rope/mountain-adjacent, function intact); mourning_practice_reading claims stable moderate extractiveness with no atrophy (rope, function was always symbolic); this hybrid_atrophy_reading claims declining extractiveness with rising theater_ratio (piton, function has decayed from operational to symbolic). Each carries its own ε and its own stakeholder set per the ε-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
