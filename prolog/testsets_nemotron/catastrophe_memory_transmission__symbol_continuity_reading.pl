% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__symbol_continuity_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: catastrophe_memory_transmission__symbol_continuity_reading
 *   human_readable: Catastrophe Memory Transmission — Symbol Continuity Reading
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   A community transmits memory of a founding catastrophe through a ritual
 *   with fixed symbolic form. The ritual is claimed to be the survival
 *   mechanism itself — preserving identity and mourning practice as intrinsic
 *   communal goods. This reading (symbol_continuity_reading) instantiates a
 *   constraint where high ritual fidelity is enforced, adaptive capacity is
 *   the extracted cost, and the beneficiary is communal identity continuity.
 *   The constraint is authored as tangled_rope: it performs genuine
 *   coordination (identity transmission across generations) AND asymmetric
 *   extraction (adaptive practitioners and younger generations pay the cost
 *   of frozen form). The kernel contestation is routed to omegas and
 *   cs_structure per committer-frame rules.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__symbol_continuity_reading, 0.42).
domain_priors:suppression_score(catastrophe_memory_transmission__symbol_continuity_reading, 0.35).
domain_priors:theater_ratio(catastrophe_memory_transmission__symbol_continuity_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__symbol_continuity_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__symbol_continuity_reading, "Catastrophe Memory Transmission — Symbol Continuity Reading").
narrative_ontology:topic_domain(catastrophe_memory_transmission__symbol_continuity_reading, "religious_studies/collective_memory/ritual_studies").

domain_priors:requires_active_enforcement(catastrophe_memory_transmission__symbol_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__symbol_continuity_reading, '0850c268-e2e0-4a8b-85d7-74903fedfaab').
narrative_ontology:cs_kernel_codification('0850c268-e2e0-4a8b-85d7-74903fedfaab', fixed_text).
narrative_ontology:cs_authority_grounding('0850c268-e2e0-4a8b-85d7-74903fedfaab', lineage).
narrative_ontology:cs_interpretation_layer_present('0850c268-e2e0-4a8b-85d7-74903fedfaab').
narrative_ontology:cs_reading_relation('0850c268-e2e0-4a8b-85d7-74903fedfaab', catastrophe_memory_transmission__hybrid_embedded_reading, coexists_with).
narrative_ontology:cs_reading_relation('0850c268-e2e0-4a8b-85d7-74903fedfaab', catastrophe_memory_transmission__operational_competence_reading, coexists_with).
narrative_ontology:cs_axiom('0850c268-e2e0-4a8b-85d7-74903fedfaab', foundational, symbolic_form_is_survival_mechanism).
narrative_ontology:cs_axiom_status(symbolic_form_is_survival_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('0850c268-e2e0-4a8b-85d7-74903fedfaab', symbolic_form_is_survival_mechanism, deontological).
narrative_ontology:cs_axiom('0850c268-e2e0-4a8b-85d7-74903fedfaab', foundational, fidelity_to_form_preserves_identity).
narrative_ontology:cs_axiom_status(fidelity_to_form_preserves_identity, holdable).
narrative_ontology:cs_axiom_grounding('0850c268-e2e0-4a8b-85d7-74903fedfaab', fidelity_to_form_preserves_identity, deontological).
narrative_ontology:cs_axiom('0850c268-e2e0-4a8b-85d7-74903fedfaab', secondary, adaptation_is_betrayal_of_memory).
narrative_ontology:cs_axiom_status(adaptation_is_betrayal_of_memory, holdable).
narrative_ontology:cs_axiom_grounding('0850c268-e2e0-4a8b-85d7-74903fedfaab', adaptation_is_betrayal_of_memory, deontological).
narrative_ontology:cs_reference_frame('0850c268-e2e0-4a8b-85d7-74903fedfaab', catastrophe_survival_identity).
narrative_ontology:cs_drift_state('0850c268-e2e0-4a8b-85d7-74903fedfaab', contemporary_environmental_pressure, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0850c268-e2e0-4a8b-85d7-74903fedfaab', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, survivor_community).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, ritual_elders).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, descendant_groups).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_practitioners).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, younger_generations).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, reformist_factions).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__symbol_continuity_reading, identity_preservation_through_symbol).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__symbol_continuity_reading, mourning_as_communal_good).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__symbol_continuity_reading, form_transmission_as_survival).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The community that survived the catastrophe and maintains the ritual as the vessel of their collective identity. Their self-concept is fused with the symbolic form; exit would mean losing the coherence of their identity as survivors. They benefit from the ritual's continuity but bear the cost of policing fidelity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, survivor_community, beneficiary,
    organized, generational, identity_locked, regional).

% The designated transmitters and guardians of the ritual form. Their authority derives entirely from their fidelity to the inherited symbolic structure. They administer the constraint by judging deviations, training successors, and controlling the conditions of performance. Their status and identity are constituted through this role; exit is professionally and existentially impossible.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, ritual_elders, agenda_setter,
    institutional, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__symbol_continuity_reading, ritual_elders, beneficiary).

% Groups descended from the survivor community who inherit the ritual as their primary link to the catastrophe and their collective identity. They benefit from the identity continuity the ritual provides but have limited power to shape its form. Exit means severing their connection to ancestry and communal belonging; mobility is constrained by the ritual's role as identity anchor.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, descendant_groups, beneficiary,
    moderate, generational, constrained, regional).

% Community members who seek to adapt the ritual to changed environmental, social, or epistemic conditions — incorporating new threats, new knowledge, or new communal needs. They pay the cost of the fidelity constraint: their adaptations are suppressed, their expertise is marginalized, and their proposals are treated as betrayals of the symbolic form. Exit is constrained because leaving the community means losing the very identity the ritual preserves.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_practitioners, payer,
    moderate, biographical, constrained, local).

% Young people born into the community who inherit the ritual as a non-negotiable identity requirement. They bear the cost of frozen symbolic form — the ritual cannot respond to their lived conditions, their questions, or the catastrophes they actually face. Their adaptive capacity is sacrificed to preserve a form that answers a past they did not live. Exit is identity-locked: rejecting the ritual means rejecting their community, their ancestors, and their own belonging.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, younger_generations, payer,
    powerless, biographical, identity_locked, local).

% Organized groups within or adjacent to the community advocating for ritual adaptation as a condition of communal survival. They are excluded from ritual authority structures; their proposals are treated as heresy or assimilation. They pay the cost of suppression: their organizational energy is diverted to resistance rather than adaptation, and their members face social sanction. Exit is trapped: they cannot effect change from within, and leaving abandons the community they seek to save.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, reformist_factions, payer,
    organized, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__symbol_continuity_reading, reformist_factions, excluded).

% Academic observers who document and analyze the ritual's transmission dynamics. They see the full structure: the identity-preservation function, the adaptive costs, the enforcement mechanisms, and the contestation between readings. They neither collect from nor pay into the constraint; their exit is analytical — they can change their interpretive frame without personal cost.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, ritual_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective identity and mourning across generations by fixing a symbolic form that all participants recognize as the authentic vessel of the catastrophe memory. The ritual solves the problem of how a community survives its own catastrophe without dissolving — it provides a stable referent for 'who we are' that outlives any individual.
% TRANSFER_FUNCTION: Transfers adaptive capacity (the ability to modify practice in response to new conditions) from adaptive_practitioners, younger_generations, and reformist_factions to the ritual_elders and survivor_community who hold the fixed symbolic form. The transfer is not monetary; it is the foreclosure of responsive action in favor of faithful repetition.
% ABSENT_VOICES: The dead of the catastrophe itself — the ritual claims to speak for them, but they cannot object to how their memory is used. Future generations beyond the current horizon — they will inherit whatever form the ritual has at that point, with no say in its shaping. Neighboring communities affected by the community's adaptive failures — when ritual fidelity prevents effective response to shared threats (e.g., environmental degradation, epidemic), the costs spill over to those not party to the ritual.
% DISAPPEARANCE_RATIONALE: If the ritual and its fidelity constraint vanished overnight, the survivor community would lose its primary identity anchor — the symbolic form that makes them 'the people who survived.' Mourning would lose its communal structure. The adaptive_practitioners and reformist_factions would be freed to develop new practices, but the community might fragment without the shared symbol. The world rearranges because the constraint currently holds the community's identity in a specific shape; removing it releases that shape.
% FOUNDING_PROBLEM: After the catastrophe, the community faced existential dissolution: loss of shared world, incoherent grief, no stable basis for collective action or identity transmission. The ritual was built to answer: how do we remain a people after this? The fixed symbolic form was the solution — a portable, repeatable anchor that could survive dispersal, oppression, and time.
% FOUNDING_PROBLEM_CORROBORATION: The survivor community and ritual elders attest the founding problem remains live — the catastrophe's shadow still threatens identity dissolution. Adaptive practitioners and reformist factions (documented in oral histories and community meeting records) attest the founding problem is substantially solved — the community has survived, reconstituted, and now faces new problems the ritual cannot address. Independent anthropologists of ritual transmission (e.g., studies of post-Holocaust liturgies, post-genocide memorial practices) corroborate the shifted-function reading: the ritual's original survival function has been achieved, and its persistence now serves identity maintenance rather than existential survival.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__symbol_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__symbol_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__symbol_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(catastrophe_memory_transmission__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__symbol_continuity_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__symbol_continuity_reading_tests).
:- end_tests(catastrophe_memory_transmission__symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the measured adaptive capacity foreclosed by fidelity enforcement — substantial but not total, because the coordination function (identity transmission) is real and valued by all seats. Suppression (0.35) is moderate: enforcement operates through social sanction and identity threat rather than physical coercion, but is effective because exit is identity_locked for key agents. Theater ratio (0.18) is low: the ritual's performative elements serve the coordination function; little activity is purely theatrical. Accessibility collapse (0.65) is moderately high: alternative adaptive practices are cognitively available but structurally foreclosed by the identity logic. Resistance (0.28) is low-moderate: reformist_factions exist but are marginalized; younger_generations' resistance is largely internalized.
 *
 * PERSPECTIVAL GAP:
 *   From the survivor_community and ritual_elders' seats, the constraint is genuine coordination — the ritual works, identity holds, the community survives. From adaptive_practitioners and younger_generations' seats, the same structure operates as extraction — their capacity to respond to present conditions is harvested to preserve a past form. The engine computes this divergence from the structural data; the authored claim (tangled_rope) asserts both functions are real simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   survivor_community and ritual_elders are structural beneficiaries (d near 0.0): the constraint subsidizes their identity coherence and authority. descendant_groups are moderate beneficiaries (d ~0.3): they receive identity continuity but bear some adaptive cost. adaptive_practitioners are symmetric-to-target (d ~0.6): they contribute expertise that is suppressed. younger_generations are full targets (d ~0.9): identity-locked, they bear the full adaptive cost with no voice. reformist_factions are excluded targets (d ~0.95): trapped, they pay organizational costs for suppressed advocacy. ritual_studies_scholars are analytical observers (d = 0.5): symmetric by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (existential identity dissolution after catastrophe) is contested: the community has survived, but the ritual's defenders argue the threat of dissolution persists. The mandate has not atrophied to piton — the coordination function is actively valued, not merely performed. But the extraction has accumulated as the environment changed and the ritual did not. This is the tangled_rope zone: coordination persists, extraction accumulates, enforcement hardens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the symbol_continuity_reading a distinct constraint from the hybrid_embedded_reading and operational_competence_reading, or do they describe different aspects of the same constraint?',
    'Test whether the three readings produce different beneficiary/victim structures, different extractiveness values under the same referent, and different type classifications when authored as separate constraint stories. If ε-invariance holds (each reading has a stable ε across observables), they are distinct constraints linked by network.affects_constraints.',
    'If distinct, each reading gets its own constraint story with its own ε, stakeholders, and classification. If they are one constraint, the symbol_continuity_reading''s metrics conflate coordination and extraction that the other readings separate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether the kernel decomposes into multiple ε-invariant constraints per the BGS principle.').

omega_variable(
    identity_lock_mechanism,
    'What specific identity-fusion mechanism binds younger_generations and ritual_elders to the ritual such that exit is identity_locked rather than merely constrained?',
    'Ethnographic investigation of identity narratives: does the ritual constitute the self (relational/ideological identity) or merely express a pre-existing identity (institutional identity)? Longitudinal study of exit attempts — do those who leave report identity rupture or merely social sanction?',
    'If identity is constituted through the ritual (relational/ideological), the constraint''s extraction is amplified by identity_lock — χ approaches full-target values. If identity merely uses the ritual (institutional), exit_options should be ''constrained'' and effective extraction is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Mechanism of identity fusion that produces identity_locked exit options.').

omega_variable(
    adaptive_cost_measurement,
    'Can the adaptive capacity sacrificed to ritual fidelity be measured, or is it inherently counterfactual?',
    'Compare communities with similar catastrophe histories but different ritual fidelity constraints: measure survival outcomes, innovation rates, and threat response times. Natural experiments from communities where the ritual was disrupted (migration, suppression) and later restored vs. those where it adapted continuously.',
    'If measurable and substantial, the extraction value (0.42) is supported. If counterfactual and unmeasurable, the extraction may be over-authored — the constraint may be closer to rope (coordination without asymmetric extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adaptive_cost_measurement, empirical, 'Whether the victim-side extraction (adaptive capacity loss) is empirically grounded.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__symbol_continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 60, 0.13).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 80, 0.16).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 100, 0.18).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 40, 0.3).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 60, 0.36).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 80, 0.4).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 20, 0.18).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 40, 0.25).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 60, 0.28).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 80, 0.32).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 100, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__symbol_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_transmission__symbol_continuity_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission__hybrid_embedded_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission__operational_competence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_memory_transmission kernel. The three readings decompose the kernel into distinct ε-invariant constraints: symbol_continuity_reading (identity maintenance vs. environmental responsiveness, tangled_rope), hybrid_embedded_reading (inseparable form/competence, likely rope or mountain), operational_competence_reading (competence transmission, likely rope). They are linked via affects_constraints because the kernel's natural-language label conflates them; the ε-invariance principle requires separate stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_transmission__symbol_continuity_reading, organized, 0.3).
constraint_indexing:directionality_override(catastrophe_memory_transmission__symbol_continuity_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
