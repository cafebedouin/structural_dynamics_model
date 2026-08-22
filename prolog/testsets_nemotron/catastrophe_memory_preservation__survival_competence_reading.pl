% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__survival_competence_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: catastrophe_memory_preservation__survival_competence_reading
 *   human_readable: Ritual preserves operational threat-recognition capacity across generations
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint story instantiates the survival_competence_reading of the
 *   catastrophe_memory_preservation kernel. The ritual is a
 *   repetition-compulsion that encodes operational threat-recognition: the
 *   community that survived a catastrophe re-enacts the survival behaviors at
 *   regular intervals, paying high present costs (resource destruction,
 *   physical ordeal, time) to maintain the muscle memory of response. The
 *   extraction is real — present participants pay for future insurance — but
 *   the coordination function is also real: without the ritual, the knowledge
 *   decays below operational threshold. The constraint is a tangled_rope
 *   because it simultaneously coordinates (preserves genuine survival
 *   competence) and extracts (demands costly participation from present
 *   agents who may never face the catastrophe). The sister readings —
 *   mourning_practice_reading and hybrid_atrophy_reading — offer different
 *   structural accounts of the same ritual corpus.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, 0.78).
domain_priors:suppression_score(catastrophe_memory_preservation__survival_competence_reading, 0.62).
domain_priors:theater_ratio(catastrophe_memory_preservation__survival_competence_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__survival_competence_reading, "Ritual preserves operational threat-recognition capacity across generations").
narrative_ontology:topic_domain(catastrophe_memory_preservation__survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_preservation__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__survival_competence_reading, '77e17703-8299-422a-88b3-5867a5df3184').
narrative_ontology:cs_kernel_codification('77e17703-8299-422a-88b3-5867a5df3184', distributed).
narrative_ontology:cs_authority_grounding('77e17703-8299-422a-88b3-5867a5df3184', practice).
narrative_ontology:cs_interpretation_layer_present('77e17703-8299-422a-88b3-5867a5df3184').
narrative_ontology:cs_reading_relation('77e17703-8299-422a-88b3-5867a5df3184', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('77e17703-8299-422a-88b3-5867a5df3184', catastrophe_memory_preservation__hybrid_atrophy_reading, influences).
narrative_ontology:cs_axiom('77e17703-8299-422a-88b3-5867a5df3184', foundational, operational_memory_transmission_is_survival_insurance).
narrative_ontology:cs_axiom_status(operational_memory_transmission_is_survival_insurance, holdable).
narrative_ontology:cs_axiom_grounding('77e17703-8299-422a-88b3-5867a5df3184', operational_memory_transmission_is_survival_insurance, empirically_contingent).
narrative_ontology:cs_axiom('77e17703-8299-422a-88b3-5867a5df3184', foundational, embodied_drill_is_necessary_for_threat_recognition).
narrative_ontology:cs_axiom_status(embodied_drill_is_necessary_for_threat_recognition, holdable).
narrative_ontology:cs_axiom_grounding('77e17703-8299-422a-88b3-5867a5df3184', embodied_drill_is_necessary_for_threat_recognition, empirically_contingent).
narrative_ontology:cs_reference_frame('77e17703-8299-422a-88b3-5867a5df3184', ancestral_catastrophe_survival_protocol).
narrative_ontology:cs_drift_state('77e17703-8299-422a-88b3-5867a5df3184', contemporary_modernity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('77e17703-8299-422a-88b3-5867a5df3184', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, future_generations).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, community_survival_continuity).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__survival_competence_reading, present_generation_participants).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__survival_competence_reading, ritual_specialists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, present_generation_participants).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__survival_competence_reading, operational_memory_transmission).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__survival_competence_reading, intergenerational_survival_competence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the direct costs of ritual participation: time, resources, emotional labor, and opportunity cost. The ritual demands costly enactment — physical ordeals, resource destruction, scheduled disruption of productive activity. Exit is constrained by community membership requirements, identity fusion with the practice, and the belief that non-participation endangers collective survival. They also receive indirect benefit through community cohesion and the insurance value of preserved threat-recognition.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, present_generation_participants, payer,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__survival_competence_reading, present_generation_participants, beneficiary).

% Inherit and transmit the ritual corpus. Their authority depends on faithful preservation; innovation is suppressed as dangerous. They bear the cognitive load of maintaining operational fidelity across generations — memorization, embodied technique, interpretive discipline. Their identity is fused with the role; exit means dissolving the self-concept constituted through the transmission chain. They also set the agenda for what counts as correct performance.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, ritual_specialists, payer,
    organized, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__survival_competence_reading, ritual_specialists, agenda_setter).

% Govern the ritual calendar, authorize variations, and adjudicate disputes about fidelity. They hold institutional authority over the constraint's enforcement but can exit to other communities or secular roles if the burden exceeds the prestige. Their decisions shape which threat-scenarios are drilled and which are allowed to atrophy.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, community_elders, agenda_setter,
    institutional, generational, arbitrage, local).

% Receive the transmitted threat-recognition capacity without having consented to the costs of its preservation. They are the ultimate beneficiaries of the operational memory — if a catastrophe recurs, the drilled responses may determine survival. They cannot exit the inheritance; they are born into the community that carries it. As non-agents, they collect no rents and exert no power; their benefit is structural and prospective.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, local).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_preservation__survival_competence_reading, future_generations).

% Study the ritual as a cultural transmission system. They see the full structure: the coordination of grief and drill, the extraction from present participants, the insurance logic for future survival. Their analysis does not affect the constraint's operation; they have full exit and no stake in the outcome.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, anthropological_observers, observer,
    analytical, biographical, analytical, global).

% Would regulate or suppress ritual elements that violate modern safety, labor, or child-protection laws. They are excluded from the ritual's internal governance but their external power constrains its enactment. They would object to costly ordeals imposed on minors or non-consenting adults; their absence from the ritual conversation is structural — the ritual predates and resists state incorporation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, state_authorities, excluded,
    institutional, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the intergenerational transmission problem for threat-recognition: how does a community preserve operational knowledge of rare, catastrophic events (flood, famine, invasion, epidemic) across generations when the events themselves are too infrequent for direct experience to maintain competence? The ritual encodes the recognition signals, the response protocols, and the coordination syntax into a repeatable enactment that survives the forgetting curve.
% TRANSFER_FUNCTION: Moves costly participation (time, resources, physical risk, emotional labor) from present-generation participants — especially ritual specialists and ordinary community members — to the insurance value of preserved threat-recognition capacity for future generations. The transfer is not monetary; it is the allocation of scarce present capacity to a prospective survival function.
% ABSENT_VOICES: State authorities (excluded by structural separation of ritual governance from legal governance); future generations (non-agent beneficiaries who cannot consent or object); dissenting community members who privately reject the ritual's premises but participate under identity-locked constraint (their dissent is internalized, not voiced).
% DISAPPEARANCE_RATIONALE: If the ritual vanished overnight, the community would lose its drilled threat-recognition capacity within one generation. The embodied protocols — evacuation routes, resource-sharing syntax, leadership succession, signal recognition — would atrophy to symbolic reference without operational muscle memory. At the next catastrophe, the community would face the event with only improvised response, dramatically increasing mortality. The world rearranges because the constraint's function is the difference between drilled and improvised survival.
% FOUNDING_PROBLEM: A community that experienced a near-extinction catastrophe (volcanic winter, pandemic, invasion) needed to ensure that the hard-won survival knowledge — what the threat looked like, what responses worked, how to coordinate under collapse — would not be lost before the next occurrence, which might be generations later.
% FOUNDING_PROBLEM_CORROBORATION: The ritual specialists and community elders attest the founding problem is live: the catastrophe could recur and the drilled capacity remains the best insurance. Anthropological observers and historical ecologists corroborate that the ritual's operational content matches paleo-environmental records of actual past catastrophes (e.g., tsunami evacuation protocols encoded in coastal dance, famine food-processing encoded in harvest rites). However, state authorities and modernization advocates attest the founding problem is dead: early warning systems, disaster relief infrastructure, and scientific modeling have superseded the ritual's operational function. No single external authority resolves the dispute.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__survival_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__survival_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(catastrophe_memory_preservation__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__survival_competence_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__survival_competence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_preservation__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the ritual demands costly, non-optional participation whose marginal benefit to the present participant is near zero — the insurance pays off only in a catastrophe that may not recur in their lifetime. Suppression is moderate (0.62) because enforcement is primarily internalized through identity fusion and community membership, not external coercion; exit exists but is constrained by the belief that non-participation endangers the collective. Theater ratio is low-moderate (0.28) and rising: the operational core (evacuation drills, resource-sharing protocols) remains functional, but elaborations (ornamental choreography, symbolic expansions) have accumulated. Accessibility collapse is moderate (0.45): alternative transmission methods (written manuals, state disaster training) exist but lack the embodied coordination syntax the ritual provides. Resistance is moderate (0.55): periodic reform movements attempt to reduce costs or modernize enactment, but are suppressed by specialists as fidelity violations.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (participants, specialists) experience the constraint as enforced extraction with a speculative payoff; the beneficiary seat (future generations) experiences it as gifted survival capacity. The agenda-setter seat (elders, specialists) experiences it as sacred trust — the coordination function is existential, not optional. The engine computes this divergence from the structural data: identity_locked exit for specialists amplifies their effective extraction; trapped status for future generations inverts their directionality toward subsidy; constrained exit for participants keeps them in the target zone.
 *
 * DIRECTIONALITY LOGIC:
 *   Present-generation participants and ritual specialists are the primary targets (high directionality): they bear the extraction through costly participation and identity-locked transmission labor. Future generations are the structural beneficiaries (low directionality): they receive the insurance value without paying the premium. Community elders sit near symmetric: they administer the constraint and gain prestige, but also bear responsibility for its fidelity. State authorities are excluded: their regulatory power is external to the ritual's internal logic. Anthropological observers are analytical: they see the structure without stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows early mandatrophy signals: rising theater_ratio and extractiveness over five centuries suggest the coordination function is being layered with symbolic elaboration that serves no operational purpose. However, the core threat-recognition protocols remain empirically validated against paleo-records — the mandate is not yet dead. The contested founding_problem_status reflects this tension: the original problem (preserving operational knowledge across rare catastrophes) is structurally live, but its necessity is contested by modern alternatives. The constraint is not yet a piton because the operational core still passes the disappearance test (world_rearranges); it would become a piton if the operational content fully atrophied to symbolic reference while the costly enactment persisted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_fidelity_threshold,
    'At what point does ritual elaboration degrade operational fidelity below the threshold for effective threat-recognition?',
    'Controlled comparison of communities with high vs. low elaboration facing simulated or actual catastrophe scenarios; measurement of response latency, coordination accuracy, and mortality differentials.',
    'If high elaboration correlates with operational failure, the rising theater_ratio signals functional decay — the constraint slides toward piton. If elaboration is orthogonal to fidelity, the coordination function persists despite symbolic accretion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(operational_fidelity_threshold, empirical, 'Whether symbolic elaboration corrupts the operational core the ritual exists to preserve.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (community sanctions, resource denial) or internalized (identity fusion, belief that non-participation causes catastrophe)?',
    'Post-exit suppression trajectory: track individuals who leave the community — if suppression persists (guilt, anxiety, perceived causal responsibility for misfortune), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the constraint more snare-like than the metrics indicate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in an identity-locked ritual context.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the three readings of the catastrophe_memory_preservation kernel disagree structurally?',
    'Decompose each reading''s claimed_type, beneficiary/victim structure, and extractiveness referent. The disagreement is located in: (1) whether the ritual''s coordination function is operational (survival_competence), symbolic (mourning_practice), or degraded (hybrid_atrophy); (2) whether present-generation autonomy is a victim (survival_competence, hybrid_atrophy) or not (mourning_practice); (3) whether future generations are beneficiaries (survival_competence) or the concept is incoherent (mourning_practice).',
    'Resolution would determine which reading''s ε-invariant constraint story accurately models the ritual''s actual structure. The three readings cannot all be true of the same constraint — they instantiate different constraints from the same kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Structural locus of disagreement among sibling readings of the catastrophe_memory_preservation kernel.').

omega_variable(
    modern_alternatives_substitution,
    'Do modern early-warning systems and disaster infrastructure genuinely substitute for the ritual''s operational function, or do they solve a different problem (population-scale vs. community-scale coordination)?',
    'Comparative analysis of community-scale response efficacy in catastrophes with and without ritual preservation, controlling for state infrastructure presence.',
    'If modern systems substitute at the community scale, the founding problem is dead and the constraint is a piton. If they operate at a different scale (national vs. local), the ritual''s coordination function remains live and non-redundant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modern_alternatives_substitution, empirical, 'Whether state disaster infrastructure renders the ritual''s operational function redundant.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__survival_competence_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmp_scr_tr_t0, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(cmp_scr_tr_t100, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 100, 0.12).
narrative_ontology:measurement(cmp_scr_tr_t200, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 200, 0.18).
narrative_ontology:measurement(cmp_scr_tr_t300, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 300, 0.22).
narrative_ontology:measurement(cmp_scr_tr_t400, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 400, 0.25).
narrative_ontology:measurement(cmp_scr_tr_t500, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 500, 0.28).

% Extraction over time
narrative_ontology:measurement(cmp_scr_be_t0, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cmp_scr_be_t100, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 100, 0.42).
narrative_ontology:measurement(cmp_scr_be_t200, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 200, 0.55).
narrative_ontology:measurement(cmp_scr_be_t300, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 300, 0.68).
narrative_ontology:measurement(cmp_scr_be_t400, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 400, 0.73).
narrative_ontology:measurement(cmp_scr_be_t500, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 500, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(cmp_scr_su_t0, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(cmp_scr_su_t100, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 100, 0.48).
narrative_ontology:measurement(cmp_scr_su_t200, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 200, 0.52).
narrative_ontology:measurement(cmp_scr_su_t300, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 300, 0.58).
narrative_ontology:measurement(cmp_scr_su_t400, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 400, 0.6).
narrative_ontology:measurement(cmp_scr_su_t500, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 500, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__survival_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_preservation__survival_competence_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation__hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the catastrophe_memory_preservation kernel into three readings: survival_competence_reading (this story, tangled_rope), mourning_practice_reading (symbolic continuity, likely rope or mountain), hybrid_atrophy_reading (degraded function, likely piton). The survival_competence reading influences both siblings by establishing the operational baseline they must account for — mourning_practice must explain why operational content persists if function is purely symbolic; hybrid_atrophy must explain the trajectory from operational to symbolic. The readings coexist in the discourse but influence each other's legitimacy conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_preservation__survival_competence_reading, organized, 0.75).
constraint_indexing:directionality_override(catastrophe_memory_preservation__survival_competence_reading, institutional, 0.3).
constraint_indexing:directionality_override(catastrophe_memory_preservation__survival_competence_reading, powerless, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
