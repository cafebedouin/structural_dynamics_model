% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__trauma_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__trauma_encoding_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__trauma_encoding_reading
 *   human_readable: Ritual as Trauma-Encoding Warning System
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint story instantiates the trauma_encoding_reading of the
 *   catastrophe_memory_kernel. Rituals that reenact historical catastrophe
 *   (e.g., Tisha B'Av, Armenian Genocide commemorations, Indigenous mourning
 *   ceremonies, Sikh Shaheedi remembrance) function as embodied warning
 *   systems: they transmit threat-recognition patterns somatically and
 *   narratively to descendants who never experienced the original event. The
 *   arrangement solves a real coordination problem — maintaining collective
 *   threat vigilance across generational gaps — but does so by imposing
 *   psychological costs on descendants who did not consent to the
 *   inheritance. The constraint is a tangled rope: genuine coordination
 *   function (early warning) fused with asymmetric extraction (descendants
 *   bear the trauma costs). Active enforcement is required — communities
 *   sanction non-participation, police ritual fidelity, and treat questioning
 *   the trauma-encoding as betrayal.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, 0.68).
domain_priors:suppression_score(catastrophe_memory_kernel__trauma_encoding_reading, 0.45).
domain_priors:theater_ratio(catastrophe_memory_kernel__trauma_encoding_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__trauma_encoding_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__trauma_encoding_reading, "Ritual as Trauma-Encoding Warning System").
narrative_ontology:topic_domain(catastrophe_memory_kernel__trauma_encoding_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__trauma_encoding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__trauma_encoding_reading, 'e613bfa0-e171-4502-a130-a1f23c7cc764').
narrative_ontology:cs_kernel_codification('e613bfa0-e171-4502-a130-a1f23c7cc764', distributed).
narrative_ontology:cs_authority_grounding('e613bfa0-e171-4502-a130-a1f23c7cc764', practice).
narrative_ontology:cs_interpretation_layer_present('e613bfa0-e171-4502-a130-a1f23c7cc764').
narrative_ontology:cs_reading_relation('e613bfa0-e171-4502-a130-a1f23c7cc764', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_reading_relation('e613bfa0-e171-4502-a130-a1f23c7cc764', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('e613bfa0-e171-4502-a130-a1f23c7cc764', catastrophe_memory_kernel__symbol_continuity_reading, influences).
narrative_ontology:cs_axiom('e613bfa0-e171-4502-a130-a1f23c7cc764', foundational, trauma_encoding_is_necessary_for_survival).
narrative_ontology:cs_axiom_status(trauma_encoding_is_necessary_for_survival, holdable).
narrative_ontology:cs_axiom_grounding('e613bfa0-e171-4502-a130-a1f23c7cc764', trauma_encoding_is_necessary_for_survival, instrumental).
narrative_ontology:cs_axiom('e613bfa0-e171-4502-a130-a1f23c7cc764', foundational, embodied_memory_superior_to_narrative_memory_for_threat_detection).
narrative_ontology:cs_axiom_status(embodied_memory_superior_to_narrative_memory_for_threat_detection, holdable).
narrative_ontology:cs_axiom_grounding('e613bfa0-e171-4502-a130-a1f23c7cc764', embodied_memory_superior_to_narrative_memory_for_threat_detection, empirically_contingent).
narrative_ontology:cs_reference_frame('e613bfa0-e171-4502-a130-a1f23c7cc764', catastrophe_as_perpetual_threat).
narrative_ontology:cs_drift_state('e613bfa0-e171-4502-a130-a1f23c7cc764', post_immediate_threat_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e613bfa0-e171-4502-a130-a1f23c7cc764', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, collective_threat_vigilance).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, community_elders).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, descendant_generations).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, ritual_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, descendant_generations).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__trauma_encoding_reading, intergenerational_trauma_as_early_warning).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__trauma_encoding_reading, ritual_as_adaptive_memory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Transmit and curate ritual forms that encode historical catastrophe; hold authority over which traumas are ritualized and how intensely. Benefit from status as keepers of collective memory and from the community's continued threat vigilance. Can step down or modify transmission practices but face community expectation to preserve the forms.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, community_elders, agenda_setter,
    organized, generational, mobile, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__trauma_encoding_reading, community_elders, beneficiary).

% Inherit ritual practices that embed ancestral trauma in liturgy, body, and narrative. Pay psychological costs: hypervigilance, inherited grief, constrained identity formation around catastrophe. Benefit from early-warning dispositions and communal cohesion. Exit requires identity rupture — leaving the ritual community means leaving the self constituted through it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, descendant_generations, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__trauma_encoding_reading, descendant_generations, beneficiary).

% Enact rituals that viscerally re-experience historical suffering (fasting, lamentation, reenactment). Pay immediate somatic and emotional costs per cycle. Constrained exit — can reduce participation but face social sanction and loss of communal belonging. No alternative community offers equivalent threat-vigilance framing.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, ritual_participants, payer,
    moderate, biographical, constrained, local).

% The emergent early-warning capacity of the community: heightened threat perception, faster mobilization, preserved escape scripts. Not an agent but the structural beneficiary of the trauma-encoding mechanism — the function the constraint serves.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, collective_threat_vigilance, beneficiary,
    analytical, generational, analytical, local).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_kernel__trauma_encoding_reading, collective_threat_vigilance).

% Would object to the pathologization of ritual trauma-encoding and offer alternative healing frameworks. Excluded from ritual authority structures; their epistemic framework (individual healing) conflicts with the collective warning-system logic. Have no standing in the community's interpretive tradition.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, trauma_therapists, excluded,
    organized, biographical, mobile, regional).

% Study the ritual as a cultural adaptation. Document the transmission mechanics, the cost-benefit trade-offs, and the community's own self-understanding. Analytical seat — neither collects nor pays.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, anthropologists_of_memory, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__trauma_encoding_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__trauma_encoding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ritual coordinates collective threat detection and response readiness across generations by embedding catastrophe memory in embodied practice, ensuring the community 'remembers with its body' what it cannot afford to forget.
% TRANSFER_FUNCTION: Moves psychological burden (hypervigilance, somatic memory, identity constraint) from the collective's need for threat detection onto descendant bodies and minds, in exchange for early-warning capacity and communal cohesion.
% ABSENT_VOICES: Trauma therapists and mental health practitioners who would frame the ritual as pathological rather than adaptive; assimilated descendants who have left the community and experience the rituals as alien impositions; secular historians who read the trauma-encoding as contingent cultural construction rather than necessary warning.
% DISAPPEARANCE_RATIONALE: If the trauma-encoding rituals vanished overnight, the community would lose its primary mechanism for maintaining threat vigilance across generations — early-warning dispositions would decay within 1-2 generations, escape scripts would be forgotten, and the collective would become vulnerable to recurrence of the catastrophe type the rituals encode. The psychological burden on descendants would lift, but so would the adaptive function.
% FOUNDING_PROBLEM: After a catastrophic near-extinction event (pogrom, genocide, forced displacement, ecological collapse), the community faced the problem of how to ensure descendants would recognize and survive the same threat type without direct experience of it.
% FOUNDING_PROBLEM_CORROBORATION: Community elders and tradition-bearers attest the founding problem remains live (threat type persists in new forms). Anthropologists of collective memory (Connerton, Assmann, Hinton) corroborate the adaptive-function reading from outside the benefiting parties. Trauma therapists and some descendant voices attest the problem is dead (original threat gone) and the arrangement persists as maladaptive burden — a cover story for unresolved grief.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__trauma_encoding_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__trauma_encoding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__trauma_encoding_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(catastrophe_memory_kernel__trauma_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__trauma_encoding_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.68: the psychological burden on descendants is substantial and recurring (per ritual cycle), while the coordination benefit (threat vigilance) is diffuse and probabilistic. Suppression 0.45: moderate — enforcement operates through social sanction and identity pressure rather than physical coercion, but identity-locked exit makes the suppression effective. Theater 0.25: some performative inflation of trauma intensity occurs over time (ritual elaboration beyond what threat-detection requires), but the core transmission mechanism remains functional. Accessibility collapse 0.55: alternatives exist (written histories, oral testimony without somatic reenactment) but are treated as insufficient by the community's own epistemic standards. Resistance 0.4: moderate — some descendants resist through assimilation, secularization, or therapeutic reframing, but open resistance within the community is rare and costly.
 *
 * PERSPECTIVAL GAP:
 *   From the elder/tradition-bearer seat, the constraint appears as a rope — a necessary coordination mechanism they maintain for collective survival. From the descendant seat, it appears as a snare — an inherited burden they cannot refuse without losing their identity and community. The engine computes this divergence from the structural data: the same ritual cycle is experienced as gift by those who curate it and as tax by those who enact it.
 *
 * DIRECTIONALITY LOGIC:
 *   Community elders (agenda_setter/beneficiary) occupy a beneficiary-adjacent position: they control the transmission and gain status/authority from it (d ~ 0.2). Descendant generations (payer/beneficiary) are the primary targets — they bear the recurring psychological costs with identity-locked exit (d ~ 0.8). Ritual participants (payer) similarly bear somatic costs per cycle with constrained exit (d ~ 0.75). Collective threat vigilance (non-agent beneficiary) receives the coordination benefit (d ~ 0.1). Trauma therapists (excluded) would experience the constraint as pure extraction if they were inside it, but their exclusion means they bear no direct cost (d ~ 0.5 analytically). Anthropologists (observer) sit at symmetric analytical distance (d ~ 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (catastrophe survival) is contested as live vs. dead. If live, the mandate persists and the constraint is a tangled rope with genuine coordination function. If dead, the mandate has atrophied and the constraint drifts toward piton (theatrical maintenance of a warning system for a threat that no longer exists in the same form) or snare (pure extraction of descendant psychological resources for community cohesion). The contested status is itself the mandatrophy signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trauma_vs_warning_boundary,
    'Where is the boundary between adaptive threat-vigilance and maladaptive trauma transmission in the ritual''s operation?',
    'Longitudinal study comparing threat-response outcomes and psychological morbidity in descendant cohorts with high vs. low ritual participation, controlling for community cohesion.',
    'If the boundary is sharp and the ritual sits on the adaptive side, the tangled rope classification holds. If the ritual''s trauma load exceeds the vigilance benefit, the constraint reclassifies toward snare. If vigilance benefit is negligible, it reclassifies toward piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(trauma_vs_warning_boundary, empirical, 'Whether the ritual''s psychological costs are proportionate to its warning function.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the catastrophe_memory_kernel a single persisting commitment with four readings, or are these four distinct constraints that share only a historical referent?',
    'Test whether the four readings'' ε values and beneficiary/victim structures are mutually irreducible — if changing the observable (threat-vigilance vs. boundary-maintenance vs. skill-transmission vs. symbol-continuity) changes ε, the ε-invariance principle demands separate constraints.',
    'If the kernel is a single commitment, the four stories form a constraint family linked by network.affects_constraints with reading_relations in cs_structure. If they are distinct constraints, the kernel_id is a linguistic convenience and each story stands alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the catastrophe memory kernel genuinely unifies the four readings or merely labels them.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.45) primarily structural (social sanction, identity policing) or internalized (descendants believe the trauma-encoding is necessary for their own survival)?',
    'Post-exit suppression trajectory: track descendants who leave the community — if hypervigilance and ritual compulsion persist after exit, the suppression is partially internalized; if they decay, it was primarily structural.',
    'If internalized, effective suppression is higher than the structural measure suggests — the target carries the suppression with them. This would increase effective extraction for identity-locked agents and strengthen the snare tendency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the trauma-encoding ritual.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__trauma_encoding_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 80, 0.24).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 100, 0.25).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 60, 0.63).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 80, 0.66).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 60, 0.42).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 80, 0.44).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 100, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__trauma_encoding_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__trauma_encoding_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__boundary_maintenance_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__symbol_continuity_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_kernel decomposes into four constraint stories, each claiming a different coordination function for the same ritual complex. This reading (trauma_encoding) centers threat-vigilance as the beneficiary and descendant psychological burden as the victim. The sibling readings center boundary-maintenance, survival-competence, and symbol-continuity respectively. All four share the same historical referent (the catastrophe event) but author different ε values and different beneficiary/victim structures — satisfying ε-invariance by decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
