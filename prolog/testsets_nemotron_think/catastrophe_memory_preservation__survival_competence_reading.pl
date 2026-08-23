% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: catastrophe_memory_preservation__survival_competence_reading
 *   human_readable: Ritual as Transgenerational Threat-Recognition Drill
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   A community maintains a demanding ritual that simulates catastrophic
 *   threat scenarios (e.g., earthquake drills encoded as ceremonial dance,
 *   epidemic response encoded as purification rites). The ritual authorities
 *   claim it preserves operational threat-recognition capacity — a genuine
 *   coordination function. However, participation is costly and enforced,
 *   extracting labor and psychological burden from current participants for
 *   the benefit of future generations who cannot consent. The constraint is a
 *   tangled rope: it coordinates survival competence while extracting from
 *   the present generation. The sibling readings (mourning_practice_reading,
 *   hybrid_atrophy_reading) offer alternative framings: that the ritual is
 *   purely symbolic identity work, or that it once had operational value but
 *   has atrophied.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__survival_competence_reading, 0.72).
domain_priors:suppression_score(catastrophe_memory_preservation__survival_competence_reading, 0.68).
domain_priors:theater_ratio(catastrophe_memory_preservation__survival_competence_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__survival_competence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__survival_competence_reading, "Ritual as Transgenerational Threat-Recognition Drill").
narrative_ontology:topic_domain(catastrophe_memory_preservation__survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_preservation__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__survival_competence_reading, '132de3e1-5195-4df7-9c54-53e1a20583ec').
narrative_ontology:cs_kernel_codification('132de3e1-5195-4df7-9c54-53e1a20583ec', distributed).
narrative_ontology:cs_authority_grounding('132de3e1-5195-4df7-9c54-53e1a20583ec', practice).
narrative_ontology:cs_interpretation_layer_present('132de3e1-5195-4df7-9c54-53e1a20583ec').
narrative_ontology:cs_reading_relation('132de3e1-5195-4df7-9c54-53e1a20583ec', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('132de3e1-5195-4df7-9c54-53e1a20583ec', catastrophe_memory_preservation__hybrid_atrophy_reading, forecloses).
narrative_ontology:cs_axiom('132de3e1-5195-4df7-9c54-53e1a20583ec', foundational, ritual_preserves_operational_threat_recognition).
narrative_ontology:cs_axiom_status(ritual_preserves_operational_threat_recognition, holdable).
narrative_ontology:cs_axiom_grounding('132de3e1-5195-4df7-9c54-53e1a20583ec', ritual_preserves_operational_threat_recognition, empirically_contingent).
narrative_ontology:cs_axiom('132de3e1-5195-4df7-9c54-53e1a20583ec', secondary, costly_participation_is_necessary_for_fidelity).
narrative_ontology:cs_axiom_status(costly_participation_is_necessary_for_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('132de3e1-5195-4df7-9c54-53e1a20583ec', costly_participation_is_necessary_for_fidelity, instrumental).
narrative_ontology:cs_reference_frame('132de3e1-5195-4df7-9c54-53e1a20583ec', transgenerational_threat_recognition_drill).
narrative_ontology:cs_drift_state('132de3e1-5195-4df7-9c54-53e1a20583ec', contemporary_modernity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('132de3e1-5195-4df7-9c54-53e1a20583ec', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__survival_competence_reading, descendant_community).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__survival_competence_reading, current_participants).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__survival_competence_reading, group_survival_competence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% They define and enforce the ritual's form, frequency, and participation requirements. They justify the ritual as essential for preserving the community's ability to recognize and respond to existential threats. Their authority derives from lineage and the ritual's claimed efficacy.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, ritual_authorities, agenda_setter,
    institutional, generational, arbitrage, regional).

% They bear the costs of ritual participation: time, physical exertion, psychological intensity, and opportunity cost. Exit is possible but entails social ostracism and loss of community membership. They are the ones who enact the drill.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, current_participants, payer,
    moderate, biographical, constrained, regional).

% The future community that inherits the threat-recognition capacity preserved by the ritual. They do not yet exist and cannot consent to or reject the arrangement. Their survival competence is the claimed benefit.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, descendant_community, beneficiary,
    moderate, generational, analytical, regional).

% External scholars who study the ritual as a cultural adaptation. They analyze whether the ritual actually preserves operational threat-recognition or has become symbolic. They have no stake in the ritual's enforcement.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__survival_competence_reading, anthropologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_preservation__survival_competence_reading, ritual_authorities).
narrative_ontology:fixing_cost_class(catastrophe_memory_preservation__survival_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The ritual solves the coordination problem of maintaining a community's ability to recognize and respond to rare but catastrophic threats across generations, by embedding threat-recognition drills in a mandatory, high-fidelity practice that survives cultural drift.
% TRANSFER_FUNCTION: Moves costly participation (time, effort, risk) from current participants to the descendant community in the form of preserved threat-recognition capacity. The transfer is intergenerational and non-reciprocal.
% ABSENT_VOICES: The descendant community (who cannot yet speak) and potential dissenters among current participants who would prefer a less costly form of threat-recognition training. Dissenters are often silenced by the ritual's framing as sacred duty.
% DISAPPEARANCE_RATIONALE: Without the ritual's drill, the community's threat-recognition would degrade over generations, leaving it vulnerable to catastrophic events that the ritual was designed to detect. The loss would be irreversible on human timescales.
% FOUNDING_PROBLEM: The community faced recurrent existential threats (e.g., natural disasters, invasions, epidemics) that required rapid, coordinated recognition and response. Individual memory was insufficient; a transgenerational mechanism was needed.
% FOUNDING_PROBLEM_CORROBORATION: Oral histories and archaeological evidence corroborate the founding threat environment. However, the claim that the ritual's current form still addresses live threats is contested by anthropologists and some community members; no external corroboration exists for the current operational efficacy.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__survival_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__survival_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__survival_competence_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is high (0.72) because the ritual demands substantial costly participation that is decoupled from immediate individual benefit. Suppression is moderate-high (0.68) because non-participation incurs severe social sanctions and the ritual's form is actively policed. Theater ratio (0.45) reflects that a significant portion of the ritual's elaboration serves performative identity functions beyond the minimal drill. Accessibility collapse (0.62) indicates that once the ritual is understood as the sole vehicle for threat-recognition, alternative training methods are dismissed. Resistance (0.55) shows ongoing friction: some participants comply grudgingly, and anthropologists document the gap between claimed and measured efficacy.
 *
 * PERSPECTIVAL GAP:
 *   From the ritual_authorities seat, the constraint is a rope (genuine coordination with shared burden). From the current_participants seat, it is a snare (extraction with suppressed exit). From the descendant_community seat (if they could speak), it would be a rope (they receive the benefit). The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Ritual authorities are structural beneficiaries (d near 0.0): they gain authority and institutional persistence from the ritual. Current participants are targets (d near 1.0): they pay the costs with constrained exit. Descendant community is the ultimate beneficiary but lacks agency (d derived as beneficiary via structural position). Anthropologists are analytical observers (d=0.5). The extraction flows from current_participants to ritual_authorities (who control the ritual) and notionally to descendant_community (who receive the competence).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (recurrent existential threats) is contested as live. If the threat environment has changed, the ritual's mandate has atrophied, pushing it toward piton or snare. The hybrid_atrophy_reading captures this possibility. The survival_competence_reading insists the mandate is live, making it a tangled rope. The omega variables document the irreducible uncertainty about whether the ritual still performs its claimed function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure,
    'This constraint is one reading of the catastrophe_memory_preservation kernel. What are the structural consequences of the sibling readings (mourning_practice_reading, hybrid_atrophy_reading) being live alternatives?',
    'Comparative analysis of the three readings'' ε values, beneficiary/victim structures, and classification outcomes. If the sibling readings produce substantially different classifications, the kernel is a site of genuine structural contestation.',
    'If the mourning_practice_reading classifies as rope (low extraction) and this reading classifies as tangled_rope (high extraction), the kernel''s classification is reading-dependent — a diagnostic signal for the framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure, conceptual, 'Committer-frame structural delta between sibling readings of the same kernel.').

omega_variable(
    operational_efficacy_uncertainty,
    'Does the ritual actually preserve operational threat-recognition capacity, or has it become purely symbolic?',
    'Controlled studies comparing threat-recognition performance in communities with and without the ritual; historical analysis of survival outcomes during actual catastrophes.',
    'If efficacy is confirmed, the coordination function is real and the tangled_rope classification holds. If efficacy is disconfirmed, the coordination story is cover and the constraint reclassifies toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(operational_efficacy_uncertainty, empirical, 'Whether the ritual''s claimed coordination function is empirically grounded.').

omega_variable(
    cost_necessity,
    'Is the ritual''s costly participation structurally necessary for high-fidelity transmission, or could less costly training achieve the same threat-recognition preservation?',
    'Experimental comparison of high-cost ritual vs. low-cost didactic training for threat-recognition retention across generations.',
    'If costly participation is unnecessary, the extraction is gratuitous and the constraint leans toward snare. If necessary, the extraction is the price of coordination and tangled_rope is warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_necessity, empirical, 'Whether the ritual''s extractiveness is functionally necessary or gratuitous.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__survival_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmp_scr_tr_t0, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cmp_scr_tr_t20, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(cmp_scr_tr_t40, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(cmp_scr_tr_t60, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(cmp_scr_tr_t80, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 80, 0.43).
narrative_ontology:measurement(cmp_scr_tr_t100, catastrophe_memory_preservation__survival_competence_reading, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(cmp_scr_be_t0, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cmp_scr_be_t20, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(cmp_scr_be_t40, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(cmp_scr_be_t60, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 60, 0.64).
narrative_ontology:measurement(cmp_scr_be_t80, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 80, 0.69).
narrative_ontology:measurement(cmp_scr_be_t100, catastrophe_memory_preservation__survival_competence_reading, base_extractiveness, 100, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(cmp_scr_su_t0, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(cmp_scr_su_t20, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(cmp_scr_su_t40, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(cmp_scr_su_t60, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(cmp_scr_su_t80, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 80, 0.65).
narrative_ontology:measurement(cmp_scr_su_t100, catastrophe_memory_preservation__survival_competence_reading, suppression_requirement, 100, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__survival_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_preservation__survival_competence_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__survival_competence_reading, catastrophe_memory_preservation__hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% This constraint and its two siblings form a constraint family decomposing the kernel 'catastrophe_memory_preservation'. The survival_competence_reading claims high extractiveness with genuine coordination; mourning_practice_reading claims low extractiveness (rope); hybrid_atrophy_reading claims temporal shift from tangled_rope to piton. They are linked via affects_constraints to enable contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_preservation__survival_competence_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
