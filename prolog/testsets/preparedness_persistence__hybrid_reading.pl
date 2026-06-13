% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__hybrid_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: preparedness_persistence__hybrid_reading
 *   human_readable: Stratified Disaster Preparedness: Competence and Ritual Mixed
 *   domain: institutional/governance/commitment-systems
 *
 * SUMMARY:
 *   Disaster preparedness as a unified institutional mandate masks
 *   stratification: engineering competence is maintained through continuous,
 *   evidence-based inspection and professional liability; evacuation drills
 *   have atrophied into administrative performance disconnected from actual
 *   population movement, infrastructure state, and real-time coordination
 *   capacity. This reading asserts that competence and ritual coexist in the
 *   same constraint system, not that one reading abolishes the other. The
 *   hybrid structure allows distributed institutions to certify preparedness
 *   without achieving uniform operational readiness — the constraint persists
 *   because no single party bears enough cost to fix the mismatch, and the
 *   administrative apparatus benefits from the appearance of stratified
 *   coverage. Suppression keeps the mismatch invisible: questioning the
 *   competence of drills is reframed as undermining preparedness itself.
 *   Theater rises over time as drill routines become increasingly decoupled
 *   from real conditions (climate change, infrastructure aging, demographic
 *   shifts) while engineering competence is sustained. The constraint is
 *   CLAIMED as Piton (institutional inertia) because the beneficiary
 *   (administrative continuity apparatus) administers it but does not capture
 *   enough value to maintain active evolution — instead, passive maintenance
 *   and ritual substitution sustain it.
 *
 * KEY AGENTS:
 *   - Engineering inspection authority: maintains structural standards through professionalized, continuous, liability-bearing work — the competent component.
 *   - Evacuation drill coordinators: administer periodic drills with declining real engagement; they collect compliance certification but not measurable competence gains — the ritualized component.
 *   - Administrative continuity apparatus: benefits from the distributed liability picture; loses if either component is questioned.
 *   - Evacuating populations: bear the actual stakes if competence and ritual diverge in real events; absent from preparedness governance.
 *   - First responders in event: must operate based on training from disconnected drills; trapped between regulatory expectations and real conditions.
 *   - Engineering knowledge holders (external observers): attest to competence of structural work and weakness of drill-based readiness.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__hybrid_reading, 0.62).
domain_priors:suppression_score(preparedness_persistence__hybrid_reading, 0.58).
domain_priors:theater_ratio(preparedness_persistence__hybrid_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__hybrid_reading, piton).
narrative_ontology:human_readable(preparedness_persistence__hybrid_reading, "Stratified Disaster Preparedness: Competence and Ritual Mixed").
narrative_ontology:topic_domain(preparedness_persistence__hybrid_reading, "institutional/governance/commitment-systems").

domain_priors:requires_active_enforcement(preparedness_persistence__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__hybrid_reading, '2442a307-56ef-400f-941a-8862c539a7d4').
narrative_ontology:cs_kernel_codification('2442a307-56ef-400f-941a-8862c539a7d4', distributed).
narrative_ontology:cs_authority_grounding('2442a307-56ef-400f-941a-8862c539a7d4', practice).
narrative_ontology:cs_interpretation_layer_present('2442a307-56ef-400f-941a-8862c539a7d4').
narrative_ontology:cs_reading_relation('2442a307-56ef-400f-941a-8862c539a7d4', preparedness_persistence__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('2442a307-56ef-400f-941a-8862c539a7d4', preparedness_persistence__husk_reading, coexists_with).
narrative_ontology:cs_axiom('2442a307-56ef-400f-941a-8862c539a7d4', foundational, preparedness_is_stratified).
narrative_ontology:cs_axiom_status(preparedness_is_stratified, holdable).
narrative_ontology:cs_axiom_grounding('2442a307-56ef-400f-941a-8862c539a7d4', preparedness_is_stratified, empirically_contingent).
narrative_ontology:cs_axiom('2442a307-56ef-400f-941a-8862c539a7d4', foundational, administrative_continuity_benefits_from_asymmetry).
narrative_ontology:cs_axiom_status(administrative_continuity_benefits_from_asymmetry, holdable).
narrative_ontology:cs_axiom_grounding('2442a307-56ef-400f-941a-8862c539a7d4', administrative_continuity_benefits_from_asymmetry, instrumental).
narrative_ontology:cs_reference_frame('2442a307-56ef-400f-941a-8862c539a7d4', unified_preparedness_mandate).
narrative_ontology:cs_drift_state('2442a307-56ef-400f-941a-8862c539a7d4', contemporary_post_event_investigation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2442a307-56ef-400f-941a-8862c539a7d4', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__hybrid_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, administrative_continuity_apparatus).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, evacuating_populations).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, first_responders_in_degraded_conditions).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__hybrid_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(preparedness_persistence__hybrid_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_persistence__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_persistence__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62 end-state) is moderate: the administrative apparatus extracts institutional legitimacy and liability distribution, not direct wealth. It rises from 0.45 to 0.62 over the interval as the divergence between competence and ritual becomes more marked — the constraint's function shifts from genuine coordination (solving preparedness) toward extraction (distributing accountability for uneven preparedness). Theater (0.61 end-state) also rises, from 0.35 to 0.61, reflecting the increasing ratio of performative drills to competence-building activity. Suppression (0.58 end-state) is required to keep the mismatch from becoming visible — questioning whether evacuations would actually work challenges the entire distributed liability picture. Resistance is moderate (0.54) because some professional voices (engineering, disaster research) do contest the ritual component, but they are systematically excluded from governance design and their objections are reframed as specialized technical critique rather than preparedness-system critique. Accessibility collapse (0.68) reflects the populations' trapped status — evacuation is mandatory, preparation is mandatory, alternatives are foreclosed by geography and regulation. The coercion grid shows stakes inflation rising steeply at individual and class levels (populations facing real evacuation) while structural stakes (institutional legitimacy) remain flatter, and resistance declining over time as populations internalize the mandatory compliance framing and first responders resign to training on potentially unreliable drills.
 *
 * PERSPECTIVAL GAP:
 *   From the engineering inspector's seat: genuine coordination problem (infrastructure safety), genuine competence maintenance, low extraction. From the drill coordinator's seat: compliance obligation, diminishing real engagement, trapped between administrative expectations and population disengagement. From the administrative apparatus's seat: successful liability distribution, mixed preparedness narrative as asset. From the evacuating population's seat: trapped and underinformed — stake is life, competence is opaque, drills do not correlate with safety. The engine should compute these as sharply divergent types: the engineering component approaches Mountain (fixed by physics and professional standards), the drill component approaches Snare (extraction of administrative legitimacy from populations with no exit), and the constraint as a whole sits as Piton (the apparatus administers both but does not profit enough to maintain genuine evolution — instead passive ritual). The hybrid reading INSTANTIATES this asymmetry, not by asserting it in advance but by declaring the structural boundaries where competence and ritual diverge.
 *
 * DIRECTIONALITY LOGIC:
 *   Administrative continuity apparatus (beneficiary, institutional power, mobile exit) derives d near the beneficiary end (~0.20–0.30): they profit from the arrangement without bearing its costs, and they can exit to reformed governance if the constraint becomes untenable. Evacuating populations (payer, powerless, trapped exit) derive d at the target end (~0.80–0.90): they bear the stakes, cannot exit, and have no input into the system. First responders (payer, moderate power, constrained exit) sit intermediate (~0.60–0.70): they are trained on drills they may doubt, cannot refuse to deploy, but have some professional autonomy to improvise in real events. Engineering inspectors (observer, institutional power, analytical exit) are not targets — they maintain genuine competence as part of a different institutional apparatus (professional licensing, evidence standards). The directive beneficiaries declare above map to specific seats through these derivations.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading avoids false certification by naming the parts: engineering component is live (competence is maintained). Evacuation drill component is dead (the founding problem — inadequate evacuation readiness — is not solved by periodic drills, as post-event investigations repeatedly show). The constraint persists as Piton because administrative continuity benefits from certifying both as equivalent preparedness, and no single actor bears enough cost to fix the asymmetry. Engineering standards are embedded in professional liability and continuous inspection. Drills are embedded in legal mandate with diffuse accountability. Dropping drills would expose lack of evacuation readiness; accelerating engineering standards would threaten institutional actors who depend on the distributed-liability framing. The piton is held by inertia — the apparatus could change it, but the cost of admitting evacuation drills are insufficient preparedness exceeds what the apparatus wants to bear.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_drill_correlation,
    'Do evacuation drills meaningfully correlate with actual evacuation success in real disasters?',
    'Comparative analysis of post-event evacuation performance (success rates, coordination, mortality) against pre-event drill participation and quality. Natural experiments from jurisdictions with different drill regimes. Controlled simulation of realistic evacuation conditions vs. standard drill conditions.',
    'If correlation is weak or absent, drills are ritual; if strong, they are competence-building. The hybrid reading assumes weak correlation, placing evacuation in the ritualized stratum. Strong correlation would push this toward competence_reading. No correlation would push toward husk_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_drill_correlation, empirical, 'Whether evacuation drills predict actual evacuation competence.').

omega_variable(
    engineering_maintenance_continuity,
    'Does professionalized engineering inspection actually maintain structural competence across generations, or does inspection itself become ritualized when not actively exercised by operational actors?',
    'Longitudinal analysis of infrastructure condition and failure rates across decades; comparison of jurisdictions with different inspection regimes; post-event investigation of structural performance vs. pre-event inspection certification. Professional knowledge-transfer and training continuity in engineering communities.',
    'If inspection maintains real competence, the engineering stratum is stable and genuinely competent. If inspection becomes certificational theater (signing off on standards without real verification), engineering is also ritualized, and the constraint is husk_reading. The hybrid reading assumes inspection is genuinely competent; this omega tests that assumption.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(engineering_maintenance_continuity, empirical, 'Whether engineering inspection maintains real structural competence or is itself ritualized.').

omega_variable(
    stratification_visibility_suppression,
    'Is the divergence between engineering competence and evacuation ritual actively suppressed (reframed as unified preparedness) or passively invisible?',
    'Analysis of institutional rhetoric and governance documents — does the preparedness apparatus explicitly deny the divergence, or simply fail to acknowledge it? Discourse analysis of responses to post-event investigations that reveal evacuation failures. Interviews with institutional designers about whether the stratification was intentional or emergent.',
    'If suppression is active (institutional actors deny divergence), the suppression metric is correctly high and the constraint is actively enforced. If invisible (the apparatus simply never articulated the boundary between engineering and drills), suppression is lower and the constraint operates by inertia rather than active coercion. The distinction matters for remediation: active suppression requires confronting institutional interests; passive invisibility might be fixed by simple acknowledgment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stratification_visibility_suppression, conceptual, 'Whether the strata''s divergence is actively suppressed or passively invisible.').

omega_variable(
    kernel_reading_selection_justification,
    'Is this hybrid reading a structurally distinct third position in the preparedness_persistence kernel, or is it a compromise position that obscures a genuine binary choice between competence_reading and husk_reading?',
    'Detailed structural analysis: can a single coherent framing hold both ''some components are genuinely competent'' and ''others are ritual''? Or do the two claims require different framings of what preparedness IS? Test by considering: if engineering is competent but drills fail in real events, is preparedness adequately maintained? If not, the hybrid is saying ''preparedness is inadequate but persists anyway'' — which is Piton semantics. If yes, it is saying ''engineering adequately addresses the founding problem'' — which undercuts the need for drills and pushes toward pure competence_reading.',
    'If the hybrid reading is a genuine third pole (stratified competence + ritual persistence), it stands as its own constraint. If it is a compromise that dissolves under pressure, it may be more accurate to frame as husk_reading with a competent engineering appendix (two constraints, not one). This omega gates whether this reading has conceptual coherence as a single constraint story or should be decomposed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_justification, conceptual, 'Whether the hybrid reading is a coherent third pole or a compromise obscuring a simpler choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__hybrid_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(prep_tr_t8, preparedness_persistence__hybrid_reading, theater_ratio, 8, 0.42).
narrative_ontology:measurement(prep_tr_t16, preparedness_persistence__hybrid_reading, theater_ratio, 16, 0.51).
narrative_ontology:measurement(prep_tr_t24, preparedness_persistence__hybrid_reading, theater_ratio, 24, 0.59).
narrative_ontology:measurement(prep_tr_t32, preparedness_persistence__hybrid_reading, theater_ratio, 32, 0.61).
narrative_ontology:measurement(prep_tr_t40, preparedness_persistence__hybrid_reading, theater_ratio, 40, 0.61).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__hybrid_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(prep_be_t8, preparedness_persistence__hybrid_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(prep_be_t16, preparedness_persistence__hybrid_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(prep_be_t24, preparedness_persistence__hybrid_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement(prep_be_t32, preparedness_persistence__hybrid_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement(prep_be_t40, preparedness_persistence__hybrid_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_persistence__hybrid_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(prep_su_t8, preparedness_persistence__hybrid_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(prep_su_t16, preparedness_persistence__hybrid_reading, suppression_requirement, 16, 0.54).
narrative_ontology:measurement(prep_su_t24, preparedness_persistence__hybrid_reading, suppression_requirement, 24, 0.57).
narrative_ontology:measurement(prep_su_t32, preparedness_persistence__hybrid_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(prep_su_t40, preparedness_persistence__hybrid_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_persistence__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, preparedness_persistence__competence_reading).
narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, preparedness_persistence__husk_reading).

% DUAL FORMULATION NOTE:
% The preparedness_persistence kernel decomposes into three structurally distinct constraint stories: (1) competence_reading asserts that drills and inspections maintain live knowledge; (2) husk_reading asserts that preparedness is memorial form disconnected from real capacity; (3) hybrid_reading (this story) asserts that the constraint embeds both strata and persists because their asymmetry is invisible or suppressed. The three readings are not alternatives that the same observation could distinguish — they are different framings of what the preparedness system IS. The engine computes which reading best explains the structural data (who benefits, who pays, what persists, how suppression operates). This reading influences both siblings because it declares that the strata exist and their divergence is the structural fact to be explained. If this reading's strata are real, competence_reading must explain why drills persist despite their ritual character, and husk_reading must explain why engineering remains competent when everything else decays.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_persistence__hybrid_reading, powerless, 0.87).
constraint_indexing:directionality_override(preparedness_persistence__hybrid_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
