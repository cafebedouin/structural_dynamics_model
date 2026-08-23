% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__behavioral_competence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stone_land_use_rule__behavioral_competence, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: stone_land_use_rule__behavioral_competence
 *   human_readable: Stone-Marked Hazard Avoidance Rule (Behavioral Competence Reading)
 *   domain: disaster_anthropology/land_use_governance
 *
 * SUMMARY:
 *   This constraint story models a stone marker that functions as a live
 *   land-use prohibition in a disaster-affected community. The
 *   behavioral_competence reading treats the stone as an active coordination
 *   device: for 78 years, residents have accepted the economic cost of steep
 *   climbs to avoid a hazard zone, with daily spatial practice serving as the
 *   enforcement mechanism. The reading claims the stone is not a memorial
 *   relic but a binding rule whose violation is checked by routine. The
 *   kernel is contested by the commemorative_husk reading, which asserts the
 *   stone has decayed to symbolic status without behavioral force. This JSON
 *   instantiates only the behavioral_competence reading as a clean
 *   epsilon-invariant constraint.
 *
 * KEY AGENTS:
 *   - local_residents (moderate/constrained): Bear symmetric coordination costs (steep climbs) and receive hazard avoidance.
 *   - customary_council (moderate/constrained): Maintains the stone and institutional memory; no material extraction.
 *   - disaster_anthropologist (analytical/analytical): Observes the 78-year compliance record and the kernel contest.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__behavioral_competence, 0.18).
domain_priors:suppression_score(stone_land_use_rule__behavioral_competence, 0.22).
domain_priors:theater_ratio(stone_land_use_rule__behavioral_competence, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, extractiveness, 0.18).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__behavioral_competence, rope).
narrative_ontology:human_readable(stone_land_use_rule__behavioral_competence, "Stone-Marked Hazard Avoidance Rule (Behavioral Competence Reading)").
narrative_ontology:topic_domain(stone_land_use_rule__behavioral_competence, "disaster_anthropology/land_use_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__behavioral_competence, 'd70bdb56-3adc-4b3b-9278-7ade7b2f9a2e').
narrative_ontology:cs_kernel_codification('d70bdb56-3adc-4b3b-9278-7ade7b2f9a2e', fixed_text).
narrative_ontology:cs_authority_grounding('d70bdb56-3adc-4b3b-9278-7ade7b2f9a2e', practice).
narrative_ontology:cs_interpretation_layer_present('d70bdb56-3adc-4b3b-9278-7ade7b2f9a2e').
narrative_ontology:cs_reading_relation('d70bdb56-3adc-4b3b-9278-7ade7b2f9a2e', stone_land_use_rule__commemorative_husk, forecloses).
narrative_ontology:cs_axiom('d70bdb56-3adc-4b3b-9278-7ade7b2f9a2e', foundational, stone_retains_binding_force).
narrative_ontology:cs_axiom_status(stone_retains_binding_force, holdable).
narrative_ontology:cs_axiom_grounding('d70bdb56-3adc-4b3b-9278-7ade7b2f9a2e', stone_retains_binding_force, conventional).
narrative_ontology:cs_axiom('d70bdb56-3adc-4b3b-9278-7ade7b2f9a2e', foundational, hazard_avoidance_overrides_convenience).
narrative_ontology:cs_axiom_status(hazard_avoidance_overrides_convenience, holdable).
narrative_ontology:cs_axiom_grounding('d70bdb56-3adc-4b3b-9278-7ade7b2f9a2e', hazard_avoidance_overrides_convenience, instrumental).
narrative_ontology:cs_reference_frame('d70bdb56-3adc-4b3b-9278-7ade7b2f9a2e', active_prohibitory_boundary).
narrative_ontology:cs_drift_state('d70bdb56-3adc-4b3b-9278-7ade7b2f9a2e', contemporary, gap(repudiation_pressure, minor, false)).
narrative_ontology:cs_created_at('d70bdb56-3adc-4b3b-9278-7ade7b2f9a2e', '').
narrative_ontology:cs_kernel_id(stone_land_use_rule__behavioral_competence, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, local_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Community members who daily accept steep climbs and longer routes to avoid the stone-marked zone, sustaining a 78-year compliance pattern in exchange for perceived hazard avoidance and intergenerational safety.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, local_residents, beneficiary,
    moderate, generational, constrained, local).

% Local governance body that maintains the physical stone, transmits the origin narrative, and legitimizes the boundary. They enforce no sanctions but sustain the institutional frame through ritual remembrance and land-use planning.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, customary_council, agenda_setter,
    moderate, generational, constrained, local).

% Researchers who document the 78-year compliance record and debate whether the stone governs behavior or has decayed to a commemorative husk.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, disaster_anthropologist, observer,
    analytical, generational, analytical, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective avoidance of a hazardous zone by marking a boundary and aligning pedestrian and land-use practice across generations, preventing individual shortcut-taking that would undermine group safety.
% TRANSFER_FUNCTION: Transfers time and physical effort (steep climbs) from individual community members to the collective account of hazard avoidance; no asymmetric material transfer.
% ABSENT_VOICES: Younger residents and external developers who would prefer to route through the prohibited zone for economic convenience; they are absent from the customary decision-making space or overruled by intergenerational consensus.
% DISAPPEARANCE_RATIONALE: If the prohibition disappeared overnight, the coordinated avoidance pattern would fragment; some residents would take the shorter route, land-use pressure would mount, and the 78-year safety buffer would erode within a generation as institutional memory decoupled from practice.
% FOUNDING_PROBLEM: A past disaster created a persistent hazard zone that required coordinated community avoidance to prevent individual risk-taking that would endanger lives and livelihoods.
% FOUNDING_PROBLEM_CORROBORATION: Geological hazard assessments and historical disaster records from outside the community corroborate the original risk; however, independent surveys contest whether the current risk profile justifies the sustained economic cost of avoidance.
narrative_ontology:disappearance_verdict(stone_land_use_rule__behavioral_competence, world_rearranges).
narrative_ontology:founding_problem_status(stone_land_use_rule__behavioral_competence, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__behavioral_competence, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(stone_land_use_rule__behavioral_competence, 'none', 1).
narrative_ontology:epsilon_provenance(stone_land_use_rule__behavioral_competence, 0.18, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stone_land_use_rule__behavioral_competence_tests).
:- end_tests(stone_land_use_rule__behavioral_competence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the constraint moves only time and effort into collective hazard avoidance, with no captured surplus. Suppression is low-moderate (0.22) because deviation is checked by social practice and spatial routine, not by coercion or sanction. Theater ratio is very low (0.12): the steep climbs are real costs, not performative. Accessibility collapse is moderate (0.40): once a resident knows the rule, the shortcut socially collapses even if it remains physically open. Resistance is minimal (0.10) because compliance is sustained and largely internalized across generations. The measurement series show stable low extraction with minor fluctuations, consistent with 78 years of sustained coordination.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (customary council) and beneficiary seat (local residents) both experience the constraint as legitimate coordination protecting against a shared hazard. There is no strong perspectival divergence into extraction because no party captures asymmetric rents; the engine will compute both seats as near-symmetric or mild-beneficiary, consistent with rope classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Local residents are declared beneficiaries (low d) because they receive the primary coordination good (hazard avoidance). Their directionality is not pushed toward target because they are not structurally victimized; the steep climb is a symmetric coordination cost. The customary council sits near the agenda-setter center with no extraction to capture. No directionality overrides are needed because the structural derivation from beneficiary declarations and exit options accurately reflects the symmetric coordination picture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â a disaster-created hazard requiring coordinated avoidance â is contested but still plausibly live. The constraint persists because it solves a genuine coordination problem (preventing individual risk-taking that would undermine collective safety), not because an entrenched party extracts from its continuation. If the hazard were proven extinct, the constraint would risk mandatrophy (becoming a piton or zombie rope), but the behavioral_competence reading asserts the hazard rationale remains structurally operative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_force_vs_memorial,
    'Does the stone currently enforce binding land-use avoidance, or has it decayed to a commemorative symbol without behavioral compulsion?',
    'Ethnographic observation of actual pedestrian routing and land-use patterns; interviews with residents on whether shortcut-taking triggers social sanction or hazard concern.',
    'If behavioral force is absent, this reading collapses toward the commemorative_husk sibling (low/zero extraction, possible piton); if present, the live rope classification is sustained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_force_vs_memorial, empirical, 'Kernel contest: whether the stone retains behavioral competence').

omega_variable(
    hazard_persistence,
    'Does the geological or disaster hazard that originally justified the prohibition still obtain at levels that warrant sustained economic cost?',
    'Independent geological and disaster-risk assessment of the prohibited zone compared with historical baseline.',
    'If hazard is proven extinct, the coordination function becomes mandatrophic (zombie rope/piton risk); if hazard persists, the rope classification is strongly corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hazard_persistence, empirical, 'Whether the underlying hazard remains live').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is compliance driven by internalized norms, external social sanction, or rational hazard avoidance?',
    'Post-exit or generational-change observation: if relocated residents still avoid analogous zones, suppression is internalized; if compliance drops when social observers are absent, it is external.',
    'Internalized suppression raises effective extraction modestly because the constraint travels with the agent; external suppression keeps extraction bounded by the local social field.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__behavioral_competence, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ston_tr_t0, stone_land_use_rule__behavioral_competence, theater_ratio, 0, 0.08).
narrative_ontology:measurement(ston_tr_t13, stone_land_use_rule__behavioral_competence, theater_ratio, 13, 0.09).
narrative_ontology:measurement(ston_tr_t26, stone_land_use_rule__behavioral_competence, theater_ratio, 26, 0.1).
narrative_ontology:measurement(ston_tr_t39, stone_land_use_rule__behavioral_competence, theater_ratio, 39, 0.11).
narrative_ontology:measurement(ston_tr_t52, stone_land_use_rule__behavioral_competence, theater_ratio, 52, 0.12).
narrative_ontology:measurement(ston_tr_t65, stone_land_use_rule__behavioral_competence, theater_ratio, 65, 0.13).
narrative_ontology:measurement(ston_tr_t78, stone_land_use_rule__behavioral_competence, theater_ratio, 78, 0.12).

% Extraction over time
narrative_ontology:measurement(ston_be_t0, stone_land_use_rule__behavioral_competence, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(ston_be_t13, stone_land_use_rule__behavioral_competence, base_extractiveness, 13, 0.16).
narrative_ontology:measurement(ston_be_t26, stone_land_use_rule__behavioral_competence, base_extractiveness, 26, 0.17).
narrative_ontology:measurement(ston_be_t39, stone_land_use_rule__behavioral_competence, base_extractiveness, 39, 0.18).
narrative_ontology:measurement(ston_be_t52, stone_land_use_rule__behavioral_competence, base_extractiveness, 52, 0.17).
narrative_ontology:measurement(ston_be_t65, stone_land_use_rule__behavioral_competence, base_extractiveness, 65, 0.18).
narrative_ontology:measurement(ston_be_t78, stone_land_use_rule__behavioral_competence, base_extractiveness, 78, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(stone_land_use_rule__behavioral_competence, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
