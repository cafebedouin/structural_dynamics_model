% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_commitment__behavioral_competence_reading, []).

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
 *   constraint_id: aneyoshi_stone_commitment__behavioral_competence_reading
 *   human_readable: Aneyoshi Stone as Live Land-Use Rule
 *   domain: disaster_anthropology/commitment_systems
 *
 * SUMMARY:
 *   The Aneyoshi stone, inscribed after the 1933 tsunami with a warning not
 *   to build below its elevation, is analyzed here under the behavioral
 *   competence reading: the inscription functioned as a live, operationally
 *   effective land-use rule that constrained building location across 78
 *   years. In 2011, the village's compliance with this directive resulted in
 *   zero fatalities despite massive tsunami inundation. This reading treats
 *   the stone not as a decayed monument but as a persistent rope coordinating
 *   generational settlement behavior around an ever-present geophysical risk.
 *
 * KEY AGENTS:
 *   - Aneyoshi community (organized/local): Net beneficiary of the coordination rule â survives tsunamis by maintaining elevated settlement.
 *   - Village custodians (moderate/local): Agenda-setters who transmit and enforce the land-use norm through intergenerational practice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__behavioral_competence_reading, 0.06).
domain_priors:suppression_score(aneyoshi_stone_commitment__behavioral_competence_reading, 0.12).
domain_priors:theater_ratio(aneyoshi_stone_commitment__behavioral_competence_reading, 0.03).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 0.03).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__behavioral_competence_reading, rope).
narrative_ontology:human_readable(aneyoshi_stone_commitment__behavioral_competence_reading, "Aneyoshi Stone as Live Land-Use Rule").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__behavioral_competence_reading, "disaster_anthropology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__behavioral_competence_reading, '315470be-7130-4119-a4d9-b0fa7fa527c4').
narrative_ontology:cs_kernel_codification('315470be-7130-4119-a4d9-b0fa7fa527c4', fixed_text).
narrative_ontology:cs_authority_grounding('315470be-7130-4119-a4d9-b0fa7fa527c4', practice).
narrative_ontology:cs_interpretation_layer_present('315470be-7130-4119-a4d9-b0fa7fa527c4').
narrative_ontology:cs_reading_relation('315470be-7130-4119-a4d9-b0fa7fa527c4', aneyoshi_stone_commitment__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('315470be-7130-4119-a4d9-b0fa7fa527c4', foundational, inscribed_directive_governs_settlement).
narrative_ontology:cs_axiom_status(inscribed_directive_governs_settlement, holdable).
narrative_ontology:cs_axiom_grounding('315470be-7130-4119-a4d9-b0fa7fa527c4', inscribed_directive_governs_settlement, conventional).
narrative_ontology:cs_reference_frame('315470be-7130-4119-a4d9-b0fa7fa527c4', active_settlement_constraint).
narrative_ontology:cs_drift_state('315470be-7130-4119-a4d9-b0fa7fa527c4', post_2011_tsunami, gap(stable, minor, true)).
narrative_ontology:cs_created_at('315470be-7130-4119-a4d9-b0fa7fa527c4', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Residents of Aneyoshi, Iwate Prefecture, who have located homes and public buildings above the stone's inscribed tsunami boundary since its emplacement circa 1933. Their settlement pattern meant that when the 2011 tsunami struck, the village's built environment remained above the inundation line, resulting in zero fatalities and minimal structure loss.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_community, beneficiary,
    organized, generational, constrained, local).

% Elders and household heads who transmit the stone's directive during land-use discussions, mediating disputes and approving building sites. Their authority derives from the village's continuous practice of heeding the marker, not from external enforcement.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, village_custodians, agenda_setter,
    moderate, generational, identity_locked, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates intergenerational settlement location across a coastal community to ensure habitation remains above historical tsunami inundation limits, solving the collective-action problem of individual short-term building convenience versus long-term communal survival.
% TRANSFER_FUNCTION: Moves risk exposure from the collective settlement pool to the safer topographic zone; individual households forgo the convenience of low-elevation construction in exchange for the community's continued existence across generations.
% ABSENT_VOICES: Short-term developers and younger households seeking low-elevation coastal lots for convenience or economy are not present in the village's decision framework; their preferences are structurally excluded by the communal norm rather than admitted and overruled.
% DISAPPEARANCE_RATIONALE: Without the stone and its accompanying tradition, settlement would likely have crept seaward for convenience, as occurred in neighboring villages that lacked such persistent markers; the 2011 tsunami would then have caused fatalities and structural destruction in Aneyoshi.
% FOUNDING_PROBLEM: The 1896 Meiji Sanriku tsunami destroyed earlier settlement patterns, creating a need for a persistent, generation-spanning mechanism to prevent the gradual reclamation of dangerous low-elevation land for housing.
% FOUNDING_PROBLEM_CORROBORATION: Geological records of the 1896 and 1933 tsunamis corroborate the inundation risk. The 2011 Tohoku tsunami provided independent empirical validation that villages maintaining such markers survived while those relying on modern seawalls without behavioral rules suffered catastrophic loss.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_stone_commitment__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, 0.06, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment__behavioral_competence_reading_tests).
:- end_tests(aneyoshi_stone_commitment__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.06) because the stone does not transfer resources from one party to another; it coordinates survival. Suppression is low (0.12) because compliance rests on shared recognition of tsunami risk and the stone's informational value, not on coercive exclusion of alternatives. Theater ratio is minimal (0.03): there is little performative maintenance because the rule operates through lived practice rather than display. Resistance is negligible (0.04) because no party within the village is materially harmed by the constraint. The flat measurement series across 78 years reflects institutional stability rather than drift.
 *
 * PERSPECTIVAL GAP:
 *   The community and custodian seats experience this constraint as protective coordination. An external developer would experience it as a barrier to low-elevation coastal construction, though such actors are excluded from the village decision frame. The engine computes this divergence from structural position: beneficiaries with generational time horizons and constrained but survivable exits see a rope; excluded short-term actors with mobile capital would see a barrier.
 *
 * DIRECTIONALITY LOGIC:
 *   Both declared stakeholders are on the beneficiary side of the directionality axis: the community receives survival and the custodians receive social coordination. There are no declared victims because the constraint extracts from no one; its 'cost' is the forgone convenience of lower-elevation building, which is voluntarily surrendered in light of the physical danger. No directionality overrides are required because the structural derivation (beneficiary declarations plus exit options) correctly places both agents near d=0.0.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the stone as a piton or snare. A piton reading would require atrophied function and theatrical maintenance, but the 2011 survival demonstrates continued operational competence. A snare reading would require identifiable victims suffering coerced extraction, which is absent. The rope classification captures that the constraint solves a genuine collective-action problem â intergenerational land-use coordination against natural hazard â without asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Did the Aneyoshi stone retain operational force in land-use decisions across the full 78-year interval, or did it decay to a commemorative artifact without behavioral constraint?',
    'Archaeological and ethnographic reconstruction of building-permit and household-location records in the decades before 2011; comparison with villages that lacked such markers.',
    'If the stone was merely commemorative, this reading misclassifies a piton or husk as a rope; if operational, the sibling reading mischaracterizes a functioning coordination device.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Contested kernel ambiguity between live regulatory rule and symbolic memorial husk').

omega_variable(
    causal_attribution_2011,
    'Was the village''s 2011 survival causally attributable to compliance with the stone''s directive specifically, rather than to topography or independent risk knowledge?',
    'Comparative geospatial analysis of inundation patterns against building locations relative to the stone boundary in Aneyoshi versus adjacent settlements with similar topography but no persistent markers.',
    'If survival was due to topography independent of the stone, the stone''s coordination function is overstated; if due to stone-guided settlement, the rope classification is strongly reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_attribution_2011, empirical, 'Causal attribution of survival to the stone versus alternative factors').

omega_variable(
    authority_framing_ambiguity,
    'Is the stone''s authority better framed as lineage (deriving from the founding generation''s warning) or as practice (deriving from ongoing community behavioral compliance)?',
    'Ethnographic observation of whether custodians cite ancestral command or living common sense when justifying the boundary; generational transmission pattern analysis.',
    'If lineage, the commitment system is more brittle to generational change and may shift toward extraction or piton if transmission breaks; if practice, it is more resilient and remains a rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_framing_ambiguity, conceptual, 'Framing under-determination of the commitment system''s authority grounding').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__behavioral_competence_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(aney_tr_t15, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 15, 0.02).
narrative_ontology:measurement(aney_tr_t30, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 30, 0.03).
narrative_ontology:measurement(aney_tr_t45, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 45, 0.02).
narrative_ontology:measurement(aney_tr_t60, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 60, 0.03).
narrative_ontology:measurement(aney_tr_t78, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 78, 0.03).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(aney_be_t15, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 15, 0.05).
narrative_ontology:measurement(aney_be_t30, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 30, 0.06).
narrative_ontology:measurement(aney_be_t45, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 45, 0.05).
narrative_ontology:measurement(aney_be_t60, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 60, 0.06).
narrative_ontology:measurement(aney_be_t78, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 78, 0.06).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t0, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(aney_su_t15, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 15, 0.11).
narrative_ontology:measurement(aney_su_t30, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 30, 0.1).
narrative_ontology:measurement(aney_su_t45, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 45, 0.12).
narrative_ontology:measurement(aney_su_t60, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 60, 0.11).
narrative_ontology:measurement(aney_su_t78, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 78, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__behavioral_competence_reading, resource_allocation).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__behavioral_competence_reading, commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% This constraint is the behavioral_competence_reading of the aneyoshi_stone_commitment kernel, which decomposes into two structurally distinct claims: one where the stone retains operational force (this file) and one where it decayed to commemorative husk (commemorative_husk_reading). The epsilon values differ: this reading claims near-zero extraction through active coordination; the sibling likely claims higher theater_ratio and extraction through inertial performance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
