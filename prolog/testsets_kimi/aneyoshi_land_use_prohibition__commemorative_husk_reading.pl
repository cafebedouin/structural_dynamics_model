% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_land_use_prohibition__commemorative_husk_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: aneyoshi_land_use_prohibition__commemorative_husk_reading
 *   human_readable: Aneyoshi Stone Commemorative Husk (Decayed Prohibition)
 *   domain: disaster_anthropology/commitment_systems
 *
 * SUMMARY:
 *   The Aneyoshi tsunami stone was erected by survivors of the 1933 Showa
 *   Sanriku tsunami with the inscription 'Do not build below this line.' In
 *   the commemorative husk reading, the stone has decayed from a live
 *   land-use prohibition into a heritage memorial without behavioral force.
 *   This reading instantiates a constraint where the commemorative frame is
 *   maintained by municipal planning authorities, property developers capture
 *   building rights in the hazardous zone, and future residents bear the
 *   catastrophic risk. The constraint is structurally distinct from the
 *   behavioral competence reading, which treats the stone as an enforced rule
 *   across 78 years.
 *
 * KEY AGENTS:
 *   - property_developers (beneficiary/powerful/mobile) â capture coastal building rights enabled by the non-enforcement of the prohibition
 *   - municipal_planning_authority (agenda_setter/institutional/constrained) â administers the heritage classification and issues permits below the line
 *   - future_residents_below_line (payer/powerless/trapped) â occupy the developed hazard zone and bear tsunami risk
 *   - tsunami_survivor_descendants (excluded/moderate/identity_locked) â maintain the stone's memory but are excluded from zoning authority
 *   - disaster_anthropologists (observer/analytical/analytical) â analyze the institutional decay and competing readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.78).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.65).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 0.82).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__commemorative_husk_reading, tangled_rope).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__commemorative_husk_reading, "Aneyoshi Stone Commemorative Husk (Decayed Prohibition)").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__commemorative_husk_reading, "disaster_anthropology/commitment_systems").

domain_priors:requires_active_enforcement(aneyoshi_land_use_prohibition__commemorative_husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__commemorative_husk_reading, '1c1b2f9a-4d96-4e61-aff6-b2d53175cae7').
narrative_ontology:cs_kernel_codification('1c1b2f9a-4d96-4e61-aff6-b2d53175cae7', fixed_text).
narrative_ontology:cs_authority_grounding('1c1b2f9a-4d96-4e61-aff6-b2d53175cae7', practice).
narrative_ontology:cs_interpretation_layer_present('1c1b2f9a-4d96-4e61-aff6-b2d53175cae7').
narrative_ontology:cs_reading_relation('1c1b2f9a-4d96-4e61-aff6-b2d53175cae7', aneyoshi_land_use_prohibition__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('1c1b2f9a-4d96-4e61-aff6-b2d53175cae7', foundational, stone_is_cultural_heritage_not_law).
narrative_ontology:cs_axiom_status(stone_is_cultural_heritage_not_law, holdable).
narrative_ontology:cs_axiom_grounding('1c1b2f9a-4d96-4e61-aff6-b2d53175cae7', stone_is_cultural_heritage_not_law, conventional).
narrative_ontology:cs_axiom('1c1b2f9a-4d96-4e61-aff6-b2d53175cae7', foundational, disaster_memory_served_by_presence_not_prohibition).
narrative_ontology:cs_axiom_status(disaster_memory_served_by_presence_not_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('1c1b2f9a-4d96-4e61-aff6-b2d53175cae7', disaster_memory_served_by_presence_not_prohibition, conventional).
narrative_ontology:cs_reference_frame('1c1b2f9a-4d96-4e61-aff6-b2d53175cae7', commemorative_frame_operational).
narrative_ontology:cs_drift_state('1c1b2f9a-4d96-4e61-aff6-b2d53175cae7', contemporary_development_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1c1b2f9a-4d96-4e61-aff6-b2d53175cae7', '').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, property_developers).
narrative_ontology:constraint_victim(aneyoshi_land_use_prohibition__commemorative_husk_reading, future_residents_below_line).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Purchase and develop coastal land below the tsunami stone line, capitalizing on ocean access and views that would be unavailable if the prohibition were enforced as binding. They lobby for heritage-frame zoning that treats the stone as non-regulatory and capture the resulting land-value premiums.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, property_developers, beneficiary,
    powerful, biographical, mobile, local).

% Administers zoning and heritage designations. Officially classifies the stone as a cultural memorial without regulatory force, issues building permits below the line, and maintains the monument for tourism and education. Politically constrained by development interests and tax-base imperatives.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, municipal_planning_authority, agenda_setter,
    institutional, generational, constrained, local).

% Occupy housing constructed below the stone line, often unaware of the tsunami risk or the stone's original prohibitory intent. They bear the catastrophic risk that returns on generational timescales, with no effective voice in the planning decisions that preceded their arrival.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, future_residents_below_line, payer,
    powerless, biographical, trapped, local).

% Maintain the stone, conduct commemorative rituals, and transmit oral history of the 1933 tsunami. They would enforce the prohibition as a live rule if included in planning authority, but their testimony is solicited only for heritage ceremonies, not zoning decisions.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, tsunami_survivor_descendants, excluded,
    moderate, generational, identity_locked, local).

% Document the transformation of the stone from warning to memorial. They analyze the institutional decay, the competing readings of the kernel, and the shifting beneficiary structure across generations.
narrative_ontology:constraint_stakeholder(aneyoshi_land_use_prohibition__commemorative_husk_reading, disaster_anthropologists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_land_use_prohibition__commemorative_husk_reading, property_developers).
narrative_ontology:fixing_cost_class(aneyoshi_land_use_prohibition__commemorative_husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves intergenerational memory of the 1933 Showa Sanriku tsunami and the ancestor warning through monument maintenance, ritual visitation, and educational heritage framing.
% TRANSFER_FUNCTION: Moves coastal building rights and associated profits to property developers; moves tsunami catastrophe risk to future residents who occupy land developed below the stone line.
% ABSENT_VOICES: Future residents are excluded from planning decisions made decades before their arrival; survivor descendants who treat the stone as a live warning are consulted for heritage ceremonies but excluded from zoning authority.
% DISAPPEARANCE_RATIONALE: If the commemorative-husk framing vanished and the prohibition were reactivated as binding, coastal development below the line would halt, existing structures would face devaluation or removal, and settlement patterns would reorganize above the hazard zone.
% FOUNDING_PROBLEM: The 1933 tsunami destroyed Aneyoshi; survivors erected the inscribed stone to prohibit building below the line and prevent future loss of life.
% FOUNDING_PROBLEM_CORROBORATION: Disaster geologists and independent historians attest the tsunami hazard remains live. Municipal records confirm the prohibition is not enforced as law. The 'dead' status of the prohibition-as-rule is asserted by planning authorities who simultaneously permit development; risk researchers outside the benefiting parties attest the founding hazard persists.
narrative_ontology:disappearance_verdict(aneyoshi_land_use_prohibition__commemorative_husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_land_use_prohibition__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_land_use_prohibition__commemorative_husk_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_land_use_prohibition__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_land_use_prohibition__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_land_use_prohibition__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because development interests realize concentrated gains from coastal land that would be unavailable under enforcement; the cost is externalized to future residents who face catastrophe. Suppression (0.65) reflects the institutional suppression of the prohibition's regulatory intent through heritage framing. Theater_ratio is high (0.82) because the stone is actively maintained as a performative memorial while its protective function is hollowed out. Accessibility_collapse (0.60) measures how thoroughly alternative enforcement readings have been displaced by the commemorative narrative. Resistance (0.40) captures intermittent descendant and researcher objections that have not reversed the zoning pattern. The temporal series align on a shared grid to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   The property developer and planning authority seats should compute as near-beneficiary: the constraint subsidizes their interests (development profit, administrative simplicity). Future residents compute as full-target: they bear the catastrophic extraction. Descendant communities sit ambiguously â they value the memorial but suffer the risk, producing a near-symmetric d that the engine will resolve based on exit options and power. The divergence is structural, not perspectival in the sense of opinion.
 *
 * DIRECTIONALITY LOGIC:
 *   Property developers are declared beneficiaries with mobile exit (can invest elsewhere), yielding low d and damped extraction. Future residents are declared victims with trapped exit (cannot easily relocate, no voice in prior decisions), yielding high d and amplified extraction. The municipal planning authority is agenda_setter with constrained exit (political economy binds them to the development coalition), deriving moderate d. Descendants are excluded with identity-locked exit (the stone is constitutive of community memory), yielding complex directionality that the engine computes as near-symmetric with slight target skew due to victimization risk.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by separating the genuine commemorative coordination (memory preservation) from the extraction it enables (hazard-zone development). A mandatrophy-resolved piton reading would be incorrect because there is a concentrated beneficiary capturing the extraction (developers), and there is identifiable harm (future residents). The commemorative function is not vestigial performance but actively functional memory-work â it is the coupling of this real function to asymmetric extraction that makes the constraint a tangled rope rather than a snare or piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the Aneyoshi stone a live prohibitory rule or a commemorative memorial?',
    'Archival zoning record review and oral history triangulation to determine whether permits below the line were denied or granted across the interval.',
    'If the behavioral competence reading is correct, the constraint is a different type with different victims and lower extractiveness; if this reading is correct, the commemorative frame enables concentrated development benefit at catastrophic risk to future residents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Structural ambiguity between live rule and commemorative husk readings of the same stone.').

omega_variable(
    enforcement_decay_mechanism,
    'Did the prohibition decay through gradual cultural forgetting, active administrative reclassification, or economic capture by development interests?',
    'Municipal planning document genealogy tracing the heritage designation and its sponsorship; interview data from planning officials across decades.',
    'Decay through passive forgetting would suggest institutional entropy; active capture would confirm tangled_rope or snare classification with culpable beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_decay_mechanism, empirical, 'Whether the decay to memorial was passive or actively engineered.').

omega_variable(
    commemorative_function_authenticity,
    'Does the memorial function serve genuine collective remembrance, or does it primarily legitimate non-enforcement?',
    'Ethnographic observation of commemorative practice versus analysis of zoning outcomes: if ritual activity is robust but regulatory force is systematically withheld, the commemorative function is authentically present yet structurally coupled to extraction.',
    'An authentic commemorative function sustains tangled_rope classification; a purely legitimating function would push toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commemorative_function_authenticity, conceptual, 'Whether memorial function is genuine coordination or cover story.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(aney_tr_t15, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(aney_tr_t30, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 30, 0.5).
narrative_ontology:measurement(aney_tr_t45, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 45, 0.6).
narrative_ontology:measurement(aney_tr_t60, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 60, 0.68).
narrative_ontology:measurement(aney_tr_t75, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 75, 0.75).
narrative_ontology:measurement(aney_tr_t90, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 90, 0.82).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(aney_be_t15, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 15, 0.3).
narrative_ontology:measurement(aney_be_t30, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(aney_be_t45, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 45, 0.55).
narrative_ontology:measurement(aney_be_t60, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(aney_be_t75, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 75, 0.72).
narrative_ontology:measurement(aney_be_t90, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 90, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t0, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(aney_su_t15, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 15, 0.3).
narrative_ontology:measurement(aney_su_t30, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(aney_su_t45, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 45, 0.5).
narrative_ontology:measurement(aney_su_t60, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement(aney_su_t75, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 75, 0.64).
narrative_ontology:measurement(aney_su_t90, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 90, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__commemorative_husk_reading, behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% The aneyoshi_land_use_prohibition kernel decomposes into two epsilon-invariant constraints: the behavioral_competence_reading (live enforcement across 78 years) and the commemorative_husk_reading (decayed to memorial). The readings share the same stone text but instantiate different constraints with different beneficiary/victim structures and different epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
