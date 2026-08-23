% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_commitment__commemorative_husk_reading, []).

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
 *   constraint_id: aneyoshi_stone_commitment__commemorative_husk_reading
 *   human_readable: Aneyoshi Stone Commitment â Commemorative Husk Reading
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   The Aneyoshi stone is an inscribed tsunami warning marker in northeastern
 *   Japan, historically commanding settlement above a certain elevation. This
 *   constraint story instantiates the commemorative husk reading of the
 *   contested aneyoshi_stone_commitment kernel: the claim that over
 *   approximately 78 years the commitment decayed into pure symbolic
 *   observance, no longer constraining land-use decisions, with the 2011
 *   survival of the village attributable to luck, geography, or modern
 *   early-warning systems rather than stone compliance. The stone persists as
 *   a museum-piece heritage object administered by municipal authorities,
 *   extracting diffuse costs from residents who inherit its cultural
 *   maintenance without receiving protective coordination.
 *
 * KEY AGENTS:
 *   - Municipal heritage board (agenda_setter): Administers the stone as cultural property without extracting concentrated rents; could change its status but faces prohibitive political cost relative to benefit.
 *   - Coastal village residents (payer/powerless/identity_locked): Bear the diffuse costs of a non-functional memorial occupying the disaster-preparedness governance slot; their community identity is fused with stone veneration, making exit from the commemorative frame unthinkable.
 *   - Modern disaster planners (excluded): Their evidence-based mitigation proposals are marginalized by the stone's presence as a 'sufficient' heritage memory of risk.
 *   - Disaster anthropologists (observer/analytical): Provide the evidentiary base for the husk reading through ethnographic documentation of land-use independence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__commemorative_husk_reading, 0.78).
domain_priors:suppression_score(aneyoshi_stone_commitment__commemorative_husk_reading, 0.35).
domain_priors:theater_ratio(aneyoshi_stone_commitment__commemorative_husk_reading, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 0.82).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__commemorative_husk_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_stone_commitment__commemorative_husk_reading, "Aneyoshi Stone Commitment â Commemorative Husk Reading").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__commemorative_husk_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__commemorative_husk_reading, 'd5f9a137-faed-4199-9fc4-c446f0d21366').
narrative_ontology:cs_kernel_codification('d5f9a137-faed-4199-9fc4-c446f0d21366', fixed_text).
narrative_ontology:cs_authority_grounding('d5f9a137-faed-4199-9fc4-c446f0d21366', lineage).
narrative_ontology:cs_interpretation_layer_present('d5f9a137-faed-4199-9fc4-c446f0d21366').
narrative_ontology:cs_reading_relation('d5f9a137-faed-4199-9fc4-c446f0d21366', aneyoshi_stone_commitment__behavioral_competence_reading, influences).
narrative_ontology:cs_axiom('d5f9a137-faed-4199-9fc4-c446f0d21366', foundational, memorial_observance_supersedes_warning_function).
narrative_ontology:cs_axiom_status(memorial_observance_supersedes_warning_function, holdable).
narrative_ontology:cs_axiom_grounding('d5f9a137-faed-4199-9fc4-c446f0d21366', memorial_observance_supersedes_warning_function, empirically_contingent).
narrative_ontology:cs_axiom('d5f9a137-faed-4199-9fc4-c446f0d21366', foundational, tsunami_survival_independent_of_stone_compliance).
narrative_ontology:cs_axiom_status(tsunami_survival_independent_of_stone_compliance, holdable).
narrative_ontology:cs_axiom_grounding('d5f9a137-faed-4199-9fc4-c446f0d21366', tsunami_survival_independent_of_stone_compliance, empirically_contingent).
narrative_ontology:cs_reference_frame('d5f9a137-faed-4199-9fc4-c446f0d21366', tsunami_avoidance_commitment).
narrative_ontology:cs_drift_state('d5f9a137-faed-4199-9fc4-c446f0d21366', contemporary_heritage_management, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('d5f9a137-faed-4199-9fc4-c446f0d21366', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__commemorative_husk_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_victim(aneyoshi_stone_commitment__commemorative_husk_reading, coastal_village_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the Aneyoshi stone as a registered tangible cultural property, maintains the physical marker, and organizes annual memorial observances. Holds formal authority to reclassify or relocate the stone but faces no constituency demanding change; the political cost of altering a disaster-memorial landmark exceeds any administrative benefit.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, municipal_heritage_board, agenda_setter,
    moderate, generational, constrained, local).

% Inhabit the coastal settlement adjacent to the stone. Their land-use and construction decisions are driven by modern economic necessity and infrastructure policy, not by the inscription, yet they remain physically exposed to tsunami risk in the absence of contemporary engineered defenses. They participate in stone veneration as an inherited community obligation fused with local identity, while receiving no operational protective coordination from the marker.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, coastal_village_residents, payer,
    powerless, generational, identity_locked, local).

% Advocate for evidence-based setback ordinances, seawalls, and early-warning networks. Their proposals are consistently deprioritized in budget allocation because the stone's visible presence satisfies heritage and disaster-memory constituencies, preempting the political will required for expensive structural mitigation.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, modern_disaster_planners, excluded,
    moderate, biographical, constrained, regional).

% Document the stone's social function through ethnography and historical comparison. They observe the disconnection between the inscription's warning intent and contemporary building practice, supplying the evidentiary basis for the commemorative husk reading.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__commemorative_husk_reading, disaster_anthropologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_commitment__commemorative_husk_reading, diffuse).
narrative_ontology:fixing_cost_class(aneyoshi_stone_commitment__commemorative_husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the present interval. Historically, the stone coordinated settlement location by inscribing a permanent tsunami-risk boundary; that coordination function has atrophied entirely, leaving only symbolic memorial observance.
% TRANSFER_FUNCTION: Moves the burden of risk memory and disaster preparedness from institutional land-use governance to symbolic heritage performance, and from public infrastructure budgets to coastal residents who inherit the stone's cultural maintenance without its protective force.
% ABSENT_VOICES: Modern disaster planners and younger residents who would prefer engineered mitigation or enforced zoning over memorial observance are systematically excluded from heritage governance decisions.
% DISAPPEARANCE_RATIONALE: If the stone and its maintenance regime vanished overnight, land-use patterns would remain unchanged because they are already independent of the inscription; however, local disaster-memory governance and heritage tourism would reorganize around alternative commemorative practices, while disaster planners might finally gain budgetary traction for structural mitigation.
% FOUNDING_PROBLEM: To prevent repeat tsunami mortality by establishing a permanent, visible land-use prohibition that would persist across generational memory loss.
% FOUNDING_PROBLEM_CORROBORATION: Historical prefectural records and the physical inscription itself attest the founding warning function. Contemporary disaster geologists and land-use planners attest that the problem is no longer solved by the stone; municipal heritage administrators corroborate that they maintain the object for cultural remembrance, not risk governance.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__commemorative_husk_reading, contested).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(aneyoshi_stone_commitment__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__commemorative_husk_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_stone_commitment__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_stone_commitment__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the stone's commemorative status captures the institutional slot for disaster memory, substituting heritage performance for material risk reduction and preempting budget for engineered defenses. Theater_ratio is very high (0.82) because nearly all observable activityâcleaning, ceremonial observance, heritage registrationâperforms memory rather than governing behavior. Suppression is low (0.35) because the constraint persists by inertia and identity, not by active enforcement; suppression_requirement declines monotonically across the interval as enforcement atrophies. Accessibility_collapse is moderate (0.40): modern zoning alternatives are technically available but politically collapsed by the stone's occupation of the disaster-governance discourse. Resistance is low (0.20) because the object is culturally beloved; opposition takes the form of quiet professional marginalization rather than public contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the municipal heritage board, the stone is a low-cost cultural asset preserving ancestor memory. From the coastal resident seat, it is an inherited identity obligation that returns no protective value. From the behavioral competence reading, the same object is a live coordination mechanism; from this reading it is an inertial piton whose continued presence extracts safety investment from the governance agenda.
 *
 * DIRECTIONALITY LOGIC:
 *   The heritage board administers the constraint but is not a concentrated beneficiary; structural derivation places it near symmetric d. Coastal residents are declared victims with identity_locked exit, placing them near the full-target end (high d, amplified Ï). Disaster planners are excluded from the decision frame, receiving no voice in the heritage prioritization. No directionality overrides are required because the structural derivation matches the actual relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpreventing tsunami mortality through a permanent settlement boundaryâwas solved historically but the solution decayed. The mandate is dead: the stone no longer governs building practice, and its survival in 2011 is read as independent of its warning function. The constraint persists not because the problem is live, but because the institutional and identity costs of removing a memorial exceed the benefits. This mandatrophy diagnosis prevents misclassification as a functioning scaffold or rope; the high theater ratio and lack of beneficiaries confirm piton status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commemorative_husk_reading_contest,
    'Does the Aneyoshi stone currently function as a commemorative heritage object with no behavioral constraint on land use, or as a live disaster-avoidance rule?',
    'Longitudinal ethnographic observation of building-permit decisions and household relocation patterns in Aneyoshi, compared against villages lacking comparable stones.',
    'If the stone is shown to still constrain building location decisions, this reading''s high-extraction classification collapses and the constraint reverts to a low-extraction coordination mechanism under the behavioral competence reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commemorative_husk_reading_contest, conceptual, 'Kernel contest between commemorative husk and live rule readings').

omega_variable(
    survival_attribution_to_luck,
    'Was the survival of Aneyoshi village in the 2011 tsunami attributable to compliance with the stone''s warning, or to independent factors such as geography, early warning, or chance?',
    'Geospatial inundation mapping relative to the stone''s inscribed boundary, coupled with oral-history documentation of pre-2011 relocation decisions.',
    'Causal attribution to the stone would weaken the commemorative husk reading''s empirical foundation and strengthen the behavioral competence sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_attribution_to_luck, empirical, 'Empirical basis for the stone''s operational force').

omega_variable(
    governance_substitution_ambiguity,
    'Does the stone''s heritage status actively substitute for modern land-use governance and structural mitigation budgets, or merely coexist with an independent governance gap?',
    'Comparative budget analysis of municipalities with inscribed disaster stones versus matched controls without such heritage markers.',
    'If active substitution is demonstrated, the constraint''s effective extraction is higher than memorial maintenance alone suggests; if mere coexistence, the extraction is limited to symbolic performance costs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(governance_substitution_ambiguity, empirical, 'Whether the stone pre-empts modern disaster governance investment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__commemorative_husk_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(aney_tr_t13, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 13, 0.2).
narrative_ontology:measurement(aney_tr_t26, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 26, 0.32).
narrative_ontology:measurement(aney_tr_t39, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 39, 0.45).
narrative_ontology:measurement(aney_tr_t52, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 52, 0.58).
narrative_ontology:measurement(aney_tr_t65, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 65, 0.7).
narrative_ontology:measurement(aney_tr_t78, aneyoshi_stone_commitment__commemorative_husk_reading, theater_ratio, 78, 0.82).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(aney_be_t13, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 13, 0.28).
narrative_ontology:measurement(aney_be_t26, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 26, 0.42).
narrative_ontology:measurement(aney_be_t39, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 39, 0.54).
narrative_ontology:measurement(aney_be_t52, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 52, 0.63).
narrative_ontology:measurement(aney_be_t65, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 65, 0.71).
narrative_ontology:measurement(aney_be_t78, aneyoshi_stone_commitment__commemorative_husk_reading, base_extractiveness, 78, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t0, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(aney_su_t13, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 13, 0.45).
narrative_ontology:measurement(aney_su_t26, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 26, 0.38).
narrative_ontology:measurement(aney_su_t39, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 39, 0.3).
narrative_ontology:measurement(aney_su_t52, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 52, 0.24).
narrative_ontology:measurement(aney_su_t65, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 65, 0.19).
narrative_ontology:measurement(aney_su_t78, aneyoshi_stone_commitment__commemorative_husk_reading, suppression_requirement, 78, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(aneyoshi_stone_commitment__commemorative_husk_reading, behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'Aneyoshi stone commitment' conflates two structurally distinct constraints: a live land-use rule (behavioral_competence_reading) and a commemorative heritage artifact with no operational force (commemorative_husk_reading). Their epsilon values differ by a wide margin, their stakeholder directionalities are inverted, and their founding-problem statuses are opposed. Decomposed per the Îµ-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
