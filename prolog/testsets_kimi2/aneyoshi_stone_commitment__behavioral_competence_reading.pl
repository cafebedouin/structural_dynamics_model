% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Aneyoshi Stone as Live Land-Use Rule (Behavioral Competence Reading)
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   The Aneyoshi stone is an inscribed coastal marker in Japan warning
 *   descendants not to build below its elevation. This constraint story
 *   adopts the behavioral_competence_reading of the contested kernel
 *   aneyoshi_stone_commitment: the proposition that the stone retained
 *   operational force as a land-use regulator for 78 years, with the
 *   village's survival in the 2011 tsunami serving as empirical
 *   corroboration. The constraint coordinates intergenerational disaster
 *   avoidance with negligible extraction.
 *
 * KEY AGENTS:
 *   - Aneyoshi residents: primary beneficiaries (organized/constrained) â receive survival benefit and spatial safety through intergenerational compliance
 *   - Ancestor inscribers: agenda-setters (moderate/mobile) â established the rule following prior tsunami destruction
 *   - Disaster anthropologists: analytical observers â assess whether the constraint is live regulation or decayed symbol
 *   - Modern developers: excluded parties (powerful/mobile) â would contest the constraint if present in the local domain
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__behavioral_competence_reading, 0.06).
domain_priors:suppression_score(aneyoshi_stone_commitment__behavioral_competence_reading, 0.12).
domain_priors:theater_ratio(aneyoshi_stone_commitment__behavioral_competence_reading, 0.04).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 0.04).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__behavioral_competence_reading, rope).
narrative_ontology:human_readable(aneyoshi_stone_commitment__behavioral_competence_reading, "Aneyoshi Stone as Live Land-Use Rule (Behavioral Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__behavioral_competence_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__behavioral_competence_reading, 'acd95c85-ab73-4824-ad8e-c47403b34d3d').
narrative_ontology:cs_kernel_codification('acd95c85-ab73-4824-ad8e-c47403b34d3d', fixed_text).
narrative_ontology:cs_authority_grounding('acd95c85-ab73-4824-ad8e-c47403b34d3d', lineage).
narrative_ontology:cs_reading_relation('acd95c85-ab73-4824-ad8e-c47403b34d3d', aneyoshi_stone_commitment__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('acd95c85-ab73-4824-ad8e-c47403b34d3d', foundational, ancestor_directive_retains_operational_force).
narrative_ontology:cs_axiom_status(ancestor_directive_retains_operational_force, holdable).
narrative_ontology:cs_axiom_grounding('acd95c85-ab73-4824-ad8e-c47403b34d3d', ancestor_directive_retains_operational_force, empirically_contingent).
narrative_ontology:cs_axiom('acd95c85-ab73-4824-ad8e-c47403b34d3d', foundational, compliance_explains_2011_survival).
narrative_ontology:cs_axiom_status(compliance_explains_2011_survival, holdable).
narrative_ontology:cs_axiom_grounding('acd95c85-ab73-4824-ad8e-c47403b34d3d', compliance_explains_2011_survival, empirically_contingent).
narrative_ontology:cs_reference_frame('acd95c85-ab73-4824-ad8e-c47403b34d3d', ancestor_mandated_elevation_limit).
narrative_ontology:cs_drift_state('acd95c85-ab73-4824-ad8e-c47403b34d3d', terminal_pre_2011, gap(stable, minor, true)).
narrative_ontology:cs_created_at('acd95c85-ab73-4824-ad8e-c47403b34d3d', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inhabit the coastal village of Aneyoshi and observe the ancestral stone's prohibition on building below its mark. They situate residences and communal infrastructure above the elevation line, maintaining the rule as intergenerational practice, and received the survival benefit during the 2011 tsunami.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_residents, beneficiary,
    organized, generational, constrained, local).

% Erected the inscribed warning stone following a destructive tsunami, establishing the elevation threshold and the directive intended to regulate descendant settlement patterns.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, ancestor_inscribers, agenda_setter,
    moderate, generational, mobile, local).

% Study the stone as a case of long-term institutional memory, assessing whether the inscription maintained genuine behavioral constraint over 78 years or decayed into symbolic observance.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, disaster_anthropologists, observer,
    analytical, generational, analytical, global).

% External commercial builders and government planners who might prefer lower-elevation coastal development for economic efficiency but are absent from the village's internal normative order; their influence is exerted only if they penetrate the local land-use domain.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, modern_developers, excluded,
    powerful, biographical, mobile, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates intergenerational disaster avoidance by establishing a visible, permanent elevation threshold for settlement, solving the collective knowledge-loss problem between infrequent tsunami events.
% TRANSFER_FUNCTION: Transfers spatial discipline and settlement caution from each generation to the next; the cost is foregone low-elevation development opportunity, and the benefit is collective survival.
% ABSENT_VOICES: Modern coastal developers and short-term economic planners who would prefer unrestricted lower-elevation construction are absent from the village's normative sphere; their absence allows the stone to function without contest.
% DISAPPEARANCE_RATIONALE: If the stone and its normative force vanished, intergenerational memory of the tsunami threshold would fade over time; new construction would likely encroach on lower elevations, increasing catastrophic risk and fundamentally altering the village's spatial structure and vulnerability profile.
% FOUNDING_PROBLEM: A prior catastrophic tsunami destroyed lower-elevation settlement and killed villagers; the community needed a persistent mechanism to transmit danger knowledge across generations where oral memory alone might decay or be overridden by short-term economic pressure.
% FOUNDING_PROBLEM_CORROBORATION: Geological and meteorological records confirm recurrent tsunamis in the Sanriku region, and disaster anthropology independently corroborates that oral memory often fails between long-interval events. The beneficiary community attests the origin narrative, but the underlying hazard is verified by external scientific agencies.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is very low (0.06) because the constraint collects no rents and imposes only opportunity costs on low-elevation development. Suppression is low (0.12) because persistence depends on shared memory and observed benefit rather than coercion. Theater ratio is minimal (0.04) because the stone is functionally integrated into settlement decisions, not performatively maintained. Accessibility collapse is moderate (0.35): the alternative of building lower remains physically possible but is normatively and prudentially collapsed by understood disaster risk. Resistance is negligible (0.02) because compliance correlates with survival benefit. The measurement series shows stable, slightly rising but still negligible extraction over the interval, consistent with mild modernity pressure.
 *
 * PERSPECTIVAL GAP:
 *   The resident seat experiences the constraint as protective tradition with negligible subjective cost; the ancestor seat experienced it as urgent disaster mitigation for descendants; the anthropologist seat sees a puzzle of institutional persistence. An excluded developer seat would experience the same elevation limit as an arbitrary barrier to profitable land use, but that seat is outside the constraint's current operational scope.
 *
 * DIRECTIONALITY LOGIC:
 *   Aneyoshi residents are unambiguous beneficiaries (low d) because the constraint subsidizes their survival and safety. There are no victims within the scope; the only costs are diffuse opportunity costs of foregone low-elevation construction. Excluded developers would sit at high d if they entered the scope, but they are structurally absent.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope classification is protected from mandatrophy mislabeling by the absence of extraction asymmetry: there is no concentrated beneficiary capturing rents from a payer population. The coordination function (intergenerational disaster avoidance) is genuine, the costs are symmetrically borne, and the low theater ratio confirms the absence of performative maintenance without function. If the constraint had decayed into a husk, the theater ratio would rise and the type would drift toward piton; the stable low metrics guard against this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest__commemorative_husk,
    'Does the stone''s persistence across 78 years represent a live behavioral constraint on land use, or a commemorative husk observed ritually without locational effect?',
    'Archaeological survey of building foundations relative to the stone mark across the interval; if structures were actively placed above the line and avoided below, the behavioral competence reading holds; if encroachment occurred and the stone was honored only ceremonially, the husk reading holds.',
    'If the husk reading is correct, the constraint''s epsilon is higher than measured (theater without function) and the type would drift toward piton or inertial scaffold; if this reading holds, the rope classification is sustained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest__commemorative_husk, conceptual, 'Uncertainty whether the stone functioned as active regulation or decayed memorial').

omega_variable(
    causal_attribution_2011_survival,
    'Was the village''s survival in the 2011 tsunami causally attributable to stone-directed land-use compliance rather than to random variation in wave height, evacuation timing, or ad hoc individual decisions?',
    'Comparative analysis of casualty rates and settlement elevation in neighboring villages lacking equivalent warning stones; building-registry verification of Aneyoshi house locations relative to the mark.',
    'If survival was not causally linked to the stone''s land-use rule, the constraint''s coordination function is weakened and its persistence becomes indistinguishable from theater or post-hoc rationalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_attribution_2011_survival, empirical, 'Causal attribution of survival to the stone''s regulatory function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__behavioral_competence_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t0, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(aney_tr_t13, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 13, 0.02).
narrative_ontology:measurement(aney_tr_t26, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 26, 0.03).
narrative_ontology:measurement(aney_tr_t39, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 39, 0.03).
narrative_ontology:measurement(aney_tr_t52, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 52, 0.04).
narrative_ontology:measurement(aney_tr_t65, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 65, 0.04).
narrative_ontology:measurement(aney_tr_t78, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 78, 0.05).

% Extraction over time
narrative_ontology:measurement(aney_be_t0, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(aney_be_t13, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 13, 0.05).
narrative_ontology:measurement(aney_be_t26, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 26, 0.06).
narrative_ontology:measurement(aney_be_t39, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 39, 0.06).
narrative_ontology:measurement(aney_be_t52, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 52, 0.06).
narrative_ontology:measurement(aney_be_t65, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 65, 0.07).
narrative_ontology:measurement(aney_be_t78, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 78, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_stone_commitment__behavioral_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__behavioral_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__behavioral_competence_reading, commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'Aneyoshi Stone' conflates two structurally distinct constraints: a live intergenerational land-use rule (this reading) and a decayed commemorative marker (commemorative_husk_reading). Their epsilon values, stakeholder configurations, and directionality profiles differ. This decomposition follows the Îµ-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
