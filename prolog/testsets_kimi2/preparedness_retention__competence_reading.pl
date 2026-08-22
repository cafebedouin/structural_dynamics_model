% ============================================================================
% CONSTRAINT STORY: preparedness_retention__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__competence_reading, []).

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
 *   constraint_id: preparedness_retention__competence_reading
 *   human_readable: Preparedness as Live Exercised Knowledge (Competence Reading)
 *   domain: disaster_preparedness/institutional_memory/governance
 *
 * SUMMARY:
 *   This constraint story instantiates the competence_reading of the
 *   preparedness_retention kernel. It treats preparedness not as static
 *   stockpiling or ceremonial performance, but as live exercised knowledge
 *   maintained through recurrent drills and inspections. The beneficiary is
 *   population safety; the cost is fiscal and temporal, borne by participants
 *   and taxpayers, but the reading structurally positions all parties as net
 *   beneficiaries of maintained operational capacity. The constraint
 *   coordinates resource allocation toward skill retention that would
 *   otherwise atrophy. Sibling readings (husk_reading, hybrid_reading)
 *   contest this framing but are structurally external to this constraint.
 *
 * KEY AGENTS:
 *   - emergency_management_agencies: agenda_setter (institutional/constrained) â designs and administers drill programs
 *   - response_personnel: beneficiary (organized/constrained) â maintains skills through participation
 *   - general_population: beneficiary (powerless/constrained) â enjoys reduced disaster risk
 *   - fiscal_conservatives: excluded (organized/analytical) â challenges opportunity cost
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__competence_reading, 0.18).
domain_priors:suppression_score(preparedness_retention__competence_reading, 0.1).
domain_priors:theater_ratio(preparedness_retention__competence_reading, 0.14).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, theater_ratio, 0.14).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__competence_reading, rope).
narrative_ontology:human_readable(preparedness_retention__competence_reading, "Preparedness as Live Exercised Knowledge (Competence Reading)").
narrative_ontology:topic_domain(preparedness_retention__competence_reading, "disaster_preparedness/institutional_memory/governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__competence_reading, 'baf4159e-fc8b-45ea-adc5-3f481f2f4235').
narrative_ontology:cs_kernel_codification('baf4159e-fc8b-45ea-adc5-3f481f2f4235', formalized).
narrative_ontology:cs_authority_grounding('baf4159e-fc8b-45ea-adc5-3f481f2f4235', practice).
narrative_ontology:cs_interpretation_layer_present('baf4159e-fc8b-45ea-adc5-3f481f2f4235').
narrative_ontology:cs_reading_relation('baf4159e-fc8b-45ea-adc5-3f481f2f4235', preparedness_retention__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('baf4159e-fc8b-45ea-adc5-3f481f2f4235', preparedness_retention__hybrid_reading, influences).
narrative_ontology:cs_axiom('baf4159e-fc8b-45ea-adc5-3f481f2f4235', foundational, preparedness_requires_exercise).
narrative_ontology:cs_axiom_status(preparedness_requires_exercise, holdable).
narrative_ontology:cs_axiom_grounding('baf4159e-fc8b-45ea-adc5-3f481f2f4235', preparedness_requires_exercise, empirically_contingent).
narrative_ontology:cs_axiom('baf4159e-fc8b-45ea-adc5-3f481f2f4235', secondary, operational_competence_measurable).
narrative_ontology:cs_axiom_status(operational_competence_measurable, holdable).
narrative_ontology:cs_axiom_grounding('baf4159e-fc8b-45ea-adc5-3f481f2f4235', operational_competence_measurable, instrumental).
narrative_ontology:cs_reference_frame('baf4159e-fc8b-45ea-adc5-3f481f2f4235', live_exercised_competence).
narrative_ontology:cs_drift_state('baf4159e-fc8b-45ea-adc5-3f481f2f4235', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('baf4159e-fc8b-45ea-adc5-3f481f2f4235', '').
narrative_ontology:cs_kernel_id(preparedness_retention__competence_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, response_personnel).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, general_population).
narrative_ontology:constraint_vindicates(preparedness_retention__competence_reading, live_exercise_effectiveness).
narrative_ontology:constraint_vindicates(preparedness_retention__competence_reading, institutional_memory_validity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, schedules, and evaluates drills and inspections; maintains formal protocols; allocates budgets toward exercise programs; accountable to legislative oversight for demonstrated operational readiness.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, emergency_management_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Participates in recurrent drills and inspections; maintains perishable operational skills; bears time and effort costs but gains current competence, inter-agency familiarity, and reduced personal risk during actual events.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, response_personnel, beneficiary,
    organized, biographical, constrained, regional).

% Does not directly participate but relies on maintained responder competence for disaster outcomes; enjoys lower aggregate risk because response capacity is rehearsed rather than theoretical.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, general_population, beneficiary,
    powerless, biographical, constrained, national).

% Contests sustained preparedness spending in low-disaster periods; argues opportunity cost is excessive; excluded from operational design tables but active in budgetary politics and public audit discourse.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, fiscal_conservatives, excluded,
    organized, generational, analytical, national).

% Audits preparedness budgets and exercise outcomes; evaluates whether expenditures correlate with competence metrics rather than ceremonial compliance; can alter funding and mandates.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, legislative_oversight, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains distributed operational competence that would otherwise atrophy due to infrequent real-world activation; coordinates resource allocationâtime, personnel, equipmentâtoward skill retention across disparate response organizations.
% TRANSFER_FUNCTION: Moves public funds and personnel time into recurrent exercise and inspection cycles; transfers maintained competence from training institutions and drill environments to operational response units.
% ABSENT_VOICES: Fiscal conservatives who view sustained spending as wasteful absent frequent disasters; communities in low-risk zones who bear tax cost without perceived benefit; retired response personnel who could attest post-career skill atrophy but are outside current institutional feedback loops.
% DISAPPEARANCE_RATIONALE: Without recurrent exercise, operational skills atrophy, inter-agency coordination protocols decay, and institutional memory fragments; the next disaster would encounter ad-hoc response rather than rehearsed capacity.
% FOUNDING_PROBLEM: Perishable operational skills and coordination protocols decay when not exercised; rare disasters do not provide sufficient real-world practice to maintain readiness across responder organizations.
% FOUNDING_PROBLEM_CORROBORATION: Independent disaster sociology and organizational studies (e.g., Turner, Pidgeon, Weick) attest to the atrophy of safety culture and response capacity without recurrent practice; post-disaster inquiries repeatedly identify skill degradation and protocol unfamiliarity as contributing factors. These sources are outside the immediate beneficiary set of emergency management agencies.
narrative_ontology:disappearance_verdict(preparedness_retention__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_retention__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__competence_reading, 0.18, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__competence_reading_tests).
:- end_tests(preparedness_retention__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the constraint moves resources into competence retention rather than rent capture; fiscal costs are diffuse and reciprocated by safety benefits. Suppression is low (0.10) because participation, while often mandatory, is not coercively enforced against resistant partiesâalternatives (non-participation, exit from service) exist, and participants are net beneficiaries. Theater ratio is low (0.14) per the reading's defining claim of low ceremony-to-competence ratio. Accessibility collapse is moderate-low (0.25): alternatives (ad-hoc response) exist but are inferior. Resistance is low (0.12) because the coordination function is widely accepted. The temporal series show stable, low-extraction operation with minor fluctuation.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (emergency management agencies) experiences the constraint as mission fulfillment and legitimate resource use; the beneficiary seats (response personnel, general population) experience it as maintained safety and skill. The excluded fiscal-conservative seat would experience it as wasteful spending, but because this reading treats the constraint as efficient coordination, the structural derivation does not produce a high-directionality victim. Seat divergence is therefore narrow in this reading: all seated agents are either beneficiaries or agenda-setters. A sibling husk reading would produce strong divergence by introducing payer seats experiencing ceremonial extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   No victims are declared in this reading, so directionality for all named agents clusters near the beneficiary end. Emergency management agencies are agenda-setters with constrained exit (political mandate) but are not extractors; their role is administrative. Response personnel are direct beneficiaries (skill retention). The general population is the diffuse beneficiary (safety). The small fiscal cost is not assigned to a specific victim stakeholder because the reading treats it as investment rather than extraction. No directionality overrides are needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâperishable operational skills in rare-event environmentsâis corroborated as live by independent organizational research and post-disaster inquiries. The low theater ratio and stable extraction profile prevent misclassification as piton (degraded performance) or snare (coercive extraction). If the founding problem were dead and the constraint persisted with high theater, it would compute as piton; here, the problem remains live and the metrics reflect genuine coordination, supporting the rope claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    husk_alternative_framing,
    'Does the same institutional practice of drills and inspections function as competence preservation or as memorial performance, and what observable distinguishes the two?',
    'Outcome-based audit: correlate drill design fidelity and inspection stringency with actual disaster response performance metrics, controlling for event severity.',
    'If performance correlation is weak, the competence reading reclassifies toward husk or piton; if strong, it remains rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(husk_alternative_framing, conceptual, 'Alternative reading of the same practices as ceremonial rather than competence-preserving.').

omega_variable(
    resource_allocation_efficiency,
    'Does resource allocation for preparedness optimize for skill retention, or does it drift toward ceremonial compliance and budget capture?',
    'Budgetary analysis tracing expenditure categories to competence metrics versus administrative overhead and ritualized exercise components.',
    'Over-investment without competence return would introduce a diffuse payer cost and shift classification toward tangled_rope; efficient allocation supports the rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_allocation_efficiency, empirical, 'Whether preparedness spending tracks competence or bureaucratic expansion.').

omega_variable(
    practice_vs_expertise_authority,
    'Is the authority of this preparedness constraint grounded in the practitioner community''s continuous action (practice) or in credentialed expertise and organizational standards?',
    'Trace legitimacy claims in doctrine: whether protocols are revised through professional-expert peer review or through practitioner after-action consensus.',
    'If grounded in practice, the constraint is more resilient to top-down mandate but vulnerable to craft mythology; if grounded in expertise, it is more auditable but vulnerable to credentialism. Classification as rope is stable under both, but the directionality of the agenda_setter shifts slightly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practice_vs_expertise_authority, conceptual, 'Alternative authority grounding for the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__competence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_comp_tr_t0, preparedness_retention__competence_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(prep_comp_tr_t10, preparedness_retention__competence_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(prep_comp_tr_t20, preparedness_retention__competence_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement(prep_comp_tr_t30, preparedness_retention__competence_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement(prep_comp_tr_t40, preparedness_retention__competence_reading, theater_ratio, 40, 0.13).
narrative_ontology:measurement(prep_comp_tr_t50, preparedness_retention__competence_reading, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(prep_comp_be_t0, preparedness_retention__competence_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(prep_comp_be_t10, preparedness_retention__competence_reading, base_extractiveness, 10, 0.16).
narrative_ontology:measurement(prep_comp_be_t20, preparedness_retention__competence_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement(prep_comp_be_t30, preparedness_retention__competence_reading, base_extractiveness, 30, 0.17).
narrative_ontology:measurement(prep_comp_be_t40, preparedness_retention__competence_reading, base_extractiveness, 40, 0.16).
narrative_ontology:measurement(prep_comp_be_t50, preparedness_retention__competence_reading, base_extractiveness, 50, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_retention__competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__competence_reading, resource_allocation).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__husk_reading).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the preparedness_retention kernel, decomposed per the epsilon-invariance principle. The siblings are peer readings of the same institutional practices, not causal dependents; they are linked here to satisfy constraint-family linkage requirements. Each reading carries a distinct epsilon and stakeholder structure because the same natural-language label ('preparedness') conflates structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
