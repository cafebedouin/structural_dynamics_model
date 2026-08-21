% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__bodily_autonomy_primary, []).

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
 *   constraint_id: public_health_mandate_authority__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy as Primary in Public Health Mandates
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'bodily_autonomy_primary' reading
 *   of the 'public_health_mandate_authority' kernel. From this perspective,
 *   any public health mandate requiring non-consensual medical intervention
 *   is a categorical violation of individual bodily sovereignty. No
 *   collective benefit, however great, can justify such an infringement. The
 *   constraint is framed as a snare because it is seen as purely extractive,
 *   coercing individuals into medical actions against their will, with the
 *   'coordination story' of public health benefit serving as cover for this
 *   extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, 0.9).
domain_priors:suppression_score(public_health_mandate_authority__bodily_autonomy_primary, 0.85).
domain_priors:theater_ratio(public_health_mandate_authority__bodily_autonomy_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, extractiveness, 0.9).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(public_health_mandate_authority__bodily_autonomy_primary, "Bodily Autonomy as Primary in Public Health Mandates").
narrative_ontology:topic_domain(public_health_mandate_authority__bodily_autonomy_primary, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__bodily_autonomy_primary, '2512da67-9de9-4144-8f77-0fe2e6f8643c').
narrative_ontology:cs_kernel_codification('2512da67-9de9-4144-8f77-0fe2e6f8643c', formalized).
narrative_ontology:cs_authority_grounding('2512da67-9de9-4144-8f77-0fe2e6f8643c', lineage).
narrative_ontology:cs_interpretation_layer_present('2512da67-9de9-4144-8f77-0fe2e6f8643c').
narrative_ontology:cs_reading_relation('2512da67-9de9-4144-8f77-0fe2e6f8643c', public_health_mandate_authority__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('2512da67-9de9-4144-8f77-0fe2e6f8643c', public_health_mandate_authority__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('2512da67-9de9-4144-8f77-0fe2e6f8643c', foundational, bodily_autonomy_absolute).
narrative_ontology:cs_axiom_status(bodily_autonomy_absolute, holdable).
narrative_ontology:cs_axiom_grounding('2512da67-9de9-4144-8f77-0fe2e6f8643c', bodily_autonomy_absolute, deontological).
narrative_ontology:cs_axiom('2512da67-9de9-4144-8f77-0fe2e6f8643c', foundational, no_collective_justification_for_invasion).
narrative_ontology:cs_axiom_status(no_collective_justification_for_invasion, holdable).
narrative_ontology:cs_axiom_grounding('2512da67-9de9-4144-8f77-0fe2e6f8643c', no_collective_justification_for_invasion, deontological).
narrative_ontology:cs_reference_frame('2512da67-9de9-4144-8f77-0fe2e6f8643c', absolute_individual_sovereignty).
narrative_ontology:cs_drift_state('2512da67-9de9-4144-8f77-0fe2e6f8643c', contemporary_public_health_crisis, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('2512da67-9de9-4144-8f77-0fe2e6f8643c', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, public_health_authorities).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, individuals_seeking_medical_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who, by choice or conviction, do not comply with public health mandates (e.g., vaccination requirements) and face social, economic, or legal penalties as a result. They experience the mandate as a direct violation of their bodily autonomy.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, unvaccinated_individuals, payer,
    powerless, immediate, trapped, national).

% Governmental bodies responsible for public health, which issue and enforce mandates. From this reading's perspective, they benefit from the power to impose interventions, even if that power is seen as illegitimate.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% A broader group of citizens who prioritize individual control over medical decisions and view any state-imposed intervention as an infringement on fundamental rights. They bear the cost of defending this principle against collective claims.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, individuals_seeking_medical_autonomy, payer,
    moderate, biographical, identity_locked, national).

% Individuals whose health status makes them vulnerable to infectious diseases. This reading explicitly excludes them from the victim set of the *mandate* because it rejects the premise that their protection justifies bodily invasion of others.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, immunocompromised_individuals, excluded,
    powerless, biographical, trapped, local).

% Advocates who believe that collective public health is paramount and justifies mandates. They are not victims of this constraint (the mandate as a violation) and are not direct beneficiaries of the violation itself, but rather of the *idea* of collective action.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_primary_advocates, observer,
    organized, generational, analytical, global).

% Legal professionals who argue for a strict interpretation of individual rights, particularly bodily autonomy, against state power. They analyze the mandate's legality and ethical implications from this specific rights-based perspective.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, constitutional_lawyers_autonomy_focused, observer,
    powerful, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None, as this reading categorically rejects the premise that any collective benefit can legitimately justify non-consensual medical intervention, thus denying a valid coordination function for mandates.
% TRANSFER_FUNCTION: Transfers the fundamental right to bodily self-determination from individuals to the state or collective, imposing non-consensual medical interventions and associated social or economic costs.
% ABSENT_VOICES: Individuals whose deeply held religious, philosophical, or ethical beliefs prohibit specific medical interventions, or those who prioritize individual liberty above all collective goods, are often marginalized or silenced in public health discourse that assumes a collective good justification.
% DISAPPEARANCE_RATIONALE: If public health mandates vanished overnight, individuals would immediately regain full, unencumbered control over their medical decisions. Public health strategies would be forced to rely exclusively on voluntary measures, education, and incentives, fundamentally altering the state's role in health interventions and the social contract around collective health.
% FOUNDING_PROBLEM: The perceived problem of collective vulnerability to disease and the need for population-level protection, which this reading argues is not a legitimate justification for infringing on individual bodily autonomy.
% FOUNDING_PROBLEM_CORROBORATION: Advocates for individual liberty and bodily autonomy attest that the founding problem, while potentially real, does not justify the means of coercive mandates. Public health authorities and many medical professionals contest this, asserting the necessity of collective action for public safety; legislative hearings and public health literature from outside the benefiting parties support the necessity of mandates.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__bodily_autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(public_health_mandate_authority__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__bodily_autonomy_primary, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.9) reflects the view that the loss of bodily autonomy is a severe, non-negotiable cost. Suppression (0.85) is high because mandates are enforced through various penalties (e.g., job loss, travel restrictions), actively suppressing the choice of non-compliance. Theater ratio is low (0.1) because the violation is considered direct and functional, not performative; the 'public health' justification is seen as a cover, not a genuine, albeit flawed, function. Resistance is high (0.9) due to the strong opposition this perspective generates. Accessibility collapse is high (0.8) because the mandate effectively removes the option of non-intervention without penalty.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between this reading's absolute prioritization of individual bodily autonomy and other readings that balance individual rights against collective welfare. From the perspective of public health authorities, mandates are a necessary coordination mechanism; from this reading, they are an illegitimate, coercive extraction. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Unvaccinated individuals and those seeking medical autonomy are the primary targets (payers), bearing the direct costs of bodily invasion and associated penalties. Public health authorities are the beneficiaries, gaining the power to enforce interventions, even if this reading views that power as illegitimate. Immunocompromised individuals are explicitly excluded from the victim set, as this reading rejects the premise that their protection justifies infringing on others' autonomy. Public health primary advocates are observers, as they are not directly coerced by this constraint (the mandate as a violation).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_collective_benefit,
    'Does a demonstrable collective benefit (e.g., herd immunity, reduced healthcare burden) ever legitimately override individual bodily autonomy in the context of public health mandates?',
    'Philosophical and legal consensus on the limits of state power in liberal democracies, potentially informed by empirical data on the severity and inevitability of collective harm without intervention.',
    'If collective benefit is deemed to legitimately override autonomy under certain conditions, this constraint''s extractiveness would be re-evaluated downward, potentially shifting its classification from snare to tangled_rope or even rope, depending on the proportionality. If not, its snare classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_collective_benefit, conceptual, 'The fundamental conceptual disagreement on the balance between individual rights and collective good in public health.').

omega_variable(
    definition_of_non_consensual,
    'What constitutes ''non-consensual'' in the context of public health mandates? Does social pressure, economic disadvantage, or the threat of losing privileges (e.g., employment, travel) count as coercion equivalent to physical force?',
    'Legal precedent and ethical frameworks defining coercion and informed consent in medical and public policy contexts. Empirical studies on the perceived voluntariness of compliance under various mandate structures.',
    'If ''non-consensual'' is narrowly defined (e.g., only direct physical force), the measured suppression and extractiveness of this constraint would be lower. If broadly defined to include indirect pressures, the current high values are reinforced, strengthening the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_non_consensual, empirical, 'Ambiguity in the scope of ''non-consensual'' action under public health mandates.').

omega_variable(
    kernel_reading_contest,
    'Is the ''bodily_autonomy_primary'' reading of public health mandate authority structurally foreclosed by the ''public_health_primary'' or ''proportionality_reading'' within a single coherent legal/ethical framework, or do these readings merely coexist as competing perspectives?',
    'Analysis of legal and philosophical arguments for internal consistency and mutual exclusivity of the core axioms across readings. Judicial rulings that explicitly reject one reading in favor of another as a matter of fundamental principle.',
    'If this reading is truly foreclosed, it implies a fundamental incompatibility that cannot be resolved by compromise, reinforcing the deep structural conflict. If they merely coexist, it suggests a political rather than logical impasse, potentially allowing for future synthesis or negotiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The structural relationship between this reading and its siblings within the ''public_health_mandate_authority'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__bodily_autonomy_primary, 0, 3).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(publ_tr_t1, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 1, 0.1).
narrative_ontology:measurement(publ_tr_t2, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 2, 0.1).
narrative_ontology:measurement(publ_tr_t3, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 3, 0.1).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(publ_be_t1, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 1, 0.88).
narrative_ontology:measurement(publ_be_t2, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 2, 0.9).
narrative_ontology:measurement(publ_be_t3, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 3, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(publ_su_t1, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 1, 0.83).
narrative_ontology:measurement(publ_su_t2, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 2, 0.85).
narrative_ontology:measurement(publ_su_t3, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 3, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__bodily_autonomy_primary, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
