% ============================================================================
% CONSTRAINT STORY: competence_occupation__hybrid_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__hybrid_occupation, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: competence_occupation__hybrid_occupation
 *   human_readable: Multi-Mechanism Competence Maintenance Regime (Hybrid Occupation Reading)
 *   domain: organizational/safety/high_reliability_operations
 *
 * SUMMARY:
 *   In high-reliability domains, regulators and organizations have concluded
 *   that no single competence-maintenance mechanism (simulation, periodic
 *   refresher training, procedural reinforcement, or line audit) is
 *   sufficient on its own to keep operators safely competent between formal
 *   qualifications. This reading of the competence-occupation kernel holds
 *   that ALL FOUR mechanisms, run continuously and in combination, are
 *   required — and that because no research consensus fixes an optimal
 *   configuration or weighting among them, the training apparatus is
 *   structurally open-ended: new findings, incidents, or vendor proposals can
 *   always justify adding another layer rather than substituting for an
 *   existing one. This is one of three competing readings of the same
 *   underlying kernel (competence occupation): the sibling readings hold that
 *   simulation alone suffices (simulation_sufficiency) or that only real
 *   incidents provide authentic occupation of the competence kernel
 *   (real_incident_necessity). This story authors ONLY the hybrid_occupation
 *   reading, with its own ε, its own beneficiary/victim structure, and its
 *   own claimed type — it does not average across, or take a position
 *   resolving, the sibling readings.
 *
 * KEY AGENTS:
 *   - frontline_operators: primary payer (powerless/constrained) — bears the compounding time and certification burden of all four mechanisms
 *   - shift_supervisors: dual payer/agenda_setter (moderate/constrained) — administers the regime locally without designing it
 *   - training_program_vendors: primary beneficiary (organized/arbitrage) — sells continuously expanding modules under cover of the no-consensus problem
 *   - compliance_and_audit_departments: beneficiary/agenda_setter (institutional/mobile) — institutional relevance tied to configuration remaining unsettled
 *   - senior_operations_leadership: beneficiary (institutional/mobile) — uses the full regime as liability insurance
 *   - safety_researchers: analytical observer (analytical/analytical) — studies the underlying decay/transfer questions the regime claims to answer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, 0.52).
domain_priors:suppression_score(competence_occupation__hybrid_occupation, 0.44).
domain_priors:theater_ratio(competence_occupation__hybrid_occupation, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, extractiveness, 0.52).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__hybrid_occupation, tangled_rope).
narrative_ontology:human_readable(competence_occupation__hybrid_occupation, "Multi-Mechanism Competence Maintenance Regime (Hybrid Occupation Reading)").
narrative_ontology:topic_domain(competence_occupation__hybrid_occupation, "organizational/safety/high_reliability_operations").

domain_priors:requires_active_enforcement(competence_occupation__hybrid_occupation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__hybrid_occupation, '2bacacde-e454-4433-8668-79822b23727a').
narrative_ontology:cs_kernel_codification('2bacacde-e454-4433-8668-79822b23727a', distributed).
narrative_ontology:cs_authority_grounding('2bacacde-e454-4433-8668-79822b23727a', distributed).
narrative_ontology:cs_reading_relation('2bacacde-e454-4433-8668-79822b23727a', competence_occupation__simulation_sufficiency, influences).
narrative_ontology:cs_reading_relation('2bacacde-e454-4433-8668-79822b23727a', competence_occupation__real_incident_necessity, coexists_with).
narrative_ontology:cs_axiom('2bacacde-e454-4433-8668-79822b23727a', foundational, no_single_mechanism_suffices).
narrative_ontology:cs_axiom_status(no_single_mechanism_suffices, holdable).
narrative_ontology:cs_axiom_grounding('2bacacde-e454-4433-8668-79822b23727a', no_single_mechanism_suffices, empirically_contingent).
narrative_ontology:cs_axiom('2bacacde-e454-4433-8668-79822b23727a', secondary, configuration_optimality_is_an_open_research_problem).
narrative_ontology:cs_axiom_status(configuration_optimality_is_an_open_research_problem, holdable).
narrative_ontology:cs_axiom_grounding('2bacacde-e454-4433-8668-79822b23727a', configuration_optimality_is_an_open_research_problem, empirically_contingent).
narrative_ontology:cs_reference_frame('2bacacde-e454-4433-8668-79822b23727a', single_mechanism_qualification_standard).
narrative_ontology:cs_drift_state('2bacacde-e454-4433-8668-79822b23727a', contemporary_multi_mechanism_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('2bacacde-e454-4433-8668-79822b23727a', '').
narrative_ontology:cs_kernel_id(competence_occupation__hybrid_occupation, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, training_program_vendors).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, compliance_and_audit_departments).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, senior_operations_leadership).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, frontline_operators).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, shift_supervisors).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, training_budget_departments).
narrative_ontology:constraint_vindicates(competence_occupation__hybrid_occupation, no_single_mechanism_suffices_for_competence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Must complete simulation sessions, refresher courses, procedural reinforcement drills, and pass line audits on overlapping but non-aligned schedules, on top of full operational duty. Cannot decline any single mechanism without risking certification lapse, yet no configuration of the four is presented as sufficient on its own — each new incident or audit finding adds another layer rather than replacing an existing one. Exit means leaving the licensed occupation entirely.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, frontline_operators, payer,
    powerless, biographical, constrained, local).

% Responsible for scheduling and documenting all four mechanisms for their crews, absorbing the coordination burden between simulation vendors, refresher calendars, procedural checklists, and audit visits. They administer the regime locally but do not set its design, and are held accountable when any single mechanism's paperwork is incomplete regardless of whether competence was actually maintained.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, shift_supervisors, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(competence_occupation__hybrid_occupation, shift_supervisors, agenda_setter).

% Sell simulation platforms, refresher curricula, and procedural-reinforcement software as separate, continuously billed products. Because no consensus configuration exists, they can always propose an additional module or an enhanced version of an existing one as the missing piece that finally closes the competence gap, without ever being required to demonstrate that the combination they sell is sufficient.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, training_program_vendors, beneficiary,
    organized, generational, arbitrage, national).

% Design and administer the line-audit component and set documentation standards across all four mechanisms. Their institutional relevance depends on the absence of a settled, minimal-sufficiency standard; a converged, cheap configuration would shrink their audit mandate and headcount justification.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, compliance_and_audit_departments, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__hybrid_occupation, compliance_and_audit_departments, agenda_setter).

% Can point to the full multi-mechanism regime as evidence of due diligence after any incident, regardless of whether the specific mechanisms implicated were the ones actually deficient. The redundancy itself functions as liability insurance for leadership, independent of its marginal safety contribution.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, senior_operations_leadership, beneficiary,
    institutional, generational, mobile, national).

% Fund all four mechanisms simultaneously because no research consensus permits dropping any one without perceived liability exposure. Budget requests to consolidate or trial a reduced configuration are routinely rejected as too risky, so costs accumulate additively rather than being optimized.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, training_budget_departments, payer,
    moderate, biographical, constrained, national).

% Study skill-decay curves, simulation transfer validity, and audit-behavior data across organizations, attempting to establish which mechanisms or combinations actually predict incident reduction. Their findings are contested and slow to converge, which is itself part of what sustains the multi-mechanism status quo.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, safety_researchers, observer,
    analytical, civilizational, analytical, global).

% Bear the consequences of any actual competence failure (accidents, service disruptions) but have no voice in how the training regime is configured, funded, or audited. They would presumably want whatever configuration actually minimizes incidents at lowest cost, but that question is never put to them.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, downstream_public_and_customers, excluded,
    powerless, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__hybrid_occupation, diffuse).
narrative_ontology:fixing_cost_class(competence_occupation__hybrid_occupation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely reduces catastrophic-failure risk by exercising competence through multiple partially-independent channels, so that a weakness in any single mechanism (e.g., simulation not capturing a real failure mode) is caught by another (e.g., a line audit).
% TRANSFER_FUNCTION: Moves operator time, supervisor coordination labor, and training budget toward vendors, compliance departments, and leadership's liability-insurance function, in exchange for a redundancy whose marginal safety contribution beyond some subset of the mechanisms is not established.
% ABSENT_VOICES: Frontline operators bearing the cumulative time burden have informal but not decision-making input into mechanism design; the downstream public who would benefit from an optimized (cheaper, equally safe) configuration has no voice at all in what is authored as a purely internal training-design question.
% DISAPPEARANCE_RATIONALE: Compliance departments and senior leadership would say the world rearranges catastrophically (uncontrolled skill decay, liability exposure). Safety researchers and some frontline operators would say a leaner, evidence-selected subset of the four mechanisms would likely maintain most of the safety benefit at substantially lower burden — meaning the FULL four-mechanism regime, as opposed to some reduced regime, could disappear without the underlying safety outcome changing much. The dispute over which reading is correct is precisely the unresolved configuration question the constraint names.
% FOUNDING_PROBLEM: Real catastrophic failures in high-reliability domains (aviation, nuclear, chemical process) revealed that competence certified once at qualification does not persist — skills decay, procedures drift, and simulators cannot reproduce every real failure mode, so a single mechanism was empirically shown insufficient.
% FOUNDING_PROBLEM_CORROBORATION: Safety researchers outside the training-vendor and compliance ecosystem corroborate that skill decay and simulation-transfer gaps are real and that some ongoing multi-channel exercise is warranted; they do NOT corroborate that the specific four-mechanism configuration currently mandated is the minimal or optimal one — that stronger claim is asserted mainly by the parties who administer and sell the mechanisms.
narrative_ontology:disappearance_verdict(competence_occupation__hybrid_occupation, contested).
narrative_ontology:founding_problem_status(competence_occupation__hybrid_occupation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__hybrid_occupation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_occupation__hybrid_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__hybrid_occupation, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__hybrid_occupation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_occupation__hybrid_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at a moderate 0.52 by interval end: there is a genuine, well-evidenced coordination function (skills decay, single mechanisms have known blind spots), but the absence of a sufficiency consensus is exploited by vendors and compliance bodies to add mechanisms rather than optimize them, producing a slow additive cost escalation captured in the rising base_extractiveness series. Theater ratio rises from 0.22 to 0.40 as documentation-for-audit's-sake grows relative to mechanisms' actual incremental safety contribution — this is the Goodhart signature of a metric-substitution drift, not yet dominant but clearly trending. Suppression is moderate (0.44) rather than high: operators are not coerced by force, but certification lapse is a real structural cost that forecloses most alternatives, and the regime is defended by institutional actors whose relevance depends on it staying unresolved.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting compliance and leadership seats, the multi-mechanism regime looks like prudent, evidence-driven redundancy — exactly what a due-diligence framework should look like after past catastrophic failures. From the frontline operator and supervisor seats, the same structure looks like an ever-growing, unprioritized checklist whose marginal safety value per additional hour of burden is never demonstrated before the next mechanism is added. The engine's per-seat computation should reflect this: agenda-setter/beneficiary seats trend toward rope-like readings of the same data that payer seats read as extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   Vendors, compliance/audit departments, and senior leadership are structural beneficiaries: none of them personally executes the drills, but all either collect revenue, collect institutional mandate, or collect liability cover from the regime's persistence and expansion. Frontline operators and shift supervisors are structural targets: they absorb the compounding time burden and bear certification risk, with constrained exit (leaving the licensed occupation). Training budget departments are also targets in a resource sense — they fund an ever-expanding basket without the standing to consolidate it. Safety researchers sit outside the directionality axis as analytical observers whose findings could, in principle, resolve the ambiguity but currently do not.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — single-mechanism competence certification failing to catch real-world skill decay and simulation-transfer gaps — remains genuinely live per outside corroboration (safety researchers), which is why this reading is NOT classified as a pure snare or piton: there is a real coordination function underneath. But the specific four-mechanism configuration currently mandated has outrun what the founding problem demands, sustained instead by parties (vendors, compliance departments, leadership) whose interests are served by non-convergence. The tangled_rope classification captures exactly this: a genuine coordination core (preventing decay) fused with an asymmetric extraction layer (compounding, unoptimized mechanism accumulation) that requires active enforcement (certification-lapse consequences) to hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_configuration_existence,
    'Does an optimal (or even a dominant, cost-minimizing) configuration of the four mechanisms actually exist and remain undiscovered, or is ''no consensus on optimal configuration'' itself a structurally maintained condition that benefits parties who profit from non-convergence?',
    'A large-scale, independently funded (non-vendor, non-compliance-department) comparative study varying mechanism subsets and measuring incident rates and skill-decay curves directly, with results made binding on regulatory configuration requirements.',
    'If an optimal reduced configuration is found and adopted, the regime reclassifies toward scaffold (transitional toward a settled standard) or even rope; if no such configuration is discoverable in principle (genuine irreducible complexity), the tangled_rope reading is reinforced as structurally correct rather than as an artifact of vendor/compliance incentives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(optimal_configuration_existence, empirical, 'Whether the lack of configuration consensus is a genuine open research problem or a maintained condition serving non-coordination interests.').

omega_variable(
    kernel_reading_partition_location,
    'Where exactly does the disagreement between the hybrid_occupation, simulation_sufficiency, and real_incident_necessity readings actually live — is it a disagreement about EMPIRICAL sufficiency (which mechanisms causally prevent incidents) or about EVIDENTIARY standard (what counts as proof of competence, independent of cost)?',
    'Structured elicitation of each reading''s proponents to identify whether their disagreement would resolve given identical incident data, or persists due to different standards for what ''occupying the competence kernel'' requires.',
    'If the disagreement is purely empirical, resolving the causal question in omega above would likely resolve all three readings'' practical convergence; if it is evidentiary/normative, the three readings could remain permanently coexisting even under full empirical consensus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_partition_location, conceptual, 'Locates whether the kernel contest is empirical or normative in character, per the committer-frame routing rule.').

omega_variable(
    theater_versus_function_boundary,
    'Within the rising theater_ratio (0.22 to 0.40), what proportion of documentation and audit activity is verifying real competence versus generating a compliance record independent of competence?',
    'Blind comparison of documented compliance status against independently assessed operator competence (e.g., unannounced skills assessment) across a sample of certified operators.',
    'A large gap between documented compliance and assessed competence would confirm the theater_ratio trend as genuine Goodhart drift rather than legitimate increased rigor, strengthening the tangled_rope classification''s extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_versus_function_boundary, empirical, 'Distinguishes genuine verification growth from proxy-goal substitution in the rising theater ratio.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__hybrid_occupation, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__hybrid_occupation, theater_ratio, 0, 0.22).
narrative_ontology:measurement(comp_tr_t4, competence_occupation__hybrid_occupation, theater_ratio, 4, 0.26).
narrative_ontology:measurement(comp_tr_t8, competence_occupation__hybrid_occupation, theater_ratio, 8, 0.3).
narrative_ontology:measurement(comp_tr_t12, competence_occupation__hybrid_occupation, theater_ratio, 12, 0.33).
narrative_ontology:measurement(comp_tr_t16, competence_occupation__hybrid_occupation, theater_ratio, 16, 0.36).
narrative_ontology:measurement(comp_tr_t20, competence_occupation__hybrid_occupation, theater_ratio, 20, 0.38).
narrative_ontology:measurement(comp_tr_t24, competence_occupation__hybrid_occupation, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__hybrid_occupation, base_extractiveness, 0, 0.33).
narrative_ontology:measurement(comp_be_t4, competence_occupation__hybrid_occupation, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(comp_be_t8, competence_occupation__hybrid_occupation, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(comp_be_t12, competence_occupation__hybrid_occupation, base_extractiveness, 12, 0.46).
narrative_ontology:measurement(comp_be_t16, competence_occupation__hybrid_occupation, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(comp_be_t20, competence_occupation__hybrid_occupation, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(comp_be_t24, competence_occupation__hybrid_occupation, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__hybrid_occupation, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(comp_su_t4, competence_occupation__hybrid_occupation, suppression_requirement, 4, 0.33).
narrative_ontology:measurement(comp_su_t8, competence_occupation__hybrid_occupation, suppression_requirement, 8, 0.36).
narrative_ontology:measurement(comp_su_t12, competence_occupation__hybrid_occupation, suppression_requirement, 12, 0.38).
narrative_ontology:measurement(comp_su_t16, competence_occupation__hybrid_occupation, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(comp_su_t20, competence_occupation__hybrid_occupation, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(comp_su_t24, competence_occupation__hybrid_occupation, suppression_requirement, 24, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__hybrid_occupation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_occupation__hybrid_occupation, 0.12).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, real_incident_necessity).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'competence occupation' under the ε-invariance principle. hybrid_occupation claims moderate, additive, slowly-rising extraction (0.52) reflecting a genuine but exploited coordination core. simulation_sufficiency and real_incident_necessity are separate stories with their own ε values, beneficiary/victim structures, and claimed types — they are not alternate measurements of this constraint but structurally distinct claims about what satisfies the same underlying kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
