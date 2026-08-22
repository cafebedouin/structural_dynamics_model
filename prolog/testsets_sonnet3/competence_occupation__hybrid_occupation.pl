% ============================================================================
% CONSTRAINT STORY: competence_occupation__hybrid_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: competence_occupation__hybrid_occupation
 *   human_readable: Multi-Mechanism Competence Maintenance Regime (Hybrid Occupation Reading)
 *   domain: high_reliability_organizations/safety_training
 *
 * SUMMARY:
 *   In domains where catastrophic failure must be prevented rather than
 *   merely responded to (aviation, nuclear operations, process safety,
 *   surgery), organizations maintain operator competence between rare real
 *   events using a stack of substitute mechanisms: simulator sessions,
 *   periodic refresher training, procedural rehearsal, and independent line
 *   audits. This reading of the competence-occupation kernel holds that no
 *   single mechanism is sufficient and that legitimate occupation of the
 *   competence claim requires continuous exercise across all four channels
 *   simultaneously, evaluated by mutually independent observables (simulator
 *   score, refresher completion, procedural compliance, audit rating).
 *   Because no empirical consensus exists on which combination and weighting
 *   of these four channels is optimal, the training regime becomes a
 *   perpetual, never-finalized research problem — every incident or audit
 *   finding becomes grounds to add a mechanism or tighten an observable, and
 *   none is ever formally justified for removal. This is a distinct
 *   constraint from the sibling readings: the simulation_sufficiency reading
 *   holds that simulator performance alone can occupy the kernel (much lower
 *   ε — a single, well-validated, low-cost observable), and the
 *   real_incident_necessity reading holds that only actual catastrophic
 *   incidents provide authentic occupation (a different, and arguably
 *   ungovernable, extraction profile since it requires disaster as evidence).
 *   Each is authored as its own constraint story per the epsilon-invariance
 *   principle; this file addresses only the hybrid, multi-mechanism reading.
 *
 * KEY AGENTS:
 *   - compliance_departments: agenda_setter (institutional/constrained) - design and enforce the mechanism stack, ratchet it after incidents
 *   - training_vendors: beneficiary (organized/arbitrage) - sell modules for each mechanism, profit from unresolved configuration debate
 *   - frontline_shift_operators: payer (powerless/trapped) - absorb the compounding workload with no exit from licensure dependency
 *   - junior_trainees: payer (powerless/trapped) - gated by independent observables any one of which can block certification
 *   - safety_researchers: observer (analytical/analytical) - document the lack of consensus but findings are asymmetrically operationalized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, 0.52).
domain_priors:suppression_score(competence_occupation__hybrid_occupation, 0.58).
domain_priors:theater_ratio(competence_occupation__hybrid_occupation, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, extractiveness, 0.52).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__hybrid_occupation, tangled_rope).
narrative_ontology:human_readable(competence_occupation__hybrid_occupation, "Multi-Mechanism Competence Maintenance Regime (Hybrid Occupation Reading)").
narrative_ontology:topic_domain(competence_occupation__hybrid_occupation, "high_reliability_organizations/safety_training").

domain_priors:requires_active_enforcement(competence_occupation__hybrid_occupation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__hybrid_occupation, '6d51e5b2-10d6-4fe1-b497-0dbb7c795413').
narrative_ontology:cs_kernel_codification('6d51e5b2-10d6-4fe1-b497-0dbb7c795413', distributed).
narrative_ontology:cs_authority_grounding('6d51e5b2-10d6-4fe1-b497-0dbb7c795413', expertise).
narrative_ontology:cs_interpretation_layer_present('6d51e5b2-10d6-4fe1-b497-0dbb7c795413').
narrative_ontology:cs_reading_relation('6d51e5b2-10d6-4fe1-b497-0dbb7c795413', competence_occupation__simulation_sufficiency, coexists_with).
narrative_ontology:cs_reading_relation('6d51e5b2-10d6-4fe1-b497-0dbb7c795413', competence_occupation__real_incident_necessity, influences).
narrative_ontology:cs_axiom('6d51e5b2-10d6-4fe1-b497-0dbb7c795413', foundational, no_single_mechanism_suffices_for_competence).
narrative_ontology:cs_axiom_status(no_single_mechanism_suffices_for_competence, holdable).
narrative_ontology:cs_axiom_grounding('6d51e5b2-10d6-4fe1-b497-0dbb7c795413', no_single_mechanism_suffices_for_competence, empirically_contingent).
narrative_ontology:cs_axiom('6d51e5b2-10d6-4fe1-b497-0dbb7c795413', secondary, mechanism_stack_must_be_continuously_revised_not_finalized).
narrative_ontology:cs_axiom_status(mechanism_stack_must_be_continuously_revised_not_finalized, holdable).
narrative_ontology:cs_axiom_grounding('6d51e5b2-10d6-4fe1-b497-0dbb7c795413', mechanism_stack_must_be_continuously_revised_not_finalized, instrumental).
narrative_ontology:cs_reference_frame('6d51e5b2-10d6-4fe1-b497-0dbb7c795413', pre_stack_ad_hoc_training_regime).
narrative_ontology:cs_drift_state('6d51e5b2-10d6-4fe1-b497-0dbb7c795413', contemporary_high_reliability_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6d51e5b2-10d6-4fe1-b497-0dbb7c795413', '').
narrative_ontology:cs_kernel_id(competence_occupation__hybrid_occupation, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, training_vendors).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, compliance_departments).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, senior_operators_with_tenure_protection).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, frontline_shift_operators).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, junior_trainees).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, line_supervisors_conducting_audits).
narrative_ontology:constraint_vindicates(competence_occupation__hybrid_occupation, multi_mechanism_necessity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sell simulation platforms, refresher curricula, procedural checklists, and audit software as separate, continuously renewed products. Because no configuration of the four mechanisms is ever declared sufficient, every incident or near-miss becomes grounds to sell an additional module. Their revenue depends on the optimal configuration remaining permanently unresolved.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, training_vendors, beneficiary,
    organized, generational, arbitrage, national).

% Design and enforce the four-mechanism regime, mandate hours and audit frequencies, and answer to regulators and boards after incidents. They administer the constraint and could in principle simplify it, but doing so exposes them to liability if a future incident is later traced to a dropped mechanism, so they keep adding rather than subtracting.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, compliance_departments, agenda_setter,
    institutional, generational, constrained, national).

% Absorb simulation hours, refresher sessions, procedural drills, and audit scrutiny on top of full operational shift load, with no say in which mechanisms are retained or dropped. Fatigue from stacked requirements is treated as a discipline or attitude problem rather than a workload problem. Leaving the occupation means losing licensure and years of accumulated qualification.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, frontline_shift_operators, payer,
    powerless, immediate, trapped, local).

% Must clear all four mechanisms sequentially to be certified competent, with each mechanism graded by a different observable (sim score, refresher attendance, procedural checklist compliance, audit rating) that can independently gate advancement. A trainee who is genuinely safe on the line can still be held back by a single lagging observable, with no way to demonstrate overall competence directly.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, junior_trainees, payer,
    powerless, biographical, trapped, local).

% Conduct the line audits that feed the competence determination while also being audited themselves on audit thoroughness, creating a second-order compliance burden. They both administer part of the mechanism stack and are measured by it, and have limited ability to simplify the audit protocol they did not design.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, line_supervisors_conducting_audits, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(competence_occupation__hybrid_occupation, line_supervisors_conducting_audits, agenda_setter).

% Long tenure and grandfathered qualification pathways mean the multi-mechanism burden falls disproportionately on newer staff; senior operators are often exempted from full refresher cycles or graded on relaxed audit standards, benefiting from the same regime that burdens juniors.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, senior_operators_with_tenure_protection, beneficiary,
    moderate, civilizational, constrained, local).

% Study skill decay curves, simulation validity, and audit reliability across organizations, publishing findings that no single mechanism configuration is empirically optimal. Their work is cited to justify adding mechanisms but almost never to justify removing any, since a removal recommendation carries downstream liability if wrong.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, safety_researchers, observer,
    analytical, generational, analytical, global).

% Set minimum requirements after incident investigations, generally by requiring an additional mechanism rather than revising the existing stack, since a proven gap is easier to close with an addition than a substitution. They have authority to consolidate the regime but no political incentive to do so.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, regulators, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__hybrid_occupation, regulators, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__hybrid_occupation, diffuse).
narrative_ontology:fixing_cost_class(competence_occupation__hybrid_occupation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely reduces the probability that an operator loses a critical skill between rare, high-consequence events, by exercising competence through several partially-overlapping channels (simulated practice, periodic refresh, procedural rehearsal, and independent line observation) so that failure in one channel is caught by another.
% TRANSFER_FUNCTION: Moves operator time, cognitive load, and career risk from a hypothetical future incident into continuous present-tense compliance labor; moves revenue from operating budgets into training-vendor and compliance-department budgets; moves liability exposure from individual decision-makers into the documented existence of the stacked mechanism regime.
% ABSENT_VOICES: Frontline operators who experience the stacked workload are rarely consulted on mechanism design; their input is filtered through supervisors who are themselves measured by compliance metrics. Independent human-factors researchers studying training fatigue and diminishing returns are cited selectively — findings supporting addition are amplified, findings supporting consolidation are not operationalized.
% DISAPPEARANCE_RATIONALE: Regulators and compliance departments would say the world rearranges catastrophically — competence would erode and incidents would follow. Frontline operators and independent researchers would say the world mostly stays the same for actual safety outcomes but the administrative and financial burden vanishes, since no consensus configuration has ever been shown superior to a leaner one. The dispute over which claim is true is exactly the unresolved research question the constraint's name identifies.
% FOUNDING_PROBLEM: High-consequence, low-frequency operations (aviation, nuclear, surgical, process safety) cannot rely on real incidents to maintain operator competence because real incidents are both too costly and too rare to serve as a training signal, so some substitute exercise regime is structurally necessary.
% FOUNDING_PROBLEM_CORROBORATION: Independent safety researchers outside the training-vendor and compliance ecosystem corroborate that the underlying problem (skill decay between rare events) is real and measurable via decay-curve studies. However, those same researchers do NOT corroborate that the specific four-mechanism configuration is the solution the problem requires — their published work explicitly states no consensus configuration exists, which is the gap this reading names rather than resolves.
narrative_ontology:disappearance_verdict(competence_occupation__hybrid_occupation, contested).
narrative_ontology:founding_problem_status(competence_occupation__hybrid_occupation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__hybrid_occupation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at a moderate 0.52 rather than high, because a genuine coordination function is present and real: skill decay between rare catastrophic events is a documented empirical phenomenon, and multi-channel exercise plausibly does catch failures that any single channel would miss. The extraction is in the asymmetric direction of ratchet-only revision: mechanisms are added after incidents but never formally removed even when researchers show diminishing marginal safety return, and the workload cost of the accumulation lands almost entirely on frontline and junior staff rather than on the compliance departments or vendors who design and sell the stack. Theater ratio rises across the interval (0.22 to 0.44) as more of the audit and refresher activity becomes about producing a defensible paper trail (documented mechanism completion) rather than validated competence, which is the classic signature of an under-consolidated multi-mechanism regime drifting toward compliance theater. Suppression is authored moderately (0.58) and is structural rather than coercive in the classical sense: it operates through licensure dependency (trapped exit) and through the fact that no individual actor can unilaterally simplify the stack without assuming personal liability for a hypothetical future incident.
 *
 * PERSPECTIVAL GAP:
 *   Compliance departments and training vendors, from their seats, see continuous multi-mechanism exercise as prudent coordination against catastrophic tail risk — the engine is expected to read their seat closer to a coordination-dominant classification given their power and exit profile. Frontline operators and junior trainees, from their seats, experience the same stacked mechanism regime as compounding, ratchet-only extraction of their time and career risk with no corresponding say in configuration — the engine is expected to read their seat closer to extraction-dominant. Line supervisors sit in between, administering part of the stack while being measured by it, which is why they carry a secondary agenda_setter role alongside payer.
 *
 * DIRECTIONALITY LOGIC:
 *   Training vendors and senior operators with tenure protection are declared beneficiaries: vendors profit directly from perpetual reconfiguration, and tenured seniors are frequently grandfathered out of the heaviest current requirements. Frontline shift operators, junior trainees, and line supervisors are declared victims/payers: they bear the compounding workload, the multi-gate certification risk, and the second-order audit burden respectively, with constrained-to-trapped exit because licensure and accumulated qualification are not portable outside the specific mechanism regime. Compliance departments are the agenda_setter — they administer the constraint and could in principle simplify it, but institutional liability incentives push them toward addition rather than consolidation, which is why they are not classified as pure beneficiaries despite controlling the regime.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (skill decay between rare high-consequence events) remains genuinely live, which prevents this constraint from being mislabeled a pure snare — there is real coordination content, corroborated by researchers outside the beneficiary set. But the specific four-mechanism configuration is not itself validated as the necessary solution to that live problem; the six-questions mismatch check (founding_problem_status=live, disappearance_verdict=contested) flags exactly the ambiguity the tangled_rope classification is built to hold: genuine coordination function coexists with asymmetric extraction riding on the same structure, sustained by ratchet-only revision and liability-driven mechanism accumulation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_configuration_indeterminacy,
    'Is there in fact no optimal configuration of the four mechanisms because the problem is genuinely under-determined by available evidence, or has the appearance of indeterminacy been manufactured/sustained by parties who profit from perpetual reconfiguration?',
    'A blinded, cross-organizational meta-analysis of skill-decay outcomes under varying mechanism-stack configurations, conducted or audited by researchers with no financial relationship to training vendors or compliance consultancies.',
    'If genuinely under-determined, the tangled_rope classification is well-founded — real coordination uncertainty coexists with extraction. If the indeterminacy is manufactured or sustained by selective citation, the constraint moves closer to snare, since the coordination story becomes primarily cover for an unresolvable-by-design revenue and liability-shifting structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_configuration_indeterminacy, empirical, 'Whether the lack of configuration consensus is genuine or vendor/liability-sustained.').

omega_variable(
    ratchet_only_revision_asymmetry,
    'Why do mechanism additions after incidents get institutionalized permanently while researcher-documented diminishing returns never trigger formal consolidation or removal?',
    'Track the rate and outcome of formal proposals to remove or downweight an existing mechanism versus proposals to add one, across a sample of high-reliability organizations, and interview compliance decision-makers on their stated reasoning for asymmetric treatment.',
    'If the asymmetry is driven by liability exposure (removing a mechanism creates personally attributable risk if a future incident occurs) rather than by safety evidence, this corroborates the extraction reading of the ratchet and supports keeping requires_active_enforcement and the tangled_rope classification rather than treating the regime as pure evolving-rope coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ratchet_only_revision_asymmetry, empirical, 'Whether mechanism accumulation is evidence-driven or liability-driven.').

omega_variable(
    cs_framing_kernel_vs_legitimacy_narrative,
    'Is the correct CS framing the training-mechanism regime itself (formalized kernel, compliance-department authority), or is the deeper kernel the narrative of ''due diligence against catastrophic failure'' that compliance departments and regulators invoke to legitimize whichever mechanism stack currently exists?',
    'Compare classification outcomes under both framings: does treating ''due diligence narrative'' as the kernel and ''specific mechanism stack'' as an interpretation-layer artifact change the authority_grounding assessment from practice/expertise to something closer to extraction?',
    'Under the narrower framing (mechanism regime as kernel), authority_grounding reads as expertise/practice exercised by compliance departments and regulators. Under the broader framing (legitimacy narrative as kernel), authority_grounding reads closer to extraction, since the narrative of due diligence is what licenses indefinite mechanism accumulation regardless of evidence. This story adopts the narrower framing because the six-questions interview and stakeholder situations are authored at the mechanism-regime level, not the meta-narrative level; the broader framing would be a separate, related constraint story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_legitimacy_narrative, conceptual, 'Alternative CS framings: mechanism-regime-as-kernel versus due-diligence-narrative-as-kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__hybrid_occupation, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__hybrid_occupation, theater_ratio, 0, 0.22).
narrative_ontology:measurement(comp_tr_t4, competence_occupation__hybrid_occupation, theater_ratio, 4, 0.27).
narrative_ontology:measurement(comp_tr_t8, competence_occupation__hybrid_occupation, theater_ratio, 8, 0.32).
narrative_ontology:measurement(comp_tr_t12, competence_occupation__hybrid_occupation, theater_ratio, 12, 0.36).
narrative_ontology:measurement(comp_tr_t16, competence_occupation__hybrid_occupation, theater_ratio, 16, 0.4).
narrative_ontology:measurement(comp_tr_t20, competence_occupation__hybrid_occupation, theater_ratio, 20, 0.42).
narrative_ontology:measurement(comp_tr_t24, competence_occupation__hybrid_occupation, theater_ratio, 24, 0.44).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__hybrid_occupation, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(comp_be_t4, competence_occupation__hybrid_occupation, base_extractiveness, 4, 0.39).
narrative_ontology:measurement(comp_be_t8, competence_occupation__hybrid_occupation, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(comp_be_t12, competence_occupation__hybrid_occupation, base_extractiveness, 12, 0.47).
narrative_ontology:measurement(comp_be_t16, competence_occupation__hybrid_occupation, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(comp_be_t20, competence_occupation__hybrid_occupation, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(comp_be_t24, competence_occupation__hybrid_occupation, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__hybrid_occupation, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(comp_su_t4, competence_occupation__hybrid_occupation, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(comp_su_t8, competence_occupation__hybrid_occupation, suppression_requirement, 8, 0.49).
narrative_ontology:measurement(comp_su_t12, competence_occupation__hybrid_occupation, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(comp_su_t16, competence_occupation__hybrid_occupation, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(comp_su_t20, competence_occupation__hybrid_occupation, suppression_requirement, 20, 0.57).
narrative_ontology:measurement(comp_su_t24, competence_occupation__hybrid_occupation, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__hybrid_occupation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_occupation__hybrid_occupation, 0.12).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, competence_occupation__simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, competence_occupation__real_incident_necessity).

% DUAL FORMULATION NOTE:
% Three constraint files decompose the natural-language concept 'competence occupation kernel': hybrid_occupation (this file, tangled_rope, moderate ε from genuine multi-channel coordination plus ratchet-only extraction), simulation_sufficiency (expected lower ε, closer to rope, single validated low-cost observable), and real_incident_necessity (expected structurally distinct extraction profile requiring disaster as the only valid evidence, likely much higher suppression/resistance due to its refusal of substitute mechanisms). Each carries its own ε and classification per the epsilon-invariance principle; they are linked here as siblings within one contested kernel rather than merged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
