% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__catastrophe_as_necessary_selector
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__catastrophe_as_necessary_selector, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: catastrophe_avoidance_retention__catastrophe_as_necessary_selector
 *   human_readable: Catastrophe as Necessary Selection Pressure for Competence Maintenance
 *   domain: safety_engineering/organizational_learning
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.68).
domain_priors:suppression_score(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.72).
domain_priors:theater_ratio(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, snare).
narrative_ontology:human_readable(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, "Catastrophe as Necessary Selection Pressure for Competence Maintenance").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 'd32ae857-573c-494a-9bab-2967eefe7ce7').
narrative_ontology:cs_kernel_codification('d32ae857-573c-494a-9bab-2967eefe7ce7', distributed).
narrative_ontology:cs_authority_grounding('d32ae857-573c-494a-9bab-2967eefe7ce7', extraction).
narrative_ontology:cs_reading_relation('d32ae857-573c-494a-9bab-2967eefe7ce7', catastrophe_avoidance_retention__hybrid_near_miss_learning, coexists_with).
narrative_ontology:cs_reading_relation('d32ae857-573c-494a-9bab-2967eefe7ce7', catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, forecloses).
narrative_ontology:cs_axiom('d32ae857-573c-494a-9bab-2967eefe7ce7', foundational, mortality_salience_uniquely_necessary).
narrative_ontology:cs_axiom_status(mortality_salience_uniquely_necessary, holdable).
narrative_ontology:cs_axiom_grounding('d32ae857-573c-494a-9bab-2967eefe7ce7', mortality_salience_uniquely_necessary, empirically_contingent).
narrative_ontology:cs_axiom('d32ae857-573c-494a-9bab-2967eefe7ce7', foundational, peacetime_competence_decay_inevitable).
narrative_ontology:cs_axiom_status(peacetime_competence_decay_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('d32ae857-573c-494a-9bab-2967eefe7ce7', peacetime_competence_decay_inevitable, empirically_contingent).
narrative_ontology:cs_reference_frame('d32ae857-573c-494a-9bab-2967eefe7ce7', catastrophic_necessity_doctrine).
narrative_ontology:cs_drift_state('d32ae857-573c-494a-9bab-2967eefe7ce7', contemporary_safety_engineering, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d32ae857-573c-494a-9bab-2967eefe7ce7', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_survivors).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, post_incident_reformers).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, peacetime_organizations).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, simulation_dependent_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operating high-reliability systems (aviation, nuclear power, medicine, maritime) during extended periods without catastrophic failure. Competence gradually erodes through inattention, procedure drift, training normalization, and personnel turnover. They invest in simulation and drills, but these lack the mortality salience that actual failure creates. They cannot 'exit' the operational domain without abandoning the profession entirely, and their institutional identity is bound to the peacetime maintenance of safety.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, peacetime_organizations, payer,
    organized, generational, identity_locked, global).

% Trained extensively in simulation environments where failures are consequence-free, mistakes are reset, and high-fidelity mimicry of catastrophe is standard. They develop false confidence that simulation-tested competence transfers to real operations. When catastrophe does occur, the gap between rehearsed and actual stakes becomes lethal. Exit is costly: retraining in real-world systems, career path disruption, admission that simulation confidence was misplaced.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, simulation_dependent_practitioners, payer,
    moderate, biographical, constrained, global).

% Individuals who lived through or narrowly escaped organizational catastrophe (Fukushima, Deepwater Horizon, 737 MAX incidents, hospital infection outbreaks). They acquire intensified competence through mortality salience, trauma-driven learning, and forced attention to failure modes. They benefit from the constraint because it validated their hard-earned understanding; peacetime organizations that ignore their warnings pay the cost of re-learning the same lessons.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_survivors, beneficiary,
    moderate, biographical, mobile, global).

% Regulatory agencies, industry bodies, and reform movements that consolidate authority in the aftermath of catastrophe. They mandate new standards, rewrite procedures, launch public inquiries, and capture institutional legitimacy from the crisis response. Their authority lasts as long as the catastrophe's salience does. They benefit directly from catastrophe because it converts their reform agenda from peripheral to mandatory.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, post_incident_reformers, beneficiary,
    powerful, generational, arbitrage, global).

% Researchers, safety engineers, and organizational theorists who argue that distributed learning from near-misses, foreign incidents, and high-fidelity drills can sustain competence without catastrophe. They are systematically excluded from the constraint's logic because the constraint asserts their alternative is ineffective. They would argue for hybrid learning models but are outshouted by the catastrophe-necessity narrative after each incident.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, near_miss_learning_advocates, excluded,
    organized, generational, constrained, global).

% Engineers and architects who must decide whether to build redundancy, fail-safes, and graceful degradation into systems, or to rely on operator competence maintenance through periodic catastrophe. They observe the constraint's operation and its costs but cannot unilaterally override it without challenging the entire epistemology of competence maintenance.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, system_designers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, post_incident_reformers).
narrative_ontology:fixing_cost_class(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates learning resources and organizational attention on failure modes through the highest-salience mechanism available: actual catastrophic consequences. Ensures that competence maintenance is taken seriously by making the cost of neglect non-negotiable.
% TRANSFER_FUNCTION: Transfers accumulated organizational and individual competence from peacetime practitioners (who lose attention and skill) to post-catastrophe reformers and survivors (who acquire intensified understanding). Also transfers risk from well-resourced organizations to underprotected populations during the peacetime periods when competence has decayed.
% ABSENT_VOICES: Practitioners and theorists in near-miss and hybrid learning communities are excluded because the constraint asserts their alternatives are inadequate. International safety networks, distributed learning organizations, and incident report analysts operate outside the 'catastrophe as necessary selector' framework and would argue for preventive competence maintenance that does not require periodic large-scale failure.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared—if organizations maintained competence through systematic near-miss learning, high-realism drills, distributed incident reporting, and formal competence audits independent of catastrophic pressure—the entire risk landscape would shift. Peacetime competence maintenance would become a solvable organizational problem rather than an inevitability. Catastrophe rates would fall. The constraint's disappearance would not mean competence is automatically maintained; it would mean competence maintenance is decoupled from catastrophic selection pressure.
% FOUNDING_PROBLEM: Early aviation, nuclear, and maritime operations experienced devastating failures due to operator error, procedural drift, and knowledge loss during extended safe periods. The constraint emerged as a natural observation: only catastrophic consequences drive the attention and fear necessary to keep operators sharp and procedures current.
% FOUNDING_PROBLEM_CORROBORATION: Catastrophe-survival communities and post-incident reform movements attest the founding problem persists—peacetime organizations do relax, drift, and lose competence. Researchers in organizational learning, safety engineering, and high-reliability theory (including those outside the benefiting parties) largely contest this framing, arguing instead that the founding problem is organizational learning structure, not inevitability of decay. Independent studies of near-miss learning effectiveness and distributed competence maintenance contradict the necessity claim.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    peacetime_competence_decay_inevitability,
    'Is competence decay during extended peacetime periods an inevitable feature of human attention and organizational dynamics, or a solvable problem through structured near-miss learning, distributed incident reporting, and high-realism drills?',
    'Comparative study of organizations using hybrid competence-maintenance models (near-miss learning + high-realism drills + incident analysis) versus those relying solely on simulation and routine training. Measure competence retention during peacetime periods across comparable high-reliability domains.',
    'If competence decay is avoidable through alternative structures, the constraint''s classification shifts from snare (extraction normalized as inevitable) toward tangled_rope (real coordination benefit coupled with unnecessary extraction). The catastrophe-necessity claim loses its structural grounding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(peacetime_competence_decay_inevitability, empirical, 'Whether peacetime competence decay is inevitable or preventable through distributed learning.').

omega_variable(
    simulation_fidelity_and_mortality_salience_gap,
    'Can high-fidelity simulation replicate the mortality salience and organizational trauma components that catastrophe provides, or are these psychological/social factors structurally necessary to competence maintenance?',
    'Neurocognitive research on learning retention under simulated vs. real stakes; longitudinal studies of practitioners trained under high-consequence simulation versus those trained in consequence-free environments; analysis of procedural compliance and competence audit results post-simulation versus post-incident.',
    'If simulation can be designed to incorporate mortality salience through immersive consequences (career stakes, institutional accountability, failure recognition), the constraint''s extraction mechanism weakens. If mortality salience is uniquely available through actual catastrophe, the constraint''s necessity claim strengthens but raises a design failure: systems should not require actual death to maintain safety.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_and_mortality_salience_gap, empirical, 'Whether mortality salience can be replicated in simulation or requires actual catastrophic consequences.').

omega_variable(
    kernel_contest_reading_boundary,
    'Is the catastrophe-as-necessary-selector reading a logically coherent epistemic position, or does it conflate ''catastrophe reveals competence decay'' (empirically true) with ''only catastrophe can prevent decay'' (empirically contested)?',
    'Formal analysis of the argument structure: catastrophe as evidence (posterior belief update) versus catastrophe as mechanism (causal necessity). Comparison with the sibling readings (simulation_as_proxy_catastrophe, hybrid_near_miss_learning) to identify the precise structural claim that differentiates this reading.',
    'If the reading conflates evidence and mechanism, it is a failure of epistemic rigor, not a genuine empirical claim. Reclassification would shift from snare (extraction normalized) to piton (inertial maintenance of a degraded function). If the reading is coherent, the classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_reading_boundary, conceptual, 'Whether the catastrophe-necessity claim is a coherent empirical hypothesis or a logical slippage.').

omega_variable(
    beneficiary_incentive_capture,
    'Do post-incident reformers and catastrophe survivors have structural incentives to advocate for catastrophe-necessity because their institutional authority and moral standing depend on catastrophe occurring?',
    'Institutional analysis of reform movement funding, career pathways, and authority consolidation post-catastrophe. Track whether the same reformers advocate for preventive competence-maintenance structures that would reduce their post-incident authority.',
    'If beneficiaries have incentives to perpetuate catastrophe-necessity framing, the constraint''s extraction component is substantially explained by capture rather than genuine epistemic necessity. This would strengthen the snare classification but also point toward piton (theater-dependent maintenance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_incentive_capture, empirical, 'Whether beneficiary incentives drive advocacy for catastrophe-necessity doctrine.').

omega_variable(
    alternative_reading_feasibility,
    'Is the sibling reading (hybrid_near_miss_learning) structurally viable as an alternative competence-maintenance regime, or does it face irreducible barriers within existing organizational architectures?',
    'Case studies of organizations that have institutionalized near-miss learning, distributed incident reporting, and high-realism drills. Measure outcomes (competence retention, incident rates, procedural compliance, mortality-salience maintenance) against organizations using catastrophe-dependent models.',
    'If the alternative is viable, the catastrophe-necessity claim loses its epistemic foundation and the constraint becomes a choice (snare) rather than an imposed necessity. If the alternative faces structural barriers, the constraint''s extraction is partially justified but still asymmetric.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_reading_feasibility, empirical, 'Whether hybrid near-miss learning can maintain competence without catastrophe-dependent selection pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cata_tr_t5, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 5, 0.42).
narrative_ontology:measurement(cata_tr_t10, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 10, 0.48).
narrative_ontology:measurement(cata_tr_t15, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 15, 0.52).
narrative_ontology:measurement(cata_tr_t25, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 25, 0.58).
narrative_ontology:measurement(cata_tr_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cata_be_t5, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(cata_be_t10, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(cata_be_t15, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(cata_be_t25, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(cata_be_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cata_su_t5, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(cata_su_t10, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(cata_su_t15, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(cata_su_t25, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(cata_su_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.18).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, hybrid_near_miss_learning).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, simulation_as_proxy_catastrophe).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_avoidance_retention kernel. The sibling readings (hybrid_near_miss_learning, simulation_as_proxy_catastrophe) are separate constraints with their own ε-invariant structures, not alternative frames for the same constraint. The kernel contest hinges on whether competence maintenance REQUIRES catastrophic selection pressure (this reading) or can be sustained through alternative mechanisms (the siblings). Each reading instantiates a distinct constraint; they are linked via network effects because institutional adoption of one reading structurally affects the viability of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
