% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__simulation_as_proxy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__simulation_as_proxy, []).

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
 *   constraint_id: competence_exercise_validity__simulation_as_proxy
 *   human_readable: Simulation-as-Sufficient-Proxy for Catastrophe Competence
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   In high-consequence operational domains (nuclear plant operations,
 *   aviation, emergency response, industrial process control) the question of
 *   what counts as valid competence-retention exercise is contested. This
 *   story instantiates the 'simulation_as_proxy' reading of that contest: the
 *   position, held by training institutions, simulator vendors, and
 *   compliance regulators, that repeated simulation drills constitute
 *   sufficient exercise of catastrophe-response competence, and that an
 *   acceptable safety record plus regulatory sign-off demonstrates adequacy
 *   without requiring exposure to real catastrophic conditions. This reading
 *   treats the drill as a legitimate proxy-catastrophe rather than merely a
 *   rehearsal technique. The coordination function is real (real catastrophes
 *   cannot be manufactured for training purposes), but the same
 *   simulation-sufficiency doctrine that solves that problem also lets the
 *   parties who administer and profit from the training regime avoid a
 *   harder, more expensive, and more revealing verification standard —
 *   creating a tangled rope where genuine coordination and asymmetric
 *   extraction ride the same structure.
 *
 * KEY AGENTS:
 *   - simulation_vendors: organized beneficiary selling the proxy standard's continued acceptance
 *   - training_department_management: institutional agenda-setter administering the drill regime against budget-tied metrics
 *   - regulatory_compliance_officers: institutional beneficiary whose audit burden shrinks under simulation sufficiency
 *   - frontline_operators: moderate-power payer certified competent on a basis that may not transfer to real events
 *   - downstream_public_at_risk: powerless, trapped payer bearing tail-risk of the proxy's failure
 *   - incident_investigators: moderate-power payer/observer whose findings are structurally narrowed away from standard-level critique
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__simulation_as_proxy, 0.58).
domain_priors:suppression_score(competence_exercise_validity__simulation_as_proxy, 0.52).
domain_priors:theater_ratio(competence_exercise_validity__simulation_as_proxy, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, extractiveness, 0.58).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__simulation_as_proxy, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__simulation_as_proxy, "Simulation-as-Sufficient-Proxy for Catastrophe Competence").
narrative_ontology:topic_domain(competence_exercise_validity__simulation_as_proxy, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__simulation_as_proxy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__simulation_as_proxy, '92ca0a49-ebf0-48fe-a9df-886f92b81c6a').
narrative_ontology:cs_kernel_codification('92ca0a49-ebf0-48fe-a9df-886f92b81c6a', formalized).
narrative_ontology:cs_authority_grounding('92ca0a49-ebf0-48fe-a9df-886f92b81c6a', extraction).
narrative_ontology:cs_interpretation_layer_present('92ca0a49-ebf0-48fe-a9df-886f92b81c6a').
narrative_ontology:cs_reading_relation('92ca0a49-ebf0-48fe-a9df-886f92b81c6a', competence_exercise_validity__real_catastrophe_only, forecloses).
narrative_ontology:cs_reading_relation('92ca0a49-ebf0-48fe-a9df-886f92b81c6a', competence_exercise_validity__continuous_refresh_hybrid, influences).
narrative_ontology:cs_axiom('92ca0a49-ebf0-48fe-a9df-886f92b81c6a', foundational, simulation_exposure_constitutes_genuine_catastrophe_exercise).
narrative_ontology:cs_axiom_status(simulation_exposure_constitutes_genuine_catastrophe_exercise, holdable).
narrative_ontology:cs_axiom_grounding('92ca0a49-ebf0-48fe-a9df-886f92b81c6a', simulation_exposure_constitutes_genuine_catastrophe_exercise, empirically_contingent).
narrative_ontology:cs_axiom('92ca0a49-ebf0-48fe-a9df-886f92b81c6a', foundational, regulatory_compliance_and_safety_record_jointly_demonstrate_competence_adequacy).
narrative_ontology:cs_axiom_status(regulatory_compliance_and_safety_record_jointly_demonstrate_competence_adequacy, holdable).
narrative_ontology:cs_axiom_grounding('92ca0a49-ebf0-48fe-a9df-886f92b81c6a', regulatory_compliance_and_safety_record_jointly_demonstrate_competence_adequacy, conventional).
narrative_ontology:cs_reference_frame('92ca0a49-ebf0-48fe-a9df-886f92b81c6a', pre_simulation_apprenticeship_standard).
narrative_ontology:cs_drift_state('92ca0a49-ebf0-48fe-a9df-886f92b81c6a', contemporary_high_fidelity_simulator_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('92ca0a49-ebf0-48fe-a9df-886f92b81c6a', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, simulation_vendors).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, training_department_management).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, regulatory_compliance_officers).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, operations_leadership).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, frontline_operators).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, downstream_public_at_risk).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, incident_investigators).
narrative_ontology:constraint_vindicates(competence_exercise_validity__simulation_as_proxy, regulatory_compliance_sufficiency_doctrine).
narrative_ontology:constraint_vindicates(competence_exercise_validity__simulation_as_proxy, safety_record_as_proof_of_competence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sell simulator hours, certification packages, and refresher modules to operators and regulators. Revenue depends directly on simulation being accepted as sufficient rather than merely supplementary; they fund research and standards-body participation that reinforces this acceptance.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, simulation_vendors, beneficiary,
    organized, generational, arbitrage, national).

% Designs and administers the training regime, sets simulation hour requirements, and reports compliance metrics upward. Their budget, headcount, and performance reviews are tied to hitting simulation-based certification targets, not to any independently verified competence outcome, so they have direct incentive to treat drills as the terminal validation rather than a rehearsal for something harder.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, training_department_management, agenda_setter,
    institutional, biographical, mobile, national).

% Certify that operators meet training requirements defined largely in terms of logged simulation hours and passed drill scenarios. Their own audit burden shrinks when simulation counts as sufficient evidence; a stricter standard requiring real-incident validation would multiply their workload and expose gaps in current oversight capacity.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, regulatory_compliance_officers, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__simulation_as_proxy, regulatory_compliance_officers, agenda_setter).

% Benefits from a training pipeline that is cheap, schedulable, and produces a clean paper trail of certified personnel. Real-catastrophe exposure cannot be manufactured on demand and would disrupt operations, so simulation sufficiency lets staffing and scheduling proceed without acknowledging unmeasured competence gaps.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, operations_leadership, beneficiary,
    institutional, biographical, mobile, national).

% Undergo the simulation drills and are certified competent on that basis, but carry the actual risk when a real event departs from the simulated failure modes — modeling assumptions, time pressure, and multi-system cascades that simulators cannot fully replicate. If a real event exposes a gap, the operator is individually blamed for 'failing to apply training' rather than the training regime being questioned.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, frontline_operators, payer,
    moderate, immediate, constrained, local).

% Lives or works near the facility or system the trained personnel operate. Has no visibility into whether the certification regime tracks real competence and no channel to demand a harder validation standard; bears the tail-risk cost if simulated competence proves inadequate during an actual crisis.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, downstream_public_at_risk, payer,
    powerless, generational, trapped, regional).

% Conduct post-incident reviews and repeatedly find that certified, drill-passing personnel failed to respond adequately to conditions simulations did not model. Their findings are consistently narrowed to individual performance rather than training-standard adequacy, because reopening the standard question implicates the certifying bodies that commission their reviews.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, incident_investigators, payer,
    moderate, biographical, analytical, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__simulation_as_proxy, incident_investigators, observer).

% Argue simulation can never substitute for the psychological and systemic load of genuine catastrophe, and that certifying on simulation alone is a category error. Their position is structurally excluded from standard-setting bodies dominated by simulation vendors and compliance officers who benefit from the current standard.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, real_catastrophe_only_advocates, excluded,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_validity__simulation_as_proxy, diffuse).
narrative_ontology:fixing_cost_class(competence_exercise_validity__simulation_as_proxy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a scalable, repeatable, safety-preserving way to expose personnel to failure scenarios without waiting for or manufacturing real disasters — genuinely solves the problem that real catastrophes are rare, costly, and too dangerous to use as the sole training vehicle.
% TRANSFER_FUNCTION: Moves certification legitimacy and budget allocation from a harder-to-achieve real-world competence standard toward a cheaper, more controllable simulation-based standard; moves residual risk from the certifying institutions onto frontline operators and the downstream public who bear the cost when the proxy diverges from the real thing.
% ABSENT_VOICES: Real-catastrophe-only advocates and independent competence researchers are structurally absent from standard-setting committees, which are populated by simulation vendors, training departments, and compliance officers who all benefit from simulation sufficiency being the accepted doctrine.
% DISAPPEARANCE_RATIONALE: If simulation-as-sufficient-proxy were abandoned overnight, certification pipelines would collapse (no scalable alternative exists), training budgets and vendor contracts would be renegotiated, compliance officers would face a much harder verification problem, and operations leadership would have to acknowledge unquantified competence gaps in current staff — a substantial institutional rearrangement, not a null event.
% FOUNDING_PROBLEM: Real catastrophic events are too rare, too dangerous, and too costly to use as the primary or sole mechanism for training and certifying personnel in high-consequence systems; some proxy for catastrophe exposure was needed to build and maintain operational competence at scale.
% FOUNDING_PROBLEM_CORROBORATION: Simulation vendors, training departments, and compliance officers attest the problem is solved: simulation fidelity has improved and safety records remain acceptable. Incident investigators — an outside party whose institutional position is not simulation-vendor-aligned but whose funding still flows through the certifying bodies — repeatedly find in post-incident reports that simulated scenarios failed to prepare personnel for the actual failure modes encountered, suggesting the founding problem (producing genuine competence, not merely a defensible paper trail) remains substantially unsolved even where compliance is met.
narrative_ontology:disappearance_verdict(competence_exercise_validity__simulation_as_proxy, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__simulation_as_proxy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__simulation_as_proxy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_validity__simulation_as_proxy, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__simulation_as_proxy, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__simulation_as_proxy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__simulation_as_proxy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_validity__simulation_as_proxy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 (rising from 0.34) because the doctrine's cost is not visible in ordinary operation — it only surfaces when a real event departs from simulated failure modes, and the gap between simulated and real competence has widened as simulation fidelity claims have outpaced actual validation against catastrophic ground truth. Theater ratio rises to 0.61 because an increasing share of the training apparatus (certification ceremonies, compliance dashboards, hour-logging) functions to demonstrate compliance rather than to build or measure real competence. Suppression is moderate (0.52): the doctrine is maintained less by coercion than by structural exclusion of the sibling readings from standard-setting bodies, and by incident-investigation processes that are institutionally narrowed to avoid indicting the training standard itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Simulation vendors, training management, compliance officers, and operations leadership are beneficiaries: they collect budget, revenue, reduced audit burden, or scheduling predictability from simulation being treated as sufficient, and none personally bears the tail-risk if the proxy fails. Frontline operators are targets: they are certified and then blamed individually if real conditions diverge from drilled scenarios, despite the standard — not their performance — being the more plausible point of failure. The downstream public is the most extracted-from party: powerless, trapped, generational time horizon, with zero visibility into or leverage over the certification standard, yet fully exposed to its failure modes. Incident investigators occupy a dual payer/observer position — structurally paid by the same institutions whose standard they might otherwise indict, which dampens their institutional capacity to escalate systemic findings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (producing genuine catastrophe-response competence at scale, safely) remains partially live, but the doctrine that simulation is SUFFICIENT (rather than necessary-but-partial) has drifted from a coordination solution into a self-protective standard maintained by the very institutions it audits. The tangled_rope classification prevents this from being mislabeled as pure coordination (a rope) — there IS a genuine coordination function performing real work — while also preventing it from being mislabeled as pure extraction (a snare) with no coordination value at all. Incident investigation findings that are repeatedly narrowed to individual failure, rather than standard failure, are the diagnostic signature of the extraction component riding on the coordination component.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_ceiling,
    'Is there an irreducible fidelity ceiling below which simulation cannot expose the cognitive and systemic failure modes that real catastrophic events produce, or is the gap closable with sufficiently advanced simulation technology?',
    'Longitudinal comparison of certified-competent personnel''s real-event performance against simulation-predicted performance across multiple domains and simulator generations; convergence over time with improving fidelity would support the closable-gap view, persistent divergence would support an irreducible ceiling.',
    'If the ceiling is irreducible, simulation-as-sufficient is structurally a false summit regardless of technological improvement, strengthening the case for the real_catastrophe_only or continuous_refresh_hybrid readings. If closable, the current extraction may be a temporary artifact of immature simulation technology rather than a permanent structural feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_ceiling, empirical, 'Whether simulation fidelity has a structural ceiling relative to real catastrophic exposure.').

omega_variable(
    kernel_reading_contest_location,
    'Where exactly does the simulation_as_proxy reading diverge from the continuous_refresh_hybrid reading — is it a binary sufficiency claim (simulation alone certifies) versus a cadence claim (simulation certifies only when continuously repeated), and does the current standard actually enforce continuous refresh in practice even while claiming one-time sufficiency?',
    'Audit actual recertification cadences against the doctrine''s stated claims: if institutions practicing ''simulation_as_proxy'' in fact require frequent recertification, the reading may be closer to continuous_refresh_hybrid in practice than in its stated justification, meaning the two readings may be less distinct in operation than in doctrine.',
    'If practice already resembles continuous_refresh_hybrid, this story''s ε may overstate the extraction attributable specifically to the ''sufficiency'' doctrine, since much of the actual coordination benefit may come from refresh cadence rather than simulation-as-such.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Whether this reading is doctrinally distinct from continuous_refresh_hybrid in actual institutional practice, not merely in stated justification.').

omega_variable(
    safety_record_as_proof_validity,
    'Does an acceptable historical safety record actually demonstrate that the current training standard is adequate, or is it consistent with the standard being inadequate but the triggering conditions for catastrophic failure simply not having occurred yet (survivorship / absence-of-evidence problem)?',
    'Statistical analysis of near-miss reports and close-call incidents for evidence that operators were saved by factors external to their training (luck, redundant systems, unusually favorable conditions) rather than by the competence the training was meant to instill.',
    'If near-misses show non-training factors doing the protective work, the ''safety record proves adequacy'' vindicated proposition is substantially undermined, sharpening the extraction reading; if near-misses show training-driven successful interventions, the sufficiency claim is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_record_as_proof_validity, empirical, 'Whether a clean safety record is genuine evidence of training adequacy or an absence-of-evidence artifact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__simulation_as_proxy, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_validity__simulation_as_proxy, theater_ratio, 0, 0.28).
narrative_ontology:measurement(comp_tr_t4, competence_exercise_validity__simulation_as_proxy, theater_ratio, 4, 0.35).
narrative_ontology:measurement(comp_tr_t8, competence_exercise_validity__simulation_as_proxy, theater_ratio, 8, 0.42).
narrative_ontology:measurement(comp_tr_t12, competence_exercise_validity__simulation_as_proxy, theater_ratio, 12, 0.48).
narrative_ontology:measurement(comp_tr_t16, competence_exercise_validity__simulation_as_proxy, theater_ratio, 16, 0.53).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_validity__simulation_as_proxy, theater_ratio, 20, 0.58).
narrative_ontology:measurement(comp_tr_t24, competence_exercise_validity__simulation_as_proxy, theater_ratio, 24, 0.61).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(comp_be_t4, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(comp_be_t8, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(comp_be_t12, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(comp_be_t16, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(comp_be_t20, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(comp_be_t24, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(comp_su_t4, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 4, 0.35).
narrative_ontology:measurement(comp_su_t8, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(comp_su_t12, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(comp_su_t16, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(comp_su_t20, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(comp_su_t24, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__simulation_as_proxy, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_validity__simulation_as_proxy, 0.12).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, real_catastrophe_only).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, continuous_refresh_hybrid).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the competence_exercise_validity kernel: simulation_as_proxy (this story), real_catastrophe_only, and continuous_refresh_hybrid. Each reading authors its own ε and beneficiary/victim structure over the same underlying institutional practice of catastrophe-competence training. simulation_as_proxy shows the highest and most stable extractiveness among the three because it is the reading most institutionally entrenched (backed by simulation vendors and compliance officers with concentrated interest in sufficiency doctrine); real_catastrophe_only would author near-zero extraction with negligible institutional backing (an aspirational rather than operative standard); continuous_refresh_hybrid sits structurally between, since it retains simulation's coordination benefit while imposing a cadence requirement that dilutes (but does not eliminate) the sufficiency-doctrine's extraction. The three stories are linked via affects_constraints because a shift in institutional consensus toward either sibling reading would directly erode this reading's beneficiary base and enforcement mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
