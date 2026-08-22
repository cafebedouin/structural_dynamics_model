% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__simulation_as_sufficient
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__simulation_as_sufficient, []).

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
 *   constraint_id: competence_retention_exercise__simulation_as_sufficient
 *   human_readable: High-Fidelity Simulation as Sufficient Catastrophe-Avoidance Competence Exercise
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This story is the 'simulation_as_sufficient' reading of the
 *   competence_retention_exercise kernel: the contested claim of whether
 *   high-fidelity simulation constitutes genuine exercise of
 *   catastrophe-avoidance competence, structurally equivalent to real events.
 *   Under this reading, training infrastructure — simulators, certifying
 *   bodies, simulator-metric-based curricula — becomes the primary (and
 *   eventually sole) legitimate mechanism for competence maintenance, and
 *   real catastrophes are treated as failures to be prevented rather than
 *   pedagogical necessities. The reading genuinely coordinates a real problem
 *   (real catastrophes are too rare, dangerous, and destructive to train on
 *   directly) but has hardened into an arrangement with identifiable
 *   beneficiaries (vendors, certifiers, management) who profit from the
 *   equivalence claim being accepted uncritically, and identifiable payers
 *   (frontline operators, downstream public) who bear the consequences if the
 *   equivalence claim understates any dimension of real-event demand. The
 *   sibling readings — catastrophe_as_necessary and near_miss_as_bridge — are
 *   separate constraints, not alternatives folded into this one; this file's
 *   ε describes only the simulation-as-sufficient arrangement as it currently
 *   operates, assessed by that reading's own lights.
 *
 * KEY AGENTS:
 *   - training_program_administrators: agenda-setter, institutional power, sets simulator curricula and certification thresholds
 *   - simulator_vendors: primary beneficiary, organized power, revenue tied to the sufficiency claim being accepted
 *   - regulatory_certifying_bodies: beneficiary and co-agenda-setter, institutional power, credibility staked on equivalence holding
 *   - operations_management: beneficiary, powerful, gains staffing efficiency and liability cover
 *   - frontline_operators: primary payer, moderate power, bears consequence if fidelity gap exists
 *   - downstream_public_exposed_to_residual_risk: payer, powerless, bears tail risk with zero voice
 *   - catastrophe_experience_advocates: excluded voice, argues visceral-stakes gap is real
 *   - independent_safety_researchers: analytical observer, studies transfer-of-training validity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__simulation_as_sufficient, 0.42).
domain_priors:suppression_score(competence_retention_exercise__simulation_as_sufficient, 0.38).
domain_priors:theater_ratio(competence_retention_exercise__simulation_as_sufficient, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, extractiveness, 0.42).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__simulation_as_sufficient, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__simulation_as_sufficient, "High-Fidelity Simulation as Sufficient Catastrophe-Avoidance Competence Exercise").
narrative_ontology:topic_domain(competence_retention_exercise__simulation_as_sufficient, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_retention_exercise__simulation_as_sufficient).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__simulation_as_sufficient, '4684b33c-96d0-446e-b79a-dd0ac32f5dd7').
narrative_ontology:cs_kernel_codification('4684b33c-96d0-446e-b79a-dd0ac32f5dd7', formalized).
narrative_ontology:cs_authority_grounding('4684b33c-96d0-446e-b79a-dd0ac32f5dd7', expertise).
narrative_ontology:cs_interpretation_layer_present('4684b33c-96d0-446e-b79a-dd0ac32f5dd7').
narrative_ontology:cs_reading_relation('4684b33c-96d0-446e-b79a-dd0ac32f5dd7', competence_retention_exercise__catastrophe_as_necessary, forecloses).
narrative_ontology:cs_reading_relation('4684b33c-96d0-446e-b79a-dd0ac32f5dd7', competence_retention_exercise__near_miss_as_bridge, coexists_with).
narrative_ontology:cs_axiom('4684b33c-96d0-446e-b79a-dd0ac32f5dd7', foundational, simulation_cognitive_procedural_equivalence).
narrative_ontology:cs_axiom_status(simulation_cognitive_procedural_equivalence, holdable).
narrative_ontology:cs_axiom_grounding('4684b33c-96d0-446e-b79a-dd0ac32f5dd7', simulation_cognitive_procedural_equivalence, empirically_contingent).
narrative_ontology:cs_axiom('4684b33c-96d0-446e-b79a-dd0ac32f5dd7', secondary, prevented_catastrophe_is_preferable_learning_substrate).
narrative_ontology:cs_axiom_status(prevented_catastrophe_is_preferable_learning_substrate, holdable).
narrative_ontology:cs_axiom_grounding('4684b33c-96d0-446e-b79a-dd0ac32f5dd7', prevented_catastrophe_is_preferable_learning_substrate, instrumental).
narrative_ontology:cs_reference_frame('4684b33c-96d0-446e-b79a-dd0ac32f5dd7', simulator_certification_as_competence_standard).
narrative_ontology:cs_drift_state('4684b33c-96d0-446e-b79a-dd0ac32f5dd7', post_high_fidelity_simulator_maturation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4684b33c-96d0-446e-b79a-dd0ac32f5dd7', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, simulator_vendors).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, training_program_administrators).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, regulatory_certifying_bodies).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, operations_management).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, frontline_operators).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, downstream_public_exposed_to_residual_risk).
narrative_ontology:constraint_vindicates(competence_retention_exercise__simulation_as_sufficient, cognitive_procedural_equivalence_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and mandate the simulator curriculum, set pass thresholds, and certify operators as competent based on simulator performance. Control the budget line that funds simulator infrastructure rather than exposure-based training or expanded near-miss investigation programs. Their institutional standing depends on the claim that simulation is sufficient — abandoning that claim would require justifying the cost of alternative competence-maintenance mechanisms they do not control.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, training_program_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Sell and service the high-fidelity simulator platforms that the sufficiency claim renders mandatory rather than supplementary. Revenue scales directly with the industry's acceptance that simulation constitutes genuine competence exercise rather than mere rehearsal. Face essentially no downside if the claim is later found overstated, since contracts are structured around delivered fidelity specifications, not downstream safety outcomes.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, simulator_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% Accept simulator hours and simulator performance scores as the legal and administrative basis for certification renewal. This lets them scale oversight without commissioning costly, dangerous, or logistically impossible real-event testing. Their credibility is now partially staked on the equivalence claim holding, since reversing it implicates decades of certifications issued on that basis.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, regulatory_certifying_bodies, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__simulation_as_sufficient, regulatory_certifying_bodies, agenda_setter).

% Rely on simulator-based certification to keep staffing pipelines moving and insurance/liability postures defensible ('our people are certified per industry-standard simulation protocols'). Benefit from lower operating cost relative to any real-exposure or catastrophe-preservation alternative, and from the legal cover the certification regime provides in post-incident review.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, operations_management, beneficiary,
    powerful, biographical, mobile, national).

% Undergo repeated simulator cycles as the sole mechanism by which their catastrophe-response competence is exercised and certified, despite having no institutional voice in whether simulator fidelity actually captures the physiological and decision-making demands of a real event. If a real catastrophe reveals a fidelity gap, they bear the direct consequence — professional, physical, sometimes fatal — while the certifying apparatus that vouched for their readiness bears little of it. Exit means leaving the profession; there is no alternate competence-certification pathway to move to.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, frontline_operators, payer,
    moderate, biographical, constrained, regional).

% Live or work near the facilities and systems whose operators are certified competent via simulation. Have no visibility into simulator fidelity validation and no standing to demand it. If the equivalence claim is wrong in some untested dimension, they absorb the tail risk of a mishandled real catastrophe without ever having been party to the decision that treated simulation as sufficient.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, downstream_public_exposed_to_residual_risk, payer,
    powerless, generational, trapped, regional).

% Include retired operators, incident investigators, and some safety researchers who argue that simulators cannot replicate the visceral stakes, physiological stress response, and irreversible-consequence cognition of a genuine catastrophe. They are largely excluded from curriculum-design conversations because their position implies either accepting periodic real catastrophes as pedagogically necessary (politically and morally untenable) or funding alternative competence mechanisms the current apparatus does not have budget lines for.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, catastrophe_experience_advocates, excluded,
    moderate, generational, constrained, national).

% Study transfer-of-training validity — whether simulator performance predicts real-event performance — across domains (aviation, nuclear, maritime, medicine). Publish comparative findings but have no enforcement power over certification standards; their findings are cited selectively by whichever side of the equivalence debate they support.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, independent_safety_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_retention_exercise__simulation_as_sufficient, diffuse).
narrative_ontology:fixing_cost_class(competence_retention_exercise__simulation_as_sufficient, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a repeatable, safe, scalable mechanism for exercising and verifying catastrophe-response competence without requiring operators to be exposed to actual catastrophic events, which would be either impossible to schedule, unacceptably dangerous, or destructive of the very infrastructure being protected.
% TRANSFER_FUNCTION: Moves the cost and risk of competence verification away from real-world catastrophic exposure and onto a capital-intensive training-and-certification apparatus; moves budget from alternative competence-maintenance investments (near-miss investigation depth, real-exposure rotations, redundant staffing) toward simulator procurement and licensing; moves epistemic authority over 'what counts as competence' from lived catastrophe experience to simulator-metric performance, concentrating that authority in vendors and certifying bodies.
% ABSENT_VOICES: Frontline operators who have quietly noted fidelity gaps (a scenario the simulator handles cleanly that behaved differently in a real near-miss) rarely have a formal channel to challenge certification adequacy without appearing to question their own competence. Catastrophe-experience advocates are structurally excluded because their position has no palatable operational translation.
% DISAPPEARANCE_RATIONALE: If the sufficiency claim were withdrawn overnight, certifying bodies would lose their legal basis for competence attestation, operations management would face an immediate staffing and liability crisis, simulator vendors would lose their primary market rationale, and the industry would be forced to either fund a genuinely validated alternative (expensive, slow) or operate with acknowledged, unverified competence gaps. This is not a constraint whose removal leaves the world unchanged — an entire training-industrial and certification apparatus is built on top of it.
% FOUNDING_PROBLEM: Real catastrophes are too rare, too dangerous, too destructive, and too ethically fraught to serve as the primary mechanism for training and re-certifying operators in catastrophe response; some substitute was needed to keep competence current between rare real events.
% FOUNDING_PROBLEM_CORROBORATION: Simulator vendors, certifying bodies, and operations management all attest the founding problem is fully solved by current simulation fidelity. Independent safety researchers and catastrophe-experience advocates — parties outside the beneficiary set — attest that transfer-of-training validity remains only partially established across domains, and that several high-consequence post-incident reviews (documented in aviation and nuclear near-miss literature) found operators handled scenarios differently under real stakes than their simulator scores predicted. No party outside the certifying apparatus itself attests the equivalence claim is fully settled.
narrative_ontology:disappearance_verdict(competence_retention_exercise__simulation_as_sufficient, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__simulation_as_sufficient, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__simulation_as_sufficient, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_retention_exercise__simulation_as_sufficient, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__simulation_as_sufficient, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__simulation_as_sufficient_tests).
:- end_tests(competence_retention_exercise__simulation_as_sufficient_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end, rising from 0.22) reflecting a genuine coordination function (competence must be maintained somehow) layered with growing institutional capture: as simulator infrastructure investment compounds, the industry becomes progressively less willing to fund or even investigate alternative validation (near-miss deep-dives, cross-domain transfer studies) that might surface fidelity gaps, because doing so would threaten sunk investment and certification legitimacy. Suppression (0.38, rising) captures the structural discouragement of operators and researchers who raise fidelity concerns — not violent coercion, but professional and institutional friction against challenging the equivalence claim. Theater ratio (0.31, rising) reflects a growing share of simulator time and certification ceremony that functions more to produce a certifiable record than to close any remaining fidelity gap. Accessibility collapse is moderate (0.55): alternative competence-verification mechanisms (extended real-exposure rotations, deeper near-miss forensics) are not physically impossible, just institutionally unfunded and career-discouraged, so the collapse is real but not total. Resistance (0.45) reflects the ongoing, if marginalized, presence of catastrophe-experience advocates and independent researchers who continue to contest the equivalence claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Simulator vendors, certifying bodies, and operations management sit near the beneficiary end: they collect revenue, administrative efficiency, or liability cover from the arrangement and hold arbitrage/mobile exit options that let them adjust posture if the claim becomes untenable. Frontline operators and the downstream public sit near the target end: they bear the consequence of any undetected fidelity gap, and their exit options (constrained/trapped) mean they cannot arbitrage away from the risk the way institutional actors can. Training program administrators are declared agenda_setter rather than pure beneficiary because they administer the standard rather than merely collecting from it, though the coordination function they administer also confers institutional legitimacy on them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (real catastrophes are too rare/dangerous to train on directly) remains partially live — simulation genuinely cannot be replaced by real-event training without reintroducing exactly the harms it was built to avoid. This blocks a simple 'mandatrophy resolved, abolish it' reading. But the founding problem's STATUS is contested rather than settled: independent evidence suggests the equivalence claim (simulation = structurally equivalent cognitive/procedural demand) is not fully validated across all catastrophe types, meaning the arrangement may have outrun what its own justification can support. Classifying this as tangled_rope rather than rope or snare captures exactly this: a genuine coordination function (rope-like) that has accreted asymmetric extraction and active suppression of contrary evidence (snare-like) without either canceling the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulator_fidelity_equivalence_uncertainty,
    'Does high-fidelity simulation actually replicate the full cognitive and physiological demand profile of a genuine catastrophe, or does it systematically omit dimensions (acute stress physiology, irreversibility cognition, moral weight of real consequence) that matter for competence?',
    'Longitudinal cross-domain comparison of simulator performance scores against real-event and near-miss performance outcomes, ideally using physiological stress markers (cortisol, heart-rate variability) alongside procedural accuracy, across aviation, nuclear, and maritime domains where both simulator and real-event data exist.',
    'If equivalence holds fully, the arrangement is closer to a genuine rope — real coordination with minimal extraction. If a persistent, undetected gap exists, the arrangement is closer to a snare dressed as coordination, since the certifying apparatus is vouching for a competence level it cannot verify.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulator_fidelity_equivalence_uncertainty, empirical, 'Whether simulator-based competence verification actually captures the full demand profile of real catastrophic events.').

omega_variable(
    kernel_reading_selection_pressure,
    'Among the three competing readings of the competence_retention_exercise kernel (simulation_as_sufficient, catastrophe_as_necessary, near_miss_as_bridge), what structural or institutional pressures determine which reading a given industry or organization adopts, and is that selection driven by evidence of transfer validity or by cost/liability convenience?',
    'Comparative institutional analysis: track which industries/organizations shifted between readings over time and correlate the shift with cost data, liability events, and published transfer-validity findings versus budget and vendor-relationship changes.',
    'If reading-selection tracks cost/liability convenience more than evidence, that supports classifying the dominant reading (simulation_as_sufficient) as partially extraction-driven rather than purely evidence-driven; if it tracks evidence, the tangled_rope classification should weight more toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether the dominant kernel reading is selected on evidentiary or institutional-convenience grounds.').

omega_variable(
    residual_tail_risk_distribution,
    'How is the residual tail risk — the probability that a real catastrophe exposes an undetected simulator fidelity gap — actually distributed between the certifying apparatus and the downstream public, and is that distribution disclosed to the public that bears it?',
    'Regulatory or academic audit of post-incident reviews where certified-competent operators underperformed relative to simulator scores, cross-referenced with whether affected communities were informed of residual uncertainty in the certification basis.',
    'A finding of undisclosed, concentrated tail risk on powerless downstream parties strengthens the case for the victim declaration and the tangled_rope classification''s asymmetric-extraction gate; a finding of well-disclosed, actuarially managed residual risk would weaken it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(residual_tail_risk_distribution, empirical, 'Whether the tail risk of fidelity gaps is disclosed to and consented to by the powerless parties who bear it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__simulation_as_sufficient, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0, 0.12).
narrative_ontology:measurement(comp_tr_t8, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 8, 0.16).
narrative_ontology:measurement(comp_tr_t16, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 16, 0.2).
narrative_ontology:measurement(comp_tr_t24, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 24, 0.24).
narrative_ontology:measurement(comp_tr_t32, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 32, 0.28).
narrative_ontology:measurement(comp_tr_t40, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(comp_be_t8, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 8, 0.27).
narrative_ontology:measurement(comp_be_t16, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 16, 0.32).
narrative_ontology:measurement(comp_be_t24, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 24, 0.36).
narrative_ontology:measurement(comp_be_t32, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 32, 0.39).
narrative_ontology:measurement(comp_be_t40, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(comp_su_t8, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 8, 0.24).
narrative_ontology:measurement(comp_su_t16, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 16, 0.28).
narrative_ontology:measurement(comp_su_t24, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 24, 0.32).
narrative_ontology:measurement(comp_su_t32, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 32, 0.35).
narrative_ontology:measurement(comp_su_t40, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__simulation_as_sufficient, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_retention_exercise__simulation_as_sufficient, 0.12).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, catastrophe_as_necessary).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, near_miss_as_bridge).

% DUAL FORMULATION NOTE:
% Part of the competence_retention_exercise kernel family (3 readings). This story (simulation_as_sufficient) shares the kernel with catastrophe_as_necessary and near_miss_as_bridge but is a structurally distinct constraint with its own ε: it describes an arrangement where simulator infrastructure is treated as the primary and sufficient competence-maintenance mechanism, with its own beneficiary/victim structure (vendors and certifiers as beneficiaries, operators and public as payers) that does not appear in the sibling readings, which describe arrangements where real catastrophes or near-misses respectively occupy that structural role instead.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
