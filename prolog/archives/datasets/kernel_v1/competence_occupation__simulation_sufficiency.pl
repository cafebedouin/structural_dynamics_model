% ============================================================================
% CONSTRAINT STORY: competence_occupation__simulation_sufficiency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__simulation_sufficiency, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: competence_occupation__simulation_sufficiency
 *   human_readable: Simulation Sufficiency for Competence Occupation
 *   domain: high_reliability_organizations/safety_training
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested kernel
 *   'competence occupation' — the claim that simulation-based drills
 *   constitute sufficient exercise to occupy the competence kernel and
 *   prevent skill decay. The reading emerges from a specific institutional
 *   settlement: regulatory mandates (FAA, NRC) have converged on
 *   simulation-based training as the primary evidence of competence
 *   maintenance, simulation vendors have become economically powerful through
 *   this standardization, and the empirical question ('is simulation
 *   sufficient?') has been largely removed from active contestation. This
 *   constraint exhibits high extractiveness (0.58) and high suppression
 *   (0.62) because the regulatory mandate prevents experimentation with
 *   alternative verification methods, line operators cannot refuse the
 *   training requirement, and the simulation industry benefits financially
 *   from expansion of fidelity/frequency mandates. Theater ratio (0.68)
 *   reflects that certification compliance is decoupled from actual
 *   competence measurement — passing a simulator does not guarantee
 *   operational competence, but certification requires simulator passage. The
 *   measurement trajectory shows extractiveness and theater increasing over
 *   the 10-year interval (t=0 to t=10), modeling the regulatory ratchet: as
 *   simulation fidelity requirements expand and compliance becomes the
 *   primary observable, the constraint's extraction mechanism strengthens.
 *   This constraint is the upstream pump that supplies demand to the
 *   simulation industry; actual skill decay rates remain largely invisible
 *   unless catastrophic failure occurs.
 *
 * KEY AGENTS:
 *   - Line Operators: Primary victims (powerless/trapped) — mandated to spend training hours in simulation, cannot refuse certification, skill decay unmonitored except via failure
 *   - Operating Organizations: Institutional actors (institutional/arbitrage) — benefit from compliance demonstration and insurance reductions; coordinate on safety via simulation but also subject to vendor influence
 *   - Simulation Training Vendors: Primary beneficiaries (powerful/arbitrage) — capture recurring revenue through certification expansion; influence regulatory standards-setting; have strong incentive to frame simulation sufficiency as settled
 *   - Regulatory Certification Authorities: Institutional actors (institutional/arbitrage) — maintain simulation requirements through mandate enforcement; see their own process as performative (piton perspective); resist deeper competence verification mechanisms
 *   - Safety Engineering Community: Organized actors (organized/mobile) — experience mixed function — coordinate on safety mechanisms but also extract publication opportunity and funding by validating vendor claims
 *   - Competence Assessment Reform Coalition: Organized actors (organized/constrained) — propose alternative verification pathways (line audits, hybrid protocols); structural sunset logic via competence science advancement
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the institutional choice as an immutable law of human skill maintenance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__simulation_sufficiency, 0.58).
domain_priors:suppression_score(competence_occupation__simulation_sufficiency, 0.62).
domain_priors:theater_ratio(competence_occupation__simulation_sufficiency, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, extractiveness, 0.58).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__simulation_sufficiency, tangled_rope).
narrative_ontology:human_readable(competence_occupation__simulation_sufficiency, "Simulation Sufficiency for Competence Occupation").
narrative_ontology:topic_domain(competence_occupation__simulation_sufficiency, "high_reliability_organizations/safety_training").

domain_priors:requires_active_enforcement(competence_occupation__simulation_sufficiency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__simulation_sufficiency, '113239bd-ea0a-4cce-b621-79f7519ee894').
narrative_ontology:cs_kernel_codification('113239bd-ea0a-4cce-b621-79f7519ee894', formalized).
narrative_ontology:cs_authority_grounding('113239bd-ea0a-4cce-b621-79f7519ee894', extraction).
narrative_ontology:cs_interpretation_layer_present('113239bd-ea0a-4cce-b621-79f7519ee894').
narrative_ontology:cs_reading_relation('113239bd-ea0a-4cce-b621-79f7519ee894', competence_occupation__real_incident_necessity, coexists_with).
narrative_ontology:cs_reading_relation('113239bd-ea0a-4cce-b621-79f7519ee894', competence_occupation__hybrid_occupation, influences).
narrative_ontology:cs_axiom('113239bd-ea0a-4cce-b621-79f7519ee894', foundational, simulation_fidelity_occupation).
narrative_ontology:cs_axiom_status(simulation_fidelity_occupation, holdable).
narrative_ontology:cs_axiom_grounding('113239bd-ea0a-4cce-b621-79f7519ee894', simulation_fidelity_occupation, empirically_contingent).
narrative_ontology:cs_axiom('113239bd-ea0a-4cce-b621-79f7519ee894', foundational, compliance_as_competence_proxy).
narrative_ontology:cs_axiom_status(compliance_as_competence_proxy, holdable).
narrative_ontology:cs_axiom_grounding('113239bd-ea0a-4cce-b621-79f7519ee894', compliance_as_competence_proxy, instrumental).
narrative_ontology:cs_reference_frame('113239bd-ea0a-4cce-b621-79f7519ee894', simulation_first_competence_assurance).
narrative_ontology:cs_drift_state('113239bd-ea0a-4cce-b621-79f7519ee894', contemporary_competence_science_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('113239bd-ea0a-4cce-b621-79f7519ee894', '').
narrative_ontology:cs_kernel_id(competence_occupation__simulation_sufficiency, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, simulation_training_vendors).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, compliance_certification_bodies).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, line_operators).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, actual_safety_outcomes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LINE OPERATOR (SNARE) — Trapped in mandatory simulation-based compliance regimen; cannot exit training requirement or refuse certification; skill decay unmonitored unless failure occurs in field. Extraction mechanism: operator bears cost of training time, certification maintenance, and degraded competence (if simulation fidelity insufficient) while organization captures compliance checkmark and insurance benefit. Operator experiences maximum suppression — refusal to participate results in termination or grounding.
constraint_indexing:constraint_classification(competence_occupation__simulation_sufficiency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: OPERATING ORGANIZATION (ROPE) — Institutional actor (airlines, nuclear plants, military squadrons) benefits from simulation certification: demonstrates compliance to regulators, reduces insurance premiums, avoids catastrophic incident liability. Experiences constraint as coordination mechanism — the simulation protocol solves the legitimate problem of demonstrating competence to external authorities. Net beneficiary but also genuinely coordinates (reduces true operational risk when simulation fidelity is adequate).
constraint_indexing:constraint_classification(competence_occupation__simulation_sufficiency, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: SIMULATION TRAINING VENDOR (SNARE) — Powerful institutional actor (CAE, THALES, military training contractors) with arbitrage exit. Benefits from expansion of simulation-based training mandates; revenue scales with frequency and fidelity requirements. Extraction mechanism: vendor captures compliance margin — training compliance becomes the observable, not actual competence. Vendor has strong incentive to frame simulation sufficiency as settled (axiom: simulation_fidelity_occupation) and resist real-incident feedback as 'outlier learning'. Acts as primary beneficiary through regulatory capture and standards-setting influence.
constraint_indexing:constraint_classification(competence_occupation__simulation_sufficiency, snare,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SAFETY ENGINEERING COMMUNITY (TANGLED ROPE) — Organized agents (human factors researchers, HRO researchers, incident investigators) with mobile exit options. Coordinate on safety mechanisms but also experience extraction: research funding increasingly tied to simulation validation; critical incident data often restricted to legal/insurance proceedings (suppressed from research). Mixed function — genuine coordination on competence mechanisms but also asymmetric incentive to validate vendor claims (publication bias toward 'simulation works' because funding from simulation industry).
constraint_indexing:constraint_classification(competence_occupation__simulation_sufficiency, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: COMPETENCE ASSESSMENT REFORM COALITION (SCAFFOLD) — Organized agents (incident investigation boards, pilot unions, safety advocates) proposing alternative verification pathways: line audits, competency observation programs, incident-driven recertification triggers. Sees simulation-sufficiency mandate as temporary coordination failure with structural sunset. Exit path: hybrid occupation protocols (simulation + line observation + incident-triggered recertification) that de-center simulation as sole occupier. Sunset mechanism: as competence science improves, simulation fidelity requirements become obsolete or are supplemented by more sensitive measures.
constraint_indexing:constraint_classification(competence_occupation__simulation_sufficiency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY CERTIFICATION AUTHORITY (PITON) — Institutional actor (FAA, NRC, national aviation authorities) that mandates simulation-based certification. Sees its own process as largely performative: certification passes or fails based on simulation metrics, but these metrics are weak proxies for actual skill. Maintains simulation requirement through institutional inertia — alternative verification (line audits, incident tracking) would require deeper engagement with operators and organizations. Theater ratio (0.68) reflects that certification checklist is largely ritual; actual competence verification remains epistemically weak. Maintains because alternatives aren't fully in place, not because it works.
constraint_indexing:constraint_classification(competence_occupation__simulation_sufficiency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal scope, skill maintenance in complex domains inherently requires distributed practice over time. No single intervention (simulation, lecture, incident exposure) can occupy the competence kernel. This perspective frames the debate as reflecting an immutable law of human motor learning and complex skill maintenance — decay is inevitable without continuous multi-mechanism reinforcement. However, this reading is a false summit: the structural data reveals that treating simulation sufficiency as natural law naturalizes a contingent institutional choice (privileging simulation fidelity metrics over actual field performance metrics). The constraint operates through regulatory mandate and vendor incentive, not through laws of learning.
constraint_indexing:constraint_classification(competence_occupation__simulation_sufficiency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__simulation_sufficiency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(competence_occupation__simulation_sufficiency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(competence_occupation__simulation_sufficiency, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__simulation_sufficiency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(competence_occupation__simulation_sufficiency, TR),
    TR >= 0.70.

:- end_tests(competence_occupation__simulation_sufficiency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The simulation industry captures recurring revenue through mandated training expansion; regulators capture compliance evidence without investing in deeper competence verification; line operators bear cost (time, opportunity cost, potential skill gap if fidelity insufficient) without corresponding benefit. The extraction is substantial but not maximal because operating organizations genuinely benefit (insurance reduction, liability protection) and the coordination function (demonstrating safety to external stakeholders) is real. Suppression (0.62): High. Multiple mechanisms prevent experimentation with alternative verification: regulatory mandate prohibits most alternatives; incident data restricted by legal/insurance privilege; career risk for operators who refuse or criticize training; publication bias in competence research favoring simulation industry findings. Theater ratio (0.68): High-moderate. Certification compliance is primarily performative: passing a simulator demonstrates regulatory adherence, not actual operational skill. Reviewers of certification cannot access operational performance data to validate the simulator-to-field transfer. The theater has increased over the measurement interval as fidelity requirements have expanded without corresponding competence validation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full perspectival range from structured institutional incentives. The vendor sees rope (coordination) — they are solving a real problem (demonstrating competence to regulators). The operating organization sees rope (coordination) — the constraint demonstrates safety to external stakeholders and reduces insurance costs. The line operator sees snare (pure extraction) — trapped in mandatory training with no choice and no corresponding skill verification. The reform coalition sees scaffold (temporary problem with sunset) — hybrid occupation protocols and improved competence metrics will replace simulation-only mandates. The regulatory authority sees piton (degraded ritual) — certification requirement is maintained through inertia, not function. The safety engineering community sees tangled rope (mixed coordination and extraction) — genuine safety function but also captured by vendor incentives. The civilizational analytical observer risks seeing a mountain (immutable law) but the structural data reveals false summit: simulation sufficiency is framed as natural law but operates through regulatory mandate and vendor benefit concentration.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derives from structural position — who benefits, who bears costs, what exit options exist. Simulation vendors have low d (beneficiaries with strong exit — they can arbitrage between regulatory markets, customer bases, and alternative training products). Line operators have high d (victims with no exit — trapped by employment and regulatory mandate). Operating organizations have moderate-low d (mixed beneficiary/victim — capture compliance benefit but also trapped by regulatory requirement). Safety engineering community has moderate d (they benefit from research funding tied to simulation validation but have some exit — can propose alternatives or seek independent funding). Reform coalition has moderate d (organized actors with mobile exit but constrained by regulatory barriers to implementation). The analytical observer has d ≈ 0.72 (derived from canonical analytical power value). The false summit detection will flag the mountain perspective: because the constraint declares beneficiaries (simulation vendors), the engine computes that a genuine natural law would not concentrate benefits on identifiable actors. FSM fires and the mountain is reclassified (typically to tangled_rope) — revealing that the 'natural law of skill decay' framing naturalizes what is actually a contingent institutional arrangement.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fidelity_sufficiency_threshold,
    'What simulation fidelity threshold constitutes ''sufficient occupation'' of the competence kernel for a given operational task?',
    'Longitudinal competence tracking: correlate simulator performance (across fidelity levels) with actual line performance metrics (error rates, critical decision quality); identify fidelity floor below which correlation breaks. Compare against real-incident learning curves.',
    'If threshold exists and is measurable: simulation sufficiency becomes a technical engineering problem (optimize fidelity). If threshold is task-dependent or context-dependent without stable empirical anchor: current mandates may be extractive theater disguised as competence maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fidelity_sufficiency_threshold, empirical, 'Whether simulation fidelity has a measurable threshold for competence occupation').

omega_variable(
    decay_measurement_accessibility,
    'Can skill decay be measured directly in operational personnel without incident, or only inferred from incident rates?',
    'Development of non-incident competence metrics (line observation protocols, decision-tree performance scoring, tacit knowledge probes); validation against actual incident frequency; comparison of decay rates measured proactively vs retroactively via incident analysis.',
    'If measurable proactively: competence occupation becomes observable independent of incident frequency; simulation sufficiency claim can be falsified. If measurable only retroactively via incidents: current compliance model is fundamentally reactive, and simulation-based prediction claims are speculative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decay_measurement_accessibility, empirical, 'Whether skill decay can be measured directly without incident').

omega_variable(
    authenticity_of_simulation_threat,
    'Does the absence of authentic catastrophic consequence (death, injury, system failure) in simulation create a fundamental occupancy gap that fidelity cannot bridge?',
    'Comparative study: competence decay trajectories in simulation-trained cohorts vs incident-exposed cohorts vs hybrid-trained cohorts (simulation + line observation + controlled incident exposure). Measure via objective task performance, decision quality under time pressure, and error recovery.',
    'If authenticity is fundamental: real_incident_necessity reading becomes structurally dominant; simulation-sufficiency claim is falsified, and hybrid occupation becomes required. If authenticity effects are marginal (< 5% variance): simulation sufficiency claim is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authenticity_of_simulation_threat, empirical, 'Whether authentic catastrophic consequence is necessary for competence occupation').

omega_variable(
    reading_vs_natural_law_ambiguity,
    'Is the debate over competence occupation driven by genuine empirical uncertainty about human skill maintenance, or by institutional incentives to frame simulation sufficiency as settled?',
    'Textual and historical analysis: track how competence occupation has been framed in regulatory documents (1990s-present); identify moments when empirical contestation was classified as ''settled'' vs when new evidence prompted reclassification. Analyze funding flows and publication bias in competence research.',
    'If institutional incentives dominate: current ''simulation sufficiency'' mandate is a reading of the competence kernel, not a natural law. If genuine empirical uncertainty: debate reflects legitimate scientific pluralism. This omega documents whether the false summit detector should fire.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_natural_law_ambiguity, conceptual, 'Whether debate reflects empirical uncertainty or institutional incentive capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__simulation_sufficiency, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_sim_tr_t0, competence_occupation__simulation_sufficiency, theater_ratio, 0, 0.45).
narrative_ontology:measurement(comp_sim_tr_t5, competence_occupation__simulation_sufficiency, theater_ratio, 5, 0.58).
narrative_ontology:measurement(comp_sim_tr_t10, competence_occupation__simulation_sufficiency, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(comp_sim_be_t0, competence_occupation__simulation_sufficiency, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(comp_sim_be_t5, competence_occupation__simulation_sufficiency, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(comp_sim_be_t10, competence_occupation__simulation_sufficiency, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comp_sim_su_t0, competence_occupation__simulation_sufficiency, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(comp_sim_su_t5, competence_occupation__simulation_sufficiency, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(comp_sim_su_t10, competence_occupation__simulation_sufficiency, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__simulation_sufficiency, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, competence_occupation__hybrid_occupation).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, competence_occupation__real_incident_necessity).

% DUAL FORMULATION NOTE:
% The competence_occupation kernel has three structurally distinct readings, each corresponding to a different constraint story with different epsilon values and different beneficiary/victim structures. All three readings compete to define what constitutes sufficient occupation of the competence kernel. They are linked via network.affects_constraints and share the same kernel_id. Each story is ε-invariant within its own reading, but the three stories differ in epsilon because they measure different observables: simulation_sufficiency treats training compliance as the observable (medium epsilon = 0.58); hybrid_occupation treats consensus on multi-mechanism configuration as the observable (higher epsilon because consensus is hard to achieve); real_incident_necessity treats incident frequency as the observable (potentially lower epsilon if incidents are genuinely rare, or higher if the reading claims incidents are suppressed from analysis). The three readings do not merge into one constraint — they are three separate stories linked by kernel identity, not three perspectives within one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_occupation__simulation_sufficiency, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
