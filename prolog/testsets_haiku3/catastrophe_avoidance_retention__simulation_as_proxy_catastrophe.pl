% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__simulation_as_proxy_catastrophe
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, []).

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
 *   constraint_id: catastrophe_avoidance_retention__simulation_as_proxy_catastrophe
 *   human_readable: Simulation-as-Catastrophe Functional Equivalence for Competence Retention
 *   domain: safety_engineering/organizational_learning/high_reliability_systems
 *
 * SUMMARY:
 *   High-reliability organizations in safety-critical domains (nuclear
 *   operations, aviation, emergency medicine, catastrophic event response)
 *   face a fundamental training problem: competence decays without exercise,
 *   but waiting for actual catastrophes to occur as training events is
 *   ethically indefensible and operationally inefficient. The
 *   simulation-as-proxy-catastrophe reading claims that sufficiently
 *   high-fidelity simulation constitutes genuine functional practice — that
 *   drills produce the same competence-maintenance effects as actual
 *   catastrophic events, so certification and competence oversight can be
 *   entirely mediated through simulated training. This reading instantiates
 *   one pole of a contested kernel about how organizations should maintain
 *   catastrophe-avoidance competence. The other readings ('catastrophe as
 *   necessary selector' and 'hybrid near-miss learning') dispute the
 *   functional equivalence claim from different angles: one argues actual
 *   catastrophes provide irreplaceable selection pressure; the other argues
 *   neither simulation nor catastrophe alone suffices, and competence is
 *   distributed across multiple learning channels.
 *
 * KEY AGENTS:
 *   - simulation_infrastructure_operators: institutional agents setting and controlling simulation scenarios, fidelity standards, and performance metrics — near-total control over what counts as 'equivalent' practice
 *   - regulatory_certification_bodies: institutional agents certifying competence based on simulation performance, receiving authority and liability reduction from the equivalence claim
 *   - organizational_risk_management: institutional beneficiary substituting scheduled, measurable drill cycles for catastrophe-frequency-based competence assessment
 *   - operational_personnel: moderate-power agents bearing identity-lock (professional certification depends entirely on simulation performance) and experiencing functionally different training from actual catastrophe
 *   - front_line_responders: powerless agents depending on operator competence but excluded from determining whether simulation adequately maintains it
 *   - catastrophe-domain_experts: excluded powerful agents holding empirical knowledge about gaps between simulation and catastrophic stress
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.68).
domain_priors:suppression_score(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.71).
domain_priors:theater_ratio(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, "Simulation-as-Catastrophe Functional Equivalence for Competence Retention").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, "safety_engineering/organizational_learning/high_reliability_systems").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, '411da48e-07c7-4fbe-b4ad-9dddb877dfb0').
narrative_ontology:cs_kernel_codification('411da48e-07c7-4fbe-b4ad-9dddb877dfb0', formalized).
narrative_ontology:cs_authority_grounding('411da48e-07c7-4fbe-b4ad-9dddb877dfb0', extraction).
narrative_ontology:cs_interpretation_layer_present('411da48e-07c7-4fbe-b4ad-9dddb877dfb0').
narrative_ontology:cs_reading_relation('411da48e-07c7-4fbe-b4ad-9dddb877dfb0', catastrophe_avoidance_retention__catastrophe_as_necessary_selector, coexists_with).
narrative_ontology:cs_reading_relation('411da48e-07c7-4fbe-b4ad-9dddb877dfb0', catastrophe_avoidance_retention__hybrid_near_miss_learning, influences).
narrative_ontology:cs_axiom('411da48e-07c7-4fbe-b4ad-9dddb877dfb0', foundational, simulation_functional_equivalence_to_catastrophe).
narrative_ontology:cs_axiom_status(simulation_functional_equivalence_to_catastrophe, holdable).
narrative_ontology:cs_axiom_grounding('411da48e-07c7-4fbe-b4ad-9dddb877dfb0', simulation_functional_equivalence_to_catastrophe, empirically_contingent).
narrative_ontology:cs_axiom('411da48e-07c7-4fbe-b4ad-9dddb877dfb0', secondary, centralized_certification_via_simulation).
narrative_ontology:cs_axiom_status(centralized_certification_via_simulation, holdable).
narrative_ontology:cs_axiom_grounding('411da48e-07c7-4fbe-b4ad-9dddb877dfb0', centralized_certification_via_simulation, conventional).
narrative_ontology:cs_reference_frame('411da48e-07c7-4fbe-b4ad-9dddb877dfb0', simulation_substitutes_for_catastrophe).
narrative_ontology:cs_drift_state('411da48e-07c7-4fbe-b4ad-9dddb877dfb0', contemporary_neuroscience_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('411da48e-07c7-4fbe-b4ad-9dddb877dfb0', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_infrastructure_operators).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, regulatory_certification_bodies).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, organizational_risk_management).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, operational_personnel).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, front_line_responders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, operational_personnel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, validate, and maintain high-fidelity simulation environments that claim functional equivalence to real catastrophic events. Control scenario content, difficulty progression, performance metrics, and certification standards. Benefit from sustained funding, organizational authority, and institutional legitimacy in competence assessment. Bear minimal exposure to actual catastrophic outcomes because training is mediated through simulation.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_infrastructure_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Certify organizations and personnel as competent on the basis of simulation performance. Reduce regulatory liability by substituting measurable simulation metrics for statistical catastrophe frequency. Maintain authority over competence standards without bearing direct catastrophic exposure. Simplify compliance auditing: simulation hours and scores replace real-world incident analysis.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, regulatory_certification_bodies, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, regulatory_certification_bodies, agenda_setter).

% Substitute scheduled, controlled, measured drill cycles for uncertainty about actual competence. Manage catastrophe avoidance through simulation scheduling and cost optimization. Reduce insurance liability exposure by demonstrating compliance training. Budget simulation costs predictably; actual catastrophes create unbudgeted, unquantifiable damage.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, organizational_risk_management, beneficiary,
    institutional, biographical, constrained, national).

% Required to maintain certification and employment by performing in simulations deemed equivalent to real catastrophic events. Experience simulations as fundamentally different from catastrophic events (no genuine stakes, no irreversible consequences, no full-body autonomic response, no memory consolidation via mortality salience). Bear the cognitive load of simulation training with uncertain transfer to actual catastrophe response. Professional identity depends on certification status, which is determined entirely by simulation performance.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, operational_personnel, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, operational_personnel, beneficiary).

% Depend on operational personnel trained via simulation to perform competently in actual catastrophes. Have no voice in whether simulation adequately maintains competence. Bear the catastrophic consequences if personnel trained only through simulation degrade under actual catastrophic conditions. Cannot exit the reliance on operator competence.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, front_line_responders, payer,
    powerless, immediate, trapped, local).

% Hold empirical knowledge about catastrophe psychology, stress physiology, and the limits of simulation transfer. Would dispute functional equivalence claims based on neuroscience of trauma, decision-making under actual mortality threat, and statistical analysis of post-catastrophe competence decay. Structurally excluded from simulation validation frameworks that assume equivalence rather than test it.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_domain_experts, excluded,
    powerful, generational, constrained, global).

% Have lived through actual organizational catastrophes and understood how simulation differed from the real event in ways that mattered for competence. Their experiential knowledge contradicts the functional-equivalence claim. Systematically excluded from scenario design and validation because their testimony would undermine simulation authority.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, organizational_learners_from_real_incidents, excluded,
    moderate, biographical, constrained, global).

% External position analyzing whether simulation fidelity, even at high technical sophistication, can substitute for the selection pressures of actual catastrophe without empirical validation. Measures the gap between simulation performance and actual competence under catastrophic stress.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, analytical_observer, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_infrastructure_operators).
narrative_ontology:fixing_cost_class(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains competence levels across a distributed organizational system without requiring actual catastrophic events as the training substrate. Centralizes and standardizes competence assessment so certification is portable, auditable, and decoupled from geographic catastrophe frequency. Enables competence maintenance in organizations that have been fortunate enough to avoid catastrophes.
% TRANSFER_FUNCTION: Transfers the burden of competence maintenance from catastrophe-based selection pressure (where only survivors demonstrate true competence, at human cost) to a regulated, simulated training regime. Moves the locus of competence validation from lived experience to metricated simulation performance. Concentrates certification authority in simulation-operating institutions.
% ABSENT_VOICES: Catastrophe-domain experts (trauma neurologists, decision-making researchers, post-incident organizational analysts) are excluded from simulation validation frameworks. Organizational practitioners who survived actual catastrophes and can testify about the gaps between simulation and reality are not systematically incorporated into scenario design. Front-line responders who depend on operator competence have no voice in whether simulation adequately maintains it.
% DISAPPEARANCE_RATIONALE: If the simulation-as-proxy-catastrophe constraint vanished, regulatory frameworks would need an alternative competence-verification substrate: either return to catastrophe-frequency-based statistics (ethically untenable, organizationally chaotic), or develop empirical validation studies comparing simulated vs. actually-catastrophic training cohorts (research-intensive, decades-long). Certification bodies would lose authority to certify without continuous incident data. Simulation infrastructure would shrink dramatically or be reframed as supplementary. Organizations would fragment into post-catastrophe-trained (high competence, high human cost) and pre-catastrophe-trained (untested competence, lower cost) cohorts.
% FOUNDING_PROBLEM: Organizations in high-consequence domains (nuclear, aviation, emergency response, medicine) need competence maintenance systems that do not require waiting for actual catastrophes to occur as training events. Catastrophe-based learning is ethically indefensible (incompatible with duty to minimize harm) and organizationally inefficient (long intervals between learning events, selection bias toward survivors). Simulation offers a way to decouple competence maintenance from actual catastrophe frequency.
% FOUNDING_PROBLEM_CORROBORATION: Safety engineers and organizational researchers affirm the founding problem: competence does decay with time since last high-stress event, and organizations need a mechanism to refresh it without waiting for catastrophe. However, neuroscientists and empirical researchers studying transfer of training under mortality-threat conditions dispute whether simulation adequately solves the problem — they document that simulation, even high-fidelity, produces different physiological and cognitive states than actual catastrophe, and these differences correlate with competence gaps under real stress. No corroboration from outside the simulation-operating institutions attests that simulation alone is sufficient for competence maintenance.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) reflects the constraint's asymmetry: simulation operators, regulators, and organizational risk managers benefit from a simplified, auditable, metricated competence system that transfers the burden of competence maintenance from catastrophe-based selection (with high human cost) to institutional authority. Operational personnel bear the cost: they must perform in simulation, accept certification as equivalent to real competence, and face uncertainty about actual performance under catastrophic stress. The measurement series shows extractiveness rising early (t=0 to t=20) as simulation infrastructure becomes institutionally embedded and regulatory authority crystallizes around simulation metrics, then plateaus (t=20 to t=40) as the constraint stabilizes — the institutionalization process completes within 20-25 time units. Theater_ratio rising from 0.28 to 0.44 indicates growing performative character: early simulation emphasized genuine training value; later simulation increasingly emphasizes certification metrics, compliance documentation, and stakeholder theater. Suppression_requirement rises from 0.62 to 0.73, reflecting growing institutional effort to maintain the equivalence claim against mounting empirical critique: more rigorous validation studies, exclusion of contradicting expert voices, and defensive framing of simulation limitations as 'expected gaps that do not compromise certification'.
 *
 * PERSPECTIVAL GAP:
 *   The simulation operators and regulators experience this constraint as pure coordination: 'We have solved the competence-maintenance problem by replacing catastrophe-dependent training with systematic, measurable simulation.' Operational personnel experience it as forced substitution: 'We are required to perform in simulations that we know from embodied experience are fundamentally different from actual catastrophe, and our certification and employment depend on simulation performance.' Front-line responders experience it as abstract risk: 'Our safety depends on people trained in environments that may not transfer to actual catastrophe, but we have no way to assess whether they are actually competent.' The engine computes these divergences from the structural data: operational_personnel sit at high directionality (payer, identity_locked, moderate power, constrained exit), placing them near the extraction target end; simulation operators sit at low directionality (beneficiary, institutional power, arbitrage exit), placing them near the subsidy end.
 *
 * DIRECTIONALITY LOGIC:
 *   Simulation operators: d ≈ 0.05–0.15 (full beneficiary). They control the rules, design the metrics, determine what counts as 'equivalent,' and collect the institutional authority and resources. They have arbitrage-grade exit: they could shift to other institutional domains without personal loss. Regulatory bodies: d ≈ 0.08–0.18 (beneficiary with minor cost). They receive authority, liability reduction, and simplified compliance auditing. They have institutional power and arbitrage exit. Organizational risk management: d ≈ 0.15–0.25 (beneficiary with shared cost). They benefit from predictable budgeting and reduced catastrophic liability, but bear some cost from simulation infrastructure investment. Operational personnel: d ≈ 0.68–0.78 (target). They bear the training burden, the identity-lock (certification depends entirely on simulation), and the uncertainty about actual competence. They have constrained exit: leaving means forfeiting professional standing. Front-line responders: d ≈ 0.85–0.95 (full target). They bear the catastrophic risk if simulation-trained personnel degrade under real stress, with no voice or exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids the mandatrophy classification by retaining a real coordination function: competence must be maintained, and simulation is the ethically preferable training substrate to actual catastrophes. This prevents the classification from drifting to pure snare. However, mandatrophy is incipient in the theater_ratio trajectory: as theater increases (certification performance becomes decoupled from actual competence), the functional mandate (maintaining real competence) risks becoming secondary to the administrative mandate (certifying simulation performance). If theater_ratio reaches 0.60+, the constraint becomes mandatrophic: it will persist as a certification apparatus long after its competence-maintenance function has atrophied, sustained by institutional inertia and regulatory capture. The constraint is currently tangled_rope because the coordination function is real (maintaining competence is genuinely necessary) and the extraction is substantial but not total (operators and regulators benefit significantly, but operational personnel and responders are not stripped of all resources). If the founding problem (competence maintenance) ever becomes obsolete — if organizations develop non-simulation alternatives that are demonstrably superior — the constraint would become piton: persisting as theater and institutional theater long after its function evaporated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_transfer_validity,
    'Does high-fidelity simulation actually maintain catastrophe-avoidance competence at levels equivalent to real catastrophic training events?',
    'Empirical longitudinal cohort study comparing simulated-trained operators (trained only through simulation) with post-catastrophe-trained operators (operators who survived real catastrophic events and subsequent retraining). Measure competence under simulated and actual high-stress conditions, with sufficient sample size and follow-up time to detect decay. Control for selection bias (post-catastrophe cohorts are survivors, hence non-random).',
    'If simulation maintains competence equivalently: the constraint is validated as genuine coordination with asymmetric distribution. If simulation shows measurable competence gaps under actual catastrophe: the constraint reclassifies from tangled_rope toward snare, and regulatory frameworks require substantial reform. If gaps exist but are small enough that regulatory risk tolerance is maintained: the constraint remains tangled_rope but with documented limits requiring supplementary training.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_transfer_validity, empirical, 'The core empirical claim the reading hangs on: can high-fidelity simulation substitute for actual catastrophic training?').

omega_variable(
    exclusion_vs_epistemic_gate,
    'Is the exclusion of catastrophe-domain experts from simulation validation frameworks structural institutional design, or the result of legitimate technical specialization?',
    'Audit of simulation validation bodies and regulatory certification frameworks to determine: (1) who is explicitly authorized to contribute to equivalence validation; (2) what expertise is required; (3) whether interdisciplinary expertise from trauma neuroscience, stress physiology, and organizational learning is included; (4) what happens when experts outside the simulation-operating institutions attempt to contribute critical findings.',
    'If exclusion is structural design: suppression_requirement values are correct, and the constraint is extractive by institutional gating. If exclusion is incidental to specialization: the framework is permeable, and suppression_requirement may be overstated. If the framework is designed to be technically closed but empirical critiques are nonetheless filtered: the suppression is internalized as well as structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_vs_epistemic_gate, empirical, 'Whether the constraint''s extraction is maintained through institutional gatekeeping of disconfirming evidence.').

omega_variable(
    kernel_reading_boundary,
    'Does the simulation-as-proxy-catastrophe reading genuinely foreclose the catastrophe-as-necessary-selector reading, or do they coexist as different organizational strategies?',
    'Examine whether any single organization or regulatory framework can adopt simulation-as-proxy-catastrophe AND catastrophe-as-necessary-selector simultaneously. If frameworks permit both (e.g., ''simulation for routine maintenance, catastrophe-learning for rare major events''), the readings coexist. If adopting one mandates rejecting the other, they foreclose.',
    'If readings coexist: this reading influences but does not eliminate the sibling reading, and both can be live positions in the organizational ecosystem. If readings foreclose: this reading''s institutional ascendance suppresses the alternative, suggesting stronger structural conflict than mere difference of opinion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether this reading''s institutional adoption precludes the catastrophe-as-necessary-selector reading or permits coexistence.').

omega_variable(
    competence_decay_rate_uncertainty,
    'What is the actual rate of competence decay in catastrophe-avoidance domains, and how frequently must retraining occur to maintain minimal safe levels?',
    'Prospective measurement of competence decay in post-catastrophe-trained cohorts without retraining, and comparison with decay rates in simulation-trained cohorts. Establish empirically the minimum retraining frequency required for safe competence maintenance.',
    'If decay is rapid and frequent retraining is mandatory: simulation becomes more economically necessary (supports the reading). If decay is slow and infrequent retraining is sufficient: competence maintenance via other mechanisms becomes viable, and the necessity of simulation infrastructure is reduced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competence_decay_rate_uncertainty, empirical, 'The temporal characteristics of competence decay, which determine retraining frequency requirements and the economic case for simulation infrastructure.').

omega_variable(
    theater_ratio_ratchet_risk,
    'Will theater_ratio continue rising toward 0.60+ as simulation metrics become increasingly decoupled from actual competence measurement?',
    'Continued measurement of theater_ratio alongside independent competence assessments (post-incident performance, external audits, catastrophe outcomes where they occur). If theater_ratio rises while competence validation remains external, the constraint is becoming mandatrophic. If theater_ratio stabilizes or external competence measures remain coupled, the constraint retains its function.',
    'If mandatrophy occurs: the constraint will persist as institutional theater long after its functional mandate atrophies, generating zombie certification that does not correspond to actual competence. If theater plateaus: the constraint stabilizes as a functioning extraction mechanism with real coordination benefits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_ratchet_risk, empirical, 'Whether the simulation-certification apparatus risks separating from actual competence maintenance, becoming a self-sustaining institutional theater.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 0, 0.28).
narrative_ontology:measurement(cata_tr_t5, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 5, 0.31).
narrative_ontology:measurement(cata_tr_t10, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 10, 0.34).
narrative_ontology:measurement(cata_tr_t15, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 15, 0.37).
narrative_ontology:measurement(cata_tr_t20, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 20, 0.39).
narrative_ontology:measurement(cata_tr_t25, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 25, 0.41).
narrative_ontology:measurement(cata_tr_t30, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 30, 0.42).
narrative_ontology:measurement(cata_tr_t35, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 35, 0.43).
narrative_ontology:measurement(cata_tr_t40, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 40, 0.44).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(cata_be_t5, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(cata_be_t10, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(cata_be_t15, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(cata_be_t20, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(cata_be_t25, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(cata_be_t30, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(cata_be_t35, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(cata_be_t40, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(cata_su_t5, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 5, 0.64).
narrative_ontology:measurement(cata_su_t10, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(cata_su_t15, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(cata_su_t20, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(cata_su_t25, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(cata_su_t30, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(cata_su_t35, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 35, 0.72).
narrative_ontology:measurement(cata_su_t40, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 40, 0.73).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.14).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_as_necessary_selector).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, hybrid_near_miss_learning).

% DUAL FORMULATION NOTE:
% This constraint and its sibling readings (catastrophe_as_necessary_selector, hybrid_near_miss_learning) are members of the catastrophe_avoidance_retention constraint family. Each story represents one reading of the contested kernel about how organizations should maintain competence in catastrophe-avoidance domains. This reading (simulation_as_proxy_catastrophe) claims functional equivalence between high-fidelity simulation and actual catastrophic events. The sibling readings dispute this equivalence claim from different angles: catastrophe_as_necessary_selector argues actual catastrophes provide irreplaceable selection pressure; hybrid_near_miss_learning argues competence is distributed across multiple learning channels. The three readings coexist as different organizational strategies adopted by different institutional communities, but this reading's regulatory institutionalization creates structural pressure on the siblings by concentrating authority in simulation-operating institutions. All three stories should be linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
