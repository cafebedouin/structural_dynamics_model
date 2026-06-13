% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__hybrid_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__hybrid_dependency, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: competence_exercise_requirement__hybrid_dependency
 *   human_readable: Hybrid Competence Maintenance Regime (Simulation + Real-World Anchoring)
 *   domain: safety/organizational_learning
 *
 * SUMMARY:
 *   Aviation competence certification rests on a hybrid regime: pilots must
 *   demonstrate mastery through high-fidelity simulators (where engine
 *   failures, system emergencies, and rare scenarios can be repeatedly and
 *   safely practiced) AND through periodic real-world anchoring (line
 *   operations, non-jeopardy check flights, line audits) where actual
 *   aircraft systems, crew coordination under operational pressure, and the
 *   irreducible complexity of live operations cannot be fully replicated.
 *   This constraint instantiates the HYBRID_DEPENDENCY reading of the
 *   competence_exercise_requirement kernel. It is NOT the
 *   simulation_as_adequate reading (which holds high-fidelity simulation
 *   alone is sufficient) nor the catastrophe_as_necessary reading (which
 *   holds only genuine jeopardy provides irreducible exercise). This reading
 *   asserts both simulation AND real-world anchoring are necessary; neither
 *   alone is sufficient; the regime must enforce both. The constraint is
 *   claimed as tangled_rope: it coordinates a genuine problem (verifiable
 *   competence across a safety-critical system) AND it extracts from those
 *   required to perform under the regime (crews bear time, scheduling
 *   friction, evaluation anxiety). The metrics author a substantially
 *   extractive, actively enforced arrangement with rising theater component
 *   (simulator-based evaluation drifting toward proxy-goal dynamics over the
 *   interval).
 *
 * KEY AGENTS:
 *   - operational_crews: structural payers, subject to mandated hybrid training with time/schedule costs and evaluation burden
 *   - junior_pilots: powerless, identity_locked payers bearing early-career training costs and dependency on certification pathways
 *   - training_infrastructure_operators: institutional agenda-setters, beneficiaries of sustained simulator demand
 *   - certification_authorities: institutional agenda-setters, design and enforce hybrid standard
 *   - airline_management: powerful beneficiaries (liability standardization) and partial payers (facility costs)
 *   - aviation_safety_board: observer seat, investigates competence-gap incidents
 *   - catastrophe_advocates: excluded, would argue only real jeopardy provides irreducible exercise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__hybrid_dependency, 0.62).
domain_priors:suppression_score(competence_exercise_requirement__hybrid_dependency, 0.71).
domain_priors:theater_ratio(competence_exercise_requirement__hybrid_dependency, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, extractiveness, 0.62).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__hybrid_dependency, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__hybrid_dependency, "Hybrid Competence Maintenance Regime (Simulation + Real-World Anchoring)").
narrative_ontology:topic_domain(competence_exercise_requirement__hybrid_dependency, "safety/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_requirement__hybrid_dependency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__hybrid_dependency, '87390dc8-c3b7-4495-8d81-8ef10bcb3c59').
narrative_ontology:cs_kernel_codification('87390dc8-c3b7-4495-8d81-8ef10bcb3c59', fixed_text).
narrative_ontology:cs_authority_grounding('87390dc8-c3b7-4495-8d81-8ef10bcb3c59', extraction).
narrative_ontology:cs_interpretation_layer_present('87390dc8-c3b7-4495-8d81-8ef10bcb3c59').
narrative_ontology:cs_reading_relation('87390dc8-c3b7-4495-8d81-8ef10bcb3c59', competence_exercise_requirement__simulation_as_adequate_exercise, forecloses).
narrative_ontology:cs_reading_relation('87390dc8-c3b7-4495-8d81-8ef10bcb3c59', competence_exercise_requirement__catastrophe_as_necessary_anchor, coexists_with).
narrative_ontology:cs_axiom('87390dc8-c3b7-4495-8d81-8ef10bcb3c59', foundational, simulation_necessary_but_insufficient).
narrative_ontology:cs_axiom_status(simulation_necessary_but_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('87390dc8-c3b7-4495-8d81-8ef10bcb3c59', simulation_necessary_but_insufficient, empirically_contingent).
narrative_ontology:cs_axiom('87390dc8-c3b7-4495-8d81-8ef10bcb3c59', foundational, real_world_anchoring_irreducible).
narrative_ontology:cs_axiom_status(real_world_anchoring_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('87390dc8-c3b7-4495-8d81-8ef10bcb3c59', real_world_anchoring_irreducible, empirically_contingent).
narrative_ontology:cs_axiom('87390dc8-c3b7-4495-8d81-8ef10bcb3c59', secondary, catastrophe_mandate_ethically_intolerable).
narrative_ontology:cs_axiom_status(catastrophe_mandate_ethically_intolerable, holdable).
narrative_ontology:cs_axiom_grounding('87390dc8-c3b7-4495-8d81-8ef10bcb3c59', catastrophe_mandate_ethically_intolerable, deontological).
narrative_ontology:cs_reference_frame('87390dc8-c3b7-4495-8d81-8ef10bcb3c59', competence_via_hybrid_simulation_and_operations).
narrative_ontology:cs_drift_state('87390dc8-c3b7-4495-8d81-8ef10bcb3c59', contemporary_regulatory_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('87390dc8-c3b7-4495-8d81-8ef10bcb3c59', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, training_infrastructure_operators).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, certification_authorities).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, airline_management).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, operational_crews).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, junior_pilots).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, junior_pilots).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, airline_management).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Pilots and flight engineers operating commercial aircraft under the hybrid competence regime. They must complete mandated simulator sessions (scheduled regularly, time-intensive, scheduling friction with operational rosters), participate in line audits (observational oversight during actual operations, psychological burden of evaluation during duty), and maintain real-aircraft time on assigned routes or check flights (scheduling constraint, operational opportunity cost). The regime justifies this burden as necessary for safety; crews increasingly contest the cost-to-benefit ratio, particularly as simulator training drifts toward check-ride optimization rather than authentic scenario practice. Exit means career change or violation of certification requirements (not realistic for established professionals).
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, operational_crews, payer,
    moderate, biographical, constrained, global).

% Aspiring commercial pilots undergoing initial training and early-career development. They must log extensive simulator hours (often at their own expense during early career, 500-1000 hours typical before first line operations), demonstrate competence in structured scenarios, pass line audits with perfect attendance, and accumulate real-aircraft time under supervision. The regime is justified partly as their protection (ensures they are genuinely competent before independent command), but it creates dependency on institutional pathways, certification authorities, and training infrastructure. Professional identity is fused with the certification system — career prospects depend entirely on passing the regime. Exit (leaving aviation, pursuing alternative credentials) carries identity cost proportional to prior commitment.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, junior_pilots, payer,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__hybrid_dependency, junior_pilots, beneficiary).

% Organizations that operate simulator facilities, conduct structured training sessions, and administer recurrent competence requirements. They design simulator curricula, set training-session prices, control access to training slots, and sell ancillary services (debriefing, scenario customization). They benefit directly from mandated simulator training: as requirements expand (more hours mandated, higher frequency), utilization increases and revenue grows. They have arbitrage options: pivot to different aviation domains (helicopter, military, unmanned systems), adjacent industries (maritime, industrial process control), or international markets with different regulatory regimes.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, training_infrastructure_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Government regulatory bodies (FAA, EASA, national authorities) that establish competence standards, define what constitutes adequate simulation and real-world anchoring, mandate recurrence frequencies, and conduct oversight audits. They design and enforce the hybrid regime. They justify it as necessary for public safety and industry confidence. They conduct regulatory inspections, levy penalties for non-compliance, and revise standards based on incident investigation. They do not directly profit but maintain institutional authority and resource allocation through the regime's administration.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, certification_authorities, agenda_setter,
    institutional, generational, analytical, global).

% Airlines that employ flight crews and must allocate resources to comply with the hybrid regime. They fund simulator facility costs, manage crew scheduling around training requirements, maintain compliance infrastructure, and enforce pilot participation. They benefit from a uniformly certified workforce (reduces their liability variance, creates industry-standard credibility, offloads training design to external authorities). They experience the regime as a constraint on operational efficiency (scheduling friction, training-slot availability) but accept it because all competitors face the same constraint (level playing field, liability standardization reduces their disadvantage relative to peers).
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, airline_management, beneficiary,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__hybrid_dependency, airline_management, payer).

% Independent agencies (NTSB, AAIB, national accident investigation authorities) that investigate accidents and incidents, correlate causal factors to training regimes and competence levels, and publish findings. Their analysis feeds back into regulatory authority deliberations. They occupy an analytical seat: they observe the regime's outcomes, document competence-gap failures, and validate or challenge the regime's adequacy. They have no enforcement power but significant epistemic authority (their public-facing investigation reports shape industry and political understanding of safety causation).
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, aviation_safety_board, observer,
    institutional, generational, analytical, global).

% Safety researchers, practitioners, and minority voices in aviation safety who argue that the hybrid regime's non-jeopardy components (simulators, line audits) create false confidence and that only real catastrophic events (or high-jeopardy near-misses that authentically trigger crisis psychology) provide the irreducible exercise that maintains true competence. They are excluded from competence-standard design — their voice is heard in academic literature, occasional regulatory testimony, and professional conferences, but regulatory authorities do not seriously entertain pure-catastrophe models as policy (ethically intolerable, operationally infeasible). Their exclusion is structural: the regime's foundational premise (we can engineer safe practice without mandating catastrophe) is incompatible with their core claim (only catastrophe teaches what must be learned). They experience suppression via regulatory authority dismissal and resource scarcity for alternative training research.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, catastrophe_advocates, excluded,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__hybrid_dependency, training_infrastructure_operators).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__hybrid_dependency, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a verifiable, uniform standard for crew competence that solves a genuine collective-action problem: without standardized hybrid training, airlines would minimize training spend (especially costly real-aircraft exposure), competence variance would be invisible until accidents revealed it, and public confidence in aviation safety would erode. The hybrid approach (simulation + real-world anchoring) coordinates a solution: simulators enable cost-controlled repetition of rare scenarios (engine failures, system cascades) that are too dangerous or infrequent to practice in actual aircraft; real-world anchoring (line audits, check flights) ensures crews can translate simulation skills to actual aircraft complexity, crew coordination under operational pressure, and the irreducible contingencies of live operations that no simulator fully captures. Both are necessary; neither alone is sufficient. The regime creates a shared baseline across all airlines and jurisdictions.
% TRANSFER_FUNCTION: Moves time commitment (simulator hours, line-audit participation), scheduling friction (incompatibility with operational rosters), evaluation anxiety (simultaneous assessment during actual duty), financial cost (training fees, facility charges), and professional-status contingency (career advancement depends on certification passage) from airlines and certification authorities to operational crews and junior pilots. Moves authority to determine competence standards and verification methods from individual airlines or crews to centralized certification bodies. Generates sustained revenue flow to training infrastructure operators (simulator operations, facility maintenance, scenario development). Generates administrative burden and resource allocation to airlines (scheduling, compliance infrastructure).
% ABSENT_VOICES: Catastrophe advocates are structurally excluded from competence-standard design; they would argue that the regime creates false confidence by substituting simulator metrics for real jeopardy and that authentic competence exercise requires genuine crisis psychology (only accessed via catastrophic events or ethically-unjustifiable near-miss mandates). Operational crews have limited voice in the regime's design — they are subjects of requirements they did not author. Alternative training models (risk-stratified by actual incident history, crew-self-directed proficiency, peer-certification networks) are not entertained in regulatory deliberations. Pilots in jurisdictions with lower training stringency (some international regimes) cannot participate in global standard-setting; their experience of lower-cost, still-safe alternatives is not imported into design conversations.
% DISAPPEARANCE_RATIONALE: If the hybrid mandate and its enforcement machinery disappeared overnight, airlines would immediately reduce simulator training budgets (simulators are expensive, circa $10-20M per facility, and per-pilot-hour costs are substantial); line audits would cease (regulatory overhead eliminated); real-aircraft time on non-critical operations would be minimized (operational efficiency pressure). Training infrastructure operators would face immediate facility utilization collapse and bankruptcy risk. Certification authorities would lose their enforcement lever and would need to reconstruct competence verification from scratch, likely via incident-driven retrofits (expensive, reactive). Crew schedules would simplify but competence assurance would become fragmented and invisible until accidents restored urgency. The aviation system would reorganize around lower-oversight, spot-check models until major accidents or near-misses triggered political pressure to re-mandate the hybrid regime or some alternative standard.
% FOUNDING_PROBLEM: Early aviation training was entirely real-world: pilots learned by flying actual aircraft under graduated responsibility, with high financial cost (aircraft ownership or rental), inherent safety risk (training accidents), and inability to practice rare emergency scenarios (too dangerous, too infrequent). As aviation scaled and aircraft became more complex, real-world training alone became economically unsustainable and safety-unacceptable. Simulator technology emerged as a promise: reduce cost (simulator per-hour cheaper than aircraft), reduce risk (crashes in simulators have no fatalities), enable repetition of rare scenarios (engine failures, system cascades practiced thousands of times, refined into automaticity). The founding problem was: How can we leverage simulation's efficiency and safety benefits while ensuring crews retain the competence that only real aircraft operations can fully verify? How do we build a standard that works across all airlines and jurisdictions without economically strangling training?
% FOUNDING_PROBLEM_CORROBORATION: Certification authorities and regulatory bodies attest the problem is live and the hybrid regime is the solution: simulator fidelity improves continuously but no simulator perfectly captures actual aircraft complexity, crew-coordination dynamics under real operational pressure, or the psychological realities of commanding an aircraft with human lives on board. Aviation safety boards (NTSB, AAIB) attest from incident investigation that competence gaps correlate with insufficient real-world exposure — crews trained heavily in simulator, light in actual operations, have shown slower decision-making, missed cues, and coordination breakdowns when facing novel real-aircraft situations. However, catastrophe advocates counter that the problem is fundamentally unsolved: simulators create false confidence (check-ride passing does not equal competence), and only genuine jeopardy (catastrophic events or mandated near-miss scenarios) provides the irreducible exercise. No peer-reviewed comparative study of cohorts with different simulator-to-real-aircraft ratios has definitively established the claimed competence differential (the finding would require tracking incident rates and response quality across pilots with systematically different training mixes — ethically difficult, methodologically complex). Operational crews testify that the burden (mandated hours, scheduling friction, evaluation anxiety) feels excessive relative to demonstrated safety gains, but no independent economic analysis of regime cost-to-benefit exists in the public record that is not authored by certification authorities or training operators.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__hybrid_dependency, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__hybrid_dependency, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__hybrid_dependency, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_requirement__hybrid_dependency, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__hybrid_dependency_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_requirement__hybrid_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_requirement__hybrid_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 to 0.62 over the interval, then plateaus. The rise reflects increasing regulatory stringency (more simulator hours required, more frequent line audits) and the capture of training infrastructure operators whose interests align with expanded mandates. The plateau at t=25-40 suggests regime stabilization — extractiveness hits a steady state once simulator capacity constraints and crew fatigue limit further expansion. Theater ratio rises from 0.32 to 0.51 (mid-interval peak), then slightly declines. This trajectory reflects mission creep: simulators designed to exercise rare emergency scenarios increasingly used for routine proficiency check and regulatory box-checking (the theater peak). The decline after t=30 may reflect backlash (crews reduce voluntary simulator use, airlines optimize scheduling) or regulatory recalibration. Suppression rises steadily from 0.58 to 0.72 and holds stable: the regime's enforcement machinery (regulatory audit, certification jeopardy, airline deployment gatekeeping) maintains high suppressive force to prevent crews from skipping simulator sessions or resisting line audits. The three-metric alignment (all sharing the same time grid) enables cross-temporal analysis: extractiveness and theater rise together (mission creep), while suppression rises slightly faster (enforcement hardening) — a classic pattern of a constraint defending expanding scope against rising resistance.
 *
 * PERSPECTIVAL GAP:
 *   The certification-authority and training-operator seats compute toward rope or tangled_rope depending on whether they weight the genuine coordination function (ensuring verifiable competence) heavily; they see a regime solving a real problem. The crew seats compute heavily toward snare or tangled_rope: they experience the regime as enforcing costly time commitment with disputed added safety benefit (the catastrophe-advocate framing lurks in their resistance). Airline-management seats compute as moderate payers with offsetting benefits (liability standardization reduces their variance). The engine's per-seat classification should diverge sharply: agenda-setters may compute as coordination or as asymmetric extractors depending on their power and exit positioning, while payer seats compute toward higher extractiveness. The directionality override for junior_pilots is needed: the automatic derivation (powerless + identity_locked exit + payer role) produces very high d (close to 1.0 = full target), which is structurally correct — junior pilots bear the regime's highest burden relative to power. But the secondary_role beneficiary reflects that the regime's justification is partly their protection (early-career gate-keeping, standardized credentialing). The override is: d stays high (~0.75-0.85) because the payer burden dominates, but the secondary role prevents full foreclosure.
 *
 * DIRECTIONALITY LOGIC:
 *   Operational_crews are moderate-power payers with constrained exit: d~0.70 (high target). Junior_pilots are powerless with identity_locked exit AND the regime is framed as protecting them: the structural derivation would produce d~0.85 (almost pure target), but secondary_role beneficiary tempters this; override to d~0.78 (high target, not pure). Training_infrastructure_operators are institutional beneficiaries with arbitrage options (can pivot to other simulation domains); d~0.15 (beneficiary). Certification_authorities are institutional agenda-setters with analytical exit; they defend the regime not for captured rents but for its enforcement logic; d~0.45 (symmetric, leaning slightly toward target for enforcement burden). Airline_management holds both roles: beneficiary (liability standardization) and payer (facility costs); derivation produces d~0.50 (symmetric). Catastrophe_advocates are excluded and mobile; they experience suppression (regulatory barriers to alternative training models) but can exit via academic dissent; d~0.55 (leaning target, but not trapped).
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid regime does NOT exhibit classical mandatrophy (founding problem solved but enforcement persists). Instead, it exhibits a different pathology: contested framing of the founding problem. The founding problem is WELL-DEFINED ('how to leverage simulation's efficiency while retaining real-world competence assurance') but the parties disagree fundamentally on whether it is SOLVED. Certification authorities and training operators assert it is solved — the hybrid regime works, competence is maintained, safety metrics are stable. Catastrophe advocates assert it is NOT solved — only genuine jeopardy provides irreducible exercise; the hybrid regime creates false confidence. Crews contest that the problem's cost-to-benefit ratio is misaligned (we pay a high burden for disputed marginal safety gain). The theater_ratio rise (0.32 to 0.51) suggests mission creep toward Goodhart dynamics: the regime optimizes for measurable proxy goals (simulator check-ride scores, line-audit compliance) rather than actual competence (the outcome is harder to verify, so metrics-gaming emerges). This is NOT mandatrophy-as-atrophy (the regime still enforces), but it is degradation-as-metric-substitution. The classification is stable tangled_rope, not yet piton, because the coordination function (verifiable competence) remains substantive and the extraction is still contested rather than accepted-theater. But the rising theater component is a yellow flag for piton transition if the regime continues to optimize metrics over competence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_sufficiency,
    'At what level of simulator fidelity does real-world anchoring become unnecessary? Is there a technological threshold beyond which high-fidelity simulation alone maintains competence?',
    'Longitudinal competence tracking across cohorts with varying simulator access: compare pilots trained 90% simulator (minimal real-aircraft time) vs. balanced hybrid vs. high real-aircraft exposure (if such cohorts exist). Correlate with incident rates, system-failure response times, and crew coordination metrics under operational stress.',
    'If simulator fidelity rises to a point where pure-simulation cohorts maintain parity in safety metrics with hybrid cohorts, the hybrid mandate becomes unnecessary extraction (reclassifies toward snare). If no such threshold exists, hybrid necessity is validated (stays tangled_rope). If the threshold is technological-path-dependent (achievable in 5 years), the regime''s current form is time-bounded (scaffold characteristics).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_sufficiency, empirical, 'Whether high-fidelity simulation alone can adequately maintain competence.').

omega_variable(
    catastrophe_necessity_vs_near_miss_equivalence,
    'Do genuine catastrophic events (or high-jeopardy near-misses with authentic crisis psychology) provide competence exercise that structured non-jeopardy audits cannot replicate?',
    'Incident investigation data correlating crew response quality with prior training exposure: did pilots who survived high-jeopardy incidents (engine failure, system cascade) demonstrate superior crisis response vs. those trained only on simulator scenarios rated equivalent but non-jeopardy?',
    'If high-jeopardy experience correlates with superior crisis response, catastrophe advocates'' core claim is validated (hybrid regime is missing an irreducible component; mandating near-miss participation would improve competence). If no differential emerges, the claim is empirically weaker (hybrid regime''s non-jeopardy anchoring may be sufficient). If differential exists but is correctable via simulator fidelity, simulation_as_adequate reading gains traction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(catastrophe_necessity_vs_near_miss_equivalence, empirical, 'Whether catastrophic jeopardy provides irreducible competence exercise.').

omega_variable(
    extraction_vs_legitimate_coordination_cost,
    'What portion of the regime''s measured extractiveness (0.62) represents necessary coordination cost (competence verification infrastructure, simulator operations) vs. pure extraction (institutional expansion, certification-authority capture, training-operator profit-maximization)?',
    'Cost-accounting study of simulator operation (marginal cost per pilot-hour), line-audit administration, and regulatory overhead; benchmark against peer safety regimes in other high-reliability domains (nuclear operations, medical credentialing). Economic modeling of counterfactual regimes with lower enforcement overhead.',
    'High coordination-cost ratio (>70% of 0.62 is necessary) validates the regime as legitimate tangled_rope (extraction is the price of coordination, within reason). Low ratio (<50%) establishes extractive overlay (reclassifies toward snare, suggests remedies like outsourcing, automated auditing, or fee deregulation). Moderate ratio supports mandatrophy-trajectory analysis (how has the ratio drifted over time as the regime matured?).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_legitimate_coordination_cost, empirical, 'What portion of measured extractiveness is legitimate coordination cost vs. institutional capture.').

omega_variable(
    identity_lock_mechanism_in_junior_pilots,
    'Is the junior_pilots'' identity_locked exit option primarily structural (legal/contractual barriers to career outside certification system) or internalized (professional identity inseparable from FAA/EASA certification, self-imposed lock)?',
    'Post-exit trajectory study: pilots who fail certification or abandon the regime — do they report relief (identity lock was exogenous) or crisis identity (lock was internalized, exit cost was psychological)? Career-path analysis of pilots who transition to non-certified roles (flight instruction, unmanned systems, military) vs. those who remain trapped.',
    'If lock is primarily structural, removing regulatory barriers (accepting alternative certifications, reciprocal agreements) would enable exit and pressure the regime toward more responsive design. If internalized, the regime''s extractiveness on junior_pilots is higher than the structural measure suggests (they carry the lock with them post-exit). Distinguishing mechanisms informs whether the constraint''s suppression is maintainable long-term or will erode as identity-lock psychologically decoheres.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_junior_pilots, empirical, 'Whether junior_pilots'' identity lock is structural or internalized.').

omega_variable(
    alternative_competence_frameworks_excluded,
    'Are there coherent alternative competence-verification frameworks (e.g., risk-stratified by incident history, crew-self-directed, peer-certification models) that would achieve safety outcomes at lower extraction cost but are excluded from regulatory deliberation?',
    'Regulatory history analysis: has the certification authority entertained alternative frameworks in design processes? Comparative institutional analysis from aviation systems with different regimes (some jurisdictions have lower line-audit frequencies, higher simulator discretion, risk-based models). Counterfactual modeling of alternative frameworks'' safety-equivalence.',
    'If alternatives are genuinely unavailable (no coherent alternative exists), the hybrid regime''s dominance is justified; suppression protects a necessary standard. If alternatives exist but are deliberately excluded (regulatory capture by training infrastructure), that establishes extractive overlay; reclassifies toward snare. If alternatives are known but deliberately rejected as higher-risk, that''s a deliberate tradeoff (regime is defensible but contingent on risk-preference assumptions).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_competence_frameworks_excluded, conceptual, 'Whether excluded alternative competence frameworks exist and what constrains their consideration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__hybrid_dependency, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__hybrid_dependency, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(comp_tr_t0, observed).
narrative_ontology:measurement(comp_tr_t5, competence_exercise_requirement__hybrid_dependency, theater_ratio, 5, 0.35).
narrative_ontology:measurement_basis(comp_tr_t5, observed).
narrative_ontology:measurement(comp_tr_t10, competence_exercise_requirement__hybrid_dependency, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(comp_tr_t10, observed).
narrative_ontology:measurement(comp_tr_t15, competence_exercise_requirement__hybrid_dependency, theater_ratio, 15, 0.42).
narrative_ontology:measurement_basis(comp_tr_t15, observed).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_requirement__hybrid_dependency, theater_ratio, 20, 0.46).
narrative_ontology:measurement_basis(comp_tr_t20, observed).
narrative_ontology:measurement(comp_tr_t25, competence_exercise_requirement__hybrid_dependency, theater_ratio, 25, 0.49).
narrative_ontology:measurement_basis(comp_tr_t25, observed).
narrative_ontology:measurement(comp_tr_t30, competence_exercise_requirement__hybrid_dependency, theater_ratio, 30, 0.51).
narrative_ontology:measurement_basis(comp_tr_t30, observed).
narrative_ontology:measurement(comp_tr_t35, competence_exercise_requirement__hybrid_dependency, theater_ratio, 35, 0.5).
narrative_ontology:measurement_basis(comp_tr_t35, observed).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_requirement__hybrid_dependency, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(comp_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(comp_be_t0, observed).
narrative_ontology:measurement(comp_be_t5, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(comp_be_t5, observed).
narrative_ontology:measurement(comp_be_t10, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(comp_be_t10, observed).
narrative_ontology:measurement(comp_be_t15, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 15, 0.59).
narrative_ontology:measurement_basis(comp_be_t15, observed).
narrative_ontology:measurement(comp_be_t20, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(comp_be_t20, observed).
narrative_ontology:measurement(comp_be_t25, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 25, 0.63).
narrative_ontology:measurement_basis(comp_be_t25, observed).
narrative_ontology:measurement(comp_be_t30, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 30, 0.63).
narrative_ontology:measurement_basis(comp_be_t30, observed).
narrative_ontology:measurement(comp_be_t35, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 35, 0.62).
narrative_ontology:measurement_basis(comp_be_t35, observed).
narrative_ontology:measurement(comp_be_t40, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(comp_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(comp_su_t0, observed).
narrative_ontology:measurement(comp_su_t5, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(comp_su_t5, observed).
narrative_ontology:measurement(comp_su_t10, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(comp_su_t10, observed).
narrative_ontology:measurement(comp_su_t15, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(comp_su_t15, observed).
narrative_ontology:measurement(comp_su_t20, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(comp_su_t20, observed).
narrative_ontology:measurement(comp_su_t25, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(comp_su_t25, observed).
narrative_ontology:measurement(comp_su_t30, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(comp_su_t30, observed).
narrative_ontology:measurement(comp_su_t35, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(comp_su_t35, observed).
narrative_ontology:measurement(comp_su_t40, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(comp_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__hybrid_dependency, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_requirement__hybrid_dependency, 0.18).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, simulation_as_adequate_exercise).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, catastrophe_as_necessary_anchor).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel (competence_exercise_requirement). The sibling readings (simulation_as_adequate, catastrophe_as_necessary) instantiate alternative claims about what competence maintenance requires. All three readings have distinct ε values and beneficiary structures. This reading (hybrid_dependency) occupies the middle position: rejecting pure-simulation adequacy AND rejecting pure-catastrophe necessity, instead arguing both are necessary. Links to siblings via network.affects_constraints; each reading is a separate constraint story with its own stakeholder structure and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_exercise_requirement__hybrid_dependency, powerless, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
