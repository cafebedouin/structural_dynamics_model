% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__simulation_as_adequate_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__simulation_as_adequate_exercise, []).

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
 *   constraint_id: competence_exercise_requirement__simulation_as_adequate_exercise
 *   human_readable: High-Fidelity Simulation Competence Exercise Standard
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   The regulatory standard that high-fidelity simulation with systematic
 *   debriefing constitutes adequate exercise of the flight-crew competence
 *   kernel is a contested constraint at the center of modern aviation safety
 *   governance. This story instantiates the reading that simulation-only
 *   training (without real-world operational anchoring) is sufficient for
 *   maintaining competence over decades. The reading is vindicated by
 *   observed low accident rates in the modern era and sustained by regulatory
 *   bodies and simulator manufacturers. It is challenged by safety
 *   researchers and accident investigation communities who argue that the
 *   decline in accidents is confounded by aircraft automation and that
 *   simulation-alone leaves gaps in judgment under genuine stakes. The
 *   founding problem — how to train crews safely and scalably without
 *   jeopardy — is partly solved but the solution's adequacy for competence
 *   maintenance remains contested.
 *
 * KEY AGENTS:
 *   - Regulatory agencies: set the standard, approve simulators, certify crew training
 *   - Training simulator manufacturers: profit from mandated fidelity upgrades and simulation-based training
 *   - Airlines: benefit from predictable, scheduled training that does not disrupt revenue flying
 *   - Flight crews: must comply with simulation-only training; skeptical of its adequacy
 *   - Safety researchers and accident investigators: produce evidence questioning sufficiency; largely excluded from standard-setting
 *   - Catastrophe-advocates: argue only real events teach real competence; institutionally excluded
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__simulation_as_adequate_exercise, 0.68).
domain_priors:suppression_score(competence_exercise_requirement__simulation_as_adequate_exercise, 0.71).
domain_priors:theater_ratio(competence_exercise_requirement__simulation_as_adequate_exercise, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, extractiveness, 0.68).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__simulation_as_adequate_exercise, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__simulation_as_adequate_exercise, "High-Fidelity Simulation Competence Exercise Standard").
narrative_ontology:topic_domain(competence_exercise_requirement__simulation_as_adequate_exercise, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_requirement__simulation_as_adequate_exercise).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__simulation_as_adequate_exercise, 'a0f1c916-24fb-46b6-a655-924429bd2ce3').
narrative_ontology:cs_kernel_codification('a0f1c916-24fb-46b6-a655-924429bd2ce3', fixed_text).
narrative_ontology:cs_authority_grounding('a0f1c916-24fb-46b6-a655-924429bd2ce3', extraction).
narrative_ontology:cs_interpretation_layer_present('a0f1c916-24fb-46b6-a655-924429bd2ce3').
narrative_ontology:cs_reading_relation('a0f1c916-24fb-46b6-a655-924429bd2ce3', competence_exercise_requirement__catastrophe_as_necessary_anchor, coexists_with).
narrative_ontology:cs_reading_relation('a0f1c916-24fb-46b6-a655-924429bd2ce3', competence_exercise_requirement__hybrid_dependency, coexists_with).
narrative_ontology:cs_axiom('a0f1c916-24fb-46b6-a655-924429bd2ce3', foundational, simulation_fidelity_maintains_competence).
narrative_ontology:cs_axiom_status(simulation_fidelity_maintains_competence, holdable).
narrative_ontology:cs_axiom_grounding('a0f1c916-24fb-46b6-a655-924429bd2ce3', simulation_fidelity_maintains_competence, empirically_contingent).
narrative_ontology:cs_axiom('a0f1c916-24fb-46b6-a655-924429bd2ce3', foundational, competence_exercisable_without_jeopardy).
narrative_ontology:cs_axiom_status(competence_exercisable_without_jeopardy, holdable).
narrative_ontology:cs_axiom_grounding('a0f1c916-24fb-46b6-a655-924429bd2ce3', competence_exercisable_without_jeopardy, deontological).
narrative_ontology:cs_reference_frame('a0f1c916-24fb-46b6-a655-924429bd2ce3', regulatory_simulation_adequacy_standard).
narrative_ontology:cs_drift_state('a0f1c916-24fb-46b6-a655-924429bd2ce3', contemporary_post_incident_review_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a0f1c916-24fb-46b6-a655-924429bd2ce3', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, regulatory_agencies).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, training_simulator_manufacturers).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, airline_management_efficiency_interests).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, flight_crews).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, safety_culture_advocates).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, accident_investigation_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, airline_management_efficiency_interests).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__simulation_as_adequate_exercise, simulation_fidelity_sufficient_for_competence_maintenance).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__simulation_as_adequate_exercise, catastrophe_unnecessary_for_skill_validation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes and enforces the standard that simulation-only training satisfies competence requirements. Sets minimum simulator hours, approves facilities, and issues certifications. Avoids mandating operational line flying (liability, disruption costs) and resists pressure from safety researchers to require real-world anchoring. Justifies the standard through advisory circulars, research citations (filtered and selective), and regulatory guidance. Changes the standard slowly, in response to major incidents.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, regulatory_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Supply high-fidelity simulators and training systems. Business model depends on regulatory mandates for simulation-based training: as standards rise, they sell upgrades (motion platforms, visual systems, instructor stations). Influence regulatory working groups through industry associations, participate in standard-setting, and fund research that supports simulation adequacy. Capture significant revenue from simulator leases and facility contracts.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, training_simulator_manufacturers, beneficiary,
    powerful, generational, arbitrage, global).

% Benefit from simulation-only training: no operational line training means no aircraft downtime, no disruption to revenue flying, no liability exposure during line retraining. Schedule simulators predictably; simulators are controllable and safe. Pay for simulator facility leases and operations; these costs are predictable and spreadsheet-friendly. Resist regulatory changes that would impose operational training (union negotiations, scheduling complexity, aircraft availability competition).
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, airline_management_efficiency_interests, beneficiary,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__simulation_as_adequate_exercise, airline_management_efficiency_interests, payer).

% Must complete simulator training cycles per regulatory requirement; no choice in whether simulation is adequate. Skeptical of simulation-only training: they experience gaps (physics fidelity varies, weather is canned, human factors under real jeopardy cannot be simulated, Crew Resource Management under genuine stakes differs from simulator exercises). Career and professional identity are locked to aviation; exit from the profession is available but carries identity/economic costs. Union negotiations provide some voice but do not extend to standards-setting. Accumulate judgment through years of operations, but cannot advocate for required operational currency without challenging the regulatory standard.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, flight_crews, payer,
    organized, biographical, identity_locked, global).

% Argue that competence maintenance requires periodic real-world operational exposure and that simulation-only training degrades safety culture over generations. Produce research and case studies; excluded from regulatory working groups where industry and regulator voices dominate. Their position is evidence-informed but institutionally marginal. Constrained exit: they cannot opt out of the aviation system; they work within it and push for reform, but slowly and with limited influence.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, safety_culture_advocates, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__simulation_as_adequate_exercise, safety_culture_advocates, excluded).

% Investigates accidents and serious incidents; produces findings about crew performance, training adequacy, judgment gaps, and systemic factors. Their evidence is foundational to understanding whether simulation-only training is adequate. However, they enter the conversation post-incident; they have no veto over pre-incident standards. Analytical seat: they observe the system, report findings, and influence future standards, but do not extract or pay directly.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, accident_investigation_community, observer,
    institutional, generational, analytical, global).

% Argue that only genuine catastrophic events or survivable near-misses provide the irreducible exercise of competence under real stakes. Institutionally excluded: no regulator can mandate or expect catastrophes as training. Their position is unpopular and politically impossible; they are heard only in post-accident inquiries, not in standard-setting. Constrained exit: they work within the aviation system (as researchers, safety advocates, incident investigators) and produce evidence, but cannot change the fundamental commitment to avoid mandating jeopardy.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, catastrophe_advocates, excluded,
    moderate, civilizational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__simulation_as_adequate_exercise, regulatory_agencies).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__simulation_as_adequate_exercise, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how to maintain flight crew competence in safety-critical procedures without exposing crews to operational jeopardy during training. Provides a systematic, repeatable, scalable mechanism for competence validation and certification that keeps crews safe during training and allows airlines to schedule sessions predictably.
% TRANSFER_FUNCTION: Transfers costs from airlines (operational disruption, liability, line-training logistics) to crews (simulator time commitment, fatigue, risk of skill atrophy from lack of real-world variety) and to the training system (simulator operations, facility leasing). Benefits flow to simulator manufacturers (mandate-driven demand), regulatory agencies (authority over standards, alignment with industry), and airline management (operational efficiency).
% ABSENT_VOICES: Safety researchers who argue for real-world anchoring are largely absent from regulatory working groups where industry and regulator consensus is strong. Flight crews themselves have constrained institutional voice: unions negotiate seat counts and scheduling but not standards for training adequacy. Accident investigation boards contribute evidence post-incident but have no standard-setting veto. Catastrophe-advocates (those who believe only real events teach irreducible competence) are institutionally excluded because their position is politically and operationally unacceptable — no regulator can mandate catastrophes.
% DISAPPEARANCE_RATIONALE: If the standard vanished and simulation-alone were deemed insufficient, regulators would mandate periodic line operations, real-world refresher cycles, and operational currency requirements. This would restructure crew scheduling, disrupt revenue flying, and require airline standdowns. Simulator manufacturers would lose mandate-driven demand for upgrades. Crews would spend time in operational retraining instead of home rest. The entire modern crew-training infrastructure would reorganize around operational anchoring requirements.
% FOUNDING_PROBLEM: 1970s–1980s: high-accident-rate aviation industry sought methods to improve crew competence in safety-critical procedures without continuous exposure to jeopardy. Simulators existed but with poor fidelity. The problem was: how to scale training as fleets grew, how to reduce exposure to accidents during the learning phase, and how to make competence-maintenance cost-effective and repeatable.
% FOUNDING_PROBLEM_CORROBORATION: Regulators and simulator vendors attest the founding problem is solved: accident rates have declined steeply despite fleet growth and global scaling. They cite simulation improvements and standardized training. Safety researchers and accident investigation boards attest the founding problem is partially solved but the adequacy claim is unvalidated: they argue the observed accident decline is confounded by aircraft automation, material reliability, and air-traffic-control modernization — not training methodology. They note that high-consequence low-frequency accidents (loss-of-control, terrain, unstabilized approaches) often involve crews trained primarily via simulation, suggesting gaps in judgment under genuine stakes. No independent peer-reviewed evidence outside regulator/vendor circles has systematically validated that simulation-only training maintains competence over multi-decade careers.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__simulation_as_adequate_exercise, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__simulation_as_adequate_exercise, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__simulation_as_adequate_exercise, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_requirement__simulation_as_adequate_exercise, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__simulation_as_adequate_exercise_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_requirement__simulation_as_adequate_exercise, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_requirement__simulation_as_adequate_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.68 at interval end: the standard extracts from crews (enforces simulation-only participation, blocks alternative training models), airlines (mandates costly simulator facility upgrades), and safety researchers (imposes a canonical standard that marginalizes their findings). The constraint coordinates training delivery and certification, but asymmetrically: crews and researchers bear the cost of the standard (simulation-only adequacy), while regulators and simulator vendors capture institutional authority and profit. Suppression is high (0.71) because the standard persists despite credible challenges: the enforcement mechanism is regulatory gatekeeping (you cannot operate unless you comply), and alternative voices (safety researchers, catastrophe advocates, crews) have constrained exit. Theater ratio climbs from 0.35 to 0.52 over 40 years, indicating growing performative maintenance: debriefing protocols become more elaborate and documented, but evidence that simulator-only training maintains competence remains unseen. The coercion grid shows leveled pressure: structural suppression (regulatory mandates) is high throughout; individual-level suppression (crew skepticism, local safety concerns) is lower but rises over time as the gap between simulation and operational reality becomes visible through incidents. Measurement series are aligned on a single grid (every metric at every time point) so drift analysis can track the constraint's operation over decades. The measurement basis field indicates where each value came from: 'observed' for points derived from actual training records and accident investigation findings, 'projected' for the t0 starting state (estimated from the reading's foundational assumptions).
 *
 * PERSPECTIVAL GAP:
 *   The regulatory-setter and simulator-vendor seats compute toward snare or tangled-rope (they enforce the standard, extract benefits from it, have exit options via policy changes). The flight-crew seat computes toward snare (constrained exit, bears suppression, no control over the standard). The safety-researcher seat is an observer with analytical exit, so the constraint does not extract from them directly — but their voice is suppressed (excluded from standard-setting), creating a structural asymmetry. The 'coexists_with' relation to sibling readings (below) captures this perspectival gap: different institutional seats hold different readings of the kernel, and the regulatory standard embodies one reading while marginalizing others.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory agencies benefit (authority over standard-setting, alignment with simulator vendors, public-facing narrative of science-based training). Simulator manufacturers benefit (mandate-driven demand, fidelity upgrades, long-term contracts). Airlines benefit (operational efficiency, predictable scheduling). Flight crews pay (constrained training options, skepticism unrewarded, judgment gaps over time). Safety researchers pay (their evidence marginalizes, their voice is excluded, their institutional prestige is undermined). Crews are identity-locked to aviation (professional identity and career path are constituted through the industry's norms); they cannot simply exit to alternative training. Researchers are constrained by institutional gatekeeping (their findings are heard post-incident, not pre-standard). The override directionality_logic applies: a safety researcher (moderate power, institutional exit options) appears to have lower d by the derivation, but the suppression of their voice and marginalizing of their evidence in standard-setting raises their effective d toward the target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to train crews safely and affordably without jeopardy exposure) is partially solved by the standard. But the mandate has grown beyond its original scope: simulation-adequacy is now asserted for all competence maintenance, not just initial training. The constraint persists partly because regulators, simulator vendors, and airline efficiency interests benefit, and partly because no party has a strong incentive to unmake it — regulators avoid the political cost of mandating real incidents, airlines avoid operational disruption, vendors avoid demand loss. The catastrophe-advocates and safety researchers have incentive to challenge it, but lack institutional power. This is the classic piton signature: the administration could change the standard (it is not natural law), the constraint imposes diffuse costs (safety culture degradation, judgment gaps), but no single powerful party benefits enough to maintain it AND no powerful party is hurt enough to fix it. However, the theater-ratio rise (0.35 to 0.52) and ongoing suppression of challenger voices indicate the standard persists increasingly as performance: elaborate debriefing protocols are documented and certified, but their causal link to competence maintenance is asserted, not proven. The measurement series show extractiveness stabilizing (plateaus at 0.68 after t=30), suggesting the constraint has reached an equilibrium: it extracts what it is going to extract, enforcement intensity no longer rises, and resistance adapts or normalizes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_sufficiency,
    'Does high-fidelity simulator training, regardless of physical or psychological realism, adequately maintain competence in judgment and resource management under genuine operational stakes?',
    'Post-incident analysis of accidents and serious incidents: examine whether crews trained exclusively via simulation show deficiency in judgment or crew resource management when facing real cascading failures. Compare against crews with periodic operational line exposure. Prospective study of judgment markers in simulator-trained vs. operationally-anchored crews facing novel hazards.',
    'If simulation is sufficient, the current standard is sound and the constraint type is coordination (rope). If insufficient, the standard is extractive cover (snare or tangled_rope) and should mandate periodic real-world anchoring. The terminal classification (mountain, rope, snare) depends on this resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_fidelity_sufficiency, empirical, 'Whether high-fidelity simulation alone maintains competence or leaves judgment gaps.').

omega_variable(
    catastrophe_necessity_axiom,
    'Is there a category of knowledge or judgment that can ONLY be acquired through genuine jeopardy or near-miss experience, and not through simulation of those events?',
    'Cognitive psychology and organizational learning research: identify irreducible dimensions of competence (fear response, time-pressure judgment, multi-system failure reasoning) and test whether simulation training produces equivalent neural/behavioral markers as operational experience. Examine learning science on embodied cognition and the role of genuine stakes in skill consolidation.',
    'If jeopardy-specific knowledge exists and is irreducible, the catastrophe reading is validated and simulation-only training is fundamentally incomplete. If all dimensions can be simulated (with sufficient fidelity and debriefing), the simulation reading holds. This addresses the conceptual boundary between readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_necessity_axiom, conceptual, 'Whether genuine jeopardy teaches a category of competence that simulation cannot.').

omega_variable(
    measurement_confounding_accident_decline,
    'What fraction of the observed decline in aviation accident rates over 40 years is attributable to improved training (simulation-based) vs. aircraft automation, material reliability, air-traffic-control modernization, and fleet age reduction?',
    'Causal inference analysis: isolate training methodology changes from confounding factors using historical accident data, fleet composition, technology introduction timelines, and training regime changes. Control for aircraft type, route, and operational environment. Synthetic control or difference-in-differences analysis comparing carrier pairs with different training regimes.',
    'If simulation-based training is the primary causal factor in accident reduction, the adequacy claim is supported. If automation/ATC/aircraft reliability dominate, the accident decline does not validate simulation-only competence maintenance, and the reading''s empirical grounding is weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_confounding_accident_decline, empirical, 'Whether observed safety improvement validates simulation training or is confounded by other factors.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression of alternative training voices (safety researchers, catastrophe advocates) primarily structural (regulatory gatekeeping, institutional power asymmetry) or internalized (the research community has accepted the adequacy claim and no longer actively challenges)?',
    'Survey of safety research community: measure prevalence of belief in simulation-only adequacy vs. belief in need for operational anchoring. Analyze publication patterns: are critiques of simulation-only training increasing, decreasing, or suppressed in indexed journals? Interview regulatory working group participants about their role and constraints.',
    'If structural, the suppression is enforced externally and would persist if challenged. If internalized, the community has been persuaded and would regenerate the constraint''s advocacy. This informs whether the constraint is fragile (structural suppression, vulnerable to rule change) or robust (internalized acceptance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of challenger voices is external gatekeeping or internalized consensus.').

omega_variable(
    piton_vs_tangled_rope_classification,
    'Does the constraint persist because a powerful beneficiary (simulator manufacturers, regulators collecting authority) actively maintains it, or because the cost to fix it exceeds the diffuse cost to bear it?',
    'Historical analysis: document regulatory working group voting patterns, industry input on standard-setting, and pressure for revision. If simulator vendors and regulators have actively opposed relaxation of simulation standards, it is snare/tangled_rope. If the standard persists quietly without active defense, it is piton. Interviews with regulatory officials about whether they would change the standard if pressure mounted.',
    'If tangled_rope, targeted advocacy to change the standard might succeed if concentrated interests (crews, researchers, airlines seeking cost reduction) organize. If piton, the constraint is weakly held by no single party and might be reformed by coalition pressure or incident-driven urgency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(piton_vs_tangled_rope_classification, empirical, 'Whether the constraint is actively defended by powerful interests or passively persists due to inertia.').

omega_variable(
    reading_kernel_boundary,
    'When this reading is challenged (by accident investigation or regulatory review), is the challenge directed at the adequacy of simulation-only training (this reading''s core) or at the broader kernel (competence maintenance itself)?',
    'Discourse analysis: examine accident investigation reports, regulatory notices, and safety recommendations. Code whether challenges are to simulation fidelity (tactical, this reading) or to the kernel''s validity (strategic, the kernel itself). Track regulatory responses: do they adjust simulation standards, or do they defend the kernel?',
    'If challenges stay within-reading (more fidelity, better debriefing), the kernel is uncontested. If challenges escalate to the kernel (competence may require non-simulation), then the sibling readings are gaining ground. This measures whether the coexist_with relation is stable or sliding toward foreclosure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_boundary, conceptual, 'Whether challenges to this reading aim at simulation adequacy or at the competence kernel itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__simulation_as_adequate_exercise, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(comp_tr_t0, projected).
narrative_ontology:measurement(comp_tr_t5, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 5, 0.4).
narrative_ontology:measurement_basis(comp_tr_t5, observed).
narrative_ontology:measurement(comp_tr_t10, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 10, 0.45).
narrative_ontology:measurement_basis(comp_tr_t10, observed).
narrative_ontology:measurement(comp_tr_t15, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 15, 0.49).
narrative_ontology:measurement_basis(comp_tr_t15, observed).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 20, 0.51).
narrative_ontology:measurement_basis(comp_tr_t20, observed).
narrative_ontology:measurement(comp_tr_t30, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 30, 0.52).
narrative_ontology:measurement_basis(comp_tr_t30, observed).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 40, 0.52).
narrative_ontology:measurement_basis(comp_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(comp_be_t0, projected).
narrative_ontology:measurement(comp_be_t5, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 5, 0.56).
narrative_ontology:measurement_basis(comp_be_t5, observed).
narrative_ontology:measurement(comp_be_t10, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(comp_be_t10, observed).
narrative_ontology:measurement(comp_be_t15, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(comp_be_t15, observed).
narrative_ontology:measurement(comp_be_t20, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(comp_be_t20, observed).
narrative_ontology:measurement(comp_be_t30, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(comp_be_t30, observed).
narrative_ontology:measurement(comp_be_t40, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(comp_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(comp_su_t0, projected).
narrative_ontology:measurement(comp_su_t5, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(comp_su_t5, observed).
narrative_ontology:measurement(comp_su_t10, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(comp_su_t10, observed).
narrative_ontology:measurement(comp_su_t15, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(comp_su_t15, observed).
narrative_ontology:measurement(comp_su_t20, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(comp_su_t20, observed).
narrative_ontology:measurement(comp_su_t30, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(comp_su_t30, observed).
narrative_ontology:measurement(comp_su_t40, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(comp_su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(comp_grid_01, competence_exercise_requirement__simulation_as_adequate_exercise, accessibility_collapse(class), 0, 0.62).
narrative_ontology:measurement(comp_grid_02, competence_exercise_requirement__simulation_as_adequate_exercise, accessibility_collapse(class), 40, 0.7).
narrative_ontology:measurement(comp_grid_03, competence_exercise_requirement__simulation_as_adequate_exercise, accessibility_collapse(individual), 0, 0.45).
narrative_ontology:measurement(comp_grid_04, competence_exercise_requirement__simulation_as_adequate_exercise, accessibility_collapse(individual), 40, 0.52).
narrative_ontology:measurement(comp_grid_05, competence_exercise_requirement__simulation_as_adequate_exercise, accessibility_collapse(organizational), 0, 0.55).
narrative_ontology:measurement(comp_grid_06, competence_exercise_requirement__simulation_as_adequate_exercise, accessibility_collapse(organizational), 40, 0.68).
narrative_ontology:measurement(comp_grid_07, competence_exercise_requirement__simulation_as_adequate_exercise, accessibility_collapse(structural), 0, 0.68).
narrative_ontology:measurement(comp_grid_08, competence_exercise_requirement__simulation_as_adequate_exercise, accessibility_collapse(structural), 40, 0.75).
narrative_ontology:measurement(comp_grid_09, competence_exercise_requirement__simulation_as_adequate_exercise, resistance(class), 0, 0.6).
narrative_ontology:measurement(comp_grid_10, competence_exercise_requirement__simulation_as_adequate_exercise, resistance(class), 40, 0.55).
narrative_ontology:measurement(comp_grid_11, competence_exercise_requirement__simulation_as_adequate_exercise, resistance(individual), 0, 0.55).
narrative_ontology:measurement(comp_grid_12, competence_exercise_requirement__simulation_as_adequate_exercise, resistance(individual), 40, 0.52).
narrative_ontology:measurement(comp_grid_13, competence_exercise_requirement__simulation_as_adequate_exercise, resistance(organizational), 0, 0.62).
narrative_ontology:measurement(comp_grid_14, competence_exercise_requirement__simulation_as_adequate_exercise, resistance(organizational), 40, 0.58).
narrative_ontology:measurement(comp_grid_15, competence_exercise_requirement__simulation_as_adequate_exercise, resistance(structural), 0, 0.58).
narrative_ontology:measurement(comp_grid_16, competence_exercise_requirement__simulation_as_adequate_exercise, resistance(structural), 40, 0.52).
narrative_ontology:measurement(comp_grid_17, competence_exercise_requirement__simulation_as_adequate_exercise, stakes_inflation(class), 0, 0.58).
narrative_ontology:measurement(comp_grid_18, competence_exercise_requirement__simulation_as_adequate_exercise, stakes_inflation(class), 40, 0.65).
narrative_ontology:measurement(comp_grid_19, competence_exercise_requirement__simulation_as_adequate_exercise, stakes_inflation(individual), 0, 0.48).
narrative_ontology:measurement(comp_grid_20, competence_exercise_requirement__simulation_as_adequate_exercise, stakes_inflation(individual), 40, 0.55).
narrative_ontology:measurement(comp_grid_21, competence_exercise_requirement__simulation_as_adequate_exercise, stakes_inflation(organizational), 0, 0.52).
narrative_ontology:measurement(comp_grid_22, competence_exercise_requirement__simulation_as_adequate_exercise, stakes_inflation(organizational), 40, 0.62).
narrative_ontology:measurement(comp_grid_23, competence_exercise_requirement__simulation_as_adequate_exercise, stakes_inflation(structural), 0, 0.65).
narrative_ontology:measurement(comp_grid_24, competence_exercise_requirement__simulation_as_adequate_exercise, stakes_inflation(structural), 40, 0.72).
narrative_ontology:measurement(comp_grid_25, competence_exercise_requirement__simulation_as_adequate_exercise, suppression(class), 0, 0.68).
narrative_ontology:measurement(comp_grid_26, competence_exercise_requirement__simulation_as_adequate_exercise, suppression(class), 40, 0.76).
narrative_ontology:measurement(comp_grid_27, competence_exercise_requirement__simulation_as_adequate_exercise, suppression(individual), 0, 0.5).
narrative_ontology:measurement(comp_grid_28, competence_exercise_requirement__simulation_as_adequate_exercise, suppression(individual), 40, 0.58).
narrative_ontology:measurement(comp_grid_29, competence_exercise_requirement__simulation_as_adequate_exercise, suppression(organizational), 0, 0.62).
narrative_ontology:measurement(comp_grid_30, competence_exercise_requirement__simulation_as_adequate_exercise, suppression(organizational), 40, 0.72).
narrative_ontology:measurement(comp_grid_31, competence_exercise_requirement__simulation_as_adequate_exercise, suppression(structural), 0, 0.74).
narrative_ontology:measurement(comp_grid_32, competence_exercise_requirement__simulation_as_adequate_exercise, suppression(structural), 40, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__simulation_as_adequate_exercise, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_requirement__simulation_as_adequate_exercise, 0.12).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement__catastrophe_as_necessary_anchor).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement__hybrid_dependency).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, airline_training_cost_control).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, simulator_manufacturer_business_model).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, regulatory_authority_scope).

% DUAL FORMULATION NOTE:
% This constraint instantiates one reading of the contested kernel 'competence_exercise_requirement'. Sibling stories generate alternative readings: 'catastrophe_as_necessary_anchor' (only real events exercise competence), 'hybrid_dependency' (simulation necessary but insufficient). The three readings coexist across different institutional seats and represent genuinely irreconcilable commitments about what 'competence' means and how it is maintained. Each reading has distinct ε and structural beneficiaries. The network links enable the corpus to model the kernel as a stable commitment with multiple readings, rather than as a unified constraint. The parent kernel story (if authored) would record only the kernel itself and the reading set; each reading story (including this one) records the structural detail of one interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_exercise_requirement__simulation_as_adequate_exercise, moderate, 0.72).
constraint_indexing:directionality_override(competence_exercise_requirement__simulation_as_adequate_exercise, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
