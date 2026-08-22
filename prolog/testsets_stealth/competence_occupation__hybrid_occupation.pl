% ============================================================================
% CONSTRAINT STORY: competence_occupation__hybrid_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Multi-Mechanism Continuous Competence Occupation Mandate
 *   domain: institutional/safety-regulatory
 *
 * SUMMARY:
 *   High-reliability domains (aviation, nuclear power, major surgery,
 *   emergency response) operate a standing mandate that licensed competence
 *   must be continuously occupied through multiple simultaneous mechanisms:
 *   simulator sessions, classroom refreshers, procedural reinforcement
 *   drills, and line audits. This story instantiates the hybrid_occupation
 *   reading of the competence_occupation kernel: the claim that no single
 *   mechanism suffices, that the mechanisms are jointly necessary, and that
 *   the optimal configuration is unknown — which converts training
 *   optimization into a perpetual research problem. The epsilon referent is
 *   the standing multi-mechanism arrangement as this reading assesses it: the
 *   reading prices IN the genuine multi-mechanism core (skill decay is
 *   empirically real and this reading holds the mechanisms jointly
 *   necessary), and counts as extraction the unvalidated configuration
 *   layers, hour-based mandates lacking dose-response support, and the
 *   vendor/regulatory/departmental economy that feeds on the never-settled
 *   configuration question. Claim and metrics are authored independently:
 *   claimed_type states the structure I believe true (tangled_rope — real
 *   coordination function with asymmetric capture riding the same apparatus);
 *   the metrics describe observed operation without being tuned to any
 *   predicted engine verdict.
 *
 * KEY AGENTS:
 *   - - safety_regulators: Agenda setter (institutional/constrained) — mandates mechanisms, audits compliance, gains jurisdiction from the open configuration question
 *   - - training_providers_simulator_vendors: Primary beneficiary (powerful/arbitrage) — collects fees; every added mechanism expands their market
 *   - - in_house_training_departments: Secondary beneficiary (organized/identity_locked) — administers delivery; professional identity fused to the apparatus
 *   - - frontline_operators_crews: Primary payer (organized/constrained) — surrenders duty days; carries license and career consequences
 *   - - operating_organizations: Payer with beneficiary secondary position (institutional/constrained) — funds the apparatus, collects liability and insurance value
 *   - - liability_insurers: Beneficiary and co-enforcer (institutional/arbitrage) — prices and imposes training conditions without delivering them
 *   - - passengers_and_public: Diffuse beneficiary (powerless/mobile) — receives the safety margin, bears pass-through costs, holds no seat
 *   - - training_research_community: Observer with beneficiary exposure (moderate/mobile) — the open question sustains the research program
 *   - - evidence_based_training_reformers: Excluded voice (organized/constrained) — advisory-only status; would replace hours with validated competency assessment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, 0.58).
domain_priors:suppression_score(competence_occupation__hybrid_occupation, 0.52).
domain_priors:theater_ratio(competence_occupation__hybrid_occupation, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, extractiveness, 0.58).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__hybrid_occupation, tangled_rope).
narrative_ontology:human_readable(competence_occupation__hybrid_occupation, "Multi-Mechanism Continuous Competence Occupation Mandate").
narrative_ontology:topic_domain(competence_occupation__hybrid_occupation, "institutional/safety-regulatory").

domain_priors:requires_active_enforcement(competence_occupation__hybrid_occupation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__hybrid_occupation, 'c6f5a7d3-b846-4cdf-b115-6a069b21f0af').
narrative_ontology:cs_kernel_codification('c6f5a7d3-b846-4cdf-b115-6a069b21f0af', formalized).
narrative_ontology:cs_authority_grounding('c6f5a7d3-b846-4cdf-b115-6a069b21f0af', expertise).
narrative_ontology:cs_interpretation_layer_present('c6f5a7d3-b846-4cdf-b115-6a069b21f0af').
narrative_ontology:cs_reading_relation('c6f5a7d3-b846-4cdf-b115-6a069b21f0af', competence_occupation__simulation_sufficiency, forecloses).
narrative_ontology:cs_reading_relation('c6f5a7d3-b846-4cdf-b115-6a069b21f0af', competence_occupation__real_incident_necessity, forecloses).
narrative_ontology:cs_axiom('c6f5a7d3-b846-4cdf-b115-6a069b21f0af', foundational, no_single_mechanism_sufficient).
narrative_ontology:cs_axiom_status(no_single_mechanism_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('c6f5a7d3-b846-4cdf-b115-6a069b21f0af', no_single_mechanism_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('c6f5a7d3-b846-4cdf-b115-6a069b21f0af', foundational, optimal_configuration_indeterminate).
narrative_ontology:cs_axiom_status(optimal_configuration_indeterminate, holdable).
narrative_ontology:cs_axiom_grounding('c6f5a7d3-b846-4cdf-b115-6a069b21f0af', optimal_configuration_indeterminate, empirically_contingent).
narrative_ontology:cs_reference_frame('c6f5a7d3-b846-4cdf-b115-6a069b21f0af', multi_mechanism_continuous_exercise_framework).
narrative_ontology:cs_drift_state('c6f5a7d3-b846-4cdf-b115-6a069b21f0af', contemporary_evidence_based_training_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('c6f5a7d3-b846-4cdf-b115-6a069b21f0af', '').
narrative_ontology:cs_kernel_id(competence_occupation__hybrid_occupation, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, training_providers_simulator_vendors).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, in_house_training_departments).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, safety_regulators).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, liability_insurers).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, passengers_and_public).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, frontline_operators_crews).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, operating_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, operating_organizations).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, training_research_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets minimum recurrent training hours, approves which mechanisms and devices satisfy them, and audits compliance through certificate actions. Gains staffing, jurisdiction, and post-incident defensibility from administering an expanding apparatus. Cannot declare any configuration final: certifying a configuration as sufficient attaches blame to the certifier if an incident later implicates a skill the dropped mechanism would have maintained.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, safety_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Manufactures full-flight simulators in an oligopoly market and sells simulator hours, courseware, and contracted instruction to operators worldwide. Every additional mandated mechanism enlarges the addressable market; because no configuration is ever agreed to be optimal, no product line is ever declared sufficient and retired. Can redirect sales across industries and jurisdictions if any single regulatory market tightens.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, training_providers_simulator_vendors, beneficiary,
    powerful, generational, arbitrage, global).

% Instructor and examiner corps whose headcount, seniority ladders, instructional craft, and professional self-conception are constituted by the multi-mechanism apparatus they administer. Delivering simulator sessions, refreshers, drills, and line checks is their vocation; proposals to collapse mechanisms into a single validated method register as existential threats to their profession rather than as efficiency opportunities.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, in_house_training_departments, beneficiary,
    organized, biographical, identity_locked, continental).

% Pilots, reactor operators, and clinical teams surrender recurring duty days to simulator sessions, computer-based refreshers, procedural drills, and line checks. Failed checks carry license and career consequences. Unions negotiate training load and compensation but do not design the configuration of mechanisms. Leaving the profession forfeits a licensed identity and accumulated seniority, so exit is costly even where formally available.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, frontline_operators_crews, payer,
    organized, biographical, constrained, global).

% Airlines, nuclear operators, and hospital systems fund simulator acquisition or lease, training centers, instructor payroll, and crew time off-line. In exchange they hold documented-competence records used in liability defense, insurance negotiation, and public trust. They lobby for relief when training costs spike and for expanded mandates when competitors might otherwise gain a cost advantage.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, operating_organizations, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_occupation__hybrid_occupation, operating_organizations, beneficiary).

% Receives the safety margin the regime purchases and absorbs its costs indirectly through fares, rates, and prices. Has no seat in configuration decisions, no visibility into which mechanisms actually maintain skill, and no channel through which to trade training burden against cost.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, passengers_and_public, beneficiary,
    powerless, immediate, mobile, global).

% Prices premiums partly on evidenced training compliance and writes training requirements directly into coverage terms. Benefits from the legibility the mandate creates and reinforces enforcement without running the regime; can reprice or withdraw from a line of business, shifting pressure onto operators without bearing delivery costs.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, liability_insurers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(competence_occupation__hybrid_occupation, liability_insurers, agenda_setter).

% Studies transfer of training, skill decay curves, and device fidelity across aviation, nuclear, and medical domains. The unresolved configuration question sustains a continuing research program and its funding; published findings periodically re-weight mechanisms without settling the question, which preserves both scientific access and relevance.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, training_research_community, observer,
    moderate, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(competence_occupation__hybrid_occupation, training_research_community, beneficiary).

% Competency-based assessment advocates seated on advisory working groups with recommend-only status. They argue that hour-based mandates persist without dose-response validation and that assessment of demonstrated competency should replace accumulated hours. Their proposals enter rulemaking slowly and are diluted by blame-risk conservatism before adoption.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, evidence_based_training_reformers, excluded,
    organized, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__hybrid_occupation, training_providers_simulator_vendors).
narrative_ontology:fixing_cost_class(competence_occupation__hybrid_occupation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the skill-decay and self-assessment-failure problem: individual operators cannot reliably detect decay of rarely exercised critical skills, and organizations cannot trust self-declaration. The multi-mechanism regime provides repeated, externally verified contact with rare high-consequence events across simulation, classroom refresher, procedural drill, and line observation, so that no single mechanism's blind spots stand unchecked.
% TRANSFER_FUNCTION: Moves money (training fees, simulator-hour purchases, instructor payroll, oversight appropriations) and time (crew days off-line) from operating organizations and frontline crews to training providers, internal training departments, and regulators; moves documented assurance of competence back toward organizations, insurers, and the public.
% ABSENT_VOICES: Evidence-based training reformers hold advisory-only seats and would reconfigure the regime around validated competency assessment; frontline crews' experiential knowledge of which mechanisms actually maintain skill enters through union negotiation rather than configuration design; passengers fund the regime through prices with no seat at all.
% DISAPPEARANCE_RATIONALE: If the multi-mechanism mandate vanished overnight, licensing frameworks, insurance coverage terms, the simulator manufacturing and training-services industry, instructor and examiner professions, and crew scheduling systems would all lose their organizing structure; competence assurance would have to be reinvented and every license condition renegotiated before operations continued as before.
% FOUNDING_PROBLEM: Mid-century aviation and nuclear accidents demonstrated that skills for rare high-consequence events decay between real encounters and that operators systematically misjudge their own degradation; regimes were built to force periodic, externally verified rehearsal of those events through whatever artificial means were available.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated from outside the benefiting parties by the motor-learning and skill-decay literature and by accident-investigation board findings attributing events to degraded rarely-used skills. By contrast, no independent body attests that the CURRENT multi-mechanism configuration specifically is correct or optimal; configuration adequacy is attested only by parties inside the regime, which is itself signal.
narrative_ontology:disappearance_verdict(competence_occupation__hybrid_occupation, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__hybrid_occupation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__hybrid_occupation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_occupation__hybrid_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__hybrid_occupation, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.58: substantial but bounded — the reading itself holds the multi-mechanism core necessary, so only the unvalidated configuration layers, hour mandates without dose-response evidence, and the rent economy in the configuration gaps count as excess. Suppression 0.52: enforcement is structural (certificate action for failed checks, mandatory attendance as a license condition) but broadly legitimated; it coerces at the margin, not in bulk. Theater ratio 0.40: simulator work catches real decay, but a growing share of activity is sign-off ritual, paper-compliance e-learning, and audits that inspect records rather than behavior — the classic paper-compliance gap documented in HRO studies. Accessibility collapse 0.35: alternatives (competency-based, evidence-based training regimes) demonstrably exist and are partially adopted, so understanding the constraint does not close the option space. Resistance 0.45: unions and operators actively contest load and cost, yet safety legitimacy blunts opposition — few argue against training itself, only against its configuration and volume. The temporal series runs on one shared six-point grid for all three metrics. The dynamic is a post-accident ratchet, not an oscillation: each salient incident adds a mandated mechanism or hour block, and nothing symmetric removes them, producing the monotonic rise in all three series. The suppression_requirement series is authored deliberately: enforcement capacity demonstrably matured over the interval (new mechanisms, new device approvals, expanded inspector corps), so the enforcement trajectory is part of the story, not static background.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda seats should compute differently. From the vendor and training-department seats the apparatus is the product and the vocation — coordination they sell and administer, with the open configuration question as market protection. From the crew seat the same apparatus is a compulsory time tax with license stakes attached. From the regulator seat it is prudent redundancy under blame asymmetry: no one is dismissed for mandating more training, everyone is blamed for the incident that follows a reduction. From the public seat it is simply safety. The researcher seat experiences the unresolved question as opportunity rather than burden. The engine computes these divergences from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (vendors, departments, regulators, insurers, public) derive low directionality for those seats; victim declarations (crews, operating organizations) derive high directionality. Operating organizations are genuinely dual-positioned — they bear the budget burden (victim-side) while collecting liability defense, insurance pricing, and reputational value (beneficiary-side) — expressed here through secondary_role rather than a directionality override, because the declaration-level data already encodes the duality and no override is needed. The identity_locked exit on in_house_training_departments matters even from the beneficiary side: their fusion with the apparatus locks them into defending it, which stabilizes enforcement without their being targets. No directionality_overrides are authored: the derivation chain from beneficiary/victim declarations plus exit options produces the correct qualitative structure for every seat, and the coarse power-atom keying of overrides would misfire across same-power seats with opposed roles (organized crews vs. organized departments).
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents mislabeling in both directions. Reading the regime as pure snare would erase the genuine coordination function: skill decay is real, self-assessment fails, and the multi-mechanism architecture addresses documented failure modes of each single mechanism — the founding problem is corroborated as live by sources outside the beneficiary set. Reading it as pure rope would erase the asymmetric capture: the same structure that maintains competence channels fees to an oligopolistic vendor sector, headcount to internal empires, and jurisdiction to regulators, sustained by active enforcement and by the blame asymmetry that makes de-escalation career-dangerous. It is not scaffold: no sunset clause exists anywhere in the regime and the reading itself declares the optimization perpetual. It is not piton: the function is alive, the founding problem is live, and concentrated beneficiaries demonstrably maintain the apparatus — the mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges and finds no zombie flag, correctly. Rising theater_ratio is monitored as the early symptom that would eventually warrant reclassification if sign-off ritual displaced functional exercise.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_simulation_sufficiency,
    'Does the multi-mechanism necessity claim survive controlled comparison against the simulation_sufficiency sibling reading — i.e., is any mechanism besides simulation actually necessary?',
    'Longitudinal randomized or quasi-randomized comparisons of single-mechanism versus multi-mechanism regimes, measuring transfer to line performance and skill-decay curves over multi-year horizons.',
    'If simulation alone suffices, the refresher, procedural, and audit layers convert to extractive overhead and this constraint drifts sharply snare-ward; if the multi-mechanism claim holds, a larger share of measured extraction is genuine coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_simulation_sufficiency, empirical, 'Whether the hybrid reading''s core premise defeats its nearest sibling on evidence.').

omega_variable(
    kernel_reading_contest_real_incident_necessity,
    'Does any artificial mechanism occupy the competence kernel, or does only actual catastrophic incident exposure do so (the real_incident_necessity sibling reading)?',
    'Fidelity-gradient studies comparing retention and adaptation after simulated versus real event exposure, using ethically observable cases (serious near-events, post-hoc analysis of real-event performance).',
    'If the sibling reading is right, the entire simulated and procedural apparatus is performance without function — theater_ratio approaches unity and the constraint reclassifies; if wrong, simulation retains a functional core and the current classification stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_real_incident_necessity, empirical, 'Whether artificial exercise occupies the kernel at all, per the most radical sibling.').

omega_variable(
    optimal_configuration_existence,
    'Is ''no consensus on optimal configuration'' a temporary epistemic gap that data will close, or a structural fact — the optimum varies with fleet composition, automation level, and route/incident profile, so no invariant optimum exists?',
    'Cross-domain meta-analysis testing whether any single configuration dominates across contexts, or whether context-dependence is systematic.',
    'If no invariant optimum exists, perpetual optimization is permanent, the research economy is permanent, and the extraction floor stays elevated indefinitely; if one exists, the regime could converge, shed excess mechanisms, and the constraint could decay toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_configuration_existence, conceptual, 'Whether the perpetual-research property of this reading is contingent or built in.').

omega_variable(
    blame_risk_rulemaking_driver,
    'How much of the persistent over-provision of mandated training is driven by regulator blame aversion (no career penalty for mandating more, catastrophic penalty for the incident following a cut) rather than by dose-response evidence?',
    'Content analysis of rulemaking dockets coding evidentiary citations against precautionary rhetoric, keyed to whether rulemaking followed salient incidents.',
    'If blame-driven, the suppression component is political rather than technical, and effective reform requires liability-allocation change rather than better training science; measured extraction attributable to blame asymmetry would resist evidence-based remedies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(blame_risk_rulemaking_driver, empirical, 'The share of enforcement persistence explained by blame asymmetry rather than evidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__hybrid_occupation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__hybrid_occupation, theater_ratio, 0, 0.22).
narrative_ontology:measurement(comp_tr_t6, competence_occupation__hybrid_occupation, theater_ratio, 6, 0.26).
narrative_ontology:measurement(comp_tr_t12, competence_occupation__hybrid_occupation, theater_ratio, 12, 0.3).
narrative_ontology:measurement(comp_tr_t18, competence_occupation__hybrid_occupation, theater_ratio, 18, 0.33).
narrative_ontology:measurement(comp_tr_t24, competence_occupation__hybrid_occupation, theater_ratio, 24, 0.37).
narrative_ontology:measurement(comp_tr_t30, competence_occupation__hybrid_occupation, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__hybrid_occupation, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(comp_be_t6, competence_occupation__hybrid_occupation, base_extractiveness, 6, 0.44).
narrative_ontology:measurement(comp_be_t12, competence_occupation__hybrid_occupation, base_extractiveness, 12, 0.48).
narrative_ontology:measurement(comp_be_t18, competence_occupation__hybrid_occupation, base_extractiveness, 18, 0.51).
narrative_ontology:measurement(comp_be_t24, competence_occupation__hybrid_occupation, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(comp_be_t30, competence_occupation__hybrid_occupation, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__hybrid_occupation, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(comp_su_t6, competence_occupation__hybrid_occupation, suppression_requirement, 6, 0.42).
narrative_ontology:measurement(comp_su_t12, competence_occupation__hybrid_occupation, suppression_requirement, 12, 0.46).
narrative_ontology:measurement(comp_su_t18, competence_occupation__hybrid_occupation, suppression_requirement, 18, 0.49).
narrative_ontology:measurement(comp_su_t24, competence_occupation__hybrid_occupation, suppression_requirement, 24, 0.51).
narrative_ontology:measurement(comp_su_t30, competence_occupation__hybrid_occupation, suppression_requirement, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__hybrid_occupation, identity_coordination).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, competence_occupation__simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, competence_occupation__real_incident_necessity).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'competence maintenance training' decomposes into three structurally distinct constraints corresponding to three readings of the competence_occupation kernel. This story (hybrid_occupation) authors epsilon for the standing multi-mechanism arrangement as the hybrid reading assesses it: necessary core plus unvalidated configuration plus configuration-gap rents (epsilon 0.58). The simulation_sufficiency sibling authors epsilon for a hypothetical collapsed single-mechanism regime and treats the non-simulation layers of the standing arrangement as removable overhead (lower epsilon for the collapsed form, higher for the residual layers). The real_incident_necessity sibling authors epsilon for the standing arrangement as seen from the position that artificial exercise is categorically insufficient (near-total theater attribution). The upstream reading in empirical confidence is hybrid_occupation — the multi-mechanism consensus is the operating orthodoxy — and it influences both siblings by defining the apparatus whose sufficiency they contest. All three stories link mutually through network.affects_constraints; none is evaluable through another's observables without violating epsilon-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
