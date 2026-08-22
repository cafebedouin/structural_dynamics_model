% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__real_catastrophe_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__real_catastrophe_only, []).

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
 *   constraint_id: competence_exercise_validity__real_catastrophe_only
 *   human_readable: Real-Catastrophe-Only Competence Exercise Doctrine
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   Across nuclear, aviation, chemical, and emergency-response sectors
 *   operates a standing doctrine: competence is truly exercised only by real
 *   catastrophe, and simulation — however faithful — is an insufficient
 *   substitute. This story instantiates the real_catastrophe_only reading of
 *   the competence_exercise_validity kernel as a clean, epsilon-invariant
 *   constraint: the referent of every measure below is the operative
 *   real-catastrophe-only regime itself (experience-weighted promotion,
 *   drill-discounting review boards, simulation budgets trimmed under the
 *   insufficiency argument), never the simulation-forward arrangements the
 *   sibling readings would install. The regime has a genuine epistemic core —
 *   some stressors are not yet reproducible — and a growing extractive
 *   superstructure: a status economy in which firsthand disaster experience
 *   is the only fully valid credential, budget deferral dressed as epistemic
 *   rigor, and readiness decay that no one is licensed to detect between
 *   events. Per the epsilon-invariance principle, the colloquial debate 'does
 *   simulation count?' decomposes into three linked stories (this one;
 *   simulation_as_proxy; continuous_refresh_hybrid), each with its own
 *   epsilon, beneficiary structure, and enforcement form; the network edges
 *   here point at the siblings.
 *
 * KEY AGENTS:
 *   - veteran_operators_with_incident_experience: primary beneficiary (organized/identity_locked) — collects the status and authority premium the doctrine mints from real-event scarcity
 *   - safety_directors_and_review_boards: agenda setter (institutional/mobile) — administers the experience-weighting rules that enforce the doctrine
 *   - junior_operators_without_incident_experience: primary target (moderate/constrained) — bears discounted credentials and stalled advancement
 *   - simulation_and_training_departments: target (organized/constrained) — product ruled categorically insufficient; budgets trimmed accordingly
 *   - executive_budget_holders: secondary beneficiary (powerful/mobile, immediate horizon) — converts the doctrine into deferred training spend
 *   - incident_response_professions: secondary beneficiary (organized/identity_locked) — funding and professional identity ride on real-event primacy
 *   - hazard_adjacent_public: diffuse target (powerless/trapped) — carries residual risk and undetected decay
 *   - resilience_engineering_researchers: excluded voice (organized/mobile) — holds the strongest counter-evidence, holds no vote
 *   - regulators_mandating_drills: analytical observer (institutional/analytical) — keeps a floor of mandated simulated exercise in place
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__real_catastrophe_only, 0.58).
domain_priors:suppression_score(competence_exercise_validity__real_catastrophe_only, 0.55).
domain_priors:theater_ratio(competence_exercise_validity__real_catastrophe_only, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, extractiveness, 0.58).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__real_catastrophe_only, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__real_catastrophe_only, "Real-Catastrophe-Only Competence Exercise Doctrine").
narrative_ontology:topic_domain(competence_exercise_validity__real_catastrophe_only, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__real_catastrophe_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__real_catastrophe_only, '8f0b422f-24d5-4c33-b336-ddd07e3a6163').
narrative_ontology:cs_kernel_codification('8f0b422f-24d5-4c33-b336-ddd07e3a6163', distributed).
narrative_ontology:cs_authority_grounding('8f0b422f-24d5-4c33-b336-ddd07e3a6163', practice).
narrative_ontology:cs_interpretation_layer_present('8f0b422f-24d5-4c33-b336-ddd07e3a6163').
narrative_ontology:cs_reading_relation('8f0b422f-24d5-4c33-b336-ddd07e3a6163', competence_exercise_validity__simulation_as_proxy, forecloses).
narrative_ontology:cs_reading_relation('8f0b422f-24d5-4c33-b336-ddd07e3a6163', competence_exercise_validity__continuous_refresh_hybrid, forecloses).
narrative_ontology:cs_axiom('8f0b422f-24d5-4c33-b336-ddd07e3a6163', foundational, only_real_catastrophe_validates_competence).
narrative_ontology:cs_axiom_status(only_real_catastrophe_validates_competence, holdable).
narrative_ontology:cs_axiom_grounding('8f0b422f-24d5-4c33-b336-ddd07e3a6163', only_real_catastrophe_validates_competence, empirically_contingent).
narrative_ontology:cs_axiom('8f0b422f-24d5-4c33-b336-ddd07e3a6163', secondary, genuine_stakes_irreducible_by_simulation).
narrative_ontology:cs_axiom_status(genuine_stakes_irreducible_by_simulation, holdable).
narrative_ontology:cs_axiom_grounding('8f0b422f-24d5-4c33-b336-ddd07e3a6163', genuine_stakes_irreducible_by_simulation, empirically_contingent).
narrative_ontology:cs_reference_frame('8f0b422f-24d5-4c33-b336-ddd07e3a6163', catastrophe_validated_readiness).
narrative_ontology:cs_drift_state('8f0b422f-24d5-4c33-b336-ddd07e3a6163', high_fidelity_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8f0b422f-24d5-4c33-b336-ddd07e3a6163', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, veteran_operators_with_incident_experience).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, incident_response_professions).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, executive_budget_holders).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, junior_operators_without_incident_experience).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, simulation_and_training_departments).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, hazard_adjacent_public).
narrative_ontology:constraint_vindicates(competence_exercise_validity__real_catastrophe_only, stressor_irreducibility_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior operators, pilots, and shift supervisors who personally managed one or more major real-world failures. Their accounts of those events anchor incident reviews, promotion boards, and training curricula. Promotion opportunities and consulting income flow disproportionately to them because review panels weight firsthand catastrophe experience above any simulated performance. Leaving the profession would forfeit the standing their event histories confer; their self-description as the ones who have actually been through it is inseparable from the credential's value.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, veteran_operators_with_incident_experience, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__real_catastrophe_only, veteran_operators_with_incident_experience, agenda_setter).

% Set the weighting rules that decide what counts as qualifying experience for licensure, promotion, and post-incident accountability. Convene after-action boards, certify which events enter the official experience ledger, and decide whether simulator outcomes may substitute for operational history. Members move between firms and regulators carrying the same weighting conventions with them.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, safety_directors_and_review_boards, agenda_setter,
    institutional, generational, mobile, national).

% Licensed crews whose demonstrated simulator performance carries reduced weight in promotion and assignment decisions because it is classified as unproven. They accumulate credentials the prevailing standard discounts, and their advancement effectively waits for scarce real events — some of which they may be assigned toward. Exiting the profession means abandoning licensed, sunk training investment.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, junior_operators_without_incident_experience, payer,
    moderate, biographical, constrained, national).

% In-house simulator centers, training vendors, and instructional-design staff whose product is formally categorized as never fully valid. Budget requests are trimmed on the argument that the product cannot do the one thing that matters, and findings from full-scope scenarios are admitted only as supplementary. Capital equipment and specialized staff tie them to the training market they serve.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, simulation_and_training_departments, payer,
    organized, biographical, constrained, national).

% Fire services, emergency-management agencies, and industrial response teams whose staffing levels, budget justifications, and professional honor structures are organized around responding to real events. Actual incidents validate their methods, recruit their members, and justify their funding cycles; a long quiet period reads internally as erosion of relevance rather than success.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, incident_response_professions, beneficiary,
    organized, biographical, identity_locked, regional).

% Plant managers, airline operations executives, and finance delegates who can defer simulator procurement, scenario refresh cycles, and backfill-for-training time on the stated ground that such spending purchases an inferior substitute. The savings land in the current budget period; the readiness consequences surface on timelines longer than their tenure.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, executive_budget_holders, beneficiary,
    powerful, immediate, mobile, national).

% Residents near chemical plants, flight paths, and reactor sites who carry the residual risk that accumulates when readiness investment is deferred and decay goes undetected between real events. They have no seat in review boards and no practical ability to relocate away from the facilities their towns depend on for employment.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, hazard_adjacent_public, payer,
    powerless, generational, trapped, local).

% Human-factors academics and safety-science practitioners who publish transfer-of-training studies arguing that high-fidelity simulation plus structured drill cycles captures most of what real events teach. Cited rarely in review-board deliberations and holding no vote in experience-weighting decisions; their main channel of influence is regulator advisory panels.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, resilience_engineering_researchers, excluded,
    organized, generational, mobile, global).

% Aviation, nuclear, and chemical regulators that require recurring simulator checks and emergency drills regardless of the prevailing doctrine, keeping a floor of simulated exercise in place. They inspect both real-event responses and drill records, and their fidelity requirements steadily raise the quality of the proxy the doctrine discounts.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, regulators_mandating_drills, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_validity__real_catastrophe_only, veteran_operators_with_incident_experience).
narrative_ontology:fixing_cost_class(competence_exercise_validity__real_catastrophe_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates validation authority on the only stressor class that reproduces genuine stakes; guards against false assurance from low-fidelity drills and from drill-always-passes complacency; maintains institutional respect for the gravity of rare high-consequence events.
% TRANSFER_FUNCTION: Moves professional status, promotion eligibility, and post-incident authority from credentialed-but-untested practitioners to incident-experienced veterans; moves training budget away from simulation programs and toward operational exposure or simple deferral; moves residual operational risk onto junior staff and hazard-adjacent publics.
% ABSENT_VOICES: Human-factors and resilience-engineering researchers hold the strongest counter-evidence and are outside the review rooms where weighting rules are set; junior operators are present without vote; the hazard-adjacent public is absent entirely. The unanimity of review-board practice partly reflects who was never in the room.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, promotion criteria, review-board weighting, training budgets, response-force funding justifications, and the entire experience-ledger economy built on real-event scarcity would reorganize around whichever validity standard replaced it; veteran authority would need a new basis, and simulation investment would reroute toward whichever sibling reading won.
% FOUNDING_PROBLEM: In the era before full-scope simulators, high-hazard industries had no way to distinguish genuine crew readiness from paper compliance except waiting for real events; early part-task trainers and tabletop exercises demonstrably failed to predict how crews behaved under actual failure conditions, and disasters occurred in organizations with spotless exercise records.
% FOUNDING_PROBLEM_CORROBORATION: Official accident-inquiry reports from the pre-simulator era corroborate that the founding problem was real: investigations repeatedly found crews unprepared despite passed exercises — corroboration independent of the benefiting parties. Whether the problem remains live is attested differently by seat: veteran incumbents and response-profession leadership assert liveness; transfer-of-training researchers and regulator fidelity assessments contend modern full-scope simulation has closed most of the gap. Corroboration for the historical problem exists outside the beneficiary set; corroboration for its present liveness does not extend beyond the doctrine's holders.
narrative_ontology:disappearance_verdict(competence_exercise_validity__real_catastrophe_only, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__real_catastrophe_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__real_catastrophe_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_validity__real_catastrophe_only, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__real_catastrophe_only, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__real_catastrophe_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__real_catastrophe_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_validity__real_catastrophe_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-high (0.58) because the doctrine converts an epistemic position into a transfer: status, promotion eligibility, and training budget move along channels the doctrine itself defines, and the discount grows more costly to honor as the discounted tool improves. Suppression (0.55, raw and unscaled — only extractiveness is scaled by directionality and scope) is institutional gatekeeping rather than force: weighting rules, certification criteria, and budget arguments, with an internalized residue in junior self-assessment (estimated roughly 70% structural, 30% internalized; see the internalized_discount_mechanism omega). Theater (0.32) is moderate-low but rising: lessons-learned bureaucracies and ritual incident retrospectives increasingly surround a core doctrinal labor that has stayed constant — and there is a specific irony worth recording: the doctrine condemns simulation as unreal, while its own review apparatus accretes performances that change no weighting rule. Accessibility collapse (0.48) is partial: simulation alternatives persist physically and are regulator-mandated at a floor, but their evidentiary standing collapses once the doctrine is accepted. Resistance (0.62) is substantial and organized: training departments, human-factors researchers, and regulator fidelity mandates push back continuously. The three measurement series share one time grid (every tracked metric authored at every point 0–50). All three rise monotonically for one reason: each simulator-generation upgrade increased the counter-evidence the review apparatus had to discount, so enforcement hardened (suppression_requirement 0.40 to 0.55), the doctrine-vs-proxy gap widened into accumulating extraction (0.45 to 0.58), and the surrounding ritual layer thickened (0.20 to 0.32). This is a ratchet, not a cycle — no oscillation mechanism is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the veteran seat the arrangement is meritocracy: scar tissue is the only honest credential, and discounting untested performance is epistemic hygiene. From the junior-operator seat the same rules operate as a gated queue in which advancement waits on scarce real events — some of which they may be assigned toward. From the executive seat it is prudence: refusing to buy an admittedly inferior substitute. From the regulator seat it is a manageable bias, offset by mandated drill floors. Identity-lock dynamics bind the veteran seat specifically: the fusion is professional-relational — self-concept constituted through having been present at real failure ('the ones who have actually been through it') — so the credential's market value and the holder's self-description are the same object, and exit would forfeit both at once. If that frame broke (a credible transfer study showing parity), veteran authority would need wholesale reconstruction and the enforcement coalition would fragment along the seam between those with and without event histories. Coalition potential for the powerless seat: hazard-adjacent publics coordinate episodically through tort litigation and siting hearings; the coordination is real but intermittent and locally defeated, which is why the seat remains effectively trapped.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to the deep-beneficiary end: veterans (identity_locked — no exit from the identity the doctrine certifies), incident-response professions (identity_locked — quiet periods read as erosion of relevance), and executives (mobile, immediate horizon — gains land inside their tenure, costs land after it). Victim declarations map to the target end: junior operators (constrained — licensed, sunk training investment), simulation departments (constrained — capital and specialization tied to the discounted product), and the hazard-adjacent public (trapped — employment tied to the facility, no relocation path), who sit nearest the full-target end. Review boards derive a mid-to-beneficiary directionality as administrators who do not collect the largest rents but set the rules that mint them. Receipt: the doctrine's minted currency — validated-experience standing — accrues demonstrably to the veteran seat, so gain_flow names it; budget savings are real but spread across many firms and fiscal periods, and no single seat captures them. No directionality overrides are used: role plus exit-option derivation reproduces every structural relationship without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the doctrine as pure extraction would erase its live epistemic function — the founding demonstrations (crews passing primitive exercises, then failing real events) were real, and some stressor gap plausibly remains. Reading it as pure coordination would launder the status economy: the same rule that guards against false assurance also mints veteran authority, defers training spend, and licenses the phrase 'the record proves nothing' as unfalsifiable shelter. The tangled_rope claim keeps both faces visible and forces the engine to price them together. On obsolescence: the founding problem is contested, not dead — holders assert liveness, while transfer researchers and regulator fidelity assessments date its decline to the full-scope simulator era — so this is not an inertial relic administered by nobody; it is actively maintained and profitably maintained, which bars the piton reading. Fixing cost is prohibitive for whoever could fix it: the review boards that would rewrite the weighting rules derive their own legitimacy from the current standard, reweighting would invalidate incumbent credentials en masse, and if the reading is epistemically right, full proxy-trust removes the only guard against false assurance — the cost of being wrong about the fix exceeds the benefit in both directions. The mismatch consumer should watch founding_problem_status x disappearance_verdict closely: if fidelity studies ever close the gap, the coordination half dies while enforcement persists — the classic zombie transition this analysis exists to catch early.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is the real_catastrophe_only reading of the competence_exercise_validity kernel; what structural changes follow if a sibling reading displaces it?',
    'Track adoption of the sibling readings in review-board weighting rules and regulator fidelity policy: simulation_as_proxy adoption dissolves the veteran status premium and reclassifies junior staff from discounted to validated; continuous_refresh_hybrid adoption converts the standing regime into a scheduled drill-cycle arrangement with transitional obligations.',
    'Under simulation_as_proxy the standing arrangement reads as rent collection on a rejected epistemic standard; under continuous_refresh_hybrid it reads as a transitional stage; the victim and beneficiary sets swap places.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel contest: which reading of competence_exercise_validity is operative.').

omega_variable(
    simulation_fidelity_threshold,
    'Does a simulation-fidelity threshold exist beyond which transfer to real-event performance is adequate, collapsing the categorical insufficiency claim?',
    'Matched-cohort transfer studies linking full-scope simulator performance to subsequent real-event outcomes (line-oriented flight training program data, nuclear crew evaluation records), pooled across simulator fidelity generations.',
    'A demonstrated threshold converts the doctrine''s coordination half into pure gatekeeping and pushes the arrangement toward pure extraction; persistent non-transfer at all tested fidelities strengthens the genuine-coordination half.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_threshold, empirical, 'Whether the insufficiency claim is categorical or a moving threshold.').

omega_variable(
    veteran_advantage_survivorship_bias,
    'Does firsthand catastrophe experience causally improve later judgment, or does the observed veteran advantage reflect survivorship selection — those whose errors were fatal never lived to teach?',
    'Longitudinal decision-quality studies comparing incident-experienced and simulator-trained operators under matched high-stakes conditions, controlling for selection into incident exposure.',
    'If the advantage is a selection artifact, the beneficiary structure inverts — veterans collect standing on unearned epistemic authority — and the arrangement trends toward pure extraction; if causal, the status premium prices a real capability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veteran_advantage_survivorship_bias, empirical, 'Causal versus survivorship origin of the veteran advantage.').

omega_variable(
    clean_record_luck_or_redundancy,
    'Can a clean safety record under this regime be decomposed into proven adequacy versus luck versus system redundancy, as the reading''s own structural delta asserts?',
    'Precursor-event and near-miss density analysis calibrated against redundant-barrier counts, per facility, across the interval.',
    'If records are largely luck plus redundancy, the doctrine''s central concession is accurate and its demand for real tests is epistemically motivated; if records track competence, the concession functions as unfalsifiable excuse-making that shields the regime from accountability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clean_record_luck_or_redundancy, empirical, 'Epistemic status of safety records under the doctrine.').

omega_variable(
    internalized_discount_mechanism,
    'Is the suppression borne by junior operators structural (board weighting rules, promotion criteria) or internalized (self-discounting that persists even where boards credit drill performance)?',
    'Post-mobility trajectory: compare self-assessed readiness and advancement negotiation behavior of junior staff who move to simulation-forward firms against those who stay; persistence of the discount after the rule environment changes indicates internalization.',
    'If substantially internalized, effective suppression exceeds the structural measure — targets carry the discount with them across employers — and remediation requires more than rule change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_discount_mechanism, empirical, 'Structural versus internalized suppression in credential discounting.').

omega_variable(
    exercise_sense_ambiguity,
    'Does ''exercise'' in the kernel mean validation-under-genuine-stakes or retention-maintenance? The foreclosure relations this reading declares assume the undivided sense.',
    'Catalog review-board usage: whether the doctrine is invoked to reject validation claims only, or also to reject maintenance claims (the refresher value of recurring drills).',
    'If the senses split in practice, continuous_refresh_hybrid becomes holdable alongside this reading (drills maintain, catastrophe validates) and the declared foreclosure against it overstates the logical structure; classification of the hybrid regime shifts accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exercise_sense_ambiguity, conceptual, 'Sense ambiguity in ''exercise'' underlying the foreclosure structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__real_catastrophe_only, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_validity__real_catastrophe_only, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(comp_tr_t0, observed).
narrative_ontology:measurement(comp_tr_t10, competence_exercise_validity__real_catastrophe_only, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(comp_tr_t10, observed).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_validity__real_catastrophe_only, theater_ratio, 20, 0.25).
narrative_ontology:measurement_basis(comp_tr_t20, observed).
narrative_ontology:measurement(comp_tr_t30, competence_exercise_validity__real_catastrophe_only, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(comp_tr_t30, observed).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_validity__real_catastrophe_only, theater_ratio, 40, 0.3).
narrative_ontology:measurement_basis(comp_tr_t40, observed).
narrative_ontology:measurement(comp_tr_t50, competence_exercise_validity__real_catastrophe_only, theater_ratio, 50, 0.32).
narrative_ontology:measurement_basis(comp_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(comp_be_t0, observed).
narrative_ontology:measurement(comp_be_t10, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(comp_be_t10, observed).
narrative_ontology:measurement(comp_be_t20, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 20, 0.51).
narrative_ontology:measurement_basis(comp_be_t20, observed).
narrative_ontology:measurement(comp_be_t30, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 30, 0.53).
narrative_ontology:measurement_basis(comp_be_t30, observed).
narrative_ontology:measurement(comp_be_t40, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 40, 0.56).
narrative_ontology:measurement_basis(comp_be_t40, observed).
narrative_ontology:measurement(comp_be_t50, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 50, 0.58).
narrative_ontology:measurement_basis(comp_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(comp_su_t0, observed).
narrative_ontology:measurement(comp_su_t10, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 10, 0.43).
narrative_ontology:measurement_basis(comp_su_t10, observed).
narrative_ontology:measurement(comp_su_t20, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 20, 0.46).
narrative_ontology:measurement_basis(comp_su_t20, observed).
narrative_ontology:measurement(comp_su_t30, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 30, 0.49).
narrative_ontology:measurement_basis(comp_su_t30, observed).
narrative_ontology:measurement(comp_su_t40, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(comp_su_t40, observed).
narrative_ontology:measurement(comp_su_t50, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 50, 0.55).
narrative_ontology:measurement_basis(comp_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__real_catastrophe_only, information_standard).
narrative_ontology:affects_constraint(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity__simulation_as_proxy).
narrative_ontology:affects_constraint(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity__continuous_refresh_hybrid).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial debate 'does simulation count as real exercise?' is one label over three structurally distinct claims with different epsilon values, beneficiary structures, and enforcement forms. This story (real_catastrophe_only) authors epsilon for the incumbent real-catastrophe-only regime only; simulation_as_proxy authors epsilon for the simulation-validated regime the training industry would install; continuous_refresh_hybrid authors epsilon for the drill-cycle regime. Upstream/downstream: the historical demonstration cases (drill-passing crews failing real events) are cited as evidence by this reading against the proxy reading, so this story exerts legitimating pressure on the family's contested edge; the hybrid mediates. Each member links the others via affects_constraints per the family rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
