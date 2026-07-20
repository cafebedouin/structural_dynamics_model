% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__credibility_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__credibility_paradox_reading, []).

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
 *   constraint_id: nuclear_impossibility_kernel__credibility_paradox_reading
 *   human_readable: Nuclear Deterrence Credibility Paradox
 *   domain: strategic/international_relations/nuclear_deterrence
 *
 * SUMMARY:
 *   The nuclear impossibility kernel is the stabilized commitment that
 *   nuclear weapons fundamentally alter the possibility of great-power war.
 *   The credibility_paradox_reading interprets this kernel as generating a
 *   structural trap: because use guarantees mutual destruction, the threat to
 *   use is inherently incredible; yet deterrence requires credibility. This
 *   paradox drives a dynamic of costly signaling, force modernization, and
 *   escalation-ladder planning that extracts enormous resources and imposes
 *   existential risk while coordinating against direct great-power war. The
 *   constraint is claimed as tangled_rope because it couples genuine
 *   coordination (the long peace between nuclear powers) with asymmetric
 *   extraction (existential risk borne by civilians, resource capture by the
 *   weapons complex, and structural subordination of non-nuclear states). The
 *   metrics are authored independently: high extractiveness and suppression
 *   reflect the active enforcement of the nuclear order, while the high
 *   theater ratio reflects the performative nature of making an incredible
 *   threat credible.
 *
 * KEY AGENTS:
 *   - nuclear_weapons_states: Primary agenda-setter (institutional/constrained) â designs deterrence strategy and bears strategic dilemma costs
 *   - nuclear_weapons_complex: Primary beneficiary (powerful/mobile) â captures resource flows from modernization imperatives
 *   - global_civilian_population: Primary payer (powerless/trapped) â bears existential risk without representation
 *   - non_nuclear_weapon_states: Secondary payer (moderate/constrained) â structurally subordinated by the NPT-extended-deterrence architecture
 *   - strategic_studies_establishment: Analytical observer (institutional/analytical) â generates doctrinal legitimacy for the credibility imperative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, 0.78).
domain_priors:suppression_score(nuclear_impossibility_kernel__credibility_paradox_reading, 0.72).
domain_priors:theater_ratio(nuclear_impossibility_kernel__credibility_paradox_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__credibility_paradox_reading, tangled_rope).
narrative_ontology:human_readable(nuclear_impossibility_kernel__credibility_paradox_reading, "Nuclear Deterrence Credibility Paradox").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__credibility_paradox_reading, "strategic/international_relations/nuclear_deterrence").

domain_priors:requires_active_enforcement(nuclear_impossibility_kernel__credibility_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__credibility_paradox_reading, '9e8a3e3e-9a9d-4604-a174-9bbe93b4d338').
narrative_ontology:cs_kernel_codification('9e8a3e3e-9a9d-4604-a174-9bbe93b4d338', formalized).
narrative_ontology:cs_authority_grounding('9e8a3e3e-9a9d-4604-a174-9bbe93b4d338', expertise).
narrative_ontology:cs_interpretation_layer_present('9e8a3e3e-9a9d-4604-a174-9bbe93b4d338').
narrative_ontology:cs_reading_relation('9e8a3e3e-9a9d-4604-a174-9bbe93b4d338', nuclear_impossibility_kernel__structural_contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('9e8a3e3e-9a9d-4604-a174-9bbe93b4d338', nuclear_impossibility_kernel__rational_dropout_reading, influences).
narrative_ontology:cs_axiom('9e8a3e3e-9a9d-4604-a174-9bbe93b4d338', foundational, deterrence_requires_usable_options).
narrative_ontology:cs_axiom_status(deterrence_requires_usable_options, holdable).
narrative_ontology:cs_axiom_grounding('9e8a3e3e-9a9d-4604-a174-9bbe93b4d338', deterrence_requires_usable_options, empirically_contingent).
narrative_ontology:cs_axiom('9e8a3e3e-9a9d-4604-a174-9bbe93b4d338', foundational, incredible_threat_generates_instability).
narrative_ontology:cs_axiom_status(incredible_threat_generates_instability, holdable).
narrative_ontology:cs_axiom_grounding('9e8a3e3e-9a9d-4604-a174-9bbe93b4d338', incredible_threat_generates_instability, empirically_contingent).
narrative_ontology:cs_reference_frame('9e8a3e3e-9a9d-4604-a174-9bbe93b4d338', incredible_deterrent_base).
narrative_ontology:cs_drift_state('9e8a3e3e-9a9d-4604-a174-9bbe93b4d338', modernization_and_counterforce_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9e8a3e3e-9a9d-4604-a174-9bbe93b4d338', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_weapons_states).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_weapons_complex).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, global_civilian_population).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_weapon_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain nuclear arsenals and deterrence doctrines. They design escalation ladders, modernization programs, and alliance guarantees to make incredible threats appear credible. They cannot unilaterally disarm without risking strategic subordination, yet they bear the fiscal and risk burden of arsenal maintenance.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_weapons_states, agenda_setter,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_weapons_states, beneficiary).

% Designs, manufactures, and maintains nuclear delivery systems and warheads. Captures sustained revenue from modernization cycles justified by the credibility imperative. Their interests align with threat inflation and the perpetual upgrading of arsenals.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_weapons_complex, beneficiary,
    powerful, generational, mobile, national).

% Lives under the permanent shadow of accidental or deliberate nuclear annihilation without representation in strategic planning. They fund arsenals through taxation and bear the existential risk, but have no seat at deterrence decision tables.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, global_civilian_population, payer,
    powerless, civilizational, trapped, universal).

% Bound by the NPT regime that legitimizes the nuclear arsenals of recognized weapons states while denying them equivalent weapons. They experience structural coercion through extended deterrence frameworks and bear downstream environmental and security costs without strategic autonomy.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_weapon_states, payer,
    moderate, generational, constrained, global).

% Generates the deterrence theory, game-theoretic models, and strategic doctrine that interpret the credibility paradox. They operate as the interpretive authority between the kernel and practice, with career incentives favoring doctrinal continuity over radical disarmament proposals.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, strategic_studies_establishment, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_weapons_complex).
narrative_ontology:fixing_cost_class(nuclear_impossibility_kernel__credibility_paradox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents direct great-power nuclear war by making the costs of use unacceptable to rational adversaries through mutual vulnerability.
% TRANSFER_FUNCTION: Moves security, resources, and existential risk: nuclear states gain relative security and strategic dominance; the global population and non-nuclear states bear the risk of annihilation and structural subordination; the weapons complex captures resource flows from modernization.
% ABSENT_VOICES: Anti-nuclear activists, disarmament advocates, and non-nuclear states in the Global South are formally included in treaty frameworks but structurally excluded from the deterrence logic that governs their security environment; civilian populations have no seat at the strategy table despite bearing the risk.
% DISAPPEARANCE_RATIONALE: If the credibility paradox constraint vanishedâif nuclear threats were either inherently credible without cost or universally acknowledged as incredibleâthe entire strategic architecture of the nuclear age would collapse: alliance structures would renegotiate, arms expenditure would plummet, and crisis dynamics would lose their current escalation logic.
% FOUNDING_PROBLEM: How to prevent catastrophic great-power war in an era of apocalyptic weapons.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear strategists and security scholars outside the weapons-complex corroborate the founding problem; peace researchers and disarmament scholars contest that the current arrangement solves it, arguing the paradox creates its own catastrophic risks.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__credibility_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__credibility_paradox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__credibility_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is authored high because the credibility paradox forces continuous, expensive modernization and crisis-posturing that extracts trillions in resources and imposes civilizational-scale risk. Suppression (0.72) is high: alternatives such as global disarmament or no-first-use are structurally suppressed by security-dilemma logic and alliance enforcement. Theater ratio (0.62) reflects that much nuclear strategyâlimited options, escalation dominance, demonstration strikesâis performative signaling to bridge the credibility gap rather than functional war-fighting. Accessibility collapse (0.60) is moderate-high: within the strategic community, alternatives appear naive, but outside it resistance retains epistemic standing. Resistance (0.50) is moderate: anti-nuclear movements and non-aligned states contest the arrangement but lack institutional power to alter it. The temporal series show extraction and theater rising through the Cold War, dipping slightly in the unipolar moment, and resurging with great-power competition.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (nuclear weapons states) experiences the constraint as a necessary, burdensome coordination mechanism that prevents worse outcomes; their directionality is toward the beneficiary end despite fiscal costs. The payer seats (global civilians, non-nuclear states) experience the same structure as imposed existential risk and structural coercion without consent. The weapons complex experiences pure subsidy. The strategic studies establishment experiences it as an analytical puzzle. The engine should compute divergent seat classifications: the weapons complex and extended beneficiaries as near-rope, global civilians as near-snare, and the states themselves as tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapons states are declared beneficiaries (security from deterrence) and agenda-setters (they design the constraint), which pushes their d toward the beneficiary pole, though their constrained exit and civilizational risk prevent full subsidy. The weapons complex is a pure beneficiary with mobile exit, giving it very low d. Global civilians are declared victims with trapped exit and powerless status, placing d near 1.0. Non-nuclear states are declared victims with constrained exit, placing d high but slightly below civilians. The strategic studies establishment is observer/analytical, excluded from beneficiary/victim derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the tangled-rope classification, this constraint could be misread as a Mountain (the 'nuclear revolution' as immutable strategic law) or as a pure Snare (the military-industrial complex extracting rents). The mountain reading ignores the constructed, doctrinal nature of the credibility paradox and its active enforcement. The snare reading ignores the genuine coordination against great-power war. Tangled rope captures both: the coordination is real (direct nuclear war has been avoided between great powers), but the same structure that coordinates also extracts (through arms-race costs, accidental-risk imposition, and nuclear coercion). The R5 genealogy corroborates this: the founding problemâpreventing catastrophic warâremains live, but the specific solution (credible incredible threats) is contested and generates its own pathologies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credibility_paradox_naturalness,
    'Is the credibility paradox an inherent structural feature of nuclear technology, or a construct of strategic doctrine that could dissolve under different interpretive traditions?',
    'Comparative analysis of non-Western nuclear doctrines to see if the paradox manifests independently of Western strategic culture.',
    'If doctrine-dependent, the constraint is a constructed commitment system rather than a natural strategic law; this would reclassify the kernel''s authority from expertise to extraction or practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credibility_paradox_naturalness, conceptual, 'Whether the paradox is natural or constructed.').

omega_variable(
    extraction_beneficiary_concentration,
    'Does the credibility paradox primarily extract diffusely from all states and populations, or do concentrated beneficiaries capture specific rents from the constraint?',
    'Economic tracing of nuclear modernization budgets to contractor concentration, and alliance-cost analysis of extended deterrence.',
    'Concentrated capture would shift classification toward snare; diffuse cost-bearing with genuine security provision would support tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_beneficiary_concentration, empirical, 'Whether extraction is captured or diffuse.').

omega_variable(
    usable_options_stability,
    'Do limited nuclear options and counterforce postures genuinely restore credibility, or do they accelerate the path to mutual destruction?',
    'Wargame data, crisis simulation outcomes, and historical close-call analysis.',
    'If usable options restore credibility, the paradox is mitigated; if they accelerate escalation, the constraint is more extractive than coordinating.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(usable_options_stability, empirical, 'Whether usable options stabilize or destabilize.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__credibility_paradox_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t0, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(nucl_tr_t10, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(nucl_tr_t20, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(nucl_tr_t30, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement(nucl_tr_t40, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement(nucl_tr_t50, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 50, 0.52).
narrative_ontology:measurement(nucl_tr_t60, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 60, 0.58).
narrative_ontology:measurement(nucl_tr_t70, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 70, 0.62).

% Extraction over time
narrative_ontology:measurement(nucl_be_t0, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(nucl_be_t10, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(nucl_be_t20, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(nucl_be_t30, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(nucl_be_t40, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(nucl_be_t50, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 50, 0.66).
narrative_ontology:measurement(nucl_be_t60, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 60, 0.72).
narrative_ontology:measurement(nucl_be_t70, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 70, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t0, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(nucl_su_t10, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(nucl_su_t20, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(nucl_su_t30, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(nucl_su_t40, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(nucl_su_t50, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 50, 0.62).
narrative_ontology:measurement(nucl_su_t60, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(nucl_su_t70, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 70, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel__structural_contraction_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel__rational_dropout_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the nuclear_impossibility_kernel. The kernel decomposes into three structurally distinct claims: structural_contraction (mutual annihilation as physical impossibility), rational_dropout (cost-benefit rational abstention), and credibility_paradox (inherent incredibility requiring costly signaling). Each has distinct epsilon, stakeholders, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
