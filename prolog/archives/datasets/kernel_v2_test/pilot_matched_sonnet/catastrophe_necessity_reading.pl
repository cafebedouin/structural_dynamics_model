% ============================================================================
% CONSTRAINT STORY: catastrophe_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_necessity_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: catastrophe_necessity_reading
 *   human_readable: Catastrophe Necessity for Competence Maintenance
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   The catastrophe necessity reading claims that only actual catastrophic
 *   events provide the irreducible stress and uncertainty necessary to
 *   maintain genuine organizational competence in high-reliability domains.
 *   This reading treats simulation as fundamentally insufficient — not merely
 *   as a current technological limitation, but as a natural law of human
 *   learning and organizational memory. The constraint operates through
 *   claimed psychological and physiological mechanisms: the stress responses,
 *   memory consolidation, and organizational attention triggered by actual
 *   catastrophes cannot be replicated in simulation, no matter how
 *   high-fidelity. Organizations that successfully avoid catastrophes for
 *   extended periods face inevitable competence decay, creating a tragic
 *   trade-off between safety (avoiding catastrophes) and competence
 *   maintenance (requiring catastrophic stress). This reading is one of four
 *   perspectives on the catastrophe_proxy_sufficiency kernel; sibling
 *   readings dispute whether simulation can ever provide sufficient proxy
 *   stress, whether sufficiency depends on crossing a fidelity threshold, or
 *   whether simulation provides a degrading partial proxy.
 *
 * KEY AGENTS:
 *   - Operational Safety Margins: Primary victim (powerless/trapped) — the abstract collective good of maintained competence; bears full cost of competence decay in catastrophe-free periods if the mountain claim is true
 *   - Catastrophe-Free Organizations: Secondary victim (moderate/constrained) — organizations that have successfully avoided catastrophes; face claimed inevitable competence erosion regardless of simulation investment
 *   - Safety Engineering Discipline: Institutional actor (institutional/arbitrage) — maintains the catastrophe necessity doctrine as core premise; has arbitrage options but experiences the constraint as natural law
 *   - Catastrophe Necessity Doctrine: Primary beneficiary (non-agent) — the doctrine itself is vindicated by organizational failures in catastrophe-free periods; listed as beneficiary to trigger FSM evaluation but marked as non-agent in stakeholder layer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_necessity_reading, 0.15).
domain_priors:suppression_score(catastrophe_necessity_reading, 0.2).
domain_priors:theater_ratio(catastrophe_necessity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_necessity_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(catastrophe_necessity_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(catastrophe_necessity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_necessity_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(catastrophe_necessity_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_necessity_reading, mountain).
narrative_ontology:human_readable(catastrophe_necessity_reading, "Catastrophe Necessity for Competence Maintenance").
narrative_ontology:topic_domain(catastrophe_necessity_reading, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:emerges_naturally(catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_necessity_reading, '0c324e8c-90cb-48b2-ac58-b7dadc3e5871').
narrative_ontology:cs_kernel_codification('0c324e8c-90cb-48b2-ac58-b7dadc3e5871', distributed).
narrative_ontology:cs_authority_grounding('0c324e8c-90cb-48b2-ac58-b7dadc3e5871', expertise).
narrative_ontology:cs_interpretation_layer_present('0c324e8c-90cb-48b2-ac58-b7dadc3e5871').
narrative_ontology:cs_reading_relation('0c324e8c-90cb-48b2-ac58-b7dadc3e5871', catastrophe_necessity_reading__simulation_as_proxy_catastrophe_reading, coexists_with).
narrative_ontology:cs_reading_relation('0c324e8c-90cb-48b2-ac58-b7dadc3e5871', catastrophe_necessity_reading__hybrid_degradation_reading, coexists_with).
narrative_ontology:cs_reading_relation('0c324e8c-90cb-48b2-ac58-b7dadc3e5871', catastrophe_necessity_reading__simulation_fidelity_threshold, influences).
narrative_ontology:cs_axiom('0c324e8c-90cb-48b2-ac58-b7dadc3e5871', foundational, stress_response_irreducibility).
narrative_ontology:cs_axiom_status(stress_response_irreducibility, holdable).
narrative_ontology:cs_axiom_grounding('0c324e8c-90cb-48b2-ac58-b7dadc3e5871', stress_response_irreducibility, empirically_contingent).
narrative_ontology:cs_axiom('0c324e8c-90cb-48b2-ac58-b7dadc3e5871', foundational, simulation_categorical_insufficiency).
narrative_ontology:cs_axiom_status(simulation_categorical_insufficiency, holdable).
narrative_ontology:cs_axiom_grounding('0c324e8c-90cb-48b2-ac58-b7dadc3e5871', simulation_categorical_insufficiency, empirically_contingent).
narrative_ontology:cs_reference_frame('0c324e8c-90cb-48b2-ac58-b7dadc3e5871', psychological_irreducibility_premise).
narrative_ontology:cs_drift_state('0c324e8c-90cb-48b2-ac58-b7dadc3e5871', contemporary_simulation_technology_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0c324e8c-90cb-48b2-ac58-b7dadc3e5871', '').
narrative_ontology:cs_kernel_id(catastrophe_necessity_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_necessity_reading, catastrophe_necessity_doctrine).
narrative_ontology:constraint_victim(catastrophe_necessity_reading, operational_safety_margins).
narrative_ontology:constraint_victim(catastrophe_necessity_reading, catastrophe_free_organizations).
narrative_ontology:constraint_vindicates(catastrophe_necessity_reading, irreducible_stress_requirement).
narrative_ontology:constraint_vindicates(catastrophe_necessity_reading, simulation_insufficiency_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The abstract collective good of maintained organizational competence in catastrophe-free periods. If the catastrophe necessity claim is true, this good necessarily erodes during safe periods — competence decays without actual catastrophic stress, and no simulation investment can prevent it. Bears the full cost of the claimed natural law with no exit option and no capacity to organize.
narrative_ontology:constraint_stakeholder(catastrophe_necessity_reading, operational_safety_margins, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_non_agent(catastrophe_necessity_reading, operational_safety_margins).

% Organizations that have successfully avoided catastrophes for extended periods (nuclear plants with decades of safe operation, airlines with strong safety records, chemical facilities with no major incidents). Face claimed inevitable competence decay if the necessity thesis is true. Can invest in simulation training but the reading claims this investment has a natural ceiling — the irreducible stress of actual catastrophe cannot be replicated. Constrained by resource limits and by the claimed psychological limits of simulation fidelity.
narrative_ontology:constraint_stakeholder(catastrophe_necessity_reading, catastrophe_free_organizations, payer,
    moderate, biographical, constrained, national).

% The institutional body of safety engineering practice, including professional societies, academic departments, regulatory bodies, and training organizations. Sets the agenda for competence maintenance methodologies and maintains the catastrophe necessity doctrine as a core premise. Has arbitrage options (can choose among simulation technologies, training regimes, organizational structures) but experiences the constraint as a natural law of human learning. The discipline's foundational claim is that certain forms of knowledge require actual catastrophic stress and cannot be acquired through simulation alone.
narrative_ontology:constraint_stakeholder(catastrophe_necessity_reading, safety_engineering_discipline, agenda_setter,
    institutional, generational, arbitrage, global).

% The doctrine itself — the claim that only actual catastrophes provide irreducible stress necessary for competence maintenance. Not a human actor but listed as beneficiary to trigger FSM evaluation. Vindicated by organizational failures in catastrophe-free periods and sustained by institutional investment in training methodologies that assume simulation insufficiency. The doctrine collects no rents in the traditional sense but shapes resource allocation, regulatory requirements, and research priorities.
narrative_ontology:constraint_stakeholder(catastrophe_necessity_reading, catastrophe_necessity_doctrine, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_non_agent(catastrophe_necessity_reading, catastrophe_necessity_doctrine).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains organizational competence in high-reliability domains by ensuring exposure to the stress and uncertainty that the reading claims are necessary for genuine learning. Coordinates attention, resource allocation, and training priorities around the premise that simulation is fundamentally insufficient.
% TRANSFER_FUNCTION: Transfers risk from operational safety margins (which erode during catastrophe-free periods) to organizations (which must either accept competence decay or accept periodic catastrophic stress). Transfers legitimacy and resources to training methodologies and regulatory frameworks that assume catastrophe necessity.
% ABSENT_VOICES: Simulation technology developers, organizations with strong safety records that dispute the necessity claim, researchers investigating high-fidelity simulation alternatives. These voices are not excluded from the conversation but are structurally disadvantaged — the burden of proof is on them to demonstrate that simulation CAN provide sufficient stress, while the necessity doctrine is treated as the default assumption.
% DISAPPEARANCE_RATIONALE: If the catastrophe necessity doctrine disappeared, organizational training methodologies, regulatory requirements, and resource allocation would rearrange substantially. Organizations would invest more heavily in simulation alternatives. Regulatory bodies would revise requirements for periodic real-world stress testing. Insurance underwriters would adjust risk models. The rearrangement would be significant because the doctrine currently shapes institutional practice across high-reliability domains.
% FOUNDING_PROBLEM: The founding problem is organizational competence decay in catastrophe-free periods — the observation that organizations with long periods of safe operation sometimes fail catastrophically when rare events occur, suggesting that competence eroded during the safe period. The catastrophe necessity reading frames this as a natural law: competence decays because simulation cannot provide the irreducible stress of actual catastrophe.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (competence decay in catastrophe-free periods) is corroborated by multiple sources: (1) Post-incident analyses of catastrophic failures in organizations with long safe-operation histories (Challenger, Fukushima, BP Deepwater Horizon). (2) Organizational learning research documenting 'competency traps' and 'success-induced forgetting.' (3) High-reliability organization studies showing performance variation with incident frequency. However, the STATUS of the problem is contested: simulation advocates argue that observed decay reflects insufficient simulation investment, not inherent simulation limits. The necessity reading's claim that the problem is a natural law (rather than a coordination failure or technological limit) is disputed by sibling readings and lacks definitive empirical resolution.
narrative_ontology:disappearance_verdict(catastrophe_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_necessity_reading, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPERATIONAL SAFETY MARGINS (MOUNTAIN) — The abstract collective good of maintained competence in catastrophe-free periods. Trapped by the claimed natural law: if simulation cannot substitute for real catastrophe, then competence necessarily decays during safe periods. No exit from the competence erosion cycle. Experiences the constraint as immutable physical/psychological limit.
constraint_indexing:constraint_classification(catastrophe_necessity_reading, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CATASTROPHE-FREE ORGANIZATIONS (MOUNTAIN) — Organizations that have successfully avoided catastrophes for extended periods. Constrained by resource limits on simulation fidelity but also by the claimed natural law that simulation cannot provide the irreducible stress of real events. If the mountain claim is true, these organizations face inevitable competence decay regardless of simulation investment.
constraint_indexing:constraint_classification(catastrophe_necessity_reading, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SAFETY ENGINEERING DISCIPLINE (MOUNTAIN) — The institutional body of safety engineering practice. Has arbitrage options (can choose simulation methodologies, training regimes, organizational structures) but experiences the constraint as a natural law: human psychological and organizational learning mechanisms have inherent limits that no simulation can overcome. The discipline's core premise is that certain forms of knowledge require actual catastrophic stress.
constraint_indexing:constraint_classification(catastrophe_necessity_reading, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — From the civilizational/universal perspective, this reading claims that catastrophe necessity is a natural law of human learning and organizational memory: the psychological and physiological stress responses triggered by actual catastrophic events cannot be replicated in simulation, and these responses are necessary for maintaining genuine competence. This is the claimed mountain — but the presence of beneficiaries (the catastrophe necessity doctrine itself) triggers FSM evaluation.
constraint_indexing:constraint_classification(catastrophe_necessity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_necessity_reading_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(catastrophe_necessity_reading, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophe_necessity_reading, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(catastrophe_necessity_reading, ExtMetricName, E),
    domain_priors:suppression_score(catastrophe_necessity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(catastrophe_necessity_reading),
    narrative_ontology:constraint_metric(catastrophe_necessity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(catastrophe_necessity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(catastrophe_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Low but non-zero. The constraint extracts primarily from operational safety margins — organizations that successfully avoid catastrophes pay a competence maintenance cost if the necessity claim is true. The extraction is modest because the constraint does not concentrate benefits on identifiable human actors (the doctrine itself is the primary beneficiary, which is why it appears in beneficiaries but will be marked agent=false in stakeholder layer). The slight increase over the interval (0.10 → 0.15) reflects growing institutional investment in the necessity doctrine. Suppression (0.20): Low. Organizations have substantial freedom to choose simulation methodologies and training regimes. The suppression comes from the claimed natural law itself — if true, no alternative pathway exists. But the claim does not actively suppress investigation of alternatives. Theater ratio (0.10): Very low. The catastrophe necessity reading is not performative — it makes a genuine empirical claim about human learning mechanisms. The modest theater reflects some ritualistic invocation of the doctrine without empirical grounding, but most applications are functionally motivated. Accessibility collapse (0.85): High. If the psychological irreducibility claim is true, alternatives collapse nearly completely — no simulation methodology can substitute for actual catastrophic stress. Resistance (0.15): Low. The doctrine meets modest resistance from simulation advocates and organizations with strong safety records, but the resistance is not widespread or organized.
 *
 * PERSPECTIVAL GAP:
 *   This constraint is claimed as mountain from all perspectives because the catastrophe necessity reading treats the constraint as a natural law of human psychology and organizational learning. The powerless victim (operational safety margins) sees an immutable limit. The moderate victim (catastrophe-free organizations) sees an immutable limit. The institutional actor (safety engineering discipline) sees an immutable limit. The analytical observer sees an immutable limit. There is no perspectival gap in the traditional sense — all agents experience the constraint as mountain IF the reading's core premise is true. However, the presence of a beneficiary (the catastrophe necessity doctrine) triggers the false summit detector. The FSM evaluation asks: is this genuinely a natural law, or is it a constructed constraint that naturalizes a contingent training methodology and benefits identifiable institutional actors (training contractors, regulatory bodies, insurance underwriters who mandate periodic real-world stress testing)? The omega variables document this irreducible uncertainty. The sibling readings (simulation_as_proxy_catastrophe_reading, hybrid_degradation_reading, simulation_fidelity_threshold) provide alternative framings that dispute the mountain classification.
 *
 * DIRECTIONALITY LOGIC:
 *   The catastrophe necessity reading has an unusual directionality structure because the primary beneficiary is a doctrine rather than a human actor. Operational safety margins (powerless/trapped) are the primary victim — they bear maximum extraction if the mountain claim is true, because competence necessarily decays in catastrophe-free periods with no exit option. Catastrophe-free organizations (moderate/constrained) experience moderate extraction — they face competence decay but have some agency through simulation investment, even if the reading claims that investment has a natural ceiling. The safety engineering discipline (institutional/arbitrage) is listed as experiencing the constraint as mountain despite being a beneficiary of the doctrine — the discipline genuinely believes the claim and experiences it as natural law, not as constructed extraction. The catastrophe necessity doctrine itself is listed in beneficiaries to trigger FSM evaluation: a mountain with declared beneficiaries requires omega variables documenting the natural-law vs. constructed ambiguity. The doctrine collects no rents in the traditional sense, but it is vindicated by organizational failures and sustained by institutional investment in training methodologies that assume simulation insufficiency.
 *
 * MANDATROPHY ANALYSIS:
 *   The catastrophe necessity reading resolves mandatrophy by making an empirical claim about natural limits: IF human stress responses and organizational memory consolidation genuinely require actual catastrophic events and cannot be replicated in simulation, THEN the constraint is a mountain — an immutable feature of human psychology. The mandate (maintain competence) and the trophy (avoid catastrophes) are in genuine tension, and no coordination mechanism can resolve it. However, the constraint is authored as a FALSE SUMMIT CANDIDATE by declaring the catastrophe necessity doctrine as a beneficiary. This triggers the engine's FSM evaluation: does the mountain classification naturalize what is actually a constructed constraint? The omega variables document the irreducible uncertainties: (1) Are stress responses genuinely irreproducible or merely technologically limited? (2) Does competence decay rapidly in catastrophe-free periods regardless of simulation investment? (3) Do identifiable institutional actors benefit from maintaining the necessity doctrine? If the answers are 'technologically limited,' 'simulation-sensitive,' and 'yes,' then the mountain is false — the constraint is actually a tangled rope (genuine learning coordination with embedded extraction by training contractors and regulatory bodies). The sibling readings in the catastrophe_proxy_sufficiency kernel represent alternative empirical hypotheses about the same phenomenon.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Is catastrophe necessity a natural law of human learning, or a constructed constraint that benefits identifiable doctrines and practices?',
    'This constraint is the catastrophe_necessity_reading of the catastrophe_proxy_sufficiency kernel. Sibling readings: simulation_as_proxy_catastrophe_reading (simulation CAN provide sufficient stress), hybrid_degradation_reading (simulation provides partial but degrading proxy), simulation_fidelity_threshold (sufficiency depends on crossing a fidelity threshold). Resolution requires empirical comparison of competence maintenance across organizations with different catastrophe/simulation histories.',
    'If natural law: simulation investment has inherent ceiling; some catastrophe exposure is unavoidable for competence maintenance. If constructed: the doctrine naturalizes a contingent training methodology and may suppress investigation of high-fidelity simulation alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Whether catastrophe necessity is natural law or constructed doctrine').

omega_variable(
    stress_response_irreducibility,
    'Are the psychological and physiological stress responses triggered by actual catastrophes genuinely irreproducible in simulation, or is this a current technological/methodological limit?',
    'Neuroscience and psychophysiology research on stress response fidelity in high-realism simulation (VR, full-scale exercises with consequence). Comparison of cortisol levels, decision-making under pressure, memory consolidation between actual incidents and high-fidelity simulation.',
    'If genuinely irreproducible: mountain classification confirmed. If technologically limited: the constraint is a scaffold (temporary limit being overcome by advancing simulation technology).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stress_response_irreducibility, empirical, 'Whether catastrophic stress responses are irreproducible or technologically limited').

omega_variable(
    competence_decay_rate,
    'What is the actual rate of competence decay in catastrophe-free periods, and does it vary systematically with simulation investment?',
    'Longitudinal studies of high-reliability organizations tracking competence metrics (incident response time, error rates, near-miss detection) against catastrophe-free duration and simulation training intensity. Natural experiments from organizations with varying catastrophe frequencies.',
    'If decay is rapid and simulation-invariant: supports mountain claim. If decay is slow or simulation-sensitive: undermines necessity thesis and suggests the constraint is constructed rather than natural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_decay_rate, empirical, 'Rate of competence decay and simulation sensitivity').

omega_variable(
    beneficiary_structure_ambiguity,
    'Does the catastrophe necessity doctrine benefit identifiable institutional actors (training contractors, insurance underwriters, regulatory bodies that mandate periodic real-world testing)?',
    'Follow the money: who collects rents from the doctrine that simulation is insufficient? Identify institutional actors whose business models or regulatory authority depend on the necessity claim. Cross-reference with funding sources for research supporting the necessity thesis.',
    'If clear beneficiaries exist with structural interest in maintaining the doctrine: FSM reclassification to tangled_rope (genuine learning constraint with embedded extraction). If no beneficiaries beyond the doctrine itself: mountain classification stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_structure_ambiguity, empirical, 'Whether identifiable actors benefit from catastrophe necessity doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_necessity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cat_nec_tr_t0, catastrophe_necessity_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(cat_nec_tr_t10, catastrophe_necessity_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(cat_nec_tr_t20, catastrophe_necessity_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(cat_nec_be_t0, catastrophe_necessity_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(cat_nec_be_t10, catastrophe_necessity_reading, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(cat_nec_be_t20, catastrophe_necessity_reading, base_extractiveness, 20, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_necessity_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_necessity_reading, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_necessity_reading, hybrid_degradation_reading).
narrative_ontology:affects_constraint(catastrophe_necessity_reading, simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% The catastrophe_proxy_sufficiency kernel decomposes into four constraint stories because the ε values differ substantially across readings. The catastrophe_necessity_reading has low extraction (0.15) because it claims a natural law with no identifiable human beneficiaries. The simulation_as_proxy_reading will have higher extraction if it reveals institutional actors suppressing high-fidelity simulation alternatives. The hybrid_degradation_reading will have moderate extraction reflecting the coordination costs of periodic stress exposure. The simulation_fidelity_threshold reading will have extraction that varies with technological state. These are not the same constraint viewed from different angles — they are structurally distinct claims with different beneficiary structures, different empirical status, and different failure modes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
