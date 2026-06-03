% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__catastrophe_as_necessary_anchor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__catastrophe_as_necessary_anchor, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: competence_exercise_requirement__catastrophe_as_necessary_anchor
 *   human_readable: Competence Decay and Catastrophe-Dependent Activation (Catastrophe-as-Anchor Reading)
 *   domain: safety_engineering/organizational_learning/high_reliability_operations
 *
 * SUMMARY:
 *   The competence-exercise requirement under the
 *   catastrophe-as-necessary-anchor reading posits that only real
 *   catastrophic events (or structurally equivalent near-misses) provide the
 *   irreducible exercise that maintains genuine operational competence in
 *   high-reliability domains. This reading contradicts the
 *   simulation-as-adequate-exercise reading (which claims that high-fidelity
 *   simulation with rigorous debriefing provides sufficient competence
 *   maintenance) and coexists with the hybrid-dependency reading (which
 *   claims simulation is necessary but not sufficient, requiring periodic
 *   real-world anchoring). This constraint story instantiates the
 *   catastrophe-anchor reading: the reading that insists on actual emergency
 *   experience as epistemically irreplaceable. The empirical context is
 *   high-reliability operations (aviation, nuclear, maritime, medical
 *   emergency response) where skill decay during long accident-free periods
 *   represents genuine risk. The constraint exhibits pure extraction when
 *   viewed from the powerless operator's perspective: the system holds
 *   operators responsible for competencies that degrade without catastrophe,
 *   creating a perverse incentive to tolerate near-catastrophes to maintain
 *   proof-of-competence. The constraint exhibits coordination benefits when
 *   viewed from the institutional perspective: catastrophe-proven operators
 *   provide legal and market reassurance that competence is genuine. The
 *   tension between these perspectives reveals the core structural question:
 *   is the requirement for catastrophe-anchored competence a natural
 *   consequence of human skill decay, or an institutional arrangement that
 *   benefits regulators and risk carriers at the expense of front-line
 *   operators who cannot maintain competence without the very events the
 *   system is designed to prevent?
 *
 * KEY AGENTS:
 *   - Front-line operators (pilots, nuclear technicians, emergency responders): Primary victims (powerless/trapped) — bear responsibility for competence they cannot maintain without catastrophe
 *   - System resilience and organizational learning: Primary victim (moderate/constrained) — suppressed learning capacity until catastrophe forces reflection
 *   - Regulatory authority: Primary beneficiary (institutional/arbitrage) — maintains authority and oversight through asymmetric epistemic claim (operators unproven without catastrophe)
 *   - Institutional risk carrier (airline, power plant, hospital): Secondary beneficiary (institutional/arbitrage) — gains liability protection through catastrophe-validated competence claims
 *   - Safety advocacy coalition (pilot unions, safety boards, incident investigators): Mixed (organized/constrained) — benefits from real incident investigation but cannot advocate proactively until incidents occur
 *   - Analytical observer: Sees the constraint structure (analytical/analytical) — recognizes extraction mechanism that naturalizes contingent institutional choices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.68).
domain_priors:suppression_score(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.72).
domain_priors:theater_ratio(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, extractiveness, 0.68).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__catastrophe_as_necessary_anchor, snare).
narrative_ontology:human_readable(competence_exercise_requirement__catastrophe_as_necessary_anchor, "Competence Decay and Catastrophe-Dependent Activation (Catastrophe-as-Anchor Reading)").
narrative_ontology:topic_domain(competence_exercise_requirement__catastrophe_as_necessary_anchor, "safety_engineering/organizational_learning/high_reliability_operations").

domain_priors:requires_active_enforcement(competence_exercise_requirement__catastrophe_as_necessary_anchor).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__catastrophe_as_necessary_anchor, '753d9c38-5e3f-46a2-9c8a-9b353ed2d764').
narrative_ontology:cs_kernel_codification('753d9c38-5e3f-46a2-9c8a-9b353ed2d764', implicit).
narrative_ontology:cs_authority_grounding('753d9c38-5e3f-46a2-9c8a-9b353ed2d764', extraction).
narrative_ontology:cs_reading_relation('753d9c38-5e3f-46a2-9c8a-9b353ed2d764', competence_exercise_requirement__simulation_as_adequate_exercise, forecloses).
narrative_ontology:cs_reading_relation('753d9c38-5e3f-46a2-9c8a-9b353ed2d764', competence_exercise_requirement__hybrid_dependency, coexists_with).
narrative_ontology:cs_axiom('753d9c38-5e3f-46a2-9c8a-9b353ed2d764', foundational, catastrophe_epistemically_irreplaceable).
narrative_ontology:cs_axiom_status(catastrophe_epistemically_irreplaceable, holdable).
narrative_ontology:cs_axiom_grounding('753d9c38-5e3f-46a2-9c8a-9b353ed2d764', catastrophe_epistemically_irreplaceable, empirically_contingent).
narrative_ontology:cs_axiom('753d9c38-5e3f-46a2-9c8a-9b353ed2d764', secondary, simulation_as_preparation_not_validation).
narrative_ontology:cs_axiom_status(simulation_as_preparation_not_validation, holdable).
narrative_ontology:cs_axiom_grounding('753d9c38-5e3f-46a2-9c8a-9b353ed2d764', simulation_as_preparation_not_validation, empirically_contingent).
narrative_ontology:cs_reference_frame('753d9c38-5e3f-46a2-9c8a-9b353ed2d764', catastrophe_as_proof_of_competence).
narrative_ontology:cs_drift_state('753d9c38-5e3f-46a2-9c8a-9b353ed2d764', contemporary_safety_culture_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('753d9c38-5e3f-46a2-9c8a-9b353ed2d764', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, regulatory_authority).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, institutional_risk_carriers).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, front_line_operators).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, system_resilience).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONT-LINE OPERATOR (SNARE) — Trapped in a cycle where simulation cannot substitute for genuine emergency response. Competence atrophies during long periods without actual catastrophic events. When catastrophe finally arrives, the operator faces a choice: execute degraded muscle memory or admit decay. No exit from the requirement for real-world anchoring, no way to prove competence until the crisis that justifies the crisis. Maximum extraction — the system holds them responsible for capabilities that cannot be maintained without catastrophe.
constraint_indexing:constraint_classification(competence_exercise_requirement__catastrophe_as_necessary_anchor, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SYSTEM RESILIENCE / EPISTEMIC COMMONS (SNARE) — The broader system's ability to learn from near-misses and maintain competence through distributed knowledge is suppressed. Organizational learning is deferred until catastrophe forces it; near-misses that could trigger learning are rationalized as 'we handled it fine in simulation.' The constraint prevents proactive resilience building — learning is extracted from the system, held captive until catastrophe makes it unavoidable.
constraint_indexing:constraint_classification(competence_exercise_requirement__catastrophe_as_necessary_anchor, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: REGULATORY AUTHORITY (ROPE) — Sees the constraint as coordination function: the requirement for catastrophe-anchored competence justifies continuous regulatory presence, audit authority, and institutional preservation of regulatory bodies. The authority benefits from the epistemic asymmetry — operators cannot claim competence until proven in actual emergency. This coordination benefit is asymmetric; operators bear the burden. The authority experiences extraction flowing toward it but frames it as legitimate oversight.
constraint_indexing:constraint_classification(competence_exercise_requirement__catastrophe_as_necessary_anchor, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL RISK CARRIER (ROPE) — The organization that holds insurance, liability, and reputation stakes sees the constraint as coordination: catastrophe-proven competence provides legal and market reassurance that operators have been 'tested.' Until catastrophe, the organization cannot claim that operators are genuinely competent — only simulator-trained. Catastrophe provides retroactive validation. The institution benefits from this asymmetry through reduced liability claims after proof-of-competence via actual emergency.
constraint_indexing:constraint_classification(competence_exercise_requirement__catastrophe_as_necessary_anchor, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SAFETY ADVOCACY COALITION (TANGLED ROPE) — Organized agents (pilot unions, safety boards, incident investigators) see the constraint as mixed. There is genuine coordination: catastrophe-driven learning protects the system from decay. But there is asymmetric extraction: the coalition cannot effectively advocate for proactive competence maintenance — they must wait for incidents to gain credibility. They experience both the coordination benefit (real incident investigation strengthens safety) and the extraction (their advocacy is discounted until validated by catastrophe).
constraint_indexing:constraint_classification(competence_exercise_requirement__catastrophe_as_necessary_anchor, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From the civilizational perspective, this constraint exhibits pure extraction. The system has transformed a natural feature of human skill decay (competence atrophies without practice) into an epistemic requirement (only catastrophe proves competence was ever real). The analytical observer sees that the system could maintain competence through deliberate, graduated real-world exercise — line operations, non-jeopardy audits, actual equipment time — but instead defers learning to catastrophe. This deferral is not inherent; it is enforced by institutional structure that reserves crisis response authority for catastrophes only.
constraint_indexing:constraint_classification(competence_exercise_requirement__catastrophe_as_necessary_anchor, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__catastrophe_as_necessary_anchor_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(competence_exercise_requirement__catastrophe_as_necessary_anchor, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(competence_exercise_requirement__catastrophe_as_necessary_anchor, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_requirement__catastrophe_as_necessary_anchor, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_requirement__catastrophe_as_necessary_anchor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint forces operators to remain dependent on catastrophe for competence validation. Simulation training is theatricalized — treated as preparation but not proof. Only real emergency response proves the operator was ever competent. This asymmetry extracts continuously from operators who must maintain readiness without being able to demonstrate it. The extractiveness rises over time (0.42 → 0.68) as long periods without catastrophe accumulate, increasing the gap between operators' formal training and any genuine proof of capability. Suppression (0.72): High. Multiple suppression mechanisms prevent operators from exercising competence without catastrophe. Liability restrictions prevent deliberate high-stakes exercises on live systems. Regulatory structures reserve emergency response authority for certified catastrophes only. Organizational culture treats near-misses as 'almost disasters averted' rather than learning opportunities. Alternatives to catastrophe are institutionally suppressed. Theater ratio (0.55): Moderate. Simulation-based competence maintenance is partially performative. Simulators provide realistic scenarios and muscle memory training, but stakeholders and operators alike understand that simulator success does not constitute proof of actual capability. The theater is moderate rather than high because simulation does genuinely prepare — the performativity lies in treating preparation as validation.
 *
 * PERSPECTIVAL GAP:
 *   The crucial perspectival gap is between the powerless operator (snare: trapped in competence-by-catastrophe cycle) and the institutional beneficiary (rope: benefits from asymmetric epistemic validation). The operator sees no exit — they cannot practice genuine emergency response without catastrophe; cannot prove competence without response; cannot escape the waiting period between training and proof. The institution sees this as legitimate oversight and risk management. The analytical observer recognizes that this gap is not inevitable: organizations could mandate periodic non-jeopardy real-world exercises, graduated emergency scenarios, or line operations with oversight, providing genuine competence anchoring without waiting for catastrophe. The deferral to catastrophe is a choice, enforced by liability and regulatory structures. The hybrid-dependency reading coexists with this reading because it suggests a middle path (simulation + periodic real-world exercise) that this reading denies is adequate.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from beneficiary/victim status and exit options. Front-line operators are victims with trapped exit (no way to prove competence except through catastrophe) → d ≈ 0.95 → high f(d) → high experienced extraction. The regulatory authority is a beneficiary with arbitrage exit (can withdraw oversight if operators prove competence; can maintain oversight by withholding that validation) → d ≈ 0.05 → negative f(d) → negative experienced extraction (subsidy). System resilience is a victim with constrained exit (cannot exit the suppression of proactive learning) → d ≈ 0.80 → high f(d). The safety coalition is organized and has some exit (can advocate loudly, can push for alternatives) but constrained by needing catastrophe to gain credibility → d ≈ 0.55 → moderate-high f(d). The analytical observer (analytical, analytical) uses canonical d ≈ 0.73 → moderate f(d) ≈ 1.15. These directionalities produce the perspectival gap: operators experience extreme snare; institutions experience protective rope; the coalition experiences mixed tangled_rope; the observer sees the structure clearly as extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_threshold,
    'At what threshold of simulation fidelity does muscle memory transfer from simulator to real emergency become adequate without catastrophe-anchoring?',
    'Longitudinal study of operator performance post-incident, stratified by simulation recency and fidelity rating; correlation between simulator hours and real-world performance decay across incident severity levels',
    'If threshold is achievable (simulator can genuinely maintain muscle memory): the catastrophe-anchor reading is partially overridden; hybrid_dependency becomes more plausible. If threshold is unachievable (simulator cannot substitute for real-world stakes): catastrophe-as-necessary-anchor remains empirically sound.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(simulation_fidelity_threshold, empirical, 'Whether simulator fidelity can fully substitute for real-world catastrophe in maintaining muscle memory').

omega_variable(
    near_miss_learning_equivalence,
    'Do non-catastrophic near-misses (incidents that did not result in harm, but could have) provide equivalent competence-anchoring to actual catastrophes?',
    'Case study analysis of operator competence trajectories following near-misses vs actual incidents; comparison of organizational learning signal strength and persistence',
    'If near-misses are equivalent: the reading can be expanded to ''real high-stakes events'' rather than ''catastrophe only'' — opening space for non-catastrophic anchoring. If only catastrophes trigger genuine learning: the pure catastrophe requirement is empirically sound.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(near_miss_learning_equivalence, empirical, 'Whether near-misses provide equivalent competence-anchoring to catastrophes').

omega_variable(
    institutional_suppression_of_noncatastrophic_exercise,
    'Is the deferral of competence-anchoring to catastrophe a structural necessity, or an institutional choice enforced by liability and regulatory structures?',
    'Comparative analysis of organizations that have implemented continuous non-catastrophic real-world exercise (line operations, non-jeopardy audits, graduated emergency scenarios) and tracked competence maintenance; cost-benefit analysis of proactive exercise vs catastrophe-reactive learning',
    'If deferral is chosen (not necessary): the constraint is a snare enforced by institutional extraction, not a natural law. Hybrid_dependency becomes the more accurate reading. If deferral is necessary (physics/cognition constraint): catastrophe-as-anchor is unavoidable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_suppression_of_noncatastrophic_exercise, empirical, 'Whether catastrophe-deferral is structural necessity or institutional choice').

omega_variable(
    kernel_ambiguity_catastrophe_vs_realworld_stakes,
    'Is the kernel ''only real catastrophic events'' or ''only genuine high-stakes real-world events''? Catastrophe and high-stakes near-miss differ structurally — catastrophe cannot be practiced; high-stakes exercise can.',
    'Textual and structural analysis of the kernel readings'' foundational commitments. Examine whether the reading''s axiom commits to catastrophe as intrinsically necessary (cannot be avoided, cannot be simulated) or high-stakes authenticity as necessary (can be scaled but not simulated).',
    'If kernel scope is ''high-stakes authenticity'': this reading forecloses simulation_as_adequate_exercise but coexists_with hybrid_dependency. If kernel scope is ''catastrophe only'': this reading forecloses both siblings and stands alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_ambiguity_catastrophe_vs_realworld_stakes, conceptual, 'Whether the catastrophe requirement is intrinsic to the kernel or contingent on the framing of ''real-world exercise''').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_cata_tr_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 0, 0.48).
narrative_ontology:measurement(comp_cata_tr_t2, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 2, 0.52).
narrative_ontology:measurement(comp_cata_tr_t4, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 4, 0.55).

% Extraction over time
narrative_ontology:measurement(comp_cata_be_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(comp_cata_be_t2, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 2, 0.55).
narrative_ontology:measurement(comp_cata_be_t4, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 4, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(comp_cata_su_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(comp_cata_su_t2, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 2, 0.68).
narrative_ontology:measurement(comp_cata_su_t4, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 4, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__catastrophe_as_necessary_anchor, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.18).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement__simulation_as_adequate_exercise).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement__hybrid_dependency).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, learning_lag_in_high_reliability_systems).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, catastrophe_production_and_prevention_paradox).

% DUAL FORMULATION NOTE:
% The competence_exercise_requirement kernel has three structurally distinct readings: catastrophe-as-anchor (this story, high extraction), hybrid-dependency (moderate extraction, permits proactive exercise), and simulation-as-adequate (low extraction, denies catastrophe necessity). Each reading produces different base_extractiveness values because each specifies different suppression and enforcement mechanisms. This story does not hedge across readings — it commits to the catastrophe-anchor framing. Sibling readings are separate constraint stories with their own ε values and perspectives. Network edges represent structural dependencies: the catastrophe-anchor reading is upstream of the hybrid-dependency reading (hybrid reading emerges as attempted escape from catastrophe-anchor's extraction). All three readings affect downstream constraints about learning lag and the catastrophe-prevention paradox.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
