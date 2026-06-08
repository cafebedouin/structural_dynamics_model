% ============================================================================
% CONSTRAINT STORY: simulation_as_proxy_catastrophe_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simulation_as_proxy_catastrophe_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: simulation_as_proxy_catastrophe_reading
 *   human_readable: Simulation as Proxy Catastrophe: Organizational Learning via Controlled Disaster Practice
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint models the use of simulation exercises as a proxy for
 *   catastrophic disaster training in high-reliability organizations (nuclear
 *   power plants, air traffic control systems, hospitals, emergency response
 *   teams). The reading instantiates the commitment that 'simulation training
 *   is sufficient to maintain operational competence indefinitely' — the core
 *   organizational and regulatory doctrine that justifies replacing
 *   rare-but-essential real-catastrophe learning with controlled, repeatable,
 *   documentable simulation practice. The constraint exhibits characteristics
 *   of tangled rope: a genuine coordination function (simulation does develop
 *   competence and demonstrates regulatory compliance) coupled with
 *   asymmetric extraction (regulatory bodies benefit from liability
 *   protection while organizations bear the operational risk of potential gap
 *   between simulation training and real catastrophic conditions). The
 *   theater_ratio is moderate-high (0.58), reflecting that simulation
 *   exercises are substantially performative — they demonstrate that training
 *   occurred and competence was tested, but the epistemic transfer mechanism
 *   (how simulation learning maps to catastrophe response) remains
 *   unvalidated and often unmeasured. The constraint operates at the
 *   intersection of organizational learning, risk management, and liability
 *   allocation, making it a diagnostic exemplar for how institutional
 *   arrangements naturalize their own contingent assumptions.
 *
 * KEY AGENTS:
 *   - Regulatory Bodies: Primary beneficiary (institutional/arbitrage) — gain liability protection, documented compliance, and measurable training verification without requiring actual disasters
 *   - Insurance Frameworks: Institutional beneficiary (institutional/arbitrage) — reduce liability exposure and set predictable premium structures based on simulation documentation
 *   - Operating Organizations: Secondary beneficiary/victim (moderate/constrained) — benefit from regulatory compliance and reduced scrutiny but bear operational risk of training-reality gap
 *   - Operational Staff: Trapped learner (powerless/trapped) — receive simulation training mandated as sufficient, cannot access the experiential learning actual catastrophe provides
 *   - Learning Gap (Abstract): Victim (powerless/trapped) — the organizational knowledge gap between simulation competence and catastrophe competence remains unlearned and unaddressed
 *   - HRO Reform Advocates: Organized skeptics (organized/mobile) — argue simulation is transitional (scaffold) pending development of higher-fidelity training mechanisms
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choice as inherent limit to catastrophic reality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simulation_as_proxy_catastrophe_reading, 0.28).
domain_priors:suppression_score(simulation_as_proxy_catastrophe_reading, 0.35).
domain_priors:theater_ratio(simulation_as_proxy_catastrophe_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simulation_as_proxy_catastrophe_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(simulation_as_proxy_catastrophe_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(simulation_as_proxy_catastrophe_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simulation_as_proxy_catastrophe_reading, tangled_rope).
narrative_ontology:human_readable(simulation_as_proxy_catastrophe_reading, "Simulation as Proxy Catastrophe: Organizational Learning via Controlled Disaster Practice").
narrative_ontology:topic_domain(simulation_as_proxy_catastrophe_reading, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(simulation_as_proxy_catastrophe_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simulation_as_proxy_catastrophe_reading, '938fdcc0-98f4-4525-bcf2-a2791deadc61').
narrative_ontology:cs_kernel_codification('938fdcc0-98f4-4525-bcf2-a2791deadc61', formalized).
narrative_ontology:cs_authority_grounding('938fdcc0-98f4-4525-bcf2-a2791deadc61', extraction).
narrative_ontology:cs_interpretation_layer_present('938fdcc0-98f4-4525-bcf2-a2791deadc61').
narrative_ontology:cs_reading_relation('938fdcc0-98f4-4525-bcf2-a2791deadc61', simulation_as_proxy_catastrophe_reading__catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('938fdcc0-98f4-4525-bcf2-a2791deadc61', simulation_as_proxy_catastrophe_reading__hybrid_degradation_reading, influences).
narrative_ontology:cs_reading_relation('938fdcc0-98f4-4525-bcf2-a2791deadc61', simulation_as_proxy_catastrophe_reading__simulation_fidelity_threshold, influences).
narrative_ontology:cs_axiom('938fdcc0-98f4-4525-bcf2-a2791deadc61', foundational, simulation_fidelity_sufficient_indefinite_competence).
narrative_ontology:cs_axiom_status(simulation_fidelity_sufficient_indefinite_competence, holdable).
narrative_ontology:cs_axiom_grounding('938fdcc0-98f4-4525-bcf2-a2791deadc61', simulation_fidelity_sufficient_indefinite_competence, empirically_contingent).
narrative_ontology:cs_axiom('938fdcc0-98f4-4525-bcf2-a2791deadc61', secondary, catastrophe_stress_simulation_achievable).
narrative_ontology:cs_axiom_status(catastrophe_stress_simulation_achievable, overridden).
narrative_ontology:cs_axiom_grounding('938fdcc0-98f4-4525-bcf2-a2791deadc61', catastrophe_stress_simulation_achievable, empirically_contingent).
narrative_ontology:cs_reference_frame('938fdcc0-98f4-4525-bcf2-a2791deadc61', simulation_maintains_indefinite_competence).
narrative_ontology:cs_drift_state('938fdcc0-98f4-4525-bcf2-a2791deadc61', post_vr_emergence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('938fdcc0-98f4-4525-bcf2-a2791deadc61', '').
narrative_ontology:cs_kernel_id(simulation_as_proxy_catastrophe_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simulation_as_proxy_catastrophe_reading, regulatory_bodies).
narrative_ontology:constraint_beneficiary(simulation_as_proxy_catastrophe_reading, liability_shield_institutions).
narrative_ontology:constraint_victim(simulation_as_proxy_catastrophe_reading, operational_reality_learning_gap).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(simulation_as_proxy_catastrophe_reading, insurance_frameworks).
narrative_ontology:constraint_beneficiary(simulation_as_proxy_catastrophe_reading, operating_organizations).
narrative_ontology:constraint_victim(simulation_as_proxy_catastrophe_reading, operating_organizations).
narrative_ontology:constraint_victim(simulation_as_proxy_catastrophe_reading, operational_staff).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce simulation training mandates as the standard for demonstrating organizational competence. Benefit from simulation-sufficiency doctrine through liability protection and audit-ready compliance checkpoints. Can arbitrage between simulation mandate and alternative approaches but choose to maintain simulation standard because it provides institutional efficiency and legal defensibility.
narrative_ontology:constraint_stakeholder(simulation_as_proxy_catastrophe_reading, regulatory_bodies, agenda_setter,
    institutional, immediate, arbitrage, global).

% Reduce liability exposure and set predictable premium structures based on simulation training documentation. Collect ongoing benefit from the constraint through reduced claim exposure and the ability to demand proof of simulation compliance as condition of coverage. No cost to maintaining the framework — benefit is continuous and requires no active enforcement.
narrative_ontology:constraint_stakeholder(simulation_as_proxy_catastrophe_reading, insurance_frameworks, beneficiary,
    institutional, immediate, arbitrage, global).

% Must conduct and document simulation exercises to meet regulatory mandate and insurance requirements. Benefit from reduced regulatory scrutiny and liability risk reduction. Pay costs of simulation exercise design, execution, and the opportunity cost of staff time diverted from operational duties. Structurally constrained — cannot abandon simulation mandate without losing regulatory license, but can supplement with hybrid fidelity approaches at additional cost.
narrative_ontology:constraint_stakeholder(simulation_as_proxy_catastrophe_reading, operating_organizations, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(simulation_as_proxy_catastrophe_reading, operating_organizations, beneficiary).

% Receive simulation training mandated as competence-building and competence-demonstration mechanism. Learn from simulation but lack access to the stress-learning and experiential depth that rare actual catastrophic events provide. Constrained by regulatory requirement and organizational policy — cannot request actual-catastrophe-equivalent exposure without violating safety norms. Bear operational risk if simulation fidelity is insufficient for real-catastrophe response.
narrative_ontology:constraint_stakeholder(simulation_as_proxy_catastrophe_reading, operational_staff, payer,
    moderate, biographical, constrained, local).

% The organizational learning gap between simulation competence and catastrophic-reality competence. Cannot organize to advocate for itself. Trapped in the constraint because regulatory frameworks suppress alternative learning mechanisms (hybrid fidelity exposure, controlled low-consequence events). The gap represents unlearned failure modes, psychological stress adaptation, and cascading interdependency patterns that simulation cannot fully replicate.
narrative_ontology:constraint_stakeholder(simulation_as_proxy_catastrophe_reading, learning_gap, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(simulation_as_proxy_catastrophe_reading, learning_gap).

% Safety engineers, organizational learning researchers, and pilot unions advocating that simulation is transitional — higher-fidelity training mechanisms (immersive VR, hybrid simulations with real-world feedback, distributed micro-catastrophes under controlled conditions) can achieve competence maintenance with better epistemic transfer. Currently excluded from regulatory rulemaking but gaining influence through peer-reviewed publications, professional conferences, and selective organizational adoption of hybrid approaches despite regulatory acceptability of simulation alone.
narrative_ontology:constraint_stakeholder(simulation_as_proxy_catastrophe_reading, hro_reform_advocates, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provide documented, repeatable, scalable training mechanism that maintains organizational competence in high-consequence catastrophic response without requiring actual catastrophe exposure. Create audit trail demonstrating due diligence in competence maintenance.
% TRANSFER_FUNCTION: Regulatory bodies and insurance frameworks extract liability reduction and efficiency gains (they can verify competence via documentation without actual disasters). Operating organizations pay costs of simulation exercise execution and bear operational risk of potential fidelity gaps. Operational staff pay time and attention costs; learning gap absorbs unlearned failure modes.
% ABSENT_VOICES: Operational learning researchers and pilot unions who argue simulation fidelity is insufficient; organizations that have experienced actual catastrophes and can testify that simulation training had unanticipated failure modes; near-miss analysts who document how pre-catastrophic signals were missed in simulation exercises; personnel from organizations with hybrid training approaches (who could demonstrate alternative models) are largely absent from regulatory standards-setting.
% DISAPPEARANCE_RATIONALE: If the simulation-sufficiency doctrine disappeared, regulatory frameworks would collapse and require rebuilding around alternative fidelity standards. Insurance premium structures would need recalibration. Organizations would face pressure to develop hybrid training mechanisms combining simulation with higher-fidelity exposure. Operational competence patterns would shift as organizations experiment with alternative learning mechanisms. The regulatory coordination function would persist (need for verification) but the specific mechanism would rearrange completely.
% FOUNDING_PROBLEM: In the 1960s-70s, high-reliability organizations (aviation, nuclear power) faced the problem that catastrophic disasters were too rare and too costly to use as training events, but operational competence in catastrophic response required exposure to catastrophic-scale stress, decision velocity, and resource constraints. Simulation was developed to solve this coordination problem: repeatable, documentable, scalable training that replicated catastrophic stress without actual catastrophe.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by: (1) Regulatory documents from FAA (1970s) and NRC (1970s) that explicitly identify training-reality fidelity gap as the founding problem; (2) High-reliability organization literature (Weick, Roberts) documenting the original mandate; (3) Organizational learning research showing competence maintenance in the absence of actual disasters. The problem remains live — organizations still cannot access actual-catastrophe training. However, the solution (simulation-sufficiency) is now contested by operational practitioners who report fidelity gaps, and by emerging alternative mechanisms (VR, hybrid approaches) that offer higher fidelity. The founding problem is live; the solution is increasingly questioned.
narrative_ontology:disappearance_verdict(simulation_as_proxy_catastrophe_reading, world_rearranges).
narrative_ontology:founding_problem_status(simulation_as_proxy_catastrophe_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPERATIONAL LEARNING GAP (SNARE) — The gap between simulation training and actual catastrophic conditions cannot exit the constraint. Unforeseen failure modes, cascading interdependencies, and psychological stress under real conditions remain unlearned. No voice at the table advocating for real disaster training. Trapped by institutional policy and liability frameworks that mandate simulation as sufficient.
constraint_indexing:constraint_classification(simulation_as_proxy_catastrophe_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OPERATING ORGANIZATION (TANGLED ROPE) — Benefits from regulatory compliance and liability protection through documented simulation practice, yet constrained by the knowledge that simulation training may be insufficient for rare, high-consequence events. Bears cost of potential catastrophic failure while benefiting from reduced regulatory scrutiny and insurance discounts. Mixed coordination (competence maintenance) and extraction (liability transfer).
constraint_indexing:constraint_classification(simulation_as_proxy_catastrophe_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATORY BODIES (ROPE) — Experience pure coordination through simulation mandates: documented training demonstrates due diligence, reduces legal liability, and creates measurable compliance checkpoints. No extraction — the constraint solves a genuine regulatory need (verification of competence without actual disasters). Net beneficiary through liability protection and audit-ready documentation.
constraint_indexing:constraint_classification(simulation_as_proxy_catastrophe_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HRO REFORM MOVEMENT (SCAFFOLD) — Organized advocates (safety engineers, organizational learning researchers, pilot unions) see simulation as transitional training whose sunset is triggered by the development of better fidelity mechanisms: immersive VR, hybrid simulations with real-world feedback loops, distributed micro-catastrophes under controlled conditions. The constraint has an inherent sunset as technology enables higher-fidelity training without actual catastrophe cost.
constraint_indexing:constraint_classification(simulation_as_proxy_catastrophe_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: TABLETOP EXERCISE INDUSTRY (PITON) — The industry of simulation design and execution has institutional inertia despite low measurable validation of transfer to real-world competence. Theater ratio high: the exercises are elaborate, well-designed, and performative — they demonstrate training occurred, but the mechanism by which simulation fidelity translates to catastrophe competence remains unvalidated. Maintained through regulatory mandate and organizational habit, not through demonstrated effectiveness.
constraint_indexing:constraint_classification(simulation_as_proxy_catastrophe_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational perspective, some gap between training and catastrophic reality is inherent to high-consequence systems: the psychological stress, novelty, and time-compression of actual disasters cannot be fully replicated without the real catastrophe. This perspective naturalizes simulation as 'good enough' due to the impossibility of perfect fidelity. However, structural data reveals beneficiary presence (regulatory bodies benefit from liability reduction) — engine will classify this as false summit, showing the 'inherent gap' naturalizes a contingent institutional choice.
constraint_indexing:constraint_classification(simulation_as_proxy_catastrophe_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simulation_as_proxy_catastrophe_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(simulation_as_proxy_catastrophe_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(simulation_as_proxy_catastrophe_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(simulation_as_proxy_catastrophe_reading, TR),
    TR >= 0.70.

:- end_tests(simulation_as_proxy_catastrophe_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. This reading claims minimal extraction because simulation fundamentally solves a genuine coordination problem — organizations need documented training, regulators need verification, insurance requires due diligence. The constraint is NOT primarily extractive in structure. However, extractiveness is non-zero (not rope) because beneficiaries (regulatory/insurance institutions) collect liability reduction and efficiency gains that flow from NOT requiring actual catastrophe exposure, while victims (operational learning gap) bear the unquantified risk that simulation fidelity is insufficient. The moderate extractiveness reflects the asymmetry: beneficiaries benefit from reduced liability exposure; victims absorb potential competence gaps. Suppression (0.35): Moderate. The constraint carries institutional suppression — regulatory mandates, insurance requirements, and organizational policy frameworks suppress alternative learning mechanisms (hybrid fidelity approaches, controlled low-consequence exposure). However, suppression is not total — organized HRO reformers advocate for higher-fidelity methods, and some organizations supplement simulation with hybrid approaches despite regulatory acceptability of simulation alone. Theater ratio (0.58): Moderate-high. Simulation exercises are designed to demonstrate competence, not exclusively to build it. The elaborate scenario design, documentation, and assessment checklist serve regulatory visibility more than organizational learning depth. Participants often recognize the performative element (scenarios are curated, stress levels controlled, consequences removed). Trajectory shows rising theater_ratio over the interval, indicating increasing performative content as regulatory auditing intensifies and exercise design focuses on documentation rather than learning.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a substantial perspectival gap between beneficiaries and victims. Regulatory bodies see pure coordination (rope) — simulation solves the verification problem cleanly. Operating organizations see mixed coordination and extraction (tangled rope) — they benefit from compliance but pay operational risk. The operational learning gap (abstract victim) sees pure extraction (snare) — the gap cannot exit and is forced to absorb the risk that simulation is insufficient. HRO reformers see a temporary problem with known sunset (scaffold) — higher-fidelity training methods are emerging and will obsolete the simulation-sufficiency doctrine. The traditional safety establishment sees a degraded ritual (piton) — tabletop exercises persist through regulatory mandate despite mounting evidence that fidelity limits their learning transfer. The analytical observer risks seeing natural law (mountain) — 'some gap is inherent to reality' — but structural data contradicts this: if better fidelity mechanisms exist or could exist, the gap is not natural but institutional. The perspectival gap reveals that the constraint's persistence is maintained by beneficiary institutional power (regulatory mandate) rather than by genuine irreducibility.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality and effective extraction is derived from structural position: power level, exit options, and role in the beneficiary/victim flow. Regulatory bodies with arbitrage options (can mandate simulation or abandon the framework entirely) experience low directionality (d~0.1) toward the constraint — they are beneficiaries whose effective extraction is negative (the constraint subsidizes their liability reduction). Operating organizations with constrained exit (cannot abandon simulation mandate but can supplement with hybrid approaches) experience moderate directionality (~0.5) — they both benefit (compliance) and pay (operational risk). The operational learning gap (powerless, trapped) experiences high directionality (d~0.9) — it can neither exit the constraint nor organize to defend itself. The scaffold perspective (organized, mobile) experiences low-moderate directionality (~0.3) because advocates have exit capacity (they can publish alternative approaches, consult with organizations that adopt hybrids) and some organizing capacity. The mountain perspective risks false summit detection because it declares beneficiaries (regulatory bodies benefit) despite claiming natural law — the engine will flag the contradiction and reclassify unless the beneficiary presence is incidental rather than structural. In this reading, beneficiary presence is structural (regulatory bodies DO benefit from liability reduction via simulation mandate), so the mountain classification is vulnerable to FSM override.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATE DIAGNOSIS: The founding mandate was genuine — early high-reliability organizations (nuclear aviation in the 1960s-70s) needed a scalable training method that could replicate catastrophic conditions repeatedly without actual catastrophe cost. Simulation solved that mandate. However, the mandate has partially atrophied: modern high-fidelity simulation, immersive VR, distributed micro-catastrophe learning, and hybrid approaches have emerged that can achieve higher fidelity while maintaining control. The institutional response has been to MAINTAIN the simulation-sufficiency doctrine (piton behavior) because regulatory frameworks and insurance structures now depend on simulation as the compliance standard. Regulatory bodies collect ongoing benefit (liability protection, audit checklist validation) and have institutional resistance to upgrading standards. The constraint exhibits mandatrophy characteristics: the founding problem (need for documentable, repeatable training) is still live, but the solution (simulation as sufficient) is increasingly questioned by operational practitioners and learning researchers. The theater_ratio rising over the interval (0.42 → 0.58) indicates that the constraint is shifting from functional training mechanism toward performative compliance ritual. Mandatrophy is NOT yet fully resolved — the constraint still has real coordination function — but resolution is in progress via the scaffold pathway (HRO reform movement developing alternative fidelity mechanisms that will obsolete simulation-sufficiency doctrine).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catastrophe_proxy_sufficiency_empirical,
    'Does simulation-trained competence actually maintain operational readiness for real catastrophic conditions, or does catastrophe introduce failure modes simulation cannot capture?',
    'Post-incident analysis: compare actual catastrophe response competence in organizations with varying simulation frequency/fidelity to simulated outcome predictions. Track organizations that experienced rare high-consequence events to assess whether simulation training prevented escalation.',
    'If sufficient (high correlation): simulation_as_proxy_catastrophe_reading is structurally justified (rope/scaffold). If insufficient (low correlation): the constraint becomes pure snare/extraction mechanism and beneficiary presence reveals false summit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_proxy_sufficiency_empirical, empirical, 'Whether simulation fidelity is sufficient for real catastrophe competence').

omega_variable(
    fidelity_threshold_discovery,
    'What simulation fidelity threshold is necessary and sufficient to maintain catastrophe competence? Is threshold 70%, 85%, 95%, or impossible to achieve without real catastrophe exposure?',
    'Comparative study of catastrophe response outcomes across organizations with different simulation fidelity investments. Measure learning transfer via surprise failure modes in real operations that simulations did not include.',
    'If threshold is discoverable and achievable (< 95% required): scaffold perspective dominates — better simulation technology can solve the learning problem. If threshold requires approaching 100% or actual exposure: catastrophe_necessity_reading and hybrid_degradation_reading gain structural support.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fidelity_threshold_discovery, empirical, 'Discovery of necessary simulation fidelity threshold').

omega_variable(
    kernel_reading_contest,
    'Is the foundational claim that ''simulation constitutes catastrophe-equivalent practice'' a stable commitment of high-reliability organizational culture, or a contingent institutional choice made for liability reduction?',
    'Institutional history analysis: examine regulatory evolution, insurance industry incentives, and organizational learning literature. Track when the simulation-sufficiency claim became doctrine vs. when it was empirically justified.',
    'If stable commitment: this reading grounds organizational authority (lineage/expertise authority). If contingent choice: the constraint is extractive (beneficiaries extracted liability reduction while victims absorbed learning gap risk).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether simulation-sufficiency is stable doctrine or contingent institutional choice').

omega_variable(
    natural_law_false_summit,
    'Does the mountain perspective''s claim that ''some gap is inherent to catastrophic reality'' naturalize what is actually a contingent institutional arrangement (the choice to rely on simulation rather than hybrid fidelity approaches)?',
    'Cross-reading comparative analysis: simulate the catastrophe_necessity_reading and hybrid_degradation_reading structural data. If either produces lower extractiveness via higher fidelity investment, the mountain is a false summit.',
    'If mountain is false summit: the constraint is tangled_rope/snare depending on organizational learning depth. If mountain is genuine: all readings converge on natural barrier and beneficiary presence is incidental.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_false_summit, conceptual, 'Whether apparent natural law is false summit disguising institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simulation_as_proxy_catastrophe_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_proxy_tr_t0, simulation_as_proxy_catastrophe_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(simu_proxy_tr_t5, simulation_as_proxy_catastrophe_reading, theater_ratio, 5, 0.52).
narrative_ontology:measurement(simu_proxy_tr_t10, simulation_as_proxy_catastrophe_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(simu_proxy_be_t0, simulation_as_proxy_catastrophe_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(simu_proxy_be_t5, simulation_as_proxy_catastrophe_reading, base_extractiveness, 5, 0.24).
narrative_ontology:measurement(simu_proxy_be_t10, simulation_as_proxy_catastrophe_reading, base_extractiveness, 10, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(simu_proxy_su_t0, simulation_as_proxy_catastrophe_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(simu_proxy_su_t5, simulation_as_proxy_catastrophe_reading, suppression_requirement, 5, 0.32).
narrative_ontology:measurement(simu_proxy_su_t10, simulation_as_proxy_catastrophe_reading, suppression_requirement, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simulation_as_proxy_catastrophe_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(simulation_as_proxy_catastrophe_reading, 0.12).
narrative_ontology:affects_constraint(simulation_as_proxy_catastrophe_reading, catastrophe_necessity_reading).
narrative_ontology:affects_constraint(simulation_as_proxy_catastrophe_reading, hybrid_degradation_reading).
narrative_ontology:affects_constraint(simulation_as_proxy_catastrophe_reading, simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% This reading (simulation-as-proxy-sufficient) is one commitment system reading within the catastrophe_proxy_sufficiency kernel. It is upstream of the empirical constraint simulation_fidelity_threshold and coexistent with catastrophe_necessity_reading and hybrid_degradation_reading. Each reading has distinct beneficiary/victim structures, ε values, and perspectives. The readings decompose the contested kernel by separating the normative commitment (sufficiency claim) from the empirical threshold question and the alternative approaches. Network edges indicate that evidence on fidelity thresholds (simulation_fidelity_threshold story) and experience with hybrid approaches (hybrid_degradation_reading story) will structurally influence whether this reading maintains institutional authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(simulation_as_proxy_catastrophe_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
