% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading
 *   human_readable: Simulation as Catastrophe-Equivalent Practice (Proxy Reading)
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   catastrophe_proxy_sufficiency. The reading asserts: simulation exercises
 *   constitute catastrophe-equivalent practice sufficient to maintain
 *   operational competence indefinitely. This is the regulatory doctrine in
 *   force in aviation, nuclear, and maritime sectors—the official framework
 *   for certifying crew competence without requiring real-event exposure. The
 *   kernel is contested because the empirical sufficiency of simulation
 *   remains ambiguous: sibling readings argue that only actual catastrophes
 *   maintain certain forms of tacit knowledge, that competence degrades on
 *   generational timescales despite simulations, or that sufficiency depends
 *   on crossing a technology-dependent fidelity threshold. This reading
 *   claims the affirmative: simulation alone is categorically sufficient. The
 *   engine computes per-seat type; this reading's claim/metric gap is
 *   intentional (low extractiveness authored for a coordination mechanism;
 *   low suppression authored for a non-coercive arrangement) but the reading
 *   itself carries the committer-frame uncertainty: is the doctrine
 *   reflecting reality or a false summit maintained by regulatory and
 *   organizational benefit?
 *
 * KEY AGENTS:
 *   - regulatory_bodies: Institutional agenda-setter (national scope, analytical exit) — certify simulation-sufficiency doctrine; benefit from liability protection.
 *   - high_reliability_organizations: Institutional beneficiary (global scope, constrained exit) — depend on doctrine for training cost management; bear operational risk if insufficient.
 *   - operational_crews: Moderate-power payers (global scope, constrained exit) — undergo mandatory simulation training; risk competence gaps at real events.
 *   - catastrophe_experience_experts: Moderate-power observers (national scope, analytical exit) — assess doctrine against real-world evidence; divided in attestation.
 *   - excluded_catastrophe_survivors: Moderate-power excluded voices (global scope, constrained exit) — possess contradictory experiential knowledge; systematically marginalized in regulatory discourse.
 *   - incident_victims: Powerless excluded voices (global scope, trapped) — affected by competence gaps; no formal standing until post-incident.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.28).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.15).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, "Simulation as Catastrophe-Equivalent Practice (Proxy Reading)").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, "safety_engineering/organizational_learning").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 'd408bfaf-0b14-4260-8e60-8594ba660128').
narrative_ontology:cs_kernel_codification('d408bfaf-0b14-4260-8e60-8594ba660128', formalized).
narrative_ontology:cs_authority_grounding('d408bfaf-0b14-4260-8e60-8594ba660128', extraction).
narrative_ontology:cs_interpretation_layer_present('d408bfaf-0b14-4260-8e60-8594ba660128').
narrative_ontology:cs_reading_relation('d408bfaf-0b14-4260-8e60-8594ba660128', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('d408bfaf-0b14-4260-8e60-8594ba660128', catastrophe_proxy_sufficiency__hybrid_degradation_reading, coexists_with).
narrative_ontology:cs_reading_relation('d408bfaf-0b14-4260-8e60-8594ba660128', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, influences).
narrative_ontology:cs_axiom('d408bfaf-0b14-4260-8e60-8594ba660128', foundational, simulation_categorically_sufficient).
narrative_ontology:cs_axiom_status(simulation_categorically_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('d408bfaf-0b14-4260-8e60-8594ba660128', simulation_categorically_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('d408bfaf-0b14-4260-8e60-8594ba660128', secondary, competence_indefinite_retention_without_catastrophe_exposure).
narrative_ontology:cs_axiom_status(competence_indefinite_retention_without_catastrophe_exposure, holdable).
narrative_ontology:cs_axiom_grounding('d408bfaf-0b14-4260-8e60-8594ba660128', competence_indefinite_retention_without_catastrophe_exposure, empirically_contingent).
narrative_ontology:cs_reference_frame('d408bfaf-0b14-4260-8e60-8594ba660128', simulation_sufficiency_doctrine_enacted).
narrative_ontology:cs_drift_state('d408bfaf-0b14-4260-8e60-8594ba660128', contemporary_2025, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d408bfaf-0b14-4260-8e60-8594ba660128', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, regulatory_bodies).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, high_reliability_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, high_reliability_organizations).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, operational_crews).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulation_fidelity_as_categorical_sufficiency).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, competence_indefinite_retention_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Certify and oversee safety programs in high-reliability sectors (aviation, nuclear, maritime). Sets the standard that simulation-based training constitutes sufficient proof of operational competence for licensing and recertification. Benefits from the doctrine by having a liability shield: if an incident occurs despite certified simulation training, the regulator has discharged its oversight duty. Maintains the framework through certification rules and periodic review of simulation standards.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Operate critical infrastructure (airlines, nuclear plants, hospitals) and depend on the simulation-sufficiency doctrine to keep training costs manageable and to justify personnel rotation without expensive real-event practice. They benefit from the doctrine's simplified competence model: periodic simulation refresher + certification = competent crew indefinitely. They also bear the cost of funding and conducting the simulations, and carry the operational risk if the doctrine's sufficiency is wrong.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, high_reliability_organizations, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, high_reliability_organizations, payer).

% Undergo periodic simulation training as a condition of licensure and employment. They rely on the doctrine for career stability: if simulation alone were deemed insufficient, they would face mandatory real-event exposure or continuous training burden. They bear the operational risk that simulation-only training leaves gaps in their actual competence when a real event occurs—gaps they may not recognize until the event itself.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, operational_crews, payer,
    moderate, biographical, constrained, global).

% Domain experts (incident investigators, systems theorists, veteran operators who have lived through actual events) who can assess whether the doctrine matches reality. They see the full range of stress patterns, tacit knowledge integration, and crew psychology under real stakes. Some attest the doctrine is sound; others challenge it. They have no formal seat in certification but their analysis feeds regulatory review and litigation.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, catastrophe_experience_experts, observer,
    moderate, biographical, analytical, national).

% Operators and crews who have survived actual catastrophes or near-misses know firsthand how simulation deviates from real conditions (uncertainty, time pressure, sensory overload, psychological states that simulation cannot fully replicate). Their testimony contradicts the sufficiency doctrine but is systematically marginalized in regulatory discourse because they represent rare events and their numbers are small. If empowered, they would testify that simulation alone is insufficient for genuine competence.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, excluded_catastrophe_survivors, excluded,
    moderate, biographical, constrained, global).

% Passengers, patients, and communities affected by safety failures. They would object strongly to the sufficiency doctrine if given a voice—arguing that simulation-only training leaves undetected gaps that real catastrophes exploit. They have no formal standing in the doctrine's maintenance and their testimony enters only through litigation after incidents occur, too late to reshape the framework.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, incident_victims, excluded,
    powerless, immediate, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared standard for measuring operational competence across distributed high-reliability organizations: simulation training can be standardized, measured, documented, and certified, replacing ad-hoc competence assessment. Solves the coordination problem of how to ensure crews across thousands of organizations meet a consistent safety bar without centralizing all training.
% TRANSFER_FUNCTION: Transfers regulatory liability risk FROM certification bodies TO operators and organizations: if simulation training is deemed sufficient, the regulator's duty is satisfied even if an incident occurs post-certification. It also transfers the burden of proving competence decay FROM regulatory oversight TO individual organizations monitoring their own crews.
% ABSENT_VOICES: Catastrophe survivors and incident victims are systematically excluded. They possess experiential knowledge of simulation-reality gaps that contradicts the sufficiency doctrine, but their small numbers, post-incident timing, and lack of institutional standing keep them out of the rulemaking conversation. Their objections surface only through litigation and accident investigations, after the doctrine has already shaped organizational practice.
% DISAPPEARANCE_RATIONALE: If the simulation-sufficiency doctrine vanished and regulators reverted to demanding real-event exposure or continuous field-based assessment for competence certification, the global aviation, nuclear, and maritime training industries would reorganize dramatically: crews would rotate through operational theaters more frequently, organizations would invest in high-fidelity disaster scenarios (creating whole new industries), personnel scheduling would shift, and certification pathways would become more expensive and time-consuming. The doctrine's absence would cascade through hiring, promotion, and operational deployment.
% FOUNDING_PROBLEM: Mid-20th-century high-reliability sectors faced a competence sustainment crisis: you cannot subject crews to real catastrophes frequently enough to maintain stress-response skills, but you cannot afford to have incompetent crews during actual operations. How do you keep crews sharp without exposing them to the real thing?
% FOUNDING_PROBLEM_CORROBORATION: Regulators attest the problem is live and simulation is the solution. Catastrophe-response experts and incident investigators outside the regulatory establishment attest the problem is partially solved but the solution is incomplete: simulation maintains procedural competence but leaves tacit knowledge and psychological stress-response capacity partially unaddressed. Some operational crews who have lived through real events explicitly dispute the sufficiency of simulation-only training in post-incident investigations.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading_tests).
:- end_tests(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28 endpoint) because the constraint's primary function is genuine coordination: establishing a standardized, measurable competence criterion across distributed organizations. The doctrine benefits from this: regulators can certify uniformly, organizations can deploy crews confidently, and the system scales. However, extractiveness is not zero because regulatory bodies gain a secondary benefit—liability protection. If an incident occurs, the regulator can point to certification and discharge its oversight duty, shifting the liability burden to operators. This secondary benefit grows slightly over the interval (1970→2025, 0.15→0.28) as litigation becomes more common and liability frameworks mature—regulatory bodies have learned to weaponize the doctrine for liability defense, incrementally raising its extractive character. Theater ratio is low-to-moderate (0.22 endpoint) because the constraint serves a real function (standardized competence measurement) but some proportion of ongoing activity is performative: certification rituals, regulatory documentation, attestation procedures that exist to defend the doctrine itself rather than improve competence. Theater rises modestly over the interval as regulatory burden increases. Suppression is very low (0.15) because the constraint is not coercive—crews and organizations voluntarily participate in the system; alternatives exist in principle (real-event training, continuous assessment), though regulatory pressure constrains them in practice. The constraint coordinates rather than coerces.
 *
 * PERSPECTIVAL GAP:
 *   Regulatory bodies experience the constraint as legitimate coordination + liability shield: a win on both dimensions. Crews and organizations experience it as necessary coordination with embedded operational risk: they cannot refuse but carry the downside if the doctrine fails. Catastrophe survivors and excluded voices experience it as false sufficiency maintained by regulatory capture: they know the gaps exist but lack standing to correct the doctrine. This perspectival divergence is structural, not merely disagreement—it follows from power asymmetry and exit-option constraints. The engine computes divergence from beneficiary/victim declarations and exit data.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory bodies are declared beneficiaries: they collect liability protection and establish their authority through doctrine-setting (d near 0.1, low extractiveness end). High-reliability organizations are declared beneficiaries (cost-managed training) AND secondary payers (operational risk): their d is near 0.5 (mixed sign), reflecting symmetric costs and benefits. Operational crews are declared implicitly as payers (no explicit victim set, but they shoulder competence-gap risk): their d is moderate-to-high (0.6–0.7), reflecting constrained exit and downside exposure. The excluded and observer seats have no formal structural relationship to the constraint's extraction—they are not beneficiaries or victims in the organizational sense, so their d is analytical (1.0 or undefined). This is deliberate: the constraint's extractiveness is measured against the seats that organize around it; the excluded seats' knowledge is a form of power outside the constraint's structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not show mandatrophy symptoms: the founding problem (how to maintain competence without real-event exposure) is live and contested, not dead. The doctrine persists because it solves a genuine coordination problem AND because regulatory bodies benefit from liability protection. The constraint is not a piton: beneficiaries (regulatory bodies, organizations) actively maintain it through rule-setting and resource allocation, not through inertia. The theater ratio (0.22) is moderate, not high—it indicates some performative activity but a genuine coordination function underneath. The constraint is best classified as rope (claimed type), with low but non-zero extractiveness from the secondary liability-protection benefit. Mandatrophy would manifest if the founding problem died but the doctrine persisted anyway through institutional inertia; here the problem is explicitly contested, so the persistence is contestable, not inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_empirical_boundary,
    'What is the actual empirical boundary between simulation fidelity that maintains competence and simulation fidelity that loses critical elements (stress physiology, time pressure under uncertainty, crew psychology under real stakes)?',
    'Longitudinal studies comparing crews trained under current simulation standards with crews receiving intermittent real-event exposure (rare but documented in military and disaster-response sectors). Post-incident investigation analysis correlating competence gaps to pre-incident simulation recency and fidelity.',
    'If the boundary is crossed by current simulation technology, the doctrine is validated and extractiveness is correctly low. If current simulation systematically falls short, the doctrine is a false summit and extractiveness should be higher (regulatory bodies protecting themselves by declaring sufficiency when they know it is contested). The fidelity boundary is the empirical root of the kernel contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_fidelity_empirical_boundary, empirical, 'Whether simulation crosses the fidelity threshold for categorical competence maintenance.').

omega_variable(
    tacit_knowledge_stress_response_capacity,
    'Is the tacit knowledge component of operational competence (pattern recognition under uncertainty, intuitive risk assessment, crew coordination under stress) maintained by simulation alone, or does it degrade over generational timescales without real-event exposure?',
    'Generational cohort studies tracking crews across career spans, comparing competence metrics for cohorts with and without catastrophe experience. Analysis of crew performance in novel-failure scenarios (events outside simulation playbooks) comparing simulation-trained-only versus experience-mixed crews.',
    'If tacit knowledge degrades, the hybrid_degradation_reading is more accurate and the doctrine requires supplementation (e.g., mandatory crew rotation through operational theaters, periodic real-event scenario exposure). If it is maintained, the simulation-only doctrine holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_knowledge_stress_response_capacity, empirical, 'Whether stress-response and tacit knowledge degrade without real-event exposure.').

omega_variable(
    regulatory_capture_liability_asymmetry,
    'Is the doctrine''s persistence driven by genuine belief in simulation sufficiency, or partly by regulatory and organizational benefit from liability protection and cost management?',
    'Analysis of regulatory rulemaking history: do regulatory bodies adjust simulation standards based on post-incident analysis, or do they maintain static standards despite evidence of gaps? Comparison of regulatory positions in sectors with high litigation (aviation, healthcare) versus low-litigation sectors (military, some industrial). Testimony from regulatory insiders about the balance between safety confidence and liability defense in doctrine-setting.',
    'If the doctrine persists despite known gaps for liability and cost reasons, it is a false summit (extractive constraint maintaining regulatory authority and organizational cost management while accepting competence risk). If the doctrine adjusts based on evidence, it is a genuine coordination mechanism that learns.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_liability_asymmetry, empirical, 'Whether the doctrine''s persistence reflects safety confidence or regulatory/organizational benefit.').

omega_variable(
    excluded_voice_integration,
    'Would integrating catastrophe survivors and incident investigators into regulatory rulemaking change the doctrine''s formulation?',
    'Comparative analysis of jurisdictions that have formally included survivor testimony and expert challenge in simulation-standard review versus those that have not. Analysis of post-incident investigations: do survivor reports and expert challenges systematically contradict the sufficiency doctrine?',
    'If excluded voices, when centered, systematically challenge the doctrine''s sufficiency, the doctrine''s persistence despite exclusion is a false summit maintained by marginalizing contradictory evidence. The constraint would shift to snare-flavored (regulatory and organizational benefit protected by excluding certain voices).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(excluded_voice_integration, empirical, 'Whether the doctrine''s sufficiency claim holds when contested by those with real-event experience.').

omega_variable(
    kernel_reading_incommensurability,
    'Are the sibling readings—catastrophe_necessity_reading, hybrid_degradation_reading, and simulation_fidelity_threshold—genuinely alternative framings of one kernel, or do they describe structurally different constraints?',
    'Epistemic analysis: each reading proposes a different empirical mechanism (necessity, generational decay, technology-threshold) for why simulation may be insufficient. If the mechanisms are incompatible (only one can be true), they are sibling readings of one contested kernel. If multiple mechanisms can coexist (e.g., some competence is technology-dependent AND some decays generationally), they may describe different structural constraints.',
    'If they are incommensurable readings, the kernel framing is correct and the constraint family is well-structured. If they describe orthogonal mechanisms, the corpus should be decomposed (each mechanism gets its own constraint). The decomposition affects how the regulatory system can learn: incommensurable readings block resolution; orthogonal mechanisms allow complementary evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the sibling readings are alternative framings of one contested kernel or separate structural claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t1970, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 1970, 0.08).
narrative_ontology:measurement_basis(cata_tr_t1970, projected).
narrative_ontology:measurement(cata_tr_t1985, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 1985, 0.12).
narrative_ontology:measurement_basis(cata_tr_t1985, observed).
narrative_ontology:measurement(cata_tr_t2000, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 2000, 0.16).
narrative_ontology:measurement_basis(cata_tr_t2000, observed).
narrative_ontology:measurement(cata_tr_t2010, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement_basis(cata_tr_t2010, observed).
narrative_ontology:measurement(cata_tr_t2018, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 2018, 0.21).
narrative_ontology:measurement_basis(cata_tr_t2018, observed).
narrative_ontology:measurement(cata_tr_t2025, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 2025, 0.22).
narrative_ontology:measurement_basis(cata_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t1970, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 1970, 0.15).
narrative_ontology:measurement_basis(cata_be_t1970, projected).
narrative_ontology:measurement(cata_be_t1985, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 1985, 0.18).
narrative_ontology:measurement_basis(cata_be_t1985, observed).
narrative_ontology:measurement(cata_be_t2000, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 2000, 0.22).
narrative_ontology:measurement_basis(cata_be_t2000, observed).
narrative_ontology:measurement(cata_be_t2010, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 2010, 0.25).
narrative_ontology:measurement_basis(cata_be_t2010, observed).
narrative_ontology:measurement(cata_be_t2018, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 2018, 0.27).
narrative_ontology:measurement_basis(cata_be_t2018, observed).
narrative_ontology:measurement(cata_be_t2025, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 2025, 0.28).
narrative_ontology:measurement_basis(cata_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t1970, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 1970, 0.08).
narrative_ontology:measurement_basis(cata_su_t1970, projected).
narrative_ontology:measurement(cata_su_t1985, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 1985, 0.1).
narrative_ontology:measurement_basis(cata_su_t1985, observed).
narrative_ontology:measurement(cata_su_t2000, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 2000, 0.12).
narrative_ontology:measurement_basis(cata_su_t2000, observed).
narrative_ontology:measurement(cata_su_t2010, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 2010, 0.14).
narrative_ontology:measurement_basis(cata_su_t2010, observed).
narrative_ontology:measurement(cata_su_t2018, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 2018, 0.15).
narrative_ontology:measurement_basis(cata_su_t2018, observed).
narrative_ontology:measurement(cata_su_t2025, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 2025, 0.15).
narrative_ontology:measurement_basis(cata_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, hybrid_degradation_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel catastrophe_proxy_sufficiency. The sibling readings—catastrophe_necessity_reading (real catastrophes are necessary), hybrid_degradation_reading (tacit knowledge degrades over generations), and simulation_fidelity_threshold (sufficiency is technology-dependent)—are SEPARATE constraint stories with distinct epsilon values and structural data. Each reading instantiates a different claim about what maintains competence indefinitely. This reading (simulation_as_proxy_catastrophe_reading) asserts categorical sufficiency; the siblings assert necessity, degradation, or threshold-dependency. All four constraints are linked via network.affects_constraints because they contest a single kernel and regulatory choices about one reading propagate pressure on the others. The kernel contest is unresolved; no reading has been foreclosed by evidence. The ε-invariance principle requires separate stories because each reading makes a different empirical claim with different beneficiary/victim structures and different persistence mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
