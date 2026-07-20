% ============================================================================
% CONSTRAINT STORY: liability_attribution__shared_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liability_attribution__shared_liability, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: liability_attribution__shared_liability
 *   human_readable: Shared Liability by Causal Contribution and Control
 *   domain: technology_governance/legal_theory/regulatory_design
 *
 * SUMMARY:
 *   This constraint story models the 'shared liability' reading of the
 *   liability attribution kernel in AI governance: a legal regime
 *   distributing joint liability along the technology value chain according
 *   to each actor's causal contribution and degree of control. Unlike
 *   readings that assign primary liability to developers or deployers alone,
 *   this reading places both in the cost-bearing set, generates contractual
 *   and insurance intermediation, and is defended as solving the
 *   collective-action problem of diffuse causation. The kernel is contested;
 *   this file instantiates only the shared-liability reading.
 *
 * KEY AGENTS:
 *   - ai_system_developers: Primary target (powerful/constrained) â bear design-phase liability, compliance costs, and insurance burdens.
 *   - ai_system_deployers: Primary target (powerful/constrained) â bear deployment-phase liability and operational safety obligations.
 *   - injured_parties: Primary beneficiary (powerless/constrained) â receive a compensation pathway without isolating a single tortfeasor.
 *   - commercial_insurers: Secondary beneficiary (institutional/arbitrage) â capture premium flows from mandated risk-pooling and intermediation.
 *   - regulatory_enforcers: Agenda setter (institutional/arbitrage) â design and enforce the liability allocation framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__shared_liability, 0.66).
domain_priors:suppression_score(liability_attribution__shared_liability, 0.7).
domain_priors:theater_ratio(liability_attribution__shared_liability, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, extractiveness, 0.66).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__shared_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__shared_liability, "Shared Liability by Causal Contribution and Control").
narrative_ontology:topic_domain(liability_attribution__shared_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__shared_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__shared_liability, 'e23d69d1-29af-48c9-82ae-126b492d053e').
narrative_ontology:cs_kernel_codification('e23d69d1-29af-48c9-82ae-126b492d053e', formalized).
narrative_ontology:cs_authority_grounding('e23d69d1-29af-48c9-82ae-126b492d053e', lineage).
narrative_ontology:cs_interpretation_layer_present('e23d69d1-29af-48c9-82ae-126b492d053e').
narrative_ontology:cs_reading_relation('e23d69d1-29af-48c9-82ae-126b492d053e', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('e23d69d1-29af-48c9-82ae-126b492d053e', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_axiom('e23d69d1-29af-48c9-82ae-126b492d053e', foundational, liability_proportional_to_causal_contribution).
narrative_ontology:cs_axiom_status(liability_proportional_to_causal_contribution, holdable).
narrative_ontology:cs_axiom_grounding('e23d69d1-29af-48c9-82ae-126b492d053e', liability_proportional_to_causal_contribution, conventional).
narrative_ontology:cs_axiom('e23d69d1-29af-48c9-82ae-126b492d053e', foundational, joint_control_expands_duty_beyond_single_actor).
narrative_ontology:cs_axiom_status(joint_control_expands_duty_beyond_single_actor, holdable).
narrative_ontology:cs_axiom_grounding('e23d69d1-29af-48c9-82ae-126b492d053e', joint_control_expands_duty_beyond_single_actor, conventional).
narrative_ontology:cs_reference_frame('e23d69d1-29af-48c9-82ae-126b492d053e', proportional_contribution_framework).
narrative_ontology:cs_drift_state('e23d69d1-29af-48c9-82ae-126b492d053e', ai_liability_emergence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e23d69d1-29af-48c9-82ae-126b492d053e', '').
narrative_ontology:cs_kernel_id(liability_attribution__shared_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, injured_parties).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, commercial_insurers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, ai_system_developers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, ai_system_deployers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and train AI systems whose architectural choices are subsequently scrutinized for causal contribution to downstream harms. They must document design decisions, engage in safety testing, and purchase liability insurance or set aside capital reserves. They cannot opt out of the legal regime without exiting the market entirely, and their ability to contractually shift liability to deployers is constrained by the shared-liability rule.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, ai_system_developers, payer,
    powerful, biographical, constrained, global).

% Integrate AI systems into operational contexts such as healthcare, finance, and transportation, bearing liability for deployment decisions, monitoring failures, and interaction effects with local conditions. They face compliance costs, mandatory insurance, and litigation risk even when the underlying model was created by another party. Exit is constrained by licensing requirements and market access rules that make full withdrawal prohibitively expensive.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, ai_system_deployers, payer,
    powerful, biographical, constrained, global).

% Individuals or organizations harmed by AI system behavior who seek compensation through the shared-liability regime. They benefit from not needing to isolate a single responsible party in a complex value chain, but their recovery is mediated by slow litigation, contingency fees, and insurance payout schedules. They have no practical exit from the need for redress.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, injured_parties, beneficiary,
    powerless, biographical, constrained, national).

% Offer liability insurance products to AI developers and deployers, pricing premiums based on risk assessments of causal contribution and control. They benefit from the mandatory or de-facto mandatory nature of the liability regime, which creates a captured demand base. They can exit unprofitable lines or jurisdictions but are structurally incentivized to maintain a regime complex enough to sustain intermediation.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, commercial_insurers, beneficiary,
    institutional, generational, arbitrage, global).

% Design the liability allocation framework, define evidentiary standards for causal contribution and control, and enforce compliance through courts and administrative bodies. They set the agenda for how liability is distributed but do not themselves bear costs or collect damages. They can reform or repeal the regime, though political and institutional inertia makes abrupt exit unlikely.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, regulatory_enforcers, agenda_setter,
    institutional, generational, arbitrage, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates responsibility for technology-related harms across the AI value chain when causal attribution is ambiguous, ensuring injured parties can recover damages without isolating a single responsible party and incentivizing safety investment by all contributors.
% TRANSFER_FUNCTION: Moves financial liability, compliance costs, and insurance premiums from injured parties to AI system developers and deployers based on their causal contribution and control, while also generating premium flows into emerging insurance and indemnification markets.
% ABSENT_VOICES: Small open-source developers without legal departments are largely absent from the rulemaking conversation; they would argue that proportional liability based on opaque causal contribution forces them out of the market. Consumer advocates seeking absolute strict liability rather than distributed proportionality are also underrepresented.
% DISAPPEARANCE_RATIONALE: If the shared liability regime vanished, developers and deployers would revert to contractual risk-shifting and warranty disclaimers, injured parties would face higher barriers to recovery when causation is diffuse, and the emerging AI liability insurance market would contract sharply.
% FOUNDING_PROBLEM: Single-point liability regimes fail to account for distributed causation in complex AI supply chains, leaving injured parties uncompensated when harm arises from the interaction of design choices and deployment contexts.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and law-and-economics researchers attest to the attribution gap; empirical incident databases document causal diffusion across the value chain. However, the claim that joint liability improves compensation outcomes relative to no-fault or strict single-party regimes is contested by industry economists, and independent empirical corroboration from outside the legal academy remains limited.
narrative_ontology:disappearance_verdict(liability_attribution__shared_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__shared_liability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__shared_liability, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(liability_attribution__shared_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__shared_liability, 0.66, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_attribution__shared_liability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(liability_attribution__shared_liability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(liability_attribution__shared_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.66 at interval end) is substantial because the regime imposes compliance, litigation, and insurance costs on developers and deployers that may exceed the direct compensation delivered to injured parties. Suppression (0.70) reflects the active legal enforcement needed to override contractual waivers and maintain the liability chain. Theater ratio (0.52) rises over the interval as performative safety documentation and contractual box-checking partially displace genuine risk reduction. Accessibility collapse (0.60) captures the legal unavailability of pure contractual risk-shifting once the regime is entrenched. Resistance (0.58) is moderate-to-high because both developer and deployer coalitions actively lobby for alternative single-point regimes.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (regulatory enforcers) experiences the constraint as a necessary coordination mechanism that solves the attribution gap; the payer seats (developers and deployers) experience it as an extractive cost layer that compounds opacity burdens. The beneficiary seat (injured parties) perceives improved access to compensation, while the secondary beneficiary seat (insurers) sees a new revenue line. The engine computes these divergences from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers and deployers are declared victims because the constraint extracts compliance costs, insurance premiums, and damages from them. Injured parties are declared beneficiaries because the constraint subsidizes their recovery pathway. Commercial insurers are declared beneficiaries because the constraint creates a mandated demand for their product. Regulatory enforcers are agenda setters, not beneficiaries. The exit modulation (constrained for devs/deployers, arbitrage for insurers) amplifies the extraction asymmetry: devs/deployers cannot easily exit the legal regime, while insurers can price and segment risk across jurisdictions.
 *
 * MANDATROPHY ANALYSIS:
 *   The regime could atrophy into a piton if the coordination function (genuine victim compensation) degrades while the extraction layer (insurance premiums and legal fees) persists. Mandatrophy is not yet resolved: the founding problem (diffuse causation) remains live, but the current reading's effectiveness is contested. A scaffold reading would require a sunset clause tied to empirical evaluation of compensation rates; absent such a clause, the regime risks ossification into theatrical compliance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    shared_liability_naturalness,
    'Does joint liability distributed by causal contribution represent a natural legal response to technological diffusion, or is it a constructed regime whose primary effect is to sustain litigation and insurance intermediation?',
    'Compare compensation rates and administrative costs under shared-liability regimes versus no-fault or single-point strict-liability regimes in comparable technology domains.',
    'If administrative costs consume most of the transferred value, the regime functions more as extraction for intermediaries than coordination for victims; if victim compensation rises disproportionately, the coordination function is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shared_liability_naturalness, conceptual, 'Whether the regime is a natural coordination response or a constructed extraction mechanism.').

omega_variable(
    insurance_market_extraction,
    'Do emerging liability insurance markets for AI improve risk-pooling efficiency, or do they capture value that would otherwise flow to injured parties?',
    'Analyze loss-ratio data and premium-to-payout spreads in AI liability insurance as the market matures; compare to established product liability lines.',
    'High loss ratios with controlled premiums would indicate genuine coordination; low loss ratios with inflated premiums would indicate extractive intermediation layered onto the liability constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(insurance_market_extraction, empirical, 'Whether insurance markets pool risk or extract rent.').

omega_variable(
    causal_contribution_opacity,
    'Can causal contribution and control be operationalized transparently enough to avoid arbitrary judicial allocation, or does the opacity of AI systems make ''proportionate'' liability a mask for random extraction?',
    'Track variance in liability allocation across courts for similar AI harm profiles; high variance suggests opacity drives arbitrary extraction.',
    'If allocation is unpredictable, developers and deployers face extraction without clear behavioral guidance, undermining the coordination justification; if predictable, the regime genuinely coordinates safety investment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_contribution_opacity, empirical, 'Whether proportionate liability is transparent or arbitrary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__shared_liability, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__shared_liability, theater_ratio, 0, 0.2).
narrative_ontology:measurement(liab_tr_t4, liability_attribution__shared_liability, theater_ratio, 4, 0.25).
narrative_ontology:measurement(liab_tr_t8, liability_attribution__shared_liability, theater_ratio, 8, 0.32).
narrative_ontology:measurement(liab_tr_t12, liability_attribution__shared_liability, theater_ratio, 12, 0.4).
narrative_ontology:measurement(liab_tr_t16, liability_attribution__shared_liability, theater_ratio, 16, 0.46).
narrative_ontology:measurement(liab_tr_t20, liability_attribution__shared_liability, theater_ratio, 20, 0.52).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__shared_liability, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(liab_be_t4, liability_attribution__shared_liability, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(liab_be_t8, liability_attribution__shared_liability, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(liab_be_t12, liability_attribution__shared_liability, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(liab_be_t16, liability_attribution__shared_liability, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(liab_be_t20, liability_attribution__shared_liability, base_extractiveness, 20, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__shared_liability, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(liab_su_t4, liability_attribution__shared_liability, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(liab_su_t8, liability_attribution__shared_liability, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(liab_su_t12, liability_attribution__shared_liability, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(liab_su_t16, liability_attribution__shared_liability, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(liab_su_t20, liability_attribution__shared_liability, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__shared_liability, resource_allocation).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, liability_attribution__developer_liability).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, liability_attribution__deployer_liability).

% DUAL FORMULATION NOTE:
% The liability_attribution kernel decomposes into three structurally distinct readings: shared_liability (this file), developer_liability, and deployer_liability. Each reading assigns a different locus of primary obligation and produces a different beneficiary/victim structure. They compete within the same policy domain and cannot be averaged into a single epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
