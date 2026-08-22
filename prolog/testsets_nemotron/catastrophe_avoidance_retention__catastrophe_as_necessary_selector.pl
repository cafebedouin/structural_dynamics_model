% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__catastrophe_as_necessary_selector
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__catastrophe_as_necessary_selector, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_avoidance_retention__catastrophe_as_necessary_selector
 *   human_readable: Catastrophe as Necessary Selector for Competence Retention
 *   domain: safety_engineering/organizational_learning/high_reliability_systems
 *
 * SUMMARY:
 *   This constraint encodes the reading that only genuine catastrophic events
 *   — with their chaos, mortality salience, and organizational trauma —
 *   generate the selection pressure necessary to maintain competence in
 *   high-reliability systems. Long peacetime periods inevitably produce
 *   competence decay; simulation and drills create false confidence by
 *   lacking the irreducible stakes of real catastrophe. The industry becomes
 *   vulnerable to black swan re-emergence as institutional memory fades. This
 *   is one reading of the catastrophe_avoidance_retention kernel; sibling
 *   readings (simulation_as_proxy_catastrophe, hybrid_near_miss_learning)
 *   contest whether alternative mechanisms can sustain competence.
 *
 * KEY AGENTS:
 *   - high_reliability_organizations: Primary beneficiary (institutional/biographical/arbitrage) — maintain competence through catastrophe memory; collect rents from barrier to entry
 *   - legacy_operators_with_survival_experience: Beneficiary (organized/biographical/mobile) — their catastrophe experience becomes scarce, valuable capital
 *   - new_entrants_without_catastrophe_memory: Primary victim (moderate/biographical/constrained) — face competence demands they cannot meet without catastrophe experience; excluded from markets or forced into costly simulation
 *   - simulation_reliant_operators: Victim (powerful/biographical/constrained) — invest heavily in simulation but suffer false confidence; extraction via wasted investment and blind spots
 *   - frontline_personnel_in_decaying_systems: Victim (powerless/immediate/trapped) — bear the mortality risk when decayed competence meets catastrophe; no exit from the system they operate in
 *   - safety_regulators: Beneficiary/agenda_setter (institutional/generational/arbitrage) — set standards that encode catastrophe memory; their authority derives from the 'only catastrophes teach' premise
 *   - insurance_underwriters: Beneficiary (powerful/generational/arbitrage) — price catastrophe risk based on operator catastrophe memory; extract premia from the decay gradient
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.68).
domain_priors:suppression_score(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.42).
domain_priors:theater_ratio(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, "Catastrophe as Necessary Selector for Competence Retention").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, "safety_engineering/organizational_learning/high_reliability_systems").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, '46fb9fe2-c0a2-4543-9cdd-d0dccd76bd64').
narrative_ontology:cs_kernel_codification('46fb9fe2-c0a2-4543-9cdd-d0dccd76bd64', distributed).
narrative_ontology:cs_authority_grounding('46fb9fe2-c0a2-4543-9cdd-d0dccd76bd64', practice).
narrative_ontology:cs_interpretation_layer_present('46fb9fe2-c0a2-4543-9cdd-d0dccd76bd64').
narrative_ontology:cs_reading_relation('46fb9fe2-c0a2-4543-9cdd-d0dccd76bd64', catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, forecloses).
narrative_ontology:cs_reading_relation('46fb9fe2-c0a2-4543-9cdd-d0dccd76bd64', catastrophe_avoidance_retention__hybrid_near_miss_learning, coexists_with).
narrative_ontology:cs_axiom('46fb9fe2-c0a2-4543-9cdd-d0dccd76bd64', foundational, mortality_salience_irreducible).
narrative_ontology:cs_axiom_status(mortality_salience_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('46fb9fe2-c0a2-4543-9cdd-d0dccd76bd64', mortality_salience_irreducible, deontological).
narrative_ontology:cs_axiom('46fb9fe2-c0a2-4543-9cdd-d0dccd76bd64', foundational, organizational_trauma_as_competence_encoder).
narrative_ontology:cs_axiom_status(organizational_trauma_as_competence_encoder, holdable).
narrative_ontology:cs_axiom_grounding('46fb9fe2-c0a2-4543-9cdd-d0dccd76bd64', organizational_trauma_as_competence_encoder, empirically_contingent).
narrative_ontology:cs_axiom('46fb9fe2-c0a2-4543-9cdd-d0dccd76bd64', secondary, simulation_cannot_replicate_irreversibility).
narrative_ontology:cs_axiom_status(simulation_cannot_replicate_irreversibility, holdable).
narrative_ontology:cs_axiom_grounding('46fb9fe2-c0a2-4543-9cdd-d0dccd76bd64', simulation_cannot_replicate_irreversibility, empirically_contingent).
narrative_ontology:cs_reference_frame('46fb9fe2-c0a2-4543-9cdd-d0dccd76bd64', catastrophe_memory_as_competence_standard).
narrative_ontology:cs_drift_state('46fb9fe2-c0a2-4543-9cdd-d0dccd76bd64', post_fukushima_deepwater_horizon_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('46fb9fe2-c0a2-4543-9cdd-d0dccd76bd64', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, high_reliability_organizations).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, safety_regulators).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, insurance_underwriters).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, legacy_operators_with_survival_experience).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, new_entrants_without_catastrophe_memory).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, simulation_reliant_operators).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, cost_pressured_operators).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, frontline_personnel_in_decaying_systems).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, experience_based_competence).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, trauma_as_learning_mechanism).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, selection_via_failure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizations in nuclear, aviation, chemical, and oil/gas sectors that have survived catastrophes and retained the institutional memory. They set industry standards, write the training curricula, and define what counts as 'competence.' Their catastrophe memory is a moat: new entrants cannot replicate it, regulators defer to it, insurers price against it. They can arbitrage across jurisdictions and sectors.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, high_reliability_organizations, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, high_reliability_organizations, agenda_setter).

% Individual operators, managers, and specialists who personally experienced catastrophic events (e.g., Three Mile Island veterans, Chernobyl liquidators, Piper Alpha survivors, Deepwater Horizon responders). Their embodied trauma-knowledge is scarce capital. They command premium consulting rates, sit on review boards, and train the next generation. They can move between organizations and sectors that value catastrophe memory.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, legacy_operators_with_survival_experience, beneficiary,
    organized, biographical, mobile, global).

% New companies, new facilities, and new workforces entering high-reliability domains without any catastrophic event in their institutional or personal history. They must either: (a) hire legacy operators at premium cost, (b) invest massively in simulation that this reading says creates false confidence, or (c) accept higher accident risk. Their exit is constrained by capital requirements, regulatory barriers, and the time needed to build organic memory.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, new_entrants_without_catastrophe_memory, payer,
    moderate, biographical, constrained, global).

% Well-resourced organizations that have invested heavily in high-fidelity simulation, digital twins, VR/AR training, and synthetic environments as substitutes for operational experience. They believe simulation can replicate catastrophe's selection pressure. This reading says they are extracting a false confidence: their drills lack mortality salience, organizational trauma, and the chaotic irreversibility of real catastrophe. They pay twice — for the simulation and for the blind spots it creates. Exit is constrained by sunk costs and regulatory acceptance of simulation-based certification.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, simulation_reliant_operators, payer,
    powerful, biographical, constrained, global).

% Operators, maintenance crews, shift supervisors, and field personnel working in systems where catastrophe memory has decayed but the hazard remains. They inherit procedures they don't understand the rationale for, work in organizations that have normalized deviance, and face the actual mortality risk when the next catastrophe occurs. They cannot exit without leaving their profession, community, and identity. Their bodies are the ultimate test of whether competence was maintained.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, frontline_personnel_in_decaying_systems, payer,
    powerless, immediate, trapped, local).

% Regulatory bodies (NRC, FAA, OSHA, HSE, IAEA) that set competence standards, certify training programs, and license facilities. Their authority and legitimacy derive from the 'only catastrophes teach' premise: they exist because catastrophes happened, and they prevent the next one by enforcing catastrophe-memory standards. They arbitrage across international regulatory harmonization and industry capture pressures.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, safety_regulators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, safety_regulators, beneficiary).

% Insurance and reinsurance markets (Lloyd's syndicates, nuclear pools, aviation pools) that price catastrophe risk. They extract premia from the competence decay gradient: operators with fresh catastrophe memory get better rates; those in long peacetime decay pay more. They have no interest in simulation substituting for catastrophe memory — that would compress their risk spreads. They arbitrage across global capacity and alternative risk transfer markets.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, insurance_underwriters, beneficiary,
    powerful, generational, arbitrage, global).

% Companies selling high-fidelity simulators, digital twin platforms, VR training systems, and synthetic environment tools. They would argue simulation constitutes genuine practice (the simulation_as_proxy_catastrophe reading). This reading structurally excludes them: their product is defined as creating false confidence. Their exit is constrained because the high-reliability market is their primary customer base, and the catastrophe-memory establishment controls procurement standards.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, simulation_technology_vendors, excluded,
    powerful, biographical, constrained, global).

% Safety scientists, organizational learning researchers, and practitioners championing the hybrid_near_miss_learning reading. They argue that distributed learning from near-misses, foreign incidents, and high-realism drills can sustain competence without actual catastrophes. This reading treats them as well-meaning but wrong: near-miss learning lacks mortality salience and organizational trauma. They are excluded from the dominant standard-setting process but persist in academic and some progressive industry corners.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, near_miss_learning_advocates, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains competence in high-reliability domains by making catastrophe memory the selection criterion: only organizations and individuals who have survived catastrophe (or inherited its memory through rigorous apprenticeship) are certified as competent. This solves the coordination problem of who to trust with catastrophic hazard by using an unfakeable signal (survival).
% TRANSFER_FUNCTION: Moves status, authority, market access, and insurance capacity from those without catastrophe memory (new entrants, simulation-reliant operators) to those with it (legacy operators, catastrophe-experienced organizations). Also transfers mortality risk to frontline personnel in decaying systems. The 'currency' is catastrophe memory — an unfakeable but decaying asset.
% ABSENT_VOICES: Frontline personnel in decaying systems (trapped, powerless) would object to being the ultimate test of competence decay but are structurally silenced by their trapped position. Simulation technology vendors and near-miss learning advocates (excluded) would argue for alternative competence pathways but are kept out by the catastrophe-memory establishment that controls standards. Future catastrophe victims (not yet born) have no voice at all.
% DISAPPEARANCE_RATIONALE: If the 'only catastrophes teach' constraint vanished overnight: new entrants could enter high-reliability domains without hiring legacy operators at premium; simulation vendors would compete on fidelity rather than legitimacy; regulators would have to develop competence metrics not based on catastrophe memory; insurance spreads would compress; frontline personnel mortality risk would shift as competence maintenance mechanisms changed. The entire high-reliability ecosystem would reorganize around whatever substitute selection mechanism emerged (simulation, near-miss learning, or nothing).
% FOUNDING_PROBLEM: Early high-reliability domains (nuclear, aviation, chemical) discovered that procedural compliance and theoretical training were insufficient to prevent catastrophes. Only organizations that had survived a catastrophe — and internalized its lessons through organizational trauma — maintained the chronic unease, procedural rigor, and mortality salience necessary to prevent the next one. The founding problem: how to select for and maintain this unfakeable competence state without requiring new catastrophes.
% FOUNDING_PROBLEM_CORROBORATION: The catastrophe-memory establishment (legacy operators, regulators, insurers) attests the problem remains live: competence still decays without catastrophe, simulation still creates false confidence. Hybrid near-miss learning advocates (Hopkins, Dekker, Reason's intellectual descendants) and simulation advocates (NASA's simulation heritage, nuclear digital twin programs) attest the problem is substantially solved: distributed learning and high-fidelity simulation can maintain competence without new catastrophes. Commercial aviation's 2010-2020 safety record without major hull-loss catastrophes is cited by both sides — the establishment as 'luck running out,' the advocates as 'near-miss learning working.'
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial: the constraint extracts from new entrants (barriers to entry without catastrophe memory), simulation-reliant operators (wasted investment in false confidence), and frontline personnel (mortality risk from decay). Theater ratio (0.55) is high: the 'safety culture' and 'learning organization' rhetoric increasingly performs competence maintenance while actual catastrophe memory decays. Suppression (0.42) is moderate: the constraint persists through regulatory standards, insurance requirements, and industry norms that encode catastrophe memory — not through direct coercion. Accessibility collapse (0.65) reflects that alternatives (simulation, near-miss learning) are structurally devalued by this reading's premise. Resistance (0.35) is low-moderate: new entrants and simulation advocates contest the premise but lack the catastrophe-memory capital to displace it.
 *
 * DIRECTIONALITY LOGIC:
 *   High-reliability organizations and legacy operators are structural beneficiaries (d ~0.15): they hold the scarce catastrophe memory that the constraint values, and they set the standards. New entrants and simulation-reliant operators are targets (d ~0.75-0.85): they pay the cost of the constraint's premise through barriers, wasted investment, and blind spots. Frontline personnel are deeply trapped targets (d ~0.95): they bear mortality risk with no exit. Regulators and insurers sit near the beneficiary end (d ~0.2): their authority and pricing power derive from the constraint's premise. The directionality gradient maps directly to catastrophe-memory possession.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem — maintaining competence in high-stakes domains — remains live (catastrophes still occur, competence still decays). But the reading's specific claim ('only actual catastrophes work') may be obsoleted by hybrid near-miss learning if that reading proves empirically adequate. If hybrid learning suffices, this reading becomes a piton: a formerly functional coordination mechanism (catastrophe memory as selection pressure) whose function has atrophied but persists through institutional inertia and the vested interests of catastrophe-memory holders. The high theater ratio (0.55) already signals this drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of a contested kernel (catastrophe_avoidance_retention) rather than a standalone claim?',
    'Compare structural predictions across readings: if sibling readings (simulation_as_proxy_catastrophe, hybrid_near_miss_learning) produce different beneficiary/victim structures and different ε for the same domain, the kernel framing is validated.',
    'If validated, this reading''s ε (0.68) and structural data describe only the catastrophe-as-necessary-selector position; the other readings are separate constraint stories with their own metrics. Misidentifying them as one constraint would average over structurally distinct positions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this constraint is properly understood as one reading of the catastrophe_avoidance_retention kernel').

omega_variable(
    simulation_fidelity_boundary,
    'At what fidelity threshold (if any) does simulation cease to produce false confidence and become genuine practice?',
    'Empirical studies of drill-to-reality transfer in nuclear, aviation, and chemical sectors; longitudinal tracking of operators who train exclusively on high-fidelity simulators vs. those with operational incident exposure.',
    'If a fidelity threshold exists, the ''simulation creates false confidence'' claim becomes contingent rather than structural; the constraint''s extraction profile would shift for operators above the threshold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_fidelity_boundary, empirical, 'Whether high-fidelity simulation can structurally substitute for catastrophic selection pressure').

omega_variable(
    near_miss_learning_sufficiency,
    'Can distributed near-miss learning (foreign incidents, close calls, high-realism drills) sustain competence without any actual catastrophes?',
    'Case studies of industries with long catastrophe-free periods but strong near-miss cultures (commercial aviation 2010-2020, nuclear power post-Chernobyl/Fukushima learning); measure competence decay rates against near-miss reporting density.',
    'If near-miss learning suffices, the hybrid_near_miss_learning reading captures the functional structure and this reading''s ''only actual catastrophes'' claim is false — reducing its ε and reclassifying toward scaffold or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(near_miss_learning_sufficiency, empirical, 'Whether the hybrid reading''s coordination function is empirically adequate').

omega_variable(
    competence_decay_measurement,
    'How is ''competence decay'' during peacetime periods operationally measured and distinguished from efficiency gains?',
    'Develop metrics that separate genuine competence erosion from legitimate procedural streamlining; track latent error rates, procedural drift, and organizational memory loss in long-incident-free periods.',
    'If decay cannot be reliably measured separately from efficiency, the constraint''s extraction claim rests on an unobservable; the ''inevitable decay'' narrative may be a justification for maintaining costly redundancy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_decay_measurement, conceptual, 'Whether competence decay is a measurable structural phenomenon or a narrative construct').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catastrophe_selector_tr_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 0, 0.35).
narrative_ontology:measurement(catastrophe_selector_tr_t5, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 5, 0.4).
narrative_ontology:measurement(catastrophe_selector_tr_t10, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 10, 0.45).
narrative_ontology:measurement(catastrophe_selector_tr_t15, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 15, 0.5).
narrative_ontology:measurement(catastrophe_selector_tr_t20, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 20, 0.53).
narrative_ontology:measurement(catastrophe_selector_tr_t25, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 25, 0.55).

% Extraction over time
narrative_ontology:measurement(catastrophe_selector_be_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(catastrophe_selector_be_t5, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(catastrophe_selector_be_t10, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(catastrophe_selector_be_t15, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(catastrophe_selector_be_t20, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(catastrophe_selector_be_t25, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(catastrophe_selector_su_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(catastrophe_selector_su_t5, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 5, 0.33).
narrative_ontology:measurement(catastrophe_selector_su_t10, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 10, 0.36).
narrative_ontology:measurement(catastrophe_selector_su_t15, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 15, 0.39).
narrative_ontology:measurement(catastrophe_selector_su_t20, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(catastrophe_selector_su_t25, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 25, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.1).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_avoidance_retention__hybrid_near_miss_learning).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, regulatory_capture_via_catastrophe_memory).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, simulation_industry_rent_extraction).

% DUAL FORMULATION NOTE:
% This constraint is one member of the catastrophe_avoidance_retention constraint family (kernel_id: catastrophe_avoidance_retention). Three readings decompose the kernel: catastrophe_as_necessary_selector (this story, ε=0.68, tangled_rope), hybrid_near_miss_learning (ε≈0.35, rope/tangled_rope boundary), simulation_as_proxy_catastrophe (ε≈0.25, rope). The ε values differ substantially because each reading identifies different beneficiary/victim structures and different extraction mechanisms. This reading's high ε reflects extraction from those without catastrophe memory; the simulation reading's low ε reflects genuine coordination via drills; the hybrid reading sits between. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, institutional, 0.15).
constraint_indexing:directionality_override(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, organized, 0.2).
constraint_indexing:directionality_override(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, powerful, 0.75).
constraint_indexing:directionality_override(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, moderate, 0.8).
constraint_indexing:directionality_override(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
