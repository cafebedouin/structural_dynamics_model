% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__hybrid_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__hybrid_encoding_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: catastrophe_memory_survival__hybrid_encoding_reading
 *   human_readable: Catastrophe Memory as Hybrid Symbolic-Competence Encoding
 *   domain: religious/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   Communities practice ritual that simultaneously encodes symbolic
 *   boundary-maintenance (identity, group cohesion) and practical survival
 *   knowledge (resource management, seasonal timing, family protocols,
 *   adaptation strategies learned through historical catastrophe). This
 *   reading instantiates a hybrid-register interpretation: the ritual's
 *   function and legitimacy depend on holding both registers inseparably.
 *   External analysts — whether symbolic reductionists or functionalist
 *   reductionists — systematically misread the constraint by choosing one
 *   register and treating the other as ornamental. The cost of this
 *   misreading falls on communities that absorb the explanatory labor or
 *   suffer intervention failures. The kernel 'catastrophe memory survival' is
 *   contested: different readings separate the registers (symbol-only,
 *   competence-only) while this reading maintains their integration. The
 *   claimed type (rope) reflects the genuine coordination function; the
 *   metrics reflect the modest extraction cost imposed on communities forced
 *   to work in binary analytical frameworks.
 *
 * KEY AGENTS:
 *   - Ritual-practicing communities: maintain the constraint by practicing the dual-register ritual, absorbing identity-lock to the practice, bearing the cost of external misinterpretation.
 *   - External analysts (symbolic reductionists): classify ritual as identity/boundary-work, missing embedded practical knowledge, excluded from the community's internal frame.
 *   - External analysts (functionalist reductionists): classify ritual as survival-knowledge transmission, treating symbolic registers as ornamental, excluded from the community's internal frame.
 *   - Development practitioners and policy makers: operate on half-models derived from reductive analysts, inadvertently harm communities by stripping registers or dismissing knowledge.
 *   - Anthropological observers: witness the dual-register structure and the cost of binary classification; analytical seat with full scope visibility.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__hybrid_encoding_reading, 0.38).
domain_priors:suppression_score(catastrophe_memory_survival__hybrid_encoding_reading, 0.22).
domain_priors:theater_ratio(catastrophe_memory_survival__hybrid_encoding_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__hybrid_encoding_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_survival__hybrid_encoding_reading, "Catastrophe Memory as Hybrid Symbolic-Competence Encoding").
narrative_ontology:topic_domain(catastrophe_memory_survival__hybrid_encoding_reading, "religious/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__hybrid_encoding_reading, '2b289c1b-7f90-40c0-9542-6b8dc7eccc9c').
narrative_ontology:cs_kernel_codification('2b289c1b-7f90-40c0-9542-6b8dc7eccc9c', implicit).
narrative_ontology:cs_authority_grounding('2b289c1b-7f90-40c0-9542-6b8dc7eccc9c', practice).
narrative_ontology:cs_interpretation_layer_present('2b289c1b-7f90-40c0-9542-6b8dc7eccc9c').
narrative_ontology:cs_reading_relation('2b289c1b-7f90-40c0-9542-6b8dc7eccc9c', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_reading_relation('2b289c1b-7f90-40c0-9542-6b8dc7eccc9c', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_axiom('2b289c1b-7f90-40c0-9542-6b8dc7eccc9c', foundational, dual_register_inseparability).
narrative_ontology:cs_axiom_status(dual_register_inseparability, holdable).
narrative_ontology:cs_axiom_grounding('2b289c1b-7f90-40c0-9542-6b8dc7eccc9c', dual_register_inseparability, conventional).
narrative_ontology:cs_axiom('2b289c1b-7f90-40c0-9542-6b8dc7eccc9c', foundational, analytic_reductionism_harm).
narrative_ontology:cs_axiom_status(analytic_reductionism_harm, holdable).
narrative_ontology:cs_axiom_grounding('2b289c1b-7f90-40c0-9542-6b8dc7eccc9c', analytic_reductionism_harm, empirically_contingent).
narrative_ontology:cs_reference_frame('2b289c1b-7f90-40c0-9542-6b8dc7eccc9c', catastrophe_adaptive_ritual_integrity).
narrative_ontology:cs_drift_state('2b289c1b-7f90-40c0-9542-6b8dc7eccc9c', contemporary_development_intervention_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2b289c1b-7f90-40c0-9542-6b8dc7eccc9c', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, ritual_practicing_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_survival__hybrid_encoding_reading, external_analysts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities that practice ritual as both symbolic boundary-maintenance and embedded transmission of practical survival knowledge. The ritual simultaneously marks group identity and encodes water-sourcing, seasonal timing, family protocols, and resource management learned through catastrophe. They maintain the practice because abandoning it breaks both the symbolic frame that holds collective identity and the practical knowledge pipeline that supports survival in resource-scarce environments.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, ritual_practicing_communities, beneficiary,
    organized, generational, identity_locked, local).

% Researchers, development practitioners, and policy makers from outside the community who interpret ritual through either symbolic (identity) or functional (survival-competence) lenses exclusively. They pay a cost: when they classify ritual as 'merely symbolic,' they systematically miss embedded practical knowledge and sometimes recommend practices that destroy the knowledge-encoding structure. When they classify ritual as 'merely functional,' they misread cultural boundaries and inadvertently erode the symbolic registers that hold community cohesion during crises.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, external_analysts, payer,
    institutional, biographical, mobile, global).

% Analysts who read ritual exclusively through symbolic/identity registers and treat practical knowledge claims as post-hoc justifications or secondary effects. If they were in the conversation, they would argue that the 'real' function is boundary-maintenance and that any survival benefits are coincidental. Their exclusion from the ritual community's internal logic means their reductive frame often goes unchallenged in policy and research settings.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, symbolic_reductionists, excluded,
    institutional, biographical, mobile, global).

% Analysts who read ritual exclusively through practical-knowledge registers and treat symbolic registers as ornamental or delusional. They would argue the ritual's 'real' function is survival-competence transmission and that the symbolic boundary-work is window dressing. Their absence from the community's frame means their reductive critique often appears in development interventions that aim to 'extract the useful knowledge' and discard the 'superstitious wrapping.'
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, functionalist_reductionists, excluded,
    institutional, biographical, mobile, global).

% Disciplinary observers who document the constraint's operation and the structural cost of binary classification schemes. They witness how communities manage the dual register and how external pressure to choose one register or the other creates practical harm.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, anthropological_observers, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__hybrid_encoding_reading, external_analysts).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__hybrid_encoding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ritual coordinates two simultaneous survival functions: it marks and maintains group identity boundaries (symbolic register) while transmitting practical knowledge of resource management, seasonal timing, family protocols, and adaptation strategies learned through historical catastrophe (competence register). The ritual holds both registers together such that neither functions at full capacity without the other.
% TRANSFER_FUNCTION: Transfers the cost of theoretical classification work to external analysts and policy makers. When communities are forced to choose between symbolic and functional framings (by researchers, development practitioners, or policy designers), they absorb the cost of explaining why the choice destroys the ritual's effectiveness. Alternatively, the ritual transfers to analysts the burden of holding two registers in tension without reducing them.
% ABSENT_VOICES: Symbolic reductionists and functionalist reductionists are structurally excluded from the community's self-description. Their absence from the conversation means that external policy and research operates on half-models: interventions designed by functionalists strip the symbolic registers and break community cohesion; interventions designed by symbolists dismiss the practical knowledge and leave communities undefended against resource crises. The cost is borne by communities navigating between incompatible external frameworks.
% DISAPPEARANCE_RATIONALE: If the ritual disappeared, the community loses both symbolic identity cohesion and practical knowledge transmission. The world would rearrange: collective identity would fragment (no boundary-maintenance), and survival knowledge would degrade over generational transmission, leaving younger members unprepared for resource crises. Whether this constitutes catastrophic loss or manageable adaptation depends on whether the knowledge can be re-encoded in non-ritual forms (contested) and whether the symbolic boundaries can be maintained through alternative means (also contested).
% FOUNDING_PROBLEM: Historical catastrophe (drought, famine, displacement, violence) created selection pressure for mechanisms that simultaneously maintained group identity (so people remained organized to respond collectively) and transmitted survival knowledge (so adaptive strategies persisted across generations). Ritual emerged as a structure that encodes both registers inseparably: the symbolic performance carries embedded practical information.
% FOUNDING_PROBLEM_CORROBORATION: Communities describe ongoing resource scarcity and identity pressure, attesting the founding problem remains. External anthropologists and historians confirm the dual-register structure and its adaptive function. Development practitioners document the cost of reductive interventions — when the symbolic register is stripped, knowledge transmission fails; when the symbolic frame is treated as mere decoration, communities resist the 'extracted knowledge' because it breaks their identity coherence. Survival knowledge specialists (agricultural experts, water-resource managers) attest that the encoded practical knowledge is accurate and sophisticated, confirming it is not ornamental.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__hybrid_encoding_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__hybrid_encoding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__hybrid_encoding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_survival__hybrid_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__hybrid_encoding_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).
:- end_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-moderate (0.38) because the constraint primarily coordinates genuine survival functions; the extraction cost is imposed by external analysts who lack the community's dual-register frame. Suppression is low (0.22) — communities practice the ritual voluntarily; the constraint's persistence depends on identity-lock and genuine functionality, not coercion. Theater is moderate (0.41), reflecting the growing pressure to justify one register to external audiences while maintaining both internally: communities increasingly explain the practical knowledge component to justify the ritual to development practitioners, and increasingly emphasize symbolic identity to justify it to policy makers concerned with 'efficiency.' This theatricality is not deception but necessary translation work. The measurement series shows shallow growth in extractiveness and theater over the interval (external pressure intensifying) with suppression stable at a low level (the constraint persists by legitimacy and functionality, not enforcement). All metrics are authored at every shared time point per the alignment rule.
 *
 * PERSPECTIVAL GAP:
 *   From the community's seat, the ritual is a genuine coordination of survival and identity — it persists because it works on both registers. From the external analyst's seat, the ritual appears to overencode: it could be 'improved' by stripping the symbolic register (functionalist view) or by acknowledging the symbolic register is all that matters (symbolist view). The engine computes these as structurally different directionalities: the community benefits from the constraint as-is; the analyst pays the cost of the constraint's refusal to decompose. This is not disagreement about facts but disagreement about which register the constraint 'really' is — a kernel contest that this reading resolves by saying both registers are equally primary.
 *
 * DIRECTIONALITY LOGIC:
 *   Ritual-practicing communities are beneficiaries: they hold identity, transmit knowledge, and survive resource crises through the dual-register structure. Their directionality is near the beneficiary end (d ≈ 0.2). External analysts are the targets: they pay the cost of working in binary classification frameworks that exclude the dual-register reading. Their directionality is near the target end (d ≈ 0.75). Anthropological observers are symmetric analysts (d ≈ 0.5), witnessing both the coordination function and the extraction cost without personally bearing either. The identity-lock on the community is crucial: they cannot exit the ritual without breaking identity coherence, so their exit_options are 'identity_locked,' amplifying the beneficiary directionality (low cost, high cultural necessity). External analysts have 'mobile' exit options — they can adopt alternative frameworks — but they pay extraction costs while operating under binary classification schemes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (catastrophe survival through identity + knowledge transmission) is live: resource scarcity and identity pressure continue. The ritual persists because it solves the dual problem; it is not a zombie mandate. However, external analytical pressure increasingly forces communities to justify the ritual through one register or the other, creating a theater cost as they explain the 'hidden function' (practical knowledge) to skeptics or the 'identity benefits' to efficiency-focused policy makers. The constraint avoids mandatrophy because it solves a persistent real problem; the theater cost reflects adaptation to external misreading, not a decaying function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    register_separability_ambiguity,
    'Are the symbolic boundary-maintenance and practical knowledge-transmission functions structurally inseparable, or are they contingently coupled in this community but separable in principle?',
    'Ethnographic documentation of communities that have attempted to preserve one register while abandoning the other (or introducing the knowledge through non-ritual channels): do both registers degrade, or does one function at reduced capacity?',
    'If inseparable, the hybrid-register reading is the only coherent framework for understanding the ritual; reduction to either register destroys the constraint''s function. If separable, the functionalist and symbolist readings become viable alternatives, and the constraint could be decomposed into two distinct constraints (one per register) with lower ε values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(register_separability_ambiguity, empirical, 'Whether symbolic and competence registers are logically or only empirically coupled.').

omega_variable(
    external_analytical_misreading_mechanism,
    'Is the extraction cost borne by communities because external analysts genuinely cannot see the dual register, or because institutional pressures (funding, policy frameworks, disciplinary norms) force a choice between registers?',
    'Institutional analysis: examine whether researchers and practitioners operating outside binary categorical frameworks (e.g., integrated anthropological-ecological approaches) can perceive and work with the dual register.',
    'If the misreading is cognitive (structural opacity of dual-register encoding), the extraction cost is intrinsic and communities must continually educate external observers. If the misreading is institutional (funding silos, policy categories), the extraction cost is extrinsic and remediable through restructuring research and policy interfaces.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_analytical_misreading_mechanism, empirical, 'Whether dual-register opacity is cognitive or institutional.').

omega_variable(
    theater_escalation_trajectory,
    'Will theater_ratio continue rising as external pressures intensify, or will it plateau once communities develop stable translation protocols for explaining the dual register?',
    'Longitudinal measurement of community communication strategies: track whether explanation burdens increase indefinitely or stabilize around preferred frames (e.g., ''this ritual encodes water-sourcing strategies in symbolic form'').',
    'If theater continues rising, the constraint evolves toward piton-like performance (increasing shares of the ritual''s operation devoted to justifying the other register to external audiences). If theater plateaus, communities have developed sustainable translation protocols and the constraint remains a stable rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theater_escalation_trajectory, empirical, 'Whether theater_ratio ceiling is indeterminate or community-stabilized.').

omega_variable(
    kernel_reading_contest_underdetermination,
    'Is the contest between hybrid-register and reductive readings structurally indeterminate (both coherent, undecidable by data), or is it a false contest resolvable by empirical evidence?',
    'Construct observational criteria for distinguishing genuine dual-function encoding from post-hoc rationalization: examine the precision and specificity of embedded knowledge, the learning efficiency when transmitted through ritual vs. extracted-and-documented form, and the functionality of knowledge in stress conditions.',
    'If the contest is indeterminate, the three readings remain live alternatives and the kernel itself carries irreducible uncertainty (omega_c: conceptual). If empirically decidable, one reading may foreclose others (e.g., if competence emerges only through the symbolic medium, the hybrid and symbol readings win; if competence survives extraction, the functionalist reading viable).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_underdetermination, conceptual, 'Whether the three readings of the catastrophe memory survival kernel are empirically decidable or irreducibly contested.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__hybrid_encoding_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(cata_tr_t5, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 5, 0.34).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement(cata_tr_t15, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(cata_tr_t35, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cata_be_t5, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 5, 0.31).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(cata_be_t15, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 15, 0.36).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 30, 0.39).
narrative_ontology:measurement(cata_be_t35, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 35, 0.38).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(cata_su_t5, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 5, 0.19).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(cata_su_t15, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 15, 0.21).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 25, 0.23).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 30, 0.22).
narrative_ontology:measurement(cata_su_t35, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 35, 0.22).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 40, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__hybrid_encoding_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_survival__hybrid_encoding_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__competence_transmission_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__symbol_survival_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'catastrophe_memory_survival,' decomposed into three reading-specific constraint stories to resolve the struct-ural ambiguity of dual-register encoding. The hybrid_encoding_reading (this story) maintains both registers as equally primary; the competence_transmission_reading isolates practical knowledge as the core function; the symbol_survival_reading isolates identity-maintenance as the core function. All three readings share the same referent (the standing ritual practice) but author different ε values reflecting each reading's structural assumptions. The hybrid reading's ε (0.38) is higher than pure-coordination readings because it insists on the cost imposed by external analytical binary classification. Network edges link all three readings; no reading forecloses the others (they coexist as live alternatives in the kernel contest).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
