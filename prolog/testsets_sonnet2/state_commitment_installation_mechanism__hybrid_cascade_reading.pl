% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__hybrid_cascade_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__hybrid_cascade_reading, []).

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
 *   constraint_id: state_commitment_installation_mechanism__hybrid_cascade_reading
 *   human_readable: Apex-Cascade Commitment Installation Requiring Fringe Ratification (Hybrid Reading)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This story instantiates the hybrid-cascade reading of the state
 *   commitment installation kernel: a new commitment (legal, fiscal, or
 *   religious) is authored and issued at the apex of state authority, then
 *   cascades downward through intermediary elites who translate and enforce
 *   it, but the commitment does not stabilize until fringe actors — local
 *   brokers and communities at the periphery — validate it through partial
 *   adaptation and local reinterpretation. This is neither a pure bottom-up
 *   climb (endogenous_climb_reading) nor a pure top-down imposition that
 *   succeeds by fiat alone (exogenous_imposition_reading): the apex
 *   initiates, but stabilization is two-phase and depends on a second,
 *   distinct act of fringe legitimation that the center cannot fully script
 *   or predict. Extraction here is the value that flows from having
 *   compliance labor and interpretive labor performed by intermediaries and
 *   localities without their having authored the commitment's terms.
 *
 * KEY AGENTS:
 *   - central_state_apparatus: apex authority (institutional/arbitrage) — designs and issues the commitment
 *   - cascade_intermediary_elites: transmission layer (powerful/constrained) — translates and enforces, gains derived authority
 *   - peripheral_communities: recipients (powerless/trapped) — bear the compliance cost with no drafting voice
 *   - local_interpretive_brokers: fringe validators (moderate/constrained) — perform the stabilizing labor the constraint structurally depends on
 *   - rival_normative_orders: excluded (moderate/trapped) — displaced systems with no seat in the process
 *   - court_historians_and_state_scribes: analytical observer (institutional/analytical) — records and often overstates smooth adoption
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.58).
domain_priors:suppression_score(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.62).
domain_priors:theater_ratio(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__hybrid_cascade_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__hybrid_cascade_reading, "Apex-Cascade Commitment Installation Requiring Fringe Ratification (Hybrid Reading)").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__hybrid_cascade_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__hybrid_cascade_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__hybrid_cascade_reading, '9675ef4c-91eb-4beb-b904-11ab84d20190').
narrative_ontology:cs_kernel_codification('9675ef4c-91eb-4beb-b904-11ab84d20190', distributed).
narrative_ontology:cs_authority_grounding('9675ef4c-91eb-4beb-b904-11ab84d20190', extraction).
narrative_ontology:cs_interpretation_layer_present('9675ef4c-91eb-4beb-b904-11ab84d20190').
narrative_ontology:cs_reading_relation('9675ef4c-91eb-4beb-b904-11ab84d20190', state_commitment_installation_mechanism__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('9675ef4c-91eb-4beb-b904-11ab84d20190', state_commitment_installation_mechanism__exogenous_imposition_reading, influences).
narrative_ontology:cs_axiom('9675ef4c-91eb-4beb-b904-11ab84d20190', foundational, legitimacy_requires_two_phase_ratification).
narrative_ontology:cs_axiom_status(legitimacy_requires_two_phase_ratification, holdable).
narrative_ontology:cs_axiom_grounding('9675ef4c-91eb-4beb-b904-11ab84d20190', legitimacy_requires_two_phase_ratification, empirically_contingent).
narrative_ontology:cs_axiom('9675ef4c-91eb-4beb-b904-11ab84d20190', foundational, apex_initiation_is_necessary_but_insufficient).
narrative_ontology:cs_axiom_status(apex_initiation_is_necessary_but_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('9675ef4c-91eb-4beb-b904-11ab84d20190', apex_initiation_is_necessary_but_insufficient, conventional).
narrative_ontology:cs_reference_frame('9675ef4c-91eb-4beb-b904-11ab84d20190', apex_initiated_two_phase_stabilization).
narrative_ontology:cs_drift_state('9675ef4c-91eb-4beb-b904-11ab84d20190', post_consolidation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9675ef4c-91eb-4beb-b904-11ab84d20190', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__hybrid_cascade_reading, central_state_apparatus).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__hybrid_cascade_reading, cascade_intermediary_elites).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, peripheral_communities).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, local_interpretive_brokers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__hybrid_cascade_reading, local_interpretive_brokers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and promulgates the new commitment (legal code, religious reform, administrative standard, currency, land regime) at the apex and issues it downward through provincial and local administration. Bears the cost of drafting and initial promulgation but collects the long-run gain of a standardized, legible polity once the commitment stabilizes. Can revise the commitment's letter at will but cannot single-handedly make it stick in practice.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, central_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Provincial governors, regional clergy, guild heads, and local notables who transmit the apex commitment downward and translate it into locally intelligible form. They gain expanded authority as the indispensable translators of the new order, and can shape how harshly or leniently the commitment lands in their domain. Their exit is constrained: refusing to transmit invites replacement, but faithful transmission without local buy-in leaves them exposed to fringe non-compliance they are blamed for.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, cascade_intermediary_elites, beneficiary,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__hybrid_cascade_reading, cascade_intermediary_elites, agenda_setter).

% Villages, frontier settlements, and minority populations who receive the commitment as an imposition on existing practice — a new tax regime, a new legal code, a new religious observance. They have no seat in the drafting and cannot leave the jurisdiction. Their only leverage is partial, localized non-compliance or reinterpretation, which the state tolerates as long as it does not threaten the commitment's formal existence.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, peripheral_communities, payer,
    powerless, biographical, trapped, local).

% Village elders, local judges, low clergy, and headmen who must reconcile the apex commitment with existing local norms to make it livable — this is the fringe validation labor the constraint's stability depends on. They absorb the friction between the letter of the new commitment and its lived application, taking blame from above for imperfect compliance and from below for enforcing an unwanted order. They cannot decline this brokering role without losing standing in both directions, but their local reinterpretations also give them small pockets of discretionary power.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, local_interpretive_brokers, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__hybrid_cascade_reading, local_interpretive_brokers, beneficiary).

% Pre-existing customary, religious, or kin-based normative systems that the new apex commitment displaces or subordinates. They are not consulted in the design of the new commitment and their claims to legitimacy are structurally excluded from the cascade process, though their residual authority is precisely what fringe validation must contend with or absorb.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, rival_normative_orders, excluded,
    moderate, generational, trapped, regional).

% Chroniclers and record-keepers who document the installation and its reception, producing the official narrative of successful adoption. Their accounts are the primary surviving evidence of how cascade and fringe validation interacted, though their institutional position gives them incentive to overstate smooth uptake.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, court_historians_and_state_scribes, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_commitment_installation_mechanism__hybrid_cascade_reading, central_state_apparatus).
narrative_ontology:fixing_cost_class(state_commitment_installation_mechanism__hybrid_cascade_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, legible, apex-issued standard (legal, fiscal, religious, or administrative) that lets a state coordinate distant populations without renegotiating terms locally each time — a genuine solution to the problem of governing at scale without infinite local bargaining.
% TRANSFER_FUNCTION: Moves the authority to define legitimate practice from dispersed local and customary sources to the apex, while moving the labor of making that authority livable — translation, absorption of friction, management of local resistance — onto intermediary elites and local brokers, and moving compliance costs onto peripheral communities.
% ABSENT_VOICES: Peripheral communities and the rival normative orders being displaced have no seat in the commitment's drafting; their objections surface only indirectly, through the pace and quality of fringe validation, and are read by the center as compliance friction to be managed rather than as substantive claims to be answered.
% DISAPPEARANCE_RATIONALE: If the apex commitment and its cascade apparatus vanished, the standardization it imposed would fragment back toward locally varying customary practice; intermediary elites would lose their translator function and much of their derived authority; peripheral communities would revert to prior normative orders or negotiate new local arrangements without central reference.
% FOUNDING_PROBLEM: The state needed a way to install new commitments (legal codes, fiscal regimes, religious reforms) across a large, normatively heterogeneous territory faster than organic, bottom-up convergence would produce, while still achieving enough local buy-in that the commitment would not require permanent garrison-level enforcement.
% FOUNDING_PROBLEM_CORROBORATION: The central state apparatus and cascade intermediary elites attest the problem remains live — heterogeneous local practice still threatens governability. Local interpretive brokers and independent historical-sociological analysis of comparable cascades (e.g., comparative work on state legal codification and religious reform diffusion) attest that in many stabilized regions the founding coordination problem has been substantially solved for generations, and the continued apex-cascade machinery persists partly to preserve intermediary elite authority rather than to solve an active problem.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__hybrid_cascade_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__hybrid_cascade_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__hybrid_cascade_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__hybrid_cascade_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__hybrid_cascade_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_commitment_installation_mechanism__hybrid_cascade_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_commitment_installation_mechanism__hybrid_cascade_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58 by interval end) reflects a genuine coordination function — a legible common standard across a heterogeneous territory — layered with real asymmetric cost: the state and its intermediaries capture the long-run governability gain while peripheral communities and local brokers absorb the friction, translation labor, and compliance burden without having shaped the commitment's content. Suppression starts higher (0.70) during the initial imposition phase when enforcement is heaviest and least locally adapted, then eases modestly (0.62) as fringe validation absorbs resistance through local interpretation rather than continued coercion — this is the hybrid mechanism's signature: suppression substitutes toward interpretive absorption over time rather than escalating. Theater ratio rises mildly (0.20 to 0.34) as intermediary elites develop increasingly ceremonial compliance rituals that perform adoption without always reflecting substantive local acceptance.
 *
 * DIRECTIONALITY LOGIC:
 *   The central state apparatus sits nearest full beneficiary: it authors the commitment, bears drafting cost, and captures the governability and legitimacy gain once stabilization occurs — d low. Cascade intermediary elites are a genuine dual seat: they benefit from expanded derived authority (low-to-mid d) but also bear real risk and labor as the transmission point, justifying the secondary payer-adjacent framing without a directionality override, since the derivation from beneficiary+constrained-exit already captures this reasonably. Peripheral communities and local interpretive brokers are structural targets: trapped or constrained exit, no drafting voice, bearing the transfer's cost — d high. Rival normative orders are excluded rather than coordinated, which the model treats as adjacent to victim status even though they are not payers in the transactional sense — their exclusion is what fringe validation must overcome.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification (rather than a pure Snare) is required by the coordination function being real and separable: standardized law, fiscal regimes, or religious observance genuinely solve a governance-at-scale problem that ad hoc local bargaining cannot. What prevents this from being mislabeled as pure extraction is that fringe validation is not merely coerced compliance — local brokers exercise genuine discretionary reinterpretation that shapes the commitment's lived form, which is itself a (constrained) form of participation. What prevents this from being mislabeled as pure coordination (a Rope) is that the participation is not voluntary and the intermediary/apex seats capture disproportionate gain from labor performed by seats with no say in the terms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cascade_vs_climb_causal_priority,
    'In documented cases of successful commitment installation, does the historical record show apex initiation genuinely preceding fringe adaptation (supporting hybrid_cascade), or does closer archival examination reveal the fringe adaptation was already underway before apex codification merely ratified it (which would collapse this reading toward endogenous_climb)?',
    'Fine-grained dating of documentary evidence — court promulgation records versus local court/parish/guild records — to establish sequence rather than relying on the state''s own chronicle, which has institutional incentive to claim initiation.',
    'If fringe adaptation systematically precedes apex codification in well-documented cases, the hybrid_cascade_reading may be an artifact of state historiography rather than a distinct structural mechanism, and cases currently classified here would reclassify toward endogenous_climb_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cascade_vs_climb_causal_priority, empirical, 'Whether cascade sequencing is genuine or a retrospective state narrative artifact.').

omega_variable(
    fringe_validation_necessity_threshold,
    'How much fringe non-validation can an apex commitment absorb before it fails to stabilize — i.e., is fringe validation a soft preference that improves durability, or a hard structural requirement without which the commitment reverts?',
    'Comparative case analysis of commitments that were apex-issued but met sustained fringe rejection: track whether they were eventually withdrawn, enforced by permanent garrison, or quietly abandoned in practice while remaining nominally in force.',
    'If fringe rejection routinely produces quiet nominal survival with no real-world effect (a de facto piton), some hybrid_cascade cases may better classify as piton once the coordination function has atrophied; if fringe rejection routinely produces reversal, the hard-requirement reading is vindicated and the tangled_rope classification with high suppression sensitivity is confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fringe_validation_necessity_threshold, conceptual, 'Whether fringe validation is a hard stabilization requirement or a durability-improving preference.').

omega_variable(
    intermediary_elite_capture_degree,
    'Do cascade intermediary elites structurally benefit from the arrangement independent of whether the commitment itself succeeds, such that they have incentive to prolong ambiguous fringe validation indefinitely rather than resolve it?',
    'Track intermediary elite wealth, office tenure, and status across cases where commitments stabilized quickly versus cases where fringe validation dragged on for generations.',
    'If intermediaries benefit more from prolonged ambiguous validation than from resolution, this indicates a self-sustaining extraction dynamic layered on top of the coordination function, strengthening the tangled_rope reading against a more benign coordination-dominant account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intermediary_elite_capture_degree, empirical, 'Whether intermediary elites have incentive to prolong rather than resolve fringe validation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__hybrid_cascade_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(stat_tr_t8, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(stat_tr_t16, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(stat_tr_t24, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(stat_tr_t32, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 32, 0.32).
narrative_ontology:measurement(stat_tr_t40, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 40, 0.34).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(stat_be_t8, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(stat_be_t16, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(stat_be_t24, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 24, 0.54).
narrative_ontology:measurement(stat_be_t32, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(stat_be_t40, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(stat_su_t8, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(stat_su_t16, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(stat_su_t24, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 24, 0.63).
narrative_ontology:measurement(stat_su_t32, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 32, 0.62).
narrative_ontology:measurement(stat_su_t40, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__hybrid_cascade_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism__endogenous_climb_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism__exogenous_imposition_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language concept 'how new state commitments gain legitimacy.' Each sibling reading locates the causal engine of legitimation differently (fringe-bottom-up, apex-top-down-sufficient, or apex-initiated-but-fringe-dependent) and carries its own ε, beneficiary/victim structure, and classification, per the ε-invariance principle. They are linked here rather than merged because the historical record supports genuinely distinct mechanisms in different cases, not one mechanism observed from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
