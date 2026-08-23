% ============================================================================
% CONSTRAINT STORY: maat_order_principle__distributed_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__distributed_maintenance_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: maat_order_principle__distributed_maintenance_reading
 *   human_readable: Ma'at Distributed Maintenance Norm
 *   domain: ancient_history/political_philosophy/religious_studies
 *
 * SUMMARY:
 *   This constraint instantiates the distributed_maintenance_reading of the
 *   maat_order_principle kernel. It frames Ma'at not as unidirectional divine
 *   command but as a distributed responsibility in which Pharaoh, priesthood,
 *   scribes, artisans, and cultivators each sustain cosmic order through
 *   station-specific conduct. The reading is distinguished from its siblings
 *   by lowest extraction, multiple legitimate interpreters, and authority
 *   contingent on demonstrated maintenance rather than inherent status.
 *
 * KEY AGENTS:
 *   - pharaonic_office: Primary agenda-setter (institutional/identity_locked) â administers justice and ritual, bound by demonstrated maintenance rather than inherent divinity
 *   - temple_priesthood: Legitimate interpreter (organized/constrained) â shares interpretive authority and benefits from cultic role without exclusive royal delegation
 *   - commoner_cultivators: Primary maintenance agents at the base (powerless/identity_locked) â sustain cosmic order through labor, tax, and local ritual
 *   - egyptological_analyst: Analytical observer (analytical/analytical) â reconstructs the distributed reading from textual and archaeological evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__distributed_maintenance_reading, 0.3).
domain_priors:suppression_score(maat_order_principle__distributed_maintenance_reading, 0.3).
domain_priors:theater_ratio(maat_order_principle__distributed_maintenance_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__distributed_maintenance_reading, rope).
narrative_ontology:human_readable(maat_order_principle__distributed_maintenance_reading, "Ma'at Distributed Maintenance Norm").
narrative_ontology:topic_domain(maat_order_principle__distributed_maintenance_reading, "ancient_history/political_philosophy/religious_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__distributed_maintenance_reading, 'f16177da-b61f-43a8-b284-895c4a0c67ec').
narrative_ontology:cs_kernel_codification('f16177da-b61f-43a8-b284-895c4a0c67ec', distributed).
narrative_ontology:cs_authority_grounding('f16177da-b61f-43a8-b284-895c4a0c67ec', practice).
narrative_ontology:cs_reading_relation('f16177da-b61f-43a8-b284-895c4a0c67ec', maat_order_principle__divine_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('f16177da-b61f-43a8-b284-895c4a0c67ec', maat_order_principle__reciprocity_reading, coexists_with).
narrative_ontology:cs_axiom('f16177da-b61f-43a8-b284-895c4a0c67ec', foundational, maat_maintained_by_all_stations).
narrative_ontology:cs_axiom_status(maat_maintained_by_all_stations, holdable).
narrative_ontology:cs_axiom_grounding('f16177da-b61f-43a8-b284-895c4a0c67ec', maat_maintained_by_all_stations, theological).
narrative_ontology:cs_axiom('f16177da-b61f-43a8-b284-895c4a0c67ec', foundational, authority_from_maintenance_not_inherent_status).
narrative_ontology:cs_axiom_status(authority_from_maintenance_not_inherent_status, holdable).
narrative_ontology:cs_axiom_grounding('f16177da-b61f-43a8-b284-895c4a0c67ec', authority_from_maintenance_not_inherent_status, conventional).
narrative_ontology:cs_reference_frame('f16177da-b61f-43a8-b284-895c4a0c67ec', maat_as_distributed_practice).
narrative_ontology:cs_drift_state('f16177da-b61f-43a8-b284-895c4a0c67ec', centralized_ideology_ascendant, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f16177da-b61f-43a8-b284-895c4a0c67ec', '').
narrative_ontology:cs_kernel_id(maat_order_principle__distributed_maintenance_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, pharaonic_office).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, temple_priesthood).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, scribal_bureaucracy).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, commoner_cultivators).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, urban_artisans).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers high justice, leads state ritual, and commands military action as part of cosmic maintenance; legitimacy depends on demonstrated performance of these duties rather than on inherent divine status; the office is constitutive of the holder's identity and cannot be exited without dissolving the social order.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, pharaonic_office, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, pharaonic_office, beneficiary).

% Maintains temple cults, interprets divine signs, preserves sacred texts, and teaches proper conduct; they are one of multiple legitimate interpreters of Ma'at, deriving authority from demonstrated ritual competence rather than from delegation by the pharaonic office alone.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, temple_priesthood, beneficiary,
    organized, generational, constrained, national).

% Records legal judgments, manages granaries and labor assignments, and copies wisdom literature that encodes station-specific duties; benefits from the literacy premium and administrative stability the Ma'at framework provides.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, scribal_bureaucracy, beneficiary,
    organized, biographical, constrained, national).

% Perform agricultural labor, render taxes in grain and corvÃ©e, and participate in local festivals; receive the benefits of flood management, dispute resolution, and a cosmologically meaningful station; their identity is fused with their occupational role and local cultic community.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, commoner_cultivators, beneficiary,
    powerless, biographical, identity_locked, local).

% Produce craft goods and construction labor for temples and royal tombs; their specialized skill grants moderate status within the station framework and access to state rations; they rely on the scribal-temple network for material supply and ritual timing.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, urban_artisans, beneficiary,
    moderate, biographical, constrained, regional).

% Modern scholarly observer who reconstructs the distributed maintenance reading from archaeological and textual evidence; recognizes that the same Ma'at kernel supports competing centralized sibling readings with divergent beneficiary structures.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, egyptological_analyst, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__distributed_maintenance_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates society-wide cosmic and social order by assigning specific maintenance duties to every station, from ruler to cultivator, such that collective ritual-judicial action sustains Ma'at without requiring centralized command of all legitimate interpretation.
% TRANSFER_FUNCTION: Moves legitimacy, agricultural surplus, and labor obligation across stations in exchange for the promise of cosmic stability and social predictability; the transfer is symmetrically distributed rather than unidirectional, with each station giving and receiving according to its role.
% ABSENT_VOICES: Foreign peoples and internal heterodox movements who reject the station-based cosmology are structurally excluded; they would argue that Ma'at is merely an ideology naturalizing hierarchy, but their absence from the interpretive community is constitutive of the framework's boundary maintenance.
% DISAPPEARANCE_RATIONALE: If the distributed maintenance framework vanished, the legitimacy of station-specific duties would collapse, priestly and scribal roles would lose their coordinating function, and the pharaonic office would have to renegotiate authority on purely coercive or charismatic grounds rather than cosmological ones.
% FOUNDING_PROBLEM: The problem of maintaining cosmic and social order across a vast, agrarian, river-valley civilization without institutional mechanisms that scale beyond kinship or raw coercion.
% FOUNDING_PROBLEM_CORROBORATION: Modern Egyptological scholarship and comparative anthropology attest that distributed ritual-judicial responsibilities were real in some periods; royal inscriptions and rebel texts such as the Admonitions of Ipuwer contest whether the framework was ever genuinely distributed or always royal-dominated.
narrative_ontology:disappearance_verdict(maat_order_principle__distributed_maintenance_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__distributed_maintenance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__distributed_maintenance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(maat_order_principle__distributed_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__distributed_maintenance_reading, 0.3, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__distributed_maintenance_reading_tests).
:- end_tests(maat_order_principle__distributed_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.30) because the reading distributes accountability and benefit across all stations rather than concentrating extraction upward. Suppression is moderate-low (0.30): the framework is enforced primarily by internalized cosmological belief and station-identity fusion rather than by active coercive machinery. Theater ratio is low (0.20) because the ritual and judicial functions are largely operational, though some performative maintenance increases as centralizing ideologies compete. Accessibility collapse is moderate-high (0.60) because once the station framework is accepted, alternatives appear cosmologically illegitimate; resistance is low (0.20) because participants experience the arrangement as symmetric coordination.
 *
 * PERSPECTIVAL GAP:
 *   The pharaonic office and commoner cultivators occupy vastly different power levels but both experience the constraint as a source of cosmological meaning and social predictability; the engine computes low directionality divergence because both are declared beneficiaries and neither is declared a victim. The analytical observer sees a coordination equilibrium that distributes accountability broadly, while an adherent sees cosmic necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   All named seats are beneficiaries in this reading: the pharaonic office gains conditional legitimacy, the priesthood and scribal class gain status and stability, and cultivators and artisans gain cosmic meaning and predictable order. Because no victim group is declared, the engine derives low d (beneficiary-side) for all seats, producing low effective extraction. Identity-locked exit for the pharaonic office and commoners dampens but does not override the beneficiary derivation, reflecting that exit is costly but the constraint still subsidizes relative to chaos.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â maintaining order in a complex agrarian civilization â remained live throughout pharaonic history, so classical mandatrophy (arrangement outliving its problem) does not cleanly apply. However, the drift toward centralized divine-kingship ideology captured in the divine_mandate_reading represents a reading-level capture: the distributed accountability framework was progressively reinterpreted to concentrate extraction. The distributed maintenance reading resists this by insisting on the original multi-polar maintenance structure, preventing mislabeling of the captured version as the only authentic Ma'at.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Is this constraint a genuine distributed accountability framework or a legitimacy narrative layered over a more centralized extraction structure?',
    'Comparative analysis with sibling readings and archaeological evidence of administrative decentralization versus centralization.',
    'If the distributed reading is primarily a legitimizing narrative, the constraint''s extractiveness is higher than authored and directionality shifts toward the pharaonic office.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Whether the distributed maintenance reading reflects real structure or legitimizing cover.').

omega_variable(
    interpreter_plurality_or_elite_diffusion,
    'Does multiple legitimate interpreters mean genuine plural authority, or merely the diffusion of elite ideology across multiple elite roles?',
    'Prosopographic study of decision-making records: do non-royal elites exercise independent Ma''at judgment or merely transmit royal directives?',
    'If interpreters are elite diffusers, the constraint is a tangled_rope coordinating elite hierarchy with commoner extraction; if genuinely plural, it remains a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpreter_plurality_or_elite_diffusion, empirical, 'Whether interpretive plurality is genuine or elite diffusion.').

omega_variable(
    sibling_reading_stability,
    'How would classification change if the divine_mandate_reading or reciprocity_reading were adopted as the operative framework?',
    'Cross-reading analysis: the divine_mandate_reading would concentrate directionality at the pharaonic office, while reciprocity_reading would create symmetric mutual obligation with different extraction profiles.',
    'The kernel''s contested nature means this reading''s low extraction is unstable against competing readings that reintroduce hierarchy or alter mutuality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_stability, conceptual, 'Structural instability against sibling readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__distributed_maintenance_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_dist_tr_t0, maat_order_principle__distributed_maintenance_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(maat_dist_tr_t5, maat_order_principle__distributed_maintenance_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement(maat_dist_tr_t10, maat_order_principle__distributed_maintenance_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(maat_dist_tr_t15, maat_order_principle__distributed_maintenance_reading, theater_ratio, 15, 0.14).
narrative_ontology:measurement(maat_dist_tr_t20, maat_order_principle__distributed_maintenance_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(maat_dist_tr_t25, maat_order_principle__distributed_maintenance_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement(maat_dist_tr_t30, maat_order_principle__distributed_maintenance_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(maat_dist_be_t0, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(maat_dist_be_t5, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(maat_dist_be_t10, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 10, 0.23).
narrative_ontology:measurement(maat_dist_be_t15, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 15, 0.25).
narrative_ontology:measurement(maat_dist_be_t20, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 20, 0.26).
narrative_ontology:measurement(maat_dist_be_t25, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 25, 0.28).
narrative_ontology:measurement(maat_dist_be_t30, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 30, 0.3).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(maat_order_principle__distributed_maintenance_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__distributed_maintenance_reading, identity_coordination).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__reciprocity_reading).

% DUAL FORMULATION NOTE:
% The maat_order_principle kernel decomposes into three structurally distinct readings: distributed_maintenance_reading (low extraction, distributed accountability), divine_mandate_reading (high extraction, inherent royal status), and reciprocity_reading (mutual obligation, symmetric extraction). Each reading carries a different epsilon and stakeholder directionality. This decomposition follows the epsilon-invariance principle: the natural-language label 'Ma'at' conflates multiple structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
