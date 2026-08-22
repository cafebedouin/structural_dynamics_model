% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__incumbent_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence__incumbent_preservation_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: qwerty_persistence__incumbent_preservation_reading
 *   human_readable: QWERTY Persistence via Incumbent Capital Defense
 *   domain: technology_history/industrial_standards/path_dependence
 *
 * SUMMARY:
 *   This constraint story instantiates the incumbent_preservation_reading of
 *   the qwerty_persistence kernel. It treats the persistence of the QWERTY
 *   keyboard layout as a tangled rope: a genuine coordination standard that
 *   has become an extractive mechanism through active incumbent defense.
 *   Manufacturers, training institutions, and professional typists benefit
 *   from the standard's persistence because it protects sunk capital
 *   investments, curriculum value, and skill premiums. Alternative layout
 *   adopters, efficiency-seeking users, and input innovators bear the costs
 *   through suppressed alternatives and suboptimal ergonomics. The reading is
 *   structurally distinct from the lapsed_alternatives_reading, which
 *   attributes persistence to coordination failure rather than active
 *   defense. This is a kernel reading: the constraint is authored clean, with
 *   sibling relationships routed to cs_structure and omegas.
 *
 * KEY AGENTS:
 *   - keyboard_manufacturers: Primary beneficiary (organized/constrained) â avoids retooling costs and protects supply-chain investments by withholding alternative-layout production.
 *   - typing_training_institutions: Primary beneficiary (moderate/constrained) â protects curriculum and certification revenue tied to QWERTY.
 *   - professional_typists: Primary beneficiary (organized/identity_locked) â protects skill value and professional identity; fuses self-concept with QWERTY proficiency.
 *   - alternative_layout_adopters: Primary target (moderate/constrained) â bears compatibility friction and social cost for deviating from the incumbent standard.
 *   - efficiency_seeking_users: Secondary target (moderate/constrained) â bears cumulative efficiency loss from a suboptimal default.
 *   - ergonomic_input_innovators: Secondary target (moderate/constrained) â blocked from mainstream market by OEM exclusion and software defaults.
 *   - standards_economists: Analytical observer (analytical/analytical) â documents path dependence and lock-in from outside the constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, 0.7).
domain_priors:suppression_score(qwerty_persistence__incumbent_preservation_reading, 0.82).
domain_priors:theater_ratio(qwerty_persistence__incumbent_preservation_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__incumbent_preservation_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence__incumbent_preservation_reading, "QWERTY Persistence via Incumbent Capital Defense").
narrative_ontology:topic_domain(qwerty_persistence__incumbent_preservation_reading, "technology_history/industrial_standards/path_dependence").

domain_priors:requires_active_enforcement(qwerty_persistence__incumbent_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__incumbent_preservation_reading, 'c86e5292-0c1a-47ea-bd50-751e0508489f').
narrative_ontology:cs_kernel_codification('c86e5292-0c1a-47ea-bd50-751e0508489f', formalized).
narrative_ontology:cs_authority_grounding('c86e5292-0c1a-47ea-bd50-751e0508489f', distributed).
narrative_ontology:cs_reading_relation('c86e5292-0c1a-47ea-bd50-751e0508489f', qwerty_persistence__lapsed_alternatives_reading, coexists_with).
narrative_ontology:cs_axiom('c86e5292-0c1a-47ea-bd50-751e0508489f', foundational, incumbent_capital_protection_legitimate_priority).
narrative_ontology:cs_axiom_status(incumbent_capital_protection_legitimate_priority, holdable).
narrative_ontology:cs_axiom_grounding('c86e5292-0c1a-47ea-bd50-751e0508489f', incumbent_capital_protection_legitimate_priority, conventional).
narrative_ontology:cs_axiom('c86e5292-0c1a-47ea-bd50-751e0508489f', foundational, installed_base_authority_is_binding).
narrative_ontology:cs_axiom_status(installed_base_authority_is_binding, holdable).
narrative_ontology:cs_axiom_grounding('c86e5292-0c1a-47ea-bd50-751e0508489f', installed_base_authority_is_binding, conventional).
narrative_ontology:cs_reference_frame('c86e5292-0c1a-47ea-bd50-751e0508489f', mechanical_infrastructure_preservation).
narrative_ontology:cs_drift_state('c86e5292-0c1a-47ea-bd50-751e0508489f', contemporary_software_input_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c86e5292-0c1a-47ea-bd50-751e0508489f', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, typing_training_institutions).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, professional_typists).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, alternative_layout_adopters).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_users).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, ergonomic_input_innovators).
narrative_ontology:constraint_vindicates(qwerty_persistence__incumbent_preservation_reading, path_dependence_theory).
narrative_ontology:constraint_vindicates(qwerty_persistence__incumbent_preservation_reading, sunk_cost_defense_rationality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce keyboards and input devices optimized for QWERTY tooling and supply chains. Bear massive retooling costs if the standard shifts. Actively resist alternative layouts by declining to manufacture them at scale, citing lack of demand while protecting existing capital investments and inventory.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, keyboard_manufacturers, beneficiary,
    organized, generational, constrained, global).

% Operate certification and curriculum systems built around QWERTY touch-typing. Revenue and instructor expertise depend on QWERTY remaining the professional standard. A layout shift would obsolete teaching materials and erode the value of their credentials.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, typing_training_institutions, beneficiary,
    moderate, biographical, constrained, global).

% Have invested thousands of hours achieving speed and accuracy on QWERTY. Professional identity and labor-market value are fused with QWERTY proficiency. Face high retraining costs and identity loss if the standard changes; many actively defend QWERTY as ergonomically optimal despite evidence.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, professional_typists, beneficiary,
    organized, biographical, identity_locked, global).

% Have switched to Dvorak, Colemak, or other layouts for ergonomics or speed. Face compatibility friction on shared devices, software that assumes QWERTY defaults, and professional stigma. Their adoption is structurally penalized by the incumbent standard's network effects.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, alternative_layout_adopters, payer,
    moderate, biographical, constrained, global).

% Bear the cumulative efficiency cost of a layout designed to prevent mechanical typewriter jamming rather than optimize digital input. Locked in by market availability and network effects; alternatives exist but require unsupported self-initiative to adopt.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_users, payer,
    moderate, biographical, constrained, global).

% Design and manufacture alternative keyboards, ergonomic layouts, and input methods. Blocked from mainstream market access by OEM refusal to produce alternative-layout hardware and by operating-system QWERTY defaults. Their innovation is suppressed by the installed base.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, ergonomic_input_innovators, payer,
    moderate, biographical, constrained, global).

% Study path dependence and standard lock-in. Document the divergence between QWERTY's technical optimality and its market persistence. Neither collect from nor pay into the constraint; provide external analysis of its extractive structure.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, standards_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence__incumbent_preservation_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single interoperable keyboard layout standard enabling typists to operate any machine without relearning, and enabling manufacturers to produce one hardware and software configuration for global distribution.
% TRANSFER_FUNCTION: Transfers the cost of suboptimal input and blocked innovation from users and alternative-input innovators to incumbent manufacturers and training institutions, in the form of protected capital investments, continued curriculum relevance, and preserved skill premiums.
% ABSENT_VOICES: Ergonomic researchers demonstrating superior layouts, alternative-keyboard manufacturers excluded from OEM partnerships, and developing-world users who bear the efficiency penalty most acutely are not represented in standards-setting or manufacturing decisions; their exclusion is maintained by the incumbent beneficiary coalition.
% DISAPPEARANCE_RATIONALE: If QWERTY disappeared overnight and no incumbent defended it, manufacturers would retool within product cycles, training institutions would pivot curricula, and after a transitional relearning period global typing efficiency and ergonomics would improve. The existing capital stock and skill investments would depreciate rapidly, rearranging the input-device economy.
% FOUNDING_PROBLEM: In the mechanical typewriter era, incompatible keyboard layouts proliferated, creating genuine coordination problems for typist labor mobility, machine interoperability, and manufacturer scale economies.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and ergonomics researchers outside the beneficiary set attest the original mechanical-jamming problem is technologically obsolete and the coordination rationale has expired; incumbent manufacturers and training institutions attest it remains necessary for interoperability. Independent academic consensus from outside the benefiting parties supports the dead assessment.
narrative_ontology:disappearance_verdict(qwerty_persistence__incumbent_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__incumbent_preservation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__incumbent_preservation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence__incumbent_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__incumbent_preservation_reading, 0.7, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence__incumbent_preservation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence__incumbent_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.70) because the constraint extracts cumulative typing efficiency and ergonomic adaptation from all users to protect incumbent capital. Suppression is higher still (0.82) because persistence requires active suppression of alternative layouts through manufacturing omissions, curriculum lock-in, and operating-system defaults. Theater ratio is substantial (0.55) because much incumbent defense is performativeâclaiming QWERTY is optimal or natural when empirical evidence shows alternatives are superior. Accessibility collapse is moderate-high (0.65) because while alternatives technically exist, they are practically inaccessible due to network effects and hardware absence. Resistance is moderate-low (0.35) because alternative adopters are diffuse and lack organized leverage against incumbents.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (manufacturers, trainers, skilled typists) experience the constraint as legitimate coordination protecting their investments. The payer seats (alternative adopters, efficiency-seekers, innovators) experience the same structure as extractive lock-in. The engine computes this divergence from structural data: identical constraint, opposite directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent beneficiaries have directionality near the beneficiary end because the constraint subsidizes their capital and skill investments. Victim seats have directionality near the target end because they bear the efficiency penalty and market exclusion. The professional_typist seat is identity_locked, amplifying effective extraction through identity fusionâits defense of QWERTY is not merely financial but constitutive of professional selfhood.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâmechanical typewriter coordinationâis technologically dead. The constraint persists beyond its functional need, but unlike a pure piton it still delivers genuine coordination value (interoperability). Because concentrated beneficiaries actively defend it and identifiable victims bear its costs, it is tangled rope rather than rope (which would lack victims) or snare (which would lack genuine coordination residue). The mandatrophy is unresolved: the arrangement continues because beneficiaries defend it, not because the founding problem is live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is QWERTY persistence explained primarily by active incumbent defense (this reading) or by failed coordination among alternatives (the sibling lapsed_alternatives_reading)?',
    'Historical analysis of incumbent lobbying, patent behavior, OEM exclusivity agreements, and training-curriculum decisions versus pure adoption-externality models.',
    'If incumbent defense is primary, classification as tangled_rope holds; if pure coordination lapse, classification shifts toward rope with lower extraction and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Structural ambiguity between active defense and coordination failure as explanations for standard persistence.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the persistence of QWERTY driven by structural suppression of alternatives (OEMs refusing to manufacture alternative-layout hardware) or by internalized user lock-in (self-reinforcing skill identity and social conformity)?',
    'Market analysis of alternative-keyboard availability versus user willingness-to-switch surveys and organizational adoption studies.',
    'Structural suppression suggests higher extractiveness and snare-like dynamics; internalized lock-in suggests lower suppression but higher identity-locked directionality for the typist seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Ambiguity between structural and internalized suppression mechanisms.').

omega_variable(
    residual_coordination_value,
    'Does QWERTY retain genuine coordination value (cross-device compatibility, shared public keyboards, immediate usability) that exceeds its extractive cost in the digital era?',
    'Natural experiments in organizations that switched to alternative layouts; measuring productivity, interoperability, and retraining costs against baseline QWERTY operation.',
    'If coordination value exceeds extraction, the constraint may be rope or scaffold rather than tangled rope; if extraction dominates, tangled_rope or snare classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_coordination_value, empirical, 'Uncertainty about whether genuine coordination value persists or has been fully eclipsed by extractive lock-in.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__incumbent_preservation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_inc_tr_t0, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(qwerty_inc_tr_t20, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(qwerty_inc_tr_t40, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(qwerty_inc_tr_t60, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 60, 0.46).
narrative_ontology:measurement(qwerty_inc_tr_t80, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 80, 0.52).
narrative_ontology:measurement(qwerty_inc_tr_t100, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(qwerty_inc_be_t0, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(qwerty_inc_be_t20, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(qwerty_inc_be_t40, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(qwerty_inc_be_t60, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement(qwerty_inc_be_t80, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 80, 0.67).
narrative_ontology:measurement(qwerty_inc_be_t100, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 100, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(qwerty_inc_su_t0, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(qwerty_inc_su_t20, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(qwerty_inc_su_t40, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(qwerty_inc_su_t60, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(qwerty_inc_su_t80, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 80, 0.78).
narrative_ontology:measurement(qwerty_inc_su_t100, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 100, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence__lapsed_alternatives_reading).

% DUAL FORMULATION NOTE:
% The qwerty_persistence kernel decomposes into two readings: incumbent_preservation_reading (active defense by beneficiaries, tangled rope) and lapsed_alternatives_reading (coordination lapse, rope with lower extraction). They share the same referent but author different structural data and epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
