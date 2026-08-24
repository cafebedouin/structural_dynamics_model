% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__exogenous_imposition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__exogenous_imposition_reading, []).

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
 *   constraint_id: state_commitment_installation_mechanism__exogenous_imposition_reading
 *   human_readable: Top-Down Commitment Installation by Transformation Mandate
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint models the mechanism by which a state holding a
 *   transformation mandate installs a new commitment system (religion,
 *   ideology, legal code) from above, compelling adherence through decree and
 *   suppression of alternatives. The state and its new institutional partners
 *   are the beneficiaries; the subject population and displaced commitment
 *   communities are the victims. The mechanism combines genuine coordination
 *   (unifying administration) with asymmetric extraction (legitimacy and
 *   resources transferred upward). It is the exogenous_imposition_reading of
 *   the contested kernel state_commitment_installation_mechanism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.78).
domain_priors:suppression_score(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.85).
domain_priors:theater_ratio(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__exogenous_imposition_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__exogenous_imposition_reading, "Top-Down Commitment Installation by Transformation Mandate").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__exogenous_imposition_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__exogenous_imposition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__exogenous_imposition_reading, '855c99cb-f90a-4584-b4da-883721ac0c80').
narrative_ontology:cs_kernel_codification('855c99cb-f90a-4584-b4da-883721ac0c80', formalized).
narrative_ontology:cs_authority_grounding('855c99cb-f90a-4584-b4da-883721ac0c80', extraction).
narrative_ontology:cs_interpretation_layer_present('855c99cb-f90a-4584-b4da-883721ac0c80').
narrative_ontology:cs_reading_relation('855c99cb-f90a-4584-b4da-883721ac0c80', state_commitment_installation_mechanism__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('855c99cb-f90a-4584-b4da-883721ac0c80', state_commitment_installation_mechanism__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('855c99cb-f90a-4584-b4da-883721ac0c80', foundational, state_mandate_legitimizes_installation).
narrative_ontology:cs_axiom_status(state_mandate_legitimizes_installation, holdable).
narrative_ontology:cs_axiom_grounding('855c99cb-f90a-4584-b4da-883721ac0c80', state_mandate_legitimizes_installation, conventional).
narrative_ontology:cs_axiom('855c99cb-f90a-4584-b4da-883721ac0c80', secondary, top_down_unification_necessary_for_order).
narrative_ontology:cs_axiom_status(top_down_unification_necessary_for_order, holdable).
narrative_ontology:cs_axiom_grounding('855c99cb-f90a-4584-b4da-883721ac0c80', top_down_unification_necessary_for_order, instrumental).
narrative_ontology:cs_reference_frame('855c99cb-f90a-4584-b4da-883721ac0c80', imperial_mandate_legitimacy).
narrative_ontology:cs_drift_state('855c99cb-f90a-4584-b4da-883721ac0c80', post_axial_age_universalism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('855c99cb-f90a-4584-b4da-883721ac0c80', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, state_authority).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, new_commitment_institutions).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, subject_population).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, prior_commitment_adherents).
narrative_ontology:constraint_vindicates(state_commitment_installation_mechanism__exogenous_imposition_reading, transformation_mandate_doctrine).
narrative_ontology:constraint_vindicates(state_commitment_installation_mechanism__exogenous_imposition_reading, state_legitimacy_through_unification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the transformation mandate and issues decrees installing the new commitment system. Controls the enforcement apparatus (legal, military, administrative) that suppresses prior commitments and compels adherence. Gains legitimacy, administrative coherence, and control over cultural reproduction. Can exit by modifying or abandoning the mandate, but doing so risks the regime's founding justification.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, state_authority, agenda_setter,
    institutional, generational, arbitrage, universal).

% Receive state patronage, legal monopoly, resources, and authority over ritual/education/law in exchange for legitimizing the regime. Their clergy/bureaucrats administer the new commitment. Exit is constrained — they are institutionally fused to the state mandate; breaking with it means losing their corporate existence.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, new_commitment_institutions, beneficiary,
    organized, generational, constrained, universal).

% Subjected to compulsory conversion, ritual participation, and cultural reorientation. Prior identities, practices, and communal structures are suppressed or criminalized. Resistance carries severe penalties (exile, execution, property seizure). Exit is identity-locked — the new commitment becomes the framework through which selfhood, kinship, and social belonging are expressed; leaving it means social death.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, subject_population, payer,
    powerless, biographical, identity_locked, universal).

% Leaders and communities of the displaced commitment system (priesthoods, guilds, tribal elders, rival ideologues). Their authority, assets, and social standing are stripped. They face co-option, exile, or elimination. Exit is trapped — the structural space for their commitment has been legally and violently closed; underground persistence is possible but costly and shrinking.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, prior_commitment_adherents, payer,
    moderate, biographical, trapped, universal).

% Networks that reject the installed commitment (crypto-practitioners, millenarian movements, regional autonomists). They are excluded from the legitimate public sphere and must operate clandestinely. Their exit is constrained — they cannot openly advocate; they survive in margins or flee to frontier zones.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, resistance_movements, excluded,
    organized, biographical, constrained, universal).

% Scholars who compare cases of exogenous installation (Constantinian Christianization, Meiji State Shinto, Soviet scientific atheism, Kemalist secularization, post-colonial state Islamization). They see the full structural pattern: state as beneficiary, abrupt decree, suppression of alternatives, identity-locked compliance. Their seat is analytical — they neither collect nor pay.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, historical_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies a fragmented or conquered population under a single commitment framework, enabling state administration, law, taxation, and military mobilization at scale without negotiating with myriad local cults or traditions.
% TRANSFER_FUNCTION: Moves legitimacy, compliance, cultural capital, and material resources (temple lands, tithes, educational control) from subject population and prior commitment institutions to state authority and new commitment institutions, via compulsory adherence and suppression of alternatives.
% ABSENT_VOICES: Prior commitment adherents, organic cultural leaders, and communal authorities who would object to forced displacement of their traditions but are suppressed, co-opted, or physically eliminated during installation. Their absence is structural — the mechanism's speed and coercion prevent their participation.
% DISAPPEARANCE_RATIONALE: If top-down installation vanished overnight, prior commitments would resurface and compete, state legitimacy would lose its primary cultural anchor, and social coordination would fragment into plural commitment systems — the polity would either renegotiate a new settlement or fracture.
% FOUNDING_PROBLEM: How to rapidly legitimize a new ruling order or unify a fragmented polity after conquest, revolution, regime change, or civilizational rupture, when organic consensus is absent or too slow.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by comparative historical analysis of post-conquest and post-revolutionary states (Constantinian Christianization, Meiji Restoration, Soviet atheization, Kemalist secularization) — sources outside state beneficiaries confirm the pattern. The state claims the problem is live (ongoing unity threats); historians argue it is often a pretext for consolidation.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__exogenous_imposition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__exogenous_imposition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__exogenous_imposition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__exogenous_imposition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_commitment_installation_mechanism__exogenous_imposition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the state extracts compliance, cultural capital, and material resources while suppressing exits. Suppression is very high (0.85) — the mechanism depends on active enforcement: criminalizing prior practices, controlling education, monopolizing ritual. Theater ratio is moderate (0.45) and rising over time — early installation is brutal and functional; later phases emphasize performative legitimacy (civic rituals, monumental architecture, pedagogical indoctrination). Accessibility collapse is high (0.75) — once installed, the new commitment becomes the only viable framework for public life. Resistance is high (0.72) — the base resists through crypto-practice, flight, rebellion, and millenarian movements.
 *
 * PERSPECTIVAL GAP:
 *   From the state's seat, the mechanism is genuine coordination — it solves the problem of governing diverse populations. From the subject population's seat, it is enforced extraction — their cultural autonomy is stripped for state convenience. From prior adherents' seat, it is existential displacement. The engine computes this divergence from the structural data; the claimed_type (tangled_rope) captures the hybrid reality.
 *
 * DIRECTIONALITY LOGIC:
 *   State authority sits at d≈0.1 (full beneficiary: mandate justifies its power, collects legitimacy rents). New commitment institutions sit at d≈0.2 (beneficiary: gain monopoly status, state patronage). Subject population sits at d≈0.95 (identity-locked target: bears costs, cannot exit without social death). Prior commitment adherents sit at d≈0.9 (trapped target: structural space closed). Resistance movements sit at d≈0.85 (constrained target: excluded from legitimacy, operate in margins). Historical analysts sit at d=0.5 (analytical seat).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rapid legitimization after rupture) may be live or dead depending on the case. Where the rupture is historical (ancient conquests), the problem is dead but the mechanism persists — mandatrophy unresolved. Where the rupture is ongoing (revolutionary states), the problem is contested. The analysis prevents mislabeling: the coordination function is real (unification enables administration), but the extraction is asymmetric and enforced — hence tangled_rope, not rope or snare alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the exogenous_imposition_reading a distinct constraint from its sibling readings, or a perspectival slice of a single mechanism?',
    'Compare ε values across readings: if exogenous installation shows systematically higher extractiveness and suppression than endogenous climb across comparable cases, they are distinct constraints. If ε overlaps, they are measurement perspectives on one mechanism.',
    'If distinct, each reading gets its own classification and temporal trajectory. If unified, the kernel is a single constraint with observer-dependent classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s declared readings are structurally distinct constraints or observer frames.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal penalties, military enforcement, institutional monopoly) or internalized (subjects come to believe the new commitment is natural/desirable)?',
    'Post-installation trajectory: if suppression persists at high levels centuries after installation, it is primarily structural. If suppression declines while compliance holds, internalization has occurred. Comparative cases: Meiji State Shinto (structural to internalized shift) vs. Soviet atheism (structural persistence).',
    'If internalized, effective suppression is higher than structural measures suggest — the target carries the constraint after formal enforcement relaxes. This affects θ (theater) trajectory and piton detection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in top-down commitment installation.').

omega_variable(
    coordination_extraction_boundary,
    'Is the administrative unification achieved by installation a genuine coordination benefit that could not be obtained otherwise, or is it a cover story for extraction?',
    'Counterfactual: in polities that did NOT install a single commitment (e.g., Ottoman millet system, early US disestablishment), did administrative coherence fail? If plural systems achieve comparable coordination, the unification claim is extractive cover.',
    'If genuine coordination, the mechanism is tangled_rope (both functions real). If cover, it is snare (coordination story is pretext). This is the core classification ambiguity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the coordination function of exogenous installation is structurally necessary or rhetorical.').

omega_variable(
    identity_lock_origin,
    'Does the subject population''s identity_locked exit arise from state enforcement (structural closure) or from the commitment''s own internal logic (theological/ideological totality)?',
    'Compare cases where the same commitment was installed exogenously vs. adopted endogenously. If identity lock is stronger in exogenous cases, it is enforcement-produced. If equal, it is commitment-internal.',
    'If enforcement-produced, the identity lock is an artifact of the constraint''s suppression — removing the constraint would restore exit options. If commitment-internal, the lock persists post-constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_origin, empirical, 'Origin of identity-locked exit in imposed commitment systems.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__exogenous_imposition_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(stat_tr_t20, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(stat_tr_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(stat_tr_t60, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 60, 0.48).
narrative_ontology:measurement(stat_tr_t80, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 80, 0.52).
narrative_ontology:measurement(stat_tr_t100, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(stat_be_t20, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(stat_be_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 40, 0.72).
narrative_ontology:measurement(stat_be_t60, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(stat_be_t80, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 80, 0.65).
narrative_ontology:measurement(stat_be_t100, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 100, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(stat_su_t20, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 20, 0.88).
narrative_ontology:measurement(stat_su_t40, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 40, 0.82).
narrative_ontology:measurement(stat_su_t60, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 60, 0.75).
narrative_ontology:measurement(stat_su_t80, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 80, 0.68).
narrative_ontology:measurement(stat_su_t100, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 100, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__exogenous_imposition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.12).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, state_legitimacy_mechanism).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, cultural_unification_policy).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, prior_commitment_suppression_regime).

% DUAL FORMULATION NOTE:
% This constraint is the exogenous_imposition_reading of the kernel state_commitment_installation_mechanism. The endogenous_climb_reading and hybrid_cascade_reading are sibling constraints. All three share the kernel but instantiate different structural claims: exogenous = state beneficiary, top-down, high suppression; endogenous = fringe actors beneficiary, bottom-up, low suppression; hybrid = state + fringe beneficiaries, cascade dynamics, moderate suppression.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_commitment_installation_mechanism__exogenous_imposition_reading, institutional, 0.1).
constraint_indexing:directionality_override(state_commitment_installation_mechanism__exogenous_imposition_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
