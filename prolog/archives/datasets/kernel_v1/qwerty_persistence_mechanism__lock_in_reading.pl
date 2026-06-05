% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_lock_in, []).

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
 *   constraint_id: qwerty_persistence_mechanism__lock_in_reading
 *   human_readable: QWERTY Persistence Through Path-Dependent Coordination Failure
 *   domain: economic_history/technology_studies/path_dependence
 *
 * SUMMARY:
 *   The QWERTY keyboard layout persists despite documented technical
 *   inferiority through a path-dependent coordination failure mechanism. This
 *   is the lock-in reading of the contested QWERTY kernel: the constraint
 *   instantiates a market failure where users are individually rational to
 *   remain on QWERTY (switching cost is prohibitive) but collectively
 *   irrational (all users would benefit from coordinated migration to a
 *   superior layout). The lock-in reading treats QWERTY persistence as
 *   neither an intentional extraction mechanism nor a justified equilibrium,
 *   but as a coordination failure where the installed base size creates a
 *   barrier that prevents superior alternatives from reaching the critical
 *   mass needed to overcome switching costs. The reading rejects both the
 *   beneficiary_extraction reading (which attributes persistence to active
 *   maintenance by keyboard manufacturers or training establishments) and the
 *   naturalization reading (which argues QWERTY is adequate or that
 *   alternatives lack real superiority). Instead, lock-in reading posits that
 *   QWERTY's persistence is structural and path-dependent: once QWERTY
 *   achieved early dominance (through historical accident, not merit),
 *   subsequent adoption decisions were rational given the existing user base,
 *   producing a stable but suboptimal equilibrium. The constraint exhibits
 *   Tangled Rope characteristics (genuine coordination function exists
 *   alongside real extraction costs) from most perspectives, while the
 *   powerless user experiences it as Snare (pure lock-in with no escape
 *   option at individual level) and the digital transition window perspective
 *   sees it as Scaffold (temporary coordination failure susceptible to
 *   bypass, not overcome).
 *
 * KEY AGENTS:
 *   - Individual Users: Powerless/trapped (biographical horizon) — face prohibitive switching costs; locked in individually despite collective interest in migration
 *   - Installed QWERTY User Base: Institutional/arbitrage (immediate horizon) — beneficiary collective whose coordination creates the lock-in barrier for others
 *   - Alternative Keyboard Ecosystem: Moderate/constrained (biographical horizon) — coordinated communities around alternatives (Dvorak, Colemak) unable to reach critical mass due to network effects
 *   - Touch-Typing Profession: Organized/constrained (generational horizon) — standardized around QWERTY training; maintains through pedagogical inertia and certification
 *   - Digital Interface Manufacturers: Institutional/arbitrage (immediate horizon) — rationally default to QWERTY to maximize user compatibility; no incentive to coordinate breakpoint
 *   - Technology Standards Bodies: Organized/constrained (generational horizon) — encode QWERTY in interface standards; perpetuate through convention update cycles
 *   - Analytical Observer: Analytical/analytical (civilizational horizon) — risks naturalizing path-dependent equilibrium as inherent to keyboard technology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__lock_in_reading, 0.35).
domain_priors:suppression_score(qwerty_persistence_mechanism__lock_in_reading, 0.48).
domain_priors:theater_ratio(qwerty_persistence_mechanism__lock_in_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__lock_in_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__lock_in_reading, "QWERTY Persistence Through Path-Dependent Coordination Failure").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__lock_in_reading, "economic_history/technology_studies/path_dependence").

domain_priors:requires_active_enforcement(qwerty_persistence_mechanism__lock_in_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__lock_in_reading, 'c164a757-c421-45a5-9138-221d94ba9830').
narrative_ontology:cs_kernel_codification('c164a757-c421-45a5-9138-221d94ba9830', formalized).
narrative_ontology:cs_authority_grounding('c164a757-c421-45a5-9138-221d94ba9830', practice).
narrative_ontology:cs_reading_relation('c164a757-c421-45a5-9138-221d94ba9830', qwerty_persistence_mechanism__beneficiary_extraction_reading, influences).
narrative_ontology:cs_reading_relation('c164a757-c421-45a5-9138-221d94ba9830', qwerty_persistence_mechanism__naturalization_reading, coexists_with).
narrative_ontology:cs_axiom('c164a757-c421-45a5-9138-221d94ba9830', foundational, coordination_failure_primary_mechanism).
narrative_ontology:cs_axiom_status(coordination_failure_primary_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('c164a757-c421-45a5-9138-221d94ba9830', coordination_failure_primary_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('c164a757-c421-45a5-9138-221d94ba9830', foundational, technical_inferiority_established).
narrative_ontology:cs_axiom_status(technical_inferiority_established, holdable).
narrative_ontology:cs_axiom_grounding('c164a757-c421-45a5-9138-221d94ba9830', technical_inferiority_established, empirically_contingent).
narrative_ontology:cs_reference_frame('c164a757-c421-45a5-9138-221d94ba9830', pareto_efficient_keyboard_technology).
narrative_ontology:cs_drift_state('c164a757-c421-45a5-9138-221d94ba9830', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c164a757-c421-45a5-9138-221d94ba9830', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, installed_qwerty_user_base).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, potential_alternative_keyboard_adopters).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, social_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL USER (SNARE) — Faces prohibitive personal cost to learn alternative layouts; trapped by installed base coordination. No single user can switch profitably (coordination failure). Experiences pure extraction: forced to endure inferior technology due to collective lock-in.
constraint_indexing:constraint_classification(qwerty_persistence_mechanism__lock_in_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALTERNATIVE KEYBOARD ECOSYSTEM (TANGLED ROPE) — Constrained by network effects and user switching costs, but also coordinates alternative communities (Dvorak enthusiasts, specialized ergonomic users). Modest benefits from ecosystem coordination exist alongside significant extraction through inability to reach critical mass. Market segmentation enables some coordination function.
constraint_indexing:constraint_classification(qwerty_persistence_mechanism__lock_in_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTALLED QWERTY USER BASE (ROPE) — Beneficiary collective that experiences QWERTY as pure coordination: standard keyboard layout enables communication, training, portable skills. Network effects create positive feedback. No extraction from this perspective; benefits accumulate through coordination.
constraint_indexing:constraint_classification(qwerty_persistence_mechanism__lock_in_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TOUCH-TYPING PROFESSION (TANGLED ROPE) — Organizes around QWERTY training and certification. Genuine coordination function: standardized training enables portable skills. But also extraction: pedagogical inertia and certification lock-in prevent migration to ergonomically superior layouts even as evidence of RSI accumulates. Active enforcement via training curricula.
constraint_indexing:constraint_classification(qwerty_persistence_mechanism__lock_in_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: QWERTY INSTITUTIONAL STANDARD (PITON) — The layout persists as a ritualized standard through institutional inertia. Most enforcement is performative: computers ship with QWERTY not because it is optimal but because 'that is what computers have.' Theater rises over time as alternatives become technically superior but remain unavailable. Original function (solving typewriter jamming) is obsolete; standard persists through ceremony.
constraint_indexing:constraint_classification(qwerty_persistence_mechanism__lock_in_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DIGITAL TRANSITION WINDOW (SCAFFOLD) — The shift from mechanical to electronic keyboards created a brief coordination reset point where QWERTY could have been abandoned at lower switching cost. This window has partially closed but still exists (smartphone keyboards, specialized devices). The window is temporary — once mobile typing norms solidify around QWERTY, the next reset becomes a civilizational timescale event. Sunset clause: As hardware becomes more abstracted from physical layout (voice input, gesture interfaces), QWERTY's lock-in can be bypassed rather than overcome.
constraint_indexing:constraint_classification(qwerty_persistence_mechanism__lock_in_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — From a universal/civilizational perspective, QWERTY may appear to be an unchangeable fact of how keyboards are arranged, as inevitable as the structure of a typewriter. This perspective risks naturalizing a contingent coordination equilibrium as a law of technology. The engine's false summit detector will identify this as potential naturalization of a market-contingent institutional arrangement.
constraint_indexing:constraint_classification(qwerty_persistence_mechanism__lock_in_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__lock_in_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(qwerty_persistence_mechanism__lock_in_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(qwerty_persistence_mechanism__lock_in_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(qwerty_persistence_mechanism__lock_in_reading, TR),
    TR >= 0.70.

:- end_tests(qwerty_persistence_mechanism__lock_in_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. QWERTY lock-in extracts efficiency loss — users and societies bear the cost of suboptimal technology while no individual beneficiary captures surplus. This is different from extraction where A extracts from B; here the extraction is impersonal (collective suboptimality). The value is calibrated to the empirical literature on typing efficiency gains (estimated 5-15% speed improvement for alternatives) and switching cost burden (weeks to months of learning time). Not as severe as pure snare extraction because the coordination function is genuine (QWERTY does enable communication); not as mild as pure rope because the network effect is binding at the individual level. Suppression (0.48): Moderate-high. Significant barriers exist: switching costs (learning time, retrained muscle memory), infrastructure standardization (computers ship with QWERTY), network effects (training materials, available typists), and organizational defaults (keyboards are set to QWERTY by default). Suppression is not total because alternatives are technically possible and some communities (programmers, enthusiasts) have escaped or migrated. Theater ratio (0.55): Moderate-high. Original function (preventing typewriter jams) became obsolete once mechanical constraints were eliminated. Modern QWERTY persistence is substantially performative — justified by appeals to standardization, training investment, and compatibility rather than technical necessity. The theater ratio increases over time as alternatives become technically superior but remain unavailable, making the standard's rationale increasingly ceremonial.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between lock-in reading and alternative readings is fundamental. The beneficiary_extraction_reading attributes QWERTY's persistence to intentional coordination failure by incumbents protecting market position — this reading emphasizes malign agency and places extraction in deliberate choices by manufacturers and training establishments. The naturalization_reading attributes persistence to technical adequacy or fair competition — this reading emphasizes benign outcomes and treats QWERTY as justified by market process. The lock-in reading occupies the middle: rational decentralized choices aggregate into a coordination failure that is neither intentional extraction nor justified market outcome. From the lock-in perspective, QWERTY persists because each user is rational to stay (given others stay) but collectively irrational to remain (all would benefit from coordinated migration). The gap reveals itself in the empirical predictions: lock-in reading predicts that QWERTY persists despite technical inferiority and that users would vote for migration if coordination were costless; beneficiary_extraction reading predicts that removing incumbents' interest would enable rapid migration; naturalization reading predicts that alternatives would not outperform QWERTY in fair competition. These predictions are measurably different and refutable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from the structural relationship of each agent to the lock-in constraint. Individual users (victims, trapped) experience maximum directionality d → 1.0, as they bear the full cost of being locked in with no escape. The installed QWERTY user base (beneficiaries, arbitrage) experience low directionality d ≈ 0.1, as they benefit from network effects without bearing the cost of the lock-in. The alternative ecosystem (victims, constrained) experience high directionality d ≈ 0.75, as they bear the cost of network effects preventing critical mass adoption, with some constrained ability to exit (forming niche communities). The touch-typing profession (mixed, constrained) experiences mid-range directionality d ≈ 0.5, as they both benefit from standardization (portable skills, clear training path) and enforce it (pedagogy inertia). The digital interface manufacturers (beneficiaries, arbitrage) experience low directionality d ≈ 0.15, as they benefit from standardization reducing compatibility overhead. These directionality values map to f(d) via the sigmoid, producing the effective extractiveness chi experienced from each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The lock-in reading resolves mandatrophy by positioning QWERTY as a Tangled Rope from most perspectives — it has genuine coordination function (standardization enables portable skills, communication) alongside real extraction (lock-in prevents superior technology adoption). The snare perspective is the individual user's structural reality: they experience pure lock-in with no way out. The scaffold perspective is valid at technological discontinuities (mechanical to electric, desktop to mobile) where switching costs temporarily decrease and alternatives become feasible. The piton perspective is the ceremonial standard that persists through institutional inertia. The rope perspective is the installed base's genuine experience of coordination benefits. The mountain perspective risks false naturalization. All six types are defensible readings of the same structural data, showing that mandatrophy is resolved through perspectival decomposition: there is no single 'correct' type, only a presheaf of legitimate types indexed by observer position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_lock_in_boundary,
    'Is QWERTY persistence a coordination equilibrium (mutually beneficial, stable through rational choice) or a lock-in trap (collectively suboptimal, stable through path dependence)?',
    'Welfare analysis: comparison of individual switching costs vs collective efficiency gain if all users migrated to superior layout. If gain > cost, persistence is lock-in (not coordination). If cost > gain, persistence is coordination.',
    'If lock-in: QWERTY persistence is a market failure requiring collective action (policy, standards reset, or bypass). If coordination: QWERTY persistence is rational. Classification shifts from Tangled Rope/Snare toward Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_vs_lock_in_boundary, empirical, 'Boundary between coordination equilibrium and path-dependent lock-in').

omega_variable(
    superior_layout_empirical_status,
    'Is the technical superiority of alternatives (Dvorak, Colemak, etc.) established, or contested?',
    'Meta-analysis of typing efficiency studies; correlation with ergonomic injury rates; blinded user preference tests on novel layouts.',
    'If superiority established: this reading is confirmed (lock-in of inferior standard). If contested: the naturalization reading gains credibility (QWERTY may be adequate, alternatives no better).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(superior_layout_empirical_status, empirical, 'Whether alternative keyboard layouts are demonstrably superior to QWERTY').

omega_variable(
    reading_boundary_intentional_vs_structural,
    'Does this lock-in reading require intentional coordination failure, or is structural path dependence sufficient?',
    'Historical counterfactual: If manufacturers and users had passively done nothing, would QWERTY still persist? If yes, persistence is structural lock-in (this reading). If no, persistence required active maintenance (beneficiary_extraction_reading applies).',
    'This omega delineates this reading from the beneficiary_extraction_reading. If active maintenance was necessary, both readings may apply (nested). If structural path dependence alone suffices, this reading is sufficient; beneficiary_extraction_reading overstates agency.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_intentional_vs_structural, conceptual, 'Whether lock-in requires intentional coordination failure or emerges structurally from path dependence').

omega_variable(
    lock_in_escape_cost_trajectory,
    'Has the switching cost (cost to users + infrastructure to switch to alternative) increased, decreased, or remained stable over time?',
    'Cost analysis: training burden for new layout, software keyboard remapping, hardware standardization across devices. Measure cost at historical inflection points (mechanical to electric, desktop to mobile).',
    'If increasing: lock-in strengthens over time (more severe constraint). If decreasing: lock-in weakens at technological discontinuities (scaffold perspective valid). Trajectory determines whether the constraint is amplifying or eroding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lock_in_escape_cost_trajectory, empirical, 'Whether switching cost to escape QWERTY is increasing or decreasing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__lock_in_reading, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_lock_theater_1880, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(qwerty_lock_theater_1930, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement(qwerty_lock_theater_1980, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 100, 0.55).
narrative_ontology:measurement(qwerty_lock_theater_2020, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 140, 0.62).

% Extraction over time
narrative_ontology:measurement(qwerty_lock_extractiveness_1880, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(qwerty_lock_extractiveness_1930, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement(qwerty_lock_extractiveness_1980, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 100, 0.35).
narrative_ontology:measurement(qwerty_lock_extractiveness_2020, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 140, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(qwerty_lock_suppression_1880, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(qwerty_lock_suppression_1930, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 50, 0.45).
narrative_ontology:measurement(qwerty_lock_suppression_1980, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 100, 0.48).
narrative_ontology:measurement(qwerty_lock_suppression_2020, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 140, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__lock_in_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism__beneficiary_extraction_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism__naturalization_reading).

% DUAL FORMULATION NOTE:
% The QWERTY kernel has three structural readings, each a distinct constraint story with the same label 'QWERTY persistence'. The lock-in_reading (this story) treats persistence as a market failure (coordination failure, path dependence). The beneficiary_extraction_reading treats persistence as intentional incumbency protection. The naturalization_reading treats persistence as justified (QWERTY adequate, alternatives not genuinely superior). These are not one constraint viewed from three angles — they have different epsilon values, different victim/beneficiary structures, and different policy implications. Each reading is a separate constraint linked via network.affects_constraints. The lock-in reading constrains both siblings by establishing that if QWERTY persists despite technical inferiority through path dependence, then (a) the beneficiary_extraction reading must explain why intent is necessary given structural forces suffice, and (b) the naturalization reading must demonstrate what technical advantage counterbalances the documented efficiency loss of QWERTY.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
