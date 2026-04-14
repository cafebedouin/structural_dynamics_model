% ============================================================================
% CONSTRAINT STORY: software_keyboard_driver_standardization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_keyboard_driver_standardization, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: software_keyboard_driver_standardization
 *   human_readable: Software Keyboard Driver Standardization and Vendor Lock-in
 *   domain: technology/software_infrastructure
 *
 * SUMMARY:
 *   Software keyboard driver standardization creates a structural constraint
 *   between hardware vendors seeking platform differentiation, operating
 *   system manufacturers managing driver ecosystems, independent keyboard
 *   makers navigating multi-platform support, and end users (especially those
 *   with accessibility needs) dependent on specialized input functionality.
 *   The constraint exhibits hybrid coordination and extraction: genuine
 *   technical coordination problems (managing diverse keyboard hardware
 *   across OS platforms) coexist with vendor incentives to maintain
 *   proprietary driver stacks that create switching costs and lock-in. The
 *   extractiveness metric (0.32) reflects that standardized keyboard input
 *   protocols (HID, wayland) have partially solved the coordination problem,
 *   but proprietary extensions and platform-specific driver frameworks
 *   persist, creating residual extraction. The suppression metric (0.38)
 *   reflects moderate barriers to exit: switching OS or keyboard hardware
 *   carries significant costs for users dependent on specific driver
 *   ecosystems, but not insurmountable barriers for most users. The theater
 *   ratio (0.44) indicates that proprietary keyboard drivers increasingly
 *   deliver functionality available through open standards, suggesting
 *   degradation of the original justification.
 *
 * KEY AGENTS:
 *   - Operating System Manufacturers (Microsoft, Apple, Linux distributions): Primary beneficiaries (institutional/arbitrage) — control driver ecosystems and proprietary extensions
 *   - Hardware Vendors (keyboard manufacturers): Secondary beneficiaries (institutional/arbitrage) — benefit from differentiation through proprietary drivers but also constrained by multi-platform support costs
 *   - End Users with Accessibility Needs: Primary victims (powerless/trapped) — dependent on proprietary drivers for specialized input methods; cannot exit without losing functionality
 *   - Independent Keyboard Hardware Makers: Secondary victims (moderate/constrained) — face fragmented driver development burden across platforms; can exit by consolidating to single OS but at market cost
 *   - Open-Source Standards Coalition: Organized agents building alternative (organized/mobile) — USB HID, wayland input protocols, Linux input subsystem represent coordinated path to standardization
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing vendor lock-in as technical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_keyboard_driver_standardization, 0.32).
domain_priors:suppression_score(software_keyboard_driver_standardization, 0.38).
domain_priors:theater_ratio(software_keyboard_driver_standardization, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_keyboard_driver_standardization, extractiveness, 0.32).
narrative_ontology:constraint_metric(software_keyboard_driver_standardization, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(software_keyboard_driver_standardization, theater_ratio, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_keyboard_driver_standardization, tangled_rope).
narrative_ontology:human_readable(software_keyboard_driver_standardization, "Software Keyboard Driver Standardization and Vendor Lock-in").
narrative_ontology:topic_domain(software_keyboard_driver_standardization, "technology/software_infrastructure").

domain_priors:requires_active_enforcement(software_keyboard_driver_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_keyboard_driver_standardization, hardware_vendors).
narrative_ontology:constraint_beneficiary(software_keyboard_driver_standardization, operating_system_manufacturers).
narrative_ontology:constraint_victim(software_keyboard_driver_standardization, independent_developers).
narrative_ontology:constraint_victim(software_keyboard_driver_standardization, end_users_with_accessibility_needs).
narrative_ontology:constraint_victim(software_keyboard_driver_standardization, keyboard_hardware_makers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USER WITH ACCESSIBILITY NEEDS (SNARE) — Trapped by vendor-specific keyboard driver dependencies. Users requiring specialized input methods (eye tracking, speech-to-text integration, adaptive switches) cannot switch OS or keyboard manufacturers without losing critical functionality. No exit options exist within biographical timeframe. Extraction is maximal: accessibility accommodations are held hostage to driver standardization failures.
constraint_indexing:constraint_classification(software_keyboard_driver_standardization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDEPENDENT KEYBOARD HARDWARE MAKER (TANGLED ROPE) — Faces high costs to support multiple OS platforms (Windows, macOS, Linux) with separate driver codebases. Benefits from standardized driver APIs (genuine coordination function) while simultaneously extracted from through vendor lock-in incentives that pressure consolidation. Can theoretically exit by focusing on single platform, but at severe market cost. Experiences both extraction and coordination.
constraint_indexing:constraint_classification(software_keyboard_driver_standardization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: OPERATING SYSTEM MANUFACTURER (ROPE) — Benefits from vendor lock-in through proprietary driver frameworks. However, experiences the constraint as genuine coordination: standardizing keyboard input APIs reduces support burden and expands hardware ecosystem. Exit option (arbitrage) is available through proprietary extension of driver API, but coordination incentives are real. Classification as Rope reflects that extraction mechanisms exist but are balanced by coordination benefits.
constraint_indexing:constraint_classification(software_keyboard_driver_standardization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN-SOURCE STANDARDS COALITION (SCAFFOLD) — Organizations (USB Implementers Forum, HID standardization bodies, Linux kernel community) are building interoperable keyboard driver standards with genuine sunset architecture. HID (Human Interface Device) protocol and modern wayland/wayland input protocols represent progress toward manufacturer-neutral standardization. High mobility for coalition members; low effective extraction because sunset clause is built in (standardization eventually removes the bottleneck). Theater remains moderate because standardization process itself involves performative governance.
constraint_indexing:constraint_classification(software_keyboard_driver_standardization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY KEYBOARD DRIVER ECOSYSTEM (PITON) — Proprietary keyboard drivers bundled with hardware represent degraded institutional structure. Original function (enabling hardware-specific features like macro keys, backlighting) persists as theater: most users do not access proprietary features, yet drivers remain vendor-locked. Theater ratio is elevated (0.44) reflecting that much driver complexity is now unnecessary. Maintained through inertia rather than function.
constraint_indexing:constraint_classification(software_keyboard_driver_standardization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, keyboard driver standardization might appear immutable due to hardware diversity: different keyboard architectures, input protocols, and OS-level expectations create irreducible complexity. However, this perspective generates a false summit. The structural data shows vendor lock-in is contingent institutional policy, not natural law. HID demonstrates that cross-platform driver unification is technically feasible.
constraint_indexing:constraint_classification(software_keyboard_driver_standardization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_keyboard_driver_standardization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(software_keyboard_driver_standardization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(software_keyboard_driver_standardization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(software_keyboard_driver_standardization, TR),
    TR >= 0.70.

:- end_tests(software_keyboard_driver_standardization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate-low. The constraint exhibits extraction through vendor lock-in mechanisms, but technical solutions (HID standard, open input protocols) have substantially solved the coordination problem. Remaining extractiveness reflects proprietary extensions and platform-specific optimizations that differentiate keyboards post-standardization. The value increased from 0.18 to 0.32 over the measurement interval, suggesting that initial standardization gains (HID adoption 2000-2005) were followed by renewed proprietary layering (macOS Touch Bar integration, Windows-specific driver features 2010+). Suppression (0.38): Moderate. Users have moderate barriers to exit: switching OS platforms is costly but possible; purchasing keyboards compatible with open standards is feasible but requires technical literacy. Accessibility users face higher suppression (closer to trapped) because proprietary driver ecosystems often integrate accessibility features more tightly than open standards. Theater ratio (0.44): Moderate-high and rising. Proprietary keyboard drivers have evolved from purely functional (hardware communication) to increasingly performative: macro reconfiguration software, lighting customization, and profile management are theater layers that exist primarily for differentiation. The theater has increased from 0.28 to 0.44 because early driver ecosystems handled core input; modern drivers add feature management that overlaps with OS-level functionality. The upward trajectory suggests Goodhart drift: vendors layering cosmetic complexity to justify driver lock-in.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap centers on whether keyboard driver standardization is a coordination mechanism (Rope, Scaffold) or an extraction mechanism (Snare, Tangled Rope). OS manufacturers and their aligned vendors experience the constraint as coordination — standardized HID protocols reduce complexity and enable broader hardware compatibility (Rope perspective). The open-source coalition experiences standardization as progress on a solvable problem with a sunset clause (Scaffold perspective). Independent hardware makers and accessibility users experience mixed coordination and extraction (Tangled Rope) or pure extraction (Snare). The analytical observer risks naturalizing the vendor lock-in as a technical necessity (false Mountain). The perspectival gap reveals that whether standardization is perceived as coordination or extraction depends entirely on whether the observer benefits from proprietary extension incentives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation for each power atom: OS manufacturers (institutional/arbitrage) have d ≈ 0.15 (strong beneficiaries with exit optionality through proprietary extension — low d). Independent hardware makers (moderate/constrained) have d ≈ 0.65 (moderate victims facing real switching costs but with some agency to choose platform focus — moderate d). Accessibility users (powerless/trapped) have d ≈ 0.95 (full victims with no exit — high d). Open-source coalition (organized/mobile) has d ≈ 0.40 (mixed but mobile through coalition agency — lower-moderate d). The OS manufacturers' beneficiary status derives from their control of driver APIs and ability to extend proprietary standards. The hardware makers' victim status derives from fragmented driver development costs and inability to coordinate across platforms. The end users' victim status derives from their dependence on proprietary features for accessibility.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that keyboard driver standardization is genuinely hybrid: Open standards (HID, wayland) solve real coordination problems (cross-platform input handling). Proprietary extensions simultaneously extract value through switching costs and lock-in. The constraint is Tangled Rope because both functions exist structurally. If vendors abandoned proprietary driver layering and committed fully to open standards, the constraint would reclassify as Rope (pure coordination). If vendors eliminated open standard support and forced proprietary drivers, it would reclassify as Snare (pure extraction). The current state reflects active maintenance of both mechanisms — standardization solves enough coordination that the constraint remains legitimate; proprietary extensions extract enough value that beneficiaries maintain them. The theater ratio rise (0.28 → 0.44) signals that the extraction function is growing relative to coordination — Goodhart drift toward Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proprietary_feature_necessity,
    'How many keyboard features actually require proprietary drivers versus how many could be standardized through HID or generic protocols?',
    'Technical audit of proprietary driver feature sets; classification of essential vs cosmetic functionality; measurement of user adoption of proprietary-only features',
    'If cosmetic features dominate proprietary drivers: the constraint is primarily extractive (suppression ratio exceeds genuine hardware coordination needs). If essential features require proprietary drivers: extraction is justified by legitimate platform differentiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proprietary_feature_necessity, empirical, 'Whether proprietary keyboard features justify standardization barriers').

omega_variable(
    standardization_network_effect_threshold,
    'What adoption threshold of open driver standards (HID, wayland input protocols) triggers platform manufacturer commitment to standardization?',
    'Market analysis of driver adoption; manufacturer statements of standardization roadmaps; correlation between user demand for standardized drivers and platform roadmap changes',
    'If threshold is low (< 20% adoption): scaffold sunset is likely within generational timeframe. If threshold is high (> 50% adoption): sunset is delayed and extraction persists longer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standardization_network_effect_threshold, empirical, 'Critical adoption threshold for standardization commitment').

omega_variable(
    accessibility_driver_coupling,
    'What proportion of accessibility keyboard driver features are proprietary-locked versus available through open standards?',
    'Feature inventory of accessibility keyboard software; correlation between proprietary driver requirements and accessibility accommodation types; cost analysis of accessibility development with and without proprietary driver access',
    'If accessibility features are disproportionately proprietary: the snare classification is amplified and the constraint becomes human rights relevant. If accessibility features are mostly available through open standards: the snare applies primarily to cosmetic use cases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accessibility_driver_coupling, empirical, 'Coupling of accessibility features to proprietary keyboard drivers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_keyboard_driver_standardization, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kbdrv_tr_t0, software_keyboard_driver_standardization, theater_ratio, 0, 0.28).
narrative_ontology:measurement(kbdrv_tr_t5, software_keyboard_driver_standardization, theater_ratio, 5, 0.36).
narrative_ontology:measurement(kbdrv_tr_t10, software_keyboard_driver_standardization, theater_ratio, 10, 0.44).

% Extraction over time
narrative_ontology:measurement(kbdrv_be_t0, software_keyboard_driver_standardization, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(kbdrv_be_t5, software_keyboard_driver_standardization, base_extractiveness, 5, 0.25).
narrative_ontology:measurement(kbdrv_be_t10, software_keyboard_driver_standardization, base_extractiveness, 10, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_keyboard_driver_standardization, information_standard).
narrative_ontology:affects_constraint(software_keyboard_driver_standardization, operating_system_moat_protection).
narrative_ontology:affects_constraint(software_keyboard_driver_standardization, hardware_manufacturer_lock_in).

% DUAL FORMULATION NOTE:
% Software keyboard driver standardization represents one constraint family decomposable into two structurally distinct claims: (1) Technical standardization of keyboard input protocols (HID, open standards) — low extractiveness, primarily coordination; (2) Vendor incentives to maintain proprietary driver ecosystems layered on top of standards — higher extractiveness, primarily extraction. This story captures the hybrid state where both exist simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
