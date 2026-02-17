% ============================================================================
% CONSTRAINT STORY: smartphone_ubiquity
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_smartphone_ubiquity, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: smartphone_ubiquity
 * human_readable: The Smartphone Ubiquity Constraint
 * domain: technological/social/economic
 * * SUMMARY:
 * The smartphone has transitioned from a communication device to a "place within
 * which we live," functioning as a portable digital home that bridges physical
 * and digital realities. While it allows lower-income nations to bypass legacy
 * infrastructure (banks, desktops), it simultaneously extracts privacy and
 * creates a "shell" that can lead to social disconnect or exclusion.
 * * KEY AGENTS:
 * - Youth Under Ban: Subject (Powerless)
 * - Platform Providers & App Ecosystem: Beneficiary (Institutional)
 * - Privacy-Conscious Users: Victim (Powerless)
 * - Global User in Developing Nation: User (Moderate)
 * - Systems Auditor: Observer (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(smartphone_ubiquity, 0.75). % High extraction of user data and attention.
domain_priors:suppression_score(smartphone_ubiquity, 0.50).   % Ubiquity suppresses older infrastructure (desktops, physical banks).
domain_priors:theater_ratio(smartphone_ubiquity, 0.10).       % Highly functional, not theatrical.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(smartphone_ubiquity, extractiveness, 0.75).
narrative_ontology:constraint_metric(smartphone_ubiquity, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(smartphone_ubiquity, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a constructed utility, essential for modern life.
narrative_ontology:constraint_claim(smartphone_ubiquity, tangled_rope).
narrative_ontology:human_readable(smartphone_ubiquity, "The Smartphone Ubiquity Constraint").

% Binary flags
domain_priors:requires_active_enforcement(smartphone_ubiquity). % Required for app store rules, platform terms, and regional bans.

% Structural property derivation hooks:
% Both beneficiary and victim are required for Tangled Rope.
narrative_ontology:constraint_beneficiary(smartphone_ubiquity, platform_providers).
narrative_ontology:constraint_victim(smartphone_ubiquity, privacy_conscious_users).
narrative_ontology:constraint_victim(smartphone_ubiquity, youth_under_ban).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For youth subject to a legal ban, the constraint is a coercive exclusion from
% the digital social sphere their peers inhabit.
constraint_indexing:constraint_classification(smartphone_ubiquity, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For platform providers, this is pure coordination infrastructure, enabling
% a global marketplace. Extraction is seen as a cost of business.
constraint_indexing:constraint_classification(smartphone_ubiquity, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The system provides genuine coordination (Rope) but is built on a foundation
% of asymmetric data extraction (Snare) and requires active platform governance
% (Enforcement), making it a canonical Tangled Rope.
constraint_indexing:constraint_classification(smartphone_ubiquity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE GLOBAL USER (ROPE)
% For a user in a developing nation, the smartphone is a pure coordination tool,
% providing access to banking and information where none existed.
constraint_indexing:constraint_classification(smartphone_ubiquity, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(smartphone_ubiquity_tests).

test(perspectival_gap_powerless_vs_institutional) :-
    constraint_indexing:constraint_classification(smartphone_ubiquity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(smartphone_ubiquity, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    assertion(TypePowerless == snare),
    assertion(TypeInstitutional == rope),
    assertion(TypePowerless \= TypeInstitutional).

test(analytical_observer_sees_tangled_rope) :-
    constraint_indexing:constraint_classification(smartphone_ubiquity, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    assertion(TypeAnalytical == tangled_rope).

test(tangled_rope_structural_requirements_met) :-
    % Verify the necessary structural properties for a Tangled Rope are declared.
    narrative_ontology:constraint_beneficiary(smartphone_ubiquity, _),
    narrative_ontology:constraint_victim(smartphone_ubiquity, _),
    domain_priors:requires_active_enforcement(smartphone_ubiquity).

:- end_tests(smartphone_ubiquity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness of 0.75 reflects the immense value derived from user
 * data and attention, which is the core business model of the ecosystem. The
 * suppression score of 0.50 captures how smartphone ubiquity has made older
 * technologies (desktops, landlines) less viable alternatives.
 *
 * The Perspectival Gap is stark:
 * - For a powerless user under a ban, it's a Snare of social exclusion.
 * - For an institutional beneficiary (platform), it's a Rope for coordinating a global market.
 * - For a moderate user in a developing nation, it's a Rope providing essential infrastructure.
 * - The analytical view must reconcile these facts. The system provides undeniable
 *   coordination but is structurally dependent on asymmetric extraction and active
 *   enforcement, the three definitional pillars of a Tangled Rope.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * Classifying this as a pure Snare would ignore its profound coordination
 * benefits (e.g., leapfrogging infrastructure in developing nations). Classifying
 * it as a pure Rope would ignore the coercive, extractive data economy at its
 * core. The Tangled Rope classification correctly identifies it as a hybrid
 * system where coordination and extraction are deeply intertwined.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_smartphone_ubiquity,
    'Is the extraction of privacy a functional necessity for smartphone utility (Mountain-like property) or a predatory business model choice (Snare)?',
    'Audit of model efficiency with locally-encrypted data vs. centralized cloud-harvested data; regulatory impact assessments of data privacy laws.',
    'If necessity: Mountain of technological design. If predatory: Snare component is a policy choice, not a technical one.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(smartphone_ubiquity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the constraint's evolution from a utility tool to an
% extractive ecosystem. Extraction accumulated as network effects solidified.
% Theater ratio remains low as the system is highly functional.

% Theater ratio over time (stable):
narrative_ontology:measurement(smartphone_ubiquity_tr_t0, smartphone_ubiquity, theater_ratio, 0, 0.10).
narrative_ontology:measurement(smartphone_ubiquity_tr_t5, smartphone_ubiquity, theater_ratio, 5, 0.10).
narrative_ontology:measurement(smartphone_ubiquity_tr_t10, smartphone_ubiquity, theater_ratio, 10, 0.10).

% Extraction over time (extraction_accumulation drift):
narrative_ontology:measurement(smartphone_ubiquity_ex_t0, smartphone_ubiquity, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(smartphone_ubiquity_ex_t5, smartphone_ubiquity, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(smartphone_ubiquity_ex_t10, smartphone_ubiquity, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The smartphone ecosystem functions as a form of global infrastructure.
narrative_ontology:coordination_type(smartphone_ubiquity, global_infrastructure).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */