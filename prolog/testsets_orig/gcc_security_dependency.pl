% ============================================================================
% CONSTRAINT STORY: gcc_security_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gcc_security_dependency, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gcc_security_dependency
 *   human_readable: GCC Security Dependency Lock-In
 *   domain: software_infrastructure/compiler_economics
 *
 * SUMMARY:
 *   The GCC compiler has become an infrastructure lock-in point for large
 *   segments of the software ecosystem. While GCC was once the dominant
 *   free/open-source compiler by technical merit, the constraint now operates
 *   primarily through path dependency, institutional inertia, and switching
 *   costs rather than technical superiority. Modern alternatives (LLVM/Clang,
 *   proprietary toolchains) have comparable or superior capabilities, yet
 *   migration is suppressed by certification requirements, binary
 *   compatibility constraints, and the volunteer maintenance burden that
 *   makes replacement appear impossible. The constraint exhibits all six
 *   classification types from different perspectives: pure extraction for
 *   trapped embedded developers; hybrid coordination-extraction for mid-sized
 *   software companies; pure coordination for distribution maintainers with
 *   migration agency; scaffold properties for organized LLVM coalitions with
 *   visible exit pathways; degraded inertia (piton) for the volunteer
 *   community sustaining an increasingly complex codebase; and false natural
 *   law (mountain) at the analytical civilizational level. The extractiveness
 *   has increased from 0.32 to 0.52 over the interval as security
 *   vulnerabilities have accumulated faster than volunteer patches, while
 *   theater ratio has risen (0.35→0.55) as GCC maintenance increasingly
 *   consists of backward-compatibility theater rather than functional
 *   security improvements.
 *
 * KEY AGENTS:
 *   - Embedded Systems Developers: Primary victims (powerless/trapped) — cannot exit due to certification and binary compatibility locks; bear security vulnerability costs indefinitely
 *   - Mid-Sized Software Companies: Secondary victims (moderate/constrained) — face extraction in the form of forced upgrade cycles and toolchain replacement costs; also benefit from standardization
 *   - Linux Distribution Maintainers: Primary beneficiaries (institutional/arbitrage) — control patch selection and rollout timing; can arbitrage across GCC versions and cherry-pick upstream patches
 *   - LLVM/Clang Coalition: Organized alternative (organized/mobile) — competitive threat that provides sunset mechanism; growing adoption in modern domains (Rust, modern C++, CUDA)
 *   - GCC Volunteer Maintenance Community: Institutional actor (institutional/arbitrage) — sustains constraint through unpaid labor; experiences their own work as increasingly performative
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a contingent institutional arrangement as immutable infrastructure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gcc_security_dependency, 0.52).
domain_priors:suppression_score(gcc_security_dependency, 0.68).
domain_priors:theater_ratio(gcc_security_dependency, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gcc_security_dependency, extractiveness, 0.52).
narrative_ontology:constraint_metric(gcc_security_dependency, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(gcc_security_dependency, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gcc_security_dependency, tangled_rope).
narrative_ontology:human_readable(gcc_security_dependency, "GCC Security Dependency Lock-In").
narrative_ontology:topic_domain(gcc_security_dependency, "software_infrastructure/compiler_economics").

domain_priors:requires_active_enforcement(gcc_security_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gcc_security_dependency, gcc_maintainer_ecosystem).
narrative_ontology:constraint_beneficiary(gcc_security_dependency, gcc_dependent_vendors).
narrative_ontology:constraint_victim(gcc_security_dependency, downstream_software_projects).
narrative_ontology:constraint_victim(gcc_security_dependency, open_source_ecosystem_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMBEDDED SYSTEMS DEVELOPER (SNARE) — Trapped by critical security vulnerabilities in GCC that cannot be patched without adopting newer versions with breaking changes or proprietary toolchains. Cannot exit: legacy systems require specific GCC versions for compliance/certification; newer versions break binary compatibility or introduce license restrictions. Bears full extraction cost.
constraint_indexing:constraint_classification(gcc_security_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-SIZED SOFTWARE COMPANY (TANGLED ROPE) — Constrained by the coordination function (GCC enables product development and security updates) but also faces extraction: upgrade timelines are controlled upstream; security patches often require full toolchain replacements; relocation to LLVM requires months of testing. Benefits from standardization; bears asymmetric update costs.
constraint_indexing:constraint_classification(gcc_security_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LINUX DISTRIBUTION MAINTAINER (ROPE) — Benefits from GCC standardization and security updates (institutional arbitrage: can cherry-pick patches, maintain multiple versions, control rollout timing). Experiences the constraint as pure coordination: GCC provides the platform for all distribution features. Minimal extraction experienced.
constraint_indexing:constraint_classification(gcc_security_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LLVM/CLANG COALITION (SCAFFOLD) — Organized alternative toolchain with sunset logic: LLVM/Clang is progressively replacing GCC across domains (CUDA, Apple platforms, modern C++ standards). The GCC dependency is being solved systematically through competitive alternatives. High agency; clear exit pathway over 5-10 year horizon. Low experienced extraction because the constraint has visible sunset.
constraint_indexing:constraint_classification(gcc_security_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: GCC VOLUNTEER MAINTENANCE COMMUNITY (PITON) — Maintains the constraint through institutional inertia and volunteer labor. GCC's functional primacy (for kernel compilation, embedded systems standards certification) is real, but the maintenance burden is substantially theatrical: volunteers sustain a complex historical codebase primarily because replacing it is harder than patching it. Theater ratio reflects performative backward-compatibility maintenance that could be eliminated with coordinated ecosystem transition.
constraint_indexing:constraint_classification(gcc_security_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE MOUNTAIN) — At civilizational scope, one might naturalize the GCC constraint as inherent to software infrastructure: 'compiler standardization is an immutable necessity.' This analysis risks false summitry — the constraint's persistence is institutional/economic (path dependence, switching costs, volunteer burnout) not inherent to compilation. The engine will flag this as naturalization of contingency.
constraint_indexing:constraint_classification(gcc_security_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gcc_security_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gcc_security_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gcc_security_dependency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gcc_security_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gcc_security_dependency, TR),
    TR >= 0.70.

:- end_tests(gcc_security_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, increasing. The constraint extracts from downstream projects primarily through forced upgrade cycles, security patch delays, and switching costs. Extractiveness rose from 0.32 to 0.52 because security vulnerabilities have accumulated faster than volunteer patches, increasing the pressure on dependent projects. However, it has not reached the 0.66 threshold for pure snare classification because significant coordination benefits remain (GCC is still the standard for kernel compilation, embedded systems, and legacy systems). Suppression (0.68): High. Significant barriers to exit include: (1) certification locks (embedded systems standards require specific GCC versions), (2) binary compatibility constraints (downstream systems certified against specific GCC ABIs), (3) switching costs (full toolchain replacement requires months of testing), (4) volunteer burnout (perception that 'there is no alternative' due to maintenance burden), (5) downstream dependency chains (libraries compiled with GCC require GCC-compatible interfaces). Theater ratio (0.55): Moderate. GCC maintenance increasingly consists of performative activity: backward-compatibility testing for systems that are not meaningfully used, maintenance of ancient subsystems, and documentation updates that do not address current security issues. However, some portion (45%) is genuinely functional — security patches, standards compliance, and performance optimization. The ratio has increased from 0.35 to 0.55 because volunteer effort is increasingly concentrated on maintaining the constraint (supporting legacy configurations) rather than improving it (adding modern features).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gaps reveal the constraint's hybrid nature. The embedded systems developer (trapped, powerless) experiences pure extraction (snare) because they cannot move to alternatives — certification locks and binary compatibility constraints are insurmountable within their operational constraints. The Linux distribution maintainer (institutional, arbitrage) experiences pure coordination (rope) because they have full agency: can fork, cherry-pick, or migrate at organizational discretion. The mid-sized company (moderate, constrained) experiences the constraint as tangled rope — real coordination benefits (standardization, ecosystem stability) coexist with extraction (forced upgrade cycles, switching costs). The LLVM coalition (organized, mobile) experiences the constraint as temporary (scaffold) because alternative toolchains are progressively replacing GCC across domains. The volunteer community (institutional, arbitrage) experiences the constraint as degraded (piton) — their maintenance work is increasingly performative, sustained by institutional obligation rather than functional necessity. The analytical observer at civilizational scope risks falsely naturalizing the constraint as inherent ('compiler standardization is immutable'), but the structural data reveals it as a contingent institutional arrangement with visible exit pathways.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from the agent's position in the extraction pipeline and their exit capacity. Trapped embedded developers experience high d (0.90+) because they bear costs without exit options — certification locks and binary compatibility constraints are structural and cannot be overcome through migration. Mid-sized companies experience moderate d (0.55-0.65) because they face high but surmountable switching costs; they benefit from standardization but are extracted from through forced cycles. Distribution maintainers experience low d (0.10-0.20) because they have arbitrage options and control rollout timing; GCC subsidizes their platform rather than extracting from it. The LLVM coalition experiences very low d (0.05-0.15) because they are external beneficiaries (alternative toolchain adoption reduces GCC's market power) and have mobile exit options. The volunteer community's d is ambiguous (0.40-0.50) — they are nominally beneficiaries (GCC is 'their project') but functionally victims (unpaid labor maintaining an increasingly difficult constraint).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is structurally hybrid (tangled rope) but appears differently from each position in the ecosystem. The beneficiary side (distribution maintainers, GCC vendors) sees coordination (rope) and experiences low extraction. The victim side (embedded developers, software companies) sees extraction with coordination cover (tangled rope or snare depending on exit options). The constraint resolves mandatrophy by acknowledging that both the coordination function and the extraction function are real — GCC genuinely standardizes compilation, and it also genuinely locks downstream projects into upgrade cycles and certification constraints. The false mountain perspective (civilizational analytical observer) would naturalize this as 'inherent to infrastructure,' but the structural data shows it is institutional/economic: alternative toolchains with comparable capabilities exist, and migration is constrained by path dependency, switching costs, and volunteer burnout rather than technical necessity. Mandatrophy is resolved: the classification is tangled_rope because both genuine coordination (kernel compilation standardization, embedded systems toolchain stability) and asymmetric extraction (forced security-patch cycles, certification locks, vendor lock-in) are present.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_patch_supply_lag,
    'Is the observed security patch lag in maintained GCC versions due to technical complexity or coordinated suppression of alternative toolchain adoption?',
    'Comparison of patch delivery timelines for GCC vs LLVM/Clang for identical vulnerability classes; analysis of release cadence shifts following major ecosystem shifts (e.g., post-Rust adoption, post-C++20)',
    'If technical: constraint is lower-extractiveness coordination problem (Rope from more perspectives). If suppression: constraint is higher-extractiveness extraction with coordination cover (Snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_patch_supply_lag, empirical, 'Whether patch lag reflects technical or organizational factors').

omega_variable(
    volunteer_labor_sustainability,
    'How much of GCC''s continued maintenance burden is sustainable volunteer labor vs performative institutional obligation?',
    'Longitudinal contributor burnout analysis; correlation between feature complexity and volunteer retention; cost analysis comparing GCC maintenance to hypothetical LLVM/Clang fork costs',
    'If sustainable: piton classification overstates theater (GCC is degraded but functional). If unsustainable: piton is accurate and ecosystem migration timeline is constrained by volunteer availability, not technical factors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(volunteer_labor_sustainability, empirical, 'Sustainability of GCC volunteer maintenance').

omega_variable(
    kernel_ecosystem_locked_to_gcc,
    'Is Linux kernel exclusive reliance on GCC a technical requirement or institutional norm maintained by subsystem maintainers?',
    'Analysis of Clang kernel compilation efforts (LLVM kernel project status); comparison of technical barriers vs organizational/political resistance to diversification',
    'If technical: GCC dependency for kernel is genuinely immutable (Mountain-like at kernel scope). If institutional: dependency is contingent and could be dissolved with coordinated migration (Scaffold with real sunset).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_ecosystem_locked_to_gcc, empirical, 'Whether kernel reliance on GCC is technical or institutional').

omega_variable(
    embedded_system_certification_lock,
    'Do embedded systems standards (DO-178C, IEC-61508) genuinely require specific GCC versions or are they locked through legacy certification requirements rather than technical necessity?',
    'Audit of certification bodies'' actual GCC version specifications vs minimum security requirements; cost-benefit analysis of recertification with newer toolchains vs security patch maintenance burden',
    'If genuine technical requirement: trapped agents (embedded developers) are truly immobilized (high d → high extraction). If certification lock: constraint is artificially maintained through institutional inertia (reclassifies toward Scaffold with reachable sunset).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(embedded_system_certification_lock, empirical, 'Whether certification lock reflects technical or institutional factors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gcc_security_dependency, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gcc_tr_t0, gcc_security_dependency, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gcc_tr_t5, gcc_security_dependency, theater_ratio, 5, 0.45).
narrative_ontology:measurement(gcc_tr_t10, gcc_security_dependency, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(gcc_be_t0, gcc_security_dependency, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(gcc_be_t5, gcc_security_dependency, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(gcc_be_t10, gcc_security_dependency, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gcc_security_dependency, global_infrastructure).
narrative_ontology:affects_constraint(gcc_security_dependency, linux_kernel_compilation_monoculture).
narrative_ontology:affects_constraint(gcc_security_dependency, embedded_systems_certification_lock).
narrative_ontology:affects_constraint(gcc_security_dependency, software_vendor_toolchain_dependency).

% DUAL FORMULATION NOTE:
% The GCC security dependency is upstream of specific vendor lock-in constraints (e.g., Linux kernel exclusive reliance on GCC, embedded systems standards tied to specific GCC versions). Each downstream constraint has its own extractiveness reflecting the specific institutional lock (certification, binary compatibility), but all are enabled by the upstream GCC dependency. The constraint family should model GCC as the primary lock and specific domain locks as secondary constraints linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gcc_security_dependency, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
