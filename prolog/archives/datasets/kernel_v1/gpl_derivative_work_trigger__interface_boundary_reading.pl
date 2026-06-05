% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__interface_boundary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__interface_boundary_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gpl_derivative_work_trigger__interface_boundary_reading
 *   human_readable: GPL Derivative Work Trigger: Interface Boundary Reading
 *   domain: software_licensing/copyright_law/open_source_governance
 *
 * SUMMARY:
 *   The GPL derivative-work trigger is a contested legal-technical boundary
 *   with profound implications for open-source governance. At the center is a
 *   simple question: when does using a GPL-licensed library require the
 *   downstream user to also release their code under GPL? The
 *   interface-boundary reading proposes that clean API boundaries constitute
 *   non-derivative aggregation even with tight coupling — interaction through
 *   standardized interfaces does not constitute a 'derivative work' that
 *   would trigger copyleft obligations, whereas direct linking, embedding, or
 *   code inclusion does. This reading sits between two extremes: a broad
 *   copyleft interpretation (any technical coupling invokes GPL obligations)
 *   and a narrow linking interpretation (only binary dynamic linking triggers
 *   obligations, static linking and API use are always permitted). The
 *   interface-boundary reading is a scaffold — a temporary institutional
 *   compromise that clarifies the boundary while remaining vulnerable to
 *   being superseded as license language matures. The constraint exhibits
 *   significant perspectival variance: ecosystem integrators see liberation
 *   (Rope), organized GPL advocates see principled compromise (Tangled Rope),
 *   downstream users see licensing ambiguity as an extraction mechanism
 *   (Snare), standards bodies see a problem with a defined technical solution
 *   (Scaffold), traditional copyright doctrine sees an impossible-to-apply
 *   analog framework (Piton), and the analytical observer risks treating a
 *   contingent legal choice as a technical inevitability (false Mountain).
 *
 * KEY AGENTS:
 *   - Ecosystem Integrators (institutional/arbitrage): Primary beneficiary — companies building on GPL infrastructure benefit from API boundaries that permit mixed licensing and modular composition
 *   - GPL Enforcement Coalition (organized/constrained): Secondary beneficiary / advocate — FSF, Software Freedom Conservancy defend the interface-boundary reading as a faithful interpretation protecting copyleft intent
 *   - Downstream Users (powerless/trapped): Primary victim — receive binaries or compiled artifacts without source; licensing obligations are opaque and cannot be determined from APIs alone
 *   - Standards Bodies (powerful/mobile): Mediator / scaffold agent — Linux Foundation, Apache Foundation developing explicit API-boundary clauses in license text to codify this reading
 *   - Traditional Copyright Law (institutional/arbitrage): Institutional layer — copyright doctrine provides the legal framework but is ill-fitted to software modularity; persists through inertia
 *   - Analytical Observer (analytical/analytical): Civilizational view — risks naturalizing the chosen interface boundary as a technical fact rather than a legal convention
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__interface_boundary_reading, 0.38).
domain_priors:suppression_score(gpl_derivative_work_trigger__interface_boundary_reading, 0.48).
domain_priors:theater_ratio(gpl_derivative_work_trigger__interface_boundary_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__interface_boundary_reading, scaffold).
narrative_ontology:human_readable(gpl_derivative_work_trigger__interface_boundary_reading, "GPL Derivative Work Trigger: Interface Boundary Reading").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__interface_boundary_reading, "software_licensing/copyright_law/open_source_governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__interface_boundary_reading).
narrative_ontology:has_sunset_clause(gpl_derivative_work_trigger__interface_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__interface_boundary_reading, 'fb04b166-b518-4905-9497-0df02d42e233').
narrative_ontology:cs_kernel_codification('fb04b166-b518-4905-9497-0df02d42e233', fixed_text).
narrative_ontology:cs_authority_grounding('fb04b166-b518-4905-9497-0df02d42e233', lineage).
narrative_ontology:cs_interpretation_layer_present('fb04b166-b518-4905-9497-0df02d42e233').
narrative_ontology:cs_reading_relation('fb04b166-b518-4905-9497-0df02d42e233', gpl_derivative_work_trigger__broad_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('fb04b166-b518-4905-9497-0df02d42e233', gpl_derivative_work_trigger__narrow_linking_permissive_reading, coexists_with).
narrative_ontology:cs_axiom('fb04b166-b518-4905-9497-0df02d42e233', foundational, api_boundary_technical_separability).
narrative_ontology:cs_axiom_status(api_boundary_technical_separability, holdable).
narrative_ontology:cs_axiom_grounding('fb04b166-b518-4905-9497-0df02d42e233', api_boundary_technical_separability, empirically_contingent).
narrative_ontology:cs_axiom('fb04b166-b518-4905-9497-0df02d42e233', foundational, copyleft_intent_preserves_linking_but_not_integration).
narrative_ontology:cs_axiom_status(copyleft_intent_preserves_linking_but_not_integration, holdable).
narrative_ontology:cs_axiom_grounding('fb04b166-b518-4905-9497-0df02d42e233', copyleft_intent_preserves_linking_but_not_integration, conventional).
narrative_ontology:cs_reference_frame('fb04b166-b518-4905-9497-0df02d42e233', gpl_v2_technical_neutrality).
narrative_ontology:cs_drift_state('fb04b166-b518-4905-9497-0df02d42e233', contemporary_containerization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fb04b166-b518-4905-9497-0df02d42e233', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, ecosystem_integrators).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, modular_architecture_practitioners).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__interface_boundary_reading, broad_copyleft_expectation_holders).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__interface_boundary_reading, downstream_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOWNSTREAM USER (SNARE) — Receives binary executables or compiled artifacts without source access. Cannot determine whether GPL obligations apply based on public APIs alone. Trapped between incompatible interpretations (broad vs. narrow) with no ability to inspect the technical coupling or negotiate terms. Maximum extraction: licensing uncertainty itself is the extraction mechanism.
constraint_indexing:constraint_classification(gpl_derivative_work_trigger__interface_boundary_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GPL ENFORCEMENT COALITION (TANGLED ROPE) — Software Freedom Conservancy, FSF, organized GPL defenders see the interface boundary reading as a genuine technical-legal compromise: it protects copyleft intent against linking circumvention while permitting modular architecture. Benefits from clarifying what is and is not a derivative work (reducing enforcement uncertainty). Constrained by difficulty establishing objective technical criteria; also benefits from having a clear doctrine to defend. Mixed: some coordination benefit (clarity) but significant asymmetry (defenders do unpaid labor).
constraint_indexing:constraint_classification(gpl_derivative_work_trigger__interface_boundary_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ECOSYSTEM INTEGRATORS (ROPE) — Companies building on GPL-licensed infrastructure (Android, container ecosystems, microservice platforms) see clean API boundaries as enabling coordination: they can integrate GPL components while choosing their own licensing for proprietary layers. Benefits from the reading via architectural freedom and reduced licensing entanglement. Experiences constraint as pure coordination — the API boundary provides clear interface contract that enables integration without forced copyleft propagation.
constraint_indexing:constraint_classification(gpl_derivative_work_trigger__interface_boundary_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STANDARDS BODIES / GOVERNANCE (SCAFFOLD) — Linux Foundation, Apache Foundation, and evolving software license governance norms are converging toward explicit interface-boundary clauses. This perspective sees the current ambiguity as a temporary coordination failure with a defined exit: as license drafting practices mature (REUSE Specification, Software Package Data Exchange), the interface boundary will be formally codified in license text itself. Sunset clause: in 10-15 years, explicit derivative-work triggers based on API coupling will replace interpretive doctrine.
constraint_indexing:constraint_classification(gpl_derivative_work_trigger__interface_boundary_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL COPYRIGHT FRAMEWORK (PITON) — Copyright law's concept of 'derivative work' is fundamentally analog-era doctrine: it presumes lineal transformation and creative recombination of expression. When applied to software, where 'derivative' could mean anything from binary linking to mere API invocation, the framework becomes theatrical — courts and practitioners perform copyright reasoning but with unclear technical referents. The doctrine persists through institutional inertia (copyright is the available legal tool) rather than functional fit. Theater ratio high because the framework's language (modification, adaptation, originality) does not map cleanly onto software modular boundaries.
constraint_indexing:constraint_classification(gpl_derivative_work_trigger__interface_boundary_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a theoretical view, software modularity itself is an immutable technical fact: clean API boundaries represent genuine separability at the machine-code level, and interaction through standardized interfaces is structurally distinct from embedding or compilation. From this perspective, the interface-boundary reading is not a legal doctrine but a recognition of technical reality — derivative works (through linking, embedding, code inclusion) are objectively distinguishable from interface-based composition. However, the constraint's beneficiary structure reveals this as a FALSE SUMMIT: the reading naturalizes what is actually a chosen jurisdictional boundary that privileges integrators and disadvantages users expecting copyleft propagation.
constraint_indexing:constraint_classification(gpl_derivative_work_trigger__interface_boundary_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__interface_boundary_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gpl_derivative_work_trigger__interface_boundary_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gpl_derivative_work_trigger__interface_boundary_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(gpl_derivative_work_trigger__interface_boundary_reading, TR),
    TR >= 0.70.

:- end_tests(gpl_derivative_work_trigger__interface_boundary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-low. The interface-boundary reading trades away some copyleft reach for architectural clarity. Users expecting full-stack source coverage experience reduced copyleft scope as extraction, but the reading does provide legitimate benefit to modular architecture. The metric reflects that this is a negotiated compromise: genuine coordination value (clarity for integrators) coexists with genuine cost (reduced copyleft for users). The extractiveness increased over the interval (0.28 → 0.38) as the reading became more entrenched in standards bodies and Linux-based systems, reducing the negotiation space. Suppression (0.48): Moderate. Barriers to understanding and changing the boundary include: technical complexity of API coupling analysis, institutional entrenchment in Linux/Android ecosystems, and cost of license auditing for integrators. Downstream users are suppressed by lack of transparency about which components trigger copyleft. Theater ratio (0.62): Moderate-high. Traditional copyright review of API boundaries remains theatrical — courts and auditors apply analog-era 'derivative work' doctrine to digital modular composition, producing reasoning that maps poorly onto actual software structure. The theater increased over time (0.45 → 0.62) as the interface-boundary reading became formalized in standards, requiring practitioners to perform sophisticated API analysis to justify licensing decisions, but the underlying copyright framework remained unchanged.
 *
 * PERSPECTIVAL GAP:
 *   The interface-boundary reading produces the widest perspectival gap in the GPL derivative-work kernel. The ecosystem integrators (Rope) see a liberation: clean API boundaries enable architectural freedom and mixed licensing. The standards bodies (Scaffold) see a temporary problem being solved through technical codification. The organized advocates (Tangled Rope) see a principled compromise that requires ongoing defense. The downstream users (Snare) see licensing opacity that extracts uncertainty cost. The traditional copyright framework (Piton) sees a doctrine that no longer functions. The analytical observer (false Mountain) risks concluding that interface boundaries are technical facts rather than chosen legal boundaries. This perspectival range is diagnostic: no single classification is correct; the presheaf of classifications reveals the contested nature of the kernel.
 *
 * DIRECTIONALITY LOGIC:
 *   The interface-boundary reading's directionality differs sharply across perspectives. Ecosystem integrators (institutional/arbitrage) experience low directionality (d ≈ 0.12) — they are net beneficiaries of the reading, experiencing it as enabling coordination (Rope classification produces low chi). GPL advocates (organized/constrained) experience moderate directionality (d ≈ 0.42) — they bear the enforcement labor and must justify the reading against both broad-copyleft and narrow-linking critiques (Tangled Rope classification). Downstream users (powerless/trapped) experience high directionality (d ≈ 0.88) — they are targets of the licensing structure and cannot exit (Snare classification). The analytical observer at civilizational scale (analytical/analytical) experiences moderate directionality (d ≈ 0.72) — the observer sees both the technical and political dimensions and risks naturalizing a contingent legal boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through kernel reading decomposition. The broad-copyleft reading and narrow-linking reading are separate constraints (separate JSON files) with different ε values, beneficiary structures, and victim sets. This interface-boundary reading is a third constraint, a scaffold that mediates between the two extremes. The mandatrophy is not 'which reading is correct?' but 'how does the ecosystem navigate three coexisting interpretations?' The three readings coexist_with each other — they are held by different parties (FSF vs. Linux Foundation vs. commercial integrators) and no single reading logically forecloses the others, though this reading does influence the others by establishing a middle-ground doctrine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    api_tightness_threshold,
    'At what degree of API coupling does interface-based integration cross into ''derivative work'' territory?',
    'Technical analysis of interface specifications: measured through API surface area coverage, parameter passing patterns, and behavioral coupling; comparison with empirical linker output and compiler intermediate representations',
    'If threshold is very tight (>95% behavioral coverage): interface boundary reading collapses toward narrow permissive reading, allowing almost any integration. If threshold is loose (<50% coverage): interface boundary reading converges with broad copyleft reading, making most integrations derivative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(api_tightness_threshold, empirical, 'Technical coupling threshold for derivative work classification').

omega_variable(
    license_intent_recovery,
    'Can GPL intent regarding modular architecture be reliably recovered from GPL v2/v3 text, or is the interface-boundary reading a neo-copyleft reinterpretation imposed on ambiguous language?',
    'Historical textual analysis of GPL drafting process (Stallman interviews, FSF minutes); comparison with author statements about anticipated software architectures in 1989 vs. 2024; linguistic analysis of ''derivative work'' definition in GPL v2/v3 vs. contemporaneous case law',
    'If recoverable: reading is doctrinal clarification, not innovation — more stable, more authoritative. If reinterpretation: reading is a negotiated settlement between open-source factions, not linguistic recovery — more vulnerable to alternative readings in future contexts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(license_intent_recovery, conceptual, 'Whether interface-boundary reading recovers original GPL intent or represents reinterpretation').

omega_variable(
    technical_enforcement_tractability,
    'Can API-boundary-based derivative work detection be automated in compliance tooling, or does it require expensive human judgment?',
    'Development and deployment of automated API-coupling analysis tools (SBOM-based, compiler-output-based, behavioral profiling); measurement of false-positive/false-negative rates in real-world software stacks; cost comparison with traditional source-code auditing',
    'If automatable: reading becomes practically enforceable and will be adopted by compliance infrastructure, embedding it in developer workflows. If not: reading remains a legal doctrine that diverges from engineering practice, vulnerable to circumvention and re-litigation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_enforcement_tractability, empirical, 'Whether API-boundary detection can be automated for compliance').

omega_variable(
    reading_stability_under_modularity_increase,
    'As software architectures become more modular and distributed (microservices, serverless, mesh networks), does the interface-boundary reading remain stable or does it require continuous reinterpretation?',
    'Longitudinal case analysis: track how courts and license authorities handle boundary questions as architectures evolve; assess whether API-based derivative work analysis produces consistent results across different architectural paradigms (monolithic, SOA, containerized, serverless)',
    'If stable: reading is robust to future architectural change. If unstable: reading will fragment into domain-specific variants, potentially re-opening the broader-vs-narrow divide in different technical contexts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_stability_under_modularity_increase, empirical, 'Stability of interface-boundary reading under architectural change').

omega_variable(
    kernel_reading_contest,
    'What is the structural relationship between this interface-boundary reading and the competing broad-copyleft and narrow-linking readings of the GPL derivative-work kernel?',
    'This omega documents the committer-frame uncertainty: are the three readings logically incompatible (forecloses relation), or do they coexist as different valid interpretations of ambiguous GPL text (coexists_with relation), or does one reading shape conditions for the others (influences relation)? Resolution requires analyzing the GPL''s actual language and case-law precedents.',
    'If forecloses: only one reading can be correct; adoption of this reading excludes the alternatives. If coexists: all three readings remain live for different parties (FSF vs. Linux Foundation vs. commercial integrators). If influences: this reading''s success in standards bodies shapes whether competing readings remain viable in downstream contexts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel reading relationships and logical compatibility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__interface_boundary_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_iface_tr_t0, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(gpl_iface_tr_t3, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 3, 0.55).
narrative_ontology:measurement(gpl_iface_tr_t6, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 6, 0.62).

% Extraction over time
narrative_ontology:measurement(gpl_iface_be_t0, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(gpl_iface_be_t3, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 3, 0.33).
narrative_ontology:measurement(gpl_iface_be_t6, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__interface_boundary_reading, resource_allocation).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger__broad_copyleft_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger__narrow_linking_permissive_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__interface_boundary_reading, software_licensing_ambiguity).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__interface_boundary_reading, api_contract_enforceability).

% DUAL FORMULATION NOTE:
% The GPL derivative-work kernel has three distinct readings with different ε values. The interface-boundary reading (this file) has ε ≈ 0.38 (Scaffold). The broad-copyleft reading has ε ≈ 0.55 (Tangled Rope) — more extraction but also more copyleft clarity. The narrow-linking reading has ε ≈ 0.25 (Rope) — minimal extraction but also minimal copyleft scope. All three readings are structurally distinct constraints with different beneficiary/victim structures. They are linked through network.affects_constraints to show the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_derivative_work_trigger__interface_boundary_reading, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
