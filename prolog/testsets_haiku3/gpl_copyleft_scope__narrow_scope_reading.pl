% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__narrow_scope_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__narrow_scope_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: gpl_copyleft_scope__narrow_scope_reading
 *   human_readable: GPL Section 2(b) Narrow Copyleft Scope — Traditional Copyright Derivative Work Boundary
 *   domain: software/intellectual_property/open_source
 *
 * SUMMARY:
 *   GPL Section 2(b) establishes copyleft obligations for derivative works.
 *   The narrow-scope reading interprets 'derivative work' using traditional
 *   copyright doctrine, excluding dynamic linking, plugins, and mere
 *   aggregation from copyleft contagion. This is ONE reading of a contested
 *   kernel. Proprietary software developers and commercial integrators
 *   benefit from the narrow boundary, retaining flexibility to layer
 *   proprietary code above GPL components. Copyleft advocates bear the cost:
 *   their expectation of universal code-sharing is structurally weakened. The
 *   narrow reading is actively defended through legal interpretation and
 *   industry practice, though absent comprehensive litigation settlement. The
 *   claimed type is rope (real coordination: GPL licensing clarifies terms
 *   and enables mixed-license integration) and the metrics reflect moderate
 *   extraction (proprietary developers capture flexibility; copyleft
 *   advocates lose leverage) and low suppression (the constraint operates
 *   through legal interpretation, not active enforcement machinery).
 *
 * KEY AGENTS:
 *   - proprietary_software_developers: Benefit from the narrow boundary; can integrate GPL via plugins/dynamic linking without source-sharing obligations (powerful, arbitrage exit)
 *   - copyleft_advocates: Bear the cost; their universal code-sharing goal is structurally narrowed (moderate, constrained exit)
 *   - commercial_integrators: Benefit from assembly flexibility; treat GPL as component boundary (powerful, arbitrage exit)
 *   - free_software_communities: Maintain GPL projects but face contagion leakage; can fork or license under stronger terms (organized, constrained exit)
 *   - litigation_bodies: Agenda-setters; their interpretation solidifies or undermines the narrow boundary (institutional, trapped)
 *   - copyright_scholars: Observer seat; assess coherence with traditional doctrine (analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__narrow_scope_reading, 0.38).
domain_priors:suppression_score(gpl_copyleft_scope__narrow_scope_reading, 0.22).
domain_priors:theater_ratio(gpl_copyleft_scope__narrow_scope_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__narrow_scope_reading, rope).
narrative_ontology:human_readable(gpl_copyleft_scope__narrow_scope_reading, "GPL Section 2(b) Narrow Copyleft Scope — Traditional Copyright Derivative Work Boundary").
narrative_ontology:topic_domain(gpl_copyleft_scope__narrow_scope_reading, "software/intellectual_property/open_source").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__narrow_scope_reading, 'e82b206c-56b4-460b-bea7-6edf3e14aab8').
narrative_ontology:cs_kernel_codification('e82b206c-56b4-460b-bea7-6edf3e14aab8', fixed_text).
narrative_ontology:cs_authority_grounding('e82b206c-56b4-460b-bea7-6edf3e14aab8', lineage).
narrative_ontology:cs_interpretation_layer_present('e82b206c-56b4-460b-bea7-6edf3e14aab8').
narrative_ontology:cs_reading_relation('e82b206c-56b4-460b-bea7-6edf3e14aab8', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('e82b206c-56b4-460b-bea7-6edf3e14aab8', gpl_copyleft_scope__enforcement_vacuum_reading, coexists_with).
narrative_ontology:cs_axiom('e82b206c-56b4-460b-bea7-6edf3e14aab8', foundational, derivative_work_traditional_doctrine).
narrative_ontology:cs_axiom_status(derivative_work_traditional_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('e82b206c-56b4-460b-bea7-6edf3e14aab8', derivative_work_traditional_doctrine, conventional).
narrative_ontology:cs_axiom('e82b206c-56b4-460b-bea7-6edf3e14aab8', foundational, architectural_separation_permits_copyleft_escape).
narrative_ontology:cs_axiom_status(architectural_separation_permits_copyleft_escape, holdable).
narrative_ontology:cs_axiom_grounding('e82b206c-56b4-460b-bea7-6edf3e14aab8', architectural_separation_permits_copyleft_escape, instrumental).
narrative_ontology:cs_reference_frame('e82b206c-56b4-460b-bea7-6edf3e14aab8', traditional_copyright_derivative_work_framework).
narrative_ontology:cs_drift_state('e82b206c-56b4-460b-bea7-6edf3e14aab8', contemporary_industry_practice_settlement, gap(stable, minor, false)).
narrative_ontology:cs_created_at('e82b206c-56b4-460b-bea7-6edf3e14aab8', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, proprietary_software_developers).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, plugin_ecosystem_builders).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, dynamic_linking_practitioners).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, commercial_integrators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gpl_copyleft_scope__narrow_scope_reading, copyleft_advocates).
narrative_ontology:constraint_victim(gpl_copyleft_scope__narrow_scope_reading, free_software_communities).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__narrow_scope_reading, traditional_copyright_derivative_work_doctrine).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__narrow_scope_reading, separation_of_concerns_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Can incorporate GPL-licensed libraries into proprietary applications via dynamic linking, aggregation, or plugin architectures without triggering copyleft obligations. Retain ability to keep proprietary source code closed while benefiting from GPL component functionality. Coordinate with GPL developers through clear interface boundaries without sharing the full codebase.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, proprietary_software_developers, beneficiary,
    powerful, generational, arbitrage, global).

% Build closed-source plugins that interact with GPL host applications through defined plugin APIs. The narrow scope reading permits these architectures without requiring plugin source release. Coordinate ecosystem participation without surrendering proprietary business logic.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, plugin_ecosystem_builders, beneficiary,
    organized, biographical, mobile, global).

% Deploy GPL libraries via dynamic linking without triggering source-code-sharing obligations on the linking application. Can treat GPL components as runtime dependencies rather than derivative works. Coordinate mixed-license systems through runtime composition rather than compile-time source integration.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, dynamic_linking_practitioners, beneficiary,
    organized, biographical, constrained, global).

% Assemble integrated products from GPL and proprietary components while maintaining proprietary source protection on the assembly layer. Retain commercial model flexibility by treating GPL as a component boundary rather than a contagion mechanism.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, commercial_integrators, beneficiary,
    powerful, generational, arbitrage, global).

% Seek universal code-sharing through copyleft but confront structural narrowness: derivative work boundary excludes many intended contagion targets. Their enforcement expectation (dynamic linking should trigger copyleft) is not supported by the narrow reading's traditional copyright frame. Must litigate or fork to contest boundary interpretation.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, copyleft_advocates, payer,
    moderate, biographical, constrained, global).

% Maintain GPL projects and coordinate free-software development. Under the narrow reading, proprietary software can harvest GPL work through careful architectural layering, weakening copyleft's reach. They can fork, license under stronger terms, or accept the boundary; the narrow reading constrains their leverage.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, free_software_communities, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__narrow_scope_reading, free_software_communities, observer).

% Analyze copyright law and derivative work doctrine. The narrow reading anchors copyleft scope to traditional copyright categories rather than GPL-specific intent. They assess whether the reading coheres with common-law derivative-work precedent or stretches doctrine.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, copyright_scholars, observer,
    analytical, generational, analytical, global).

% Adjudicate disputes over GPL scope when enforcement is pursued. The narrow reading privileges traditional copyright doctrine; courts applying it defer to established derivative work boundaries rather than GPL-specific intent. Their rulings either solidify or undermine the narrow boundary.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, litigation_bodies, agenda_setter,
    institutional, generational, trapped, national).

% The FSF's interpretation (broad copyleft, dynamic linking = derivative) is the excluded sibling position. Under the narrow reading, FSF guidance is advisory but not controlling; courts apply traditional doctrine instead. The FSF can only influence by litigation, license revision, or community persuasion.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, fsf_interpreted_authority, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__narrow_scope_reading, proprietary_software_developers).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__narrow_scope_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates open-source software development by licensing terms: GPL requires that derivative works share source. The narrow scope reading limits which works count as derivatives, narrowing the contagion boundary. This reduces the copyleft coordination zone but clarifies boundaries for mixed-license integration.
% TRANSFER_FUNCTION: GPL provides source code to downstream users. Under the narrow reading, the transfer obligation applies only to direct modifications or statically-linked compilations, not to plugin authors, dynamic linkers, or aggregators. This limits who receives the transfer and who pays through code-sharing.
% ABSENT_VOICES: Embedded systems developers constrained by weak hardware are excluded from the narrow reading's design intent (they would argue dynamic linking should not be treated as separable). Jurisdictions without U.S. copyright precedent are partly excluded; the reading privileges American derivative-work doctrine. Downstream user communities in GPL projects have weak say in interpretation despite bearing contagion outcomes.
% DISAPPEARANCE_RATIONALE: If the narrow scope reading disappeared and were replaced by strong copyleft, proprietary software integrators would face source-sharing obligations on plugin architectures and dynamic linking. Commercial products would restructure to avoid GPL integration, forking to dual-license models or proprietary alternatives. The licensing ecosystem would reorganize around copyleft universality rather than narrow boundaries.
% FOUNDING_PROBLEM: GPL V2 drafters faced ambiguity: what counts as a derivative work under copyright law? They adopted traditional copyright doctrine rather than writing a GPL-specific definition. Over decades, narrow interpretations emerged allowing dynamic linking and plugins to escape copyleft. The narrow reading formalizes the result: GPL scope tracks copyright derivative-work categories.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholarship supporting narrow reading: Eben Moglen on GPL ambiguity (external, independent analyst); industry practice of GPL+proprietary integration without legal challenge (external evidence from 20+ years of coexistence). FSF disputes the status: they attest the founding problem remains live (dynamic linking SHOULD trigger copyleft). No comprehensive case law has settled it; court outcomes diverge by jurisdiction (Germany vs. U.S., narrow vs. functional readings).
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__narrow_scope_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__narrow_scope_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__narrow_scope_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_copyleft_scope__narrow_scope_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__narrow_scope_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__narrow_scope_reading_tests).
:- end_tests(gpl_copyleft_scope__narrow_scope_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at end): the narrow reading extracts flexibility from copyleft advocates (loses contagion scope) and transfers it to proprietary developers (gains integration latitude). The extraction is not zero (proprietary software does benefit, copyleft is weakened) but is bounded by the traditional copyright framework, not by GPL-specific intent. Suppression is low (0.22): the constraint operates through legal interpretation and licensing clarity, not through active suppression machinery. There is resistance (0.62): copyleft advocates dispute the boundary; they argue dynamic linking should trigger copyleft; they litigate, fork, and advocate for license revision. Theater is low (0.18): the constraint delivers real coordination (GPL licensing, mixed-license integration works), though interpretive clarity has grown over time as industry practice settled around the narrow reading. The measurement series show a gentle rise in extractiveness and theater ratio over the interval, reflecting accumulating industry reliance on the narrow boundary as a stable interpretation. Suppression requirement rises modestly as the boundary becomes more formalized, but remains low because enforcement is rare and litigation outcomes remain unsettled.
 *
 * PERSPECTIVAL GAP:
 *   From the proprietary developer seat: the narrow reading is genuine coordination—it clarifies boundaries, enables mixed-license development, and provides legal safety through traditional copyright doctrine. From the copyleft advocate seat: the narrow reading is extraction—it hollows out copyleft contagion, allows proprietary firms to harvest GPL work through architectural layering, and privileges industry interpretation over FSF intent. From the litigation body seat: the narrow reading is a faithful application of copyright law, neither extraction nor pure coordination. The engine computes these divergent d values from power, exit, and beneficiary/victim structure; the narrow reading produces structural asymmetry—high-power agents (proprietary developers) with arbitrage exit are beneficiaries; moderate-power agents (copyleft advocates) with constrained exit are payers. The claim (rope) and the metrics (moderate extraction, low suppression) cohere around a coordination mechanism that has asymmetric benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary developers: d ≈ 0.15 (beneficiary: the narrow boundary enables their desired flexibility; powerful with arbitrage exit; extraction is not their cost). Copyleft advocates: d ≈ 0.75 (target: they lose contagion scope; moderate power with constrained exit; extraction is their cost via forgone universal code-sharing). Plugin ecosystem builders: d ≈ 0.20 (beneficiary: plugins can remain closed; organized, mobile exit). Dynamic linkers: d ≈ 0.25 (beneficiary: dynamic linking is untethered from copyleft; organized, constrained exit). Free software communities: d ≈ 0.60 (mixed: genuine coordination benefit from GPL licensing, but contagion leakage cost; organized, constrained exit). The narrow reading structures asymmetry along the beneficiary/victim divide, not along power level per se—a powerful proprietary developer and a moderate copyleft advocate experience radically different d values because they have opposite relationships to the contagion boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   The narrow reading resolves a potential mandatrophy: the founding problem ('what counts as derivative work?') has dual status. From the proprietary developer frame, the problem is SOLVED—traditional copyright doctrine provides clarity. From the copyleft frame, the problem is LIVE—dynamic linking SHOULD trigger copyleft, but doesn't under the narrow reading. The narrow reading is not a zombie constraint maintained by inertia; it delivers real coordination (licensing clarity, mixed-license integration) and has active resistance (copyleft advocates litigate and license-revise). The theater ratio is low because the constraint operates through interpreted law, not through performance of compliance. This is a rope, not a piton, because the coordination function (clarifying GPL scope) persists and is valued by substantial beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivative_work_doctrine_closure,
    'Does traditional copyright derivative-work doctrine provide a closed, determinate boundary for GPL scope, or does it require GPL-specific interpretation in contexts (like dynamic linking) where copyright law is unsettled?',
    'Comprehensive appellate litigation across multiple jurisdictions establishing whether courts apply traditional doctrine directly or recognize GPL-specific semantic requirements (e.g., ''works based on'' in GPL requires intent or knowledge of GPL coupling, versus mechanical copyright derivative-work tests).',
    'If closed and determinate, the narrow reading is structurally stable and traditional doctrine suffices. If unsettled and GPL-requiring, the narrow reading conflates copyright and GPL categories, and the strong_copyleft_reading becomes more defensible within the same judicial framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derivative_work_doctrine_closure, empirical, 'Whether traditional copyright doctrine provides determinate GPL scope without GPL-specific amendment.').

omega_variable(
    dynamic_linking_technical_vs_legal_coupling,
    'Are dynamically-linked libraries technical strangers (separate executables, no shared code at runtime) or legal couplings (linked program is derivative because the linker created a unified functional entity)?',
    'Technical discovery via software architecture analysis and appellate reasoning clarifying the legal relevance of runtime composition vs. compile-time object fusion. Contrast: static linking (one binary, shared memory space) vs. dynamic linking (separate binaries, runtime symbol resolution).',
    'If technical separation matters legally, the narrow reading stands (dynamic linking ≠ derivative). If functional unity matters (users experience one integrated program), strong_copyleft_reading becomes stronger—dynamic linking IS derivative because the final user-facing product is unified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dynamic_linking_technical_vs_legal_coupling, conceptual, 'Whether legal coupling follows technical object structure or user-facing functional unity.').

omega_variable(
    reading_contest_enforcement_capacity,
    'Which interpretive community (FSF, courts, industry consortia) has effective enforcement capacity, and does their capacity vary by geographic jurisdiction or industry sector?',
    'Empirical audit of GPL enforcement history: which reading do courts adopt when litigation occurs? Do outcomes diverge by region (EU vs. U.S., copyleft-friendly vs. commerce-friendly jurisdictions)? Which reading do industry standard-setters and platforms enforce?',
    'If narrow-reading advocates (courts, platforms) have stronger enforcement capacity, the narrow reading persists despite FSF disputes. If strong-copyleft advocates (FSF, free-software courts in EU) gain capacity, the reading tilts toward strong_copyleft_reading or enforcement_vacuum_reading (plural coexistence).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_enforcement_capacity, empirical, 'Which interpretive community controls effective GPL enforcement and how jurisdiction-dependent is that control.').

omega_variable(
    kernel_reading_committer_indexing,
    'This constraint (narrow_scope_reading) is one reading of kernel gpl_copyleft_scope. Are the three declared readings (narrow_scope, strong_copyleft, enforcement_vacuum) exhaustive? Is there a fourth reading (e.g., license-revision reading: GPL V3+ adopts strong copyleft explicitly, superseding the V2 ambiguity)? What is the frame for declaring readings complete?',
    'Committer-axis review: enumerate all active, organized interpretive stances toward GPL scope in the ecosystem (FSF, OSI, courts, industry). Map each stance to a reading. If a stance lacks a constraint story, decompose it.',
    'Incompleteness affects network.affects_constraints and family cohesion. A missing strong reading may leave the kernel analysis under-provisioned. A GPL V3 revision reading would represent a different kernel-response mechanism (license evolution vs. interpretation within one license version).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_indexing, conceptual, 'Completeness and closure of the kernel reading set for gpl_copyleft_scope.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__narrow_scope_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(gpl__tr_t0, observed).
narrative_ontology:measurement(gpl__tr_t3, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 3, 0.11).
narrative_ontology:measurement_basis(gpl__tr_t3, observed).
narrative_ontology:measurement(gpl__tr_t6, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 6, 0.13).
narrative_ontology:measurement_basis(gpl__tr_t6, observed).
narrative_ontology:measurement(gpl__tr_t12, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement_basis(gpl__tr_t12, observed).
narrative_ontology:measurement(gpl__tr_t18, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 18, 0.17).
narrative_ontology:measurement_basis(gpl__tr_t18, observed).
narrative_ontology:measurement(gpl__tr_t25, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement_basis(gpl__tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(gpl__be_t0, observed).
narrative_ontology:measurement(gpl__be_t3, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 3, 0.26).
narrative_ontology:measurement_basis(gpl__be_t3, observed).
narrative_ontology:measurement(gpl__be_t6, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 6, 0.3).
narrative_ontology:measurement_basis(gpl__be_t6, observed).
narrative_ontology:measurement(gpl__be_t12, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 12, 0.35).
narrative_ontology:measurement_basis(gpl__be_t12, observed).
narrative_ontology:measurement(gpl__be_t18, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 18, 0.37).
narrative_ontology:measurement_basis(gpl__be_t18, observed).
narrative_ontology:measurement(gpl__be_t25, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(gpl__be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(gpl__su_t0, observed).
narrative_ontology:measurement(gpl__su_t3, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 3, 0.14).
narrative_ontology:measurement_basis(gpl__su_t3, observed).
narrative_ontology:measurement(gpl__su_t6, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 6, 0.16).
narrative_ontology:measurement_basis(gpl__su_t6, observed).
narrative_ontology:measurement(gpl__su_t12, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 12, 0.2).
narrative_ontology:measurement_basis(gpl__su_t12, observed).
narrative_ontology:measurement(gpl__su_t18, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 18, 0.21).
narrative_ontology:measurement_basis(gpl__su_t18, observed).
narrative_ontology:measurement(gpl__su_t25, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 25, 0.22).
narrative_ontology:measurement_basis(gpl__su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__narrow_scope_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gpl_copyleft_scope__narrow_scope_reading, 0.12).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope__strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope__enforcement_vacuum_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, software_licensing__proprietary_mixed_model).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, open_source__plugin_architecture_boundary).

% DUAL FORMULATION NOTE:
% The gpl_copyleft_scope kernel instantiates three constraint readings: narrow_scope_reading (THIS FILE), strong_copyleft_reading, and enforcement_vacuum_reading. They share the same kernel commitment (GPL Section 2(b)) but instantiate different constraints due to divergent interpretations of 'derivative work.' The narrow reading privileges traditional copyright doctrine; strong_copyleft privileges GPL-specific intent; enforcement_vacuum privileges interpretive plurality. Each reading has distinct ε, beneficiary/victim structure, and claimed type. Network links bind the family; the narrow reading influences the strong reading (by narrowing copyleft scope, it creates pressure for explicit license revision toward stronger copyleft) and coexists with the enforcement vacuum reading (both can be true in different jurisdictions or communities).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_copyleft_scope__narrow_scope_reading, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
