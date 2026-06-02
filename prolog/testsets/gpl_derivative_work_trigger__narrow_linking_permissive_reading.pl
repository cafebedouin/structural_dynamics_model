% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__narrow_linking_permissive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__narrow_linking_permissive_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: gpl_derivative_work_trigger__narrow_linking_permissive_reading
 *   human_readable: GPL Derivative Work Trigger: Narrow Linking Permissive Reading
 *   domain: software_licensing/copyright_law/open_source_governance
 *
 * SUMMARY:
 *   The GPL derivative-work trigger is a contested boundary in software
 *   copyright. The narrow reading instantiated here claims that only
 *   modifications to copyrighted GPL code trigger reciprocal licensing
 *   obligations; dynamic linking of proprietary modules does not create a
 *   derivative work. This reading operates as one interpretation of
 *   GPLv2/v3's operative language and creates a licensing wall: proprietary
 *   vendors can integrate GPL libraries (libssl, zlib, libc, Qt) without
 *   disclosing their own source code. The constraint exhibits tangled rope
 *   structure: GPL's coordination function (sharing improvements, building on
 *   prior work) is genuine, but the narrow reading allows proprietary
 *   extraction of that coordination benefit. End users receive binaries with
 *   linked proprietary components whose source remains withheld — they lose
 *   the transparency guarantee GPL intends. The measurement trajectory shows
 *   extractiveness and suppression rising over the interval (t=0 to t=10) as
 *   the narrow reading spreads from legal theory into practice: more vendors
 *   rely on it, more proprietary integrations assume the reading is correct,
 *   and the enforcement cost for FSF governance structures rises as
 *   litigation uncertainty persists.
 *
 * KEY AGENTS:
 *   - Proprietary Software Vendors: Primary beneficiary (institutional/arbitrage) — integrate GPL libraries without source-sharing obligations under narrow reading
 *   - End Users: Primary victim (powerless/trapped) — receive binaries with proprietary-linked components; source visibility guarantee is not fulfilled
 *   - GPL Community (Maintainers/Authors): Secondary victim (moderate/constrained) — lose coordination benefit reciprocity; see their work integrated into closed systems
 *   - FSF / License Governance (SFLC, Courts): Institutional interpreter (institutional/constrained) — maintain formal authority to interpret GPL but face erosion as narrow reading proliferates
 *   - AGPL / Strong Copyleft Movement: Organized counter-position (organized/mobile) — foreclose the narrow reading by explicitly defining network interaction and functional integration as derivative-work triggers
 *   - Analytical Observer: Structural analyst (analytical/analytical) — acknowledges both readings as textually defensible, revealing the kernel ambiguity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.52).
domain_priors:suppression_score(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.65).
domain_priors:theater_ratio(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, tangled_rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "GPL Derivative Work Trigger: Narrow Linking Permissive Reading").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "software_licensing/copyright_law/open_source_governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__narrow_linking_permissive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__narrow_linking_permissive_reading, '8e20bfb3-6707-448d-9b13-9f9c9a21ffcf').
narrative_ontology:cs_kernel_codification('8e20bfb3-6707-448d-9b13-9f9c9a21ffcf', fixed_text).
narrative_ontology:cs_authority_grounding('8e20bfb3-6707-448d-9b13-9f9c9a21ffcf', lineage).
narrative_ontology:cs_interpretation_layer_present('8e20bfb3-6707-448d-9b13-9f9c9a21ffcf').
narrative_ontology:cs_reading_relation('8e20bfb3-6707-448d-9b13-9f9c9a21ffcf', gpl_derivative_work_trigger__broad_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('8e20bfb3-6707-448d-9b13-9f9c9a21ffcf', gpl_derivative_work_trigger__interface_boundary_reading, influences).
narrative_ontology:cs_axiom('8e20bfb3-6707-448d-9b13-9f9c9a21ffcf', foundational, linking_mechanism_exhausts_derivative_work_determination).
narrative_ontology:cs_axiom_status(linking_mechanism_exhausts_derivative_work_determination, holdable).
narrative_ontology:cs_axiom_grounding('8e20bfb3-6707-448d-9b13-9f9c9a21ffcf', linking_mechanism_exhausts_derivative_work_determination, empirically_contingent).
narrative_ontology:cs_axiom('8e20bfb3-6707-448d-9b13-9f9c9a21ffcf', foundational, copyleft_reciprocity_applies_only_to_direct_modifications).
narrative_ontology:cs_axiom_status(copyleft_reciprocity_applies_only_to_direct_modifications, holdable).
narrative_ontology:cs_axiom_grounding('8e20bfb3-6707-448d-9b13-9f9c9a21ffcf', copyleft_reciprocity_applies_only_to_direct_modifications, deontological).
narrative_ontology:cs_reference_frame('8e20bfb3-6707-448d-9b13-9f9c9a21ffcf', broad_copyleft_derivative_work_boundary).
narrative_ontology:cs_drift_state('8e20bfb3-6707-448d-9b13-9f9c9a21ffcf', contemporary_narrow_reading_acceptance, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8e20bfb3-6707-448d-9b13-9f9c9a21ffcf', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, closed_source_integrators).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_copyleft_goal).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, end_users_source_visibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USER TRAPPED BY PROPRIETARY WALL (SNARE) — User receives compiled binary with linked proprietary modules; GPL source code is available for the GPL component only, but the proprietary component's source is withheld. User cannot obtain or modify the full working system. Trapped: cannot exit the proprietary-linked system without abandoning the functionality. High extraction: loses source-code transparency guarantee that GPL promises. Maximum suppression: no legal mechanism forces the proprietary vendor to license or disclose.
constraint_indexing:constraint_classification(gpl_derivative_work_trigger__narrow_linking_permissive_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GPL COMMUNITY DEVELOPER / MAINTAINER (TANGLED ROPE) — GPL maintainers benefit from broad adoption and ecosystem integration (coordination function: GPL code is more useful when linked into larger systems). But the linking boundary allows proprietary extraction: vendors can extract value from GPL code's functionality while withholding reciprocal source. Constrained exit: a maintainer can fork or change license, but faces community backlash and ecosystem fragmentation. Moderate extraction: the coordination benefit is real (broader use), but asymmetric (benefits flow to proprietary downstream more than upstream GPL author).
constraint_indexing:constraint_classification(gpl_derivative_work_trigger__narrow_linking_permissive_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROPRIETARY VENDOR (ROPE) — Under the narrow linking reading, vendor can integrate GPL library via dynamic linking without triggering copyleft obligations. Vendor experiences the constraint as pure coordination: GPL library solves technical problems (cryptography, compression, UI framework); linking is the mechanism that makes integration work. Arbitrage: vendor can choose proprietary or GPL dependencies based on licensing economics. Net beneficiary: extracts GPL functionality without reciprocal source obligation. Effective extraction is low from vendor perspective because the licensing boundary is treated as natural and expected.
constraint_indexing:constraint_classification(gpl_derivative_work_trigger__narrow_linking_permissive_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LICENSE INTERPRETATION GOVERNANCE (PITON) — Institutions claiming to enforce GPL intent (FSF, Software Freedom Law Center, courts) face degraded authority under the narrow reading. Their formal position (derivative works include all linked modules) is contradicted by the narrow reading (only modifications to GPL code trigger obligations). Theater: governance structures issue interpretive guidance, legal opinions, enforcement threats, but the narrow reading's legal plausibility means enforcement is uncertain and costly. The institutional position persists through authority lineage (GPLv2/v3 text) but its operative force has atrophied as the narrow reading proliferates.
constraint_indexing:constraint_classification(gpl_derivative_work_trigger__narrow_linking_permissive_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: STRONG COPYLEFT / NETWORK COMMONS MOVEMENT (SCAFFOLD) — Organized agents (Software Freedom Law Center, AGPL advocates, commons-based peer production communities) see the narrow reading as a temporary coordination failure with a structural sunset. AGPL (Affero GPL) and stricter licensing models deliberately foreclose the narrow-reading strategy by defining 'network interaction' as derivative work trigger. This perspective treats GPLv2/v3's linking ambiguity as a resolved problem: stronger licenses with explicit functional-modification boundaries are establishing a new norm. Sunset: as GPL codebases age and AGPL / service-side GPL norms mature, the linking loophole becomes recognized as a vulnerability rather than a feature. Mobile exit: communities can migrate to explicit copyleft-network frameworks that close this gap.
constraint_indexing:constraint_classification(gpl_derivative_work_trigger__narrow_linking_permissive_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE — KERNEL AMBIGUITY) — From a civilizational/global perspective, the narrow reading is a legitimate textual interpretation of GPLv2/v3's operative language ('derivative works'). The text is ambiguous between narrow (code-level modifications to copyrighted GPL material) and broad (any linked system that includes GPL code). Both readings are defensible under standard copyright law principles. The constraint exhibits tangled rope structure: GPL's coordinating function (sharing improvements) coexists with extraction asymmetry (narrow reading allows proprietary extraction of the coordination benefit). The analytical position is identity-locked in the committer framework: acknowledging that both readings are live means the analytical observer cannot simply declare one interpretation correct without accepting a normative stance on what GPL ought to mean.
constraint_indexing:constraint_classification(gpl_derivative_work_trigger__narrow_linking_permissive_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gpl_derivative_work_trigger__narrow_linking_permissive_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gpl_derivative_work_trigger__narrow_linking_permissive_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gpl_derivative_work_trigger__narrow_linking_permissive_reading, TR),
    TR >= 0.70.

:- end_tests(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The narrow reading extracts substantial value for proprietary vendors — they gain access to tested GPL functionality (cryptographic libraries, compression, standard C library) without reciprocal obligation. The extraction is not maximal because GPL's coordination function is real: GPL libraries do solve legitimate technical problems, and the narrow reading does preserve the GPL ecosystem (vendors do integrate GPL code rather than reject it). The extraction value rises from t=0 (0.35, when the narrow reading was primarily theoretical) to t=10 (0.52, when it is normalized practice across major proprietary software projects). Suppression (0.65): High. The narrow reading suppresses two mechanisms that would otherwise constrain proprietary extraction: (1) the legal/contractual mechanism (FSF enforcement threat, copyleft obligations), rendered ineffective by the reading's textual defensibility; (2) the social/reputational mechanism (community pressure on proprietary integrators), partially suppressed by the reading's apparent legitimacy under copyright law. The suppression rises as the reading gains acceptance and vendors normalize its use. Theater ratio (0.58): Moderate-high. FSF's governance structures (interpretive statements, enforcement letters, license text revisions) are partially performative under this reading — FSF claims the narrow reading is wrong, but lacks clear legal precedent and enforcement success to make the claim stick. The theatrical element is the gap between FSF's formal position (derivative works include all linked modules) and the reading's persistent legal viability. Theater rises as more vendors adopt the narrow reading despite FSF's stated intent, rendering FSF's governance noise rather than binding authority.
 *
 * PERSPECTIVAL GAP:
 *   The constraint manifests as six distinct types depending on the observer's structural position. Proprietary vendors experience pure coordination (Rope) — GPL libraries solve problems, linking is the mechanism. FSF governance structures experience degraded authority (Piton) — their interpretive position is undermined by the reading's legal viability. GPL community members experience mixed extraction and coordination (Tangled Rope) — they benefit from adoption but lose reciprocal obligation. End users experience pure extraction (Snare) — they are trapped in the proprietary-linked system with no source visibility. The strong copyleft movement experiences a solvable coordination problem (Scaffold) — AGPL and stricter copyleft models foreclose the narrow reading and establish a new norm. The analytical observer experiences the genuine kernel ambiguity (Tangled Rope at analytical scope) — acknowledging that both readings are defensible from the text reveals that GPL's derivative-work boundary is not settled, and that the narrow reading extracts value precisely because the text supports it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the structural position: proprietary vendors are beneficiaries with arbitrage options (d ≈ 0.10, low extraction), GPL maintainers are secondary victims with constrained options (d ≈ 0.55, moderate extraction), end users are victims with trapped options (d ≈ 0.90, high extraction), FSF governance is a constrained institutional actor caught between authority claim and reading defensibility (d ≈ 0.60), strong copyleft movement has mobile/organized options to foreclose the reading (d ≈ 0.40, lower extraction), analytical observer has analytical position (d ≈ 0.72, analytical standard). These d values feed the sigmoid f(d) to compute effective extractiveness chi for each perspective. The vendor perspective's low chi (rope classification) reflects that from their position, the licensing mechanism is not experienced as extraction but as normal coordination. The end-user perspective's high chi (snare classification) reflects that from their position, the linking mechanism that the vendor treats as normal is experienced as entrapment.
 *
 * MANDATROPHY ANALYSIS:
 *   The narrow reading resolves mandatrophy by showing that GPL's coordination function is real but the reading enables asymmetric extraction. The constraint is NOT pure coordination (Rope) because proprietary extraction is the operative mechanism — vendors can link without sharing source. The constraint is NOT pure extraction (Snare) because GPL libraries do genuinely solve problems and are voluntarily integrated. The tangled rope classification captures both: the coordination function (GPL code solves technical problems) coexists with extraction asymmetry (narrow reading allows proprietary vendors to benefit without reciprocal obligation). The mandate the narrow reading satisfies is proprietary software vendors' legal interest in integrating GPL functionality without source-sharing cost. The mandate that opposes it is GPL authors' interest in establishing copyleft reciprocity — software freedom through obligatory source sharing. The mandatrophy is not resolved by choosing a single type but by acknowledging that the constraint creates a structural conflict between two incompatible mandates, and that the narrow reading privileges one mandate over the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dynamic_vs_static_linking_boundary,
    'Does the GPL''s ''derivative work'' trigger depend on the linking mechanism (static vs dynamic), or on functional integration regardless of linking mechanism?',
    'Analysis of GPLv2/v3 operative language, copyright case law on integration tests (merger doctrine, combined work doctrine), and license author intent statements. Empirical: how do courts and FSF resolve this in litigation?',
    'If mechanism-dependent: narrow reading is legally defensible, and dynamic linking is a legitimate GPL-compatible interface. If function-dependent: broad reading prevails, and any functional integration triggers copyleft. This is the core axis of the kernel ambiguity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dynamic_vs_static_linking_boundary, empirical, 'Whether derivative work trigger depends on linking mechanism or functional integration').

omega_variable(
    fsf_intent_vs_textual_meaning,
    'Does FSF''s stated intent (all linked modules are derivative works / copyleft applies to the whole system) override the narrow reading''s textual defensibility (only modifications to copyrighted code trigger obligations)?',
    'License drafting history (GPL Preamble, GPLv3 Rationale), FSF enforcement actions vs court outcomes, longitudinal analysis of FSF position statements and their legal weight in jurisdictions.',
    'If intent controls: narrow reading is overridden, FSF governance authority is strengthened, broad reading prevails. If text controls: narrow reading remains defensible even against FSF''s stated intent, creating an authority-text gap that the piton perspective captures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fsf_intent_vs_textual_meaning, conceptual, 'Whether FSF authorial intent overrides the narrow reading''s textual defensibility').

omega_variable(
    network_effects_of_reading_proliferation,
    'As the narrow reading proliferates in proprietary integrations, does it create strong network effects that entrench the reading (proprietary vendors depend on the narrow boundary, making reversal costly), or does the broad reading''s ecosystem resilience withstand the fragmentation?',
    'Empirical: count of major proprietary projects using GPL dependencies under narrow-reading assumptions. Analysis of license-switching rates (GPL → Apache 2.0, MIT) by GPL authors responding to narrow-reading extraction. Modeling of ecosystem fragmentation trajectory.',
    'If strong network effects entrench narrow reading: the scaffold perspective''s sunset is delayed or prevented; broad reading becomes an unrecoverable historical position. If resilient: AGPL / stronger copyleft adoption rate increases, and the narrow reading is isolated to specific use cases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effects_of_reading_proliferation, empirical, 'Whether narrow reading''s network effects entrench or the broad reading''s ecosystem absorbs fragmentation').

omega_variable(
    committer_reading_kernel_ambiguity,
    'Is this a genuine kernel ambiguity (both readings are live under the same text), or has one reading foreclosed the other through case law, FSF enforcement success, or author intent recognition?',
    'Legal historical analysis: identify the moment (if any) when the narrow reading transitioned from ''possible interpretation'' to ''established reading'' or vice versa. Empirical corpus of court decisions, FSF enforcement letters, and license author statements.',
    'If kernel remains open: both readings coexist, committer frame applies, omega_variable routing is correct. If one reading is foreclosed: the constraint should be reclassified as a single-reading story with the foreclosed reading moved to historical commentary. If closed by FSF governance authority: the piton perspective''s degradation is temporary — authority can be recovered through litigation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_reading_kernel_ambiguity, conceptual, 'Whether the GPL derivative-work boundary is a genuine open kernel or a foreclosed reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_narrow_theater_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(gpl_narrow_theater_t5, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 5, 0.55).
narrative_ontology:measurement(gpl_narrow_theater_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(gpl_narrow_extract_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gpl_narrow_extract_t5, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(gpl_narrow_extract_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(gpl_narrow_suppress_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(gpl_narrow_suppress_t5, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(gpl_narrow_suppress_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__narrow_linking_permissive_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.18).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger__broad_copyleft_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger__interface_boundary_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_linking_wall).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, software_license_capture).

% DUAL FORMULATION NOTE:
% The GPL derivative-work kernel decomposes into three constraint stories, one per reading. This story (narrow_linking_permissive_reading) has ε=0.52 (tangled rope); the broad_copyleft_reading has ε=0.32 (snare from proprietary perspective, rope from GPL perspective) — lower extraction because the broad reading forecloses the vendor benefit; the interface_boundary_reading has ε≈0.45 (contested boundary, high theater). All three are linked via the common kernel. Each story's ε differs because the reading changes who benefits and who bears extraction cost. The narrow reading maximizes vendor benefit, so it has the highest ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_derivative_work_trigger__narrow_linking_permissive_reading, institutional, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
