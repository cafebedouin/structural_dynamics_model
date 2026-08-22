% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__narrow_scope_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-17
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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: gpl_copyleft_scope__narrow_scope_reading
 *   human_readable: GPL Section 2(b) — Narrow Copyleft Scope Reading
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   The narrow scope reading of GPL Section 2(b) treats the copyleft
 *   obligation as attaching only to works that are derivative in the
 *   traditional copyright sense — modifications, translations, and direct
 *   adaptations of the licensed code itself. Mere aggregation (distributing
 *   GPL and non-GPL code together on the same medium), plugin architectures
 *   (well-defined interfaces loading separate modules), and dynamic linking
 *   against GPL libraries are not themselves derivative works and therefore
 *   do not trigger the copyleft. This reading follows the FSF's early FAQ
 *   guidance and the GPLv2 text's reference to 'works based on the Program'
 *   interpreted through established copyright law. It enables commercial
 *   firms to incorporate GPL components into larger proprietary systems
 *   without being forced to license the entire system under GPL, creating a
 *   coordination layer that allows mixed codebases to coexist. Copyleft
 *   advocates who expect universal code-sharing find their expectations
 *   structurally weakened under this reading, and enforcement actions against
 *   dynamic linking patterns remain rare to non-existent.
 *
 * KEY AGENTS:
 *   - commercial_firms_using_gpl_components: Primary beneficiary (powerful/arbitrage) — retains flexibility to integrate GPL code with proprietary layers
 *   - gpl_licensors_accepting_narrow_interpretation: Secondary beneficiary (organized/constrained) — gains wider adoption of their code by not demanding full copyleft propagation
 *   - copyleft_advocates_fsf_aligned: Excluded (powerful/constrained) — would object to narrow reading but lack enforcement capacity against industry practice
 *   - downstream_users_of_mixed_codebases: Beneficiary (organized/mobile) — receives functional software without license contagion
 *   - legal_courts_judicial_system: Observer (institutional/analytical) — ultimate arbiter but has produced little precedent
 *   - competitors_using_strong_copyleft: Payer (moderate/constrained) — bears competitive pressure from firms using narrow-reading integrations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__narrow_scope_reading, 0.18).
domain_priors:suppression_score(gpl_copyleft_scope__narrow_scope_reading, 0.12).
domain_priors:theater_ratio(gpl_copyleft_scope__narrow_scope_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__narrow_scope_reading, rope).
narrative_ontology:human_readable(gpl_copyleft_scope__narrow_scope_reading, "GPL Section 2(b) — Narrow Copyleft Scope Reading").
narrative_ontology:topic_domain(gpl_copyleft_scope__narrow_scope_reading, "software_licensing/intellectual_property/open_source_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__narrow_scope_reading, '751e4ff6-73ae-43e4-9ccd-35a9748b325f').
narrative_ontology:cs_kernel_codification('751e4ff6-73ae-43e4-9ccd-35a9748b325f', fixed_text).
narrative_ontology:cs_authority_grounding('751e4ff6-73ae-43e4-9ccd-35a9748b325f', lineage).
narrative_ontology:cs_interpretation_layer_present('751e4ff6-73ae-43e4-9ccd-35a9748b325f').
narrative_ontology:cs_reading_relation('751e4ff6-73ae-43e4-9ccd-35a9748b325f', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('751e4ff6-73ae-43e4-9ccd-35a9748b325f', gpl_copyleft_scope__enforcement_vacuum_reading, influences).
narrative_ontology:cs_axiom('751e4ff6-73ae-43e4-9ccd-35a9748b325f', foundational, derivative_work_follows_copyright_doctrine).
narrative_ontology:cs_axiom_status(derivative_work_follows_copyright_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('751e4ff6-73ae-43e4-9ccd-35a9748b325f', derivative_work_follows_copyright_doctrine, conventional).
narrative_ontology:cs_axiom('751e4ff6-73ae-43e4-9ccd-35a9748b325f', foundational, aggregation_and_linking_not_derivative).
narrative_ontology:cs_axiom_status(aggregation_and_linking_not_derivative, holdable).
narrative_ontology:cs_axiom_grounding('751e4ff6-73ae-43e4-9ccd-35a9748b325f', aggregation_and_linking_not_derivative, conventional).
narrative_ontology:cs_reference_frame('751e4ff6-73ae-43e4-9ccd-35a9748b325f', gplv2_section2b_traditional_copyright_boundary).
narrative_ontology:cs_drift_state('751e4ff6-73ae-43e4-9ccd-35a9748b325f', contemporary_mixed_codebase_ecosystem, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('751e4ff6-73ae-43e4-9ccd-35a9748b325f', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, commercial_firms_using_gpl_components).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, gpl_licensors_accepting_narrow_interpretation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, downstream_users_of_mixed_codebases).
narrative_ontology:constraint_victim(gpl_copyleft_scope__narrow_scope_reading, competitors_using_strong_copyleft).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__narrow_scope_reading, traditional_copyright_derivative_work_doctrine).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__narrow_scope_reading, permissionless_innovation_via_aggregation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Integrates GPL-licensed libraries and components into proprietary products via dynamic linking, plugin architectures, and aggregation. Retains full proprietary control over non-GPL layers. Can switch to permissively licensed alternatives (BSD, MIT, Apache) or negotiate commercial licenses if GPL terms become unfavorable. The narrow reading allows this integration without license contagion.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, commercial_firms_using_gpl_components, beneficiary,
    powerful, biographical, arbitrage, global).

% Authors and maintainers of GPL code who accept or do not contest the narrow reading. They benefit from wider adoption of their code in commercial products, which drives contributions, testing, and ecosystem relevance. Their exit option is relicensing (difficult for projects with many contributors) or shifting to a stronger copyleft license (GPLv3, AGPL) — constrained by contributor agreements and community expectations.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, gpl_licensors_accepting_narrow_interpretation, beneficiary,
    organized, generational, constrained, global).

% Advocates and organizations (FSF, SFLC, copyleft-focused legal clinics) who hold the strong copyleft reading as the correct interpretation. They would object to commercial integrations that rely on the narrow reading, but their enforcement capacity is limited to copyright holders who assign them enforcement rights. Their identity is fused to the strong reading — abandoning it would dissolve their organizational purpose and professional reputation.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, copyleft_advocates_fsf_aligned, excluded,
    powerful, generational, identity_locked, global).

% End users and organizations deploying software that combines GPL and proprietary components under the narrow reading. They receive functional, supported software without being forced into GPL compliance for their own proprietary layers. Can switch between competing products (mobile exit) since the narrow reading enables a competitive market of mixed-codebase products.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, downstream_users_of_mixed_codebases, beneficiary,
    organized, biographical, mobile, global).

% Courts that would adjudicate GPL enforcement cases involving dynamic linking, plugin boundaries, and aggregation. Have produced minimal precedent (notable: no US appellate ruling on GPL dynamic linking; VMware v. Hellwig settled; Artifex v. Hancom addressed ghostscript but not general dynamic linking). Their eventual rulings would structurally resolve the kernel contest.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, legal_courts_judicial_system, observer,
    institutional, civilizational, analytical, national).

% Firms and projects that adopt the strong copyleft reading (or AGPL) and therefore must open-source their entire combined work when integrating GPL code. They bear a competitive cost asymmetry: their products must be fully open while narrow-reading competitors keep proprietary layers. Can adopt the narrow reading themselves (constrained by their own licensing choices and community commitments) or differentiate on 'pure copyleft' positioning.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, competitors_using_strong_copyleft, payer,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, legally grounded boundary for when GPL copyleft obligations attach — enabling developers and firms to combine GPL code with proprietary code without license contagion, solving the 'mixed codebase' coordination problem that would otherwise force all-or-nothing licensing choices.
% TRANSFER_FUNCTION: Moves the obligation to share modifications only for the GPL-licensed work itself (direct derivatives), not for independent works that merely aggregate, dynamically link, or plugin-interface with it. The transfer is: modifications to GPL code flow back to the commons; everything else stays with its author.
% ABSENT_VOICES: End users who might benefit from stronger copyleft (full source access to entire software stacks) are structurally absent — they are not parties to the licensing decision. Future generations of developers who would inherit a fully-free software ecosystem under strong copyleft are also absent. Both would object to the narrow reading if present, but have no seat at the table.
% DISAPPEARANCE_RATIONALE: If the narrow reading vanished overnight and only the strong reading remained, commercial firms would face immediate license contagion risk for all dynamic linking and plugin integrations. Many would rewrite GPL components, switch to permissive alternatives, or negotiate commercial licenses — the mixed-codebase ecosystem would collapse or reorganize around permissive licenses. The GPL commons would lose the contributions driven by commercial adoption under the narrow reading.
% FOUNDING_PROBLEM: Early free software movement needed a license that prevented proprietary forks of the *same codebase* while allowing the code to be used as a building block in larger systems. The narrow reading solves the first without blocking the second.
% FOUNDING_PROBLEM_CORROBORATION: The FSF's early FAQ (1990s) explicitly stated dynamic linking does not create derivative works — corroborating the narrow reading as original intent. However, Stallman's later statements (2000s+) and GPLv3 drafting history endorse the strong reading, contesting the founding problem's current status. Independent legal scholars (e.g., Pamela Samuelson, Lawrence Rosen) have argued both sides; no consensus exists outside the benefiting parties.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__narrow_scope_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__narrow_scope_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__narrow_scope_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(gpl_copyleft_scope__narrow_scope_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__narrow_scope_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.18) because the narrow reading imposes minimal transfer obligation — firms keep their proprietary layers, and the only extraction is the requirement to share modifications to the GPL code itself (a coordination cost, not a rent). Suppression is low (0.12) because alternatives (rewriting the GPL component, using permissively licensed alternatives, negotiating commercial licenses) remain accessible; the constraint does not close exits. Theater ratio is minimal (0.08) — the license text and community practice align closely; there is little performative enforcement of a broader scope. Accessibility collapse is low (0.25) because the boundary between derivative work and aggregation is reasonably clear in traditional doctrine, and developers can navigate it. Resistance is low (0.22) — the reading is the path of least resistance for commercial adoption and faces little organized opposition from firms; the primary resistance comes from ideological copyleft advocates who lack structural enforcement power.
 *
 * PERSPECTIVAL GAP:
 *   From the commercial firm seat, this reading is a genuine coordination mechanism (rope) enabling mixed codebases with clear boundaries. From the FSF-aligned advocate seat, the same reading is a structural weakening of copyleft's universalizing ambition — they experience it as a loss of expected code-sharing, but their exit options are constrained (ideological commitment, reputational capital tied to strong copyleft). The engine will compute this seat divergence from the structural data: the commercial firm has arbitrage-grade exit (can choose permissive alternatives), while the advocate is identity_locked to the strong copyleft framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Commercial firms using GPL components are structural beneficiaries (d ~ 0.15) — they extract value from GPL code without surrendering their proprietary layers, and their exit options are strong (arbitrage: permissive alternatives, commercial licenses, rewrites). GPL licensors accepting narrow interpretation are moderate beneficiaries (d ~ 0.3) — they gain adoption but cede some copyleft reach. Copyleft advocates are effectively excluded/payers in a diffuse sense (d ~ 0.65) — their expected code-sharing is not realized, but they are not directly extracted from; they are identity_locked to the strong reading. Downstream users are beneficiaries (d ~ 0.2) — they get working software without license contagion. Courts are analytical observers (d = 0.5). Competitors using strong copyleft are moderate payers (d ~ 0.55) — they bear competitive asymmetry but can adopt the narrow reading themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   The narrow reading prevents mislabeling the GPL's coordination function as pure extraction: the license genuinely solves the problem of 'how do we share improvements to this specific codebase?' without demanding universal code liberation. The mandatrophy risk runs the other way — the strong copyleft reading risks labeling a coordination mechanism (share improvements to the GPL work) as an extraction mechanism (claim ownership of everything that touches it). This reading resolves that by anchoring to traditional copyright doctrine, which has independent legitimacy as a coordination standard.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does the narrow scope reading instantiate a structurally distinct constraint from the strong copyleft reading, or are they observer-relative views of the same constraint?',
    'Apply the ε-invariance test: if measuring the constraint via commercial firm behavior yields ε ≈ 0.18 (low extraction) while measuring via copyleft advocate expectations yields ε ≈ 0.7 (high extraction), then there are two constraints with different ε referents, not one constraint with observer-dependent classification.',
    'If they are one constraint, the framework must model observer-relative ε — violating ε-invariance. If two constraints, they must be decomposed (as done here) and linked via network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the GPL copyleft scope kernel decomposes into multiple ε-invariant constraints.').

omega_variable(
    judicial_precedent_gap,
    'Will eventual judicial precedent on dynamic linking and plugin architectures collapse the narrow/strong distinction, or will the legal system sustain the plurality?',
    'Track appellate and supreme court rulings on GPL enforcement involving dynamic linking (e.g., VMware v. Hellwig, any future FSF enforcement actions). A definitive ruling adopting one boundary would foreclose the other reading''s structural viability in that jurisdiction.',
    'If courts adopt the strong boundary, the narrow reading becomes legally non-viable in that jurisdiction (its coordination function becomes legally risky). If courts adopt the narrow boundary, the strong reading becomes a purely normative position without legal teeth. Either outcome resolves the enforcement_vacuum_reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_precedent_gap, empirical, 'Whether judicial precedent will resolve the derivative work boundary for GPL code.').

omega_variable(
    commercial_adoption_dependency,
    'Is the narrow reading''s coordination function genuinely enabling commercial adoption, or is it a post-hoc rationalization by firms that would use GPL code regardless?',
    'Compare adoption trajectories of GPL vs permissively licensed components in commercial products before/after narrow reading became dominant interpretation (post-2000s). If GPL adoption tracks permissive adoption only under narrow reading, the coordination function is real.',
    'If the coordination function is illusory, the narrow reading''s claimed_type (rope) is falsified — the constraint would be a mountain (copyright doctrine applies regardless) with near-zero extraction either way.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_adoption_dependency, empirical, 'Whether the narrow reading''s coordination benefit is causal or correlational for commercial GPL adoption.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__narrow_scope_reading, 1991, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_tr_t1991, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 1991, 0.03).
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_tr_t1999, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 1999, 0.04).
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_tr_t2007, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 2007, 0.06).
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_tr_t2015, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 2015, 0.07).
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_tr_t2024, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 2024, 0.08).

% Extraction over time
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_be_t1991, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 1991, 0.12).
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_be_t1999, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 1999, 0.14).
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_be_t2007, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 2007, 0.16).
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_be_t2015, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 2015, 0.17).
narrative_ontology:measurement(gpl_copyleft_scope__narrow_scope_reading_be_t2024, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 2024, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(gpl_copyleft_scope__narrow_scope_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__narrow_scope_reading, information_standard).
narrative_ontology:boltzmann_floor_override(gpl_copyleft_scope__narrow_scope_reading, 0.02).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope__strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope__enforcement_vacuum_reading).

% DUAL FORMULATION NOTE:
% This narrow_scope_reading and the strong_copyleft_reading are two ε-invariant constraints decomposed from the 'GPL copyleft scope' colloquial label (BGS pattern). The narrow reading has ε ≈ 0.18 (rope: genuine coordination, minimal extraction). The strong reading has ε ≈ 0.65+ (tangled_rope or snare: substantial extraction from firms that dynamically link, enforced by FSF-aligned projects). The enforcement_vacuum_reading models the meta-constraint: the absence of judicial precedent allows both to operate as licensed plurality. All three are linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_copyleft_scope__narrow_scope_reading, powerful, 0.15).
constraint_indexing:directionality_override(gpl_copyleft_scope__narrow_scope_reading, organized, 0.3).
constraint_indexing:directionality_override(gpl_copyleft_scope__narrow_scope_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
