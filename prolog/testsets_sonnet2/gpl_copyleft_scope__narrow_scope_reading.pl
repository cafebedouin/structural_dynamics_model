% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__narrow_scope_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: GPL Section 2(b) Narrow-Scope (Traditional Derivative-Work Boundary) Reading
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   The GNU GPL's Section 2(b) is a single textual kernel read in
 *   structurally incompatible ways by different communities. This story
 *   instantiates the NARROW-SCOPE reading: derivative-work boundaries under
 *   Section 2(b) track ordinary copyright doctrine (substantial similarity,
 *   functional separability), so mere aggregation, well-defined plugin
 *   architectures, and many forms of dynamic linking fall outside the
 *   copyleft trigger. This reading is the one commercial software ecosystems
 *   have largely operationalized in practice — dual-licensing businesses,
 *   proprietary-plugin vendors, and mixed-codebase commercial products all
 *   depend on courts and industry practice continuing to read the boundary
 *   narrowly. The sibling readings (strong_copyleft_reading,
 *   enforcement_vacuum_reading) are separate constraint stories with their
 *   own ε and stakeholder structures; this story does not average over them
 *   or describe the contest internally, per the ε-invariance discipline.
 *
 * KEY AGENTS:
 *   - commercial_integrators: primary beneficiary (powerful/arbitrage) — captures value from GPL code without reciprocity obligations under the narrow boundary
 *   - small_gpl_contributors: primary target (powerless/trapped) — licensed expecting broad reciprocity that the narrow reading structurally denies them
 *   - copyleft_advocacy_organizations: secondary payer (organized/constrained) — sees its founding mission narrowed by prevailing interpretation
 *   - courts_and_legal_scholars: analytical observer (institutional/analytical) — the doctrinal mechanism producing and sustaining this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__narrow_scope_reading, 0.32).
domain_priors:suppression_score(gpl_copyleft_scope__narrow_scope_reading, 0.28).
domain_priors:theater_ratio(gpl_copyleft_scope__narrow_scope_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__narrow_scope_reading, rope).
narrative_ontology:human_readable(gpl_copyleft_scope__narrow_scope_reading, "GPL Section 2(b) Narrow-Scope (Traditional Derivative-Work Boundary) Reading").
narrative_ontology:topic_domain(gpl_copyleft_scope__narrow_scope_reading, "software_licensing/intellectual_property/open_source_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__narrow_scope_reading, '2f0957ce-8c1f-4b1d-8683-424268141d2f').
narrative_ontology:cs_kernel_codification('2f0957ce-8c1f-4b1d-8683-424268141d2f', fixed_text).
narrative_ontology:cs_authority_grounding('2f0957ce-8c1f-4b1d-8683-424268141d2f', practice).
narrative_ontology:cs_interpretation_layer_present('2f0957ce-8c1f-4b1d-8683-424268141d2f').
narrative_ontology:cs_reading_relation('2f0957ce-8c1f-4b1d-8683-424268141d2f', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f0957ce-8c1f-4b1d-8683-424268141d2f', gpl_copyleft_scope__enforcement_vacuum_reading, influences).
narrative_ontology:cs_axiom('2f0957ce-8c1f-4b1d-8683-424268141d2f', foundational, derivative_work_boundary_tracks_ordinary_copyright_doctrine).
narrative_ontology:cs_axiom_status(derivative_work_boundary_tracks_ordinary_copyright_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('2f0957ce-8c1f-4b1d-8683-424268141d2f', derivative_work_boundary_tracks_ordinary_copyright_doctrine, conventional).
narrative_ontology:cs_axiom('2f0957ce-8c1f-4b1d-8683-424268141d2f', secondary, functional_separability_defeats_copyleft_propagation).
narrative_ontology:cs_axiom_status(functional_separability_defeats_copyleft_propagation, holdable).
narrative_ontology:cs_axiom_grounding('2f0957ce-8c1f-4b1d-8683-424268141d2f', functional_separability_defeats_copyleft_propagation, conventional).
narrative_ontology:cs_reference_frame('2f0957ce-8c1f-4b1d-8683-424268141d2f', traditional_copyright_derivative_work_doctrine).
narrative_ontology:cs_drift_state('2f0957ce-8c1f-4b1d-8683-424268141d2f', contemporary_commercial_foss_ecosystem, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2f0957ce-8c1f-4b1d-8683-424268141d2f', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, commercial_integrators).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, proprietary_plugin_vendors).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, dual_licensing_vendors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__narrow_scope_reading, copyleft_advocacy_organizations).
narrative_ontology:constraint_victim(gpl_copyleft_scope__narrow_scope_reading, small_gpl_contributors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gpl_copyleft_scope__narrow_scope_reading, gpl_project_maintainers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ship products that link against or aggregate GPL components alongside proprietary code, relying on the narrow reading to avoid triggering copyleft on their own layers. Can architect around the boundary (plugin interfaces, process separation, dynamic linking) using ordinary engineering choices rather than legal risk mitigation. Face little practical enforcement exposure under this reading.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, commercial_integrators, beneficiary,
    powerful, biographical, arbitrage, global).

% Build closed-source plugins against GPL-licensed host applications. Their entire business model depends on the plugin architecture exception holding; under the strong reading their products would be unlicensable as sold. Under this reading they operate openly and advertise GPL compatibility.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, proprietary_plugin_vendors, beneficiary,
    organized, biographical, mobile, global).

% Companies that release core code under GPL but sell proprietary licenses to commercial integrators who don't want copyleft obligations. A narrow reading of the derivative-work boundary is commercially convenient in some contexts (clarifies what triggers the paid license) but the same firms sometimes prefer ambiguity that pushes customers toward paid licenses; here treated as agenda-adjacent because their commercial licensing terms implicitly ratify and rely on courts continuing to read the boundary narrowly rather than expansively.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, dual_licensing_vendors, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__narrow_scope_reading, dual_licensing_vendors, agenda_setter).

% Organizations built around the premise that copyleft propagates through combination, not just literal copying, to keep improvements in the commons. The narrow reading structurally defeats the expectation that dynamically-linked or plugin-coupled code must also be shared, shrinking the practical reach of every GPL project's copyleft guarantee. They can litigate or lobby for reform but cannot unilaterally change how courts or commercial actors read Section 2(b).
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, copyleft_advocacy_organizations, payer,
    organized, civilizational, constrained, global).

% Individual developers who license their code GPL expecting reciprocity — that anyone building substantially on their work must share improvements back. Under the narrow reading, commercial actors can integrate their code behind a plugin boundary or dynamic link and never contribute anything back, and the contributor has no realistic means to challenge that interpretation (litigation cost, uncertain precedent). They bear the gap between the license's rhetorical promise and its narrow legal reach.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, small_gpl_contributors, payer,
    powerless, biographical, trapped, global).

% Maintain widely-used GPL codebases (kernels, libraries, frameworks) and must decide licensing strategy — some adopt LGPL or explicit linking exceptions specifically because the narrow reading of GPL Section 2(b) leaves ambiguity they'd rather resolve explicitly. They observe the interpretive contest directly but also bear its costs when downstream commercial use diverges from their intent.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, gpl_project_maintainers, observer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__narrow_scope_reading, gpl_project_maintainers, payer).

% Adjudicate or analyze derivative-work boundary disputes when they reach litigation, applying traditional copyright doctrine (substantial similarity, independent creation, functional separability) largely unmodified by the GPL's coordination goals. Their doctrinal conservatism is the mechanism that produces and sustains the narrow reading.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, courts_and_legal_scholars, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides commercial software producers a predictable, legally-grounded line for combining open and proprietary code: components that are merely aggregated, linked through a defined plugin interface, or dynamically linked without deep integration can be treated as separate works, letting mixed codebases exist without wholesale relicensing.
% TRANSFER_FUNCTION: Moves the practical benefit of the GPL's reciprocity guarantee away from the commons and toward commercial integrators and plugin vendors who can capture the value of GPL-licensed work without contributing improvements back, at the expense of contributors and advocacy organizations who licensed on the expectation of broader reciprocity.
% ABSENT_VOICES: Small individual contributors who chose GPL specifically to compel sharing are not parties to the interpretive contest — courts hear commercial litigants and advocacy amici, not the individual developer whose code sits three plugin-interfaces deep in a commercial product with no realistic path to assert a claim.
% DISAPPEARANCE_RATIONALE: If the narrow-scope reading were displaced by the strong-copyleft reading (via binding precedent or FSF-favorable test litigation), a substantial share of commercial software that currently integrates GPL components behind plugin or dynamic-linking boundaries would face relicensing, re-architecture, or removal of the GPL dependency entirely — a real rearrangement of commercial dependency graphs, not a cosmetic one.
% FOUNDING_PROBLEM: The GPL was drafted to ensure that anyone who builds on and distributes a covered program keeps the resulting work free, preventing proprietary capture of community-developed code; Section 2(b) was the clause meant to define how far that reciprocity obligation extends into combined works.
% FOUNDING_PROBLEM_CORROBORATION: Copyleft advocacy organizations and many original GPL contributors attest the founding problem (preventing proprietary capture of derivative improvements) remains live and is being defeated by narrow judicial and commercial readings. Independent legal scholars analyzing derivative-work doctrine corroborate that the narrow reading tracks ordinary copyright precedent rather than the license drafters' stated intent, documenting a real gap between founding purpose and current operative scope — this corroboration comes from academic commentary outside both the advocacy and commercial-beneficiary camps.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__narrow_scope_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__narrow_scope_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__narrow_scope_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_copyleft_scope__narrow_scope_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__narrow_scope_reading, 0.32, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is moderate (0.32 at interval end) rather than high: the narrow reading does not seize anything from contributors that copyright law itself would have granted them, it simply declines to extend the GPL's reciprocity obligation beyond what traditional derivative-work doctrine would recognize elsewhere. Suppression is comparatively low (0.28) because the reading operates through ordinary legal reasoning and industry practice rather than active coercion — no one is forced to accept the narrow reading, but the absence of contrary precedent and the cost of litigation function as passive suppression of the alternative. Theater ratio is low-moderate (0.22) and rising slightly, reflecting some performative 'GPL-compliant' labeling by vendors whose actual compliance rests entirely on the plugin-boundary exception holding.
 *
 * PERSPECTIVAL GAP:
 *   From the commercial integrator's seat, this is a rope: a workable coordination mechanism letting mixed proprietary/open codebases exist, with genuine mutual benefit (open infrastructure gets adopted and improved by well-resourced commercial users even without contribution mandates). From the small contributor's seat, the same textual and doctrinal apparatus functions closer to a one-way transfer — value flows out under a license they believed guaranteed reciprocity. The engine should register this divergence rather than resolve it; the divergence is the finding.
 *
 * DIRECTIONALITY LOGIC:
 *   Commercial integrators and plugin vendors sit near the beneficiary end: they receive the coordination benefit (legal clarity, integration flexibility) with the extraction running in their favor — they capture value without the reciprocity cost the license's drafters intended. Small individual GPL contributors sit near the target end: trapped by the practical reality that they cannot litigate to enforce a broader reading, and their license grant's intended reciprocity effect is diminished by a boundary they did not choose and cannot move. Copyleft advocacy organizations are organized but still payers — they can lobby and litigate but cannot unilaterally resolve the interpretive question.
 *
 * MANDATROPHY ANALYSIS:
 *   The narrow reading is not itself a mandatrophy case in the classic sense (an institution persisting past its function) — it is better understood as a live interpretive settlement that has drifted from the license drafters' apparent intent while remaining textually defensible. The founding_problem framing captures this: the problem (preventing proprietary capture of collaboratively improved code) is still live by advocates' lights, but the operative doctrine addressing it has narrowed, producing a status of 'contested' rather than 'dead' — the arrangement has not lost its rationale, but its practical reach has shrunk relative to that rationale.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_stability_of_narrow_reading,
    'Is the narrow reading a stable settlement grounded in binding precedent, or a contingent industry practice that untested litigation could overturn?',
    'Track appellate-level GPL derivative-work litigation (there is very little to date); a single well-reasoned appellate opinion adopting the strong reading''s coupling test would substantially destabilize this reading''s practical dominance.',
    'If the reading is doctrinally fragile rather than settled, commercial actors currently relying on it (dual-licensing vendors, plugin ecosystems) carry latent legal risk that the current low-extraction, low-suppression profile understates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_stability_of_narrow_reading, empirical, 'Whether the narrow reading rests on settled precedent or untested industry assumption.').

omega_variable(
    drafter_intent_vs_textual_scope,
    'Does the narrow reading correctly implement the GPL drafters'' intent, or does it represent a divergence between the license''s rhetorical goal (universal reciprocity) and its actual enforceable textual scope under ordinary copyright doctrine?',
    'Historical analysis of FSF drafting history, contemporaneous commentary from Richard Stallman and license co-authors, and comparison to how LGPL was drafted specifically to address the linking ambiguity Section 2(b) leaves open.',
    'If the narrow reading diverges substantially from drafter intent, this constraint is better understood as a doctrinal drift outcome (traditional copyright doctrine overriding the license''s coordination purpose) rather than a reading the license itself straightforwardly supports — this would strengthen the case that the founding_problem_status should trend toward ''dead'' for the copyleft-as-drafted mechanism even though the underlying problem (proprietary capture) remains live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drafter_intent_vs_textual_scope, conceptual, 'Whether the narrow reading tracks or diverges from the GPL''s drafting intent.').

omega_variable(
    cs_framing_kernel_vs_legitimacy_narrative,
    'Should the kernel here be treated as the GPL''s Section 2(b) TEXT (fixed_text framing), or as the broader legitimacy narrative of ''copyleft as guarantor of software freedom'' that the text is read to serve (a narrative-authority framing)?',
    'Compare how outcomes differ: under the fixed_text framing, courts interpret the words of Section 2(b) using ordinary doctrine (which produces the narrow reading observed here); under a narrative-authority framing, courts or communities might read the clause purposively against the FSF''s stated software-freedom mission (which would tend to produce the strong reading instead).',
    'The fixed_text framing was adopted for this story because litigation and commercial practice both engage with the clause''s literal text rather than an explicit FSF mission statement; had the narrative-authority framing been adopted instead, this reading''s classification would likely shift toward a more contested or tangled_rope profile, since the coordination function would then be measured against an aspirational standard the narrow reading visibly falls short of.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_legitimacy_narrative, conceptual, 'Alternative CS framings (fixed-text vs. purposive-narrative) that would change classification if adopted.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__narrow_scope_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gpl__tr_t5, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 5, 0.13).
narrative_ontology:measurement(gpl__tr_t10, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(gpl__tr_t15, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(gpl__tr_t20, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(gpl__tr_t25, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 25, 0.22).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(gpl__be_t5, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(gpl__be_t10, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 10, 0.26).
narrative_ontology:measurement(gpl__be_t15, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 15, 0.29).
narrative_ontology:measurement(gpl__be_t20, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement(gpl__be_t25, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 25, 0.32).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(gpl_copyleft_scope__narrow_scope_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__narrow_scope_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gpl_copyleft_scope__narrow_scope_reading, 0.12).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, enforcement_vacuum_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the gpl_copyleft_scope kernel (GNU GPL Section 2(b)). narrow_scope_reading (this story) treats the derivative-work boundary as governed by traditional copyright doctrine, producing moderate ε and a rope-leaning coordination structure for commercial integrators. strong_copyleft_reading treats the same clause as extending to any combined or dynamically-linked work, producing a materially different beneficiary/victim structure (favoring contributors, constraining commercial integrators) and higher claimed extraction from the commercial-integrator seat. enforcement_vacuum_reading treats the absence of binding precedent itself as the operative constraint, with practical effect determined by which interpretive community has local enforcement capacity. All three share the same textual kernel but are authored as separate constraints with independent ε per the ε-invariance principle, linked here for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_copyleft_scope__narrow_scope_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
