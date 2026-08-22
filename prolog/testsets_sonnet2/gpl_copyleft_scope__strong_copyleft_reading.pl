% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__strong_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__strong_copyleft_reading, []).

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
 *   constraint_id: gpl_copyleft_scope__strong_copyleft_reading
 *   human_readable: GPL Section 2(b) Strong Copyleft Reading (Expansive Derivative-Work Boundary)
 *   domain: software_licensing/intellectual_property
 *
 * SUMMARY:
 *   This story instantiates the strong copyleft reading of GPL Section 2(b):
 *   that any combined or dynamically linked work incorporating a GPL
 *   component becomes itself a derivative work subject to GPL's
 *   source-disclosure obligations, regardless of the mechanism of coupling.
 *   This reading is asserted aggressively by FSF-aligned enforcement bodies
 *   and treated as settled by much of the copyleft community, but it has
 *   never been definitively confirmed by controlling appellate precedent in
 *   most jurisdictions for the dynamic-linking case specifically. Under this
 *   reading, proprietary vendors integrating GPL components face structural
 *   exclusion from commercial closed-source use without full source release,
 *   dual-licensing negotiation, or reimplementation — a high-epsilon
 *   extraction pattern enforced through litigation-cost asymmetry rather than
 *   judicial certainty. This is one reading among three of the same kernel
 *   (gpl_copyleft_scope); the narrow_scope_reading and
 *   enforcement_vacuum_reading are separate constraints with their own ε
 *   values, not alternative framings folded into this one.
 *
 * KEY AGENTS:
 *   - free_software_foundation: agenda-setter and primary beneficiary, institutional power, analytical exit (sets the doctrine)
 *   - proprietary_software_vendors: primary target, powerful but constrained exit (commercially costly to avoid)
 *   - commercial_plugin_developers and embedded_systems_integrators: secondary targets, moderate power, trapped/constrained exit (fewer resources to contest the boundary)
 *   - enforcement_organizations: agenda-setters who operationalize the reading through settlement pressure
 *   - courts_and_legal_scholars: analytical observers noting the doctrine remains substantially untested
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, 0.68).
domain_priors:suppression_score(gpl_copyleft_scope__strong_copyleft_reading, 0.62).
domain_priors:theater_ratio(gpl_copyleft_scope__strong_copyleft_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__strong_copyleft_reading, snare).
narrative_ontology:human_readable(gpl_copyleft_scope__strong_copyleft_reading, "GPL Section 2(b) Strong Copyleft Reading (Expansive Derivative-Work Boundary)").
narrative_ontology:topic_domain(gpl_copyleft_scope__strong_copyleft_reading, "software_licensing/intellectual_property").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__strong_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__strong_copyleft_reading, '04baa3f7-074b-488f-aef8-eeee430dd502').
narrative_ontology:cs_kernel_codification('04baa3f7-074b-488f-aef8-eeee430dd502', fixed_text).
narrative_ontology:cs_authority_grounding('04baa3f7-074b-488f-aef8-eeee430dd502', extraction).
narrative_ontology:cs_interpretation_layer_present('04baa3f7-074b-488f-aef8-eeee430dd502').
narrative_ontology:cs_reading_relation('04baa3f7-074b-488f-aef8-eeee430dd502', gpl_copyleft_scope__narrow_scope_reading, coexists_with).
narrative_ontology:cs_reading_relation('04baa3f7-074b-488f-aef8-eeee430dd502', gpl_copyleft_scope__enforcement_vacuum_reading, influences).
narrative_ontology:cs_axiom('04baa3f7-074b-488f-aef8-eeee430dd502', foundational, coupling_mechanism_irrelevant_to_derivative_status).
narrative_ontology:cs_axiom_status(coupling_mechanism_irrelevant_to_derivative_status, holdable).
narrative_ontology:cs_axiom_grounding('04baa3f7-074b-488f-aef8-eeee430dd502', coupling_mechanism_irrelevant_to_derivative_status, conventional).
narrative_ontology:cs_axiom('04baa3f7-074b-488f-aef8-eeee430dd502', secondary, combined_work_arises_from_functional_integration_not_copying).
narrative_ontology:cs_axiom_status(combined_work_arises_from_functional_integration_not_copying, holdable).
narrative_ontology:cs_axiom_grounding('04baa3f7-074b-488f-aef8-eeee430dd502', combined_work_arises_from_functional_integration_not_copying, instrumental).
narrative_ontology:cs_reference_frame('04baa3f7-074b-488f-aef8-eeee430dd502', fsf_expansive_combined_work_doctrine).
narrative_ontology:cs_drift_state('04baa3f7-074b-488f-aef8-eeee430dd502', post_2010_enforcement_maturation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('04baa3f7-074b-488f-aef8-eeee430dd502', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, free_software_foundation).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, copyleft_aligned_developer_communities).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, gpl_component_maintainers).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, commercial_plugin_developers).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, embedded_systems_integrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and promotes the expansive reading of Section 2(b), asserting that dynamic linking and tight code coupling create a single combined work that must be released under GPL terms. Funds enforcement actions and compliance advocacy (via affiliated bodies) that treat this reading as settled doctrine, even though no definitive appellate ruling on dynamic linking exists in most jurisdictions.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, free_software_foundation, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__strong_copyleft_reading, free_software_foundation, beneficiary).

% Contribute code under GPL expecting that anything linking against it will also become open. Under this reading they receive a structural guarantee that downstream derivative code stays available to the commons, reinforcing their ecosystem's growth and preventing proprietary capture of shared infrastructure.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, copyleft_aligned_developer_communities, beneficiary,
    organized, generational, mobile, global).

% Maintain widely-used GPL libraries and benefit when downstream integrators are forced either to open their code or avoid the library entirely, which channels contributions and adoption back into the copyleft commons rather than fragmenting into proprietary forks.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, gpl_component_maintainers, beneficiary,
    moderate, biographical, mobile, global).

% Want to integrate GPL-licensed components (via dynamic linking, plugin hooks, or tight API coupling) into commercial products without releasing their own proprietary source. Under this reading, any such coupling triggers full GPL obligations on the combined work, forcing them to either avoid the component entirely, re-implement it, negotiate a costly dual-license, or risk infringement litigation. Their exit is technically possible (rewrite, relicense, avoid) but commercially expensive and disruptive to existing product lines.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_vendors, payer,
    powerful, biographical, constrained, global).

% Build plugins or extensions that interoperate closely with GPL host applications. Under the strong reading, their plugin architecture itself may constitute a combined work requiring GPL licensing, even absent static linking. Many lack the resources to relitigate the boundary question and simply comply, abandon the plugin market, or operate under legal uncertainty they cannot afford to resolve.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, commercial_plugin_developers, payer,
    moderate, biographical, trapped, national).

% Ship GPL components (drivers, kernels, utilities) inside hardware products alongside proprietary firmware. The strong reading treats tight coupling in the firmware image as creating a combined work, exposing them to compliance demands or enforcement threats over their proprietary control logic, which they cannot open without losing competitive differentiation.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, embedded_systems_integrators, payer,
    moderate, biographical, constrained, global).

% Groups such as compliance labs and legal defense funds send cease-and-desist notices and pursue settlements against vendors under the strong reading, treating dynamic linking and close coupling as presumptively infringing absent a court ruling squarely resolving the boundary. Settlement leverage comes from litigation cost asymmetry, not from judicial certainty.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, enforcement_organizations, agenda_setter,
    organized, biographical, analytical, global).

% Have not definitively resolved whether dynamic linking creates a derivative or combined work under copyright doctrine in most jurisdictions. Scholarly and appellate treatment remains fragmented; the strong reading operates as an assertively enforced norm rather than settled law.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, courts_and_legal_scholars, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__strong_copyleft_reading, free_software_foundation).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__strong_copyleft_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that anyone who builds on and distributes a GPL-licensed component keeps their combined work's source available to the community, preventing free-riding where proprietary products capture the value of community-maintained code while contributing nothing back.
% TRANSFER_FUNCTION: Moves bargaining leverage and potential revenue from vendors who would otherwise monetize proprietary integrations toward the copyleft commons, either by forcing source disclosure (transferring technical value) or by forcing vendors to avoid, relicense, or pay to avoid the GPL component (transferring money via dual-licensing or reimplementation costs).
% ABSENT_VOICES: Vendors who settled quietly under enforcement pressure rarely surface publicly, so the corpus of contested cases underrepresents how often the expansive reading is asserted against parties who lack resources to litigate the boundary question; their silence is frequently read as tacit endorsement of the strong reading rather than as an artifact of settlement asymmetry.
% DISAPPEARANCE_RATIONALE: If the strong reading's enforcement threat vanished, proprietary vendors would integrate GPL components far more freely via dynamic linking and plugin architectures without the source-release exposure; some GPL projects would lose the leverage that currently pressures commercial adoption into dual-licensing revenue or contribution-back arrangements; the copyleft commons would lose a structural guarantee it currently treats as settled.
% FOUNDING_PROBLEM: Free software authors wanted to prevent proprietary vendors from taking community-built code, embedding it invisibly inside closed products, and capturing commercial value while the original contributors received nothing back and users lost access to modifiable source.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation and copyleft communities attest the problem remains live and cite ongoing embedded and commercial integration as evidence the expansive boundary is still necessary. Independent legal scholars and several appellate-adjacent commentaries note that the specific boundary question for dynamic linking has never been squarely tested in most jurisdictions, and some open-source foundations with different licensing philosophies (permissive-license advocates) argue the founding problem is real but this particular reading of the boundary overshoots what copyright doctrine actually requires — corroboration exists but is split between parties with a stake in the outcome.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__strong_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__strong_copyleft_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__strong_copyleft_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_copyleft_scope__strong_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__strong_copyleft_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__strong_copyleft_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_copyleft_scope__strong_copyleft_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects that the strong reading extends the derivative-work boundary well past what has been judicially confirmed, converting a contestable legal theory into an enforced business risk that vendors must price in as though settled. Suppression (0.62) captures the credible threat of litigation and public compliance shaming used to make vendors comply without ever needing a court to resolve the underlying boundary question — this is the coercive lever that keeps exits closed even for well-resourced actors. Theater ratio is comparatively low (0.20) because actual compliance actions (real cease-and-desist campaigns, real settlements, real source releases) substantially exceed symbolic gesture; this is not a hollowed-out constraint, it is a functioning extraction/coordination hybrid. Accessibility collapse (0.58) is moderate rather than near-total: sophisticated vendors can still avoid GPL components, reimplement functionality, or negotiate dual licenses, but for smaller integrators the practical alternatives collapse further. Resistance (0.71) is high because major vendors and legal scholars actively contest the boundary's scope in courts, standards bodies, and public commentary — this is a live, contested doctrine, not a quietly accepted one.
 *
 * DIRECTIONALITY LOGIC:
 *   The FSF and copyleft-aligned communities are structural beneficiaries: they gain a broader guarantee of code availability and stronger negotiating leverage over commercial adopters, with analytical/mobile exit options that let them set terms rather than absorb them. Proprietary vendors, plugin developers, and embedded integrators are structural targets: the reading converts their commercial integration choices into legal exposure, and their exit options range from constrained (large vendors can reimplement or dual-license, at cost) to trapped (small plugin developers who cannot afford to relitigate or rearchitect). This asymmetry — one side sets the interpretive terms and collects the compliance benefit, the other bears real cost under threat of enforcement without judicial certainty — is the core of the snare structure under this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing free-riding on community-built code) remains genuinely live in many contexts, which is why this is authored as contested rather than dead — this is not a pure mandatrophy case. However, the STRONG reading's expansive boundary claim increasingly outruns what the founding problem strictly requires: preventing free-riding does not obviously require treating every form of dynamic linking as creating a single combined work. The classification as snare (rather than tangled_rope) reflects that under this specific reading, the coordination story (protecting the commons) is used to justify an enforcement scope broader than the underlying doctrine has been shown to require, with real identifiable victims bearing disproportionate cost relative to uncertain legal grounding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dynamic_linking_boundary_uncertainty,
    'Does dynamic linking (as opposed to static linking or source-code inclusion) actually create a legally cognizable ''combined work'' under controlling copyright doctrine in the relevant jurisdictions, or is this an FSF interpretive assertion that has not been tested to judgment?',
    'A definitive appellate ruling squarely addressing dynamic linking as the sole point of coupling (most existing settlements and rulings involve static linking, source copying, or other unambiguous incorporation, leaving the pure dynamic-linking case largely untested).',
    'If courts confirm the strong reading, this constraint''s classification stabilizes as a legitimately enforceable tangled_rope (real coordination function, real enforcement, but now judicially grounded). If courts adopt the narrow reading instead, this constraint''s enforcement pattern would be substantially undermined, and its extraction would be recharacterized as having rested on an unfounded legal claim — pushing it further toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dynamic_linking_boundary_uncertainty, empirical, 'Whether the core doctrinal claim underlying the strong reading has been or would be judicially confirmed.').

omega_variable(
    kernel_reading_indexing,
    'Is ''the GPL Section 2(b) derivative-work boundary'' a single constraint with contested interpretation, or genuinely three distinct constraints (strong, narrow, enforcement-vacuum) each with their own ε, victim set, and enforcement pattern?',
    'This has been resolved by authorial decomposition per the ε-invariance principle: this file represents only the strong_copyleft_reading. The sibling readings (narrow_scope_reading, enforcement_vacuum_reading) are authored as separate constraint stories with their own ε values and linked via network.affects_constraints. A reader treating ''GPL Section 2(b)'' as one undifferentiated constraint will get an incoherent ε because the readings genuinely diverge in extraction level and victim scope.',
    'Ensures analysis of this specific reading is not diluted or averaged against the narrower or vacuum readings, which would produce a false ''moderate'' ε that describes no actual party''s experience.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexing, conceptual, 'Documents the committer-frame decomposition of the GPL Section 2(b) kernel into three sibling constraint readings.').

omega_variable(
    enforcement_settlement_selection_bias,
    'Does the visible pattern of vendor compliance and settlement under the strong reading reflect genuine legal merit, or does it reflect that litigation-averse vendors settle regardless of the underlying doctrine''s strength, creating an appearance of settled law that is actually an artifact of asymmetric litigation costs?',
    'Compare settlement rates and terms against the small number of cases that were actually litigated to judgment on the dynamic-linking question specifically; look for evidence that settlement terms tracked litigation risk pricing rather than doctrinal certainty.',
    'If settlements are driven primarily by cost-avoidance rather than doctrinal confidence, the apparent ''enforceability'' of the strong reading substantially overstates its actual legal grounding, reinforcing the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_settlement_selection_bias, empirical, 'Whether visible compliance under the strong reading reflects legal merit or litigation-cost asymmetry.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__strong_copyleft_reading, 1991, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1991, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 1991, 0.1).
narrative_ontology:measurement(gpl__tr_t2000, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2000, 0.13).
narrative_ontology:measurement(gpl__tr_t2007, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2007, 0.16).
narrative_ontology:measurement(gpl__tr_t2013, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2013, 0.18).
narrative_ontology:measurement(gpl__tr_t2018, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2018, 0.19).
narrative_ontology:measurement(gpl__tr_t2024, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1991, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 1991, 0.35).
narrative_ontology:measurement(gpl__be_t2000, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement(gpl__be_t2007, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2007, 0.55).
narrative_ontology:measurement(gpl__be_t2013, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2013, 0.62).
narrative_ontology:measurement(gpl__be_t2018, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2018, 0.66).
narrative_ontology:measurement(gpl__be_t2024, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1991, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 1991, 0.3).
narrative_ontology:measurement(gpl__su_t2000, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2000, 0.42).
narrative_ontology:measurement(gpl__su_t2007, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2007, 0.5).
narrative_ontology:measurement(gpl__su_t2013, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2013, 0.56).
narrative_ontology:measurement(gpl__su_t2018, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2018, 0.6).
narrative_ontology:measurement(gpl__su_t2024, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__strong_copyleft_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gpl_copyleft_scope__strong_copyleft_reading, 0.1).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, narrow_scope_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, enforcement_vacuum_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the gpl_copyleft_scope kernel. strong_copyleft_reading (this file) authors high ε (0.68) reflecting an expansively enforced but judicially unconfirmed derivative-work boundary. narrow_scope_reading authors substantially lower ε reflecting a boundary that tracks traditional copyright doctrine and excludes most aggregation/plugin/dynamic-linking cases. enforcement_vacuum_reading authors a split/contested ε reflecting that the operative constraint depends entirely on which interpretive community holds enforcement capacity in a given context, rather than on a single settled doctrine. All three share the same underlying kernel text (GPL Section 2(b)) but instantiate structurally distinct constraints with different beneficiary/victim sets and different persistence mechanisms — they are linked here rather than merged, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
