% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__narrow_scope_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   constraint_id: gpl_copyleft_scope__narrow_scope_reading
 *   human_readable: GPL Copyleft Scope — Narrow Derivative-Work Boundary Reading
 *   domain: legal/technological
 *
 * SUMMARY:
 *   The GPL's Section 2(b) copyleft requirement is a contested kernel: what
 *   counts as a 'work based on' the program determines whether the
 *   source-sharing obligation attaches. This story instantiates the
 *   narrow_scope_reading — the boundary follows traditional copyright
 *   doctrine: direct derivative works are covered; mere aggregation, plugin
 *   architectures, and most dynamic linking are not. The epsilon referent is
 *   the standing arrangement under contest, GPL Section 2(b) as actually
 *   scoped by this narrow boundary, assessed by this reading's own lights: a
 *   workable coordination mechanism for mixed codebases that nonetheless lets
 *   boundary-zone value flow from the commons into proprietary products, and
 *   that must be actively maintained against the strong reading's assertions.
 *   The sibling readings are separate constraints, not part of this one;
 *   their committer structure is carried in the omega variables. The claimed
 *   type and the metrics are authored independently — the engine computes
 *   per-seat classifications from the structural data, and any divergence
 *   between the rope claim and the computed type is the measurement this
 *   story exists to take.
 *
 * KEY AGENTS:
 *   - commercial_software_integrators: Primary beneficiary (powerful/arbitrage) — captures integration value at uncovered boundaries; bears copyleft costs on direct derivatives
 *   - proprietary_stack_vendors: Primary beneficiary (institutional/mobile) — platform ecosystems built on the kernel-boundary line
 *   - corporate_open_source_compliance: Agenda-setter (institutional/arbitrage) — administers where the line sits in working practice
 *   - gpl_contributors: Primary target (moderate/constrained) — reciprocity loop does not close at boundary zones
 *   - fsf_aligned_copyleft_projects: Primary target (organized/identity_locked) — licensing achieves less than their reading promises
 *   - fsf_licensing_enforcement: Excluded contesting party (organized/identity_locked) — asserts the broad boundary without operative capacity in industry venues
 *   - dual_licensing_vendors: Secondary beneficiary (powerful/arbitrage) — sells exceptions priced by the boundary's perceived strictness
 *   - downstream_software_users: Beneficiary (organized/mobile) — consumes the integrated ecosystem without obligations
 *   - courts: Inter-institutional observer (institutional/analytical) — holds the definitive answer in reserve
 *   - ip_law_scholars: Analytical observer (analytical/analytical) — maps the interpretive structure without enforcement capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__narrow_scope_reading, 0.42).
domain_priors:suppression_score(gpl_copyleft_scope__narrow_scope_reading, 0.18).
domain_priors:theater_ratio(gpl_copyleft_scope__narrow_scope_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__narrow_scope_reading, rope).
narrative_ontology:human_readable(gpl_copyleft_scope__narrow_scope_reading, "GPL Copyleft Scope — Narrow Derivative-Work Boundary Reading").
narrative_ontology:topic_domain(gpl_copyleft_scope__narrow_scope_reading, "legal/technological").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__narrow_scope_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__narrow_scope_reading, 'be767d07-342a-4ab4-a3a2-8543f1f19c84').
narrative_ontology:cs_kernel_codification('be767d07-342a-4ab4-a3a2-8543f1f19c84', fixed_text).
narrative_ontology:cs_authority_grounding('be767d07-342a-4ab4-a3a2-8543f1f19c84', practice).
narrative_ontology:cs_interpretation_layer_present('be767d07-342a-4ab4-a3a2-8543f1f19c84').
narrative_ontology:cs_reading_relation('be767d07-342a-4ab4-a3a2-8543f1f19c84', gpl_copyleft_scope__strong_copyleft_reading, forecloses).
narrative_ontology:cs_reading_relation('be767d07-342a-4ab4-a3a2-8543f1f19c84', gpl_copyleft_scope__enforcement_vacuum_reading, influences).
narrative_ontology:cs_axiom('be767d07-342a-4ab4-a3a2-8543f1f19c84', foundational, derivative_work_follows_traditional_copyright_doctrine).
narrative_ontology:cs_axiom_status(derivative_work_follows_traditional_copyright_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('be767d07-342a-4ab4-a3a2-8543f1f19c84', derivative_work_follows_traditional_copyright_doctrine, conventional).
narrative_ontology:cs_axiom('be767d07-342a-4ab4-a3a2-8543f1f19c84', secondary, license_reach_bounded_by_copyright_grant).
narrative_ontology:cs_axiom_status(license_reach_bounded_by_copyright_grant, holdable).
narrative_ontology:cs_axiom_grounding('be767d07-342a-4ab4-a3a2-8543f1f19c84', license_reach_bounded_by_copyright_grant, conventional).
narrative_ontology:cs_reference_frame('be767d07-342a-4ab4-a3a2-8543f1f19c84', traditional_copyright_derivative_boundary).
narrative_ontology:cs_drift_state('be767d07-342a-4ab4-a3a2-8543f1f19c84', contemporary_mixed_codebase_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('be767d07-342a-4ab4-a3a2-8543f1f19c84', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, commercial_software_integrators).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, proprietary_stack_vendors).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, downstream_software_users).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, dual_licensing_vendors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__narrow_scope_reading, gpl_contributors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__narrow_scope_reading, fsf_aligned_copyleft_projects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gpl_copyleft_scope__narrow_scope_reading, commercial_software_integrators).
narrative_ontology:constraint_victim(gpl_copyleft_scope__narrow_scope_reading, fsf_licensing_enforcement).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__narrow_scope_reading, traditional_copyright_derivative_work_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build commercial products that combine GPL-licensed components with proprietary code: shipping Linux-based devices, embedding GPL utilities in larger distributions, linking proprietary modules against GPL libraries. On the portions that qualify as direct derivatives they release source under the GPL; at aggregation, plugin, and most dynamic-linking boundaries they keep their code proprietary. They select licenses and integration architectures with the boundary line in mind, and can route new work to permissively licensed components whenever the GPL's terms are the costlier option.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, commercial_software_integrators, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__narrow_scope_reading, commercial_software_integrators, payer).

% Operate platform ecosystems whose cores include GPL code while the layers above stay proprietary — the Android model is the archetype: a GPL kernel beneath a licensed, proprietary application framework. Their business depends on the line between the kernel obligation and everything above it holding. They fund compliance engineering and legal defense of that line, and could, at great cost, rebuild their stacks on permissively licensed cores if the line ever moved against them.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, proprietary_stack_vendors, beneficiary,
    institutional, generational, mobile, global).

% Individual and corporate developers who contribute code to GPL projects on the understanding that improvements will stay in the shared pool. When their code is incorporated into commercial products at boundaries the license text does not clearly reach, the reciprocity loop they contributed into does not close — value moves into proprietary products without a return contribution. Their past contributions are irrevocable; their leverage over how the line is drawn is limited to where they point their next contribution.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, gpl_contributors, payer,
    moderate, generational, constrained, global).

% Projects and foundations whose purpose is keeping code in the commons under copyleft terms. The narrowing of the working boundary means their licensing achieves less than their reading of the license promises: integrations they consider covered proceed as proprietary products, and each settled non-precedent entrenches the narrower practice further. Relicensing away from copyleft would dissolve the projects' reason for existing, so they cannot walk away from the arrangement; they contest it with the resources they have.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, fsf_aligned_copyleft_projects, payer,
    organized, generational, identity_locked, global).

% The licensing staff and enforcement arms of the free-software foundations, including the Software Freedom Conservancy. They assert that combining and dynamic linking create derivative works and pursue violations through compliance letters, negotiated settlements, and the VMware litigation. In industry-dominated ecosystems their reading is not the operative one: settlements are confidential, courts have not adopted their boundary, and their funding base has contracted. They are in the public conversation about the line but are not the seat that sets it in the venues where integration decisions actually get made, and the enforcement effort itself drains resources they cannot replenish.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, fsf_licensing_enforcement, excluded,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__narrow_scope_reading, fsf_licensing_enforcement, payer).

% The in-house counsel, open-source program offices, and compliance consultancies that administer the working line for industry. They write the integration rules firms actually follow — when to link statically, when to isolate across process boundaries, when to buy a commercial license — run audits, and negotiate with enforcement parties. The arrangement's day-to-day content, meaning where the line sits in practice, is authored here. They serve many clients and masters and could reframe their guidance if a court or a major licensor forced the issue.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, corporate_open_source_compliance, agenda_setter,
    institutional, biographical, arbitrage, global).

% Enterprises and individuals who use GPL-licensed software and the commercial products built on it. They receive the code and the products without source-disclosure obligations of their own, and benefit from the wide availability that commercial integration sustains. They hold no seat in boundary-setting and switch products freely when terms or quality disappoint.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, downstream_software_users, beneficiary,
    organized, biographical, mobile, global).

% Copyright holders of popular GPL code — database and UI toolkit vendors, for example — who sell proprietary-use licenses alongside the GPL copy. The boundary's ambiguity is their inventory: the harder integration under the GPL appears, the more valuable the paid exception. They actively shape how strict the line is perceived to be through marketing and license text, and they can relicense at will because they hold the copyright.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, dual_licensing_vendors, beneficiary,
    powerful, biographical, arbitrage, global).

% National courts, principally in the United States and Germany, that would definitively settle whether combining or dynamic linking produces a derivative work. They have so far declined the occasions: cases settled confidentially, were dismissed on standing or procedural grounds, or resolved without reaching the boundary question. Their eventual answer would restructure every other seat's position; until then they observe and dispose.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, courts, observer,
    institutional, generational, analytical, national).

% Academic commentators on copyleft and derivative-work doctrine who map the interpretive positions, model the doctrinal indeterminacy, and publish on what the license text can bear. They hold no enforcement capacity and collect no rents; their analyses are cited by every other seat when convenient.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, ip_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__narrow_scope_reading, commercial_software_integrators).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__narrow_scope_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Defines a stable, doctrinally anchored line between GPL-covered and GPL-free integration: direct derivative works carry the source-sharing obligation, while mere aggregation, plugin architectures, and most dynamic linking sit outside it. This lets mixed proprietary/GPL codebases exist at all — firms can adopt GPL components without disclosing their entire stack, and the commons gains the distribution reach of commercial products.
% TRANSFER_FUNCTION: Moves source-disclosure obligations and integration freedom asymmetrically: commercial integrators gain the freedom to build proprietary layers against GPL components at uncovered boundaries, while GPL contributors and copyleft projects transfer their expectation of reciprocal improvement — boundary-zone value flows into proprietary products without returning to the shared pool.
% ABSENT_VOICES: The FSF's licensing staff and copyleft-maximalist voices are present in public discourse but structurally sidelined in the venues where integration decisions are made — their preferred boundary governs only where they hold enforcement leverage. End-user developers' licensing preferences are mediated by corporate compliance offices. Both would object that the working line was set without them; they sit outside the industry compliance process that authors it.
% DISAPPEARANCE_RATIONALE: If the narrow-scoped arrangement vanished overnight — the boundary question resolving to the strong reading, or the copyleft requirement simply ceasing — mixed codebases would reorganize within product cycles: platform vendors would face whole-stack disclosure demands or rebuild on permissive cores, dual-licensing revenue would reprice, and firms would either abandon GPL components wholesale or comply far beyond current practice. The commercial free-software ecosystem as presently constituted depends on this line holding.
% FOUNDING_PROBLEM: Early GPL adoption ran into a wall: firms could not ship GPL code in commercial products without risking demands for their entire proprietary stack, because whether 'derivative work' reaches combined or linked programs was — and remains — unsettled. The narrow reading, holding the boundary at traditional copyright doctrine, was the working compromise that let GPL components into commercial products at all, preserving adoption while keeping the core reciprocity requirement on direct derivatives.
% FOUNDING_PROBLEM_CORROBORATION: Corporate open-source counsel and the Linux Foundation's compliance programs attest the integration problem is live — their entire function exists because of it. From outside the benefiting parties: the copyleft.org GPL compliance guide (copyleft-side), academic IP scholarship on derivative-work indeterminacy, and the German procedural history in Hellwig v. VMware all corroborate that the boundary question remains doctrinally open and the founding problem unsolved at the level of settled law.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__narrow_scope_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__narrow_scope_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__narrow_scope_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_copyleft_scope__narrow_scope_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__narrow_scope_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is moderate (0.42 at interval end): the coordination core — a clear line for when source obligations attach — is genuine and heavily used, but boundary-zone value flows one way, from the commons into proprietary products, and the reciprocity loop contributors bought into does not close at aggregation, plugin, and dynamic-linking seams. Suppression is low (0.18) and is authored as a raw structural property — it is not scaled by scope or power; only extractiveness is scaled, by directionality and spatial scope in the engine's computation. Firms can route new work to permissively licensed components, buy dual licenses, or avoid GPL code entirely; the arrangement holds by adoption, not by blocking exits. Theater is low (0.22): compliance work is mostly functional, with a growing ritual component as audit programs certify what integration architecture already guarantees. Accessibility collapse is moderate-low (0.40): the alternative menu — permissive licenses, LGPL, AGPL, dual licensing, proprietary stacks — stays rich and does not collapse on contact with the constraint. Resistance is moderate (0.45): enforcement arms actively contest the boundary while firms minimize compliance costs on the covered side. The measurement series share one time grid (0, 6, 12, 18, 24, 30, 34, mapping roughly 1991–2025): extractiveness climbs through the corporate-Linux and Android-era integration wave and plateaus as the boundary stabilizes by default; theater creeps upward as compliance programs ritualize; suppression_requirement peaks mid-interval in the enforcement-contest era (confidential settlements, the VMware litigation, when the narrow boundary had to be actively defended) and then falls as enforcement against dynamic linking becomes rare and the narrow reading wins by attrition.
 *
 * PERSPECTIVAL GAP:
 *   The integrator seats and the contributor seats compute differently from identical structural data. From commercial_software_integrators and corporate_open_source_compliance, the arrangement is what makes their business possible: a stable, doctrinally anchored line they build against, with exits they actually use. From gpl_contributors and fsf_aligned_copyleft_projects, the same line is where their reciprocity expectation leaks away: value crosses it in one direction, past contributions are irrevocably in the pool, and the copyleft projects cannot exit without dissolving their purpose — an ideological identity fusion in which relicensing equals self-annihilation, so the classification of that seat would change only if the projects' self-concept broke, not if conditions did. Among same-power actors, commercial_software_integrators and dual_licensing_vendors both hold 'powerful' yet sit on opposite sides of the line's perceived strictness: integrators profit from the line staying put, dual-licensing vendors profit from its appearing strict, which is why the same power atom carries opposed directionalities. The excluded seat experiences a third thing: a public conversation about the boundary in which its position is heard but never operative. Inter-institutionally, the courts hold the definitive answer in reserve and every other seat prices its behavior against that reserve.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: commercial_software_integrators, proprietary_stack_vendors, downstream_software_users, dual_licensing_vendors — all derive low directionality (subsidized by the arrangement), with arbitrage-grade exits (integrators, dual-licensing vendors) sitting nearest the full-beneficiary end. Victims declared: gpl_contributors and fsf_aligned_copyleft_projects — high directionality, with identity-locked exit (the copyleft projects) pushing toward the full-target end and constrained exit (contributors) just behind. The agenda-setter seat administers the working line and is drawn from the beneficiary class; the excluded seat contests it without operative capacity. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms already place the seats correctly, and a per-power-atom override would be too blunt to differentiate the two 'organized' seats that need opposite placements (one victim near 1.0, one beneficiary near 0.1).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — enabling mixed commercial/GPL codebases at all — is live, so the arrangement is not a vestige: the rope claim is the honest one, and the piton signature (all performance, no function) is structurally unavailable. What the classification guards against here is the opposite mislabel in both directions: reading the boundary-zone value flow as pure extraction would erase the genuine coordination function the entire mixed-codebase ecosystem depends on; reading the arrangement as pure coordination would erase the contributors and copyleft projects who bear its asymmetric costs through the same structure that coordinates the integrators. The moderate epsilon, the declared victim set, and the active-enforcement flag keep both faces visible: a coordination function carrying an extraction layer, maintained against a live interpretive competitor. If the founding problem were ever resolved doctrinally by a definitive boundary ruling, this arrangement would either collapse into ordinary license compliance (its function absorbed into settled law) or harden into enforcement of whichever reading won — the obsolescence question is deferred, not dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint instantiates only the narrow_scope_reading of the gpl_copyleft_scope kernel: what structurally changes under the sibling readings, and where exactly is the disagreement located?',
    'A definitive appellate ruling on whether combining or dynamic linking produces a derivative work would collapse the reading plurality into a single constraint; until then the readings coexist as licensed plurality and each sibling is authored as its own constraint story with its own epsilon and victim set.',
    'Under strong_copyleft_reading the victim set expands to all combining and dynamically linking integrators and epsilon rises sharply (the arrangement''s extractive face becomes dominant); under enforcement_vacuum_reading no single epsilon is stable — classification becomes context-relative to which interpretive community holds enforcement capacity in the given venue.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one reading of the gpl_copyleft_scope kernel; siblings strong_copyleft_reading and enforcement_vacuum_reading are separate constraints with different epsilon and victim sets.').

omega_variable(
    derivative_work_doctrinal_indeterminacy,
    'Is the derivative-work boundary for combined or dynamically linked programs genuinely indeterminate under copyright doctrine, or determinate-but-unlitigated?',
    'A court reaching the merits on a dynamic-linking or plugin case with developed doctrinal analysis, rather than settlement or procedural dismissal.',
    'If determinate and narrow, this arrangement is closer to settled law with lower effective extraction and mountain-flavored stability; if indeterminate, the boundary is a maintained construct whose persistence depends on continued industry maintenance and enforcement asymmetry, raising effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derivative_work_doctrinal_indeterminacy, empirical, 'Doctrinal determinacy of the software derivative-work boundary.').

omega_variable(
    enforcement_capacity_asymmetry,
    'Does the narrow boundary persist because it is the doctrinally correct reading, or because the parties positioned to enforce the broad reading lack resources relative to the industry positioned to resist?',
    'Counterfactual capacity tests: enforcement outcomes when a well-funded copyleft plaintiff with clean standing reaches a sympathetic venue; tracking of settlement terms as enforcement funding rises or falls.',
    'If capacity rather than merit holds the boundary in place, the arrangement''s stability is contingent on the funding base of copyleft enforcement — a re-funded enforcement arm could shift the operative reading without any change in doctrine, changing the persistence mechanism the classification reads.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_asymmetry, empirical, 'Whether merit or enforcement-capacity asymmetry holds the narrow boundary in place.').

omega_variable(
    boundary_value_flow_magnitude,
    'How much commercial value actually crosses the uncovered boundaries (aggregation, plugins, dynamic linking) from the GPL commons into proprietary products?',
    'Compliance-audit data and product teardowns quantifying GPL code incorporated at non-derivative boundaries; economic analysis of platform stacks built on GPL cores.',
    'A large flow would push the arrangement''s extraction well above the coordination floor and support a hybrid coordination-plus-extraction reading of the same structure; a negligible flow would leave this a near-pure coordination mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(boundary_value_flow_magnitude, empirical, 'Magnitude of uncompensated value flow across the uncovered integration boundaries.').

omega_variable(
    reciprocity_erosion_valence,
    'Is the weakening of universal code-sharing expectations a harm the arrangement imposes on the commons, or the correction of an expectation that exceeded the license''s actual terms?',
    'Not resolvable by data alone — it depends on whether the GPL''s promissory scope is read from its text, its author community''s intent, or contributors'' understanding at contribution time.',
    'Under the harm reading the contributor seat bears a genuine imposed cost and the arrangement carries extraction even by this reading''s own lights; under the correction reading the contributor seat bears a bargained-for limit, not a harm, and the arrangement sits closer to pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_erosion_valence, preference, 'Valence of the weakened reciprocity expectation: imposed harm or correction of overreach.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__narrow_scope_reading, 0, 34).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(gpl__tr_t0, observed).
narrative_ontology:measurement(gpl__tr_t6, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement_basis(gpl__tr_t6, observed).
narrative_ontology:measurement(gpl__tr_t12, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 12, 0.15).
narrative_ontology:measurement_basis(gpl__tr_t12, observed).
narrative_ontology:measurement(gpl__tr_t18, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 18, 0.18).
narrative_ontology:measurement_basis(gpl__tr_t18, observed).
narrative_ontology:measurement(gpl__tr_t24, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 24, 0.2).
narrative_ontology:measurement_basis(gpl__tr_t24, observed).
narrative_ontology:measurement(gpl__tr_t30, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(gpl__tr_t30, observed).
narrative_ontology:measurement(gpl__tr_t34, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 34, 0.24).
narrative_ontology:measurement_basis(gpl__tr_t34, observed).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(gpl__be_t0, observed).
narrative_ontology:measurement(gpl__be_t6, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 6, 0.34).
narrative_ontology:measurement_basis(gpl__be_t6, observed).
narrative_ontology:measurement(gpl__be_t12, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement_basis(gpl__be_t12, observed).
narrative_ontology:measurement(gpl__be_t18, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 18, 0.4).
narrative_ontology:measurement_basis(gpl__be_t18, observed).
narrative_ontology:measurement(gpl__be_t24, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 24, 0.41).
narrative_ontology:measurement_basis(gpl__be_t24, observed).
narrative_ontology:measurement(gpl__be_t30, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement_basis(gpl__be_t30, observed).
narrative_ontology:measurement(gpl__be_t34, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 34, 0.4).
narrative_ontology:measurement_basis(gpl__be_t34, observed).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(gpl__su_t0, observed).
narrative_ontology:measurement(gpl__su_t6, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 6, 0.15).
narrative_ontology:measurement_basis(gpl__su_t6, observed).
narrative_ontology:measurement(gpl__su_t12, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 12, 0.2).
narrative_ontology:measurement_basis(gpl__su_t12, observed).
narrative_ontology:measurement(gpl__su_t18, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 18, 0.24).
narrative_ontology:measurement_basis(gpl__su_t18, observed).
narrative_ontology:measurement(gpl__su_t24, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 24, 0.22).
narrative_ontology:measurement_basis(gpl__su_t24, observed).
narrative_ontology:measurement(gpl__su_t30, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 30, 0.18).
narrative_ontology:measurement_basis(gpl__su_t30, observed).
narrative_ontology:measurement(gpl__su_t34, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 34, 0.15).
narrative_ontology:measurement_basis(gpl__su_t34, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__narrow_scope_reading, resource_allocation).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, enforcement_vacuum_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the GPL's copyleft requirement' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints sharing one kernel (gpl_copyleft_scope): this narrow-scope reading (boundary at traditional copyright doctrine; moderate epsilon; mixed codebases coordinated), strong_copyleft_reading (boundary at all code coupling; high epsilon; universal source-sharing asserted), and enforcement_vacuum_reading (no stable boundary; classification context-relative to enforcement capacity). The narrow and strong readings make contradictory claims about a single legal fact — where the derivative-work line lies — and cannot coexist within one adjudicative framework, hence the forecloses edge in cs_structure; the vacuum reading is a meta-observation about operation absent adjudication, and this reading's industry entrenchment structurally shapes the enforcement-capacity landscape it describes, hence the influences edge. Each reading is authored as its own story with its own epsilon, beneficiaries, and victims; this file links both siblings via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
