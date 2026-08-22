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
 *   human_readable: GPL Section 2(b) Narrow Derivative Work Scope
 *   domain: software licensing / intellectual property
 *
 * SUMMARY:
 *   GPL Section 2(b) constrains only direct derivative works, not mere
 *   aggregation, plugin architectures, or certain dynamic linking forms. This
 *   is one reading of a contested kernel — the GPL copyleft scope itself —
 *   that interprets the copyleft obligation narrowly, respecting traditional
 *   copyright-law boundaries between separate works and derivative works.
 *   Commercial software firms benefit structurally from this reading's
 *   flexibility; the free software community experiences it as a weakening of
 *   their code-sharing mandate. The narrow reading is neither natural law nor
 *   pure extraction: it is a licensing interpretation that enables a
 *   mixed-license ecosystem, a genuine coordination mechanism, but one that
 *   systematically favors commercial integrators over pure-copyleft
 *   contributors. No definitive judicial precedent has settled this boundary,
 *   allowing the narrow reading and the strong-copyleft reading to coexist as
 *   competing interpretive positions held by different parties.
 *
 * KEY AGENTS:
 *   - commercial_software_firms: Beneficiary, retain IP control over integration layers; power institutional, exit arbitrage-grade (can choose GPL or proprietary licensing per layer)
 *   - free_software_community: Payer, experience weakened copyleft mandate; power organized, exit constrained (ideological commitment to GPL)
 *   - individual_gpl_contributors: Payer, expect universal sharing, have little enforcement capacity; power powerless, exit identity_locked (career/reputation tied to GPL projects)
 *   - software_integrators_and_tool_vendors: Beneficiary, build commercial platforms via GPL + proprietary layering; power institutional, exit arbitrage-grade
 *   - copyleft_enforcement_bodies (FSF, Conservancy): Agenda-setter, enforce copyleft selectively given resource constraints and narrow-reading uncertainty; power organized, exit constrained (mission-bound)
 *   - software_users: Beneficiary and payer, gain tool diversity from narrow reading, lose universal code-sharing guarantee; power powerless, exit mobile (license choice, feature switching)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__narrow_scope_reading, 0.38).
domain_priors:suppression_score(gpl_copyleft_scope__narrow_scope_reading, 0.21).
domain_priors:theater_ratio(gpl_copyleft_scope__narrow_scope_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 0.21).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__narrow_scope_reading, rope).
narrative_ontology:human_readable(gpl_copyleft_scope__narrow_scope_reading, "GPL Section 2(b) Narrow Derivative Work Scope").
narrative_ontology:topic_domain(gpl_copyleft_scope__narrow_scope_reading, "software licensing / intellectual property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__narrow_scope_reading, '70f6379e-66ab-4748-ae49-e5c318babbe2').
narrative_ontology:cs_kernel_codification('70f6379e-66ab-4748-ae49-e5c318babbe2', fixed_text).
narrative_ontology:cs_authority_grounding('70f6379e-66ab-4748-ae49-e5c318babbe2', extraction).
narrative_ontology:cs_interpretation_layer_present('70f6379e-66ab-4748-ae49-e5c318babbe2').
narrative_ontology:cs_reading_relation('70f6379e-66ab-4748-ae49-e5c318babbe2', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('70f6379e-66ab-4748-ae49-e5c318babbe2', gpl_copyleft_scope__enforcement_vacuum_reading, influences).
narrative_ontology:cs_axiom('70f6379e-66ab-4748-ae49-e5c318babbe2', foundational, copyright_derivative_doctrine_applies_narrowly).
narrative_ontology:cs_axiom_status(copyright_derivative_doctrine_applies_narrowly, holdable).
narrative_ontology:cs_axiom_grounding('70f6379e-66ab-4748-ae49-e5c318babbe2', copyright_derivative_doctrine_applies_narrowly, conventional).
narrative_ontology:cs_axiom('70f6379e-66ab-4748-ae49-e5c318babbe2', foundational, aggregation_and_plugin_coupling_not_derivative_work).
narrative_ontology:cs_axiom_status(aggregation_and_plugin_coupling_not_derivative_work, holdable).
narrative_ontology:cs_axiom_grounding('70f6379e-66ab-4748-ae49-e5c318babbe2', aggregation_and_plugin_coupling_not_derivative_work, empirically_contingent).
narrative_ontology:cs_reference_frame('70f6379e-66ab-4748-ae49-e5c318babbe2', traditional_copyright_law_derivative_boundary).
narrative_ontology:cs_drift_state('70f6379e-66ab-4748-ae49-e5c318babbe2', contemporary_open_source_commercial_integration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('70f6379e-66ab-4748-ae49-e5c318babbe2', '2026-06-11T00:00:00Z').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, commercial_software_firms).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, proprietary_integration_practitioners).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, mixed_licensing_ecosystem).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, individual_gpl_contributors).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, software_integrators_and_tool_vendors).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, software_users_and_end_organizations).
narrative_ontology:constraint_victim(gpl_copyleft_scope__narrow_scope_reading, free_software_community).
narrative_ontology:constraint_victim(gpl_copyleft_scope__narrow_scope_reading, individual_gpl_contributors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__narrow_scope_reading, software_users_and_end_organizations).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__narrow_scope_reading, traditional_copyright_derivative_doctrine).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__narrow_scope_reading, aggregation_separate_copyrightable_works).
narrative_ontology:constraint_vindicates(gpl_copyleft_scope__narrow_scope_reading, dynamic_linking_boundary_ambiguity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain flexibility to bundle GPL-licensed components (e.g., libraries, utilities) with proprietary software layers through aggregation, plugin architectures, or dynamic linking without triggering Section 2(b) copyleft obligations. This reading permits them to commercialize mixed-license stacks while keeping proprietary source code closed. They justify this as respecting the copyright-law boundary between separate works and transformative derivative works.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, commercial_software_firms, beneficiary,
    institutional, generational, arbitrage, global).

% See their copyleft mandate weakened by the narrow reading. They expected GPL Section 2(b) to propagate licensing requirements across all forms of code coupling (dynamic linking, plugin systems, aggregation). Under this reading, commercial firms can extract GPL work without reciprocal sharing, violating the community's foundational principle of universal code-sharing as the price of using GPL'd contributions.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, free_software_community, payer,
    organized, generational, constrained, global).

% Contribute code to GPL projects expecting universal sharing; the narrow reading permits firms to use their work without reciprocation. They have little enforcement capacity and strong ideological commitment to copyleft as philosophy, creating identity lock: their participation signals endorsement of code-sharing norms, but the reading's narrow scope undermines that signal's enforceability.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, individual_gpl_contributors, payer,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__narrow_scope_reading, individual_gpl_contributors, beneficiary).

% Build commercial platforms and tools by layering proprietary logic atop GPL components via plugin architectures or dynamic linking. The narrow reading permits this stack without licensing constraint. They argue that forcing proprietary integration to GPL would destroy the viability of tool markets and force all derivative works to be open-source, which is economically unworkable.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, software_integrators_and_tool_vendors, beneficiary,
    institutional, generational, arbitrage, global).

% Organizations like the FSF and Conservancy interpret and (selectively) enforce copyleft. They have limited litigation resources and the narrow reading constrains which infringement claims they can credibly pursue. Enforcement against dynamic linking or plugin-based integration is legally uncertain under the narrow scope, forcing resource allocation toward clearer Section 2(b) violations (wholesale copying, clear derivative work transformation).
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, copyleft_enforcement_bodies, agenda_setter,
    organized, generational, constrained, global).

% Benefit from vibrant mixed-license ecosystems and tool markets enabled by the narrow reading: firms willing to invest in integration layers because they can keep them proprietary. They also lose the guarantee of code-sharing for all coupled software, which the copyleft community argues would accelerate security auditing and community contribution. Their exit is through license choice or feature/UI switching, not code modification.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, software_users_and_end_organizations, beneficiary,
    powerless, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__narrow_scope_reading, software_users_and_end_organizations, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__narrow_scope_reading, commercial_software_firms).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__narrow_scope_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables a mixed-license software ecosystem: firms can integrate GPL components into proprietary systems without losing IP control of their integration layers, which permits capital investment in commercial tool markets and integration layers that would be economically infeasible under universal copyleft. This is a coordination mechanism solving the problem 'how do GPL-licensed components and proprietary software coexist in a single market?' The narrow reading's answer: by respecting traditional copyright law's boundary between separate works and derivative works.
% TRANSFER_FUNCTION: GPL-licensed code (labor, design, debugging) flows from contributors to commercial firms and integrators; in return, firms contribute bug fixes and incremental improvements back to GPL projects (selective, not universal), and the broader ecosystem benefits from tool innovation that proprietary licensing enables. The narrow reading redistributes the copyleft obligation: instead of ALL code touching GPL'd software reverting to GPL, only direct transformative derivatives do, permitting proprietary firms to retain integration IP.
% ABSENT_VOICES: Pure-copyleft advocates (who would argue the narrow reading defeats GPL's stated purpose and should be rejected in favor of strong copyleft) are structurally excluded from commercial licensing negotiations and ecosystem architecture decisions where the narrow reading's flexibility is operationalized. They have organizational presence (FSF, Conservancy) but limited enforcement capacity, making their objections partly excluded from practical governance.
% DISAPPEARANCE_RATIONALE: If this narrow-scope reading vanished and were replaced by the strong-copyleft reading (all coupled code is derivative, subject to copyleft), commercial tool investment would collapse, integration layers would disappear, and the software market would bifurcate into GPL-only codebases and proprietary codebases with minimal mixing. The absence of the narrow reading's flexibility is not a small adjustment — it is a structural reorganization of the commercial software ecosystem.
% FOUNDING_PROBLEM: Early GPL adoption faced a dilemma: enforce copyleft so strictly that any integration with proprietary code triggered obligation (killing commercial adoption and ecosystem diversity), or permit narrow derivative-work interpretation that respects copyright-law boundaries (enabling mixed ecosystems and capital investment). The GPL 2.0 design chose the latter: Section 2(b) constrains only direct derivatives, not aggregation or plugins, respecting traditional copyright doctrine.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation and many GPL advocates attest the founding problem is live and the narrow reading is a regrettable compromise. Commercial firms and integrators attest the founding problem required the narrow reading and remains live: without it, GPL would never have achieved widespread ecosystem adoption. Independent software policy analyses (e.g., from academic computer science and law) and actual ecosystem behavior (ubiquity of GPL + proprietary mixing) support the narrow reading's necessity for the problem it solved. The strong-copyleft reading's advocates attest the founding problem was misconceived — that universal copyleft should have been enforced instead — placing them outside the founding problem's problem-diagnosis consensus.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__narrow_scope_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__narrow_scope_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__narrow_scope_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.38) because the narrow reading permits commercial firms to capture proprietary integration value that pure copyleft would force to GPL. This is not zero-extraction (a pure rope) because copyleft still constrains direct derivatives and the ecosystem remains mixed-license; firms must still use and acknowledge GPL components. Suppression is low (0.21) because the narrow reading relies on copyright-law doctrine (interpretation, not coercion) and enforcement is uncertain, making the constraint lightweight on enforcement machinery. Theater is very low (0.15) at interval end because the narrow reading is genuinely a licensing interpretation grounded in copyright law, not performance for its own sake. Accessibility collapse is moderate (0.42) because alternatives exist (firms could choose pure proprietary or pure GPL, jurisdictions could legislate different scopes), but once GPL + proprietary mixing is normalized, reversing to pure copyleft would require ecosystem reorganization. Resistance is high (0.67) because copyleft advocates actively object to the narrow reading and contest it through FSF policy, conference advocacy, and selective enforcement — the constraint persists because commercial parties have greater exit options, not because it is accepted as natural or inevitable. The measurement series shows extractiveness rising gradually (0.28 → 0.38) as commercial integration practices mature and firms become more sophisticated at legally-defensible coupling; suppression remains low because enforcement stays uncertain (no landmark court rulings); theater is stable because the interpretation remains grounded in doctrine.
 *
 * PERSPECTIVAL GAP:
 *   From the commercial-firm seat: the narrow reading is legitimate licensing interpretation respecting property-law boundaries, enabling an efficient mixed ecosystem. From the copyleft-community seat: the narrow reading is a legal escape hatch defeating GPL's foundational principle. From the user seat: the narrow reading is a boon (vibrant tool market) and a cost (weakened code-sharing guarantee) simultaneously. The engine's per-seat classification computation should expose this perspectival divergence: commercial seats see rope (genuine coordination with manageable overhead), copyleft seats see tangled_rope or snare (asymmetric extraction with enforcement manipulation), and user seats see rope (benefits and costs roughly balanced). The authored claim is rope (the narrow reading's self-description), but the metrics and stakeholder structure support seat-divergent classification.
 *
 * DIRECTIONALITY LOGIC:
 *   The narrow reading's directionality divergence is precisely its structure: it permits commercial integration (high d for beneficiaries, low χ after directionality scaling) while constraining copyleft mandate enforcement (high d for contributors, moderate χ). This is not a measurement error — it is the constraint's asymmetry. Under strong copyleft, d would be more uniform (all coupled parties equally obligated). Under pure proprietary licensing, d would be zero-extraction (no copyleft). The narrow reading's middle position creates the divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (early GPL adoption faced a dilemma: enforce copyleft so strictly that integration was infeasible, or permit mixed-licensing via narrow scope) was live and genuine. The narrow reading solved it by choosing the latter. Mandatrophy arises if: (1) the original problem (GPL adoption friction) is no longer live and the narrow reading persists as rent-protection for commercial firms; (2) GPL is now ubiquitous and the constraint's function has shifted from adoption enabler to extraction justifier. The founding_problem_status is 'contested' because copyleft advocates attest the founding problem was misconceived (universal copyleft should have been enforced from the start) while commercial parties and pragmatic practitioners attest the narrow reading was necessary and remains live (without it, GPL would not have achieved ecosystem adoption). This contest is not resolvable by fact — it is a values disagreement about whether GPL's foundational principle should have been enforced universally or traded off for ecosystem pragmatism. Mandatrophy is plausible but not certain: the narrow reading performs a genuine function (enabling mixed ecosystems) that would require explicit replacement if removed, which argues against pure inertia. However, the selective enforcement by copyleft organizations (pursuing clear violations but avoiding precedent on dynamic linking) suggests partial acceptance of the narrow reading's legal ground — not pure inertia, but not wholehearted endorsement either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivative_work_boundary_ambiguity,
    'Does traditional copyright law''s definition of ''derivative work'' map cleanly to dynamic linking, plugin architectures, and complex compilation/linking chains in modern software, or is the traditional definition context-blind?',
    'Judicial precedent from a major jurisdiction (U.S., EU, UK) explicitly ruling on GPL Section 2(b) and dynamic linking; or systematic analysis of copyright offices'' guidance on software compilation and linking as derivative-work creation.',
    'If the narrow reading''s doctrine maps cleanly, the reading is robust; if the traditional definition is inapplicable to software, courts may impose a software-specific derivative-work test, which could support either narrow or strong copyleft. This is the core unresolved question enabling kernel contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derivative_work_boundary_ambiguity, empirical, 'Whether traditional copyright derivative-work doctrine applies to software linking and compilation.').

omega_variable(
    enforcement_versus_legal_interpretation,
    'Is the narrow reading persistent because it is the correct copyright-law interpretation, or because copyleft enforcement bodies lack resources to establish stronger legal precedent through litigation?',
    'Controlled comparison of enforcement capacity across jurisdictions: observe whether copyleft enforcement bodies pursue dynamic-linking disputes more aggressively in high-resource jurisdictions, and track whether judicial outcomes differ. Or, examine copyleft enforcement strategy memos and litigation resource allocation.',
    'If the narrow reading persists due to enforcement-capacity constraints, its classification should drift toward snare as enforcement capacity grows or strong-copyleft litigation resources increase. If it reflects accurate copyright interpretation, classification remains stable regardless of enforcement capacity. This determines whether the narrow reading is a stable doctrine or a transitional enforcement vacuum.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_versus_legal_interpretation, empirical, 'Whether the narrow reading reflects legal accuracy or enforcement-capacity asymmetry.').

omega_variable(
    mixed_ecosystem_necessity,
    'Would GPL have achieved comparable ecosystem adoption under universal-copyleft rules, or was the narrow reading necessary as a pragmatic compromise to enable tool-market viability?',
    'Retrospective analysis of GPL adoption adoption timeline and decision-making (archived FSF deliberations, founding-era mailing lists); counterfactual: jurisdictions or communities that tried strong copyleft exclusively, observed adoption rates. Historical comparison with strong-copyleft licenses (AGPLv3, Affero GPL) and their ecosystem adoption relative to GPL 2.0/3.0.',
    'If the narrow reading was necessary for adoption, removing it now (to enforce strong copyleft retroactively) would restructure the ecosystem; if adoption would have been comparable under strong copyleft, the narrow reading becomes a choice for commercial benefit rather than pragmatic necessity, shifting classification toward extraction (snare/tangled_rope).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mixed_ecosystem_necessity, empirical, 'Whether the narrow reading was pragmatically necessary for GPL adoption or chosen for commercial benefit.').

omega_variable(
    copyleft_advocate_expectation_mismatch,
    'At GPL''s founding (1989), did free software advocates explicitly intend the narrow derivative-work boundary, or did they expect stronger copyleft and only later rationalize the narrow boundary as copyright-law doctrine?',
    'Examination of GPL 1.0 drafting history, FSF founding documents, and Stallman''s early writings on copyleft intent; comparison with explicit strong-copyleft language in later licenses (AGPLv3, GPLv3 preamble revisions) intended to strengthen copyleft.',
    'If the narrow reading was always intended, the constraint reflects the GPL''s original design. If the narrow reading was a later rationalization, it represents mandate drift — originally stronger copyleft intent weakened by court precedent or license-drafting constraints. This affects whether the constraint is stable doctrine or unstable compromise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(copyleft_advocate_expectation_mismatch, empirical, 'Whether the narrow reading reflects GPL''s original intent or later doctrine rationalization.').

omega_variable(
    reading_contest_structural_position,
    'Is the narrow reading structurally stable against the strong-copyleft reading within copyright law''s framework, or does the contest depend on which interpretive community has enforcement capacity in a specific jurisdiction?',
    'Comparative legal analysis: examine whether the narrow and strong readings are both legally defensible under copyright doctrine, or whether one has superior legal grounding. Track enforcement outcomes across jurisdictions with different legal traditions (U.S. common law, EU civil law, etc.) to see if the contest resolves jurisdictionally.',
    'If the narrow reading is legally defensible but not required, the contest is pluralistic (enforcement-vacuum reading accurate, per kernel_context). If the narrow reading is the only legally defensible interpretation, strong copyleft is legally foreclosed. If strong copyleft is more defensible, the narrow reading is a losing position.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_structural_position, conceptual, 'Whether the narrow reading is structurally stable or contest-dependent.').


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
narrative_ontology:measurement(gpl__tr_t5, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement_basis(gpl__tr_t5, observed).
narrative_ontology:measurement(gpl__tr_t10, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement_basis(gpl__tr_t10, observed).
narrative_ontology:measurement(gpl__tr_t15, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 15, 0.14).
narrative_ontology:measurement_basis(gpl__tr_t15, observed).
narrative_ontology:measurement(gpl__tr_t20, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement_basis(gpl__tr_t20, observed).
narrative_ontology:measurement(gpl__tr_t25, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement_basis(gpl__tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(gpl__be_t0, observed).
narrative_ontology:measurement(gpl__be_t5, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement_basis(gpl__be_t5, observed).
narrative_ontology:measurement(gpl__be_t10, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement_basis(gpl__be_t10, observed).
narrative_ontology:measurement(gpl__be_t15, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 15, 0.37).
narrative_ontology:measurement_basis(gpl__be_t15, observed).
narrative_ontology:measurement(gpl__be_t20, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(gpl__be_t20, observed).
narrative_ontology:measurement(gpl__be_t25, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(gpl__be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(gpl__su_t0, observed).
narrative_ontology:measurement(gpl__su_t5, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 5, 0.17).
narrative_ontology:measurement_basis(gpl__su_t5, observed).
narrative_ontology:measurement(gpl__su_t10, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 10, 0.19).
narrative_ontology:measurement_basis(gpl__su_t10, observed).
narrative_ontology:measurement(gpl__su_t15, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 15, 0.2).
narrative_ontology:measurement_basis(gpl__su_t15, observed).
narrative_ontology:measurement(gpl__su_t20, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 20, 0.21).
narrative_ontology:measurement_basis(gpl__su_t20, observed).
narrative_ontology:measurement(gpl__su_t25, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 25, 0.21).
narrative_ontology:measurement_basis(gpl__su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__narrow_scope_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gpl_copyleft_scope__narrow_scope_reading, 0.12).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope__strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope__enforcement_vacuum_reading).

% DUAL FORMULATION NOTE:
% The GPL copyleft scope kernel has been decomposed into three constraint stories, each instantiating a different reading. The narrow_scope_reading (this file) describes the constraint as moderate-epsilon rope with commercial flexibility. The strong_copyleft_reading describes universal-copyleft obligation (higher extraction, snare-tendency). The enforcement_vacuum_reading describes plural interpretations coexisting in legal uncertainty. All three are linked via network.affects_constraints because each reading's viability depends on which interpretive community has enforcement capacity in a specific context. No single reading logically forecloses the others; they coexist as competing framework choices held by different parties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_copyleft_scope__narrow_scope_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
