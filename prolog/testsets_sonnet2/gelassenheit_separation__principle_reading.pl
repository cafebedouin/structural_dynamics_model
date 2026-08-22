% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__principle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__principle_reading, []).

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
 *   constraint_id: gelassenheit_separation__principle_reading
 *   human_readable: Gelassenheit Separation — Functional-Isolation (Principle) Reading
 *   domain: religious/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the 'principle reading' of the gelassenheit
 *   separation kernel: separation is understood structurally — a technology
 *   is acceptable if it can be functionally isolated from worldly systems of
 *   dependency, regardless of whether it visibly resembles 'English'
 *   artifacts. This permits solar panels, pneumatic tools, and battery power
 *   when genuinely off-grid, while categorically forbidding internet access
 *   and commercial insurance even where a member could construct a case that
 *   their particular use is bounded or low-risk. The reading is
 *   lower-extraction and lower-suppression than the artifact reading (which
 *   polices visible resemblance regardless of function) because it offers a
 *   coherent functional test rather than a symbolic one; but it is not
 *   extraction-free, because the categorical bans on internet/insurance are
 *   not themselves derived from a pure isolation test — they reflect an
 *   underlying judgment about which systems are irreducibly entangling that
 *   the principle reading treats as settled rather than case-evaluable.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__principle_reading, 0.28).
domain_priors:suppression_score(gelassenheit_separation__principle_reading, 0.42).
domain_priors:theater_ratio(gelassenheit_separation__principle_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__principle_reading, rope).
narrative_ontology:human_readable(gelassenheit_separation__principle_reading, "Gelassenheit Separation — Functional-Isolation (Principle) Reading").
narrative_ontology:topic_domain(gelassenheit_separation__principle_reading, "religious/technology_governance").

domain_priors:requires_active_enforcement(gelassenheit_separation__principle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__principle_reading, '47c8c7c6-25c9-4ac8-9fb5-6b2287f5c2d1').
narrative_ontology:cs_kernel_codification('47c8c7c6-25c9-4ac8-9fb5-6b2287f5c2d1', distributed).
narrative_ontology:cs_authority_grounding('47c8c7c6-25c9-4ac8-9fb5-6b2287f5c2d1', lineage).
narrative_ontology:cs_interpretation_layer_present('47c8c7c6-25c9-4ac8-9fb5-6b2287f5c2d1').
narrative_ontology:cs_reading_relation('47c8c7c6-25c9-4ac8-9fb5-6b2287f5c2d1', gelassenheit_separation__artifact_reading, coexists_with).
narrative_ontology:cs_reading_relation('47c8c7c6-25c9-4ac8-9fb5-6b2287f5c2d1', gelassenheit_separation__consequence_reading, influences).
narrative_ontology:cs_axiom('47c8c7c6-25c9-4ac8-9fb5-6b2287f5c2d1', foundational, isolation_test_is_functional_not_visual).
narrative_ontology:cs_axiom_status(isolation_test_is_functional_not_visual, holdable).
narrative_ontology:cs_axiom_grounding('47c8c7c6-25c9-4ac8-9fb5-6b2287f5c2d1', isolation_test_is_functional_not_visual, conventional).
narrative_ontology:cs_axiom('47c8c7c6-25c9-4ac8-9fb5-6b2287f5c2d1', secondary, structural_entanglement_defines_worldliness).
narrative_ontology:cs_axiom_status(structural_entanglement_defines_worldliness, holdable).
narrative_ontology:cs_axiom_grounding('47c8c7c6-25c9-4ac8-9fb5-6b2287f5c2d1', structural_entanglement_defines_worldliness, instrumental).
narrative_ontology:cs_reference_frame('47c8c7c6-25c9-4ac8-9fb5-6b2287f5c2d1', functional_isolation_ordnung).
narrative_ontology:cs_drift_state('47c8c7c6-25c9-4ac8-9fb5-6b2287f5c2d1', contemporary_off_grid_technology_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('47c8c7c6-25c9-4ac8-9fb5-6b2287f5c2d1', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__principle_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, settled_church_members).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, bishops_and_ministers).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, technically_progressive_households).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, small_business_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicate case-by-case whether a given technology counts as 'functionally isolated' — solar panels off-grid, pneumatic tools disconnected from the public grid, batteries charged independently. They set the ordnung through this principle and can grant or withhold approval for individual applications (a phone booth in a shop, a diesel generator). Their authority rests on the coherence of the isolation test, which requires continuous case adjudication rather than a fixed visible-artifact checklist.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, bishops_and_ministers, agenda_setter,
    institutional, generational, constrained, regional).

% Live within the settlement, benefiting from a rule that lets them adopt functionally useful tools (solar water pumps, non-grid-tied power tools) without visible stigma, so long as the tool does not create dependency on outside systems. They gain productivity and modest technological comfort while remaining inside the community's good standing.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, settled_church_members, beneficiary,
    moderate, generational, constrained, local).

% Want internet access for a home business or want commercial insurance to protect a farm from catastrophic loss. Under this reading, the tool is forbidden not because it looks worldly but because it creates structural entanglement with outside systems — no case-by-case functional argument saves it. They must forgo the tool or leave the church, even though the tool would not visibly distinguish them from their neighbors.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, technically_progressive_households, payer,
    powerless, biographical, trapped, local).

% Run furniture shops, produce stands, or contracting businesses that compete against non-Amish rivals with instant online ordering and insured liability coverage. They bear a real competitive cost from the principle's blanket bar on internet and insurance, regardless of how isolated their actual equipment is, while their bishops evaluate their other tools individually and often favorably.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, small_business_operators, payer,
    powerless, biographical, constrained, regional).

% Are categorically barred from serving this population regardless of what functional-isolation argument might be made for a given product (e.g. an offline-capable device, a mutual-aid-compatible risk pool). They have no standing in the bishops' deliberation and no incentive to seek it, since the prohibition is structural rather than case-specific.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, insurance_and_telecom_providers, excluded,
    powerful, biographical, arbitrage, national).

% Study how different Amish and conservative Anabaptist communities operationalize gelassenheit into concrete technology rulings. They document that the principle reading produces more internal consistency and fewer arbitrary visible-symbol distinctions than the artifact reading, but still generates categorical bans (internet, insurance) that a pure functional test alone would not obviously require — suggesting the 'structural entanglement' criterion smuggles in judgments beyond mere isolation.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, religious_studies_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent, principle-based test — structural entanglement with worldly systems, not surface appearance — that lets church leadership adjudicate new technologies consistently as they emerge, avoiding both wholesale technological refusal and unprincipled case-by-case improvisation.
% TRANSFER_FUNCTION: Moves competitive and economic opportunity away from members whose livelihoods depend on categorically-barred infrastructure (internet, insurance) toward the settled majority whose productivity gains from isolable tools (solar, pneumatics) come with no such structural entanglement cost.
% ABSENT_VOICES: Technically progressive households and small business operators who could argue their internet or insurance use is functionally bounded (a firewalled connection, a narrowly-scoped policy) have no forum: the principle treats certain categories as entangling per se, foreclosing the individualized functional argument the reading otherwise claims to offer.
% DISAPPEARANCE_RATIONALE: If the principle-based separation test vanished, church districts would lose their operative standard for evaluating new technology case by case; without it they would either default to the stricter visible-difference test (artifact reading) or fragment into ad hoc individual permissions, materially changing which households could adopt internet-based commerce and insurance products.
% FOUNDING_PROBLEM: Early Anabaptist communities needed a way to admit genuinely useful tools (motorized equipment, later solar power) without collapsing the community's separateness from surrounding systems of finance, communication, and mutual dependency that these communities hold responsible for eroding earlier Anabaptist and Radical Reformation communities.
% FOUNDING_PROBLEM_CORROBORATION: Sociologists of Amish technology adoption (e.g. studies documenting differential adoption of solar vs. grid electricity across affiliations) corroborate that the entanglement concern tracks real observed patterns of outmigration and economic dependency following unrestricted technology adoption in some historically related communities; this corroboration comes from outside the ordained leadership who administer the rule.
narrative_ontology:disappearance_verdict(gelassenheit_separation__principle_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__principle_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__principle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gelassenheit_separation__principle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__principle_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__principle_reading_tests).
:- end_tests(gelassenheit_separation__principle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28) is moderate-low: most members experience the rule as enabling (solar, tools) rather than costly, and the categorical bans fall on a minority whose livelihoods depend on the barred categories. Suppression (0.42) is moderate: enforcement is real (shunning, loss of standing) but the rule's coherence reduces arbitrary application relative to a pure visible-difference standard. Theater ratio is low (0.15) because the adjudication does real interpretive work case by case; it is not mostly performative. Accessibility collapse (0.5) and resistance (0.45) reflect that alternatives (leaving the district, joining a more liberal affiliation) exist and are exercised, unlike a true mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Bishops and ministers are agenda-setters who benefit from the interpretive authority the principle affords them — they, not a fixed rulebook, decide what counts as structural entanglement. Settled members benefit from access to genuinely useful isolated technology without stigma. Progressive households and small business operators bear concentrated costs: the same principle that would seem to permit a case-by-case functional argument for limited internet use in fact treats internet and insurance as categorically entangling, cutting off the individualized argument the reading's own logic would otherwise support.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing the erosion of community interdependence through outside financial and communication systems — remains live by external sociological corroboration, which is why this is not classified as inertial (piton). But the categorical treatment of internet/insurance, uncoupled from actual case-by-case functional analysis the principle claims to offer elsewhere, is the seam where mandatrophy risk concentrates: if genuinely isolable, low-entanglement versions of these technologies became available and were still categorically barred, the rule would be defending a symbolic line under principle-language rather than the structural-entanglement function it claims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internet_insurance_categorical_vs_functional,
    'Is the categorical bar on internet and insurance actually derived from the functional-isolation principle, or is it a residual visible-distinction/consequence judgment imported into a reading that claims pure functional analysis?',
    'Track whether any district applying this reading has approved a genuinely isolated, non-networked internet-adjacent technology (e.g. an offline local mesh) or a narrowly-scoped mutual-aid-compatible insurance product; approval would support the pure-principle account, continued blanket refusal would support the smuggled-judgment account.',
    'If the bar is not actually principle-derived, the reading is less internally coherent than claimed and some of its ''moderate'' extraction score should be revised upward, since it forecloses the case-by-case argument for a subset of members without the functional justification it offers to others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internet_insurance_categorical_vs_functional, conceptual, 'Whether the internet/insurance ban is genuinely functional or a disguised symbolic judgment.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the principle reading diverge from the artifact and consequence readings in practice, given that all three readings are administered by the same bishops using overlapping vocabulary (separation, worldliness, entanglement)?',
    'Comparative fieldwork across districts that self-identify as applying different reasoning, cross-checking actual technology rulings against the stated test used to justify them.',
    'If the practical rulings converge across readings despite different official rationales, the three kernel readings may be less structurally distinct than the kernel contest assumes, which would bear on how sharply this story''s ε should differ from its siblings'' ε.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Whether the principle reading''s boundary is a real practical divergence from sibling readings or a rhetorical difference over the same rulings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__principle_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t0, gelassenheit_separation__principle_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gela_tr_t8, gelassenheit_separation__principle_reading, theater_ratio, 8, 0.11).
narrative_ontology:measurement(gela_tr_t16, gelassenheit_separation__principle_reading, theater_ratio, 16, 0.12).
narrative_ontology:measurement(gela_tr_t24, gelassenheit_separation__principle_reading, theater_ratio, 24, 0.13).
narrative_ontology:measurement(gela_tr_t32, gelassenheit_separation__principle_reading, theater_ratio, 32, 0.14).
narrative_ontology:measurement(gela_tr_t40, gelassenheit_separation__principle_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(gela_be_t0, gelassenheit_separation__principle_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(gela_be_t8, gelassenheit_separation__principle_reading, base_extractiveness, 8, 0.2).
narrative_ontology:measurement(gela_be_t16, gelassenheit_separation__principle_reading, base_extractiveness, 16, 0.22).
narrative_ontology:measurement(gela_be_t24, gelassenheit_separation__principle_reading, base_extractiveness, 24, 0.25).
narrative_ontology:measurement(gela_be_t32, gelassenheit_separation__principle_reading, base_extractiveness, 32, 0.27).
narrative_ontology:measurement(gela_be_t40, gelassenheit_separation__principle_reading, base_extractiveness, 40, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t0, gelassenheit_separation__principle_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(gela_su_t8, gelassenheit_separation__principle_reading, suppression_requirement, 8, 0.33).
narrative_ontology:measurement(gela_su_t16, gelassenheit_separation__principle_reading, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(gela_su_t24, gelassenheit_separation__principle_reading, suppression_requirement, 24, 0.38).
narrative_ontology:measurement(gela_su_t32, gelassenheit_separation__principle_reading, suppression_requirement, 32, 0.4).
narrative_ontology:measurement(gela_su_t40, gelassenheit_separation__principle_reading, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__principle_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gelassenheit_separation__principle_reading, 0.1).
narrative_ontology:affects_constraint(gelassenheit_separation__principle_reading, gelassenheit_separation__artifact_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__principle_reading, gelassenheit_separation__consequence_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the colloquial 'Amish technology separation rule' per the epsilon-invariance principle. The artifact reading bars technology on visible resemblance to worldly goods regardless of function (higher suppression, more symbolic enforcement). The consequence reading evaluates technology by its effect on visiting, mutual aid, and geographic rootedness (a different beneficiary/victim structure centered on community-cohesion effects rather than entanglement per se). This principle reading has the lowest epsilon of the three because its functional test permits more technology case-by-case, but it still carries categorical bans (internet, insurance) that are not fully derived from its own stated logic — the seam documented in the omega variables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
