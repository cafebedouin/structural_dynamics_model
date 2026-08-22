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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: gelassenheit_separation__principle_reading
 *   human_readable: Gelassenheit Separation — Structural-Isolation (Principle) Reading
 *   domain: religious/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the PRINCIPLE READING of the
 *   gelassenheit_separation kernel: separation is understood as the avoidance
 *   of structural entanglement in worldly economic and informational systems,
 *   tested by function rather than appearance. Under this reading, a solar
 *   array charging an isolated battery bank, or a pneumatic tool run from a
 *   community-owned compressor, is acceptable because it creates no ongoing
 *   dependency relationship — even though it looks technologically modern.
 *   Conversely, internet access and commercial insurance are forbidden
 *   categorically, not because they resemble worldly artifacts, but because
 *   the reading treats them as inherently structurally entangling: a
 *   continuing contractual or informational relationship with an outside
 *   institution that cannot be made isolated by narrowing its scope. This
 *   produces a lower extraction profile than the artifact reading (which
 *   would forbid the solar array and pneumatic tools on resemblance grounds)
 *   but retains a hard, non-negotiable exclusion zone around
 *   network/financial entanglement that the isolation logic, taken to its own
 *   conclusion, might otherwise soften. That tension — a functional test that
 *   nonetheless hardens into a bright-line rule for two specific categories —
 *   is exactly the site of internal contest with sibling readings and is
 *   documented in the omegas below rather than resolved here.
 *
 * KEY AGENTS:
 *   - ordnung_ministers: agenda-setters who administer the case-by-case isolation test
 *   - community_households and off_grid_tradespeople: beneficiaries of the reading's comparatively permissive stance on self-contained tools
 *   - technically_progressive_youth and farm_operations_needing_connectivity: payers who bear the cost of the categorical internet/insurance exclusion
 *   - outside_regulators_and_neighbors: excluded parties with practical exposure but no voice
 *   - comparative_religious_scholars: analytical observers documenting the reading's distinct adoption pattern
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
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__principle_reading, rope).
narrative_ontology:human_readable(gelassenheit_separation__principle_reading, "Gelassenheit Separation — Structural-Isolation (Principle) Reading").
narrative_ontology:topic_domain(gelassenheit_separation__principle_reading, "religious/technology_governance").

domain_priors:requires_active_enforcement(gelassenheit_separation__principle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__principle_reading, 'da090057-ac50-4af5-a04e-4e9029825585').
narrative_ontology:cs_kernel_codification('da090057-ac50-4af5-a04e-4e9029825585', distributed).
narrative_ontology:cs_authority_grounding('da090057-ac50-4af5-a04e-4e9029825585', practice).
narrative_ontology:cs_interpretation_layer_present('da090057-ac50-4af5-a04e-4e9029825585').
narrative_ontology:cs_reading_relation('da090057-ac50-4af5-a04e-4e9029825585', gelassenheit_separation__artifact_reading, coexists_with).
narrative_ontology:cs_reading_relation('da090057-ac50-4af5-a04e-4e9029825585', gelassenheit_separation__consequence_reading, coexists_with).
narrative_ontology:cs_axiom('da090057-ac50-4af5-a04e-4e9029825585', foundational, structural_entanglement_is_the_separation_test).
narrative_ontology:cs_axiom_status(structural_entanglement_is_the_separation_test, holdable).
narrative_ontology:cs_axiom_grounding('da090057-ac50-4af5-a04e-4e9029825585', structural_entanglement_is_the_separation_test, conventional).
narrative_ontology:cs_axiom('da090057-ac50-4af5-a04e-4e9029825585', secondary, functional_isolation_sufficiency_doctrine).
narrative_ontology:cs_axiom_status(functional_isolation_sufficiency_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('da090057-ac50-4af5-a04e-4e9029825585', functional_isolation_sufficiency_doctrine, instrumental).
narrative_ontology:cs_reference_frame('da090057-ac50-4af5-a04e-4e9029825585', functional_isolation_test_tradition).
narrative_ontology:cs_drift_state('da090057-ac50-4af5-a04e-4e9029825585', contemporary_leasing_and_networked_agriculture_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('da090057-ac50-4af5-a04e-4e9029825585', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__principle_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, ordnung_ministers).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, community_households).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, off_grid_tradespeople).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, technically_progressive_youth).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, farm_operations_needing_connectivity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, community_households).
narrative_ontology:constraint_vindicates(gelassenheit_separation__principle_reading, functional_isolation_sufficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicate case-by-case whether a given tool creates structural entanglement with worldly systems (a grid contract, a monthly bill, a data pipeline to an outside network) versus functioning as an isolated implement (a solar panel charging an isolated battery, a pneumatic tool run off a compressor the community itself owns). They hold discretion because the principle test requires judgment, not a fixed artifact list, and their authority rests on being trusted to apply that judgment consistently.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, ordnung_ministers, agenda_setter,
    institutional, generational, identity_locked, regional).

% Gain workable technology for milking, refrigeration, and small manufacturing as long as it stays functionally self-contained — no ongoing subscription, no grid tie, no data relationship to an outside firm. They pay a real cost in convenience relative to a fully networked household, but retain far more capability than the artifact reading would allow, since a solar array or a stationary diesel generator is permitted purely on isolation grounds regardless of appearance.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, community_households, beneficiary,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(gelassenheit_separation__principle_reading, community_households, payer).

% Run small businesses (cabinetry, welding, produce processing) that depend on generator power, pneumatic tools, and battery systems the principle reading explicitly clears because they do not tie the business into a continuous outside network. Their livelihood is more viable under this reading than under the artifact reading, which would have to litigate every tool's resemblance to worldly equipment.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, off_grid_tradespeople, beneficiary,
    moderate, biographical, constrained, local).

% Want internet access for schooling, business software, or communication with family who have left the community. Under this reading, internet and insurance are forbidden categorically regardless of whether the connection could be made narrow or isolable — a firewalled, single-purpose data link is treated the same as an open one, because the entanglement runs through the contractual and infrastructural relationship itself, not through what flows over it. They bear the cost of a bright-line rule that admits no functional workaround, even though the reading's own logic (isolation, not appearance) seems like it should leave room for one.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, technically_progressive_youth, payer,
    powerless, biographical, constrained, local).

% Operate dairy or produce businesses that increasingly require real-time market data, milk co-op reporting systems, or crop insurance to remain financially viable competing against non-plain neighbors. The principle reading blocks insurance and internet outright as inherently structurally entangling — a continuing dependency relationship with an outside institution — even when a narrowly scoped version might otherwise pass an isolation test. They absorb the resulting competitive disadvantage as the cost of the rule holding a firm line on two specific categories.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, farm_operations_needing_connectivity, payer,
    moderate, biographical, constrained, regional).

% County zoning boards, insurers, and non-plain neighbors interact with the community's infrastructure choices (septic systems, generator noise, uninsured liability after accidents) but have no voice in how the ministers apply the isolation test. Their practical exposure (e.g., liability from an uninsured plain farm accident) is not represented in the deliberation that sets or maintains the rule.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, outside_regulators_and_neighbors, excluded,
    institutional, biographical, analytical, regional).

% Study how different Anabaptist-tradition communities operationalize separation doctrine differently — some by artifact resemblance, some by consequence to community bonds, some by this reading's structural-entanglement test. They document the principle reading's comparatively higher adoption of tools (solar, pneumatics) alongside its equally hard exclusions (internet, insurance) as a distinct empirical pattern from the other two readings.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, comparative_religious_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gelassenheit_separation__principle_reading, diffuse).
narrative_ontology:fixing_cost_class(gelassenheit_separation__principle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives ministers a workable, principle-based standard for evaluating novel technologies without needing to enumerate every device in advance: ask whether adopting it creates an ongoing structural dependency on worldly institutions and networks, not whether it superficially resembles a worldly artifact.
% TRANSFER_FUNCTION: Moves practical capability (which households and trades may use which tools) from a broad worldly baseline down to a narrower isolation-tested set, while moving discretionary authority over that boundary to the ministers who apply the test.
% ABSENT_VOICES: Technically progressive youth and connectivity-dependent farm operations would argue that a narrowly scoped, firewalled internet connection or a minimal-liability insurance product could pass an honest isolation test, but the categorical exclusion of internet/insurance forecloses that argument before it can be heard by the same ministers who apply nuanced judgment to solar and pneumatic tools.
% DISAPPEARANCE_RATIONALE: If this reading's specific test vanished, communities following it would either default to the stricter artifact reading (losing solar and pneumatic tools currently justified by isolation) or drift toward ad hoc individual technology adoption absent any shared standard, materially changing which households can run diesel generators, solar arrays, and pneumatic shop tools, and reopening the internet/insurance exclusions to case-by-case litigation.
% FOUNDING_PROBLEM: Communities needed a principled way to distinguish technologies that entangle the community in worldly economic and informational networks from technologies that merely improve isolated, self-contained work — without either freezing adoption at a fixed historical artifact list or allowing unlimited technological assimilation.
% FOUNDING_PROBLEM_CORROBORATION: Ministers themselves attest the entanglement problem remains live (citing recent disputes over solar leasing arrangements that reintroduce a vendor relationship). Comparative religious scholars, writing from outside the community's own governance, corroborate that the entanglement concern is not merely rhetorical — communities using this reading show measurably different technology-adoption patterns than artifact-reading communities, consistent with a genuine functional test rather than a post-hoc justification for whatever ministers prefer.
narrative_ontology:disappearance_verdict(gelassenheit_separation__principle_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__principle_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__principle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extraction (0.28) is authored lower than a plausible artifact-reading score because the principle test actually expands permitted technology (solar, pneumatics, generators) relative to a resemblance-based test, and most households experience the constraint as a workable accommodation rather than a felt cost. Suppression (0.42, rising modestly over the interval as leasing/vendor arrangements proliferate and require more active minister judgment) reflects that enforcement still requires real interpretive labor and occasional hard refusals — particularly on internet and insurance, where the reading's own functional logic is overridden by a categorical rule. Theater ratio is low (0.15) because the isolation test is substantively applied case-by-case, not performed as ritual; there is little daylight between the stated function (assess structural entanglement) and the actual practice.
 *
 * PERSPECTIVAL GAP:
 *   From the ministers' seat, the constraint is a coherent, principle-driven rope: a genuine coordination solution to the problem of evaluating novel technology without either ossifying or capitulating. From the seat of technically progressive youth and connectivity-dependent farm operations, the same reading computes closer to a tangled arrangement: they receive the coordination benefit of a functional (not resemblance-based) test in some domains, yet are structurally overridden by a categorical exclusion in exactly the domains (internet, insurance) where the isolation logic would, if applied consistently, offer them the most relief. The engine should register this seat divergence directly from the differentiated exit_options and roles, not from any narrative adjustment.
 *
 * DIRECTIONALITY LOGIC:
 *   Ordnung ministers and community/trade beneficiaries sit near the beneficiary end: they receive expanded technological capability and administrative flexibility from the principle test relative to stricter readings. Progressive youth and connectivity-dependent farms sit near the target end: they bear a real, felt cost from a bright-line exclusion that the reading's own logic does not obviously require, and their exit options are constrained by community and economic embeddedness (leaving to gain internet access means leaving the community, not merely acquiring a permitted tool).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (distinguishing entangling from isolated technology) remains live by the ministers' own testimony and by outside scholarly corroboration showing a real, distinct adoption pattern — this is not a hollowed-out mandate performing separation theater while quietly permitting assimilation. The categorical internet/insurance exclusion is the one place mandatrophy risk concentrates: if the underlying entanglement concern could in principle be satisfied by a narrowly isolated technical implementation (a firewalled offline terminal, a minimal no-renewal insurance product), and the rule persists as categorical anyway, the founding principle (function over form) would have calcified into an artifact-style bright line for exactly two categories — a partial mandatrophy limited to that boundary rather than the whole constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internet_insurance_categorical_exception_coherence,
    'Is the categorical (non-isolation-testable) exclusion of internet and insurance internally consistent with the principle reading''s own functional-isolation logic, or is it an artifact-reading holdover smuggled into a principle-reading framework?',
    'Examine whether any community applying the principle reading has ever permitted a narrowly scoped, firewalled, or non-renewing version of either technology; if none has, despite the isolation logic seeming to permit it in principle, that is evidence the exclusion is doctrinally load-bearing rather than functionally derived.',
    'If the exclusion is a holdover rather than a functional derivation, the principle reading''s claimed lower epsilon is overstated for the two excluded categories, and those specific sub-constraints should arguably be evaluated closer to the artifact reading''s terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internet_insurance_categorical_exception_coherence, conceptual, 'Whether the internet/insurance bright line is a principled derivation or an artifact-reading residue.').

omega_variable(
    minister_discretion_vs_capture,
    'Does minister discretion in applying the isolation test track the community''s genuine entanglement concerns, or does it drift toward outcomes that entrench minister authority (since a fixed artifact list would require no ongoing interpretive role, while a discretionary principle test does)?',
    'Track whether contested cases are resolved consistently across ministers and over time, or whether resolution correlates with which minister holds interpretive authority in a given community.',
    'If discretion correlates with minister identity more than with entanglement facts, part of the measured suppression is attributable to authority-preservation rather than genuine separation function, which would push the constraint toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minister_discretion_vs_capture, empirical, 'Whether ministerial discretion tracks entanglement facts or entrenches administrative authority.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given that all three readings (artifact, consequence, principle) claim to interpret the same gelassenheit_separation commitment, is there any community-external evidence for which reading is doctrinally prior, or are all three equally legitimate contemporary elaborations with no ranking?',
    'Historical-textual analysis of founding Anabaptist separation doctrine and its early applications, cross-referenced with which reading individual communities trace their own lineage to.',
    'If the principle reading can be shown to be a later rationalization rather than a doctrinally continuous reading, its lower measured epsilon may reflect selective self-presentation rather than a structurally distinct kernel instantiation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the three sibling readings have a doctrinal priority ordering or are co-equal contemporary elaborations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__principle_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t0, gelassenheit_separation__principle_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(gela_tr_t8, gelassenheit_separation__principle_reading, theater_ratio, 8, 0.09).
narrative_ontology:measurement(gela_tr_t16, gelassenheit_separation__principle_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement(gela_tr_t24, gelassenheit_separation__principle_reading, theater_ratio, 24, 0.12).
narrative_ontology:measurement(gela_tr_t32, gelassenheit_separation__principle_reading, theater_ratio, 32, 0.13).
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
% This story is one of three constraints decomposed from the natural-language 'gelassenheit separation' concept per the ε-invariance principle. Each reading (artifact, consequence, principle) instantiates a structurally distinct test for what counts as impermissible worldly entanglement and produces a different ε: the artifact reading is expected to show the highest suppression (broadest technology exclusion by resemblance), the consequence reading varies by social-effect measurement, and this principle reading shows the lowest baseline epsilon but retains two hard categorical exclusions (internet, insurance) that sit in tension with its own functional logic. All three share the same underlying kernel commitment to gelassenheit but diverge in how separation is operationalized, which is why they are authored as separate linked constraints rather than one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
