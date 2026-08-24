% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__lapsed_alternative_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: market_as_natural_default__lapsed_alternative_reading
 *   human_readable: Market Dominance as Natural Default (Lapsed Alternative Reading)
 *   domain: political_economy/ideology_studies/economic_history
 *
 * SUMMARY:
 *   This constraint story instantiates the lapsed_alternative_reading of the
 *   kernel 'market_as_natural_default'. The reading holds that the
 *   taken-for-granted naturalness of market dominance is not a law of social
 *   physics but a D3 artifact: alternative economic imaginaries (guild
 *   economies, municipal provisioning, commons-based systems, associational
 *   socialism) were not actively crushed so much as they lapsed from
 *   collective memory through institutional attrition, war disruption, and
 *   the gradual crowding-out of non-market provision. The naturalization
 *   persists because no living generation experienced the alternatives, not
 *   because a beneficiary class actively polices the boundary. Extractiveness
 *   is low (ε ≤ 0.15) because the constraint does not operate by extracting
 *   rents from a victim class; it operates by narrowing the cognitive horizon
 *   of the possible. Suppression is low because alternatives are not
 *   forbidden — they are simply unthought. Theater is low because there is no
 *   performative enforcement apparatus; the constraint maintains itself
 *   through absence rather than presence.
 *
 * KEY AGENTS:
 *   - historical_researchers: Excluded analysts — recover forgotten alternatives but are marginalized by dominant economic epistemology
 *   - general_public: Identity-locked subjects — their economic imagination is bounded by the naturalized constraint; exit requires conceptual breakthrough, not physical relocation
 *   - mainstream_economists: Agenda setters (unwitting) — reproduce the naturalization through modeling frameworks that treat market dominance as equilibrium rather than history
 *   - policy_makers: Beneficiaries (diffuse) — inherit a governance vocabulary that only speaks market; not conscious rent-seekers but structurally advantaged by the constraint
 *   - critical_economists: Observers — contest the naturalization from within the discipline but lack institutional leverage to shift the paradigm
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__lapsed_alternative_reading, 0.1).
domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, 0.1).
domain_priors:theater_ratio(market_as_natural_default__lapsed_alternative_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__lapsed_alternative_reading, mountain).
narrative_ontology:human_readable(market_as_natural_default__lapsed_alternative_reading, "Market Dominance as Natural Default (Lapsed Alternative Reading)").
narrative_ontology:topic_domain(market_as_natural_default__lapsed_alternative_reading, "political_economy/ideology_studies/economic_history").

domain_priors:emerges_naturally(market_as_natural_default__lapsed_alternative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__lapsed_alternative_reading, 'efd1a4d4-9538-4f31-8357-67b40f7f1636').
narrative_ontology:cs_kernel_codification('efd1a4d4-9538-4f31-8357-67b40f7f1636', distributed).
narrative_ontology:cs_authority_grounding('efd1a4d4-9538-4f31-8357-67b40f7f1636', expertise).
narrative_ontology:cs_interpretation_layer_present('efd1a4d4-9538-4f31-8357-67b40f7f1636').
narrative_ontology:cs_reading_relation('efd1a4d4-9538-4f31-8357-67b40f7f1636', market_as_natural_default__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('efd1a4d4-9538-4f31-8357-67b40f7f1636', market_as_natural_default__hybrid_amnesia_reading, coexists_with).
narrative_ontology:cs_axiom('efd1a4d4-9538-4f31-8357-67b40f7f1636', foundational, market_naturalization_is_historical_artifact).
narrative_ontology:cs_axiom_status(market_naturalization_is_historical_artifact, holdable).
narrative_ontology:cs_axiom_grounding('efd1a4d4-9538-4f31-8357-67b40f7f1636', market_naturalization_is_historical_artifact, empirically_contingent).
narrative_ontology:cs_axiom('efd1a4d4-9538-4f31-8357-67b40f7f1636', secondary, alternatives_recoverable_through_research).
narrative_ontology:cs_axiom_status(alternatives_recoverable_through_research, holdable).
narrative_ontology:cs_axiom_grounding('efd1a4d4-9538-4f31-8357-67b40f7f1636', alternatives_recoverable_through_research, empirically_contingent).
narrative_ontology:cs_reference_frame('efd1a4d4-9538-4f31-8357-67b40f7f1636', post_feudal_coordination_crisis).
narrative_ontology:cs_drift_state('efd1a4d4-9538-4f31-8357-67b40f7f1636', contemporary_neoliberal_hegemony, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('efd1a4d4-9538-4f31-8357-67b40f7f1636', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_as_natural_default__lapsed_alternative_reading, policy_makers).
narrative_ontology:constraint_victim(market_as_natural_default__lapsed_alternative_reading, general_public).
narrative_ontology:constraint_vindicates(market_as_natural_default__lapsed_alternative_reading, market_naturalness_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Recover forgotten economic imaginaries through archival and comparative work. Their findings are marginalized by dominant economics curricula and policy discourse. Exit from exclusion requires interdisciplinary coalition-building and institutional footholds (e.g., heterodox economics departments, policy labs).
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, historical_researchers, excluded,
    moderate, biographical, constrained, global).

% Live within the naturalized market order; their economic imagination is bounded by it. They bear the cost of foreclosed alternatives (e.g., no municipal broadband, no commons-based care, no guild-style training) but cannot articulate the cost because the alternatives are unthinkable. Exit requires a conceptual breakthrough — learning that other worlds existed and could exist again.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, general_public, payer,
    powerless, biographical, identity_locked, global).

% Reproduce the naturalization through modeling frameworks (general equilibrium, rational choice) that treat market dominance as efficient equilibrium rather than historical contingency. They are not conscious defenders of a rent; the constraint simplifies their science. Exit is easy individually (switch to heterodox frameworks) but costly professionally (tenure, publication, policy influence).
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, mainstream_economists, agenda_setter,
    institutional, generational, arbitrage, global).

% Inherit a governance vocabulary that only speaks market (privatization, competition, incentives). They are structurally advantaged because the constraint makes their job legible — but they are not rent-seekers. Exit requires a new policy paradigm, which is institutionally risky.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, policy_makers, beneficiary,
    institutional, biographical, constrained, national).

% Contest the naturalization from within the discipline (institutional economics, feminist economics, ecological economics). They have analytical freedom but limited institutional leverage. Their exit is mobile — they can publish, teach, and organize in heterodox spaces — but those spaces are resource-poor.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, critical_economists, observer,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__lapsed_alternative_reading, diffuse).
narrative_ontology:fixing_cost_class(market_as_natural_default__lapsed_alternative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a stable, legible coordination framework for complex exchange after the collapse of feudal/guild economies. The market grammar (price, contract, property) solved the coordination problem of scaling trust beyond face-to-face networks.
% TRANSFER_FUNCTION: Does not transfer resources in the extractive sense. Transfers cognitive authority: the market frame becomes the only legitimate language for public provisioning decisions, displacing languages of need, care, commons, and democratic allocation.
% ABSENT_VOICES: Historical researchers and critical economists who would challenge the naturalization if they were heard, but are marginalized by the dominant paradigm's control of curricula, journals, and policy advisory channels. Also absent: the forgotten alternatives themselves — guild masters, municipal socialists, commons stewards — whose lived experience of non-market coordination is erased from the historical record.
% DISAPPEARANCE_RATIONALE: If the naturalization constraint vanished overnight, the cognitive horizon would expand: policy debates would include municipal provisioning, commons governance, and associational economies as live options. The world would rearrange because the constraint is the *grammar* of the possible, not a specific rule.
% FOUNDING_PROBLEM: After the collapse of feudal and guild economies (14th–18th centuries), European societies needed a scalable coordination mechanism for production and exchange that did not depend on personal loyalty or local custom. The market grammar (impersonal price signals, standardized contract, alienable property) solved this.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians (Polanyi, Braudel, Arrighi) document that the founding problem was the scaling of trust beyond kinship/guild — and that this problem was solved by the 19th century. Contemporary institutional economists (Ostrom, Williamson) demonstrate that non-market coordination scales effectively (commons, firms, networks). No serious scholar outside the neoclassical paradigm claims the founding problem remains live; the neoclassical paradigm itself treats the market as a timeless efficient form, not a historical solution to a past problem.
narrative_ontology:disappearance_verdict(market_as_natural_default__lapsed_alternative_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__lapsed_alternative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__lapsed_alternative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_as_natural_default__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__lapsed_alternative_reading, 0.1, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__lapsed_alternative_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, ExtMetricName, E),
    domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(market_as_natural_default__lapsed_alternative_reading),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(market_as_natural_default__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.1: the constraint does not transfer resources from a victim class to a beneficiary class; its cost is the foreclosure of institutional imagination, which is real but not extractive in the χ sense. Suppression 0.1: no active closure, no censorship of alternatives — they are absent from the curriculum, not banned. Theater 0.1: no ceremonial enforcement; the constraint is the water fish swim in. Accessibility_collapse 0.4: alternatives are recoverable through research (hence not mountain-typical 0.85+), but the collapse is significant because the alternatives are not merely unknown — they are structurally unthinkable within the dominant paradigm. Resistance 0.1: resistance is low because the constraint does not provoke; it precludes. The metrics are deliberately inconsistent with a pure mountain (accessibility_collapse too low, resistance too low for a natural law that admits no alternatives), which is the point: the reading claims the constraint *presents* as mountain but *is* a historical artifact.
 *
 * PERSPECTIVAL GAP:
 *   From the general_public seat (identity_locked, powerless), the constraint feels like a mountain — alternatives are not just unavailable, they are inconceivable. From the historical_researchers seat (excluded, moderate power), the constraint is a fragile cultural artifact that dissolves under archival scrutiny. From the mainstream_economists seat (agenda_setter, institutional power), the constraint is a productive simplifying assumption that enables modeling. The engine will compute different effective extraction χ for each seat because directionality d differs: general_public d ≈ 0.7 (subject to the constraint's horizon-narrowing), historical_researchers d ≈ 0.2 (they can see beyond it), mainstream_economists d ≈ 0.1 (they benefit from the constraint's simplifying power).
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary class is declared because the reading's core claim is that naturalization persists without a rent-collecting coalition. The general_public bears the cost of narrowed imagination (d → target end). Historical_researchers and critical_economists sit near the analytical end (d ≈ 0.2) because their epistemic position lets them see the constraint's contingency. Mainstream_economists and policy_makers sit near the beneficiary end (d ≈ 0.1–0.15) because the constraint reduces their cognitive transaction costs — but they do not *collect* from it, so they are not beneficiaries in the extraction sense. The absence of victims in base_properties reflects the reading's claim that no class is actively extracted from; the cost is diffuse and non-transferential.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (see six_questions) was the need for a stable coordination framework after the collapse of feudal and guild economies. That problem is dead — we now have abundant evidence of non-market coordination at scale. Yet the constraint persists because it solved the founding problem so thoroughly that it became the only grammar of economic legitimacy. This is mandatrophy: the mandate (provide a stable coordination framework) has been fulfilled, but the constraint (market dominance as the *only* legitimate form) has outlived its function and persists through institutional inertia and cognitive path-dependence. The classification prevents mislabeling by showing low extractiveness and low suppression — this is not a snare disguised as coordination; it is a coordination grammar that has become a cognitive trap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'Is the naturalization of market dominance a genuine natural law (Mountain) or a historical artifact of forgotten alternatives (this reading), vs. an actively defended beneficiary construct (beneficiary_maintained_reading) or a hybrid amnesia-capture dynamic (hybrid_amnesia_reading)?',
    'Comparative historical analysis of market ideology formation: trace whether alternative economic imaginaries were actively suppressed or simply lapsed from collective memory; examine whether identifiable beneficiary classes emerged to defend the naturalization.',
    'If beneficiary classes are identified, FSM triggers reclassification to tangled_rope; if active suppression is documented, snare/tangled_rope becomes plausible; if lapsed memory is confirmed, mountain classification holds but with low accessibility_collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Commitment-system framing: which reading of the kernel ''market_as_natural_default'' captures the structural reality?').

omega_variable(
    recoverability_of_alternatives,
    'Can historical alternatives to market dominance be substantively recovered and reanimated, or has the forgetting caused irreversible conceptual collapse?',
    'Empirical test: commission interdisciplinary research programs to reconstruct and field-test forgotten economic imaginaries (e.g., guild economies, municipal socialism, commons-based provisioning); measure uptake and viability.',
    'If alternatives are recoverable and viable, accessibility_collapse is lower than mountain-typical and the constraint is a reversible cultural artifact; if irreversibly collapsed, mountain classification strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(recoverability_of_alternatives, empirical, 'Whether the D3 naturalization artifact is reversible through historical research and institutional experimentation.').

omega_variable(
    hidden_beneficiary_structure,
    'Does the absence of an identifiable beneficiary class reflect genuine absence, or is the beneficiary structure diffuse/latent (e.g., a broad propertied class whose interests are served without conscious coordination)?',
    'Counterfactual policy simulation: model distributional effects of de-naturalizing market dominance; identify which groups lose relative advantage. Complement with discourse analysis of who resists alternative imaginaries.',
    'If a diffuse beneficiary class is detected, the constraint shifts toward tangled_rope (coordination + asymmetric extraction) even without a self-conscious beneficiary coalition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hidden_beneficiary_structure, empirical, 'Whether the ''no identifiable beneficiary'' claim survives structural distributional analysis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__lapsed_alternative_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(market_as_natural_default__lapsed_alternative_reading_tr_t0, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(market_as_natural_default__lapsed_alternative_reading_tr_t20, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 20, 0.07).
narrative_ontology:measurement(market_as_natural_default__lapsed_alternative_reading_tr_t40, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(market_as_natural_default__lapsed_alternative_reading_tr_t60, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(market_as_natural_default__lapsed_alternative_reading_tr_t80, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(market_as_natural_default__lapsed_alternative_reading_tr_t100, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(market_as_natural_default__lapsed_alternative_reading_be_t0, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(market_as_natural_default__lapsed_alternative_reading_be_t20, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 20, 0.09).
narrative_ontology:measurement(market_as_natural_default__lapsed_alternative_reading_be_t40, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 40, 0.1).
narrative_ontology:measurement(market_as_natural_default__lapsed_alternative_reading_be_t60, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 60, 0.1).
narrative_ontology:measurement(market_as_natural_default__lapsed_alternative_reading_be_t80, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 80, 0.1).
narrative_ontology:measurement(market_as_natural_default__lapsed_alternative_reading_be_t100, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 100, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(market_as_natural_default__lapsed_alternative_reading_su_t0, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(market_as_natural_default__lapsed_alternative_reading_su_t20, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 20, 0.08).
narrative_ontology:measurement(market_as_natural_default__lapsed_alternative_reading_su_t40, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 40, 0.1).
narrative_ontology:measurement(market_as_natural_default__lapsed_alternative_reading_su_t60, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 60, 0.1).
narrative_ontology:measurement(market_as_natural_default__lapsed_alternative_reading_su_t80, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 80, 0.1).
narrative_ontology:measurement(market_as_natural_default__lapsed_alternative_reading_su_t100, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 100, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__lapsed_alternative_reading, identity_coordination).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default__hybrid_amnesia_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the kernel 'market_as_natural_default' into three readings differing on the role of historical forgetting vs. active beneficiary defense in producing market naturalization. The lapsed_alternative_reading claims no beneficiary class and low extractiveness; the beneficiary_maintained_reading claims active beneficiary defense and higher extractiveness; the hybrid_amnesia_reading claims a sequential dynamic where forgetting enables capture. Their ε values diverge substantially (this reading ε ≤ 0.15; beneficiary_maintained ε ≥ 0.5; hybrid intermediate). They are linked as a family because they share the same referent (the naturalization of market dominance) but disagree on its structural genesis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
