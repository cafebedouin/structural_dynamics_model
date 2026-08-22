% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__coordination_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: article_27_veto_power__coordination_reading
 *   human_readable: Article 27 P5 Veto Power (Coordination Reading)
 *   domain: international_relations/institutional_design
 *
 * SUMMARY:
 *   The Article 27 P5 veto in the UN Security Council is contested across
 *   three distinct structural framings. This story instantiates the
 *   COORDINATION READING: the veto is a necessary unanimity gate preventing
 *   any institutional decision-making process from authorizing military
 *   action against a nuclear-armed state without that state's consent. Under
 *   this reading, the veto solves a genuine collective-action problem
 *   specific to the nuclear era—the risk that smaller-state coalitions could
 *   theoretically vote to authorize war against a great power, creating
 *   escalatory pressure and system-wide conflict. The veto's function is to
 *   prevent a class of institutional failure modes, not to entrench oligopoly
 *   or extract ongoing rents. All states benefit from avoiding such
 *   scenarios, though nuclear powers benefit more directly by holding the
 *   consent gate. No state is a victim of the coordination function itself;
 *   the cost is slower decision-making and occasional deadlock on
 *   non-existential issues.
 *
 * KEY AGENTS:
 *   - nuclear_armed_great_powers: P5 members (US, USSR/Russia, UK, France, China); hold veto authority; benefit by preventing institutional coercion into war.
 *   - smaller_non_permanent_states: General Assembly members without Security Council seats; benefit indirectly from system stability the veto provides.
 *   - un_secretariat: Institutional observer; depends on the veto preventing paralysis from great-power conflict.
 *   - international_law_corpus: Non-agent beneficiary; the legal system's legitimacy depends on institutional substrate stability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__coordination_reading, 0.15).
domain_priors:suppression_score(article_27_veto_power__coordination_reading, 0.08).
domain_priors:theater_ratio(article_27_veto_power__coordination_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, resistance, 0.11).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__coordination_reading, rope).
narrative_ontology:human_readable(article_27_veto_power__coordination_reading, "Article 27 P5 Veto Power (Coordination Reading)").
narrative_ontology:topic_domain(article_27_veto_power__coordination_reading, "international_relations/institutional_design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__coordination_reading, 'a991bbf9-9d5d-4e1f-99c7-5eb799d5098c').
narrative_ontology:cs_kernel_codification('a991bbf9-9d5d-4e1f-99c7-5eb799d5098c', formalized).
narrative_ontology:cs_authority_grounding('a991bbf9-9d5d-4e1f-99c7-5eb799d5098c', lineage).
narrative_ontology:cs_interpretation_layer_present('a991bbf9-9d5d-4e1f-99c7-5eb799d5098c').
narrative_ontology:cs_reading_relation('a991bbf9-9d5d-4e1f-99c7-5eb799d5098c', article_27_veto_power__oligopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('a991bbf9-9d5d-4e1f-99c7-5eb799d5098c', article_27_veto_power__sovereignty_reading, influences).
narrative_ontology:cs_axiom('a991bbf9-9d5d-4e1f-99c7-5eb799d5098c', foundational, nuclear_war_prevention_requires_consent_gate).
narrative_ontology:cs_axiom_status(nuclear_war_prevention_requires_consent_gate, holdable).
narrative_ontology:cs_axiom_grounding('a991bbf9-9d5d-4e1f-99c7-5eb799d5098c', nuclear_war_prevention_requires_consent_gate, instrumental).
narrative_ontology:cs_axiom('a991bbf9-9d5d-4e1f-99c7-5eb799d5098c', foundational, institutional_coercion_of_nuclear_states_destabilizing).
narrative_ontology:cs_axiom_status(institutional_coercion_of_nuclear_states_destabilizing, holdable).
narrative_ontology:cs_axiom_grounding('a991bbf9-9d5d-4e1f-99c7-5eb799d5098c', institutional_coercion_of_nuclear_states_destabilizing, empirically_contingent).
narrative_ontology:cs_reference_frame('a991bbf9-9d5d-4e1f-99c7-5eb799d5098c', great_power_war_prevention_unanimity_gate).
narrative_ontology:cs_drift_state('a991bbf9-9d5d-4e1f-99c7-5eb799d5098c', contemporary_post_cold_war, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('a991bbf9-9d5d-4e1f-99c7-5eb799d5098c', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__coordination_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, international_system_stability).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, nuclear_armed_great_powers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, smaller_non_permanent_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess veto over Security Council resolutions. Under this reading, the veto prevents any UN body from authorizing or mandating military action against them without their consent. This is structurally necessary: without the veto, a coalition of smaller states could theoretically authorize war against a nuclear power, creating existential risk and instability. The veto ensures that no state can be forced into military confrontation by institutional process alone.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, nuclear_armed_great_powers, beneficiary,
    institutional, civilizational, analytical, global).

% Benefit indirectly from the veto system by avoiding scenarios where great-power war paralyzes the UN or spills globally. The veto prevents escalatory institutional authorization cycles that could draw smaller states into forced alliance or conflict. Under this reading, they are net beneficiaries of the stability the veto provides, even without direct veto power.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, smaller_non_permanent_states, beneficiary,
    organized, generational, analytical, global).

% Observes and implements Security Council decisions. Under the coordination reading, the Secretariat's ability to function depends on the UN avoiding great-power conflict that would paralyze it. The veto's coordination function—preventing institutional decisions that would trigger such conflict—enables the Secretariat to operate as intended.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, un_secretariat, observer,
    institutional, generational, analytical, global).

% The body of international legal norms benefits from a stable institutional substrate. Under the coordination reading, the veto prevents institutional arrangements that would trigger great-power conflict and undermine the legitimacy and viability of international law itself.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, international_law_corpus, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(article_27_veto_power__coordination_reading, international_law_corpus).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_27_veto_power__coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(article_27_veto_power__coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The veto solves a collective-action problem specific to the nuclear era: how to prevent any institutional decision-making process from authorizing military action against a state with global-reach retaliatory capacity without that state's consent. Without unanimity, smaller-state coalitions could theoretically vote to authorize war. The veto ensures escalation-resistance: no Security Council majority can bind a nuclear power into military confrontation. This prevents a class of institutional failure modes that would destabilize the international system.
% TRANSFER_FUNCTION: The veto transfers authority restraint FROM institutional process TO the consent of the protected power. No money or material flows; instead, the authority to authorize military action is transferred away from majority voting and held at unanimity. The coordination gain is paid for by slower decision-making and occasional deadlock on non-existential issues.
% ABSENT_VOICES: Mid-sized regional powers and non-aligned movements have historically objected that the P5 veto freezes geopolitical hierarchy in place and prevents institutional reforms that would redistribute power. They are not excluded from UN participation but are structurally excluded from veto authority itself. Their objection is that the veto's coordination function conflates preventing great-power war with preventing institutional evolution.
% DISAPPEARANCE_RATIONALE: If the P5 veto disappeared overnight, institutional authority over military authorization would revert to majority vote or supermajority rules. This would create immediate existential risk for any P5 state that fell into conflict with a voting majority. Great powers would withdraw from the UN or defect to rival institutional frameworks rather than accept binding majoritarian decisions over war/peace. The international system would splinter into competing blocs. The veto's removal would force institutional reorganization at the system level.
% FOUNDING_PROBLEM: The founding problem was preventing great-power conflict from paralyzing the League of Nations (as happened 1920–1939) and ensuring major military powers could not be coerced into military action by institutional process. The UN Charter embedded the principle that no state with enforcement capacity can be bound by international decision without its consent, operationalized as the P5 veto.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear-armed states continue to assert that any institutional arrangement that could authorize military action against them without consent is existentially unacceptable; strategic doctrine treats unilateral security guarantees as non-negotiable. Independent strategic analysts (e.g., Waltz, Mearsheimer) argue that great-power balancing and the avoidance of forced institutional decisions remain central to international stability. The founding problem is attested by non-beneficiary parties (smaller states, regional powers) as well: they acknowledge the veto prevents great-power war even when they contest its distributional effects.
narrative_ontology:disappearance_verdict(article_27_veto_power__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_27_veto_power__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__coordination_reading, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__coordination_reading_tests).
:- end_tests(article_27_veto_power__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.15 end-state) because the coordination function solves a real problem without a concentrated beneficiary extracting rents. The veto prevents coercion, it does not extract material from losers. Suppression is minimal (0.08) because the constraint persists by structural logic, not by active enforcement of alternatives into silence—it is embedded in the Charter text and is treated as legitimate across the board. Theater is modest (0.12) because the veto is functionally active: it actually prevents votes that would authorize military action; it is not mere performance. Accessibility collapse is very high (0.92) because alternatives to the unanimity gate collapse completely once the nuclear era is understood—any smaller-state-majoritaran framework would create existential risk for nuclear powers, forcing them out of the system entirely. No actor has a realistic alternative to accepting the veto as the price of system participation. Resistance is very low (0.11) because even states that object to the P5 distribution recognize the veto's coordination function; they contest the distribution, not the mechanism's necessity. The measurement series is flat-to-slight-rise: extractiveness edges up modestly as P5 states use veto authority to block humanitarian or peacekeeping operations, suggesting some rent-extraction layered onto the core coordination function (theater_ratio confirms this slight rise). But the fundamentals remain stable—the core function has not atrophied.
 *
 * PERSPECTIVAL GAP:
 *   The main perspectival gap is between the P5's seat (the veto is a security necessity we cannot abandon) and the mid-sized-power seat (the veto is a distribution mechanism that freezes our subordination). Both parties agree the veto prevents great-power war; they disagree on whether the veto is ONLY that or WHETHER IT ALSO extracts distributional advantage. Under this reading, the veto is ONLY the coordination mechanism—the distributional frustration of smaller powers is a side effect, not an extraction victim class. The engine computes this from the stakeholder structure: if the smaller-powers seat declares it is harmed by the veto's existence, that would appear in the victims array and shift directionality upward. Instead, all seats declare benefit from system stability, which is the coordination-reading commitment.
 *
 * DIRECTIONALITY LOGIC:
 *   All named beneficiaries are at d near 0.0 (full beneficiary): nuclear powers benefit by holding the gate; non-permanent states benefit from system stability. There are no payers in the direct sense—no state bears material cost from the veto existing, only opportunity cost when a desired resolution is blocked. This is the signature of a genuine Rope: all parties prefer the veto-gated system to the alternatives. The engine's derivation from beneficiary presence (all stakeholders benefit), absence of victim classes, and the near-universal exit-cost of leaving the system (staying inside requires accepting the veto) should produce low d values across the board and confirm the Rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The veto's founding problem (prevent great-power paralysis and coercion) remains live and widely attested. Theater ratio is modest and stable, not rising sharply—there is no sign of the veto's function atrophying while the constraint persists by inertia. The slight rise in extractiveness over the interval reflects P5 use of veto authority to block humanitarian operations (a secondary effect layered onto coordination), not a decay of the core coordination into pure extraction. Mandatrophy is not present under this reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_oligopoly_boundary,
    'Does the veto persist because all states genuinely benefit from the coordination function (preventing great-power war), or because P5 states extract enough distributional advantage to maintain the system against smaller-power objections?',
    'Counterfactual analysis: if the veto''s distributional advantages were removed (e.g., equalization of voting power but retention of the unanimity gate), would P5 states still support it? If yes, the coordination is genuine; if no, the mechanism is primarily oligarchic.',
    'If coordination is genuine, the veto is Rope. If oligarchic, the veto is Tangled Rope or Snare depending on the degree of victim suppression. This is the structural divide between readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_oligopoly_boundary, conceptual, 'Whether the veto is maintained for coordination or distribution.').

omega_variable(
    nuclear_war_prevention_attribution,
    'What evidence establishes that the P5 veto specifically prevents great-power war, rather than other structural factors (nuclear deterrence, geographic distance, economic interdependence)?',
    'Historical analysis of near-miss conflict scenarios (1962 Cuban Missile Crisis, 1973 Yom Kippur War, 1983 Soviet alert): did the veto gate prevent institutional escalation, or did other deterrence mechanisms prevent conflict?',
    'Strong attribution supports the coordination reading; weak attribution raises the question whether the veto is solving the problem it claims to solve, or merely persists as a historical artifact of 1945 design.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nuclear_war_prevention_attribution, empirical, 'Causal attribution of war prevention to veto mechanism.').

omega_variable(
    alternative_unanimity_gates,
    'Could the coordination function (preventing institutional authorization of war against unwilling nuclear powers) be achieved via alternative institutional designs that do not concentrate veto power in the P5?',
    'Institutional design analysis: would a super-super-majority threshold (16 of 15 for military authorization), regional veto blocs, or nuclear-state-only councils solve the same problem without P5 monopoly?',
    'If alternatives exist that decouple coordination from oligarchic distribution, the veto shifts from defending an optimal solution to defending a suboptimal one that benefits the current P5. The reading''s claim to coordinate all states depends on no superior alternative existing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_unanimity_gates, conceptual, 'Whether the P5 veto is the only institutional solution to the coordination problem.').

omega_variable(
    kernel_reading_committer_alternative,
    'Is this constraint better understood as one reading of the contested kernel ''article_27_veto_power'', or as a different constraint from the oligopoly_reading and sovereignty_reading?',
    'The ε-invariance test: do the three readings measure the same standing arrangement (the veto exists in the Charter and blocks votes) and arrive at structurally different ε values from different frameworks? If yes, they are different constraints reading the same kernel. If they are measuring different arrangements or the same ε under different observables, the decomposition is wrong.',
    'Proper kernel framing enables cross-reading analysis of how the same structural fact gets read differently. Incorrect framing confuses perspective-taking with constraint decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_alternative, conceptual, 'Committer structure: whether this story is one reading of article_27_veto_power or a standalone constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__coordination_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_27_veto_power__coordination_reading, theater_ratio, 1945, 0.08).
narrative_ontology:measurement(arti_tr_t1962, article_27_veto_power__coordination_reading, theater_ratio, 1962, 0.09).
narrative_ontology:measurement(arti_tr_t1975, article_27_veto_power__coordination_reading, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(arti_tr_t1990, article_27_veto_power__coordination_reading, theater_ratio, 1990, 0.11).
narrative_ontology:measurement(arti_tr_t2005, article_27_veto_power__coordination_reading, theater_ratio, 2005, 0.13).
narrative_ontology:measurement(arti_tr_t2015, article_27_veto_power__coordination_reading, theater_ratio, 2015, 0.12).
narrative_ontology:measurement(arti_tr_t2026, article_27_veto_power__coordination_reading, theater_ratio, 2026, 0.12).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_27_veto_power__coordination_reading, base_extractiveness, 1945, 0.1).
narrative_ontology:measurement(arti_be_t1962, article_27_veto_power__coordination_reading, base_extractiveness, 1962, 0.12).
narrative_ontology:measurement(arti_be_t1975, article_27_veto_power__coordination_reading, base_extractiveness, 1975, 0.14).
narrative_ontology:measurement(arti_be_t1990, article_27_veto_power__coordination_reading, base_extractiveness, 1990, 0.13).
narrative_ontology:measurement(arti_be_t2005, article_27_veto_power__coordination_reading, base_extractiveness, 2005, 0.15).
narrative_ontology:measurement(arti_be_t2015, article_27_veto_power__coordination_reading, base_extractiveness, 2015, 0.16).
narrative_ontology:measurement(arti_be_t2026, article_27_veto_power__coordination_reading, base_extractiveness, 2026, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_27_veto_power__coordination_reading, suppression_requirement, 1945, 0.05).
narrative_ontology:measurement(arti_su_t1962, article_27_veto_power__coordination_reading, suppression_requirement, 1962, 0.06).
narrative_ontology:measurement(arti_su_t1975, article_27_veto_power__coordination_reading, suppression_requirement, 1975, 0.07).
narrative_ontology:measurement(arti_su_t1990, article_27_veto_power__coordination_reading, suppression_requirement, 1990, 0.08).
narrative_ontology:measurement(arti_su_t2005, article_27_veto_power__coordination_reading, suppression_requirement, 2005, 0.08).
narrative_ontology:measurement(arti_su_t2015, article_27_veto_power__coordination_reading, suppression_requirement, 2015, 0.09).
narrative_ontology:measurement(arti_su_t2026, article_27_veto_power__coordination_reading, suppression_requirement, 2026, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__coordination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_27_veto_power__coordination_reading, 0.12).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, article_27_veto_power__oligopoly_reading).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, article_27_veto_power__sovereignty_reading).

% DUAL FORMULATION NOTE:
% The article_27_veto_power kernel gives rise to three structurally distinct constraint stories, each instantiating a different reading of the same Charter text. The coordination_reading (this file) frames the veto as solving a genuine collective-action problem and classifies it as Rope with low extractiveness. The oligopoly_reading frames the same veto as structural entrenchment of geopolitical hierarchy and would classify it as Tangled Rope or Snare with high extractiveness and asymmetric victim structure. The sovereignty_reading frames the veto as instantiation of the Westphalian principle and would classify it differently depending on how sovereignty claims trade off against institutional authority. All three read the same kernel (Article 27 unanimity requirement) but instantiate different constraints (different ε values, different beneficiary/victim structures, different claimed types). The three files are linked bidirectionally via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
