% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__sovereignty_reading, []).

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
    narrative_ontology:measurement_basis/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: article_27_veto_power__sovereignty_reading
 *   human_readable: P5 Veto as Westphalian Sovereignty Principle
 *   domain: international_relations/constitutional_law
 *
 * SUMMARY:
 *   The UN Security Council's P5 veto is enshrined in Article 27(3) of the UN
 *   Charter: any permanent member can block a substantive resolution,
 *   regardless of other members' votes. This reading instantiates the veto as
 *   a structural expression of Westphalian sovereignty applied to great
 *   powers with global-reach enforcement capacity. The core claim: no
 *   international institution can bind a sovereign state without the state's
 *   consent, especially when that state commands the military capacity to
 *   resist enforcement. The veto is not presented here as a choice to
 *   entrench privilege or as a mechanism to prevent war (those are sibling
 *   readings). It is presented as an inevitable structural boundary: any
 *   system attempting to govern states without coercive authority over them
 *   must either respect their vetoes or become toothless. The veto is the
 *   acknowledgment that the Security Council chose the former path.
 *
 * KEY AGENTS:
 *   - permanent_five_great_powers: possess enforcement capacity (nuclear weapons, global military reach) that exceeds the UN's coercive capacity; the structural basis of the veto
 *   - un_security_council: formally empowered to make binding determinations, but only as long as it respects the structural limit the veto expresses
 *   - non_permanent_members_and_general_assembly: excluded from veto power; this reading does not frame their exclusion as a beneficiary structure for the P5, but as evidence that veto power correlates with enforcement capacity, not institutional privilege
 *   - future_reformers: would remove or weaken the veto; from this reading's perspective, they face an unsolvable problem—any alternative institutional form attempting to compel action on a nuclear-armed state would face the same coordination failure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__sovereignty_reading, 0.05).
domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, 0.0).
domain_priors:theater_ratio(article_27_veto_power__sovereignty_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__sovereignty_reading, mountain).
narrative_ontology:human_readable(article_27_veto_power__sovereignty_reading, "P5 Veto as Westphalian Sovereignty Principle").
narrative_ontology:topic_domain(article_27_veto_power__sovereignty_reading, "international_relations/constitutional_law").

domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__sovereignty_reading, 'c5b308ce-cff4-4621-8f12-29aade45778e').
narrative_ontology:cs_kernel_codification('c5b308ce-cff4-4621-8f12-29aade45778e', fixed_text).
narrative_ontology:cs_authority_grounding('c5b308ce-cff4-4621-8f12-29aade45778e', lineage).
narrative_ontology:cs_interpretation_layer_present('c5b308ce-cff4-4621-8f12-29aade45778e').
narrative_ontology:cs_reading_relation('c5b308ce-cff4-4621-8f12-29aade45778e', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('c5b308ce-cff4-4621-8f12-29aade45778e', article_27_veto_power__oligopoly_reading, coexists_with).
narrative_ontology:cs_axiom('c5b308ce-cff4-4621-8f12-29aade45778e', foundational, no_state_bound_without_consent).
narrative_ontology:cs_axiom_status(no_state_bound_without_consent, holdable).
narrative_ontology:cs_axiom_grounding('c5b308ce-cff4-4621-8f12-29aade45778e', no_state_bound_without_consent, deontological).
narrative_ontology:cs_axiom('c5b308ce-cff4-4621-8f12-29aade45778e', foundational, veto_structural_not_extractive).
narrative_ontology:cs_axiom_status(veto_structural_not_extractive, holdable).
narrative_ontology:cs_axiom_grounding('c5b308ce-cff4-4621-8f12-29aade45778e', veto_structural_not_extractive, conventional).
narrative_ontology:cs_reference_frame('c5b308ce-cff4-4621-8f12-29aade45778e', westphalian_sovereignty_principle).
narrative_ontology:cs_drift_state('c5b308ce-cff4-4621-8f12-29aade45778e', contemporary_enforcement_capacity_asymmetry, gap(stable, minor, false)).
narrative_ontology:cs_created_at('c5b308ce-cff4-4621-8f12-29aade45778e', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__sovereignty_reading, article_27_veto_power).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold veto power under Article 27. The constraint as stated here is not about extracting rents or strategic advantage—it is a structural consequence of Westphalian sovereignty: no entity can be coerced by an international institution into actions its domestic authority rejects, especially when that entity commands enforcement capacity (nuclear weapons, global military reach) to resist.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, permanent_five_great_powers, observer,
    institutional, civilizational, analytical, global).

% Formally empowered to make binding determinations on international peace and security, subject to Article 27 veto by any permanent member. The veto is not a flaw in the Council's design—it is the structural acknowledgment that the Council cannot enforce its will against a state that can and will resist.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, un_security_council, agenda_setter,
    institutional, generational, analytical, global).

% Lack veto power. From this seat, the veto appears to entrench great-power privilege. But this reading does not treat the veto as serving the interests of the P5 over the GA—it treats the veto as a structural inevitability of any system attempting to bind sovereign states without coercive authority over them.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, non_permanent_members_and_general_assembly, excluded,
    organized, biographical, constrained, global).

% Would redesign the institution to weaken or eliminate the veto, either by eliminating the P5 privilege or by creating enforcement mechanisms that make the veto moot. From this seat, the veto is a target for reform. This reading asserts that reform cannot escape the underlying constraint: any global institution that attempts to compel action on a nuclear-armed state faces the same structural problem the veto expresses.
narrative_ontology:constraint_stakeholder(article_27_veto_power__sovereignty_reading, future_institutional_reformers, excluded,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading does not frame the veto as solving a coordination problem. Instead, it describes the veto as the structural expression of an unsolvable coordination failure: any institution claiming authority over sovereign states must either respect their rejection of its commands (in which case it has no real authority over them) or attempt coercion (in which case it must overcome their resistance capacity). The veto is the Charter's acknowledgment that the Council chose the former path.
% TRANSFER_FUNCTION: No transfer. This reading does not attribute the veto to extraction or strategic advantage-taking. The veto moves nothing from one party to another; it is a permission structure, not a revenue mechanism.
% ABSENT_VOICES: States without military enforcement capacity and without veto power would argue that the veto entrenches great-power privilege at the expense of international law's reach. Superpowers seeking broader enforcement authority would argue the veto constrains their ability to address threats. Neither voice is present in the design of the constraint itself; both are excluded from the frame in which this reading casts the veto as an inevitability rather than a choice.
% DISAPPEARANCE_RATIONALE: This reading holds that if the veto disappeared, the world would not reorganize—instead, the underlying structural problem would reappear in a different form. A Security Council without Article 27 would either become toothless (because P5 compliance would evaporate) or would trigger major-power exit from the institution. The constraint is not a particular choice about how to organize the Council; it is the boundary condition any Council faces when attempting to govern states with enforcement capacity exceeding its own.
% FOUNDING_PROBLEM: The founding problem was not the need for a veto—it was the need for an institution that could operate with great-power participation at all. The veto was the price the Charter paid to include the USSR and future nuclear powers as members rather than adversaries outside the system.
% FOUNDING_PROBLEM_CORROBORATION: Historical analysis of the 1945 Charter negotiations (Kagan, Ikenberry, and UN archives) documents that the Soviet Union would not have joined without veto protection. The founding problem remains live: any attempt to diminish the veto faces the same adhesion problem—major powers would abandon an institution that claims authority it cannot enforce against them. This is attested by independent scholarship in international relations, not by the P5 benefiting from the veto.
narrative_ontology:disappearance_verdict(article_27_veto_power__sovereignty_reading, contested).
narrative_ontology:founding_problem_status(article_27_veto_power__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_27_veto_power__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__sovereignty_reading, 0.05, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__sovereignty_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, ExtMetricName, E),
    domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(article_27_veto_power__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading authors extractiveness near-zero (0.05) because the veto is framed as a structural constraint, not as an extraction mechanism. The veto moves nothing from one party to another; it simply sets the boundary at which the Council's authority ends. Suppression is zero because no coercive machinery maintains the veto—it is self-evident from the distribution of enforcement capacity. Theater ratio is zero because the veto has no performative component in this reading; it is what it is. Accessibility of alternatives is near-total (0.95): the structural problem the veto expresses cannot be escaped by changing the voting rule, adding more members, or reforming the Council—any institution claiming authority over nuclear powers faces the same coordination failure. Resistance is near-zero (0.08) because the veto is not defended by the P5 through active suppression; it is defended by the fact that any attempt to override it triggers major-power exit from the institution itself. The measurement series shows stability across the interval: the constraint has persisted from 1945 to 2026 without meaningful change in extractiveness, suppression, or theater, because the underlying structural condition (asymmetric enforcement capacity) has remained stable.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces no perspectival gap because it does not attribute different interests to different seats. From the P5 perspective, the veto is a protection of sovereignty. From the perspective of non-permanent members, the veto is a constraint on collective action. But this reading does not translate those different narratives into different classifications: it asserts that the veto is structurally the same for both, a boundary condition of any institution attempting to govern without coercive authority. The engine will not compute per-seat type divergence here because the structural data does not support beneficiary/victim differentiation—the veto is not extracted from anyone; it is a permission structure.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality derives in this reading because there are no beneficiaries or victims. The veto does not benefit the P5 (in the sense of extracting rents) nor does it harm non-permanent members (it simply excludes them from veto power). The P5 are described in stakeholders[] with role=observer because the veto is structurally necessary given their enforcement capacity, not because they are the constraint's constituency. The General Assembly and non-permanent members are role=excluded because they have no voice in the constraint's design, but their exclusion is not framed as extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading precludes mandatrophy analysis because the veto has no mandate to become obsolete. The founding problem (ensuring great-power participation without requiring institutional coercion over them) remains live under this reading. The veto's persistence is not explained by institutional inertia or theatrical maintenance (theater_ratio=0.0), but by the fact that the underlying structural condition (enforcement capacity asymmetry) has not changed. If and when that condition changes (secondary powers acquire enforcement capacity rivaling the P5), the structural case for veto protection would generalize beyond the P5, but the veto itself would not become mandatrophic—it would become universally applied rather than obsolete.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_structural_power_choice,
    'Is the veto a law of nature (any system attempting to bind sovereign states with enforcement capacity faces this coordination problem), or a contingent choice the Charter made that could be remade differently?',
    'Test via institutional counterfactuals: a hypothetical UN without the veto, operating under qualified majority rule, would face immediate defection by nuclear powers or would lose all enforcement authority. The observed outcomes of the real veto (power vetoes specific resolutions) versus the counterfactual (power exits the institution) would show whether the veto is structural inevitability or chosen design.',
    'If the veto is structural inevitability, it is a mountain: no alternative institutional form escapes it. If it is a chosen design that constrains but does not determine outcomes, it downgrades to tangled_rope or snare (the P5 extract privilege from the veto''s existence). The reading is contingent on this omega''s resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_structural_power_choice, conceptual, 'Whether the veto expresses an unescapable coordination problem or a contingent institutional choice').

omega_variable(
    enforcement_capacity_asymmetry_duration,
    'As the proliferation of enforcement capacity continues (smaller powers acquiring drones, cyber weapons, precision strike, autonomous systems), does the structural reason for the veto—that some states possess enforcement capacity exceeding the institution''s coercive reach—persist or decay?',
    'Historical trajectory of enforcement-capacity dispersion: if secondary powers acquire sufficient military capacity to resist UN coercion, the structural case for veto protection generalizes. If enforcement capacity remains concentrated in the P5 indefinitely, the case remains specific to them but does not fade.',
    'A widening enforcement-capacity distribution would suggest the veto is a specific expression of a more general principle (any institution must yield to states it cannot coerce), which strengthens the mountain framing. A stable concentration would suggest the veto is best understood as the P5 locking in privilege against future institutional change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_capacity_asymmetry_duration, empirical, 'Whether the structural condition justifying the veto (asymmetric enforcement capacity) persists or erodes').

omega_variable(
    kernel_reading_distinction_ambiguity,
    'Is this reading (sovereignty as structural inevitability) logically distinct from the coordination_reading (veto as preventing great-power war)? Do they rest on different core premises, or are they the same claim with different emphasis?',
    'Semantic analysis of the founding premises: sovereignty_reading grounds the veto in the principle ''no state can be bound without consent''; coordination_reading grounds it in ''the veto prevents war by reassuring nuclear powers.'' If consent protection implies war prevention only given nuclear weapons, and war prevention implies consent protection only given asymmetric enforcement capacity, the readings may be the same structural claim with different narrative frames.',
    'If readings are genuinely distinct (sovereignty is about principle; coordination is about outcomes), then they coexist as logically independent accounts. If they are the same structure narrated differently, the apparent ''kernel contest'' is actually a surface-level terminology choice, not a genuine disagreement about what the veto is.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distinction_ambiguity, conceptual, 'Whether this reading and the coordination_reading rest on different structural premises or are narrations of the same constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__sovereignty_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_27_veto_power__sovereignty_reading, theater_ratio, 1945, 0.0).
narrative_ontology:measurement_basis(arti_tr_t1945, observed).
narrative_ontology:measurement(arti_tr_t1962, article_27_veto_power__sovereignty_reading, theater_ratio, 1962, 0.0).
narrative_ontology:measurement_basis(arti_tr_t1962, observed).
narrative_ontology:measurement(arti_tr_t1989, article_27_veto_power__sovereignty_reading, theater_ratio, 1989, 0.0).
narrative_ontology:measurement_basis(arti_tr_t1989, observed).
narrative_ontology:measurement(arti_tr_t2026, article_27_veto_power__sovereignty_reading, theater_ratio, 2026, 0.0).
narrative_ontology:measurement_basis(arti_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_27_veto_power__sovereignty_reading, base_extractiveness, 1945, 0.03).
narrative_ontology:measurement_basis(arti_be_t1945, observed).
narrative_ontology:measurement(arti_be_t1962, article_27_veto_power__sovereignty_reading, base_extractiveness, 1962, 0.04).
narrative_ontology:measurement_basis(arti_be_t1962, observed).
narrative_ontology:measurement(arti_be_t1989, article_27_veto_power__sovereignty_reading, base_extractiveness, 1989, 0.05).
narrative_ontology:measurement_basis(arti_be_t1989, observed).
narrative_ontology:measurement(arti_be_t2026, article_27_veto_power__sovereignty_reading, base_extractiveness, 2026, 0.05).
narrative_ontology:measurement_basis(arti_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_27_veto_power__sovereignty_reading, suppression_requirement, 1945, 0.0).
narrative_ontology:measurement_basis(arti_su_t1945, observed).
narrative_ontology:measurement(arti_su_t1962, article_27_veto_power__sovereignty_reading, suppression_requirement, 1962, 0.0).
narrative_ontology:measurement_basis(arti_su_t1962, observed).
narrative_ontology:measurement(arti_su_t1989, article_27_veto_power__sovereignty_reading, suppression_requirement, 1989, 0.0).
narrative_ontology:measurement_basis(arti_su_t1989, observed).
narrative_ontology:measurement(arti_su_t2026, article_27_veto_power__sovereignty_reading, suppression_requirement, 2026, 0.0).
narrative_ontology:measurement_basis(arti_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__coordination_reading).
narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__oligopoly_reading).

% DUAL FORMULATION NOTE:
% Article 27 of the UN Charter is a single textual kernel that admits three structurally distinct readings. The sovereignty_reading presented here frames the veto as a structural inevitability given enforcement capacity asymmetry; the coordination_reading frames it as a mechanism preventing major-power war; the oligopoly_reading frames it as entrenchment of geopolitical privilege. Each reading has its own constraint story with its own epsilon (near-zero for sovereignty, moderate for coordination, high for oligopoly), its own beneficiary/victim structure (none for sovereignty, preventive benefit for coordination, concentrated benefit for oligopoly), and its own computed type (mountain for sovereignty, rope or tangled_rope for coordination, snare for oligopoly). The three stories are linked via network.affects_constraints to enable constraint-family analysis and cross-reading comparison.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
