% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__composite_reading, []).

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
 *   constraint_id: honor_settlement_legitimacy__composite_reading
 *   human_readable: Honor-Duel as Legitimate Method of Settling Affairs of Honor (Composite Overdetermination Reading)
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   This story instantiates the composite (overdetermination) reading of the
 *   honor-settlement-legitimacy kernel: the legitimacy of dueling as a method
 *   of settling affairs of honor did not decline through any single dominant
 *   mechanism but through several independently sufficient pressures
 *   converging — cultural unthinkability (the contraction reading's
 *   mechanism), intensifying legal prosecution, the rise of alternative
 *   status markets (professional credentialing, bourgeois wealth display),
 *   and organized moral reform. The composite reading's structural claim is
 *   that contraction was the leading edge but not the sole cause: even absent
 *   full cultural unthinkability, legal and economic pressures were
 *   independently tightening around the same period, meaning the practice's
 *   collapse would likely have occurred (on a slower or different timeline)
 *   through the other mechanisms alone. This is distinct from the
 *   contraction_reading (which treats cultural framework transformation as
 *   doing essentially all the causal work) and the drop_reading (which denies
 *   full disappearance and treats persistence among a residual honor-culture
 *   fringe as the more accurate description of the historical record).
 *
 * KEY AGENTS:
 *   - aristocratic_honor_class_incumbents: structural beneficiary of the honor economy the code sustains
 *   - second_and_witness_networks: administer the procedural apparatus, agenda-setters for the code's operation
 *   - dueling_participants_and_families: bear the direct extraction — death, injury, ruin
 *   - state_legal_authorities: one of several independent contracting forces
 *   - clergy_and_moral_reform_movements: another independent contracting force
 *   - social_historians: analytical observers whose comparative method surfaces the overdetermination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__composite_reading, 0.42).
domain_priors:suppression_score(honor_settlement_legitimacy__composite_reading, 0.55).
domain_priors:theater_ratio(honor_settlement_legitimacy__composite_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__composite_reading, piton).
narrative_ontology:human_readable(honor_settlement_legitimacy__composite_reading, "Honor-Duel as Legitimate Method of Settling Affairs of Honor (Composite Overdetermination Reading)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__composite_reading, "historical_sociology/legal_history/cultural_anthropology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__composite_reading, '60a60a6d-01a2-4c07-86b9-b02f4979c643').
narrative_ontology:cs_kernel_codification('60a60a6d-01a2-4c07-86b9-b02f4979c643', distributed).
narrative_ontology:cs_authority_grounding('60a60a6d-01a2-4c07-86b9-b02f4979c643', practice).
narrative_ontology:cs_interpretation_layer_present('60a60a6d-01a2-4c07-86b9-b02f4979c643').
narrative_ontology:cs_reading_relation('60a60a6d-01a2-4c07-86b9-b02f4979c643', honor_settlement_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('60a60a6d-01a2-4c07-86b9-b02f4979c643', honor_settlement_legitimacy__drop_reading, influences).
narrative_ontology:cs_axiom('60a60a6d-01a2-4c07-86b9-b02f4979c643', foundational, overdetermined_convergence_causal_structure).
narrative_ontology:cs_axiom_status(overdetermined_convergence_causal_structure, holdable).
narrative_ontology:cs_axiom_grounding('60a60a6d-01a2-4c07-86b9-b02f4979c643', overdetermined_convergence_causal_structure, empirically_contingent).
narrative_ontology:cs_axiom('60a60a6d-01a2-4c07-86b9-b02f4979c643', secondary, contraction_as_leading_but_nonexclusive_edge).
narrative_ontology:cs_axiom_status(contraction_as_leading_but_nonexclusive_edge, holdable).
narrative_ontology:cs_axiom_grounding('60a60a6d-01a2-4c07-86b9-b02f4979c643', contraction_as_leading_but_nonexclusive_edge, empirically_contingent).
narrative_ontology:cs_reference_frame('60a60a6d-01a2-4c07-86b9-b02f4979c643', code_duello_procedural_honor_settlement).
narrative_ontology:cs_drift_state('60a60a6d-01a2-4c07-86b9-b02f4979c643', late_nineteenth_century_legal_consolidation, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('60a60a6d-01a2-4c07-86b9-b02f4979c643', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, aristocratic_honor_class_incumbents).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, second_and_witness_networks).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, dueling_participants_and_families).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, lower_status_men_excluded_from_the_code).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__composite_reading, code_duello_procedural_fairness_doctrine).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__composite_reading, honor_as_property_requiring_defense_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold status positions whose value depends on an honor economy in which reputational challenge can be settled through a ritualized, class-restricted procedure. As industrial capitalism, professional credentialing, and mass legal institutions expand, this class's monopoly on legitimate violence-as-arbitration erodes from several directions at once — legal prosecution risk rises, bourgeois status competition shifts to markets and credentials, and the practice itself becomes socially embarrassing among peers rather than merely risky.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, aristocratic_honor_class_incumbents, beneficiary,
    powerful, generational, identity_locked, national).

% Administer the code duello's procedural apparatus — negotiating terms, certifying that honor has been satisfied, policing who may issue and accept challenges. Their function depends on continued social acceptance of the code as a legitimate dispute-resolution mechanism; as courts, press ridicule, church condemnation, and shifting masculine ideals converge, the seconds' role loses standing simultaneously from legal, cultural, and institutional directions rather than any single cause.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, second_and_witness_networks, agenda_setter,
    organized, biographical, constrained, regional).

% Bear the direct costs — death, injury, widowhood, orphaned children, financial ruin from wounds or legal exposure — of a practice whose social compulsion (refusal reads as cowardice) persists even as the surrounding legitimating structures (legal tolerance, press approval, elite peer expectation) are each independently weakening. A participant in the later period faces the same personal stakes as one from a century earlier, but with fewer reinforcing supports left standing.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, dueling_participants_and_families, payer,
    moderate, biographical, constrained, local).

% Are barred from dueling as a recognized honor mechanism because the code is class-restricted; their disputes are settled by other means (informal violence, litigation, submission) regardless of dueling's rise or fall. They have no voice in whether the practice persists, though its decline removes one visible marker of the honor class's exclusive privilege.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, lower_status_men_excluded_from_the_code, excluded,
    powerless, biographical, trapped, local).

% Increasingly criminalize and prosecute dueling as the modern state consolidates its monopoly on legitimate violence, professionalizes policing, and expands criminal codes. This is one of several independent contracting forces — legal prosecution intensifies at the same time cultural unthinkability and economic/status changes are also operating, none of which alone would fully explain the timing or completeness of the decline.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, state_legal_authorities, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__composite_reading, state_legal_authorities, observer).

% Campaign against dueling as sin and social pathology, publish condemnations, and organize anti-dueling societies. Their moral pressure operates alongside legal prosecution and shifting masculine ideals rather than as the sole cause; the composite reading treats their contribution as one reinforcing strand among several.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, clergy_and_moral_reform_movements, agenda_setter,
    organized, generational, mobile, national).

% Study the decline of dueling across multiple national contexts and time periods, comparing timing and sequencing of legal, economic, and cultural changes to assess causal weight. Their comparative method is what surfaces the overdetermination pattern — the decline's timing across regions does not track any single mechanism cleanly.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, social_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_settlement_legitimacy__composite_reading, aristocratic_honor_class_incumbents).
narrative_ontology:fixing_cost_class(honor_settlement_legitimacy__composite_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The code duello coordinated elite dispute resolution by providing a ritualized, rule-bound alternative to open-ended violence or unresolved reputational damage — it substituted procedure (seconds, terms, witnessed satisfaction) for chaotic escalation within a defined class.
% TRANSFER_FUNCTION: Moves physical risk, and sometimes life, from the abstract domain of reputation into concrete bodily stakes borne by the two principals (and their families), while the surrounding class collects the reputational-order benefit of a functioning honor economy without bearing the individual risk.
% ABSENT_VOICES: Lower-status men excluded from the code, and women affected by widowhood or family ruin from duels, had no standing to contest either the practice or its decline; their exclusion from the honor economy meant they experienced the practice's costs (through kinship and community ties) without ever holding a seat in the code's procedural apparatus.
% DISAPPEARANCE_RATIONALE: The honor-settlement legitimacy this constraint names did in fact disappear historically — and its disappearance visibly rearranged elite dispute-resolution practice: litigation, press-mediated reputation management, and formal codes of professional conduct replaced ritual combat as the mechanism for settling affairs of honor, confirming that real institutional weight rode on the constraint rather than nothing depending on it.
% FOUNDING_PROBLEM: The code duello was built to solve the problem of reputational injury among social equals in the absence of a trusted external arbiter: courts of the era were often unavailable, slow, class-biased against gentlemen appealing to them over 'points of honor,' or simply beneath the dignity of the aggrieved party to invoke.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the dueling class (drawing on court records, criminal prosecution statistics, and comparative studies of dispute-resolution institutions) attest that formal courts, professional codes of conduct, and press-mediated reputation management had become fully functional and broadly trusted substitutes well before dueling's final disappearance — the founding problem was independently solved by institutions with no stake in preserving the code, which is precisely why multiple unrelated forces (legal, religious, economic, cultural) could each independently erode the practice.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__composite_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__composite_reading_tests).
:- end_tests(honor_settlement_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.42) and essentially flat across the interval because the composite reading holds that the underlying extraction the code performs (risking participants' lives to sustain elite reputational order) did not itself intensify — what changed was the number and strength of independent forces eroding the code's legitimacy, not the extraction rate while it operated. Suppression rises steadily (0.25 to 0.55) because the composite reading tracks increasing legal, religious, and social sanction against the practice as a converging, not singular, phenomenon — the suppression_requirement series models the accumulating weight of multiple simultaneously-hardening enforcement channels (courts, church, press ridicule, changing masculine ideals) rather than one channel intensifying alone. Theater ratio rises modestly (0.08 to 0.30) reflecting the code's increasing performative residue — duels fought more for show/satisfaction of form than functional dispute resolution — as the underlying honor economy it served began hollowing out from multiple directions at once.
 *
 * PERSPECTIVAL GAP:
 *   From the aristocratic incumbent's seat, the code appears as a genuine, if declining, coordination mechanism whose erosion is regrettable cultural loss. From the participant/family seat, the same structure looks like extraction that persisted past its social utility because incumbents kept extending it for status reasons even as external alternatives (courts, credentialing) became functionally available. The composite reading holds both readings are partially right: the code's decline had a real coordination-function collapse (contraction) layered with continuing extraction from participants who remained bound by compulsory-participation norms enforced by no single institution but several weakening ones simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Aristocratic incumbents and the second/witness networks sit near the beneficiary end (d low) — they administer and derive status-order benefit from the code without bearing its physical risk directly. Dueling participants sit near the target end (d high) — they bear death/injury risk and their exit is constrained by compulsory-honor social norms that persisted even as legitimating structures eroded. Lower-status excluded men are declared but sit outside the code's benefit/cost flow entirely, which is why their power is powerless/trapped rather than payer in the strict extraction sense — they pay the cost of the honor economy's existence (a visible marker of exclusive class privilege) without ever entering its risk/reward calculus.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (no trusted external arbiter for elite reputational injury) is authored dead — courts, professional codes, and press-mediated reputation eventually filled the function fully and were attested by legal historians outside the dueling class. Because the composite reading holds multiple independent forces were simultaneously solving or supplanting this founding function, no single institution can be credited (or blamed) for the code's obsolescence — this is precisely what distinguishes the composite reading from a mandatrophy narrative pinned to one actor's deliberate reform. The classification as piton (not snare) follows from this: the code's late-stage persistence is inertial performance riding several independently weakening supports, not concentrated capture by any beneficiary who could unilaterally fix or perpetuate it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    single_mechanism_vs_convergence,
    'Was contraction (cultural unthinkability) actually doing most of the causal work with legal/economic factors merely correlated and epiphenomenal, or were the mechanisms genuinely independently sufficient such that removing any one still leaves the others to produce decline?',
    'Comparative historical analysis across jurisdictions where legal prosecution intensified without corresponding cultural-framework shift (or vice versa) — if decline tracks tightly with only one mechanism across cases, the composite reading''s overdetermination claim weakens in favor of the contraction reading.',
    'If a single mechanism dominates, this composite_reading collapses into the contraction_reading (or an alternative single-mechanism reading) and the ''multiple independently sufficient causes'' claim becomes unsupported — ε and the coordination/extraction balance would need re-authoring under that reading rather than this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(single_mechanism_vs_convergence, empirical, 'Whether decline mechanisms were genuinely independently sufficient or one dominant cause with correlated epiphenomena.').

omega_variable(
    sibling_disagreement_locus,
    'Where exactly does the composite_reading''s account diverge from contraction_reading and drop_reading, given all three describe the same historical record?',
    'This is not resolvable by further data alone — it is a framing choice about causal weighting (composite vs. contraction) and about disappearance completeness (composite/contraction''s world_rearranges verdict vs. drop''s continued-persistence claim). Documented here per Rule 2 rather than folded into this constraint''s own classification.',
    'The disagreement locus is: (1) causal architecture — composite claims convergent independent sufficiency, contraction claims cultural transformation is doing essentially all the work; (2) disappearance completeness — composite/contraction treat the code as having genuinely ended (disappearance_verdict: world_rearranges), drop treats it as persisting in diminished, fringe form (which would change the disappearance_verdict and likely the extractiveness trajectory, since a persisting fringe practice implies ongoing, not concluded, extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_disagreement_locus, conceptual, 'Structural location of disagreement among the three kernel readings.').

omega_variable(
    false_summit_of_honor_natural_order,
    'Did the aristocratic honor class present dueling''s legitimacy as flowing from a natural, quasi-inevitable social order (honor as intrinsic property requiring defense) when in fact it was a constructed, class-restricted arrangement that benefited incumbents by excluding lower-status men from an equivalent claim to formal reputational defense?',
    'Textual analysis of contemporary honor-code apologetics versus comparative anthropological evidence that honor-violence norms vary widely across societies and are not a fixed feature of male status competition — cross-cultural variation would undermine any naturalization claim.',
    'If honor''s naturalization was a rhetorical cover for a constructed, class-exclusive arrangement, this strengthens the tangled_rope/piton reading over any framing that would treat the code as an emergent, non-constructed social fact; it is documented here because this constraint declares beneficiaries (aristocratic_honor_class_incumbents) that could otherwise mistakenly appear alongside a naturalized framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_of_honor_natural_order, conceptual, 'Whether honor''s apparent naturalness masked a constructed, class-exclusive arrangement — noted for completeness though this story is not itself claimed as mountain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__composite_reading, 1780, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1780, honor_settlement_legitimacy__composite_reading, theater_ratio, 1780, 0.08).
narrative_ontology:measurement(hono_tr_t1800, honor_settlement_legitimacy__composite_reading, theater_ratio, 1800, 0.12).
narrative_ontology:measurement(hono_tr_t1820, honor_settlement_legitimacy__composite_reading, theater_ratio, 1820, 0.18).
narrative_ontology:measurement(hono_tr_t1840, honor_settlement_legitimacy__composite_reading, theater_ratio, 1840, 0.22).
narrative_ontology:measurement(hono_tr_t1860, honor_settlement_legitimacy__composite_reading, theater_ratio, 1860, 0.27).
narrative_ontology:measurement(hono_tr_t1880, honor_settlement_legitimacy__composite_reading, theater_ratio, 1880, 0.29).
narrative_ontology:measurement(hono_tr_t1900, honor_settlement_legitimacy__composite_reading, theater_ratio, 1900, 0.3).

% Extraction over time
narrative_ontology:measurement(hono_be_t1780, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1780, 0.3).
narrative_ontology:measurement(hono_be_t1800, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1800, 0.34).
narrative_ontology:measurement(hono_be_t1820, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1820, 0.38).
narrative_ontology:measurement(hono_be_t1840, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1840, 0.4).
narrative_ontology:measurement(hono_be_t1860, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1860, 0.41).
narrative_ontology:measurement(hono_be_t1880, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1880, 0.42).
narrative_ontology:measurement(hono_be_t1900, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1900, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1780, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1780, 0.25).
narrative_ontology:measurement(hono_su_t1800, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1800, 0.32).
narrative_ontology:measurement(hono_su_t1820, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1820, 0.4).
narrative_ontology:measurement(hono_su_t1840, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1840, 0.47).
narrative_ontology:measurement(hono_su_t1860, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1860, 0.5).
narrative_ontology:measurement(hono_su_t1880, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1880, 0.53).
narrative_ontology:measurement(hono_su_t1900, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1900, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__composite_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_settlement_legitimacy__composite_reading, 0.08).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy__drop_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the honor_settlement_legitimacy kernel. composite_reading (this file) claims overdetermined convergence of multiple independently sufficient mechanisms with contraction as the leading edge. contraction_reading claims cultural framework transformation alone did essentially all the causal work, rendering legal/economic co-occurrence epiphenomenal. drop_reading denies the shared disappearance premise, holding that dueling persisted as a fringe practice among residual honor-culture adherents rather than fully ending. All three share the same underlying historical record and stakeholder cast but diverge on causal architecture and disappearance completeness — per DP-001 ε-invariance, each is authored as its own constraint with its own ε and classification rather than as one story with a hidden causal-weighting parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
