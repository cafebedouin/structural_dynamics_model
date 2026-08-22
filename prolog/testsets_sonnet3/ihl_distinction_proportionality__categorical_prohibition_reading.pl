% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__categorical_prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__categorical_prohibition_reading, []).

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
 *   constraint_id: ihl_distinction_proportionality__categorical_prohibition_reading
 *   human_readable: Martens Clause Categorical Prohibition of Autonomous Weapons
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the categorical prohibition reading of the
 *   ihl_distinction_proportionality kernel: the claim that Martens Clause
 *   'principles of humanity and public conscience' prohibit autonomous
 *   weapons systems (LAWS) as a class, independent of any performance
 *   benchmark. Two sibling readings of the SAME kernel text exist as separate
 *   constraints: the human_agency_reading (which locates the violation
 *   specifically in the absence of human judgment at the moment of force, a
 *   narrower and more defensible claim) and the outcomes_based_reading (which
 *   holds the opposite — that law tracks demonstrated
 *   distinction/proportionality outcomes and is technology-neutral). This
 *   story does not adjudicate among them; it authors only the categorical
 *   reading's own structure, ε, and stakeholder set. The categorical reading
 *   is the highest-ε member of the family because it forecloses an entire
 *   technology class ex ante, regardless of what any future system could
 *   demonstrate.
 *
 * KEY AGENTS:
 *   - anti_militarist_civil_society
 *   - states_lacking_laws_capability
 *   - states_with_advanced_autonomous_systems
 *   - defense_technology_sector
 *   - military_commanders_seeking_precision_alternatives
 *   - civilians_in_conflict_zones
 *   - icrc_and_legal_scholars
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, 0.68).
domain_priors:suppression_score(ihl_distinction_proportionality__categorical_prohibition_reading, 0.71).
domain_priors:theater_ratio(ihl_distinction_proportionality__categorical_prohibition_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__categorical_prohibition_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__categorical_prohibition_reading, "Martens Clause Categorical Prohibition of Autonomous Weapons").
narrative_ontology:topic_domain(ihl_distinction_proportionality__categorical_prohibition_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__categorical_prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__categorical_prohibition_reading, '4465830f-a231-4d09-ad30-23bd7cc065f4').
narrative_ontology:cs_kernel_codification('4465830f-a231-4d09-ad30-23bd7cc065f4', distributed).
narrative_ontology:cs_authority_grounding('4465830f-a231-4d09-ad30-23bd7cc065f4', distributed).
narrative_ontology:cs_reading_relation('4465830f-a231-4d09-ad30-23bd7cc065f4', ihl_distinction_proportionality__human_agency_reading, influences).
narrative_ontology:cs_reading_relation('4465830f-a231-4d09-ad30-23bd7cc065f4', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_axiom('4465830f-a231-4d09-ad30-23bd7cc065f4', foundational, machine_decided_killing_violates_dignity_per_se).
narrative_ontology:cs_axiom_status(machine_decided_killing_violates_dignity_per_se, holdable).
narrative_ontology:cs_axiom_grounding('4465830f-a231-4d09-ad30-23bd7cc065f4', machine_decided_killing_violates_dignity_per_se, deontological).
narrative_ontology:cs_axiom('4465830f-a231-4d09-ad30-23bd7cc065f4', foundational, technical_performance_is_categorically_irrelevant_to_permissibility).
narrative_ontology:cs_axiom_status(technical_performance_is_categorically_irrelevant_to_permissibility, holdable).
narrative_ontology:cs_axiom_grounding('4465830f-a231-4d09-ad30-23bd7cc065f4', technical_performance_is_categorically_irrelevant_to_permissibility, deontological).
narrative_ontology:cs_reference_frame('4465830f-a231-4d09-ad30-23bd7cc065f4', martens_clause_1899_hague_preamble).
narrative_ontology:cs_drift_state('4465830f-a231-4d09-ad30-23bd7cc065f4', contemporary_ccw_gge_negotiations, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('4465830f-a231-4d09-ad30-23bd7cc065f4', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, states_lacking_laws_capability).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, arms_control_ngos).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, states_with_advanced_autonomous_systems).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, defense_technology_sector).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, military_commanders_seeking_precision_alternatives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Coalitions like the Campaign to Stop Killer Robots author and promote the categorical reading, treating machine-decided killing as a dignity violation regardless of performance data. They gain moral standing, funding, and diplomatic access by pressing the bright-line rule; the argument's power depends on rejecting any accuracy-based rebuttal in advance.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society, beneficiary,
    organized, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society, agenda_setter).

% States without the industrial base to build autonomous weapons back a categorical ban because it freezes the current capability gap in their favor without requiring them to compete technologically. They benefit from a rule that would cost them nothing to comply with and everything to a rival state that has already invested.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, states_lacking_laws_capability, beneficiary,
    institutional, generational, constrained, global).

% Have invested heavily in autonomous targeting systems and argue the categorical rule is not a dignity claim but a disguised attempt to deny them a battlefield advantage on which they can point to no distinction/proportionality failure. Cannot exit the treaty conversation without reputational cost, but also cannot comply without abandoning fielded capability.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, states_with_advanced_autonomous_systems, payer,
    institutional, generational, constrained, global).

% Contractors developing targeting and weapons-release automation face a market foreclosed by categorical prohibition regardless of any performance benchmark they could hit. Sunk R&D and pending contracts are stranded if the rule is codified; their only recourse is lobbying against the categorical framing toward the outcomes-based reading.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, defense_technology_sector, payer,
    powerful, biographical, constrained, global).

% Field commanders who believe automated systems could reduce civilian casualties compared to fatigued or panicked human decision-making are barred from using them by the categorical rule regardless of demonstrated accuracy. They bear the operational and moral cost of continuing to rely on human decision-makers in circumstances where the categorical rule forecloses the alternative by definition.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, military_commanders_seeking_precision_alternatives, payer,
    moderate, biographical, trapped, national).

% Bear the actual consequences of whichever targeting regime operates over them, but are not party to the treaty negotiations that decide between categorical prohibition and outcomes-based testing. Their empirical stake in whether autonomous or human targeting is safer is treated as irrelevant to the categorical reading's per se dignity claim.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, civilians_in_conflict_zones, excluded,
    powerless, immediate, trapped, local).

% Interpret Martens Clause jurisprudence and assess whether 'principles of humanity and public conscience' support a categorical bar or merely inform case-by-case proportionality review. Their scholarship is cited by all three readings but does not itself resolve which reading controls state practice.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, icrc_and_legal_scholars, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__categorical_prohibition_reading, diffuse).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__categorical_prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a normative floor below which no state may operate: a bright-line rule removes the need for case-by-case verification of each autonomous system's performance, which would otherwise require contested technical adjudication in the middle of or after hostilities.
% TRANSFER_FUNCTION: Moves strategic and market advantage away from states and firms that have invested in autonomous targeting systems toward states and civil-society coalitions that have not, by converting a capability gap into a legal prohibition that applies independently of demonstrated performance.
% ABSENT_VOICES: Civilians in conflict zones whose casualty outcomes are the ostensible subject of the debate are not parties to the treaty negotiations; their empirical stake in comparative human-vs-machine targeting accuracy is treated as inadmissible to a categorical per se claim. Field commanders who might testify to operational tradeoffs are similarly outside the room where the rule is drafted.
% DISAPPEARANCE_RATIONALE: If the categorical prohibition norm dissolved overnight, states already fielding autonomous systems would face no legal ceiling and accelerate deployment, materially rearranging battlefield practice; anti-militarist coalitions dispute this framing entirely, holding that the norm is a dignity floor whose removal would be a moral rather than merely operational event. Both descriptions are structurally coherent from their respective seats, which is why the verdict is contested rather than settled either way.
% FOUNDING_PROBLEM: Built to close a perceived gap in IHL: existing distinction and proportionality rules are stated in terms of outcomes and are silent on WHO or WHAT makes the targeting decision, leaving open the possibility that machines could satisfy the letter of the law while violating an unstated requirement of human moral agency or dignity.
% FOUNDING_PROBLEM_CORROBORATION: Civil society coalitions and a subset of legal scholars attest the problem (unregulated delegation of lethal decisions) is live and unaddressed by existing treaty text. States with advanced autonomous systems and much of the defense technology sector attest, from outside the beneficiary coalition, that the problem as framed is not empirically demonstrated — no comparative performance data establishes that autonomous targeting under existing distinction/proportionality standards produces worse outcomes than human targeting, which they argue makes the categorical framing a policy preference rather than a corroborated gap in the law.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__categorical_prohibition_reading, contested).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__categorical_prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__categorical_prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__categorical_prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ihl_distinction_proportionality__categorical_prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.68 because the categorical reading transfers real strategic and commercial value away from states/firms with fielded capability toward states/coalitions without it, and does so by legal fiat rather than by any comparative safety finding. Suppression (0.71) reflects that the rule's persistence depends on actively foreclosing the outcomes-based counterargument — a state cannot rebut the categorical claim by producing better safety data, because the claim is explicitly performance-independent ('regardless of technical performance'). This is what makes the categorical reading structurally suppressive in a way the human_agency and outcomes_based readings are not: those readings leave open an evidentiary or procedural path; this one closes it by design. Resistance is high (0.78) because the states and firms bearing the cost have both the incentive and technical sophistication to contest the framing at every treaty negotiation.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (anti-militarist civil society, capability-lacking states, arms-control NGOs) get low d: the rule costs them nothing and freezes a status quo that favors them. Victims (advanced-autonomy states, defense contractors, commanders wanting the alternative) get high d: the rule directly forecloses value they have built or could build. Civilians in conflict zones are excluded rather than positioned on the beneficiary/victim axis at all — their outcomes are the rule's stated subject matter but not its author, which is precisely the absent-voices structure this reading exhibits.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unregulated machine delegation of lethal decisions) is genuinely contested rather than resolved: proponents hold it live, opponents hold it uncorroborated by any actual performance gap. Because founding_problem_status is 'contested' rather than 'dead', this does not present as a mandatrophy case in the R5 sense — the constraint is not obviously an atrophied function coasting on inertia; it is an active, actively defended position within a live treaty fight. Coding it as tangled_rope rather than snare reflects that it does contain a genuine coordination function (a clear, verification-free bright line lowers the cost of monitoring compliance) alongside the asymmetric extraction — which is exactly the tangled_rope signature: both a real coordination story and an identifiable payer group, held together by active enforcement (treaty advocacy, export controls, diplomatic pressure).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_reading_is_one_of_three_kernel_readings,
    'Is the categorical prohibition reading the correct interpretation of Martens Clause ''principles of humanity and public conscience,'' or do the sibling readings (human_agency_reading: violation located in loss of human judgment; outcomes_based_reading: law tracks demonstrated performance) better capture the clause''s legal content?',
    'State practice and opinio juris accumulation at CCW GGE sessions; ICJ or arbitral tribunal ruling directly interpreting Martens Clause language against an actual fielded LAWS case; scholarly consensus formation over successive treaty cycles.',
    'If the human_agency_reading or outcomes_based_reading prevails in state practice, this categorical constraint''s claimed_type and its extraction profile become moot as a matter of binding law — it would persist only as an advocacy position, not a legal norm, which would sharply lower its effective χ regardless of its authored ε here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(categorical_reading_is_one_of_three_kernel_readings, conceptual, 'Which of the three sibling readings of the Martens Clause kernel controls actual state practice and treaty law.').

omega_variable(
    dignity_claim_vs_capability_freeze,
    'Is the categorical prohibition genuinely motivated by an irreducible dignity violation, or is the dignity framing a normatively persuasive cover for freezing a capability gap in favor of states and coalitions that lack autonomous weapons technology?',
    'Compare advocacy positions across states: if states with advanced autonomous systems that also have strong human-rights records still oppose the categorical ban, that is evidence the ban tracks capability interest rather than dignity principle uniformly; if opposition tracks capability alone regardless of human-rights posture, that supports the capability-freeze reading.',
    'If the capability-freeze account dominates, the constraint''s coordination function narrows and its extraction profile shifts toward pure snare (dignity as pretext); if the dignity account dominates, more of the measured extraction should be read as the legitimate cost of a genuine moral coordination function, pulling the classification back toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_claim_vs_capability_freeze, empirical, 'Whether the categorical ban''s motivating claim is dignity-per-se or disguised capability-gap protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__categorical_prohibition_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t0, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ihl__tr_t4, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 4, 0.13).
narrative_ontology:measurement(ihl__tr_t8, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(ihl__tr_t12, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(ihl__tr_t16, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(ihl__tr_t20, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 20, 0.22).

% Extraction over time
narrative_ontology:measurement(ihl__be_t0, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ihl__be_t4, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(ihl__be_t8, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 8, 0.57).
narrative_ontology:measurement(ihl__be_t12, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(ihl__be_t16, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(ihl__be_t20, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t0, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(ihl__su_t4, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 4, 0.53).
narrative_ontology:measurement(ihl__su_t8, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(ihl__su_t12, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(ihl__su_t16, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(ihl__su_t20, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 20, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__categorical_prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__outcomes_based_reading).

% DUAL FORMULATION NOTE:
% Three constraints decompose the natural-language 'Martens Clause and autonomous weapons' debate per the ε-invariance principle: this story (categorical_prohibition_reading, highest ε — bans the technology class outright regardless of performance), ihl_distinction_proportionality__human_agency_reading (narrower — violation located specifically in absent human judgment at the moment of force, a procedural rather than categorical claim), and ihl_distinction_proportionality__outcomes_based_reading (lowest ε for the prohibition function — law is technology-neutral and tracks demonstrated distinction/proportionality performance). Each reading is authored as its own constraint with its own ε, beneficiaries, and victims; none averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
