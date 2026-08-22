% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__outcomes_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__outcomes_based_reading, []).

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
 *   constraint_id: ihl_distinction_proportionality__outcomes_based_reading
 *   human_readable: Outcomes-Based Compliance Standard for Autonomous Weapons Systems (IHL)
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This story represents the outcomes-based reading of the contested IHL
 *   kernel governing autonomous weapons systems: the position that
 *   distinction and proportionality obligations are technology-neutral,
 *   satisfied whenever an autonomous system's measured performance meets or
 *   exceeds a comparable human operator's, regardless of whether a human
 *   makes the final lethal-force decision. This is a genuinely
 *   coordination-shaped claim — it resolves a real legal gap around
 *   fast-moving technology without freezing the law to a particular
 *   architecture — but it also creates a technical-metrics chokepoint that
 *   the fielding militaries and their contractors control, displacing the
 *   interpretive authority IHL custodians have historically exercised and
 *   shifting failure risk onto civilian populations. Two sibling readings of
 *   the same kernel (human_agency_reading and
 *   categorical_prohibition_reading) are NOT part of this story; they are
 *   separate constraints with their own ε and stakeholder structures, linked
 *   via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - defense_contractors: organized/arbitrage — beneficiary; converts compliance into a marketable technical specification
 *   - military_operational_commands: institutional/mobile — beneficiary and agenda_setter; controls the certification testing regime
 *   - humanitarian_law_custodians: institutional/constrained — payer; loses interpretive authority to a technical benchmark it does not control
 *   - civilian_populations_in_conflict_zones: powerless/trapped — payer; bears the consequence if certified performance does not transfer to the field
 *   - national_defense_ministries: institutional/mobile — agenda_setter; sets and can revise the comparison baseline
 *   - independent_technical_auditors: moderate/constrained — excluded; denied access to the classified data needed to verify the metrics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__outcomes_based_reading, 0.52).
domain_priors:suppression_score(ihl_distinction_proportionality__outcomes_based_reading, 0.44).
domain_priors:theater_ratio(ihl_distinction_proportionality__outcomes_based_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__outcomes_based_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__outcomes_based_reading, "Outcomes-Based Compliance Standard for Autonomous Weapons Systems (IHL)").
narrative_ontology:topic_domain(ihl_distinction_proportionality__outcomes_based_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__outcomes_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__outcomes_based_reading, '440d6c58-11e8-4fd2-af1e-5e96774c0273').
narrative_ontology:cs_kernel_codification('440d6c58-11e8-4fd2-af1e-5e96774c0273', distributed).
narrative_ontology:cs_authority_grounding('440d6c58-11e8-4fd2-af1e-5e96774c0273', distributed).
narrative_ontology:cs_reading_relation('440d6c58-11e8-4fd2-af1e-5e96774c0273', ihl_distinction_proportionality__human_agency_reading, forecloses).
narrative_ontology:cs_reading_relation('440d6c58-11e8-4fd2-af1e-5e96774c0273', ihl_distinction_proportionality__categorical_prohibition_reading, coexists_with).
narrative_ontology:cs_axiom('440d6c58-11e8-4fd2-af1e-5e96774c0273', foundational, compliance_is_measurable_outcome_not_decision_process).
narrative_ontology:cs_axiom_status(compliance_is_measurable_outcome_not_decision_process, holdable).
narrative_ontology:cs_axiom_grounding('440d6c58-11e8-4fd2-af1e-5e96774c0273', compliance_is_measurable_outcome_not_decision_process, instrumental).
narrative_ontology:cs_axiom('440d6c58-11e8-4fd2-af1e-5e96774c0273', secondary, technology_neutrality_of_legal_obligation).
narrative_ontology:cs_axiom_status(technology_neutrality_of_legal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('440d6c58-11e8-4fd2-af1e-5e96774c0273', technology_neutrality_of_legal_obligation, conventional).
narrative_ontology:cs_reference_frame('440d6c58-11e8-4fd2-af1e-5e96774c0273', additional_protocol_i_human_operator_baseline).
narrative_ontology:cs_drift_state('440d6c58-11e8-4fd2-af1e-5e96774c0273', post_autonomous_weapons_proliferation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('440d6c58-11e8-4fd2-af1e-5e96774c0273', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, military_operational_commands).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, humanitarian_law_custodians).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations_in_conflict_zones).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and sell autonomous targeting systems. A metrics-satisfaction standard converts legal compliance into a procurable, certifiable technical specification they can engineer toward and market as a compliance product, rather than an open-ended moral or political judgment they cannot sell against. They shape what counts as an adequate benchmark through participation in standard-setting bodies and classified testing regimes.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors, beneficiary,
    organized, generational, arbitrage, global).

% Field autonomous systems and administer the compliance-testing regime that determines whether a given system's performance record clears the threshold. Gain operational tempo, reduced personnel risk, and a defensible legal basis for deployment decisions once a system clears certification. Control the classified evaluation data that would let outsiders check the benchmark's rigor.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, military_operational_commands, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__outcomes_based_reading, military_operational_commands, agenda_setter).

% International Committee of the Red Cross, UN special rapporteurs, and IHL scholarly bodies whose interpretive authority over what distinction and proportionality REQUIRE is displaced by a technical metrics threshold set largely by the systems' operators and manufacturers. They can issue commentary and advocate for treaty revision but cannot compel disclosure of the classified performance data the standard depends on, and cannot re-open a certified system's legal status once fielded.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, humanitarian_law_custodians, payer,
    institutional, civilizational, constrained, global).

% Bear the direct physical consequence if the performance metrics used to certify a system do not transfer from controlled test conditions to the actual complexity of the environment where it is deployed. Have no standing in the certification process, no access to the classified test data, and no post-hoc remedy if a system that cleared the threshold in testing performs worse in the field.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations_in_conflict_zones, payer,
    powerless, immediate, trapped, regional).

% Sponsor the legal doctrine internally, argue it at treaty negotiations (e.g. the CCW Group of Governmental Experts), and decide what counts as an acceptable comparison baseline for 'human operator performance.' Set the terms of the benchmark and can revise it unilaterally when convenient to their procurement timelines.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, national_defense_ministries, agenda_setter,
    institutional, generational, mobile, national).

% Academic and NGO technical experts who could in principle verify whether the claimed performance metrics are valid, reproducible, and transferable to combat conditions, but are systematically denied access to classified training data, test environments, and after-action performance records that would let them audit the certification claims.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, independent_technical_auditors, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__outcomes_based_reading, military_operational_commands).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__outcomes_based_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common, technology-neutral standard so that IHL compliance can be assessed by measurable outcome (did the system distinguish combatants from civilians and apply force proportionately at a rate comparable to trained human operators) rather than by a categorical rule that would have to specify, in advance, exactly which technical architectures are permissible — genuinely useful given how fast the underlying technology changes.
% TRANSFER_FUNCTION: Moves interpretive authority over what counts as lawful lethal force from the accumulated case-by-case judgment of humanitarian law bodies and international courts to the technical benchmark-setting process controlled by the fielding militaries and their contractors; moves risk from operator-side decision liability onto civilian populations in the gap between tested performance and fielded performance.
% ABSENT_VOICES: Independent technical auditors and affected civilian communities have no seat in setting the compliance thresholds or auditing the classified test data the certification rests on; humanitarian law custodians can comment publicly but cannot compel disclosure or re-open a certification once granted.
% DISAPPEARANCE_RATIONALE: If the outcomes-based standard were abandoned overnight, autonomous weapons deployment would either revert to requiring demonstrable human agency at the point of lethal force (halting or slowing current procurement and deployment programs) or face categorical prohibition — either way, fielding decisions, procurement contracts, and doctrine built around performance-threshold certification would need to be unwound or renegotiated.
% FOUNDING_PROBLEM: Existing IHL treaty text (Additional Protocol I's distinction and proportionality rules) was drafted for human decision-makers and does not specify how, or whether, an autonomous system can satisfy the same legal obligations — creating genuine uncertainty for militaries seeking a lawful basis to field increasingly autonomous targeting technology.
% FOUNDING_PROBLEM_CORROBORATION: Military legal advisors and defense-affiliated policy institutes attest the metrics-threshold approach resolves a genuine legal gap. The ICRC, several UN human rights special rapporteurs, and independent legal scholars outside the fielding states dispute that a technical performance threshold can discharge the qualitative judgment IHL's proportionality test requires, arguing the standard exists to authorize deployment rather than to genuinely test compliance.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__outcomes_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__outcomes_based_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__outcomes_based_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__outcomes_based_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__outcomes_based_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__outcomes_based_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__outcomes_based_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ihl_distinction_proportionality__outcomes_based_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (ε=0.52) is authored as moderate, consistent with the expected structural delta: this reading is coordination-shaped where the performance metrics are rigorous and transferable, but extractive where the certifying body also controls disclosure of the data that would let outsiders verify the claim. Suppression (0.44) is moderate — there is no outright coercive barrier to critique, but classification of test data functions as a structural suppression of independent audit. Theater ratio (0.40) and its rising trajectory reflect a growing gap between the rhetoric of rigorous, technology-neutral standard-setting and the reality that benchmark design, baseline selection, and pass/fail thresholds are set largely by the parties who benefit from a favorable outcome. Accessibility collapse is moderate (0.40): the categorical and human-agency readings remain fully articulable and are actively argued in treaty forums (CCW GGE), so alternatives have not collapsed — this is a live, contested standard, not a settled one. Resistance is correspondingly substantial (0.60): ICRC, multiple states, and civil society coalitions actively campaign against the outcomes-based standard.
 *
 * DIRECTIONALITY LOGIC:
 *   Defense contractors and military commands are declared beneficiaries: the standard converts an open moral question into an engineerable target and a fieldable legal basis, at low direct cost to them. Humanitarian law custodians and civilian populations are declared victims: the former lose the interpretive authority they have exercised since Additional Protocol I; the latter bear the physical risk if the metrics used in certification fail to transfer from test to field conditions. National defense ministries sit as agenda_setters distinct from the beneficiary operational commands because they negotiate the doctrine internationally even when their own forces are not yet fielding systems, giving them a policy-shaping role independent of direct capture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — treaty text drafted for human decision-makers not specifying how autonomous systems can satisfy the same obligations — is genuinely contested rather than resolved or dead: it corroborates as 'live' from the perspective of militaries seeking a lawful deployment basis, while IHL scholars outside the beneficiary set argue the technical-threshold approach exists to authorize the outcome rather than genuinely discharge IHL's qualitative judgment. Classifying this as tangled_rope (not snare) reflects that the coordination function is real — the law needs SOME workable standard for autonomous systems — while requiring active enforcement (classification regimes, certification gatekeeping) and identifiable victims prevents the classification from softening into rope, which would mislabel the displaced interpretive authority and civilian risk as costless coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metric_transferability_gap,
    'Do the technical performance metrics used to certify an autonomous system''s distinction/proportionality performance in controlled testing actually predict its performance in the variable, adversarial conditions of real conflict zones?',
    'Independent post-deployment audit comparing certified test performance against documented field outcomes (civilian casualty incident review, after-action reports), conducted by parties without a stake in the certifying military''s procurement decisions.',
    'If field performance systematically falls short of certified test performance, the outcomes-based standard is validating a metric that does not track the legal obligation it claims to satisfy, which would support reclassification toward snare (extraction masquerading as compliance) rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_transferability_gap, empirical, 'Whether certified test performance transfers to field conditions.').

omega_variable(
    who_sets_the_baseline,
    'Is the ''human operator performance'' baseline used for comparison a rigorous, independently validated figure, or a benchmark selected/adjusted by the fielding institution to be clearable by its own systems?',
    'Comparative review of baseline-setting methodology across multiple national certification regimes; disclosure of baseline derivation to an independent standards body.',
    'A self-selected or movable baseline would indicate the entire ''equal or exceeding human performance'' framing is a rhetorical shell around a threshold set for convenience, sharpening the case for reclassification as extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(who_sets_the_baseline, empirical, 'Whether the comparison baseline is independently validated or self-set.').

omega_variable(
    kernel_framing_choice,
    'Is the choice to treat IHL compliance as a technology-neutral outcomes question (rather than a human-agency question or a categorical-dignity question) itself a neutral legal-interpretive choice, or a framing selected because it is the only one that permits autonomous weapons deployment?',
    'Genealogical analysis of which institutional actors first proposed and championed the outcomes-based framing in treaty forums (CCW GGE submissions), and whether the framing tracks prior legal doctrine or emerged concurrently with procurement pressure.',
    'If the framing emerged primarily from states/contractors with deployment interests rather than from independent legal scholarship, this reading''s claim to be the ''technology-neutral'' default reading is itself contestable — it would be one advocacy position among three, not a neutral baseline the others deviate from.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether the outcomes-based framing is a neutral default or an interested selection among three live readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__outcomes_based_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t0, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ihl__tr_t4, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(ihl__tr_t8, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(ihl__tr_t12, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(ihl__tr_t16, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(ihl__tr_t20, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(ihl__be_t0, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ihl__be_t4, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(ihl__be_t8, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(ihl__be_t12, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 12, 0.47).
narrative_ontology:measurement(ihl__be_t16, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(ihl__be_t20, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t0, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ihl__su_t4, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 4, 0.34).
narrative_ontology:measurement(ihl__su_t8, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(ihl__su_t12, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 12, 0.4).
narrative_ontology:measurement(ihl__su_t16, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 16, 0.42).
narrative_ontology:measurement(ihl__su_t20, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 20, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__outcomes_based_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ihl_distinction_proportionality__outcomes_based_reading, 0.1).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, categorical_prohibition_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the ihl_distinction_proportionality kernel. human_agency_reading holds that IHL requires irreducible human moral judgment at the point of lethal force, regardless of measured system performance. categorical_prohibition_reading holds that Martens Clause principles of humanity and public conscience categorically prohibit autonomous lethal decision-making, independent of any performance threshold. This outcomes_based_reading treats compliance as technology-neutral and satisfiable by measured performance parity. The three readings differ in beneficiary/victim structure and in ε — they are not the same constraint measured three ways; each carries its own stable ε per the ε-invariance principle. This reading has the LOWEST ε of the three (moderate, 0.52) because it retains a genuine coordination function (a workable standard for fast-moving technology) alongside its extractive features, whereas the excluded categorical reading would treat any performance-based accommodation as itself illegitimate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
