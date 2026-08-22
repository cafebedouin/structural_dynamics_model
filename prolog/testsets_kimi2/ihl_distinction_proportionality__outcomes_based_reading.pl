% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__outcomes_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: ihl_distinction_proportionality__outcomes_based_reading
 *   human_readable: IHL Distinction/Proportionality Outcomes-Based Reading
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This constraint story captures the outcomes-based reading of the IHL
 *   distinction/proportionality kernel: the claim that autonomous weapon
 *   systems satisfy international humanitarian law if their measurable
 *   performance in distinction and proportionality equals or exceeds human
 *   operators. The reading treats IHL as technology-neutral, governing
 *   outcomes rather than means. It is one of three live readings of a
 *   contested kernel; the others (human-agency and categorical-prohibition)
 *   are instantiated as separate constraints. This reading creates a
 *   compliance pathway for autonomous systems by translating normative
 *   obligations into technical metrics. It benefits military operators
 *   seeking operational flexibility and defense contractors seeking a lawful
 *   market, while extracting interpretive authority from traditional IHL
 *   custodians and externalizing risk onto civilian populations who bear the
 *   cost of metric failures.
 *
 * KEY AGENTS:
 *   - military_operators (institutional/constrained): Agenda-setters who define technical compliance thresholds and operate LAWS; they gain expanded operational flexibility.
 *   - defense_contractors (powerful/mobile): Beneficiaries who develop and sell autonomous systems; gain lawful market access.
 *   - ihl_custodians (organized/constrained): Payers whose interpretive authority over normative judgment is displaced by technical metrics.
 *   - affected_civilians (powerless/trapped): Payers who bear direct risk if distinction/proportionality metrics fail in operational contexts.
 *   - human_rights_advocates (moderate/constrained): Excluded voices who argue for categorical human judgment but are kept outside the compliance-certification discourse.
 *   - independent_analysts (analytical/analytical): Observers who assess whether technical metrics correlate with civilian protection outcomes.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__outcomes_based_reading, 0.58).
domain_priors:suppression_score(ihl_distinction_proportionality__outcomes_based_reading, 0.45).
domain_priors:theater_ratio(ihl_distinction_proportionality__outcomes_based_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__outcomes_based_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__outcomes_based_reading, "IHL Distinction/Proportionality Outcomes-Based Reading").
narrative_ontology:topic_domain(ihl_distinction_proportionality__outcomes_based_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__outcomes_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__outcomes_based_reading, '4f4ec17a-01a5-4bf3-a9f2-8ffbb7909c8b').
narrative_ontology:cs_kernel_codification('4f4ec17a-01a5-4bf3-a9f2-8ffbb7909c8b', fixed_text).
narrative_ontology:cs_authority_grounding('4f4ec17a-01a5-4bf3-a9f2-8ffbb7909c8b', lineage).
narrative_ontology:cs_interpretation_layer_present('4f4ec17a-01a5-4bf3-a9f2-8ffbb7909c8b').
narrative_ontology:cs_reading_relation('4f4ec17a-01a5-4bf3-a9f2-8ffbb7909c8b', ihl_distinction_proportionality__categorical_prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('4f4ec17a-01a5-4bf3-a9f2-8ffbb7909c8b', ihl_distinction_proportionality__human_agency_reading, coexists_with).
narrative_ontology:cs_axiom('4f4ec17a-01a5-4bf3-a9f2-8ffbb7909c8b', foundational, technology_neutrality_doctrine).
narrative_ontology:cs_axiom_status(technology_neutrality_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('4f4ec17a-01a5-4bf3-a9f2-8ffbb7909c8b', technology_neutrality_doctrine, conventional).
narrative_ontology:cs_axiom('4f4ec17a-01a5-4bf3-a9f2-8ffbb7909c8b', foundational, outcome_equivalence_sufficiency).
narrative_ontology:cs_axiom_status(outcome_equivalence_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('4f4ec17a-01a5-4bf3-a9f2-8ffbb7909c8b', outcome_equivalence_sufficiency, instrumental).
narrative_ontology:cs_reference_frame('4f4ec17a-01a5-4bf3-a9f2-8ffbb7909c8b', technology_neutral_ihl_framework).
narrative_ontology:cs_drift_state('4f4ec17a-01a5-4bf3-a9f2-8ffbb7909c8b', autonomous_systems_proliferation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4f4ec17a-01a5-4bf3-a9f2-8ffbb7909c8b', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, military_operators).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, ihl_custodians).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, affected_civilians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the technical compliance thresholds for autonomous weapon systems and operates them under the claim that measurable distinction and proportionality performance satisfies IHL. Benefits from expanded operational flexibility and reduced legal friction in high-tempo engagements.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, military_operators, agenda_setter,
    institutional, generational, constrained, global).

% Develop and sell autonomous weapon systems. The outcomes-based reading creates a lawful market for their products if technical metrics are met, transferring research and development costs to military procurement and expanding the addressable market.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors, beneficiary,
    powerful, biographical, mobile, global).

% Traditionally custodians of IHL interpretive authority, including institutions like the ICRC and academic humanitarian law scholars. The outcomes-based reading erodes their role by substituting technical metrics for normative judgment, reducing their influence over what constitutes lawful conduct.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, ihl_custodians, payer,
    organized, civilizational, constrained, global).

% Bear the direct risk of distinction or proportionality metric failure in conflict zones. They have no voice in threshold-setting, no exit from the targeting calculus, and no recourse if technical compliance claims prove inaccurate in operational settings.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, affected_civilians, payer,
    powerless, immediate, trapped, local).

% Would argue that human dignity and the Martens Clause require categorical human judgment in lethal decisions. Structurally excluded from the compliance-certification discourse where technical performance is weighed against legal obligations.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, human_rights_advocates, excluded,
    moderate, generational, constrained, global).

% Assess whether technical metrics for distinction and proportionality actually correlate with civilian protection outcomes. Neither collect rents nor bear direct costs, but produce the evidentiary basis for evaluating the reading's empirical claims.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, independent_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal compliance pathway for military use of autonomous weapon systems by translating IHL distinction and proportionality obligations into measurable technical performance thresholds, allowing states to field LAWS without reforming treaty law.
% TRANSFER_FUNCTION: Moves interpretive authority over lawful killing from humanitarian law custodians and human moral agents to military operators and technical compliance auditors, and moves risk of legal error from combatants to civilian populations in target areas.
% ABSENT_VOICES: Human rights advocates and categorical prohibition proponents are structurally excluded from the compliance-certification discourse; their normative objections are treated as outside the scope of technology-neutral performance evaluation.
% DISAPPEARANCE_RATIONALE: If this reading vanished, militaries employing LAWS would lose their primary legal compliance pathway, defense contractors would face a contracted lawful market, and interpretive authority would revert toward humanitarian law custodians. The global regulatory discourse on autonomous weapons would shift toward either categorical prohibition or irreducible human agency requirements.
% FOUNDING_PROBLEM: Rapid advancement in autonomous systems created uncertainty about whether IHL governed their use and how to comply; states sought a framework permitting operational adoption without awaiting treaty revision.
% FOUNDING_PROBLEM_CORROBORATION: Military operators and defense contractors attest the problem is live. IHL custodians and human rights advocates attest the problem is manufactured to bypass normative constraints; independent UN Group of Governmental Experts reports and ICRC position papers from outside the beneficiary set corroborate the contestation.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__outcomes_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__outcomes_based_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__outcomes_based_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__outcomes_based_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__outcomes_based_reading, 0.58, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.58) is moderate because the reading genuinely coordinates legal clarity for emerging technology, but asymmetrically transfers authority and risk. Suppression (0.45) is moderate: the reading does not violently suppress alternatives, but it structurally marginalizes custodial interpretation by reframing compliance as a technical rather than normative exercise. Theater ratio (0.25) is low-to-moderate because most enforcement activity is substantive (testing, certification), though some performative compliance exists. Accessibility collapse (0.40) is moderate: alternatives (categorical prohibition, human agency) remain intellectually available but are increasingly sidelined in procurement and operational law. Resistance (0.55) is significant because humanitarian law institutions and advocacy networks actively contest the reading. Temporal measurements show gradual intensification as the reading becomes embedded in state military manuals and defense procurement.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (military operators) experiences this constraint as a coordination mechanism that solves legal uncertainty and enables lawful operations. The payer seats (IHL custodians, affected civilians) experience it as an extraction of interpretive authority and a transfer of lethal risk. The engine computes this divergence from the structural data: same constraint, opposite directionality. Defense contractors, as mobile beneficiaries, experience a subsidized market expansion.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to military operators and defense contractors: both receive structural subsidies from the reading (operational flexibility and market access, respectively). Victim declarations map to IHL custodians (authority extraction) and affected civilians (risk externalization). Military operators have constrained exit because they are bound by IHL treaty frameworks but can shape interpretation; their directionality is near the beneficiary pole. Affected civilians are trapped with local scope, placing them near the full-target pole. IHL custodians are constrained at global scope, giving them high directionality though not as extreme as trapped agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading prevents mislabeling by preserving its genuine coordination function: without any outcome-based standard, militaries might field LAWS in a legal vacuum, producing worse civilian outcomes. However, the coordination is tangled with extraction because the threshold-setting process is captured by operational actors and industry, while those who bear the failure risk are excluded. If the founding problem (legal uncertainty around LAWS) were solved by a genuinely neutral international tribunal rather than operator-controlled metrics, the extraction component would drop and the constraint might approach rope. As it stands, the active enforcement of technical compliance thresholds by benefiting parties keeps it tangled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    outcomes_reading_authority_grounding,
    'Is the outcomes-based reading a good-faith legal interpretation of technology-neutral IHL, or a post-hoc strategic frame constructed to legitimize autonomous weapons?',
    'Genealogical analysis of state submissions to the UN GGE on LAWS and domestic military legal manuals: does the reading pre-date LAWS development or emerge contemporaneously with procurement pressure?',
    'If post-hoc and state-driven, authority_grounding shifts from lineage to extraction, increasing extractiveness and tilting classification toward snare. If genuinely rooted in pre-existing legal doctrine, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(outcomes_reading_authority_grounding, conceptual, 'Whether the reading''s authority derives from genuine legal lineage or strategic construction.').

omega_variable(
    metric_civilian_protection_correlation,
    'Do validated technical metrics for distinction and proportionality in test or simulation environments predict actual civilian protection outcomes in operational conflict settings?',
    'Empirical battlefield-outcome studies and post-strike assessments comparing LAWS engagements to human-operated engagements under similar operational conditions.',
    'If metrics do not predict outcomes, the coordination function is illusory and the constraint extracts by creating false compliance assurance; classification would shift toward snare. If they do predict outcomes, the extraction is partly justified as necessary coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(metric_civilian_protection_correlation, empirical, 'Empirical validity of technical compliance metrics as proxies for civilian protection.').

omega_variable(
    custodian_displacement_mechanism,
    'Does the outcomes-based reading supplement IHL custodial interpretation with technical expertise, or structurally displace custodians from compliance determination?',
    'Institutional ethnography of compliance-determination sites: composition of review boards, whose determinations are treated as dispositive in operational law, and whether ICRC or analogous bodies retain veto-equivalent authority.',
    'If custodians are fully displaced, the reading extracts maximal authority; if supplemented, tangled-rope dynamics are attenuated. This determines whether the victimization of ihl_custodians is incidental or structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custodian_displacement_mechanism, empirical, 'Degree of structural displacement of humanitarian law custodians.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__outcomes_based_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t0, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ihl__tr_t5, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(ihl__tr_t10, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(ihl__tr_t15, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement(ihl__tr_t20, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 20, 0.25).

% Extraction over time
narrative_ontology:measurement(ihl__be_t0, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ihl__be_t5, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(ihl__be_t10, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(ihl__be_t15, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 15, 0.56).
narrative_ontology:measurement(ihl__be_t20, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t0, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ihl__su_t5, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(ihl__su_t10, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 10, 0.41).
narrative_ontology:measurement(ihl__su_t15, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 15, 0.43).
narrative_ontology:measurement(ihl__su_t20, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 20, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality__categorical_prohibition_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality__human_agency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the ihl_distinction_proportionality kernel. The kernel decomposes into three structurally distinct claims: outcomes-based (this file), human-agency, and categorical-prohibition. Each reading has a distinct epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
