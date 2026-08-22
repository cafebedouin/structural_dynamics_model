% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__human_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__human_agency_reading, []).

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
 *   constraint_id: ihl_distinction_proportionality__human_agency_reading
 *   human_readable: IHL Human Agency Requirement for Lethal Force
 *   domain: international_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the human_agency_reading of the
 *   ihl_distinction_proportionality kernel. It holds that IHL's distinction
 *   and proportionality obligations require irreducible human moral judgment
 *   at the moment of lethal force application, and that the Martens Clause
 *   principles of humanity prohibit delegating life/death decisions to
 *   machines. The constraint is structurally a Tangled Rope: it coordinates
 *   around genuine civilian protection and moral accountability while
 *   asymmetrically extracting operational freedom from military actors and
 *   concentrating interpretive authority in IHL institutions (especially the
 *   ICRC). Sibling readings include the categorical_prohibition_reading (all
 *   LAWS banned regardless of performance) and the outcomes_based_reading
 *   (technology-neutral, outcomes suffice). This reading occupies the
 *   mediating position: it does not ban all autonomy but categorically
 *   requires a human final decision.
 *
 * KEY AGENTS:
 *   - ihl_interpretive_authorities (ICRC): Primary beneficiary/agenda_setter â institutional power, analytical exit, global scope, collects centrality from the constraint.
 *   - military_operators: Primary payer â powerful but constrained exit, global scope, bears operational cost of human-in-the-loop requirements.
 *   - defense_innovation_sector: Secondary payer â powerful but constrained exit, global scope, loses market for fully autonomous lethal systems.
 *   - civilian_populations_in_conflict: Beneficiary â powerless, trapped, local scope, intended protective recipient of the coordination function.
 *   - autonomous_systems_advocates: Excluded voice â organized, constrained, global scope, argues for performance-based alternatives but is kept out of IHL interpretive frameworks.
 *   - critical_legal_theorists: Analytical observer â analytical, analytical exit, global scope, evaluates whether the constraint is functional or theatrical.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, 0.79).
domain_priors:suppression_score(ihl_distinction_proportionality__human_agency_reading, 0.86).
domain_priors:theater_ratio(ihl_distinction_proportionality__human_agency_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__human_agency_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__human_agency_reading, "IHL Human Agency Requirement for Lethal Force").
narrative_ontology:topic_domain(ihl_distinction_proportionality__human_agency_reading, "international_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__human_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__human_agency_reading, '8745ed8c-9d04-4b1f-b121-d307589ea88e').
narrative_ontology:cs_kernel_codification('8745ed8c-9d04-4b1f-b121-d307589ea88e', fixed_text).
narrative_ontology:cs_authority_grounding('8745ed8c-9d04-4b1f-b121-d307589ea88e', lineage).
narrative_ontology:cs_interpretation_layer_present('8745ed8c-9d04-4b1f-b121-d307589ea88e').
narrative_ontology:cs_reading_relation('8745ed8c-9d04-4b1f-b121-d307589ea88e', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_reading_relation('8745ed8c-9d04-4b1f-b121-d307589ea88e', ihl_distinction_proportionality__categorical_prohibition_reading, coexists_with).
narrative_ontology:cs_axiom('8745ed8c-9d04-4b1f-b121-d307589ea88e', foundational, human_judgment_irreducible).
narrative_ontology:cs_axiom_status(human_judgment_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('8745ed8c-9d04-4b1f-b121-d307589ea88e', human_judgment_irreducible, deontological).
narrative_ontology:cs_axiom('8745ed8c-9d04-4b1f-b121-d307589ea88e', foundational, martens_clause_prohibits_machine_delegation).
narrative_ontology:cs_axiom_status(martens_clause_prohibits_machine_delegation, holdable).
narrative_ontology:cs_axiom_grounding('8745ed8c-9d04-4b1f-b121-d307589ea88e', martens_clause_prohibits_machine_delegation, conventional).
narrative_ontology:cs_reference_frame('8745ed8c-9d04-4b1f-b121-d307589ea88e', human_centred_targeting_tradition).
narrative_ontology:cs_drift_state('8745ed8c-9d04-4b1f-b121-d307589ea88e', autonomous_weapons_technological_maturity, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('8745ed8c-9d04-4b1f-b121-d307589ea88e', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, civilian_populations_in_conflict).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, military_operators).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, defense_innovation_sector).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__human_agency_reading, martens_clause_humanity_principle).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__human_agency_reading, distinction_proportionality_as_human_duty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Derive institutional centrality and authority from being the definitive interpreters of IHL's human-judgment requirements. Their mandate and relevance depend on maintaining that legal-moral boundary against automation. They produce interpretive guidance, treaty commentary, and diplomatic advocacy that reinforces the irreducibility of human decision-making in targeting.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities, beneficiary,
    institutional, civilizational, analytical, global).

% Receive the protective effect of distinction and proportionality obligations that are supposed to be executed through genuine human moral deliberation rather than algorithmic targeting. They do not control the constraint but are its intended beneficiaries under IHL.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, civilian_populations_in_conflict, beneficiary,
    powerless, immediate, trapped, local).

% Bear the operational cost of maintaining human-in-the-loop or human-on-the-loop systems rather than fully autonomous lethal platforms. Their targeting cycles are slowed, manpower is diverted to legal-monitoring roles, and operational tempo is constrained by the requirement for human moral judgment at the point of fire.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, military_operators, payer,
    powerful, biographical, constrained, global).

% Are restricted from developing and deploying fully autonomous lethal systems by the legal and normative requirement for human final decision. Their research trajectories and procurement markets are channeled away from unsupervised autonomy and toward human-machine interfaces that satisfy interpretive authorities.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, defense_innovation_sector, payer,
    powerful, biographical, constrained, global).

% Argue that algorithmic targeting could exceed human precision and reduce civilian harm, but are structurally excluded from the IHL interpretive framework that treats human judgment as categorically required regardless of comparative performance. Their voice is present in technology policy discourse but absent from IHL treaty interpretation.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, autonomous_systems_advocates, excluded,
    organized, generational, constrained, global).

% Analyze whether the human-judgment requirement functions as genuine legal-moral architecture or as institutional theater that preserves interpretive authority while states pursue autonomy through classification games.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, critical_legal_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__human_agency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates armed conflict around a shared legal-moral floor: ensuring that lethal targeting decisions incorporate contextual moral judgment and accountability, thereby protecting civilians from indiscriminate algorithmic killing.
% TRANSFER_FUNCTION: Transfers authority, operational tempo, and technological development pathways from military operators and defense innovators to human operators and the interpretive institutions that validate their judgment.
% ABSENT_VOICES: Autonomous weapons developers, military strategists advocating unsupervised LAWS, and technologists arguing machine precision exceeds human performance are excluded from the IHL interpretive consensus that treats human judgment as axiomatically necessary.
% DISAPPEARANCE_RATIONALE: If the requirement for irreducible human moral judgment vanished, fully autonomous lethal systems would become lawful, military procurement and doctrine would reorganize around unsupervised algorithmic targeting, and the IHL interpretive architecture centered on human agency would lose its primary boundary function.
% FOUNDING_PROBLEM: How to prevent industrial and technological warfare from severing the moral link between the killer and the killed; how to maintain legal accountability and distinction in an era of remote, automated, and potentially autonomous killing.
% FOUNDING_PROBLEM_CORROBORATION: IHL historians and ICRC archives corroborate the founding problem as protecting civilians from indiscriminate warfare. Military historians, some technologists, and outcomes-based legal scholars contest that the problem is best solved by human judgment rather than accountable algorithmic systems; no outside corroboration exists for the claim that human judgment remains the uniquely viable solution.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__human_agency_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__human_agency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__human_agency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__human_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__human_agency_reading, 0.79, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__human_agency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ihl_distinction_proportionality__human_agency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.79) because the constraint imposes a categorical, non-compensable burden on military operational efficiency and technological development. Suppression is higher (0.86) because the constraint's persistence requires actively suppressing fully autonomous alternatives through treaty interpretation, national policy, and arms-control diplomacy. Theater ratio is moderate-low (0.28): the human-judgment requirement is largely functional, but some enforcement activity is performative (states claiming human oversight while pushing autonomy boundaries). Accessibility collapse is moderate (0.58): technological alternatives to human judgment exist and are improving, but are legally and normatively closed off. Resistance is substantial (0.62): major military powers and defense industries actively resist the constraint through CCW paralysis, definitional games, and continued LAWS investment. The measurement series share a single time grid to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   From the IHL interpretive seat, the constraint is indispensable coordination that preserves the moral and legal integrity of armed conflict. From the military operator and defense innovation seats, the same structure is asymmetric extraction that privileges legal abstraction over battlefield reality and institutional centrality over operational necessity. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   IHL interpretive authorities are structural beneficiaries: the constraint validates their centrality and mandates their interpretive role (low d). Civilian populations receive protective coordination benefits (low d). Military operators and defense innovators are structural targets: they bear the costs of foregone autonomy and diverted operational capacity (high d). The divergence is driven by power (institutional vs powerful) and exit options (analytical vs constrained).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents misreading the constraint as pure extraction (snare) by acknowledging the genuine coordination function: civilian protection and moral accountability are real goods the constraint produces. It prevents misreading it as pure coordination (rope) by acknowledging the asymmetric extraction: the constraint empowers interpretive authorities and suppresses military alternatives without reciprocal benefit. If the civilian-protection function atrophied while the interpretive authority persisted, the constraint would degrade toward piton; if the suppression of LAWS intensified without protective payoff, it would sharpen toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_performance_paradox,
    'If an autonomous system demonstrably achieves superior distinction and proportionality outcomes to human operators, does the human-judgment requirement become pure extraction, or does it retain independent moral legitimacy?',
    'Comparative empirical studies of civilian harm rates in human-supervised versus algorithmic targeting, combined with normative analysis of whether outcome superiority dissolves the deontological prohibition on machine delegation.',
    'If outcome superiority dissolves the constraint''s legitimacy, the coordination function was outcome-dependent all along and the reading collapses toward the outcomes_based_reading. If legitimacy persists, the extraction asymmetry is the price of a deontological commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_performance_paradox, conceptual, 'Whether the constraint''s legitimacy is conditional on comparative performance or unconditional.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of fully autonomous systems structural (treaty and national policy barriers) or internalized (normative belief in human moral uniqueness among commanders and lawyers)?',
    'Post-normalization trajectory analysis: if military operators rapidly adopt autonomous aids when legal barriers fall, suppression was structural; if reluctance persists, suppression was partially internalized.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure because actors carry the norm with them. If purely structural, removal of legal barriers would quickly erode the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of autonomous alternatives.').

omega_variable(
    kernel_reading_boundary,
    'Does the human agency reading logically foreclose the outcomes-based reading, or do they coexist as alternative policy positions within the same legal framework?',
    'Jurisprudential analysis of whether IHL as currently codified permits technology-neutral outcome-based compliance, or whether the Martens Clause structurally requires human judgment as a constitutive element of lawful attack.',
    'If foreclosed, the readings are incommensurable and the corpus must treat them as separate constraints with no shared framework. If coexisting, they are competing interpretations of a single kernel and contamination may propagate between them.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Logical relationship between human agency and outcomes-based readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__human_agency_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl_human_agency_tr_t0, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(ihl_human_agency_tr_t5, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement(ihl_human_agency_tr_t10, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(ihl_human_agency_tr_t15, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(ihl_human_agency_tr_t20, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement(ihl_human_agency_tr_t25, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 25, 0.26).
narrative_ontology:measurement(ihl_human_agency_tr_t30, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(ihl_human_agency_be_t0, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ihl_human_agency_be_t5, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(ihl_human_agency_be_t10, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(ihl_human_agency_be_t15, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement(ihl_human_agency_be_t20, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(ihl_human_agency_be_t25, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 25, 0.76).
narrative_ontology:measurement(ihl_human_agency_be_t30, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 30, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(ihl_human_agency_su_t0, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(ihl_human_agency_su_t5, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(ihl_human_agency_su_t10, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(ihl_human_agency_su_t15, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(ihl_human_agency_su_t20, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(ihl_human_agency_su_t25, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 25, 0.83).
narrative_ontology:measurement(ihl_human_agency_su_t30, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 30, 0.86).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__human_agency_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__outcomes_based_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__categorical_prohibition_reading).

% DUAL FORMULATION NOTE:
% The kernel ihl_distinction_proportionality decomposes into three structurally distinct constraints: categorical_prohibition_reading (all LAWS banned), human_agency_reading (human final decision required), and outcomes_based_reading (technology-neutral compliance). Each reading has a distinct epsilon, beneficiary structure, and logical relationship to the others. They form a constraint family linked by mutual influence and logical pressure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
