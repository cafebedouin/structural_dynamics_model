% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__liberal_due_process_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__liberal_due_process_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: magna_carta_clause_39__liberal_due_process_reading
 *   human_readable: Clause 39 as Universal Due-Process Guarantee (Liberal Reading)
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This story authors the liberal due-process reading of Magna Carta Clause
 *   39: the claim that 'no free man' has, through centuries of judicial and
 *   political development, come to mean 'no person' — establishing a
 *   universal constraint on arbitrary state power that binds the executive
 *   against detention, dispossession, or exile without lawful judgment. This
 *   is one of three structurally distinct readings of the same kernel text.
 *   The feudal_prerogative_reading holds the clause preserved narrow
 *   procedural rights within a hierarchical order and did not contemplate
 *   universal application. The originalist_limitation_reading holds the
 *   clause addresses only the specific documented abuses of King John's reign
 *   and carries no doctrine transferable to modern executive power generally.
 *   This reading's ε is authored independently and is NOT an average or hedge
 *   across those readings — it reflects the extraction the liberal reading
 *   itself attributes to unchecked executive discretion, measured by the
 *   reading's own lights, against the standing arrangement of executive
 *   prerogative under contest.
 *
 * KEY AGENTS:
 *   - citizens_subject_to_state_power: universal beneficiary class under this reading, powerless individually, trapped within state jurisdiction
 *   - liberal_constitutional_courts: institutional agenda-setters who administer the expansive interpretation through case law
 *   - executive_branch_prerogative_holders: primary payers, constrained in their discretionary action by judicial invocation of the clause
 *   - security_state_apparatus: institutional payer bearing compliance costs from procedural constraints on detention and seizure
 *   - medieval_barons_lineage_claim: the excluded original claimants, invoked symbolically but absent from the operative modern reading
 *   - legal_historians: analytical observers assessing the reading's fidelity to the 1215 text
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, 0.68).
domain_priors:suppression_score(magna_carta_clause_39__liberal_due_process_reading, 0.55).
domain_priors:theater_ratio(magna_carta_clause_39__liberal_due_process_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__liberal_due_process_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__liberal_due_process_reading, "Clause 39 as Universal Due-Process Guarantee (Liberal Reading)").
narrative_ontology:topic_domain(magna_carta_clause_39__liberal_due_process_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__liberal_due_process_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__liberal_due_process_reading, '9913d9c4-3db3-4eab-9fd4-41e28dce08ac').
narrative_ontology:cs_kernel_codification('9913d9c4-3db3-4eab-9fd4-41e28dce08ac', fixed_text).
narrative_ontology:cs_authority_grounding('9913d9c4-3db3-4eab-9fd4-41e28dce08ac', lineage).
narrative_ontology:cs_interpretation_layer_present('9913d9c4-3db3-4eab-9fd4-41e28dce08ac').
narrative_ontology:cs_reading_relation('9913d9c4-3db3-4eab-9fd4-41e28dce08ac', magna_carta_clause_39__feudal_prerogative_reading, forecloses).
narrative_ontology:cs_reading_relation('9913d9c4-3db3-4eab-9fd4-41e28dce08ac', magna_carta_clause_39__originalist_limitation_reading, influences).
narrative_ontology:cs_axiom('9913d9c4-3db3-4eab-9fd4-41e28dce08ac', foundational, personhood_not_estate_grounds_the_protection).
narrative_ontology:cs_axiom_status(personhood_not_estate_grounds_the_protection, holdable).
narrative_ontology:cs_axiom_grounding('9913d9c4-3db3-4eab-9fd4-41e28dce08ac', personhood_not_estate_grounds_the_protection, deontological).
narrative_ontology:cs_axiom('9913d9c4-3db3-4eab-9fd4-41e28dce08ac', secondary, doctrine_legitimately_expands_through_judicial_practice).
narrative_ontology:cs_axiom_status(doctrine_legitimately_expands_through_judicial_practice, holdable).
narrative_ontology:cs_axiom_grounding('9913d9c4-3db3-4eab-9fd4-41e28dce08ac', doctrine_legitimately_expands_through_judicial_practice, conventional).
narrative_ontology:cs_reference_frame('9913d9c4-3db3-4eab-9fd4-41e28dce08ac', baronial_procedural_settlement_1215).
narrative_ontology:cs_drift_state('9913d9c4-3db3-4eab-9fd4-41e28dce08ac', contemporary_constitutional_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('9913d9c4-3db3-4eab-9fd4-41e28dce08ac', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, citizens_subject_to_state_power).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, liberal_constitutional_courts).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, rule_of_law_advocacy_movements).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, executive_branch_prerogative_holders).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, security_state_apparatus).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__liberal_due_process_reading, universal_due_process_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__liberal_due_process_reading, constitutional_supremacy_over_executive_discretion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, every individual — not merely free barons — holds a standing entitlement to lawful judgment or due process before the state can imprison, dispossess, or exile them. They cannot exit the jurisdiction that claims authority over them; the reading's value to them is precisely that it constrains the sovereign they cannot escape.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, citizens_subject_to_state_power, beneficiary,
    powerless, civilizational, trapped, national).

% Courts and constitutional scholars administer this reading by citing Clause 39 as precedent for due-process guarantees, expanding its 1215 text into modern doctrines (habeas corpus, procedural fairness, judicial review of executive detention). They set the interpretive agenda by choosing which cases invoke the clause and how broadly.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, liberal_constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Civil liberties organizations and constitutional reform movements invoke this reading as foundational lineage for universal rights claims, gaining rhetorical and legal leverage from tracing modern due-process guarantees to an 800-year-old text. They can redirect their advocacy elsewhere if the reading loses purchase, but currently derive significant institutional legitimacy from it.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, rule_of_law_advocacy_movements, beneficiary,
    organized, generational, mobile, global).

% Heads of state, ministers, and security agencies find their discretionary powers — detention, seizure, emergency action — constrained by judicial invocation of this reading. They cannot simply ignore the doctrine once courts have entrenched it; they must litigate, seek statutory carve-outs, or comply, all of which cost political and administrative capital.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, executive_branch_prerogative_holders, payer,
    powerful, biographical, constrained, national).

% Intelligence and policing bodies that rely on swift, unreviewable action find this reading imposing procedural friction — warrant requirements, judicial oversight, disclosure obligations — that did not exist under narrower interpretations. They bear the ongoing cost of compliance infrastructure and litigation exposure.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, security_state_apparatus, payer,
    institutional, generational, constrained, national).

% The historical class of free tenants-in-chief for whom the clause was originally negotiated has no living voice in the modern dispute; their narrower, class-specific claim is not part of this reading's operative logic but is invoked only as symbolic ancestry. Their absence from the conversation is structural, not incidental — this reading's universalism requires reading past their specific historical position.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, medieval_barons_lineage_claim, excluded,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(magna_carta_clause_39__liberal_due_process_reading, medieval_barons_lineage_claim).

% Scholars of medieval law assess whether the universalist reading is a faithful development or an anachronistic projection backward onto a feudal settlement. They supply the corroborating or disconfirming evidence other seats rely on but do not adjudicate the live constitutional dispute themselves.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared textual anchor that lets courts, legislatures, and citizens coordinate around a stable principle — no person shall be deprived of liberty or property except by lawful judgment — reducing the need to renegotiate the boundary of executive power in every dispute.
% TRANSFER_FUNCTION: Moves discretionary power away from the executive and security apparatus and toward courts and individuals: procedural constraints, litigation exposure, and compliance costs flow from the state to the judiciary, while protection from arbitrary action flows to the governed.
% ABSENT_VOICES: The medieval baronial class for whom Clause 39 was actually negotiated in 1215 — a narrow group of free men, not universal subjects — is not present to object that its specific settlement has been generalized beyond its original scope. Originalist historians raise this on their behalf, but the class itself has no standing seat in the modern debate.
% DISAPPEARANCE_RATIONALE: If courts stopped citing Clause 39 as authority for universal due process, a substantial body of habeas corpus and procedural-fairness jurisprudence would lose its historical anchor overnight. Litigants and courts would need to relocate the doctrinal foundation elsewhere (natural rights theory, later statutes, international human rights instruments), executive branches would face a temporary vacuum in which detention and seizure powers face less entrenched resistance, and constitutional argument would visibly reorganize around whatever alternative anchor is adopted.
% FOUNDING_PROBLEM: The 1215 barons sought protection against King John's arbitrary seizure of their lands, arbitrary imprisonment, and denial of justice — a narrow grievance about royal abuse of a specific feudal relationship.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the advocacy movements that benefit from the universalist reading (e.g., scholars of medieval feudal law) attest that the founding problem was narrow and class-specific, not a universal human-rights claim; constitutional courts and rights advocates dispute this, treating the clause's language ('no free man') as expandable to 'no person' as social conceptions of personhood widened. No party wholly outside either camp offers an unimpeachable adjudication — the corroboration itself remains split along interpretive lines.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__liberal_due_process_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__liberal_due_process_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__liberal_due_process_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_clause_39__liberal_due_process_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__liberal_due_process_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__liberal_due_process_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_clause_39__liberal_due_process_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.68 by interval end because this reading treats the accreted body of due-process jurisprudence as extracting real, binding constraint from executive and security actors who would otherwise exercise broader discretion — the reading holds this constraint is legitimate and increasingly entrenched, not merely rhetorical. Suppression is moderate (0.55): the reading depends on courts actively enforcing the doctrine against resistant executives, but does not require coercing citizens (who are beneficiaries, not targets) into compliance. Theater is moderate (0.4) reflecting that some invocations of Clause 39 in modern rights advocacy are more symbolic-lineage claims than operative legal constraints — the historical clause itself does no work; the accreted doctrine built on top of it does. Accessibility collapse is deliberately low (0.35): this reading does not claim the alternative readings have become unthinkable — feudal and originalist readings remain live scholarly and judicial positions, which is precisely why this is a kernel-reading story rather than a settled fact.
 *
 * DIRECTIONALITY LOGIC:
 *   Citizens are the structural beneficiaries under this reading: they hold the entitlement, cannot exit the jurisdiction, and would bear the cost of the constraint's removal, so directionality sits near the beneficiary end. Executive and security actors are the targets: their discretion is what the constraint binds, and their exit options are constrained by the doctrine's judicial entrenchment rather than eliminated outright (they retain political and legislative avenues to push back, hence 'constrained' rather than 'trapped'). Courts occupy the agenda-setting seat — they do not personally collect from the constraint's operation but administer and expand its application, generating the coordination function credited to this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (arbitrary royal seizure of specific baronial land and liberty in 1215) is contested as either dead-but-repurposed or genuinely generalized. This reading holds the problem is not dead but has expanded in scope: arbitrary state power against individuals remains live, and the clause's authority has been legitimately extended to address it. The mandatrophy risk this reading must guard against is treating a historically narrow settlement as if it had always been a universal charter — the omega on originalist vs. universalist meaning captures this directly. The reading is defensible precisely because courts corroborate ongoing extension through case law rather than the doctrine floating free of any live institutional practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universalist_vs_originalist_textual_warrant,
    'Does the textual and historical record support reading ''no free man'' (liber homo) as evolving toward ''no person,'' or is this an anachronistic backward projection of later liberal political theory onto a narrow 1215 feudal settlement?',
    'Close textual-historical analysis of the 1215 charter''s drafting context, comparison with the 1225 and subsequent reissues, and tracing the documented chain of judicial citation from Coke through the founding era to determine whether expansion was gradual doctrinal development or a discontinuous reinterpretation.',
    'If the expansion is textually and historically warranted, this reading''s extraction claim against modern executive discretion is well-grounded. If it is anachronistic projection, the extraction attributed to this reading is better understood as invented tradition serving present-day rights advocacy rather than genuine constraint inherited from the clause.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universalist_vs_originalist_textual_warrant, conceptual, 'Whether the universalist reading is genuine doctrinal development or anachronistic reinterpretation.').

omega_variable(
    kernel_reading_selection_and_beneficiary_incentive,
    'Do the parties who most benefit from the universalist reading — rights advocacy movements, expansive constitutional courts — have a structural incentive to select this reading over the narrower siblings regardless of its historical accuracy?',
    'Compare the institutional incentives and rhetorical utility of each reading for its primary advocates; examine whether courts invoke the universalist reading disproportionately in cases where broader precedent-setting authority benefits the judiciary''s own institutional power.',
    'If reading selection tracks beneficiary incentive rather than independent historical or legal analysis, the reading''s high extractiveness score partly reflects motivated interpretation rather than a neutral assessment of executive overreach.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_and_beneficiary_incentive, preference, 'Whether beneficiary incentive drives reading selection independent of historical merit.').

omega_variable(
    clause_39_natural_law_vs_constructed_lineage,
    'Is the modern due-process doctrine this reading credits to Clause 39 actually descended from the clause, or does the clause function as a constructed symbolic ancestor for a doctrine that developed independently through natural rights theory, common law evolution, and modern constitutional drafting?',
    'Trace citation genealogy in landmark due-process cases to determine whether Clause 39 functions as load-bearing precedent or as rhetorical invocation layered onto doctrine that would exist without it.',
    'If the clause is rhetorical rather than load-bearing, the beneficiaries named in this story (rule-of-law movements, constitutional courts) derive legitimacy benefit from a symbolic rather than causal lineage claim — relevant to whether the ''coordination function'' claimed here is genuine or a false-summit-style natural-law dressing on a constructed modern doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clause_39_natural_law_vs_constructed_lineage, empirical, 'Whether Clause 39 is causally load-bearing for modern due process or a constructed symbolic lineage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__liberal_due_process_reading, 1215, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1215, 0.5).
narrative_ontology:measurement_basis(magn_tr_t1215, observed).
narrative_ontology:measurement(magn_tr_t1689, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1689, 0.42).
narrative_ontology:measurement_basis(magn_tr_t1689, observed).
narrative_ontology:measurement(magn_tr_t1789, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1789, 0.38).
narrative_ontology:measurement_basis(magn_tr_t1789, observed).
narrative_ontology:measurement(magn_tr_t1900, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1900, 0.35).
narrative_ontology:measurement_basis(magn_tr_t1900, observed).
narrative_ontology:measurement(magn_tr_t1950, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 1950, 0.35).
narrative_ontology:measurement_basis(magn_tr_t1950, observed).
narrative_ontology:measurement(magn_tr_t2001, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 2001, 0.38).
narrative_ontology:measurement_basis(magn_tr_t2001, observed).
narrative_ontology:measurement(magn_tr_t2025, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 2025, 0.4).
narrative_ontology:measurement_basis(magn_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1215, 0.15).
narrative_ontology:measurement_basis(magn_be_t1215, observed).
narrative_ontology:measurement(magn_be_t1689, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1689, 0.25).
narrative_ontology:measurement_basis(magn_be_t1689, observed).
narrative_ontology:measurement(magn_be_t1789, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1789, 0.35).
narrative_ontology:measurement_basis(magn_be_t1789, observed).
narrative_ontology:measurement(magn_be_t1900, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1900, 0.45).
narrative_ontology:measurement_basis(magn_be_t1900, observed).
narrative_ontology:measurement(magn_be_t1950, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement_basis(magn_be_t1950, observed).
narrative_ontology:measurement(magn_be_t2001, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 2001, 0.62).
narrative_ontology:measurement_basis(magn_be_t2001, observed).
narrative_ontology:measurement(magn_be_t2025, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement_basis(magn_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1215, 0.3).
narrative_ontology:measurement_basis(magn_su_t1215, observed).
narrative_ontology:measurement(magn_su_t1689, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1689, 0.35).
narrative_ontology:measurement_basis(magn_su_t1689, observed).
narrative_ontology:measurement(magn_su_t1789, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1789, 0.4).
narrative_ontology:measurement_basis(magn_su_t1789, observed).
narrative_ontology:measurement(magn_su_t1900, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1900, 0.45).
narrative_ontology:measurement_basis(magn_su_t1900, observed).
narrative_ontology:measurement(magn_su_t1950, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement_basis(magn_su_t1950, observed).
narrative_ontology:measurement(magn_su_t2001, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 2001, 0.53).
narrative_ontology:measurement_basis(magn_su_t2001, observed).
narrative_ontology:measurement(magn_su_t2025, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 2025, 0.55).
narrative_ontology:measurement_basis(magn_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__liberal_due_process_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_clause_39__liberal_due_process_reading, 0.1).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39__feudal_prerogative_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39__originalist_limitation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint files decomposing the natural-language label 'Magna Carta Clause 39' per the ε-invariance principle. feudal_prerogative_reading treats the clause as a narrow hierarchy-preserving protection (low ε, minimal beneficiary/victim asymmetry). originalist_limitation_reading treats it as bound to 1215-specific royal abuses with no modern transferable doctrine (low-to-moderate ε, narrow scope). liberal_due_process_reading (this file) treats it as the textual root of universal due-process rights against arbitrary state power (high ε, broad victim set of executive/security actors, broad beneficiary set of all citizens). All three share the same kernel text but are structurally distinct constraints with different ε, different stakeholders, and different classifications — they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
