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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ihl_distinction_proportionality__human_agency_reading
 *   human_readable: IHL Human Agency in Lethal Targeting (Martens Clause Reading)
 *   domain: international_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   The international humanitarian law (IHL) framework—especially the Geneva
 *   Conventions and their protocols—requires combatants to distinguish
 *   combatants from civilians and to ensure that attacks are proportionate to
 *   military advantage. The Martens Clause, codified in the preambles of the
 *   1907 Hague Conventions and incorporated into subsequent IHL instruments,
 *   appeals to the "principles of humanity" and the "dictates of public
 *   conscience" as sources of legal obligation beyond specific treaty text.
 *   The human-agency reading interprets these principles to mean that the
 *   life/death decisions required by distinction and proportionality
 *   obligations cannot be delegated to machines—they require irreducible
 *   human moral judgment at the moment lethal force is applied. This reading
 *   has been articulated by the ICRC, many humanitarian law scholars, and
 *   states in the Non-Aligned Movement. It produces a high-extraction
 *   structure: it grants IHL interpretive authorities (primarily the ICRC)
 *   authority to certify or reject weapons systems based on their degree of
 *   human control, and it suppresses military development of fully autonomous
 *   lethal systems. Simultaneously, it creates an extraction from military
 *   operational efficiency and from autonomous weapons developers who must
 *   constrain their system designs. The constraint is claimed as tangled_rope
 *   because it coordinates a shared legal framework (all states interpret
 *   distinction and proportionality through the same human-agency lens) while
 *   extracting from those who bear its costs (slower operations, restricted
 *   design space). The authored metrics reflect that the constraint's
 *   extractiveness has risen over time as autonomous weapons technology has
 *   advanced, making the suppression of autonomous targeting an increasingly
 *   costly restriction; theater_ratio remains moderate because the
 *   human-agency requirement appears to be genuine (not theatrically
 *   performed) but its protective force (whether human judgment actually
 *   prevents violations) remains contingent on training and enforcement
 *   rather than on the constraint itself. This is one reading of a contested
 *   kernel (ihl_distinction_proportionality); the outcomes_based_reading and
 *   categorical_prohibition_reading represent alternative interpretations of
 *   the same underlying IHL obligations.
 *
 * KEY AGENTS:
 *   - IHL interpretive authorities (ICRC, humanitarian law bodies): maintain interpretive centrality and adjudicate weapons legality under the human-agency reading
 *   - Military forces and states: bear the operational cost of maintaining human control in targeting loops; constrained exit (must comply with certified IHL interpretation or face delegitimization)
 *   - Autonomous weapons developers: suppressed market space; can only develop human-supervised systems
 *   - Civilian populations in conflict zones: theoretically protected by the human-judgment requirement, contingent on quality of enforcement
 *   - Technology-neutral policy advocates: excluded from the adjudication framework; contest the reading's core premise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, 0.68).
domain_priors:suppression_score(ihl_distinction_proportionality__human_agency_reading, 0.72).
domain_priors:theater_ratio(ihl_distinction_proportionality__human_agency_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__human_agency_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__human_agency_reading, "IHL Human Agency in Lethal Targeting (Martens Clause Reading)").
narrative_ontology:topic_domain(ihl_distinction_proportionality__human_agency_reading, "international_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__human_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__human_agency_reading, 'c39929b3-6be9-429d-a617-203d3524d3b8').
narrative_ontology:cs_kernel_codification('c39929b3-6be9-429d-a617-203d3524d3b8', formalized).
narrative_ontology:cs_authority_grounding('c39929b3-6be9-429d-a617-203d3524d3b8', lineage).
narrative_ontology:cs_interpretation_layer_present('c39929b3-6be9-429d-a617-203d3524d3b8').
narrative_ontology:cs_reading_relation('c39929b3-6be9-429d-a617-203d3524d3b8', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_reading_relation('c39929b3-6be9-429d-a617-203d3524d3b8', ihl_distinction_proportionality__categorical_prohibition_reading, coexists_with).
narrative_ontology:cs_axiom('c39929b3-6be9-429d-a617-203d3524d3b8', foundational, human_judgment_irreducible_to_targeting).
narrative_ontology:cs_axiom_status(human_judgment_irreducible_to_targeting, holdable).
narrative_ontology:cs_axiom_grounding('c39929b3-6be9-429d-a617-203d3524d3b8', human_judgment_irreducible_to_targeting, deontological).
narrative_ontology:cs_axiom('c39929b3-6be9-429d-a617-203d3524d3b8', foundational, martens_clause_binds_process_not_outcome).
narrative_ontology:cs_axiom_status(martens_clause_binds_process_not_outcome, holdable).
narrative_ontology:cs_axiom_grounding('c39929b3-6be9-429d-a617-203d3524d3b8', martens_clause_binds_process_not_outcome, deontological).
narrative_ontology:cs_reference_frame('c39929b3-6be9-429d-a617-203d3524d3b8', humanitarian_law_with_human_agency_requirement).
narrative_ontology:cs_drift_state('c39929b3-6be9-429d-a617-203d3524d3b8', autonomous_weapons_capability_maturation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c39929b3-6be9-429d-a617-203d3524d3b8', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, military_operational_efficiency).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, autonomous_weapons_developers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__human_agency_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__human_agency_reading, 'none', 1).

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
 *   The extractiveness score (0.68 at 2035) reflects that the constraint imposes real costs on military operations and weapons development without those costs being proportionate to any benefit those actors receive. The suppression score (0.72) is high because the constraint's persistence depends on active enforcement: ICRC certification authority, states' treaty compliance monitoring, and the delegitimization machinery that sanctions states or entities that develop fully autonomous systems. Without this enforcement, states would develop autonomous systems where technically feasible. Theater_ratio is moderate (0.28) because the human-agency requirement appears to address a real IHL concern (whether machines can satisfy distinction and proportionality), but a growing share of the constraint's observed force comes from defending the human-control requirement itself rather than from demonstrable protective effects—the theater lies in the unresolved question of whether required human judgment actually prevents violations in practice, or whether it serves primarily to maintain IHL authorities' interpretive centrality. The measurement series shows extraction rising from 2010–2025 as autonomous weapons technology matured, then plateauing at 2030–2035 as the constraint's scope and strength stabilized (the suppression has reached its enforcement ceiling: either states comply or they defect, and the constraint cannot be made more extractive without losing state consent). The time grid is shared across all three metrics so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From the ICRC's and humanitarian advocates' seat, the human-agency constraint is genuine coordination—a shared legal framework that protects civilians and maintains the rule of law in warfare. From the military operator's seat, it is enforced extraction that slows targeting, increases cognitive burden, and reduces operational effectiveness without demonstrable protective gain (human operators make mistakes too). From the autonomous weapons developer's seat, it is market suppression grounded in a particular (contested) reading of IHL rather than in empirical evidence that autonomous systems cannot achieve distinction and proportionality. From the civilian population's seat (powerless, trapped), the constraint is theoretically protective but its actual protective force depends on whether the humans making required decisions are trained, conscientious, and supervised—none of which the constraint itself guarantees. The engine should compute these seats differently based on the structural data; the authored claim does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   IHL interpretive authorities sit at the beneficiary end of directionality (d ≈ 0.1–0.2): they collect institutional authority and the power to certify weapons systems; their exit options are analytical (they can opt out of interpretation but have low incentive to do so). Military forces and developers sit at the target end (d ≈ 0.8–0.9): they bear the operational and design constraints; their exit is highly constrained (they cannot simply abandon IHL compliance without state-level consequences). Civilian populations in conflict zones sit near symmetric (d ≈ 0.4–0.5): they benefit from the constraint theoretically (it restricts autonomous killing) but also bear its costs contingently (if human judgment is poor, the benefit collapses). The engine derives directionality from these structural relationships; the authored claim that this is tangled_rope is independent of and not adjusted by the directionality computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—how to interpret IHL obligations in an era of autonomous weapons technology—remains live and contested. The human-agency reading has not lost its raison d'être; humanitarian law scholars and the ICRC continue to articulate it as the correct interpretation. However, there is a real question whether the constraint's persistence is driven by the interpretive authority's ongoing commitment to the human-agency reading or by institutional inertia and the ICRC's vested interest in maintaining interpretive centrality. If military technology advances such that autonomous systems demonstrably achieve superior distinction and proportionality outcomes than human operators, the founding problem shifts: the question becomes whether superior technical performance can override the human-judgment requirement. At that inflection point, the constraint could be subject to mandatrophy pressure—the human-agency reading's justification might become outdated while the institutional enforcement machinery persists. For now, the status quo is that the founding problem remains live and the constraint's persistence is justified by the ICRC and humanitarian advocates on principled grounds (human judgment is irreducible to the obligations), but the constraint's future depends on whether that principle holds up against contrary evidence or whether it becomes theater—maintained to preserve institutional authority rather than to serve the protective function it claims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    human_judgment_irreducibility,
    'Is irreducible human moral judgment at the moment of lethal force application a logically necessary element of IHL''s distinction and proportionality obligations, or a contingent institutional interpretation that could be revised as technology advances?',
    'Comparative analysis of IHL treaty text, negotiating history, and jurisprudence from different regional human rights bodies; testing whether outcomes-based performance metrics could satisfy IHL obligations under an alternative reading. Direct empirical testing of whether human operators actually achieve superior distinction and proportionality outcomes compared to autonomous systems in controlled or real-world scenarios.',
    'If human judgment is logically irreducible, the human-agency reading is architecturally sound and the constraint persists. If it is contingent, the constraint becomes vulnerable to reinterpretation as autonomous systems improve technically, and the outcomes-based reading gains legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_judgment_irreducibility, conceptual, 'Whether the constraint rests on a logical necessity or an institutional interpretation.').

omega_variable(
    human_operator_performance,
    'In real armed conflict scenarios, how reliably do human operators in targeting roles actually achieve distinction and proportionality? Do required human-in-the-loop decisions prevent violations or merely substitute human judgment for machine judgment without improving outcomes?',
    'Systematic review of incident data from conflicts where human targeting was explicitly required (Gaza, Ukraine, drone operations); analysis of proportionality assessments and civilian harm; comparison with autonomous system performance in equivalent scenarios. Post-action reviews and legal findings from IHL compliance audits.',
    'If human judgment demonstrably prevents violations, the constraint''s protective force is validated. If human operators achieve similar violation rates to autonomous systems (or worse), the constraint becomes theater—maintained for institutional reasons rather than protective effect.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(human_operator_performance, empirical, 'The actual protective efficacy of the human-agency requirement.').

omega_variable(
    ihl_interpretive_authority_incentive,
    'To what extent does the ICRC''s and IHL authorities'' institutional interest in maintaining interpretive centrality drive the human-agency reading, as opposed to principled commitment to the reading''s normative content?',
    'Historical analysis of whether the human-agency reading would have been articulated if autonomous weapons technology did not pose a challenge to ICRC authority. Institutional analysis of whether the ICRC would relinquish the reading if outcomes-based performance criteria were proven superior and formally adopted. Examination of ICRC positions in counterfactual institutional scenarios (e.g., if a different body held interpretive authority).',
    'If institutional incentive is primary, the constraint is vulnerable to mandatrophy: it persists because the interpretive authority has vested interest, not because the protection is necessary. If principled commitment is primary, the constraint has stronger justification and resilience.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ihl_interpretive_authority_incentive, preference, 'The institutional incentive structure underlying the human-agency reading''s persistence.').

omega_variable(
    martens_clause_scope,
    'Does the Martens Clause''s appeal to ''principles of humanity'' and ''dictates of public conscience'' bind the content of IHL obligations to specific interpretations (human judgment), or does it permit evolution of interpretation as technology and understanding change?',
    'Textual and jurisprudential analysis of Martens Clause invocations across different contexts. Comparison of how different regional human rights bodies interpret the Martens Clause in relation to evolving technology (humanitarian law is not static; obligations are reinterpreted as contexts change). International Court of Justice or International Criminal Court dicta on the scope of Martens Clause obligations.',
    'If the Martens Clause permits reinterpretation, the human-agency reading is contingent on the current moment''s understanding of ''humanity'' and ''public conscience''—and those understandings could shift if autonomous systems demonstrably protect civilians. If the clause binds interpretation to specific content (human judgment), the reading has stronger normative foundation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(martens_clause_scope, conceptual, 'The interpretive scope of the Martens Clause in relation to technology change.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__human_agency_reading, 2010, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t2010, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(ihl__tr_t2015, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2015, 0.16).
narrative_ontology:measurement(ihl__tr_t2020, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2020, 0.21).
narrative_ontology:measurement(ihl__tr_t2025, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2025, 0.26).
narrative_ontology:measurement(ihl__tr_t2030, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2030, 0.28).
narrative_ontology:measurement(ihl__tr_t2035, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2035, 0.28).

% Extraction over time
narrative_ontology:measurement(ihl__be_t2010, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(ihl__be_t2015, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2015, 0.51).
narrative_ontology:measurement(ihl__be_t2020, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2020, 0.62).
narrative_ontology:measurement(ihl__be_t2025, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2025, 0.66).
narrative_ontology:measurement(ihl__be_t2030, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2030, 0.68).
narrative_ontology:measurement(ihl__be_t2035, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2035, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t2010, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2010, 0.48).
narrative_ontology:measurement(ihl__su_t2015, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2015, 0.58).
narrative_ontology:measurement(ihl__su_t2020, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2020, 0.66).
narrative_ontology:measurement(ihl__su_t2025, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2025, 0.7).
narrative_ontology:measurement(ihl__su_t2030, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2030, 0.72).
narrative_ontology:measurement(ihl__su_t2035, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2035, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__human_agency_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ihl_distinction_proportionality__human_agency_reading, 0.12).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__outcomes_based_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__categorical_prohibition_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, autonomous_weapons_systems_regulation).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, military_command_responsibility).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-reading kernel (ihl_distinction_proportionality). The human-agency reading asserts that human judgment is irreducible to distinction and proportionality; the outcomes-based reading asserts that outcomes alone satisfy the obligations; the categorical_prohibition reading asserts that autonomous targeting violates human dignity per se. All three readings interpret the same IHL obligations but extract different normative content. They are linked via network.affects_constraints because each reading's interpretive claim influences the others—a shift in one reading's authority affects the legitimacy of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
