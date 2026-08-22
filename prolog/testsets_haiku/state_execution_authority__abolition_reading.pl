% ============================================================================
% CONSTRAINT STORY: state_execution_authority__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__abolition_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: state_execution_authority__abolition_reading
 *   human_readable: State Execution Authority (Abolition Reading)
 *   domain: constitutional_law/political_philosophy/criminal_justice
 *
 * SUMMARY:
 *   The abolition reading treats state execution as categorically
 *   impermissible—a claim that the state has no legitimate authority to
 *   execute any person, regardless of crime severity, procedural safeguards,
 *   guilt certainty, or deterrent effect. This reading is one of three live
 *   interpretations of the same constitutional/moral kernel (state execution
 *   authority). The abolition reading instantiates a distinct constraint
 *   story with very high extractiveness (0.95): from this reading's
 *   perspective, execution is an irreversible taking of life that cannot be
 *   legitimated by retributive or deterrent justification. All executed
 *   persons, including those convicted of terrible crimes, enter the victim
 *   set. No institutional actor derives legitimate benefit. The measurement
 *   series show extractiveness stable across the interval—the core claim does
 *   not waver—while theater_ratio rises slightly (expanding procedural
 *   safeguards create appearance of legitimacy without changing the
 *   categorical prohibition) and suppression_requirement stays elevated (the
 *   constraint persists by active enforcement, not by consent or
 *   inevitability).
 *
 * KEY AGENTS:
 *   - executed_persons: powerless individuals subject to state killing; the ultimate extraction target
 *   - families_of_executed: moderate power; bear loss, stigma, and finality
 *   - state_execution_apparatus: institutional agenda-setter; administers the constraint
 *   - retributive_and_deterrence_proponents: institutional beneficiaries under their own frames; rejected as legitimate by this reading
 *   - wrongfully_convicted_and_exonerated: proof of systemic fallibility; structurally absent once executed
 *   - international_human_rights_bodies: analytical observers; assess the constraint as violation
 *   - abolitionist_movements: organized observers; reshape discourse around the constraint's illegitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__abolition_reading, 0.95).
domain_priors:suppression_score(state_execution_authority__abolition_reading, 0.72).
domain_priors:theater_ratio(state_execution_authority__abolition_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__abolition_reading, snare).
narrative_ontology:human_readable(state_execution_authority__abolition_reading, "State Execution Authority (Abolition Reading)").
narrative_ontology:topic_domain(state_execution_authority__abolition_reading, "constitutional_law/political_philosophy/criminal_justice").

domain_priors:requires_active_enforcement(state_execution_authority__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__abolition_reading, '36afac69-8dfc-428e-bd4b-57260757b741').
narrative_ontology:cs_kernel_codification('36afac69-8dfc-428e-bd4b-57260757b741', formalized).
narrative_ontology:cs_authority_grounding('36afac69-8dfc-428e-bd4b-57260757b741', extraction).
narrative_ontology:cs_interpretation_layer_present('36afac69-8dfc-428e-bd4b-57260757b741').
narrative_ontology:cs_reading_relation('36afac69-8dfc-428e-bd4b-57260757b741', state_execution_authority__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('36afac69-8dfc-428e-bd4b-57260757b741', state_execution_authority__deterrence_reading, coexists_with).
narrative_ontology:cs_axiom('36afac69-8dfc-428e-bd4b-57260757b741', foundational, state_execution_categorically_impermissible).
narrative_ontology:cs_axiom_status(state_execution_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('36afac69-8dfc-428e-bd4b-57260757b741', state_execution_categorically_impermissible, deontological).
narrative_ontology:cs_reference_frame('36afac69-8dfc-428e-bd4b-57260757b741', human_dignity_and_state_authority_limits).
narrative_ontology:cs_drift_state('36afac69-8dfc-428e-bd4b-57260757b741', contemporary_global_abolitionist_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('36afac69-8dfc-428e-bd4b-57260757b741', '').
narrative_ontology:cs_kernel_id(state_execution_authority__abolition_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, executed_persons).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, families_of_executed).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_execution_authority__abolition_reading, retributive_and_deterrence_proponents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals subject to execution sentences. Under the abolition reading, they are victims of the state regardless of guilt or innocence because state execution is categorically impermissible. No procedural safeguard, no crime severity, no judicial review can legitimate the arrangement from this seat. Death is the irreversible extraction.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, executed_persons, payer,
    powerless, immediate, trapped, national).

% Families bear the loss of the executed person, social stigma from association with capital crime, and institutional inability to contest the sentence once carried out. They have constrained exit (jurisdiction matters, petition processes exist but are narrow) and carry intergenerational trauma from the state's action.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, families_of_executed, payer,
    moderate, generational, constrained, national).

% The institutional machinery—legislatures, courts, executive officials, prison personnel—that administers capital sentences. From the abolition reading's perspective, the apparatus enforces an illegitimate constraint and is complicit in systematic victimization. Its decisions cannot legitimate the arrangement; legitimacy cannot flow from the mechanism itself under a categorical prohibition.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, state_execution_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Political actors, academic theorists, and judicial officers who justify execution as retribution for heinous crimes or as a deterrent. From the abolition reading, they benefit from a system that claims moral legitimacy for state killing; their institutional authority to render such judgments is the arrangement's central extraction mechanism. The abolition reading rejects their justificatory frame entirely.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, retributive_and_deterrence_proponents, beneficiary,
    institutional, generational, analytical, national).

% Persons exonerated after execution or released from death row prove the system's fallibility. Under the abolition reading, every exoneration is proof that the state extraction mechanism is illegitimate: if any innocent can be executed, the categorical prohibition is justified. They would voice the strongest objection to the arrangement if present in deliberation, but the system's finality means they are structurally absent when already executed.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, wrongfully_convicted_and_exonerated, excluded,
    powerless, biographical, trapped, national).

% Organizations like the UN Human Rights Committee, regional courts, and treaty bodies assess capital punishment against human rights law. They take testimony, conduct investigations, and issue findings that reframe the constraint as a violation. Their analytical seat sits outside the executing state's jurisdiction.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% Civil society organizations advocating for the abolition of capital punishment. They document wrongful executions, organize resistance, and reshape public discourse. From the abolition reading's perspective, they represent the true position of those who would object if systematically consulted; their analytical work makes the constraint's illegitimacy visible.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, abolitionist_movements, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__abolition_reading, state_execution_apparatus).
narrative_ontology:fixing_cost_class(state_execution_authority__abolition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. The abolition reading rejects any coordination framing for state execution. Retributive and deterrence readings claim coordination around shared values (justice, public safety); the abolition reading treats those claims as cover stories. There is no authentic coordination function to identify from this seat.
% TRANSFER_FUNCTION: Moves the ultimate extraction—the life of the executed person—from the convicted individual to state authority. Simultaneously transfers moral authority to the state apparatus (courts, legislatures, executioners) to define who deserves death and to carry out that judgment without reversibility.
% ABSENT_VOICES: Executed persons and the wrongfully convicted cannot testify after the sentence is carried out. Their structural absence from the deliberative process (finality of execution) is itself evidence of the constraint's illegitimacy from the abolition reading. Families and exonerated survivors who do voice objection are systematically discounted as emotionally invested parties rather than credible witnesses. International human rights bodies and abolition movements are excluded from the enforcing state's law-making process.
% DISAPPEARANCE_RATIONALE: If execution authority disappeared overnight, states would substitute life imprisonment without parole (or other long-term incapacitation). This is not a restoration to a prior state but a fundamental reconfiguration: the irreversibility vanishes, the finality pressure on courts disappears, the state's killing apparatus is dismantled. The social world reorganizes around the principle that the state may not kill. Crime rates, family structures, institutional legitimacy, and international relations would all shift.
% FOUNDING_PROBLEM: Retributive reading: proportionate punishment for heinous crimes requires a penalty equal to the crime's moral gravity. Deterrence reading: capital crimes require the highest deterrent force to prevent repetition. Abolition reading rejects both as justifications and identifies the founding problem differently: historical societies lacked alternatives to execution for permanent incapacitation; they built execution into criminal justice as the only means to prevent escape and recurrence. Conditions have changed.
% FOUNDING_PROBLEM_CORROBORATION: Abolitionists and international human rights bodies attest the founding problem is solved: modern incapacitation through imprisonment is technically feasible and legally available in nearly all abolitionist jurisdictions. Retributive and deterrence proponents attest the problem is live: heinous crimes still demand the highest response and maximum deterrence. Independent empirical research from criminology and penology shows no causal relationship between execution and murder rates (corroborating the abolition reading's claim that deterrence justification is spurious). Historical legal scholarship documents the contingency of execution as a necessity—other methods were always available but chosen for reasons of theology, tradition, and power rather than technical necessity.
narrative_ontology:disappearance_verdict(state_execution_authority__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__abolition_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__abolition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_execution_authority__abolition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__abolition_reading, 0.95, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__abolition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__abolition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.95) because execution is the ultimate extraction—the irreversible taking of life—and from the abolition reading's perspective there is no legitimate justification that can convert this taking into coordination. Retributive justification (this death is owed by moral balance) and deterrence justification (this death will prevent future murders) are both rejected as illegitimate under the categorical principle. Suppression is substantial (0.72) because the constraint persists through active enforcement: courts must rule on execution, legislatures must defend it, executioners must carry it out, and families/movements that resist it are overridden. Theater_ratio is low-to-moderate (0.18) because the apparatus invests real resources in procedural legitimacy—appellate review, stays for investigation—but from the abolition reading, these procedures are ornamental; they do not address the categorical claim that execution is impermissible. The series tracks a steady state: extractiveness does not rise over time (the claim is fixed), theater gradually increases (more procedural show), and suppression settles at a stable level (the constraint requires continuous institutional labor to maintain against growing resistance).
 *
 * PERSPECTIVAL GAP:
 *   The seat divergence is the entire analytical point of generating this reading as a separate constraint. From the state execution apparatus's institutional seat (or the retributive/deterrence proponent seat), the constraint might compute as justified, proportionate, and necessary—a rope coordinating around shared values of justice and public safety. From the executed person's powerless seat, it computes as pure extraction with no alternative: trapped, identity_locked (the death sentence defines all their remaining options), no exit. From the abolitionist observer seat, it computes as snare—a constructed killing mechanism maintained by institutional power and justified by cover stories that empirical evidence rejects. The engine will compute these divergences from the structural data (power atoms, exit options, beneficiary/victim declarations, spatial scope); the abolition reading's authoring establishes one such seat's full structural situation.
 *
 * DIRECTIONALITY LOGIC:
 *   Executed persons have powerless power, immediate time horizon, trapped exit, and are explicitly listed as victims. Directionality for this seat is maximum (d = 1.0): they are the pure extraction target. Families of executed have moderate power, generational horizon, constrained exit, and are listed as victims; their d is high (~0.85). Retributive/deterrence proponents are institutional power, generational horizon, and listed as beneficiaries (they collect institutional authority and legitimacy from the arrangement); their d is low (~0.1). The state execution apparatus is institutional power, generational horizon, analytical exit (they could change the law); they are the agenda_setter, not a beneficiary, because they do not collect private gain—they execute public authority. Their d is moderate (~0.5). Wrongfully convicted and exonerated persons would have powerless power, biographical horizon, and should be victimized; they are excluded from the deliberative process, which itself is proof of illegitimacy from the abolition reading.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy does not apply to this constraint. The founding problem (historical necessity of execution for permanent incapacitation) is genuinely dead—modern imprisonment provides incapacitation without death—and the disappearance verdict is world_rearranges (the constraint's removal reorganizes criminal justice). There is no gap between founding justification and current function that would trigger a mandatrophy declaration. The constraint persists not because the founding problem lives on but because institutional actors (retributive, deterrence, and sovereignty proponents) maintain it despite its founding justification being obsolete. This is a different pathology: the constraint is live and extractive, not atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_empirical_boundaries,
    'Is the abolition reading''s categorical prohibition on execution a formal principle (no execution under any circumstances) or contingent on empirical facts (wrong if execution causes innocent deaths)?',
    'Textual analysis of abolition arguments (constitutional and philosophical): do they ground the prohibition in deontological principle (human dignity, state authority limits) or in empirical outcomes (wrongful execution rates, deterrence failure)?',
    'If grounded in deontology, the reading is not subject to empirical override; wrongful execution proves the principle correct, not incorrect. If grounded in empirical claims, a system with zero wrongful executions and proven deterrence would logically defeat the reading from within its own premises. The abolition movement''s strategic discourse suggests both grounds coexist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_empirical_boundaries, conceptual, 'Whether the abolition principle is deontological or empirically contingent.').

omega_variable(
    forced_labor_vs_execution_equivalence,
    'If lifetime imprisonment (forced labor, loss of freedom, isolation) is acceptable and not categorically prohibited, what makes execution categorically prohibited while lifetime imprisonment is not?',
    'Philosophical analysis: what property does execution have that lifetime imprisonment lacks? Irreversibility, finality, dignity loss, state role in death vs. state role in confinement. If irreversibility is the criterion, then a hypothetical technology for reversible execution would shift the boundary.',
    'A satisfactory answer that distinguishes execution from lifetime imprisonment on principled grounds (not merely intensity or taboo) would strengthen the abolition reading''s foundation. An unsatisfactory answer would suggest the categorical prohibition rests on cultural contingency rather than deep principle, opening the reading to internal challenge from siblings who accept imprisonment but not execution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(forced_labor_vs_execution_equivalence, conceptual, 'The principled boundary between categorically prohibited execution and permitted lifetime imprisonment.').

omega_variable(
    sibling_axiom_foreclosure,
    'Does the abolition reading''s foundational axiom (no state execution is ever legitimate) logically foreclose the retributive and deterrence readings, or do the readings coexist as live but incompatible positions held by different institutional actors?',
    'Logical analysis: can a framework simultaneously hold ''no execution is ever legitimate'' and ''execution is legitimate punishment for heinous crimes''? No framework can. Yet multiple states, factions, and judges hold these positions simultaneously. Are the readings coexisting across different frameworks, or is one logically dominant and the other illusory/false?',
    'If the readings foreclose each other, one is true and the others false—the constraint''s classification as snare reflects factual illegitimacy. If they coexist across different parties'' frameworks, they are live but incompatible—the constraint''s classification as snare reflects structural asymmetry but not factual falsity. The reading_relations declaration (forecloses vs. coexists_with) hinges on this resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_axiom_foreclosure, conceptual, 'Logical relationship between abolition, retributive, and deterrence axioms: foreclosure or coexistence.').

omega_variable(
    identity_locked_exit_and_innocence,
    'Why does wrongful execution of innocent persons prove the abolition reading''s point more forcefully than retributive/deterrence readings acknowledge?',
    'Empirical data on wrongful executions, DNA exonerations, and system error rates; interviews with retributive and deterrence proponents about how they accommodate wrongful execution within their frameworks.',
    'If retributive and deterrence proponents treat wrongful execution as a tragic system failure but still maintain execution is legitimate in principle, the abolition reading can point to the persistent gap between theory and practice—no safeguard has eliminated wrongful execution. If they treat wrongful execution as foreclosing the principle (no execution if any risk of wrongfulness), they concede ground to the abolition reading. The measurement of how theories accommodate this asymmetry reveals whether the abolition reading''s point about categorical impermissibility is integrated into sibling framings or remains external.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_locked_exit_and_innocence, empirical, 'Whether wrongful execution proves abolition axioms or merely reveals system failures within retributive/deterrence frameworks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__abolition_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__abolition_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t5, state_execution_authority__abolition_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement_basis(stat_tr_t5, observed).
narrative_ontology:measurement(stat_tr_t10, state_execution_authority__abolition_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement_basis(stat_tr_t10, observed).
narrative_ontology:measurement(stat_tr_t20, state_execution_authority__abolition_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(stat_tr_t20, observed).
narrative_ontology:measurement(stat_tr_t30, state_execution_authority__abolition_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement_basis(stat_tr_t30, observed).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__abolition_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(stat_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__abolition_reading, base_extractiveness, 0, 0.92).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t5, state_execution_authority__abolition_reading, base_extractiveness, 5, 0.93).
narrative_ontology:measurement_basis(stat_be_t5, observed).
narrative_ontology:measurement(stat_be_t10, state_execution_authority__abolition_reading, base_extractiveness, 10, 0.94).
narrative_ontology:measurement_basis(stat_be_t10, observed).
narrative_ontology:measurement(stat_be_t20, state_execution_authority__abolition_reading, base_extractiveness, 20, 0.95).
narrative_ontology:measurement_basis(stat_be_t20, observed).
narrative_ontology:measurement(stat_be_t30, state_execution_authority__abolition_reading, base_extractiveness, 30, 0.95).
narrative_ontology:measurement_basis(stat_be_t30, observed).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__abolition_reading, base_extractiveness, 40, 0.95).
narrative_ontology:measurement_basis(stat_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__abolition_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement_basis(stat_su_t0, observed).
narrative_ontology:measurement(stat_su_t5, state_execution_authority__abolition_reading, suppression_requirement, 5, 0.7).
narrative_ontology:measurement_basis(stat_su_t5, observed).
narrative_ontology:measurement(stat_su_t10, state_execution_authority__abolition_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement_basis(stat_su_t10, observed).
narrative_ontology:measurement(stat_su_t20, state_execution_authority__abolition_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(stat_su_t20, observed).
narrative_ontology:measurement(stat_su_t30, state_execution_authority__abolition_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(stat_su_t30, observed).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__abolition_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(stat_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__abolition_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_execution_authority__abolition_reading, 0.0).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, state_execution_authority__deterrence_reading).

% DUAL FORMULATION NOTE:
% The state_execution_authority kernel decomposes into three distinct constraint stories: abolition_reading (this), retributive_reading, and deterrence_reading. Each instantiates a different ε, different beneficiary/victim structure, and different type classification. They share a single kernel text (Constitutional death-penalty clauses) but interpret it in incompatible ways. The abolition reading claims categorical prohibition (ε=0.95, snare); retributive reading claims legitimate punishment (lower ε); deterrence reading claims empirical deterrent necessity (ε contingent on deterrence evidence). Sibling links track which readings foreclose or influence each other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
