% ============================================================================
% CONSTRAINT STORY: state_execution_authority__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__retributive_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: state_execution_authority__retributive_reading
 *   human_readable: State Execution Authority (Retributive Reading)
 *   domain: criminal_justice/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the RETRIBUTIVE READING of state execution
 *   authority. The kernel—'does the state have legitimate authority to
 *   execute persons convicted of heinous crimes?'—is contested across three
 *   reading communities: retributivists who answer yes and ground it in moral
 *   restoration, deterrence theorists who answer conditionally and ground it
 *   in crime prevention, and abolitionists who answer no and treat execution
 *   as a categorical violation of human dignity. This story describes ONLY
 *   the retributive reading: execution restores moral balance to victims and
 *   the community by imposing proportionate punishment on the offender. The
 *   reading's beneficiaries are victims' families and the retributive moral
 *   order itself; the victims are the executed offender and those awaiting
 *   execution. High extractiveness (0.72) reflects the reading's own logic:
 *   the constraint requires taking a life to restore moral equilibrium, and
 *   this requirement (not alternative penalties) is what drives
 *   extraction—imprisonment cannot substitute for execution under the
 *   retributive frame because the restoration depends on proportionality to
 *   the crime, which only death provides for the most heinous murders. The
 *   sibling readings are separate constraints (deterrence_reading,
 *   abolition_reading), not alternative measurements of this one.
 *
 * KEY AGENTS:
 *   - victims_families: beneficiary of moral restoration, center of the retributive justification
 *   - executed_offender: target bearing the extraction (death penalty)
 *   - state_criminal_justice_apparatus: agenda_setter administering the system
 *   - death_row_inmates: targets bearing the cost of extended uncertainty and execution threat
 *   - abolition_advocates: excluded from the framework's validity claims, structurally opposed
 *   - retributive_justice_advocates: organized beneficiaries defending the framework
 *   - constitutional_courts: observer seats adjudicating legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__retributive_reading, 0.72).
domain_priors:suppression_score(state_execution_authority__retributive_reading, 0.65).
domain_priors:theater_ratio(state_execution_authority__retributive_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__retributive_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__retributive_reading, "State Execution Authority (Retributive Reading)").
narrative_ontology:topic_domain(state_execution_authority__retributive_reading, "criminal_justice/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(state_execution_authority__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__retributive_reading, '8c686725-1ee9-4b69-88db-74e4aa8401c5').
narrative_ontology:cs_kernel_codification('8c686725-1ee9-4b69-88db-74e4aa8401c5', formalized).
narrative_ontology:cs_authority_grounding('8c686725-1ee9-4b69-88db-74e4aa8401c5', lineage).
narrative_ontology:cs_interpretation_layer_present('8c686725-1ee9-4b69-88db-74e4aa8401c5').
narrative_ontology:cs_reading_relation('8c686725-1ee9-4b69-88db-74e4aa8401c5', state_execution_authority__abolition_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c686725-1ee9-4b69-88db-74e4aa8401c5', state_execution_authority__deterrence_reading, coexists_with).
narrative_ontology:cs_axiom('8c686725-1ee9-4b69-88db-74e4aa8401c5', foundational, proportionate_death_restores_moral_balance).
narrative_ontology:cs_axiom_status(proportionate_death_restores_moral_balance, holdable).
narrative_ontology:cs_axiom_grounding('8c686725-1ee9-4b69-88db-74e4aa8401c5', proportionate_death_restores_moral_balance, deontological).
narrative_ontology:cs_axiom('8c686725-1ee9-4b69-88db-74e4aa8401c5', foundational, heinous_crime_justifies_execution).
narrative_ontology:cs_axiom_status(heinous_crime_justifies_execution, holdable).
narrative_ontology:cs_axiom_grounding('8c686725-1ee9-4b69-88db-74e4aa8401c5', heinous_crime_justifies_execution, deontological).
narrative_ontology:cs_reference_frame('8c686725-1ee9-4b69-88db-74e4aa8401c5', classical_proportional_retribution).
narrative_ontology:cs_drift_state('8c686725-1ee9-4b69-88db-74e4aa8401c5', contemporary_empirical_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8c686725-1ee9-4b69-88db-74e4aa8401c5', '').
narrative_ontology:cs_kernel_id(state_execution_authority__retributive_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, victims_families).
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, retributive_moral_order).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, executed_offender).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, death_row_inmates_awaiting_execution).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, retributive_justice_advocates).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, innocent_persons_wrongfully_convicted).
narrative_ontology:constraint_vindicates(state_execution_authority__retributive_reading, proportional_justice_doctrine).
narrative_ontology:constraint_vindicates(state_execution_authority__retributive_reading, moral_restoration_imperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek closure and moral restoration through the execution of those convicted of murdering their loved ones. The retributive framework treats execution as the restoration of moral balance, acknowledging the irreplaceable loss of the victim by imposing an equivalent cost on the offender. Families advocate for capital punishment and participate in execution witnessing; their voice is central to the retributive justification.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, victims_families, beneficiary,
    moderate, biographical, constrained, national).

% Administers the death penalty system: convicts offenders, carries out sentences, enforces appellate review, determines clemency. Claims to execute this authority in service of proportional justice and moral restoration. Maintains the infrastructure, legal standards, and procedural safeguards that justify execution as proportionate punishment rather than arbitrary killing.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, state_criminal_justice_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Bears the ultimate cost: death imposed by state authority. Under the retributive reading, this cost is justified as proportionate to the heinous crime committed. The offender has no meaningful exit; appeal exhaustion leaves execution as the final state act. Legal representation and procedural safeguards exist, but they do not constitute exit—they are procedural fairness within a framework where execution is the determined outcome for capital crimes.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, executed_offender, payer,
    powerless, immediate, trapped, national).

% Occupy a prolonged liminal state: convicted of capital crimes, sentenced to death, in the appellate process. They bear the cost of extended uncertainty, institutional confinement, and the threat of imminent execution. Procedurally they have appeal rights; structurally they are trapped by the finality of conviction and the retributive framework's claim that execution is proportionate and therefore justified.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, death_row_inmates_awaiting_execution, payer,
    powerless, biographical, trapped, national).

% Argue that execution is the only proportionate response to heinous murder; they benefit from the constraint's operation by seeing their moral framework vindicated. They mobilize politically to maintain capital punishment, defend it in public discourse, and participate in advocacy for execution when cases reach the penalty phase.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, retributive_justice_advocates, beneficiary,
    organized, generational, mobile, national).

% Seek to eliminate capital punishment entirely on the grounds that state execution is categorically impermissible. They are excluded from the retributive framework's decision-making (their moral premise is not admitted as legitimate within the constraint) and bear structural opposition from the constraint's operation. Their voice is active but not admitted into the framework's adjudication.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, abolition_advocates, excluded,
    organized, generational, mobile, national).

% In rare cases, those convicted of capital crimes they did not commit are executed. Within the retributive framework, this is a tragic procedural error—the proportionality claim rests on accurate guilt finding. The executed innocent pay the irreversible cost; the framework acknowledges the error but does not invalidate the legitimacy of execution for those truly guilty. This stakeholder represents the structural risk the constraint carries.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, innocent_persons_wrongfully_convicted, payer,
    powerless, immediate, trapped, national).

% Review the constitutionality of capital punishment and the procedures that govern it. They measure whether execution comports with constitutional restraints (cruel-and-unusual-punishment prohibitions, due-process requirements, equal-protection guarantees). Their role is to adjudicate whether the constraint operates within the bounds of legitimate state authority.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Document capital punishment as a human-rights violation under international law. Most advanced democracies and human-rights frameworks treat execution as incompatible with human dignity. They are excluded from the decision-making of U.S. capital systems; their opposition is structural but carries no enforcement power within the national system.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, international_human_rights_bodies, excluded,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__retributive_reading, state_criminal_justice_apparatus).
narrative_ontology:fixing_cost_class(state_execution_authority__retributive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of how a political community responds to severe crime: establishes a common, solemn procedure for adjudicating guilt, determining proportionate response, and carrying out penalties. Creates a rule-governed alternative to private revenge.
% TRANSFER_FUNCTION: Transfers authority over the most severe sanction (death) from the victim's family to the state; in return, the state certifies the response as proportionate and just. The constraint moves accountability from the private sphere to the public sphere and operationalizes the moral claim that heinous crimes justify execution.
% ABSENT_VOICES: Persons opposed to capital punishment on principle (abolition framework) are structurally excluded from the retributive framework's validity claims—their position is not admitted as a legitimate ground for restraint. International human-rights bodies are excluded from national decision-making. Innocent persons who might be misidentified as capital offenders have no voice before conviction.
% DISAPPEARANCE_RATIONALE: If capital punishment disappeared, the state would shift to lifetime imprisonment as the maximum penalty. Victims' families would lose the specific form of moral restoration the retributive framework promises (proportionate death for heinous murder). The state's response to capital crimes would be materially restructured; the moral claim that execution alone restores balance would be replaced by a different penological framework (incapacitation, rehabilitation, or other justifications).
% FOUNDING_PROBLEM: How should a political community respond to heinous murder in a way that restores moral balance, acknowledges the irreplaceable loss of the victim, and maintains the rule of law? The retributive reading answers: through a proportionate response administered by the state—execution for the most severe crimes.
% FOUNDING_PROBLEM_CORROBORATION: Retributive justice theorists and victims' advocates attest the founding problem is live and unsolved by alternatives (See: retributive jurisprudence, e.g., Jeffrie Murphy, Michael Moore; victims' advocacy organizations). Abolition advocates and international human-rights organizations attest that the founding problem is either a false framing (moral restoration cannot be achieved through state killing) or is solved by alternative frameworks (life imprisonment with dignity, restorative justice practices). Constitutional scholars and empirical researchers debate whether the empirical premise of the retributive claim—that execution restores moral balance—is substantiated or is a narrative overlay constructed to justify a policy adopted for other reasons (historical deterrence theories, incapacitation). This external corroboration split reflects the constraint's status as a contested reading, not a settled matter.
narrative_ontology:disappearance_verdict(state_execution_authority__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__retributive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__retributive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_execution_authority__retributive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__retributive_reading, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__retributive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__retributive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__retributive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the retributive reading defines the constraint's necessity in terms that cannot be substituted: only proportionate death restores moral balance for heinous murder. This means the constraint extracts the offender's life as the core function, not a side effect. The measurement trajectory shows slight rise from 0.58 to 0.72 over the interval, reflecting increasing clarity of the framework's extraction function as wrongful-conviction evidence accumulates—the constraint's extractive logic becomes harder to obscure as procedural risks become undeniable. Suppression (0.65) is moderately high because the constraint requires active enforcement to prevent: (1) the victim's family from taking private revenge (the state's monopoly on lawful killing must be defended against vigilante alternatives), (2) abolition advocates from blocking executions politically, and (3) public sentiment from recoiling at the visual reality of state killing (hence the theater ratio's modest rise: execution proceduralism and privacy protections obscure the constraint's extraction). Theater ratio (0.28) is moderate because retributive ritual—the trial, sentencing, appellate process, final witness viewing—plays a real function (certifying guilt and proportionality) but an increasing share of the constraint's maintenance cost goes to managing opposition and securing consent rather than adjudicating guilt. Accessibility collapse (0.58) is moderate: alternatives (life imprisonment, restorative justice, abolition) remain coherent and mobilized, so the constraint does not collapse all alternatives; the retributive frame claims execution is uniquely justified for heinous crimes, but this claim is actively contested.
 *
 * PERSPECTIVAL GAP:
 *   The seat divergence is substantial. From the retributive advocate's seat: the constraint solves a real problem (how to restore moral balance after heinous murder) and operates as genuine coordination of the community's response, with the offender's death as the proportionate cost. From the executed offender's seat: the state imposes death with no consent and no offsetting benefit, making the constraint pure extraction enforced by institutional monopoly. From the abolition advocate's seat: the state claims moral restoration but actually enacts state killing, making the constraint snare (killing disguised as justice). From the victims' family seat: the constraint offers specific, valued restoration (acknowledgment through proportionate response) but may not reduce suffering and may extend it through prolonged appeals. The engine computes these divergences from the structural data (power, exit, beneficiary/victim roles, enforcement need); the authored claim (tangled_rope) asserts that coordination (the certifying response to heinous crime) and extraction (the death of the offender) coexist and require active enforcement to persist.
 *
 * DIRECTIONALITY LOGIC:
 *   The executed offender sits at d=1.0 (full target): bears the extraction (death) with no meaningful exit and no benefit from the constraint's operation. Death-row inmates sit near d=1.0 as well (trapped, bearing execution threat). Victims' families sit near d=0.0 to 0.3 (beneficiary-leaning): they receive the specific form of closure the retributive framework promises, though some families report it does not reduce their grief; the extraction flows FROM the offender TO the state (which executes) in service of their restoration. The state apparatus sits at d=0.3 to 0.5 (beneficiary-leaning toward symmetric): it benefits from the constraint's operation (gains moral legitimacy, maintains social order through certified justice) but bears the cost of administering a controversial, resource-intensive system. Abolition advocates sit at d=0.8 to 0.9 (near-target): they are structurally opposed and excluded from the framework's authority claims; the constraint's persistence operates against their stated position. Constitutional courts sit at d=0.5 (symmetric): they must adjudicate the constraint's legitimacy without colluding with either side, though their review can amplify suppression (by upholding execution) or resistance (by imposing procedural safeguards).
 *
 * MANDATROPHY ANALYSIS:
 *   The retributive reading frames execution as solving the founding problem: how to respond to heinous murder in a way that restores moral balance and maintains rule of law. The reading's claim is that this problem is LIVE and SOLVED by execution. The engine's measurement of whether the constraint's mandatrophy is resolved depends on the mismatch between founding_problem_status and disappearance_verdict: the verdict is 'world_rearranges' (if execution disappeared, response to capital crime would be materially restructured), which suggests the constraint genuinely solves something. However, the founding_problem_status is 'contested'—different communities dispute whether the problem is real (abolitionists say the framing is a false moral narrative), whether it is solved by execution (empiricists point to closure studies showing mixed outcomes), and whether alternative solutions exist (restorative justice, life imprisonment). The measured theater_ratio's slow rise (0.15 to 0.28) over the interval suggests the constraint's functional cost (actually adjudicating guilt and proportionality) is being overtaken by performative cost (maintaining public support, managing dissent, securing victim family participation in ritual). If theater_ratio continues rising and approaches 0.5+, mandatrophy diagnosis would trigger: the constraint would be operating primarily to validate a moral narrative (retribution restores balance) rather than to execute the function the narrative claims to justify.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_measurability,
    'Is moral restoration through execution a measurable, objective fact, or a narrative claim imposed by the state''s authority?',
    'Empirical study: do victims'' families report restored moral equilibrium after executions, compared to other closure mechanisms (life sentences, restorative justice, memorial practices)? Can ''proportionality'' be distinguished from post-hoc justification?',
    'If restoration is subjective or narrative, the high extractiveness reflects the state imposing a moral framework on unwilling participants (executed offenders, abolition advocates) rather than fulfilling an objective function. Reclassification toward snare becomes plausible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_measurability, empirical, 'Whether proportional moral restoration is measurable or a narrative cover for extraction.').

omega_variable(
    innocence_risk_framework_validity,
    'Does the retributive framework remain valid if a fraction of executions will inevitably be of the innocent due to procedural fallibility?',
    'Empirical measurement of wrongful-conviction and wrongful-execution rates; philosophical analysis of whether proportionality to a crime can justify killing someone who did not commit it; jurisdictional comparison of procedural safeguards and innocence-discovery outcomes.',
    'If innocence risk is structurally unavoidable (not merely regrettable), the framework''s core claim—that execution is proportionate to the proven crime—fails for the inevitable innocent cohort. This would classify the constraint as snare (killing the innocent is a structural cost, not an error the framework accommodates). The retributive reading would persist as a claim, but its structural validity would be undermined.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(innocence_risk_framework_validity, empirical, 'Whether the retributive framework can withstand structural wrongful-execution rates.').

omega_variable(
    sibling_reading_coexistence,
    'Can the retributive reading coexist with abolition and deterrence readings within a single legal framework, or do their core premises logically foreclose each other?',
    'Jurisdictional analysis: some countries/periods hold retribution as a live position while others hold abolition; comparative constitutional law shows these readings are held by different parties simultaneously, not logically impossible. The question is whether they can coexist WITHIN ONE JURISDICTION''S framework or only across jurisdictions.',
    'If coexistence is internal to a single jurisdiction (mixed doctrine), the three readings influence each other but do not foreclose. If separation is jurisdictional only, the relations between readings may shift from ''coexists_with'' to ''forecloses'' within a single legal tradition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, conceptual, 'Whether the three kernel readings logically foreclose each other or can coexist.').

omega_variable(
    retributive_identity_lock_suppression,
    'To what extent is the acceptance of capital punishment among retributive advocates identity-locked (fused with their worldview, constitutive of their moral identity) versus structurally contingent?',
    'Ethnographic and interview-based study of retributive advocates'' reasoning: what would cause them to abandon the position? Is it empirical evidence (innocence risk, alternative closure mechanisms), normative reframing (dignity discourse), or would abandonment itself require identity reconstruction?',
    'If identity-locked, suppression operates on the advocate side through internalized commitment, not external enforcement. This raises questions about whether the beneficiary class is truly benefiting or is itself extracted from (locked into a moral position they cannot renegotiate). Classification implication: the suppression may be higher than authored, operating through identity fusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retributive_identity_lock_suppression, empirical, 'Whether retributive belief is identity-locked or empirically contingent.').

omega_variable(
    kernel_reading_identity_retributive_vs_sibling,
    'This constraint instantiates the RETRIBUTIVE reading of state execution authority. The sibling readings (abolition, deterrence) are structurally distinct constraints with different beneficiaries, victims, and ε values. Which relation best captures the logical and structural distance between this reading and its siblings?',
    'Axiomatic analysis: (1) Retribution says execution restores moral balance; Abolition says execution is categorically impermissible; Deterrence says execution prevents future murders. Do any two of these premises logically foreclose the third? (2) Institutional analysis: can a single legal system hold retribution as the official doctrine while abolition advocates operate as legitimate dissent, or does retribution actively suppress abolition as incoherent?',
    'If retribution forecloses abolition (logical contradiction), classify as ''forecloses''. If both coexist in different institutional seats (courts, legislatures, advocacy sectors), classify as ''coexists_with''. If retribution creates institutional pressure that disadvantages abolition (e.g., executions make abolition harder to mobilize), classify as ''influences''. The relation determines the shape of the constraint family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity_retributive_vs_sibling, conceptual, 'Structural relation between this reading and the sibling readings in the kernel contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__retributive_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__retributive_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t5, state_execution_authority__retributive_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement_basis(stat_tr_t5, observed).
narrative_ontology:measurement(stat_tr_t10, state_execution_authority__retributive_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(stat_tr_t10, observed).
narrative_ontology:measurement(stat_tr_t15, state_execution_authority__retributive_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement_basis(stat_tr_t15, observed).
narrative_ontology:measurement(stat_tr_t20, state_execution_authority__retributive_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement_basis(stat_tr_t20, observed).
narrative_ontology:measurement(stat_tr_t25, state_execution_authority__retributive_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement_basis(stat_tr_t25, observed).
narrative_ontology:measurement(stat_tr_t30, state_execution_authority__retributive_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(stat_tr_t30, observed).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__retributive_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(stat_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__retributive_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t5, state_execution_authority__retributive_reading, base_extractiveness, 5, 0.61).
narrative_ontology:measurement_basis(stat_be_t5, observed).
narrative_ontology:measurement(stat_be_t10, state_execution_authority__retributive_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement_basis(stat_be_t10, observed).
narrative_ontology:measurement(stat_be_t15, state_execution_authority__retributive_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement_basis(stat_be_t15, observed).
narrative_ontology:measurement(stat_be_t20, state_execution_authority__retributive_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement_basis(stat_be_t20, observed).
narrative_ontology:measurement(stat_be_t25, state_execution_authority__retributive_reading, base_extractiveness, 25, 0.71).
narrative_ontology:measurement_basis(stat_be_t25, observed).
narrative_ontology:measurement(stat_be_t30, state_execution_authority__retributive_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement_basis(stat_be_t30, observed).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__retributive_reading, base_extractiveness, 40, 0.72).
narrative_ontology:measurement_basis(stat_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__retributive_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(stat_su_t0, observed).
narrative_ontology:measurement(stat_su_t5, state_execution_authority__retributive_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(stat_su_t5, observed).
narrative_ontology:measurement(stat_su_t10, state_execution_authority__retributive_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(stat_su_t10, observed).
narrative_ontology:measurement(stat_su_t15, state_execution_authority__retributive_reading, suppression_requirement, 15, 0.63).
narrative_ontology:measurement_basis(stat_su_t15, observed).
narrative_ontology:measurement(stat_su_t20, state_execution_authority__retributive_reading, suppression_requirement, 20, 0.64).
narrative_ontology:measurement_basis(stat_su_t20, observed).
narrative_ontology:measurement(stat_su_t25, state_execution_authority__retributive_reading, suppression_requirement, 25, 0.65).
narrative_ontology:measurement_basis(stat_su_t25, observed).
narrative_ontology:measurement(stat_su_t30, state_execution_authority__retributive_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement_basis(stat_su_t30, observed).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__retributive_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement_basis(stat_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__retributive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, state_execution_authority__deterrence_reading).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, state_execution_authority__abolition_reading).

% DUAL FORMULATION NOTE:
% State execution authority is a contested kernel with three distinct readings, each constituting a separate constraint story. The retributive reading (this file) treats execution as morally restorative and proportionate; it shares the kernel with deterrence_reading (which grounds legitimacy in crime prevention) and abolition_reading (which treats execution as categorically impermissible). Each reading has a different beneficiary structure, victim set, and ε value. They coexist across different institutional seats (courts, legislatures, advocacy communities) and influence each other through constitutional adjudication and public discourse. Network links trace the constraint family: retributive→[deterrence, abolition]; deterrence→[retributive, abolition]; abolition→[retributive, deterrence].

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_execution_authority__retributive_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
