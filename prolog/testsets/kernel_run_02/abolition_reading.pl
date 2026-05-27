% ============================================================================
% CONSTRAINT STORY: abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abolition_reading, []).

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
    narrative_ontology:omega_variable/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: abolition_reading
 *   human_readable: State Killing as Categorical Violation of Human Dignity (Abolition Reading)
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   The abolition reading instantiates one normative interpretation of the
 *   contested kernel 'state killing legitimacy.' This reading asserts that
 *   state killing categorically violates human dignity regardless of the
 *   condemned person's crimes, desert, or any consequentialist justification.
 *   From this reading's perspective, the condemned person is a rights-bearer
 *   whose dignity cannot be forfeited; state authority to execute is itself
 *   the victim (a power that should not exist); and the entire apparatus of
 *   capital punishment is a snare extracting ultimate penalty from the
 *   powerless with no exit and no alternative legitimacy frame. The abolition
 *   reading coexists with retributive and deterrence readings, each grounded
 *   in different foundational axioms about desert, dignity, and state
 *   legitimacy. The constraint's extractiveness (0.68) reflects that the
 *   formal power to execute is substantial, suppression (0.75) reflects the
 *   condemned person's total lack of exit options, and theater ratio (0.58)
 *   reflects that capital punishment combines genuine enforcement (execution
 *   happens) with significant performative elements (trial procedure,
 *   clemency rituals, solemnity of sentencing).
 *
 * KEY AGENTS:
 *   - Condemned Persons: Primary victims (powerless/trapped) — bear ultimate extraction with no exit; structurally and biologically prevented from escape or appeal to alternative authority
 *   - Demographic Groups at Disproportionate Execution Risk: Primary victims (powerless/trapped) — Black Americans, poor defendants, mentally disabled defendants face cumulative suppression through system inequality and generational harm
 *   - Human Dignity Principle: Victim (powerless/analytical) — abstract principle violated by state killing; has no institutional advocate within retentionist legal frameworks
 *   - State Authority / Retentionist Institutional Actors: Beneficiary and Constrained Agent (powerful/mobile) — derive legitimacy and enforcement power from capital punishment authority; also constrained by abolition movement and international human rights norms
 *   - Abolition Advocates & Legal Reformers: Constrained agents (moderate/constrained) — structurally oppose the constraint but depend on its existence for mobilization; face career risk and resource barriers
 *   - Analytical Observer: Sees full structure (analytical/analytical) — risks naturalizing state killing as inevitable or discovering it as contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abolition_reading, 0.68).
domain_priors:suppression_score(abolition_reading, 0.75).
domain_priors:theater_ratio(abolition_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abolition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(abolition_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(abolition_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abolition_reading, snare).
narrative_ontology:human_readable(abolition_reading, "State Killing as Categorical Violation of Human Dignity (Abolition Reading)").
narrative_ontology:topic_domain(abolition_reading, "criminal_justice/political_philosophy/legal_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abolition_reading, 'eb2addd5-d1bc-46eb-8f52-db779a8f170e').
narrative_ontology:cs_created_at('eb2addd5-d1bc-46eb-8f52-db779a8f170e', '').
narrative_ontology:cs_kernel_codification('eb2addd5-d1bc-46eb-8f52-db779a8f170e', formalized).
narrative_ontology:cs_authority_grounding('eb2addd5-d1bc-46eb-8f52-db779a8f170e', lineage).
narrative_ontology:cs_interpretation_layer_present('eb2addd5-d1bc-46eb-8f52-db779a8f170e').
narrative_ontology:cs_kernel_id(abolition_reading, state_killing_legitimacy).
narrative_ontology:cs_reading_relation('eb2addd5-d1bc-46eb-8f52-db779a8f170e', retributive_reading, forecloses).
narrative_ontology:cs_reading_relation('eb2addd5-d1bc-46eb-8f52-db779a8f170e', deterrence_reading, coexists_with).
narrative_ontology:cs_axiom('eb2addd5-d1bc-46eb-8f52-db779a8f170e', foundational, human_dignity_inalienable).
narrative_ontology:cs_axiom_status(human_dignity_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('eb2addd5-d1bc-46eb-8f52-db779a8f170e', human_dignity_inalienable, deontological).
narrative_ontology:cs_axiom('eb2addd5-d1bc-46eb-8f52-db779a8f170e', foundational, state_killing_categorically_impermissible).
narrative_ontology:cs_axiom_status(state_killing_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('eb2addd5-d1bc-46eb-8f52-db779a8f170e', state_killing_categorically_impermissible, deontological).
narrative_ontology:cs_reference_frame('eb2addd5-d1bc-46eb-8f52-db779a8f170e', universal_human_dignity_framework).
narrative_ontology:cs_drift_state('eb2addd5-d1bc-46eb-8f52-db779a8f170e', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_victim(abolition_reading, condemned_persons).
narrative_ontology:constraint_victim(abolition_reading, human_dignity_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONDEMNED PERSON (SNARE) — Trapped by state apparatus with no exit option. The constraint extracts life itself. No escape, no appeal to alternative authority, no capacity to organize. The condemned person is the binding mechanism's primary target. Maximum suppression and extraction experienced at individual biographical level.
constraint_indexing:constraint_classification(abolition_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMMUNITIES BEARING EXECUTION RISK (SNARE) — Certain demographic groups (Black Americans, poor defendants, mentally disabled defendants) face disproportionate execution risk. Structurally trapped within criminal justice system with no exit. Generational harm accumulates as execution risk becomes internalized into community experience. High suppression through both explicit barriers (defense resource scarcity) and implicit channeling (prosecutorial discretion, jury composition).
constraint_indexing:constraint_classification(abolition_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: ABOLITION ADVOCATES & LEGAL REFORMERS (TANGLED ROPE) — Face significant barriers (career risk in conservative jurisdictions, resource constraints, counter-organization by retentionist forces) but can exit through relocation, field switching, or rhetorical framing shifts. The constraint both oppresses (forces choice between career and principle) and coordinates their identity as a movement. They benefit from the constraint's existence by maintaining a mobilizing injustice. Mixed experience — constrained but not trapped, bearing costs but also deriving agency from opposition.
constraint_indexing:constraint_classification(abolition_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: RETENTIONIST STATE AUTHORITY (ROPE) — State institutions that maintain capital punishment see the constraint as coordination: executing criminals solemnly demonstrates state sovereignty and moral authority to enforce ultimate punishment. From the retentionist view, the mechanism is legitimately coordinating justice, deterrence, and closure. This perspective can exit (abolition is globally spreading, alternative punishment systems exist) but chooses retention for reasons of perceived legitimacy and authority performance. The extraction this perspective experiences is the constraint on state power — loss of the ultimate sanction.
constraint_indexing:constraint_classification(abolition_reading, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, capital punishment might appear as immutable: it is the natural consequence of crimes deserving death; human dignity itself is a disputed philosophical claim; the state's right to execute is derived from natural law or divine authority. This view treats state killing authority as a timeless feature of legitimate governance. However, the structural data contradicts this — the constraint is socially constructed, benefits identifiable actors (state authority, prosecutors, retentionist constituencies), and suppresses alternatives (abolition frameworks, dignity-based law). The engine will detect this as a false summit.
constraint_indexing:constraint_classification(abolition_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / ABOLITION READING (SNARE) — From the abolition reading's epistemic frame, state killing is a structural extraction mechanism masquerading as justice. The condemned person has no exit; the state has all enforcement power; suppression operates through both explicit legal channels (procedural limitations, evidentiary standards favoring conviction) and implicit ones (defense resource inequality, jury bias). The universal/civilizational scope reveals that abolition is not a local reform but a claim about human dignity as such. The constraint extracts ultimate penalty from the powerless while naturalizing state killing as legitimate. Classification differs from natural law view because this perspective rejects the naturalness framing and identifies concrete beneficiaries (state authority, prosecutors, retentionist constituencies) and concrete victims (condemned persons, demographic groups bearing disproportionate risk).
constraint_indexing:constraint_classification(abolition_reading, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abolition_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(abolition_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(abolition_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(abolition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): The state's formal power to execute is substantial — it extracts life itself, the maximal penalty. However, extractiveness is not maximal (0.95) because: (1) execution is not actually administered in all retentionist jurisdictions (practice varies widely), (2) appellate processes and clemency create occasional exit routes (rare but real), (3) the condemned person's structural position is powerless/trapped but the mechanism relies on legal procedure rather than pure physical force. The measurement trajectory from 0.55 to 0.68 reflects increasing institutionalization of capital punishment authority over the constraint's interval — formalization and procedural entrenchment gradually raised the extractive capacity. Suppression (0.75): The condemned person has virtually no exit — no physical escape, no legal alternative, no appeal to different authority, no arbitrage opportunity. Suppression is very high but not maximal (0.95) because appellate courts can theoretically grant relief and clemency exists as a rare escape valve. The suppression reflects both explicit barriers (limited appellate grounds, procedural bars) and implicit channeling (resource inequality in defense, prosecutorial discretion). Theater ratio (0.58): Capital punishment combines genuine enforcement (people are executed) with significant performative content (solemnity of trials, clemency rituals, jury deliberation theater, the aesthetic staging of justice). As the interval progresses, the theater ratio increases as procedural formalization adds ritualistic layers while execution rates decline in many retentionist jurisdictions, suggesting the theatrical component is becoming more prominent relative to the enforcement component.
 *
 * PERSPECTIVAL GAP:
 *   The abolished reading and retentionist reading experience radically different classifications of the same structural fact. From the abolition reading's perspective (snare), state killing is pure extraction with no legitimate coordination function — the condemned person has no dignity claim the state must respect, and the state's authority to execute is fundamentally illegitimate. From the retentionist reading (rope or tangled rope), state killing coordinates justice and closure — the condemned person has forfeited dignity through crime deserving death, and the state's authority to execute is a legitimate expression of moral authority. The analytical observer risks naturally law framing (mountain) — treating capital punishment as timeless or inevitably derived from state legitimacy — but the structural data reveals this as a false summit: the constraint benefits identifiable actors (state authority, prosecutors, retentionist constituencies), suppresses alternatives (abolition frameworks, human rights law), and persists despite growing international abolition norms (evidence of contingency, not necessity). The perspectival gap is unbridgeable within the same normative framework — the two readings make contradictory claims about human dignity's inalienability.
 *
 * DIRECTIONALITY LOGIC:
 *   The abolition reading locates all extraction in the state's power to execute and all victimization in the condemned person and dignity principle. No beneficiaries are declared in this reading's frame because the reading asserts that the constraint benefits no one legitimately — state authority benefits by maintaining coercive power, but coercive power is itself the violation. From the retentionist reading's frame, the condemned person would be correctly classified as a beneficiary of state closure and justice (their crime receives punishment proportional to its severity), and the state would see the constraint as coordinating justice rather than extracting penalty. But in the abolition reading, these actors are flipped: the condemned person is victim (denied dignity), the state is constrained agent (power it should not have), and beneficiaries are the institutional actors maintaining execution authority (prosecutors, state officials). The engine will compute directionality from the absence of declared beneficiaries, interpreting this as maximum victim concentration — d approaches 1.0 (full target) for the condemned person.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING / SNARE CLASSIFICATION: The abolition reading resolves into Snare (extractiveness 0.68, suppression 0.75, no beneficiaries declared, high victim concentration, no exit options) from the abolition frame. The retributive reading would resolve into Tangled Rope (proportional punishment as coordination, desert as legitimate extraction asymmetry). The deterrence reading would resolve into Rope or Tangled Rope (execution as communicative coordination about consequences). These are not different observations of the same constraint — they are different normative readings of the contested kernel 'state killing legitimacy,' each with its own ε value, each internally consistent, each grounded in different foundational axioms. The mandatrophy is resolved by recognizing that classification depends on which reading's axioms you accept. If human dignity is inalienable (abolition axiom), the constraint is snare. If desert-proportionality is binding (retributive axiom), the constraint is tangled rope. If behavioral consequences are primary (deterrence axiom), the constraint is rope/tangled rope. No single type is 'correct' independent of the reading chosen.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    human_dignity_grounding,
    'Is human dignity an intrinsic, inalienable property that state killing categorically violates, or is dignity a contingent social attribution that states can legitimately override in extreme circumstances?',
    'Philosophical argumentation; historical analysis of dignity concepts across cultures and legal traditions; empirical study of whether death penalty actually enhances or degrades social dignity norms over time',
    'If intrinsic/inalienable: abolition reading is correct and state killing is categorically prohibited. If contingent/overridable: state killing may be legitimate in specific contexts (retributive or deterrent), and the abolition reading becomes one normative position among competing ones rather than a truth claim about the constraint structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(human_dignity_grounding, conceptual, 'Whether human dignity is intrinsic or contingent').

omega_variable(
    desert_and_consequentialism_foreclosure,
    'Does the abolition reading''s categorical prohibition of state killing logically foreclose the retributive and deterrence readings, or are these readings about different normative principles that can coexist?',
    'Logical analysis of foundational axioms in each reading; assessment of whether a single political framework could coherently hold abolition (dignity is inalienable) AND retribution (criminals deserve proportional punishment including death) simultaneously',
    'If foreclosed: the three readings are mutually exclusive; only one can be true within a coherent framework. If coexist: the readings are different normative commitments held by different political coalitions, and classification depends on the authority structure''s choice, not on logical necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(desert_and_consequentialism_foreclosure, conceptual, 'Whether abolition reading logically forecloses retributive and deterrence readings').

omega_variable(
    execution_suppression_empirical_adequacy,
    'Does suppression (0.75) accurately capture the structural barriers to exit and alternatives for condemned persons, or does it underestimate the total coercive machinery (guilt determination, sentencing discretion, appellate limits)?',
    'Structural analysis of legal system''s barriers to defense resources, appeal routes, and alternative punishments; empirical data on execution rates vs appellate reversal rates; study of defendants'' subjective and objective capacity to contest conviction and sentence',
    'If suppression is underestimated: the constraint is closer to 0.85+ (mountain-level suppression), indicating mechanical inevitability. If adequate: suppression at 0.75 reflects high but not absolute constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(execution_suppression_empirical_adequacy, empirical, 'Whether suppression metric captures total coercive machinery').

omega_variable(
    extractiveness_as_written_vs_practiced,
    'Does extractiveness (0.68) measure the formal legal power to execute, or should it measure the gap between formal authority and actually-used execution (death penalty rate varies 100x across US jurisdictions)?',
    'Decomposition into two constraints: (1) state_killing_legal_authority (ε ≈ 0.68, formal power), (2) state_killing_actual_practice (ε varies 0.15-0.60 by jurisdiction). Test whether single ε value is appropriate or whether observable-dependence requires constraint family.',
    'If practice-dependent: abolition reading applies to formal authority (ε-invariant) but practice varies by jurisdiction''s actual usage. If ε-invariant: the constraint is the formal power to kill, independent of usage frequency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractiveness_as_written_vs_practiced, empirical, 'Whether extractiveness should measure formal authority or actual practice frequency').

omega_variable(
    reading_vs_natural_law_ambiguity,
    'Is this constraint a reading of the contested kernel ''state killing legitimacy,'' or is it an attempt to establish the abolition position as natural law (immutable prohibition)?',
    'Clarification of epistemic status: if this story claims the abolition principle is itself a constraint on legitimate authority (true by necessity of human dignity), it is FSM-candidate false summit masquerading as reading. If it is a reading instantiating one normative position within a contested kernel, the kernel-reading framing is correct.',
    'If natural law claim: the story should be reframed as declaring the dignity principle as mountain-level immutability, triggering FSM evaluation. If reading: kernel-reading framing is appropriate, and the ambiguity itself is the omega.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_natural_law_ambiguity, conceptual, 'Whether constraint is a reading or an attempted natural law declaration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abolition_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aboli_tr_t0, abolition_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(aboli_tr_t20, abolition_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(aboli_tr_t40, abolition_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(aboli_be_t0, abolition_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(aboli_be_t20, abolition_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(aboli_be_t40, abolition_reading, base_extractiveness, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abolition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(abolition_reading, retributive_reading).
narrative_ontology:affects_constraint(abolition_reading, deterrence_reading).

% DUAL FORMULATION NOTE:
% The abolition reading is one reading of the contested kernel 'state killing legitimacy.' The kernel is shared across three constraint stories: abolition_reading, retributive_reading, deterrence_reading. Each story instantiates a different normative reading with its own ε value, its own beneficiary/victim structure, and its own foundational axioms. The ε values differ substantially because the readings define victimization and extraction differently: abolition reading treats the condemned person as victim (ε=0.68); retributive reading treats the criminal act as victim and proportional punishment as coordination (ε ≈ 0.40); deterrence reading treats behavioral outcomes as the constraint target (ε ≈ 0.35). All three stories must be linked via network.affects_constraints as they compete for institutional authority over the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
