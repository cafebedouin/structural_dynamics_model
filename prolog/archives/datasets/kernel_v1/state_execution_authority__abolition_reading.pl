% ============================================================================
% CONSTRAINT STORY: state_execution_authority__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: state_execution_authority__abolition_reading
 *   human_readable: State Execution Authority (Abolition Reading): Categorical Impermissibility
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the ABOLITION READING of the contested
 *   kernel 'state_execution_authority': state execution is categorically
 *   impermissible regardless of crime severity or procedural safeguards. This
 *   is ONE OF THREE readings of the same legal/moral kernel. The other
 *   readings — retributive (execution restores proportionate justice) and
 *   deterrence (execution prevents future murders) — are structurally
 *   distinct constraints with different beneficiary/victim sets, different ε
 *   values, and different cs_structure axioms. This story generates ONLY the
 *   abolition reading. The abolition reading treats all executed persons
 *   (guilty and wrongfully condemned alike) as victims. It rejects
 *   retribution and deterrence as legitimate justifications, locating the
 *   constraint's basis in a categorical prohibition on state-inflicted death.
 *   The extractiveness (ε=0.92) reflects that execution is an irreversible
 *   extraction mechanism with no substitutable alternative — life
 *   imprisonment is qualitatively different and cannot offset the absolute
 *   penalty. The suppression (0.88) reflects that this reading suppresses
 *   alternative justifications (retribution, deterrence) as insufficient to
 *   override the categorical principle. The low theater ratio (0.35) reflects
 *   that from the abolition reading's perspective, execution is substantively
 *   extractive (not performative) — the state actually kills people, not
 *   merely theater. The measurements show rising extractiveness and
 *   suppression from 1976–2020, reflecting increasing recognition of wrongful
 *   execution risks and empirical falsification of deterrence claims, which
 *   strengthen the abolition reading's case. The false-summit analysis at
 *   Perspective 6 recognizes that the abolition reading's mountain
 *   perspective naturalizes a contingent normative commitment — the principle
 *   is not a law of physics but a constructed institutional and moral
 *   position held by abolitionist jurisdictions and human rights frameworks.
 *
 * KEY AGENTS:
 *   - Executed Persons (including wrongfully executed): Primary victims (powerless/trapped) — face irreversible extraction with no remediation possible
 *   - Wrongful Execution Victims: Secondary victims (moderate/constrained) — suffer extraction even when innocence is later proven; legal system cannot remediate death
 *   - Constitutional Legitimacy / Justice System Integrity: Tertiary victims (powerless/trapped/abstract) — collective good that cannot organize; bears cost of systemic illegitimacy when wrongful execution occurs
 *   - Abolitionist Coalition: Organized agents (organized/mobile) — human rights groups, legal advocacy organizations, abolitionist nations; perceive the constraint as pure extraction with potential policy exit paths
 *   - Retentionist State Authority: Institutional beneficiary (institutional/arbitrage) — maintains execution as sovereign state power; experiences constraint as coordination (enforcing law, deterring crime, restoring retributive balance); benefits from extraction mechanism
 *   - Justice System Institution (Procedurally Constrained): Inter-institutional perspective (institutional/constrained) — courts and legal infrastructure must implement execution while managing irreversible error risk; faces both coordination pressures and extraction burdens
 *   - Analytical Observer: Civilizational view (analytical/analytical) — evaluates whether categorical impermissibility is discovered moral law or constructed institutional position; assesses false-summit risk
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__abolition_reading, 0.92).
domain_priors:suppression_score(state_execution_authority__abolition_reading, 0.88).
domain_priors:theater_ratio(state_execution_authority__abolition_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__abolition_reading, snare).
narrative_ontology:human_readable(state_execution_authority__abolition_reading, "State Execution Authority (Abolition Reading): Categorical Impermissibility").
narrative_ontology:topic_domain(state_execution_authority__abolition_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__abolition_reading, '5afe7ec5-8f2c-404f-9771-25dd1fa71a34').
narrative_ontology:cs_kernel_codification('5afe7ec5-8f2c-404f-9771-25dd1fa71a34', formalized).
narrative_ontology:cs_authority_grounding('5afe7ec5-8f2c-404f-9771-25dd1fa71a34', extraction).
narrative_ontology:cs_reading_relation('5afe7ec5-8f2c-404f-9771-25dd1fa71a34', state_execution_authority__retributive_reading, forecloses).
narrative_ontology:cs_reading_relation('5afe7ec5-8f2c-404f-9771-25dd1fa71a34', state_execution_authority__deterrence_reading, forecloses).
narrative_ontology:cs_axiom('5afe7ec5-8f2c-404f-9771-25dd1fa71a34', foundational, execution_categorically_impermissible).
narrative_ontology:cs_axiom_status(execution_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('5afe7ec5-8f2c-404f-9771-25dd1fa71a34', execution_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('5afe7ec5-8f2c-404f-9771-25dd1fa71a34', foundational, state_inflicted_death_violates_human_dignity).
narrative_ontology:cs_axiom_status(state_inflicted_death_violates_human_dignity, holdable).
narrative_ontology:cs_axiom_grounding('5afe7ec5-8f2c-404f-9771-25dd1fa71a34', state_inflicted_death_violates_human_dignity, deontological).
narrative_ontology:cs_axiom('5afe7ec5-8f2c-404f-9771-25dd1fa71a34', secondary, wrongful_execution_proves_systemic_illegitimacy).
narrative_ontology:cs_axiom_status(wrongful_execution_proves_systemic_illegitimacy, holdable).
narrative_ontology:cs_axiom_grounding('5afe7ec5-8f2c-404f-9771-25dd1fa71a34', wrongful_execution_proves_systemic_illegitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('5afe7ec5-8f2c-404f-9771-25dd1fa71a34', universal_prohibition_on_state_execution).
narrative_ontology:cs_drift_state('5afe7ec5-8f2c-404f-9771-25dd1fa71a34', contemporary_global_abolitionist_expansion, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('5afe7ec5-8f2c-404f-9771-25dd1fa71a34', '').
narrative_ontology:cs_kernel_id(state_execution_authority__abolition_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, executed_persons_including_guilty).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, wrongful_execution_victims).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, constitutional_legitimacy).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, justice_system_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CONDEMNED PERSON (SNARE) — Faces irreversible extraction (death). No exit options. No alternatives or substitutes exist; life imprisonment is qualitatively different and cannot offset execution. Maximum experienced extraction. Wrongful execution (proven post-mortem) is irreversible systemic failure — the constraint's absolute suppression mechanism prevents even post-hoc remediation.
constraint_indexing:constraint_classification(state_execution_authority__abolition_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE WRONGFUL EXECUTION VICTIM (SNARE) — Even when guilt is later disproven, the extraction is irreversible. The constraint's suppression of due process (forensic innocence discovered years later) means the victim cannot exit or remediate. High extraction; constrained exit (legal system remains operational but cannot reverse the execution).
constraint_indexing:constraint_classification(state_execution_authority__abolition_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ABOLITIONIST COALITION (SNARE) — Organized agents (human rights groups, legal advocacy, nations that have abolished execution) perceive the constraint as pure extraction with no coordination function. They have exit options (legal advocacy, international pressure, migration to abolitionist jurisdictions) and some agency (changing laws). The classification as snare from this perspective reflects that the state extraction mechanism is fundamentally coercive despite potential policy change paths.
constraint_indexing:constraint_classification(state_execution_authority__abolition_reading, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: THE RETENTIONIST STATE AUTHORITY (ROPE) — The institutional actor that maintains execution authority experiences the constraint as coordination (enforcing state sovereignty, deterring capital crimes, restoring retributive balance). From the beneficiary's structural position, execution is framed as justified punishment serving legitimate state functions. Low or negative experienced extraction because the institution benefits from the constraint's existence and legitimacy.
constraint_indexing:constraint_classification(state_execution_authority__abolition_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: THE JUSTICE SYSTEM INSTITUTION / PROCEDURALLY CONSTRAINED READING (TANGLED ROPE) — An alternative institutional perspective: the courts and legal system that must implement execution face both coordination pressures (enforcing law, maintaining legitimacy through procedural fairness) and extraction burdens (irreversible decisions under uncertainty, wrongful execution liability that cannot be remedied). This reading sees the constraint as mixed — genuine coordination requirements (appellate review, due process) layered with inherent extraction (no perfect procedure can eliminate error in irreversible judgments). Constrained exit because legal institutions cannot simply refuse to implement the law without systemic collapse.
constraint_indexing:constraint_classification(state_execution_authority__abolition_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER / ABOLITION READING (MOUNTAIN) — From the civilizational analytical view, the abolition reading treats execution as categorically impermissible — a fixed moral and epistemic boundary that cannot be crossed by severity of crime, quality of procedure, or state justification. The mountain classification reflects the reading's own claim: that this prohibition is a fundamental principle not subject to negotiation or substitution. However, this perspective is subject to false-summit evaluation — the engine will test whether the 'categorical impermissibility' is a genuine moral law or a constructed institutional position that benefits identifiable agents (abolitionist institutions, human rights organizations, abolitionist states).
constraint_indexing:constraint_classification(state_execution_authority__abolition_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__abolition_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(state_execution_authority__abolition_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(state_execution_authority__abolition_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__abolition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.92): Very high. The abolition reading treats execution as categorically impermissible, meaning NO level of crime severity, NO quality of procedure, and NO state justification can legitimize it. This categorical position produces maximal extractiveness because: (1) the extraction is absolute (death), (2) it is irreversible (no substitution or remediation), (3) it has no beneficiaries (retribution/deterrence rejected as justifications), and (4) wrongful execution proves systemic illegitimacy even when procedurally sophisticated. The ε=0.92 reflects the reading's own premise: that this extraction cannot be balanced against competing goods or substituted with alternatives. The measurement trajectory shows rising ε from 1976–2020, reflecting increasing empirical documentation of wrongful executions (DNA exonerations) and meta-analytic falsification of deterrence claims, which strengthen the reading's case that no procedural safeguard can justify execution. Suppression (0.88): Very high. The reading suppresses alternative frameworks (retributive, deterrence) as insufficient to override the categorical principle. The suppression mechanism is epistemic/normative (the reading claims these alternatives are conceptually incoherent or empirically false), not merely coercive. The measurement trajectory shows rising suppression from 1976–2020, reflecting the accumulation of wrongful execution cases and deterrence studies that contradict the retentionist justifications. Theater ratio (0.35): Low. The abolition reading's assessment is that execution is substantively extractive, not performative. The state actually kills people; this is not ritual or symbol but material extraction. The low theater ratio distinguishes this reading from a piton reading (where the extraction mechanism would be largely theatrical). The measurement trajectory shows stable or declining theater ratio, suggesting that execution procedures, despite increasing procedural sophistication (appellate review, DNA testing, expert witnesses), remain substantively extractive rather than becoming performance.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the abolition reading and the retentionist reading is the widest in the kernel contest. The condemned person and analytical observer both see snare/mountain (extraction or categorical prohibition). The retentionist state authority sees rope (coordination and sovereign authority). The wrongful execution victim sees systemic illegitimacy. The abolitionist coalition sees snare with potential policy exit. The justice system institution sees tangled rope (procedural coordination mixed with irreversible error risk). This gap reflects the fundamental disagreement: whether execution can ever be a legitimate state function or whether it is always an illegitimate extraction mechanism, regardless of procedure or justification. The false-summit analysis recognizes that the mountain classification at Perspective 6 naturalizes what is actually a contingent normative commitment — the abolition reading's categorical principle is not discovered law but adopted institutional and moral position.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality framework does not apply in the standard way to this constraint because the abolition reading rejects all beneficiary roles. The retentionist state authority is a declared beneficiary in institutional perspectives (Perspective 4), but the abolition reading rejects the legitimacy of that benefit claim. The engine will compute high directionality (high d) for powerless agents (condemned persons, wrongfully executed) because they are trapped victims with no exit. The organized abolitionist coalition has mobile exit options and organized power, producing lower directionality. The retentionist state authority's directionality override (if declared) would show arbitrage access and institutional power, but the abolition reading treats this as false beneficiary status — the apparent 'benefit' (enforcement of sovereign execution authority) is actually participation in an illegitimate extraction mechanism. The directional asymmetry between perspectives reflects the fundamental reading contest: can execution ever be beneficial to the state (retentionist view) or is it always extraction masked as justice (abolitionist view)?
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_status_of_retribution,
    'Does retribution constitute a legitimate state purpose that can justify execution, or is retribution itself a form of extraction masked as justice?',
    'Philosophical analysis of retributive theory; comparison of retributive justification with other state coercive mechanisms (imprisonment, fines); examination of whether retribution serves victims or primarily serves state legitimacy claims',
    'If retribution is legitimate: retributive_reading and abolition_reading coexist (different normative frames). If retribution is extraction: abolition_reading forecloses retributive_reading (cannot hold both in one framework). This omega resolves the reading_relations choice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_status_of_retribution, conceptual, 'Whether retribution is legitimate state purpose or masked extraction').

omega_variable(
    wrongful_execution_proof_of_systemic_illegitimacy,
    'Does proven wrongful execution demonstrate that the constraint itself is illegitimate, or does it show only that execution procedure is fallible?',
    'Historical examination of wrongful execution reversals; analysis of whether any procedural reform can eliminate execution error risk; comparison with irredeemable errors in other state systems',
    'If wrongful execution proves illegitimacy: abolition_reading''s axiom (execution_categorically_impermissible) is empirically grounded. If fallible procedure is acceptable: the boundary between execution and life imprisonment becomes a question of acceptable error rates rather than categorical principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wrongful_execution_proof_of_systemic_illegitimacy, empirical, 'Whether wrongful execution proves systemic illegitimacy').

omega_variable(
    deterrence_efficacy_and_legitimacy_coupling,
    'Does empirical failure of deterrence efficacy undermine the deterrence_reading''s core justification, or are deterrence claims separable from execution''s legitimacy?',
    'Meta-analysis of capital punishment deterrence studies (Ehrlich 1975 vs later replications); examination of whether deterrence failure logically entails abolition or merely shifts justification to retribution',
    'If deterrence failure entails abolition: abolition_reading forecloses deterrence_reading when deterrence proves ineffective. If deterrence is separable: readings coexist regardless of empirical outcome.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_efficacy_and_legitimacy_coupling, empirical, 'Whether deterrence efficacy affects reading legitimacy').

omega_variable(
    reading_adoption_vs_natural_law_confusion,
    'Is the abolition_reading an adopted normative position (contingent on moral commitments) or a discovered natural law (necessary and universal)?',
    'Examination of abolition adoption history (which nations/traditions have adopted it, when, through what mechanisms); comparison with other deontological principles claimed as universal but historically contested',
    'If adopted position: abolition_reading is a contingent commitment system (cs_structure is appropriate). If natural law: abolition_reading grounds in fundamental moral truth (mountain classification). This omega clarifies the distinction between the abolition_reading constraint and the moral law it claims to embody.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_adoption_vs_natural_law_confusion, conceptual, 'Whether abolition is adopted commitment or discovered natural law').

omega_variable(
    life_imprisonment_as_qualitative_substitute,
    'Is life imprisonment a genuine alternative to execution (making execution substitutable) or is execution categorically distinct (making it non-substitutable)?',
    'Comparison of executed vs life-imprisoned agents'' outcomes (reintegration, family contact, redemption possibility, finality); examination of whether any quantum of imprisonment equals execution''s finality',
    'If substitutable: abolition_reading''s constraint (categorical impermissibility) may be reclassified as tangled_rope (coordination with substitution option). If non-substitutable: snare classification is confirmed (extraction cannot be replaced by alternative penalty).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(life_imprisonment_as_qualitative_substitute, conceptual, 'Whether life imprisonment substitutes for execution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__abolition_reading, 1976, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(execution_theater_1976, state_execution_authority__abolition_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(execution_theater_1990, state_execution_authority__abolition_reading, theater_ratio, 1, 0.37).
narrative_ontology:measurement(execution_theater_2020, state_execution_authority__abolition_reading, theater_ratio, 2, 0.35).

% Extraction over time
narrative_ontology:measurement(execution_extractiveness_1976, state_execution_authority__abolition_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(execution_extractiveness_1990, state_execution_authority__abolition_reading, base_extractiveness, 1, 0.88).
narrative_ontology:measurement(execution_extractiveness_2020, state_execution_authority__abolition_reading, base_extractiveness, 2, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(execution_suppression_1976, state_execution_authority__abolition_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(execution_suppression_1990, state_execution_authority__abolition_reading, suppression_requirement, 1, 0.82).
narrative_ontology:measurement(execution_suppression_2020, state_execution_authority__abolition_reading, suppression_requirement, 2, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__abolition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, state_execution_authority__deterrence_reading).

% DUAL FORMULATION NOTE:
% The 'state_execution_authority' kernel decomposes into three structurally distinct constraints, one for each reading. Each reading generates a different constraint_id with different ε, different beneficiary/victim declarations, and different cs_structure axioms. The three stories are linked via network.affects_constraints because a change in one reading's epistemic or political status (e.g., empirical falsification of deterrence claims) affects the relative legitimacy of the others. Do NOT merge these three stories into one. The reading_relations in cs_structure declare the logical relationships between readings (forecloses, coexists_with, influences); the network.affects_constraints declare the causal/institutional relationships between the resulting constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
