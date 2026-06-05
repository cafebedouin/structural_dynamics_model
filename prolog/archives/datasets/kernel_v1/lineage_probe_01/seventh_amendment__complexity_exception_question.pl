% ============================================================================
% CONSTRAINT STORY: seventh_amendment__complexity_exception_question
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_seventh_amendment_complexity_exception_question, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: seventh_amendment__complexity_exception_question
 *   human_readable: Seventh Amendment Complexity Exception: Judicial Suppression of Jury Domain Through Unacknowledged Summary Judgment Doctrine
 *   domain: constitutional_law/civil_procedure/seventh_amendment
 *
 * SUMMARY:
 *   The Seventh Amendment guarantees the right to jury trial in civil cases
 *   'at common law' — a constraint whose text does not explicitly permit
 *   exceptions. Yet for at least 50 years, federal judges have quietly
 *   invoked 'complexity' as a reason to remove cases from jury consideration
 *   through summary judgment, Daubert gating, and other procedural
 *   substitutes. This constraint is never formally blessed: no Supreme Court
 *   decision explicitly holds that complexity permits denial of jury trial.
 *   Instead, the exception is practiced through incremental doctrinal moves —
 *   each granting summary judgment on the grounds that 'lay jurors cannot be
 *   expected to resolve this dispute' — without ever crystallizing into
 *   acknowledged doctrine. The reading instantiated here focuses on this gap
 *   between acknowledged doctrine (the Seventh Amendment protects jury trial)
 *   and quiet practice (judges suppress jury access in complex cases). The
 *   constraint exhibits all six classification types from different
 *   perspectives: for the lay juror at the margin, it is pure extraction
 *   (snare); for the federal judiciary, it is coordination (rope); for the
 *   formal doctrine itself, it is degraded ritual (piton). The complexity
 *   exception is the extraction mechanism — suppression of jury domain not
 *   through explicit rule, but through an exception argued, never blessed,
 *   and quietly practiced. Extractiveness has increased over the 50-year
 *   interval as litigation has become more technically complex and judicial
 *   confidence in jury incompetence has grown. Theater ratio has increased as
 *   well, reflecting that the performative doctrinal apparatus (briefing
 *   complexity, citing precedent) has become increasingly divorced from the
 *   functional constraint (whether jurors actually get to decide factual
 *   disputes).
 *
 * KEY AGENTS:
 *   - Lay Jurors at Complexity Margin (powerless/trapped) — bear full cost of exclusion from jury service; face unacknowledged suppression of their constitutional domain
 *   - Federal Judiciary (institutional/arbitrage) — primary beneficiary; gains docket control, reduces reversal risk, elevates judicial fact-finding authority; invokes complexity without formal doctrinal blessing
 *   - Parties in Complex Litigation (moderate/constrained) — secondary beneficiaries who gain arguably more reliable fact-finding but lose constitutional right to jury trial; constrained by the exception's gatekeeping logic
 *   - Expert Witnesses and Technical Gatekeepers (institutional/arbitrage) — secondary beneficiaries; gain elevated role in dispute resolution as juries are displaced by expert-driven processes
 *   - Formal Doctrine of Jury Trial Right (piton) — performative; the written rule claims to protect jury trial but is degraded through unacknowledged exception
 *   - Court-Appointed Special Masters and ADR Institutions (organized/mobile) — building alternative pathways that may sunset the complexity exception without constitutional amendment
 *   - Analytical Observer (analytical/analytical) — risks naturalizing judicial gatekeeping as an immutable cognitive limit rather than a political choice about how much to value lay participation in government
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(seventh_amendment__complexity_exception_question, 0.52).
domain_priors:suppression_score(seventh_amendment__complexity_exception_question, 0.68).
domain_priors:theater_ratio(seventh_amendment__complexity_exception_question, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(seventh_amendment__complexity_exception_question, extractiveness, 0.52).
narrative_ontology:constraint_metric(seventh_amendment__complexity_exception_question, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(seventh_amendment__complexity_exception_question, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(seventh_amendment__complexity_exception_question, tangled_rope).
narrative_ontology:human_readable(seventh_amendment__complexity_exception_question, "Seventh Amendment Complexity Exception: Judicial Suppression of Jury Domain Through Unacknowledged Summary Judgment Doctrine").
narrative_ontology:topic_domain(seventh_amendment__complexity_exception_question, "constitutional_law/civil_procedure/seventh_amendment").

domain_priors:requires_active_enforcement(seventh_amendment__complexity_exception_question).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(seventh_amendment__complexity_exception_question, 'c5379ac0-6d10-4291-93c8-ddf9fb23f2c3').
narrative_ontology:cs_kernel_codification('c5379ac0-6d10-4291-93c8-ddf9fb23f2c3', fixed_text).
narrative_ontology:cs_authority_grounding('c5379ac0-6d10-4291-93c8-ddf9fb23f2c3', lineage).
narrative_ontology:cs_interpretation_layer_present('c5379ac0-6d10-4291-93c8-ddf9fb23f2c3').
narrative_ontology:cs_reading_relation('c5379ac0-6d10-4291-93c8-ddf9fb23f2c3', seventh_amendment__historical_test_reading, coexists_with).
narrative_ontology:cs_reading_relation('c5379ac0-6d10-4291-93c8-ddf9fb23f2c3', seventh_amendment__reexamination_clause_reading, influences).
narrative_ontology:cs_axiom('c5379ac0-6d10-4291-93c8-ddf9fb23f2c3', foundational, complexity_permits_jury_exclusion).
narrative_ontology:cs_axiom_status(complexity_permits_jury_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('c5379ac0-6d10-4291-93c8-ddf9fb23f2c3', complexity_permits_jury_exclusion, empirically_contingent).
narrative_ontology:cs_axiom('c5379ac0-6d10-4291-93c8-ddf9fb23f2c3', foundational, unacknowledged_exception_legitimacy).
narrative_ontology:cs_axiom_status(unacknowledged_exception_legitimacy, overridden).
narrative_ontology:cs_axiom_grounding('c5379ac0-6d10-4291-93c8-ddf9fb23f2c3', unacknowledged_exception_legitimacy, deontological).
narrative_ontology:cs_reference_frame('c5379ac0-6d10-4291-93c8-ddf9fb23f2c3', jury_trial_as_written_guarantee).
narrative_ontology:cs_drift_state('c5379ac0-6d10-4291-93c8-ddf9fb23f2c3', contemporary_complexity_gatekeeping, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c5379ac0-6d10-4291-93c8-ddf9fb23f2c3', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(seventh_amendment__complexity_exception_question, seventh_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(seventh_amendment__complexity_exception_question, federal_judiciary).
narrative_ontology:constraint_beneficiary(seventh_amendment__complexity_exception_question, expert_dispute_resolution_gatekeepers).
narrative_ontology:constraint_victim(seventh_amendment__complexity_exception_question, jury_domain_at_complexity_margin).
narrative_ontology:constraint_victim(seventh_amendment__complexity_exception_question, lay_participation_in_fact_finding).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LAY JUROR AT COMPLEXITY MARGIN (SNARE) — Trapped by the gating logic of complexity: if a case is deemed 'too complex' by the judge invoking the unacknowledged exception, the juror has no exit from exclusion. The very right the Seventh Amendment guarantees — jury trial in civil cases at common law — is suppressed through summary judgment on grounds never formally blessed by doctrine. Maximum extraction from the trapped agent with no recourse.
constraint_indexing:constraint_classification(seventh_amendment__complexity_exception_question, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PARTIES IN COMPLEX CIVIL LITIGATION (TANGLED ROPE) — Constrained by complexity doctrine's suppression of jury access: parties in certain domains (patent, securities, antitrust, products liability at scale) face genuine barriers to jury trial. But they also benefit from the alternative: expert-driven fact-finding may be more reliable and efficient than lay juries for technical claims. Extraction coexists with coordination function — the suppression enables a more accurate fact-finding mechanism, even as it strips away constitutional rights.
constraint_indexing:constraint_classification(seventh_amendment__complexity_exception_question, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL JUDICIARY (ROPE) — Benefits from complexity exception: gating difficult cases away from juries reduces appellate reversal risk, streamlines docket management, and elevates judicial fact-finding authority. Judges experience this as coordination — solving the problem of ensuring accurate fact-finding in cases where lay juries cannot function. But the mechanism suppresses a constitutional right without formally acknowledging it does so. The judiciary has arbitrage: they can invoke or not invoke the exception across a shifting domain of 'complexity,' making the constraint instrumentally valuable.
constraint_indexing:constraint_classification(seventh_amendment__complexity_exception_question, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FORMAL DOCTRINE OF JURY TRIAL RIGHT (PITON) — The written rule (Seventh Amendment, Rule 50 summary judgment gate) claims to protect jury trial. But in practice, the doctrine is substantially performative: judges invoke complexity as a reason to grant summary judgment without ever formally holding that complexity creates an exception. The ritual of doctrinal argument persists (parties brief the issue, judges cite precedent) but the functional verification — did the jury actually get to decide?) — is degraded through the unacknowledged exception. Theater ratio is high because the doctrinal apparatus performs a check that does not actually constrain judicial discretion.
constraint_indexing:constraint_classification(seventh_amendment__complexity_exception_question, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: SPECIAL MASTERS AND ADR PATHWAYS (SCAFFOLD) — Organized alternatives to jury trial (court-appointed special masters under Rule 53, private arbitration, mediation) are building parallel fact-finding pathways that may eventually displace jury trials in complex domains without requiring formal constitutional amendment. These alternatives have lower theater — they openly acknowledge they substitute expert judgment for lay judgment — and they offer genuine exit paths (parties can contractually avoid jury trial via arbitration clauses, though public law disputes cannot fully escape). This perspective sees complexity as a temporary structural problem being solved through institutional evolution, not as an immutable feature.
constraint_indexing:constraint_classification(seventh_amendment__complexity_exception_question, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the civilizational perspective, there may be an irreducible limit to lay jury competence at extreme complexity: if litigation involves quantum mechanics, statistical thermodynamics, or neural network architecture, the cognitive demand may exceed what lay fact-finding can reliably accomplish. This perspective sees the complexity exception as reflecting a natural law of human cognition — you cannot ask lay jurors to resolve highly technical factual disputes with the same fidelity that expert judges can. However, this naturalization risks obscuring the political choice embedded in the exception: where to draw the line between 'complex but jury-appropriate' and 'too complex for jurors' is fundamentally a question of how much we value lay participation in government, not a question of immutable cognitive limits.
constraint_indexing:constraint_classification(seventh_amendment__complexity_exception_question, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(seventh_amendment__complexity_exception_question_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(seventh_amendment__complexity_exception_question, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(seventh_amendment__complexity_exception_question, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(seventh_amendment__complexity_exception_question, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(seventh_amendment__complexity_exception_question, TR),
    TR >= 0.70.

:- end_tests(seventh_amendment__complexity_exception_question_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The complexity exception enables judges to suppress jury access without formal doctrinal blessing. The extraction mechanism is the unacknowledged exception — cases are removed from juries through summary judgment on grounds that are argued but never blessed, making the extraction harder to challenge or appeal. The value reflects that this is substantial suppression (many complex civil cases never reach juries) but not maximal (juries still decide cases below the complexity threshold, and the suppression operates at the margin of modern litigation, not across all civil cases). The temporal trajectory (0.28 → 0.42 → 0.52) shows accumulation: as litigation has become more technically complex and judges have become more confident that juries cannot handle modern disputes, the extracted domain has grown. Suppression (0.68): High. The barriers to jury access in complex cases are substantial: the judge's discretionary gating through summary judgment, Daubert gatekeeping of expert testimony, complex jury instructions that may overwhelm lay comprehension, and the absence of any formal acknowledgment that complexity justifies denying jury trial. Suppression is not total — parties can sometimes force complex cases to jury, and appellate reversal is possible — but it is substantial. The temporal trajectory (0.45 → 0.60 → 0.68) shows intensification: enforcement machinery has been built up as federal courts have developed more sophisticated complexity-gating doctrines. Theater ratio (0.64): High. The Seventh Amendment's written protection of jury trial persists (the performative ritual of briefing complexity, citing precedent, applying Rule 50 summary judgment standards) while the functional protection is degraded (judges suppress jury access through an exception never formally blessed). The gap between the written rule and the quiet practice is the theater — the performative apparatus performs a check that does not actually constrain judicial discretion. The temporal trajectory (0.48 → 0.58 → 0.64) shows increasing theater as the gap between doctrine and practice has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — judicial suppression of jury domain through unacknowledged complexity gating — classifies differently depending on the observer's structural position. The lay juror at the complexity margin sees pure extraction (snare) — an unacknowledged suppression of constitutional right with no alternative. The federal judiciary sees coordination (rope) — solving the problem of ensuring reliable fact-finding in technically demanding cases. Parties in complex litigation see mixed coordination and extraction (tangled rope) — the suppression may produce more accurate results, but it strips away constitutional participation. The formal doctrine sees its own degradation (piton) — the written rule claims to protect jury trial, but the performative apparatus does not actually constrain judicial discretion. The alternative dispute resolution ecosystem sees a temporary problem being solved through institutional evolution (scaffold) — special masters and arbitration are building parallel pathways that may sunset the complexity exception without constitutional amendment. The civilizational analytical observer risks naturalizing the exception (mountain) — treating it as an immutable limit on lay cognition — but the structural data reveals it as a political choice: the line between 'complex but jury-appropriate' and 'too complex for jurors' is not set by cognitive science but by judicial confidence estimates, which vary across judges and evolve over time.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from the agents' structural positions: their power, exit options, and relationship to the extraction flow. The lay juror (powerless/trapped) experiences high d toward victimhood: they cannot exit the complexity exception and bear its full cost. The federal judiciary (institutional/arbitrage) experiences low d toward extraction: they benefit from the exception through docket control and appellate risk reduction, and they have arbitrage options (can invoke or invoke the exception depending on their preferences). Parties in complex litigation experience moderate d: they face genuine barriers to jury access but also receive arguably more reliable fact-finding. The complexity exception increases d for all agents who would have jurors — each additional layer of gating (summary judgment, Daubert, jury instruction complexity) raises the suppression and makes the exception harder to escape. The non-acknowledgment of the exception is itself a directionality mechanism: because the exception is never formally blessed, targets cannot appeal it on the grounds that it violates the Seventh Amendment — they must argue within the gating doctrines (summary judgment standards, Daubert reliability) that leave the exception's legitimacy untouched.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the complexity exception is simultaneously a legitimate functional problem-solving mechanism (rope) and an unacknowledged suppression of constitutional rights (snare). The tension is not resolved by choosing one type, but by recognizing that the constraint has real coordination benefits (judges do achieve more reliable fact-finding in complex cases) and real extractive costs (juries are suppressed without formal acknowledgment). The mandatrophy is resolved not by eliminating one reading, but by documenting the perspectival gap: the coordination function (rope) is real from the judiciary's perspective, but it coexists with extraction (snare) from the lay juror's perspective. The extraction mechanism is non-acknowledgment — the exception persists precisely because it is never formally blessed, making it unchallengeable through doctrinal argument.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complexity_threshold_indeterminacy,
    'What threshold of factual complexity triggers the unacknowledged exception? Is it measured by the number of expert witnesses, the technical subject matter, the volume of evidence, the mathematical models required, or the judge''s confidence in jury comprehension?',
    'Systematic analysis of summary judgment grants on complexity grounds: extract stated or implicit complexity criteria from judicial opinions; cross-tabulate with case outcomes, reversals, jury verdict accuracy in comparable non-summary-judgment cases',
    'If threshold is coherent and predictable: complexity exception is rule-like, admitting doctrinal constraint. If threshold is incoherent or driven by judge-specific confidence estimates: complexity exception is a discretionary veto, converting jury trial into a judicial grace. The extractiveness value rises with incoherence (harder to predict or challenge).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complexity_threshold_indeterminacy, empirical, 'Indeterminacy of complexity threshold for summary judgment gating').

omega_variable(
    jury_competence_vs_judicial_bias,
    'Does judicial gatekeeping on complexity grounds improve fact-finding accuracy, or does it substitute judicial bias (including motivated reasoning about what juries can understand) for lay fact-finding error?',
    'Empirical studies comparing jury verdicts in complex cases allowed to proceed to trial vs. summary judgment outcomes in similar cases; measurement of judicial confidence calibration (do judges accurately estimate jury comprehension?); analysis of reversal rates and settlement distributions',
    'If juries outperform: complexity exception is pure extraction (snare). If judges outperform: exception is justified coordination (rope or tangled rope). If performance is equivalent: exception is theatrical suppression (piton).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(jury_competence_vs_judicial_bias, empirical, 'Whether complexity gating improves fact-finding accuracy or substitutes judicial discretion for jury judgment').

omega_variable(
    constitutional_amendment_vs_quiet_practice,
    'Is the complexity exception a legitimate constitutional evolution (the Seventh Amendment adapting to modern litigation), or is it a violation of the Amendment''s text that persists only because it is never formally blessed?',
    'Doctrinal analysis of whether any appellate court has formally held that complexity creates a categorical exception to jury trial rights; comparison with other constitutional amendments that explicitly created exceptions (e.g., Fifth Amendment takings clause with just compensation); analysis of stare decisis chains — is the exception inherited or continuously rederived?',
    'If legitimate evolution: the reading is mischaracterizing a normal doctrinal development as hidden extraction. If violation persisting through non-acknowledgment: the reading correctly identifies a tangled rope (extraction hidden by performative doctrine).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_amendment_vs_quiet_practice, conceptual, 'Whether complexity exception represents legitimate constitutional evolution or unacknowledged violation').

omega_variable(
    lay_jury_vs_expert_fact_finder_legitimacy,
    'Does the Seventh Amendment guarantee lay participation in fact-finding as a structural good (democratic accountability, populist check on elite judgment), or does it guarantee jury trial only instrumentally (as the fact-finding mechanism that worked in 1791)?',
    'Historical analysis of Seventh Amendment ratification debates: what did drafters mean by ''jury trial''? Was lay participation itself the protected value, or merely the procedural form that happened to exist in 1791? Comparative analysis: do other democracies with strong jury traditions extend jury rights to complex civil cases, or do they permit expert substitution?',
    'If lay participation is the structural good: complexity exception is a violation (suppresses the value the Amendment protects). If jury trial is merely instrumental: complexity exception is legitimate substitution (achieves the same fact-finding goal through better means).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_jury_vs_expert_fact_finder_legitimacy, conceptual, 'Whether Seventh Amendment protects lay participation structurally or juries only instrumentally as 1791 fact-finding form').

omega_variable(
    federal_common_law_fact_finding_authority,
    'Does the complexity exception rest on an implicit doctrine that federal judges have authority to develop a federal common law of fact-finding standards (determining when lay juries are inadequate), or is such authority foreclosed by the Seventh Amendment''s text?',
    'Doctrinal history of summary judgment doctrine: did courts explicitly claim authority to develop federal fact-finding standards, or did the complexity exception emerge through incremental grants without doctrinal acknowledgment? Analysis of Rule 50 evolution and amendment history.',
    'If implicit common-law authority is real: complexity exception reflects a structural doctrinal claim about judicial power that should be debated and potentially constrained. If no such authority is claimed: the exception is hidden, and the constraint is a snare. Either way, non-acknowledgment is the extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_common_law_fact_finding_authority, conceptual, 'Whether courts claim implicit federal authority to develop fact-finding standards excluding juries').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(seventh_amendment__complexity_exception_question, 1960, 2010).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seventh_amend_complexity_theater_1960s, seventh_amendment__complexity_exception_question, theater_ratio, 0, 0.48).
narrative_ontology:measurement(seventh_amend_complexity_theater_1985, seventh_amendment__complexity_exception_question, theater_ratio, 25, 0.58).
narrative_ontology:measurement(seventh_amend_complexity_theater_2010, seventh_amendment__complexity_exception_question, theater_ratio, 50, 0.64).

% Extraction over time
narrative_ontology:measurement(seventh_amend_complexity_extractiveness_1960s, seventh_amendment__complexity_exception_question, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(seventh_amend_complexity_extractiveness_1985, seventh_amendment__complexity_exception_question, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(seventh_amend_complexity_extractiveness_2010, seventh_amendment__complexity_exception_question, base_extractiveness, 50, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(seventh_amend_complexity_suppression_1960s, seventh_amendment__complexity_exception_question, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(seventh_amend_complexity_suppression_1985, seventh_amendment__complexity_exception_question, suppression_requirement, 25, 0.6).
narrative_ontology:measurement(seventh_amend_complexity_suppression_2010, seventh_amendment__complexity_exception_question, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(seventh_amendment__complexity_exception_question, enforcement_mechanism).
narrative_ontology:affects_constraint(seventh_amendment__complexity_exception_question, seventh_amendment__historical_test_reading).
narrative_ontology:affects_constraint(seventh_amendment__complexity_exception_question, seventh_amendment__reexamination_clause_reading).
narrative_ontology:affects_constraint(seventh_amendment__complexity_exception_question, daubert_gatekeeping_expertise_suppression).
narrative_ontology:affects_constraint(seventh_amendment__complexity_exception_question, federal_civil_procedure_summary_judgment_expansion).

% DUAL FORMULATION NOTE:
% The complexity exception reading is part of a three-constraint family modeling the Seventh Amendment kernel: historical_test_reading models the 1791-common-law-suit categorization; reexamination_clause_reading models appellate review protection; complexity_exception_question models suppression of jury domain at trial through unacknowledged gating. Each reading has its own epsilon and classification. The complexity exception affects downstream constraints about expert gatekeeping and summary judgment expansion because those procedural mechanisms operationalize the suppression this reading identifies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(seventh_amendment__complexity_exception_question, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
