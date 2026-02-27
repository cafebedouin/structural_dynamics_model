% ============================================================================
% CONSTRAINT STORY: us_legal_standard_reasonable_doubt
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_legal_standard_reasonable_doubt, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_legal_standard_reasonable_doubt
 *   human_readable: The US Legal Standard of 'Guilt Beyond a Reasonable Doubt' in Political Prosecutions
 *   domain: political/legal
 *
 * SUMMARY:
 *   The application of the 'guilty beyond a reasonable doubt' standard to
 *   high-profile political prosecutions creates a structural tension between
 *   the coordination function of the standard (protecting due process for all
 *   defendants) and its extraction potential (enabling weaponization of
 *   prosecutorial power against political opponents). The constraint operates
 *   in a polarized political environment where jurors' party affiliation
 *   predicts verdict with high correlation, where media coverage
 *   predetermines public conviction before trial, and where the standard's
 *   protective function (impartiality, high evidentiary burden) is degraded
 *   by political context. This constraint exhibits Tangled Rope structure:
 *   genuine coordination function (the standard is real protection and is
 *   essential to institutional legitimacy) coexists with asymmetric
 *   extraction (prosecutorial discretion enables selective targeting, jury
 *   predisposition enables politically motivated conviction). The theater
 *   ratio has increased over the interval as jury selection becomes
 *   increasingly sophisticated at identifying political bias rather than
 *   eliminating it, and as appellate review becomes more ritualistic and less
 *   functional at correcting political verdicts. Base extractiveness has
 *   increased as the political polarization context deepens, making the
 *   standard's protective function harder to maintain. The constraint
 *   demonstrates how a legitimate institutional mechanism (high evidentiary
 *   standards) can be transformed into an extraction tool when applied in a
 *   degraded institutional context (polarized politics, selective
 *   prosecution, partisan jury pools).
 *
 * KEY AGENTS:
 *   - Political Defendant: Primary victim (powerless/trapped) — faces criminal jeopardy in politicized environment with predetermined conviction probability
 *   - Prosecutorial Authority: Primary beneficiary (institutional/arbitrage) — gains institutional legitimacy for prosecution and can exercise discretion with legal cover
 *   - Jury Pool: Secondary participant (moderate/constrained) — required to apply standard impartially but their verdict predetermined by political identity
 *   - Democratic Institutions/Rule of Law: Hybrid beneficiary-victim (powerful/mobile) — the standard is essential coordination mechanism but vulnerable to weaponization
 *   - Appellate System: Secondary institutional actor (institutional/arbitrage) — maintains performative oversight that rarely corrects political bias
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable legal principle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_legal_standard_reasonable_doubt, 0.58).
domain_priors:suppression_score(us_legal_standard_reasonable_doubt, 0.62).
domain_priors:theater_ratio(us_legal_standard_reasonable_doubt, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_legal_standard_reasonable_doubt, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_legal_standard_reasonable_doubt, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(us_legal_standard_reasonable_doubt, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_legal_standard_reasonable_doubt, tangled_rope).
narrative_ontology:human_readable(us_legal_standard_reasonable_doubt, "The US Legal Standard of 'Guilt Beyond a Reasonable Doubt' in Political Prosecutions").
narrative_ontology:topic_domain(us_legal_standard_reasonable_doubt, "political/legal").

domain_priors:requires_active_enforcement(us_legal_standard_reasonable_doubt).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_legal_standard_reasonable_doubt, prosecutorial_authority).
narrative_ontology:constraint_beneficiary(us_legal_standard_reasonable_doubt, institutional_justice_system).
narrative_ontology:constraint_victim(us_legal_standard_reasonable_doubt, political_defendants).
narrative_ontology:constraint_victim(us_legal_standard_reasonable_doubt, due_process_reliability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POLITICAL DEFENDANT (SNARE) — Defendant facing prosecution in a charged political environment lacks meaningful exit options. Standard legal defenses are available in theory but constrained by: polarized media landscape that predetermined conviction before trial, selective charging based on political affiliation, resource asymmetry between state and defendant, and reputational destruction regardless of acquittal. The defendant experiences maximum extraction: legal jeopardy is real, suppression of alternative narrative pathways is severe, and the defendant cannot exit the constraint. Maximum d ≈ 0.95.
constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: JURY POOL (TANGLED ROPE) — Jurors are required to serve and make determinations under 'reasonable doubt' standard, but their verdict is simultaneously a legal verdict and a political statement about the defendant's party and character. Jurors benefit from the rule of law (coordination function: impartial adjudication protects all defendants). But jurors also experience extraction: exposure to contempt of court charges for political expression, social ostracism based on verdict, and polarization that makes true impartiality impossible. Constrained exit (can request exemption but faces pressure to serve). Moderate experienced extraction — both coordination and coercion present.
constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROSECUTORIAL AUTHORITY (ROPE) — Benefits directly from enforcement of the 'guilt beyond reasonable doubt' standard: maintains institutional legitimacy, demonstrates commitment to rule of law (coordination function), and exercises prosecutorial discretion with legal cover. Prosecutors experience the constraint as pure coordination: the standard constrains them to meet a high evidentiary burden, but this is a coordination benefit — it legitimizes prosecutions and prevents arbitrary conviction. Arbitrage exit: prosecutors can choose not to prosecute or can negotiate plea bargains. Experienced extraction is minimal or negative (net beneficiary). d ≈ 0.10.
constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DEMOCRATIC INSTITUTIONS / RULE OF LAW (TANGLED ROPE) — The 'reasonable doubt' standard is essential coordination mechanism: equal application of high evidentiary standards protects all citizens from state tyranny (coordination function). But when applied to political prosecutions in polarized contexts, the standard simultaneously enables weaponization: prosecutors use legitimate investigative power to target political opponents, and jurors vote along predetermined party lines under cover of 'reasonable doubt.' Institutions experience both genuine coordination benefit (the standard is real protection) and extraction vulnerability (politicized application degrades the standard's legitimacy). Active enforcement required: judges must manage jury selection, evidentiary standards, and sentencing. Mobile exit available in principle (prosecutorial discretion, impeachment) but politically constrained. d ≈ 0.55.
constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: APPELLATE SYSTEM (PITON) — The appellate process and jury instruction mechanisms are largely performative in political cases. Judges instruct juries on 'reasonable doubt' and 'impartiality' as though these are neutral technical standards, but in polarized political prosecutions, juror predisposition overwhelms instruction. Appeals courts defer to jury verdicts unless instructional error is egregious, creating a theater of appellate review that rarely overturns political convictions. The system persists through institutional inertia (all trials must go through appeals) despite low functional capacity to correct political bias. Theater ratio 0.68 reflects this gap between ritualized appellate oversight and actual correction of politicized verdicts.
constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the civilizational perspective, the 'reasonable doubt' standard is an immutable principle of due process: all legal systems with legitimacy claims must require high evidentiary standards for conviction. This is a natural law of jurisprudence — the functional requirement for a justice system to exist. However, the structural data contradicts the mountain classification. The constraint operates only in specific political contexts where polarization degrades the standard's function. The standard itself is immutable; its application in politicized contexts is contingent. The false summit reveals naturalization of contingent institutional practices (jury selection, evidentiary interpretation, sentencing disparities) as immutable principles.
constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_legal_standard_reasonable_doubt_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_legal_standard_reasonable_doubt, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_legal_standard_reasonable_doubt, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_legal_standard_reasonable_doubt, TR),
    TR >= 0.70.

:- end_tests(us_legal_standard_reasonable_doubt_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The political defendant bears significant legal jeopardy, reputational destruction, and constrained exit options. But the extraction is not total (0.70+) because: (1) the defendant has legitimate legal defenses available in theory, (2) acquittal is possible even if improbable, and (3) the prosecution must meet an actual evidentiary standard (not complete fiction). The extractiveness reflects the combination of real legal jeopardy plus severe suppression of alternative narratives. Suppression (0.62): Moderate-high. Barriers to fair adjudication include: polarized media landscape that predetermines public conviction, jury selection processes that fail to eliminate partisan bias, selective prosecution of political opponents, and reputational destruction regardless of acquittal outcome. But suppression is not total (0.80+) because independent fact-finding and appeals processes exist and retain some functional capacity. Theater ratio (0.68): Moderate-high, reflecting the gap between the ritualized application of 'reasonable doubt' instruction and jurors' actual ability to apply it impartially. The increase over the interval reflects intensifying politicization and sophistication of jury selection methods. Claimed type (Tangled Rope): The constraint requires active enforcement (judges must manage jury selection and evidentiary standards), includes genuine coordination function (the standard is essential to institutional legitimacy), and exhibits asymmetric extraction (political defendants bear disproportionate costs). All three gates are satisfied.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits profound perspectival divergence. The prosecutorial authority sees Rope — legitimate enforcement of high evidentiary standards that legitimizes prosecution and constrains arbitrary conviction. The political defendant sees Snare — predetermined conviction in a polarized environment with no meaningful exit. The jury pool sees Tangled Rope — required to apply an impartial standard while their verdict is simultaneously a political statement. Democratic institutions see Tangled Rope — the standard is essential coordination mechanism but vulnerable to weaponization in polarized contexts. The appellate system sees Piton — judicial oversight that is largely performative and fails to correct political bias. The analytical observer risks seeing Mountain (the standard is an immutable principle of justice), but the structural data reveals this as naturalization: the standard itself is immutable, but its application in polarized contexts exhibits contingent institutional failures (jury selection, prosecutorial discretion, media influence). The perspectival gaps reveal that 'reasonable doubt' is not a univocal constraint but rather a structural site where coordination mechanisms and extraction mechanisms intersect.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by each agent's structural position relative to the extraction flow. The political defendant as primary victim experiences high d ≈ 0.92 (trapped powerless agent bearing maximum extraction). The prosecutorial authority as primary beneficiary experiences low d ≈ 0.08 (institutional agent with arbitrage exit benefiting from the standard). The jury pool as constrained moderate agent experiences d ≈ 0.58 (required to apply standard impartially but politically biased). Democratic institutions as powerful agents with mobile exit experience d ≈ 0.52 (both beneficiary of coordination function and vulnerable to weaponization). The appellate system as institutional beneficiary experiences d ≈ 0.12 (maintenance of legitimacy through performative oversight). The analytical observer at civilizational context experiences d ≈ 0.72 (external view of the standard's application without structural stakes). The directionality derivation reveals that the constraint exhibits maximum perspectival differentiation — six agents with d values ranging from 0.08 to 0.92 — indicating high structural complexity and vulnerability to misclassification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by disaggregating the standard from its application context. 'Reasonable doubt' as an abstract legal principle is a Rope (pure coordination): all legitimate justice systems require high evidentiary standards for conviction. But 'reasonable doubt' as applied in polarized political prosecutions is a Tangled Rope (coordination + asymmetric extraction): the standard legitimizes prosecution while prosecutorial discretion enables selective targeting. The mandatrophy is resolved by recognizing that the coordination function is genuine (the standard is essential) but the extraction risk is real (selective prosecution and jury bias exploit the standard's application). The constraint's theater ratio increasing over time indicates that the extraction component is growing relative to the coordination component — the standard is becoming increasingly deployed as a tool for legitimizing predetermined verdicts rather than as a genuine protection mechanism. The analytical observer's false summit (natural law view) naturalizes this contingent institutional arrangement, concealing the asymmetry between the standard's theoretical function and its politicized application. True mandatrophy resolution requires maintaining the distinction: the standard is real coordination mechanism, the application is contingent institutional failure, and the two cannot be collapsed into either 'pure coordination' (false minimization) or 'pure extraction' (false maximization).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reasonable_doubt_measurement_threshold,
    'What level of evidentiary burden constitutes ''beyond a reasonable doubt'' when political polarization makes jurors'' prior beliefs on the defendant''s guilt determinative?',
    'Comparative analysis of jury questionnaires, verdict deliberation records, and post-verdict juror interviews across politically charged vs. non-political trials; statistical modeling of relationship between juror party affiliation and verdict in high-profile political cases',
    'If standard is still functional: ''reasonable doubt'' can be meaningfully applied even in politicized contexts (Rope from more perspectives). If standard is degraded: ''reasonable doubt'' becomes a cover for predetermined verdicts based on juror politics (Snare from more perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reasonable_doubt_measurement_threshold, empirical, 'Whether ''reasonable doubt'' standard maintains functional meaning in politicized contexts').

omega_variable(
    selective_prosecution_asymmetry,
    'Are prosecutions of political figures applied symmetrically across party lines, or does partisan selection constitute a structural extraction mechanism that weaponizes the legal standard?',
    'Longitudinal analysis of prosecutorial charging decisions by party affiliation of target; comparison of evidentiary thresholds applied to similarly-situated defendants of different parties; audit of DOJ investigative resource allocation',
    'If symmetric: ''reasonable doubt'' standard is neutral coordination mechanism (Rope from institutional perspective). If asymmetric: standard becomes tool for extracting political advantage (Snare/Tangled Rope from defendant perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_prosecution_asymmetry, empirical, 'Whether prosecutorial charging decisions exhibit partisan asymmetry').

omega_variable(
    jury_predisposition_vs_evidence_weight,
    'In politicized trials, what proportion of verdict variance is explained by evidentiary weight vs. juror''s pre-existing political identity?',
    'Jury simulation experiments; analysis of verdict consistency across legally similar cases with different defendants'' political affiliations; correlation of juror responses to jury questionnaire items on politics vs. final verdict',
    'If evidence dominates: ''reasonable doubt'' standard is real constraint on jury behavior (Rope/Tangled Rope). If party identity dominates: standard has become pure theater with extraction outcome predetermined (Snare/Piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jury_predisposition_vs_evidence_weight, empirical, 'Relative weight of evidentiary burden vs. juror political predisposition in verdict formation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_legal_standard_reasonable_doubt, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doubt_tr_t0, us_legal_standard_reasonable_doubt, theater_ratio, 0, 0.45).
narrative_ontology:measurement(doubt_tr_t3, us_legal_standard_reasonable_doubt, theater_ratio, 3, 0.58).
narrative_ontology:measurement(doubt_tr_t5, us_legal_standard_reasonable_doubt, theater_ratio, 5, 0.68).

% Extraction over time
narrative_ontology:measurement(doubt_be_t0, us_legal_standard_reasonable_doubt, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(doubt_be_t3, us_legal_standard_reasonable_doubt, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(doubt_be_t5, us_legal_standard_reasonable_doubt, base_extractiveness, 5, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_legal_standard_reasonable_doubt, enforcement_mechanism).
narrative_ontology:affects_constraint(us_legal_standard_reasonable_doubt, prosecutorial_discretion_asymmetry).
narrative_ontology:affects_constraint(us_legal_standard_reasonable_doubt, jury_selection_polarization).
narrative_ontology:affects_constraint(us_legal_standard_reasonable_doubt, media_trial_contamination).

% DUAL FORMULATION NOTE:
% This constraint decomposes into three structurally distinct claims: (1) The 'reasonable doubt' standard as an abstract legal principle (Rope, ε≈0.05, Mountain certainty), (2) The jury's ability to apply the standard impartially in polarized contexts (Tangled Rope, ε≈0.58, current story), (3) Prosecutorial selection of targets based on political affiliation (Snare, ε≈0.72, separate story required). The current story focuses on the gap between the standard's theoretical function and its politicized application. Upstream constraints on prosecutorial discretion and jury selection polarization feed into this constraint's extractiveness value. The standard itself remains immutable; its degradation is downstream of these institutional failures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_legal_standard_reasonable_doubt, powerful, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
