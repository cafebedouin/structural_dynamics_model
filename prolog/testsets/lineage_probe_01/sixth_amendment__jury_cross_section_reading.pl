% ============================================================================
% CONSTRAINT STORY: sixth_amendment__jury_cross_section_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sixth_amendment__jury_cross_section_reading, []).

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
 *   constraint_id: sixth_amendment__jury_cross_section_reading
 *   human_readable: Sixth Amendment Jury Cross-Section Legitimacy (Sixth Amendment Reading)
 *   domain: constitutional_law/sixth_amendment/jury_right
 *
 * SUMMARY:
 *   The jury cross-section reading of the Sixth Amendment establishes that
 *   jury legitimacy rests on whom the jury could contain — that verdicts
 *   derive their constitutional authority from being rendered by
 *   representatives of the community whole, not by any particular composition
 *   achieved in a specific trial. This reading instantiates a constraint
 *   centered on representation and demographic inclusion: the suppression of
 *   systematic exclusion of racial and gender groups from jury service. The
 *   constraint operates as a tangled rope — it coordinates legitimate jury
 *   trials (the core function) while the exclusion of demographic groups
 *   extracts legitimacy from their absence and concentrates verdict power
 *   among the included. The tension is between the coordination mechanism
 *   (jury trial as democratic check on state power) and the extraction
 *   mechanism (engineered jury pools that bias verdicts by construction). The
 *   measurement trajectory shows extractiveness declining over the 50-year
 *   interval as enforcement of cross-section principles strengthened (Batson
 *   v. Kentucky 1986, subsequent case law); suppression has slowly declined
 *   as voir dire challenge patterns became subject to statistical review; and
 *   theater has slightly increased as the performative aspect of selection
 *   (diversity statements, impartiality instructions) has grown even as
 *   structural exclusion persists.
 *
 * KEY AGENTS:
 *   - Systematically Excluded Demographic Group (powerless/trapped): faces structural suppression from jury service; extraction of legitimacy through their absence
 *   - Defendant from Excluded Community (moderate/constrained): benefits from jury-right as coordination mechanism but suffers extraction because actual jury is not cross-sectional
 *   - Prosecutor's Office (institutional/arbitrage): benefits from jury pool bias while experiencing cross-section mandate as legitimacy coordination
 *   - Trial Court Judge (institutional/constrained): coordinates trials under conflicting incentives between enforcement and political exposure
 *   - Jury Selection Ritual (institutional/arbitrage): performs legitimacy through voir dire theater while demographic homogeneity persists structurally
 *   - Analytical Observer (analytical/analytical): risks naturalizing contingent institutional exclusion as inherent structural feature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sixth_amendment__jury_cross_section_reading, 0.38).
domain_priors:suppression_score(sixth_amendment__jury_cross_section_reading, 0.62).
domain_priors:theater_ratio(sixth_amendment__jury_cross_section_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sixth_amendment__jury_cross_section_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(sixth_amendment__jury_cross_section_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sixth_amendment__jury_cross_section_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sixth_amendment__jury_cross_section_reading, tangled_rope).
narrative_ontology:human_readable(sixth_amendment__jury_cross_section_reading, "Sixth Amendment Jury Cross-Section Legitimacy (Sixth Amendment Reading)").
narrative_ontology:topic_domain(sixth_amendment__jury_cross_section_reading, "constitutional_law/sixth_amendment/jury_right").

domain_priors:requires_active_enforcement(sixth_amendment__jury_cross_section_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sixth_amendment__jury_cross_section_reading, 'd2e175f9-83ca-44cf-8c6c-e9bb9540922a').
narrative_ontology:cs_kernel_codification('d2e175f9-83ca-44cf-8c6c-e9bb9540922a', formalized).
narrative_ontology:cs_authority_grounding('d2e175f9-83ca-44cf-8c6c-e9bb9540922a', lineage).
narrative_ontology:cs_interpretation_layer_present('d2e175f9-83ca-44cf-8c6c-e9bb9540922a').
narrative_ontology:cs_reading_relation('d2e175f9-83ca-44cf-8c6c-e9bb9540922a', sixth_amendment__confrontation_crawford_reading, coexists_with).
narrative_ontology:cs_reading_relation('d2e175f9-83ca-44cf-8c6c-e9bb9540922a', sixth_amendment__gideon_counsel_revolution, coexists_with).
narrative_ontology:cs_axiom('d2e175f9-83ca-44cf-8c6c-e9bb9540922a', foundational, jury_legitimacy_rests_on_representation_potential).
narrative_ontology:cs_axiom_status(jury_legitimacy_rests_on_representation_potential, holdable).
narrative_ontology:cs_axiom_grounding('d2e175f9-83ca-44cf-8c6c-e9bb9540922a', jury_legitimacy_rests_on_representation_potential, deontological).
narrative_ontology:cs_axiom('d2e175f9-83ca-44cf-8c6c-e9bb9540922a', foundational, systematic_demographic_exclusion_delegitimizes_verdicts).
narrative_ontology:cs_axiom_status(systematic_demographic_exclusion_delegitimizes_verdicts, holdable).
narrative_ontology:cs_axiom_grounding('d2e175f9-83ca-44cf-8c6c-e9bb9540922a', systematic_demographic_exclusion_delegitimizes_verdicts, deontological).
narrative_ontology:cs_reference_frame('d2e175f9-83ca-44cf-8c6c-e9bb9540922a', community_representation_jury_ideal).
narrative_ontology:cs_drift_state('d2e175f9-83ca-44cf-8c6c-e9bb9540922a', contemporary_batson_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d2e175f9-83ca-44cf-8c6c-e9bb9540922a', '').
narrative_ontology:cs_kernel_id(sixth_amendment__jury_cross_section_reading, sixth_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sixth_amendment__jury_cross_section_reading, defendants_from_excluded_communities).
narrative_ontology:constraint_beneficiary(sixth_amendment__jury_cross_section_reading, community_representation).
narrative_ontology:constraint_victim(sixth_amendment__jury_cross_section_reading, excluded_demographic_groups).
narrative_ontology:constraint_victim(sixth_amendment__jury_cross_section_reading, verdict_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED DEMOGRAPHIC (SNARE) — The excluded group (historically African Americans, women, Latinos in many jurisdictions) faces complete suppression from jury service. Their exclusion is structural, documented in voir dire patterns, and operates with minimal apparent enforcement cost to the system. The constraint extracts legitimacy from their absence while denying them voice. No exit option from the demographic category itself. Experienced as pure extraction.
constraint_indexing:constraint_classification(sixth_amendment__jury_cross_section_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEFENDANT FROM EXCLUDED COMMUNITY (TANGLED ROPE) — The defendant from an excluded group benefits from the jury-right itself (the constraint as coordination mechanism: right to trial by jury) but suffers extraction because the actual jury drawn does not represent the community whole. The coordination function (jury trials prevent arbitrary conviction) persists, but it is asymmetrically applied — the defendant gets a jury, but not the cross-sectional one promised. Constrained exit: cannot refuse jury trial without strategic cost, but the trial system itself is constrained option.
constraint_indexing:constraint_classification(sixth_amendment__jury_cross_section_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROSECUTOR'S OFFICE (ROPE) — Prosecutors benefit from jury pools that are demonstrably biased toward conviction when juries exclude communities aligned with defendants. The constraint (cross-section mandate) provides coordination benefit through perceived legitimacy of the trial process, reducing appeal rates and maintaining public confidence in convictions. The prosecutor experiences this as a coordination mechanism: legitimate juries mean stable convictions. Arbitrage exit: can seek removal of problematic jurors within the system; prosecution is not threatened by cross-section principle itself, only by its enforcement.
constraint_indexing:constraint_classification(sixth_amendment__jury_cross_section_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TRIAL COURT JUDGE (TANGLED ROPE) — The judge coordinates the trial process (coordination function) but operates under conflicting incentives. Enforcing cross-section requirements (striking prosecutor's racially motivated voir dire challenges) creates appellate risk and political exposure in some jurisdictions, while failing to enforce it delegitimizes verdicts. The judge benefits from orderly trials and stable convictions (institutional coordination), but bears the cost of enforcement activism. Constrained: cannot exit the duty to conduct trials, but can adjust enforcement intensity.
constraint_indexing:constraint_classification(sixth_amendment__jury_cross_section_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: VOIR DIRE SELECTION RITUAL (PITON) — The jury selection process performs cross-section legitimacy without delivering it. Voir dire questions ask jurors about bias, judges instruct on impartiality, attorneys make diversity arguments — the theater of neutrality. But structural patterns show demographic homogeneity persists: challenge patterns are documented, proxy criteria for race are deployed, venires are already skewed before selection begins. The ritual maintains the legitimacy claim while the functional mechanism atrophies. Theater ratio (0.48) reflects that some genuine bias-checking happens alongside performative conformity.
constraint_indexing:constraint_classification(sixth_amendment__jury_cross_section_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZED VIEW (MOUNTAIN) — From a civilizational horizon, the jury-cross-section constraint could appear as an immutable structural feature of democratic justice: any legitimate verdict requires representation of the community, and representation is inherently difficult to achieve at scale. This perspective risks naturalizing what is actually a contingent institutional arrangement — the suppression of particular groups and the theater surrounding selection. The constraint's ε and suppression values will trigger false-summit detection, revealing that the naturalization conceals structural extraction.
constraint_indexing:constraint_classification(sixth_amendment__jury_cross_section_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sixth_amendment__jury_cross_section_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sixth_amendment__jury_cross_section_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sixth_amendment__jury_cross_section_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sixth_amendment__jury_cross_section_reading, TR),
    TR >= 0.70.

:- end_tests(sixth_amendment__jury_cross_section_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts legitimacy from excluded groups by rendering their absence invisible within the trial process — verdicts appear to come from 'the community' while systematically excluding parts of it. However, this extraction is not maximal (not 0.66+) because the constraint also genuinely provides jury trials (coordination benefit) and has enforcement mechanisms (Batson challenges, statistical review of venires) that reduce but do not eliminate bias. The declining trajectory reflects increasing enforcement. Suppression (0.62): Moderate-high. Systematic exclusion mechanisms operate structurally: biased venire sources, prosecutor challenges with proxy criteria, implicit bias in judicial questioning. However, suppression is not total (not 0.85+) because the right to jury trial exists, some demographic diversity appears in actual juries, and enforcement tools exist (though underutilized). Theater ratio (0.48): Moderate. Voir dire contains both genuine bias-checking (jurors asked about prejudice, judges instruct on impartiality) and performative elements (diversity statements, 'fair process' ritual). The constraint operates with mixed genuine and theatrical function — not predominantly performative (piton would require 0.70+) but containing significant theater.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap lies between the beneficiary and victim positions. Prosecutors and the trial system itself experience jury trials as coordination (legitimate verdicts stabilize convictions, reduce appeals). Excluded demographic groups experience the same system as pure extraction (their exclusion renders verdicts illegitimate while appearing neutral). The tangled rope middle ground belongs to defendants from excluded communities and judges: they experience genuine coordination (trials provide venue for defense, checks on state power) alongside real extraction (jury pools designed to convict them). The piton perspective captures the performative layer: jury selection rituals perform fairness (the constraint is 'working') while structural exclusion persists. The mountain perspective risks treating demographic exclusion as a natural feature of jury assembly (representation is inherently difficult), concealing what is actually a contingent institutional practice (challenge rules, venire sources, implicit bias patterns) that could be reformed.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to the constraint. Excluded demographic groups are victims with trapped exit (no way to change demographic category; cannot voluntarily exclude themselves from juries without strategic cost). The prosecutor is a beneficiary with arbitrage exit (can work within jury system, seeking jurors; can also pursue non-jury dispositions). The defendant from an excluded community is both victim and beneficiary — benefits from jury-right itself but victimized by non-cross-sectional juries; exit is constrained (cannot refuse jury trial without strategic defeat). The judge is an institutional actor constrained by conflicting incentives (enforcement creates risk, non-enforcement delegitimizes). The trial court judge's d value reflects the constraint's internal contradiction: judges nominally represent the law (beneficiary perspective) but structurally sit between prosecutors (who seek biased juries) and defendants (who seek fair juries). The analytical observer's mountain perspective risks naturalizing the suppression mechanism, triggering false-summit detection because beneficiaries (defendants, community representation) are declared.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through doctrinal differentiation. The jury cross-section reading does NOT claim that all jury procedures are equally legitimate, nor that intent-based exclusion is the same as outcome-based exclusion. Instead, it grounds legitimacy in representation potential — whom the jury could contain — which is distinct from whom it actually contains in any particular trial. This distinction allows the constraint to be simultaneously tangled rope (coordination function genuine, extraction real and measurable) and to guide enforcement: cross-section is not a guarantee of outcome but a requirement that the pool be representative before selection begins. The constraint prevents the false equivalence between 'jury trial by neutral procedures' (which might mask bias) and 'jury trial by community representatives' (which requires demographic inclusion). The mandatrophy is resolved by the reading's core axiom: legitimacy rests on representation potential, not outcome manipulation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voir_dire_proxy_criteria_detectability,
    'Can race-based voir dire exclusion be definitively distinguished from facially neutral proxy criteria (e.g., zip code, employment, educational background) in trial practice?',
    'Statistical analysis of challenge patterns within and across racial groups; comparison of articulated vs. proven reasons for juror removal; analysis of whether proxy criteria correlate with demographic exclusion in specific court jurisdictions',
    'If proxies are undetectable from the bench: enforcement of cross-section becomes nearly impossible (extractiveness rises, suppression mechanism hardens). If proxies are detectable through statistical review: enforcement becomes tractable (extractiveness can be reduced).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voir_dire_proxy_criteria_detectability, empirical, 'Detectability of race-based exclusion via facially neutral proxy criteria').

omega_variable(
    implicit_bias_vs_intentional_exclusion,
    'Does the cross-section right require proof of intentional discrimination (Batson rule), or does structural underrepresentation alone trigger the constraint''s protective function?',
    'Doctrinal review of Batson application and its critiques; empirical measurement of whether intent-based enforcement (current doctrine) reduces demographic homogeneity compared to outcome-based enforcement (hypothetical alternative)',
    'If intent-required: suppression remains high (intent is hard to prove), extractiveness persists (biased juries remain legitimate if bias is inadvertent). If outcome-based: suppression mechanism shifts (prosecutors must affirmatively maintain diversity), extractiveness drops (legitimacy requirement is objective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implicit_bias_vs_intentional_exclusion, empirical, 'Intent requirement vs. outcome-based enforcement in cross-section doctrine').

omega_variable(
    reading_kernel_contestation,
    'Is the jury cross-section principle a distinct foundational commitment (drawing from community whole = legitimacy), or is it derivative of a broader sixth-amendment guarantee of fair procedure (subsumable under confrontation or counsel rights)?',
    'Historical analysis of jury trial doctrine; examination of whether cross-section protections cohere with or against confrontation (Crawford) and counsel (Gideon) doctrines; review of whether cross-section cases cite confrontation/counsel as grounding or treat them as independent bases',
    'If independent: this reading forecloses versions of confrontation/counsel that treat procedure as text-bound rather than representation-grounded. If derivative: this reading coexists with confrontation/counsel but is subordinate to them, and cross-section enforcement becomes conditional on those other rights maturing first.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contestation, conceptual, 'Whether cross-section principle is foundational or derivative within sixth-amendment doctrine').

omega_variable(
    empirical_basis_jury_legitimacy,
    'Does actual community representation (cross-sectional juries) causally produce higher legitimacy verdicts compared to demographically skewed juries, or is the legitimacy claim purely aspirational?',
    'Comparative analysis of appeal rates, public confidence surveys, and acquittal patterns for cross-sectional vs. non-cross-sectional juries in comparable case types; study of whether excluded groups perceive verdict legitimacy differently when juries are demographically representative',
    'If representation causally improves legitimacy: the coordination function is real (tangled rope classification holds, extraction reduction is measurable). If legitimacy is aspirational but not realized: the constraint is mostly performative (piton classification gains strength, theater is the primary function).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(empirical_basis_jury_legitimacy, empirical, 'Causal relationship between jury cross-section and verdict legitimacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sixth_amendment__jury_cross_section_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jury_xsect_tr_t0, sixth_amendment__jury_cross_section_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(jury_xsect_tr_t25, sixth_amendment__jury_cross_section_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(jury_xsect_tr_t50, sixth_amendment__jury_cross_section_reading, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(jury_xsect_be_t0, sixth_amendment__jury_cross_section_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(jury_xsect_be_t25, sixth_amendment__jury_cross_section_reading, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(jury_xsect_be_t50, sixth_amendment__jury_cross_section_reading, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(jury_xsect_su_t0, sixth_amendment__jury_cross_section_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(jury_xsect_su_t25, sixth_amendment__jury_cross_section_reading, suppression_requirement, 25, 0.65).
narrative_ontology:measurement(jury_xsect_su_t50, sixth_amendment__jury_cross_section_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sixth_amendment__jury_cross_section_reading, identity_coordination).
narrative_ontology:affects_constraint(sixth_amendment__jury_cross_section_reading, sixth_amendment__confrontation_crawford_reading).
narrative_ontology:affects_constraint(sixth_amendment__jury_cross_section_reading, sixth_amendment__gideon_counsel_revolution).

% DUAL FORMULATION NOTE:
% The sixth_amendment kernel contains three structurally distinct constraint readings. The jury_cross_section_reading focuses on representation and demographic inclusion in jury composition. The sibling readings focus on confrontation (witness reliability) and counsel (defense representation). Each reading has its own ε value, suppression mechanism, and beneficiary/victim structure. They are linked in network.affects_constraints to model doctrinal interdependence: weak cross-section enforcement reduces the effectiveness of confrontation and counsel protections.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sixth_amendment__jury_cross_section_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
