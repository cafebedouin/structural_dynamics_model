% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__protective_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__protective_scaffold_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__protective_scaffold_reading
 *   human_readable: Qualified Immunity Doctrine â Protective Scaffold Reading
 *   domain: constitutional/law_enforcement/civil_rights
 *
 * SUMMARY:
 *   This constraint story instantiates the protective_scaffold_reading of the
 *   qualified_immunity_doctrine kernel. The natural-language label 'qualified
 *   immunity' conflates multiple structurally distinct claims. This reading
 *   treats the doctrine as a necessary judicial invention that protects law
 *   enforcement officers from debilitating personal liability, thereby
 *   preserving vigorous discretionary enforcement. Under this reading,
 *   officers are the primary beneficiaries, while constitutional violation
 *   survivors bear the extracted costs of remedy denial. The claim/metric
 *   independence is maintained: the reading frames itself as protective
 *   coordination while the authored metrics acknowledge moderate extraction
 *   and active enforcement, yielding a tangled_rope structural signature
 *   rather than the pure rope the protective framing might suggest. Sibling
 *   readingsâaccountability_void_reading and
 *   constitutional_fidelity_readingâare modeled as separate constraints in
 *   the same kernel family per the Îµ-invariance principle.
 *
 * KEY AGENTS:
 *   - law_enforcement_officers: Primary beneficiary (moderate/constrained) â receive immunity from personal liability and early dismissal of suits
 *   - constitutional_violation_survivors: Primary target/payer (powerless/trapped) â bear remedy denial and litigation costs
 *   - federal_judiciary: Agenda-setter (institutional/analytical) â creates and administers the clearly-established-law test
 *   - civil_rights_litigators: Observer (organized/constrained) â must operate within the doctrine's boundaries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__protective_scaffold_reading, 0.62).
domain_priors:suppression_score(qualified_immunity_doctrine__protective_scaffold_reading, 0.7).
domain_priors:theater_ratio(qualified_immunity_doctrine__protective_scaffold_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__protective_scaffold_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__protective_scaffold_reading, tangled_rope).
narrative_ontology:human_readable(qualified_immunity_doctrine__protective_scaffold_reading, "Qualified Immunity Doctrine â Protective Scaffold Reading").
narrative_ontology:topic_domain(qualified_immunity_doctrine__protective_scaffold_reading, "constitutional/law_enforcement/civil_rights").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__protective_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__protective_scaffold_reading, 'b7edaa69-b3c3-410c-a0ae-b424c1d38a15').
narrative_ontology:cs_kernel_codification('b7edaa69-b3c3-410c-a0ae-b424c1d38a15', formalized).
narrative_ontology:cs_authority_grounding('b7edaa69-b3c3-410c-a0ae-b424c1d38a15', lineage).
narrative_ontology:cs_interpretation_layer_present('b7edaa69-b3c3-410c-a0ae-b424c1d38a15').
narrative_ontology:cs_reading_relation('b7edaa69-b3c3-410c-a0ae-b424c1d38a15', qualified_immunity_doctrine__accountability_void_reading, coexists_with).
narrative_ontology:cs_reading_relation('b7edaa69-b3c3-410c-a0ae-b424c1d38a15', qualified_immunity_doctrine__constitutional_fidelity_reading, coexists_with).
narrative_ontology:cs_axiom('b7edaa69-b3c3-410c-a0ae-b424c1d38a15', foundational, qualified_immunity_necessary_for_effective_policing).
narrative_ontology:cs_axiom_status(qualified_immunity_necessary_for_effective_policing, holdable).
narrative_ontology:cs_axiom_grounding('b7edaa69-b3c3-410c-a0ae-b424c1d38a15', qualified_immunity_necessary_for_effective_policing, empirically_contingent).
narrative_ontology:cs_axiom('b7edaa69-b3c3-410c-a0ae-b424c1d38a15', foundational, common_law_officer_immunity_judicially_warranted).
narrative_ontology:cs_axiom_status(common_law_officer_immunity_judicially_warranted, holdable).
narrative_ontology:cs_axiom_grounding('b7edaa69-b3c3-410c-a0ae-b424c1d38a15', common_law_officer_immunity_judicially_warranted, conventional).
narrative_ontology:cs_reference_frame('b7edaa69-b3c3-410c-a0ae-b424c1d38a15', officer_protection_common_law).
narrative_ontology:cs_drift_state('b7edaa69-b3c3-410c-a0ae-b424c1d38a15', contemporary_accountability_movement, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('b7edaa69-b3c3-410c-a0ae-b424c1d38a15', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__protective_scaffold_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__protective_scaffold_reading, law_enforcement_officers).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__protective_scaffold_reading, constitutional_violation_survivors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Government officials exercising discretionary authority who, when sued personally for alleged constitutional violations, can seek dismissal based on whether the right was clearly established at the time of conduct. This shields them from discovery, trial, and personal financial liability. They do not individually choose whether the doctrine applies; it attaches as a matter of law and is defended by courts and their employers.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, law_enforcement_officers, beneficiary,
    moderate, biographical, constrained, national).

% Individuals who allege unconstitutional conduct by government officials. They bear the burden of proving not only that a violation occurred, but that the right was clearly established in prior case law. When courts find the right was not clearly established, claims are dismissed before discovery, leaving them without damages or a developed record of the violation. State remedies are often inadequate, and no alternative federal path exists for personal-capacity damages.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, constitutional_violation_survivors, payer,
    powerless, biographical, trapped, national).

% Federal courts created and administer the qualified immunity doctrine through the clearly-established-law test. They decide whether a constitutional right was sufficiently defined at the time of conduct to put an officer on notice, controlling whether cases proceed to discovery or trial. The doctrine has been expanded and entrenched through decades of common-law development without congressional authorization.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Attorneys representing plaintiffs in constitutional tort actions under 42 U.S.C. Â§ 1983. They must navigate the clearly-established-law test at the pleading stage, frequently seeing cases dismissed before obtaining discovery. Their ability to develop novel constitutional claims is constrained by the requirement to locate closely analogous prior precedent in the same jurisdiction.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__protective_scaffold_reading, civil_rights_litigators, observer,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__protective_scaffold_reading, law_enforcement_officers).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__protective_scaffold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protecting government officials from the burden and distraction of defending against insubstantial or bad-faith litigation when they perform discretionary functions, thereby preserving vigorous law enforcement and public administration without fear of personal financial ruin from suits that do not allege clearly established violations.
% TRANSFER_FUNCTION: Moves the costs of constitutional violations from the individual officer and the public treasury to the victim, by foreclosing personal-capacity damages remedies when courts hold the constitutional right was not clearly established at the time of the conduct.
% ABSENT_VOICES: Constitutional violation survivors whose claims are dismissed on qualified immunity grounds before discovery; state legislators in jurisdictions that have not enacted statutory alternatives; municipal insurers who bear indemnification costs but are not consulted in immunity calculus; reform advocates who would abolish the doctrine but lack a seat in the judicial forum where it is administered.
% DISAPPEARANCE_RATIONALE: If qualified immunity vanished overnight, officers would face personal liability exposure for constitutional violations; municipalities would likely expand indemnification budgets or insurance coverage; more suits would reach discovery and trial; judicial dockets would shift significantly; and law enforcement training and policy would adapt to liability risk. The protective function would disappear and the federal remedy landscape would reorganize around personal-capacity damages.
% FOUNDING_PROBLEM: The burden of defending against insubstantial and frivolous lawsuits distracts and deters public officials from vigorous performance of their duties, and the social costs of over-deterrence in law enforcement exceed the costs of occasional unremedied constitutional violations.
% FOUNDING_PROBLEM_CORROBORATION: Law enforcement unions and police management attest the problem is live, citing officer recruitment and retention risks. Civil rights organizations and empirical legal scholars contest the prevalence of frivolous constitutional tort suits, noting that qualified immunity frequently bars meritorious claims; no neutral empirical consensus exists outside the benefiting parties.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__protective_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__protective_scaffold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__protective_scaffold_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__protective_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__protective_scaffold_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__protective_scaffold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__protective_scaffold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__protective_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is moderate (0.62) because the doctrine does not block every suitâonly those where the right was not clearly establishedâbut it reliably externalizes litigation costs and remedy to survivors. Suppression is high (0.70) because the constraint's persistence depends on active judicial enforcement to dismiss claims before discovery. Theater_ratio is moderate (0.40): the clearly-established-law test is presented as a precise, predictable filter protecting good-faith officers, but its application often tracks judicial policy preferences and produces arbitrary outcomes, indicating performative maintenance layered onto genuine protective function. Accessibility_collapse is high (0.75) because federal alternatives to personal-capacity damages against the officer collapse once the doctrine is invoked; resistance is moderate (0.55) because civil rights organizations and some legislators actively contest the doctrine. The temporal series show gradual drift upward as the doctrine hardened from the Harlow era through the Pearson discretionary-balancing era, indicating enforcement intensification and accumulating extraction over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The officer seat experiences the constraint as protective coordination that enables discretionary enforcement without fear of personal ruin. The survivor seat experiences the same constraint as asymmetric extraction that forecloses federal remedy for unconstitutional conduct. The agenda-setter seat experiences it as a manageable judicial tool balancing competing policy values. The engine computes these divergent per-seat classifications from the same structural data; the authored tangled_rope claim reflects the hybrid reality rather than adjudicating any single seat's perception.
 *
 * DIRECTIONALITY LOGIC:
 *   Officers are declared beneficiaries, placing them at the low-d end of the directionality spectrum: the constraint reduces their expected liability costs and functions as a structural subsidy. Violation survivors are declared victims/payers, placing them at the high-d end: the constraint increases their cost of obtaining remedy and often eliminates it entirely. Because survivors are powerless and trapped, with no alternative federal remedy when immunity is granted, the engine amplifies effective extraction for their seat. The federal judiciary sits as agenda_setter with institutional power and analytical exit; its structural relationship is administrative rather than extractive or subsidized, yielding a mid-range d that reflects its role as rule-administrator rather than rent-collector.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling as pure rope because the victim set is non-empty and extraction is structurally significantâremedy denial is not incidental but systematic. It prevents mislabeling as pure snare because a genuine coordination problem, protecting officers from frivolous litigation, is plausibly solved by the arrangement, even if the current form extracts more than necessary. The theater_ratio of 0.40 signals that a substantial portion of the doctrine's maintenance is performative: the clearly-established-law test is presented as a neutral filter, but its application often functions as a remedy-denial mechanism. If the protective function were to atrophy entirely while remedy-denial persisted, the constraint would drift toward piton or snare; the temporal measurements track this risk over the interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    protective_framing_empirical_test,
    'Does the qualified immunity doctrine actually produce a chilling effect on law enforcement vigor when limited or abolished, or is the protective framing empirically unsupported?',
    'Comparative analysis of officer behavior, litigation rates, and departmental policy changes in jurisdictions that have statutorily limited or abolished qualified immunity (e.g., Colorado, New Mexico) versus jurisdictions that retain the federal doctrine.',
    'If abolition produces no measurable chilling effect, the protective reading''s foundational axiom is falsified and the constraint''s classification shifts toward snare. If chilling effects materialize, the protective reading retains structural support as a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protective_framing_empirical_test, empirical, 'Empirical test of the protective framing''s core premise.').

omega_variable(
    clearly_established_arbitrariness,
    'Does the clearly-established-law test provide a determinate, predictable standard, or does judicial discretion in its application produce arbitrary remedy denial?',
    'Quantitative analysis of inter-circuit and intra-circuit inconsistency in qualified immunity grants for factually similar constitutional claims, paired with qualitative review of judicial reasoning in marginal cases.',
    'High arbitrariness would increase the theater_ratio and indicate that the coordination story is partially cover for extraction; determinacy and consistency would support the protective framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clearly_established_arbitrariness, empirical, 'Arbitrariness of the clearly-established-law standard.').

omega_variable(
    kernel_reading_boundary,
    'Is the protective scaffold reading of qualified immunity structurally distinct from the accountability_void and constitutional_fidelity readings, such that the kernel must remain decomposed?',
    'Corpus-level comparison of the three readings'' base_extractiveness, beneficiary and victim structures, and coordination functions; divergence confirms Îµ-invariance requires separate stories.',
    'If the readings converge structurally, the decomposition is overfit and the kernel should be collapsed; if they diverge, the Îµ-invariance principle is validated and the family link is warranted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Structural boundary validation between this reading and its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__protective_scaffold_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qi_psr_tr_t0, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(qi_psr_tr_t8, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 8, 0.23).
narrative_ontology:measurement(qi_psr_tr_t16, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(qi_psr_tr_t24, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement(qi_psr_tr_t32, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 32, 0.37).
narrative_ontology:measurement(qi_psr_tr_t40, qualified_immunity_doctrine__protective_scaffold_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(qi_psr_be_t0, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(qi_psr_be_t8, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(qi_psr_be_t16, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(qi_psr_be_t24, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 24, 0.57).
narrative_ontology:measurement(qi_psr_be_t32, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(qi_psr_be_t40, qualified_immunity_doctrine__protective_scaffold_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(qi_psr_su_t0, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(qi_psr_su_t8, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 8, 0.54).
narrative_ontology:measurement(qi_psr_su_t16, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(qi_psr_su_t24, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(qi_psr_su_t32, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 32, 0.68).
narrative_ontology:measurement(qi_psr_su_t40, qualified_immunity_doctrine__protective_scaffold_reading, suppression_requirement, 40, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% This constraint is the protective_scaffold_reading of the qualified_immunity_doctrine kernel. The kernel decomposes into three structurally distinct readings because the natural-language label 'qualified immunity' conflates a protective policy claim (this file), an accountability critique (accountability_void_reading), and a constitutional legitimacy critique (constitutional_fidelity_reading). Each reading has distinct beneficiaries, victim sets, and epsilon values, necessitating separate constraint stories linked by shared kernel provenance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
