% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__proportionality_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: mandate_legitimacy_scope__proportionality_reading
 *   human_readable: Proportionality-Conditioned Vaccine Mandate Legitimacy
 *   domain: public_health_ethics/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the proportionality reading of the
 *   mandate_legitimacy_scope kernel: mandate legitimacy is not fixed by state
 *   authority (public_health_primary) or foreclosed by bodily integrity
 *   (bodily_autonomy_primary) but is a function of three variable inputs —
 *   disease severity, vaccine safety/efficacy, and the availability of less
 *   restrictive alternatives. The structural consequence is that the SAME
 *   mandate mechanism produces a legitimate constraint for one pathogen
 *   (measles: high severity, high vaccine efficacy, weak alternatives) and an
 *   illegitimate one for another (seasonal influenza: moderate severity,
 *   moderate/variable efficacy, alternatives like testing and masking more
 *   available) using identical enforcement machinery. The victim set is
 *   therefore conditional on pathogen parameters rather than fixed, which is
 *   the structural delta this reading is meant to capture, and ε is moderate
 *   (0.42) reflecting that the test filters out the worst mandate overreach
 *   but does not eliminate it, since the same institutions that administer
 *   the mandate also administer the proportionality test on themselves.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__proportionality_reading, 0.42).
domain_priors:suppression_score(mandate_legitimacy_scope__proportionality_reading, 0.48).
domain_priors:theater_ratio(mandate_legitimacy_scope__proportionality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__proportionality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__proportionality_reading, "Proportionality-Conditioned Vaccine Mandate Legitimacy").
narrative_ontology:topic_domain(mandate_legitimacy_scope__proportionality_reading, "public_health_ethics/constitutional_law").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__proportionality_reading, '5720f0af-03d8-4c76-bbf8-2bd92f0803fb').
narrative_ontology:cs_kernel_codification('5720f0af-03d8-4c76-bbf8-2bd92f0803fb', distributed).
narrative_ontology:cs_authority_grounding('5720f0af-03d8-4c76-bbf8-2bd92f0803fb', distributed).
narrative_ontology:cs_reading_relation('5720f0af-03d8-4c76-bbf8-2bd92f0803fb', mandate_legitimacy_scope__public_health_primary, influences).
narrative_ontology:cs_reading_relation('5720f0af-03d8-4c76-bbf8-2bd92f0803fb', mandate_legitimacy_scope__bodily_autonomy_primary, influences).
narrative_ontology:cs_axiom('5720f0af-03d8-4c76-bbf8-2bd92f0803fb', foundational, legitimacy_is_conditional_on_disease_parameters).
narrative_ontology:cs_axiom_status(legitimacy_is_conditional_on_disease_parameters, holdable).
narrative_ontology:cs_axiom_grounding('5720f0af-03d8-4c76-bbf8-2bd92f0803fb', legitimacy_is_conditional_on_disease_parameters, empirically_contingent).
narrative_ontology:cs_axiom('5720f0af-03d8-4c76-bbf8-2bd92f0803fb', foundational, less_restrictive_alternative_must_be_absent_for_compulsion).
narrative_ontology:cs_axiom_status(less_restrictive_alternative_must_be_absent_for_compulsion, holdable).
narrative_ontology:cs_axiom_grounding('5720f0af-03d8-4c76-bbf8-2bd92f0803fb', less_restrictive_alternative_must_be_absent_for_compulsion, instrumental).
narrative_ontology:cs_reference_frame('5720f0af-03d8-4c76-bbf8-2bd92f0803fb', jacobson_limiting_principle_undefined).
narrative_ontology:cs_drift_state('5720f0af-03d8-4c76-bbf8-2bd92f0803fb', post_covid19_mandate_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5720f0af-03d8-4c76-bbf8-2bd92f0803fb', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__proportionality_reading, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, immunocompromised_population).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__proportionality_reading, school_age_children).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, low_risk_mandate_targets).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__proportionality_reading, workers_under_marginal_pathogen_mandates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces mandate scope by weighing disease severity, vaccine performance data, and the existence of less restrictive alternatives (testing, masking, isolation) before compelling vaccination. Gains legitimacy and institutional credibility when the proportionality test is applied honestly, and gains cover for overreach when it is applied loosely. Administers the test itself, which gives it discretion over its own constraint.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__proportionality_reading, public_health_authorities, beneficiary).

% Cannot be vaccinated or mount adequate immune response themselves and depend entirely on herd-level uptake for protection from severe pathogens. Benefit directly and disproportionately when mandates are scoped to genuinely high-severity, high-transmissibility diseases with safe, effective vaccines. Have no independent enforcement power; their protection rides entirely on others' compliance.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, immunocompromised_population, beneficiary,
    powerless, biographical, trapped, local).

% Attend settings (schools, daycares) where outbreak dynamics concentrate; benefit from measles/pertussis-type mandates that reflect genuinely high severity and low-cost, high-efficacy alternatives being absent. Have no independent voice in the proportionality determination; represented only through parents and school boards.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, school_age_children, beneficiary,
    powerless, biographical, constrained, local).

% Healthy adults with low individual risk from a given pathogen who are nonetheless swept into a mandate because the proportionality test was applied at a population level rather than an individual one. Bear the compliance cost (medical, employment, social) even where the disease-severity or availability-of-alternatives prongs are weak for their specific risk profile. Their only recourse is exemption processes controlled by the same authority that set the mandate.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, low_risk_mandate_targets, payer,
    moderate, biographical, constrained, national).

% Subject to mandates for pathogens (e.g., seasonal influenza in some employment settings) where severity, vaccine efficacy durability, or the absence of less-restrictive alternatives are contestable rather than clear. Face job loss or exclusion for noncompliance even when the proportionality case for that specific pathogen is comparatively weak relative to the measles-type paradigm case. Cannot easily change employer or jurisdiction on short notice.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, workers_under_marginal_pathogen_mandates, payer,
    moderate, biographical, constrained, regional).

% Argue that the proportionality test is applied inconsistently and used post-hoc to justify mandates decided on other grounds. Are treated as fringe or bad-faith actors in most institutional proportionality reviews and rarely seated on the panels that set mandate scope, even though their empirical claims about marginal-case pathogens sometimes track the weaker end of the severity/efficacy/alternatives test.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, vaccine_hesitant_advocacy_groups, excluded,
    organized, biographical, constrained, national).

% Adjudicate whether a specific mandate satisfies the proportionality test when challenged, reviewing severity data, vaccine trial and surveillance evidence, and the adequacy of less restrictive alternatives. Can strike down or narrow mandates that fail the test, which makes them a check on the agenda-setter but one that acts only after enforcement has already occurred.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__proportionality_reading, courts_and_legislatures, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__proportionality_reading, courts_and_legislatures, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mandate_legitimacy_scope__proportionality_reading, diffuse).
narrative_ontology:fixing_cost_class(mandate_legitimacy_scope__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a graduated standard that sorts genuinely necessary mandates (measles, where severity is high, vaccine efficacy is strong, and alternatives are weak) from marginal ones (seasonal flu, where all three prongs are contestable), so that compulsory vaccination tracks actual public health necessity rather than blanket policy.
% TRANSFER_FUNCTION: Moves bodily-autonomy costs from the general population onto the subset of mandate targets whose individual risk-benefit profile is weak, in exchange for population-level protection that flows primarily to the immunocompromised and to institutionally concentrated settings like schools.
% ABSENT_VOICES: Vaccine-hesitant advocacy groups and individuals with weak personal risk profiles under marginal-pathogen mandates would object that the test is applied asymmetrically — rigorously invoked to justify measles-type mandates, loosely invoked to sustain influenza-type or COVID-booster-type mandates once policy momentum exists. They are rarely seated on the bodies that conduct the proportionality review itself.
% DISAPPEARANCE_RATIONALE: If the proportionality standard vanished, mandate policy would either collapse toward pure public-health-primary logic (any perceived collective benefit justifies compulsion) or pure bodily-autonomy-primary logic (no compulsion regardless of severity) — both siblings in this kernel contest. Courts would lose their primary adjudicating instrument, and the measles/flu distinction the reading currently sustains would disappear along with it.
% FOUNDING_PROBLEM: Courts and public health bodies needed a workable standard to distinguish emergency-justified compulsory vaccination (smallpox, measles) from opportunistic or poorly-justified mandate creep, after early 20th-century jurisprudence (Jacobson v. Massachusetts) established that some compulsion is constitutionally permissible without specifying its limits.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars outside both the public-health-authority and bodily-autonomy-advocacy camps (constitutional law academics writing on Jacobson's limiting principle, and independent epidemiologists auditing mandate-severity fit across pathogens) attest the proportionality test remains a live, unresolved standard rather than settled doctrine — its application to COVID-19 and influenza mandates is actively contested in ongoing litigation, which is itself evidence the founding problem (where is the line) has not been definitively answered.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__proportionality_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__proportionality_reading_tests).
:- end_tests(mandate_legitimacy_scope__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rose over the interval (0.30 to 0.44 around t=12, then relaxing to 0.42) tracking the COVID-19 period, when mandates were extended to marginal-severity, evidence-uncertain, alternative-available scenarios (workplace and travel mandates for a virus with waning vaccine efficacy against transmission) under proportionality language that was applied loosely. Theater ratio spiked in the same window (0.35 at t=12) as agencies increasingly cited the proportionality framework rhetorically while the underlying severity/efficacy/alternatives inputs were contested or shifting faster than policy updated. Suppression requirement rose correspondingly (0.55 at t=12) as enforcement (employment conditions, travel restrictions) intensified precisely where the proportionality case was weakest — a signature of the standard being invoked as legitimation rather than as a live constraint on policy.
 *
 * PERSPECTIVAL GAP:
 *   From the immunocompromised beneficiary seat, the proportionality reading looks like a genuine, working constraint — it correctly identifies measles as mandate-worthy and protects them. From the marginal-pathogen worker seat, the same reading looks like a fig leaf: proportionality language is invoked but the actual severity/efficacy/alternatives balance for their specific case was never rigorously established before enforcement began. The engine should compute these as different effective classifications from the same structural facts, because the directionality differs sharply between the two seats even though both are nominally governed by 'the same' legitimacy standard.
 *
 * DIRECTIONALITY LOGIC:
 *   Immunocompromised populations and school-age children are declared beneficiaries because they are the population the strong-case (measles-type) application of this reading is actually meant to protect, and they bear no meaningful compliance cost themselves. Low-risk mandate targets and workers under marginal-pathogen mandates are declared victims because they bear the full compliance cost of mandates whose proportionality case, for their specific risk profile or pathogen, is comparatively weak — the same standard that legitimates measles mandates gets stretched to cover cases where at least one of the three prongs (severity, efficacy, alternatives) does not clearly hold. Public health authorities sit as both agenda_setter and beneficiary because they administer the test that determines their own legitimacy, which is the structural feature that keeps ε from falling to rope-level: self-administered proportionality tests are a weaker check than external adjudication.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality reading exists precisely to prevent the mandatrophy failure mode of pure public_health_primary reasoning — mandates justified by appeal to any collective benefit regardless of magnitude. Its own risk is the opposite mandatrophy: once the standard is institutionally established, agencies can invoke its language (severity, efficacy, alternatives) without rigorously re-testing it pathogen-by-pathogen, letting the coordination function (correctly scoping mandates) atrophy into rhetorical cover for mandates that would fail the test if honestly applied. The rising theater_ratio around t=12 is exactly this drift being measured, not asserted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    who_administers_the_proportionality_test,
    'Is the proportionality test administered by an institution independent of the mandate''s beneficiaries and enforcers, or by the same public health authority that sets and enforces the mandate?',
    'Compare jurisdictions where mandate proportionality is reviewed by an independent judicial or legislative body before enforcement versus jurisdictions where the enforcing health authority self-certifies proportionality; measure divergence in mandate scope and subsequent legal reversal rates.',
    'Self-administered proportionality tests would push this reading''s effective ε toward the public_health_primary sibling''s extraction profile (the test becomes rhetorical cover); externally adjudicated tests would push it toward genuine rope-like coordination with low extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(who_administers_the_proportionality_test, empirical, 'Whether the proportionality standard is self-policed or externally adjudicated.').

omega_variable(
    severity_threshold_indeterminacy,
    'Where exactly does ''sufficient disease severity'' fall on the spectrum between measles and seasonal influenza, and who has authority to draw that line?',
    'Establish a pre-committed, quantitative severity/efficacy/alternatives scoring rubric (e.g., IFR thresholds, vaccine efficacy-against-transmission thresholds, cost-of-alternatives thresholds) adopted BEFORE a specific mandate controversy arises, then test whether post-hoc mandate decisions are consistent with the pre-committed rubric.',
    'If no such threshold can be non-arbitrarily fixed, the proportionality reading is conceptually underdetermined at its core and functions more like a rhetorical resource for whichever mandate decision has already been made on other grounds than a genuine independent constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(severity_threshold_indeterminacy, conceptual, 'The proportionality test''s core thresholds are not independently fixed, which is a structural vulnerability specific to this reading.').

omega_variable(
    reading_selection_motivation,
    'Is the choice to adopt the proportionality reading itself made for principled reasons, or because it is the reading most likely to legitimate whatever mandate policy has already been politically decided?',
    'Track whether institutions that adopt proportionality language switch to public_health_primary or bodily_autonomy_primary framing when the proportionality test would yield an inconvenient result for a preferred policy.',
    'If institutions switch readings opportunistically, this points to the proportionality_reading itself being periodically instrumentalized rather than consistently held — a conceptual matter about how kernel readings are selected, not resolvable by data internal to this story alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_motivation, conceptual, 'Whether adoption of this specific reading is principled or strategically selected case by case.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__proportionality_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(mand_tr_t4, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(mand_tr_t8, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(mand_tr_t12, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(mand_tr_t16, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(mand_tr_t20, mandate_legitimacy_scope__proportionality_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(mand_be_t4, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(mand_be_t8, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(mand_be_t12, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 12, 0.44).
narrative_ontology:measurement(mand_be_t16, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(mand_be_t20, mandate_legitimacy_scope__proportionality_reading, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(mand_su_t4, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 4, 0.32).
narrative_ontology:measurement(mand_su_t8, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(mand_su_t12, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(mand_su_t16, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(mand_su_t20, mandate_legitimacy_scope__proportionality_reading, suppression_requirement, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, public_health_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__proportionality_reading, bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the mandate_legitimacy_scope kernel. public_health_primary treats state compulsion authority as legitimate whenever collective benefit exists (broader victim set, higher and more uniform ε across pathogens). bodily_autonomy_primary treats any non-consensual compulsion as illegitimate regardless of severity (near-total victim set, ε reflects the mandate mechanism itself as the extraction). This proportionality_reading is structurally distinct: its victim set is CONDITIONAL on pathogen-specific severity/efficacy/alternatives parameters, producing moderate, pathogen-varying ε rather than the uniformly high or uniformly totalizing profiles of its siblings. Each story's ε is stable within its own reading; do not average across the three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
