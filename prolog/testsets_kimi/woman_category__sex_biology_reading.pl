% ============================================================================
% CONSTRAINT STORY: woman_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__sex_biology_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: woman_category__sex_biology_reading
 *   human_readable: Woman Category â Sex Biology Reading
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the sex_biology_reading of the
 *   contested woman_category kernel. It treats 'woman' as a biological
 *   category fixed by chromosomal, anatomical, and reproductive markers. The
 *   constraint operates across law, medicine, and institutional
 *   administration to sort individuals into sex-segregated spaces, sports
 *   categories, and epidemiological data sets. While it offers a clear
 *   coordination mechanism for institutions that require sex-based sorting,
 *   it asymmetrically extracts from people with female biology by reducing
 *   their social and legal identity to immutable biological traits, and from
 *   transgender women by excluding them from protections. Intersex
 *   individuals are included ambiguously. The structural delta marks people
 *   with female biology as the primary victim set because the constraint's
 *   enforcement fixes them as the reference class against which all others
 *   are measured, subjecting them to verification regimes and political
 *   contestation over their bodies.
 *
 * KEY AGENTS:
 *   - state_legal_systems (institutional/analytical): Agenda setter â codifies the biological definition into law and policy.
 *   - sex_segregated_operators (organized/constrained): Beneficiary â institutions that gain administrative clarity and liability shields from a binary biological sorting rule.
 *   - people_with_female_biology (powerless/identity_locked): Primary target â fixed as the biological reference class and subjected to reductionism and invasive verification.
 *   - transgender_women (powerless/trapped): Secondary target â categorically excluded from protections and legal recognition.
 *   - intersex_people (powerless/trapped): Excluded â ambiguously situated within a binary criterion that lacks protocols for atypical sex development.
 *   - gender_identity_advocates (organized/constrained): Excluded voice â structurally absent from the policy frameworks that operationalize the definition.
 *   - bioethicists (analytical/analytical): Analytical observer â tracks the boundary between biological fact and social category.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__sex_biology_reading, 0.78).
domain_priors:suppression_score(woman_category__sex_biology_reading, 0.72).
domain_priors:theater_ratio(woman_category__sex_biology_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__sex_biology_reading, "Woman Category â Sex Biology Reading").
narrative_ontology:topic_domain(woman_category__sex_biology_reading, "political_philosophy/law/social_policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__sex_biology_reading, '8402d580-03eb-41c8-b70f-20782f794a39').
narrative_ontology:cs_kernel_codification('8402d580-03eb-41c8-b70f-20782f794a39', fixed_text).
narrative_ontology:cs_authority_grounding('8402d580-03eb-41c8-b70f-20782f794a39', lineage).
narrative_ontology:cs_interpretation_layer_present('8402d580-03eb-41c8-b70f-20782f794a39').
narrative_ontology:cs_reading_relation('8402d580-03eb-41c8-b70f-20782f794a39', woman_category__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('8402d580-03eb-41c8-b70f-20782f794a39', woman_category__intersex_accommodation_reading, influences).
narrative_ontology:cs_axiom('8402d580-03eb-41c8-b70f-20782f794a39', foundational, biological_sex_binary_determines_membership).
narrative_ontology:cs_axiom_status(biological_sex_binary_determines_membership, holdable).
narrative_ontology:cs_axiom_grounding('8402d580-03eb-41c8-b70f-20782f794a39', biological_sex_binary_determines_membership, empirically_contingent).
narrative_ontology:cs_axiom('8402d580-03eb-41c8-b70f-20782f794a39', foundational, female_reproductive_anatomy_is_necessary_condition).
narrative_ontology:cs_axiom_status(female_reproductive_anatomy_is_necessary_condition, holdable).
narrative_ontology:cs_axiom_grounding('8402d580-03eb-41c8-b70f-20782f794a39', female_reproductive_anatomy_is_necessary_condition, empirically_contingent).
narrative_ontology:cs_reference_frame('8402d580-03eb-41c8-b70f-20782f794a39', binary_sex_classification).
narrative_ontology:cs_drift_state('8402d580-03eb-41c8-b70f-20782f794a39', contemporary_policy_contests, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8402d580-03eb-41c8-b70f-20782f794a39', '').
narrative_ontology:cs_kernel_id(woman_category__sex_biology_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, sex_segregated_operators).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, people_with_female_biology).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, transgender_women).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Codifies the biological definition of woman into statute, case law, and administrative regulation. Sets the criteria for sex-segregated spaces, identity documents, and data collection. Can alter the definition through legislative or judicial action but currently maintains the biological criterion as the legal standard.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, state_legal_systems, agenda_setter,
    institutional, generational, analytical, national).

% Schools, prisons, domestic violence shelters, and sports governing bodies that administer sex-segregated spaces. They rely on the biological definition to sort individuals quickly and deflect legal liability. They gain administrative clarity and a shield against challenges to their admissions policies.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, sex_segregated_operators, beneficiary,
    organized, biographical, constrained, national).

% Individuals with XX chromosomes and female reproductive anatomy whose legal recognition and access to protections depend on biological verification. Their bodies become the reference standard against which all others are measured, exposing them to invasive testing, political contestation, and reduction of social identity to anatomical traits they cannot change.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, people_with_female_biology, payer,
    powerless, biographical, identity_locked, global).

% Individuals who identify and live as women but whose biology does not match the definitional criterion. They are categorically excluded from sex-segregated protections, legal recognition as women, and associated services. They cannot alter the chromosomal or anatomical standard to gain entry.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, transgender_women, payer,
    powerless, biographical, trapped, global).

% Individuals with sex characteristics that do not fit typical binary definitions. The biological criterion includes them ambiguouslyâsometimes forced into the female category, sometimes excludedâleaving them without stable membership or clear protections.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, intersex_people, excluded,
    powerless, biographical, trapped, global).

% Advocacy groups and scholars who argue that gender identity, not biology, should determine category membership. They would object to the biological definition but are structurally excluded from the legislative and institutional frameworks that enshrine and enforce it.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, gender_identity_advocates, excluded,
    organized, generational, constrained, global).

% Analytical observers who examine the boundary between biological fact and social category. They track how the biological definition functions in policy, its empirical foundations, and its distributional consequences without being bound to either advocacy coalition.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, bioethicists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legible, verifiable criterion for administering sex-segregated spaces, sports competitions, and sex-disaggregated data collection in contexts where biological differentiation is treated as material.
% TRANSFER_FUNCTION: Moves categorical membership, legal protections, and spatial access away from transgender women and ambiguously situated intersex individuals, while fixing people with female biology as a biologically locked reference class that becomes the site of administrative verification and political contestation.
% ABSENT_VOICES: Gender identity advocates argue that self-identification should determine category membership; intersex scholars and clinicians note that biological variation undermines the binary criterion. Both are largely excluded from the legal and institutional frameworks that operationalize the biological definition.
% DISAPPEARANCE_RATIONALE: If the biological definition disappeared, sex-segregated institutions would need alternative sorting mechanisms such as gender-identity-based rules, individualized assessment, or integrated spaces. Proponents argue this would endanger people with female biology in shelters and sports; opponents argue it would correct an exclusionary structure and reduce harm to transgender women.
% FOUNDING_PROBLEM: How to administer sex-segregated spaces, sports categories, and epidemiological data collection in a way that is objective, resistant to fraud or manipulation, and grounded in observable biological traits rather than subjective identity claims.
% FOUNDING_PROBLEM_CORROBORATION: Medical and forensic institutions corroborate the need for biological sex markers in specific clinical and criminal contexts. Human rights organizations, gender studies scholars, and some bioethics panels dispute that the biological definition is the appropriate or necessary solution. No corroborating source entirely outside the dispute unambiguously attests that the founding problem remains live and that chromosomal definition is the only viable remedy; external bodies are split, with substantial dissent from outside the benefiting parties.
narrative_ontology:disappearance_verdict(woman_category__sex_biology_reading, contested).
narrative_ontology:founding_problem_status(woman_category__sex_biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__sex_biology_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_category__sex_biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__sex_biology_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__sex_biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__sex_biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_category__sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint sorts access to fundamental protections and spaces by immutable biological traits, denying category membership to transgender women and fixing people with female biology as a policed reference class. Suppression (0.72) is high because gender-identity-based alternatives are actively excluded from legal frameworks. Theater ratio (0.52) has risen over the interval as biological definitions are invoked performatively in contexts where their material relevance is thin. Accessibility collapse (0.70) reflects that once the biological criterion is accepted administratively, alternative sorting mechanisms collapse for institutional users. Resistance (0.70) is high due to sustained opposition from gender identity advocates, human rights bodies, and affected communities. The measurement series tracks rising extraction and theater from t0 to t50 as the constraint became more contested and enforcement intensified.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (state legal systems), the constraint is a necessary coordination device for sex-segregated administration. From the payer seat (people with female biology), the constraint is a reduction of personhood to anatomy that exposes them to invasive verification. From the transgender women seat, it is categorical erasure. The engine computes these divergences from the structural data; the tangled_rope claim captures that coordination and extraction are inseparable.
 *
 * DIRECTIONALITY LOGIC:
 *   State legal systems and sex-segregated operators sit near the beneficiary end: they gain administrative legibility and reduced liability from a fixed sorting rule. People with female biology sit near the full-target end because the constraint locks them into a biologically fixed category that becomes the site of enforcement and political contestation; their exit is identity_locked. Transgender women also sit near the full-target end because they are structurally excluded; their exit is trapped since they cannot alter the biological criterion. Gender identity advocates are excluded from the framework entirely, receiving neither coordination nor extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not a mountain because biological sex categories in law are constructed and contested, not natural laws. It is not a rope because the extraction is asymmetric: transgender women and people with female biology pay costs that sex-segregated operators and legal systems do not bear. It is not a snare because there is a genuine coordination problemâsex-segregated spaces, sports fairness, and medical data do require some sorting mechanism. The tangled_rope classification captures that the biological definition solves a real coordination problem for institutions while simultaneously extracting from the very people it categorizes. It is not a scaffold because it lacks a sunset clause, and not a piton because the extraction is actively maintained and politically energized rather than inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    woman_category_reading_contest,
    'Does the sex_biology_reading of the woman_category kernel correctly capture the constraint''s structural relationship, or does the gender_identity_reading produce a fundamentally different constraint with different victims and beneficiaries?',
    'Comparative analysis of the sibling reading''s stakeholder surface and epsilon profile; if the sibling reading has a non-overlapping victim set and different extraction pattern, the kernel is genuinely ambiguous and requires decomposition.',
    'If the gender_identity_reading produces a structurally distinct constraint, this validates the kernel decomposition; if the structural data converges, the readings are observer-axis variations of one constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(woman_category_reading_contest, conceptual, 'Whether the kernel decomposes into structurally distinct constraints per reading.').

omega_variable(
    biological_sex_spectrum_ambiguity,
    'Is human biological sex strictly binary and determined by chromosomes and anatomy, or do intersex variation and neurological sex differentiation undermine the binary axiom?',
    'Systematic review of empirical biology on sex differentiation, DSDs, and brain sex differences.',
    'If the empirical foundation is undermined, the sex_biology_reading''s axiom routes to foreclosed in the engine, potentially reclassifying the constraint from coordination to pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biological_sex_spectrum_ambiguity, empirical, 'Empirical status of the binary biological sex axiom.').

omega_variable(
    victim_set_ambiguity,
    'Does the sex_biology_reading primarily extract from people with female biology through biological reductionism and verification regimes, or from transgender women through categorical exclusion, and can both be simultaneous victim sets?',
    'Seat-level directionality analysis: if both groups show d near 1.0 with substantial scope, the constraint is multi-target extractive.',
    'If people with female biology are the primary victims, the constraint is a tangled rope where the coordinated group pays; if transgender women are the primary victims, the constraint is a snare with a coordination cover story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_set_ambiguity, conceptual, 'Ambiguity in primary victim set under the biological reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__sex_biology_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_category__sex_biology_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(woma_tr_t10, woman_category__sex_biology_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(woma_tr_t20, woman_category__sex_biology_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(woma_tr_t30, woman_category__sex_biology_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(woma_tr_t40, woman_category__sex_biology_reading, theater_ratio, 40, 0.5).
narrative_ontology:measurement(woma_tr_t50, woman_category__sex_biology_reading, theater_ratio, 50, 0.52).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__sex_biology_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(woma_be_t10, woman_category__sex_biology_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(woma_be_t20, woman_category__sex_biology_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(woma_be_t30, woman_category__sex_biology_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(woma_be_t40, woman_category__sex_biology_reading, base_extractiveness, 40, 0.75).
narrative_ontology:measurement(woma_be_t50, woman_category__sex_biology_reading, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__sex_biology_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(woma_su_t10, woman_category__sex_biology_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(woma_su_t20, woman_category__sex_biology_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(woma_su_t30, woman_category__sex_biology_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(woma_su_t40, woman_category__sex_biology_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(woma_su_t50, woman_category__sex_biology_reading, suppression_requirement, 50, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__sex_biology_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
