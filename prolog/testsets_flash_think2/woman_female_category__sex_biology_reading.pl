% ============================================================================
% CONSTRAINT STORY: woman_female_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__sex_biology_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: woman_female_category__sex_biology_reading
 *   human_readable: Category of Woman/Female by Sex Biology
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This constraint defines membership in the category of 'woman' or 'female'
 *   based on chromosomal sex (XX/XY), reproductive anatomy, and developmental
 *   biology. It is presented as a biological reality, often invoked to
 *   justify sex-segregated spaces, sports categories, and legal protections
 *   for natal females. However, its application is highly contested,
 *   particularly by trans women and gender identity advocates, who argue for
 *   definitions based on self-identified gender. The high extractiveness and
 *   suppression reflect the consequences of enforcing this definition against
 *   those it excludes, despite its claim to naturalness.
 *
 * KEY AGENTS:
 *   - natal_females: Primary beneficiary (organized/constrained) — benefit from sex-based protections.
 *   - trans_women: Primary target (powerless/identity_locked) — excluded from female-only spaces.
 *   - gender_critical_advocates: Agenda setter (organized/mobile) — actively promote this definition.
 *   - legal_systems: Agenda setter (institutional/analytical) — adjudicate and codify definitions.
 *   - gender_identity_advocates: Excluded (organized/mobile) — actively resist this definition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__sex_biology_reading, 0.78).
domain_priors:suppression_score(woman_female_category__sex_biology_reading, 0.88).
domain_priors:theater_ratio(woman_female_category__sex_biology_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__sex_biology_reading, mountain).
narrative_ontology:human_readable(woman_female_category__sex_biology_reading, "Category of Woman/Female by Sex Biology").
narrative_ontology:topic_domain(woman_female_category__sex_biology_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__sex_biology_reading).
domain_priors:emerges_naturally(woman_female_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__sex_biology_reading, '9654a9e3-2cb0-4cec-8099-554647d4ca46').
narrative_ontology:cs_kernel_codification('9654a9e3-2cb0-4cec-8099-554647d4ca46', implicit).
narrative_ontology:cs_authority_grounding('9654a9e3-2cb0-4cec-8099-554647d4ca46', practice).
narrative_ontology:cs_interpretation_layer_present('9654a9e3-2cb0-4cec-8099-554647d4ca46').
narrative_ontology:cs_reading_relation('9654a9e3-2cb0-4cec-8099-554647d4ca46', woman_female_category__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('9654a9e3-2cb0-4cec-8099-554647d4ca46', woman_female_category__hybrid_contextual_reading, forecloses).
narrative_ontology:cs_axiom('9654a9e3-2cb0-4cec-8099-554647d4ca46', foundational, sex_is_binary_and_immutable).
narrative_ontology:cs_axiom_status(sex_is_binary_and_immutable, holdable).
narrative_ontology:cs_axiom_grounding('9654a9e3-2cb0-4cec-8099-554647d4ca46', sex_is_binary_and_immutable, empirically_contingent).
narrative_ontology:cs_axiom('9654a9e3-2cb0-4cec-8099-554647d4ca46', foundational, woman_is_adult_human_female).
narrative_ontology:cs_axiom_status(woman_is_adult_human_female, holdable).
narrative_ontology:cs_axiom_grounding('9654a9e3-2cb0-4cec-8099-554647d4ca46', woman_is_adult_human_female, conventional).
narrative_ontology:cs_reference_frame('9654a9e3-2cb0-4cec-8099-554647d4ca46', biological_essentialism_framework).
narrative_ontology:cs_drift_state('9654a9e3-2cb0-4cec-8099-554647d4ca46', contemporary_gender_discourse, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('9654a9e3-2cb0-4cec-8099-554647d4ca46', '').
narrative_ontology:cs_kernel_id(woman_female_category__sex_biology_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, natal_females).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, trans_women).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from sex-based protections, spaces, and rights defined by biological sex. They seek to maintain these distinctions for safety, fairness, and recognition of their unique biological experiences.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, natal_females, beneficiary,
    organized, generational, constrained, global).

% Are excluded from female-only spaces (e.g., prisons, shelters, sports categories) and legal definitions of 'woman' based on this reading. They bear the cost of this exclusion through lack of access, discrimination, and denial of their self-identified gender.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, trans_women, payer,
    powerless, biographical, identity_locked, global).

% Actively promote and defend the definition of 'woman' based solely on biological sex. They organize, lobby, and litigate to ensure this definition is upheld in law and policy, viewing it as essential for women's rights.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, gender_critical_advocates, agenda_setter,
    organized, biographical, mobile, national).

% Adjudicate and codify definitions of sex and gender in law, often grappling with conflicting interpretations. Their rulings can enforce or challenge this biological reading, impacting the rights and protections of various groups.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, legal_systems, agenda_setter,
    institutional, civilizational, analytical, national).

% Provide scientific definitions of biological sex and its developmental variations. Some advocate for the primacy of biological sex in certain contexts (e.g., medicine, sports), while others emphasize the social and psychological aspects of gender.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, medical_professionals, observer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(woman_female_category__sex_biology_reading, medical_professionals, agenda_setter).

% Actively challenge this biological reading, advocating for definitions of 'woman' that include trans women based on gender identity. They are excluded from the framing of this constraint but exert significant counter-pressure.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, gender_identity_advocates, excluded,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__sex_biology_reading, natal_females).
narrative_ontology:fixing_cost_class(woman_female_category__sex_biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, historically understood, and biologically grounded definition of 'woman' or 'female' for purposes of data collection, medical treatment, sports categories, and single-sex spaces, aiming to coordinate social and legal structures around this definition.
% TRANSFER_FUNCTION: Transfers exclusive access to certain spaces, resources, and legal protections to natal females, while excluding trans women from these same categories and benefits.
% ABSENT_VOICES: Trans women and gender identity advocates are structurally excluded from the framing of this constraint, as their core premise (gender identity as primary) is directly contradicted. They would argue for inclusive definitions and against sex-based exclusions.
% DISAPPEARANCE_RATIONALE: If the definition of 'woman' based on sex biology vanished overnight, it would fundamentally alter legal frameworks for sex-based rights, protections, and data collection. Single-sex spaces would lose their biological rationale, and the concept of 'female' in medicine and sports would be radically redefined, leading to a significant reorganization of social and legal structures.
% FOUNDING_PROBLEM: To establish a clear, objective, and historically consistent definition of 'woman' or 'female' for social, legal, and biological purposes, particularly to ensure the protection and recognition of the distinct experiences and needs of biological females.
% FOUNDING_PROBLEM_CORROBORATION: Gender-critical advocates and some feminist organizations attest that the problem of defining and protecting biological females remains live, citing ongoing debates about sex-based rights and spaces. Medical and biological sciences corroborate the biological distinctions of sex. However, gender identity advocates contest the framing of the problem itself.
narrative_ontology:disappearance_verdict(woman_female_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__sex_biology_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__sex_biology_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(woman_female_category__sex_biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__sex_biology_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__sex_biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__sex_biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, ExtMetricName, E),
    domain_priors:suppression_score(woman_female_category__sex_biology_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(woman_female_category__sex_biology_reading),
    narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(woman_female_category__sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `claimed_type` is 'mountain' because proponents assert it as an immutable biological fact (`emerges_naturally: true`). However, the `extractiveness` (0.78) is high because its application actively excludes trans women from spaces and protections they seek, imposing significant costs. `Suppression` (0.88) is also high, reflecting the active enforcement required to maintain this definition in contested social and legal contexts, often against strong resistance. `Theater_ratio` is low (0.1) as the constraint's function is direct and not primarily performative. `Accessibility_collapse` is high (0.85) for those who accept the biological premise, as alternatives to biological sex are seen as non-existent. `Resistance` is very high (0.9) due to intense social and political contestation.
 *
 * PERSPECTIVAL GAP:
 *   Natal females and gender-critical advocates experience this as a natural, protective boundary, essential for their rights and safety. Trans women, however, experience it as a deeply extractive and suppressive force that denies their identity and access to necessary spaces. The engine's classification will highlight this divergence between the claimed naturalness and the experienced extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Natal females are beneficiaries (low d) as the constraint defines and protects their category. Trans women are targets (high d) as they are excluded and bear the costs of this definition. Gender-critical advocates are agenda-setters, actively shaping and enforcing the constraint. Legal systems act as institutional agenda-setters, codifying and enforcing the definition. Gender identity advocates are excluded, their alternative definitions actively suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by highlighting the tension between the constraint's 'mountain' claim (natural, unchangeable) and its high `extractiveness` and `suppression`. This signals a 'false summit' where a claimed natural law functions as a constructed, actively enforced, and highly extractive constraint for specific groups. The founding problem of defining 'woman' for protection is still 'live', but its 'contested' status and the high extraction indicate that the current resolution is not universally beneficial or accepted, suggesting a potential for mandatrophy if the original coordination function is overshadowed by extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_social_construct,
    'Is the definition of ''woman'' based on sex biology a genuine natural law, or a social construct that benefits identifiable agents?',
    'Analysis of cross-cultural and historical variations in gender definitions, alongside biological data on sex development, to determine the degree of biological determinism versus social interpretation.',
    'If primarily a natural law, the high extractiveness is a consequence of an immutable reality. If substantially a social construct, the high extractiveness points to a Snare or Tangled Rope, where the ''naturalness'' claim serves as cover for exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, conceptual, 'Ambiguity between biological reality and social construction of gender categories.').

omega_variable(
    scope_of_sex_based_protections,
    'To what extent are sex-based protections (e.g., single-sex spaces, sports categories) genuinely necessary for natal females, and to what extent do they function primarily to exclude trans women?',
    'Empirical studies on the impact of inclusive policies on the safety and fairness of single-sex spaces and sports, as well as the lived experiences of both natal and trans women.',
    'If necessity is low and exclusion is high, the constraint''s extractiveness is amplified. If necessity is high, the extractiveness is a regrettable but unavoidable consequence of a genuine coordination problem.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scope_of_sex_based_protections, empirical, 'Balancing sex-based protections with inclusion.').

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading of the ''woman_female_category'' kernel. How would the classification change if a sibling reading (e.g., ''gender_identity_reading'') were adopted?',
    'By generating and analyzing the sibling constraint stories, observing the shifts in beneficiary/victim sets, extractiveness, and suppression.',
    'The ''gender_identity_reading'' would likely shift the beneficiary/victim sets, potentially reclassifying trans women as beneficiaries and creating new victim groups (e.g., those who reject gender identity as a basis for category membership). The core disagreement is located in the foundational definition of the category itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Impact of alternative kernel readings on classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__sex_biology_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__sex_biology_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(woma_tr_t6, woman_female_category__sex_biology_reading, theater_ratio, 6, 0.1).
narrative_ontology:measurement(woma_tr_t12, woman_female_category__sex_biology_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(woma_tr_t18, woman_female_category__sex_biology_reading, theater_ratio, 18, 0.1).
narrative_ontology:measurement(woma_tr_t24, woman_female_category__sex_biology_reading, theater_ratio, 24, 0.1).
narrative_ontology:measurement(woma_tr_t30, woman_female_category__sex_biology_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__sex_biology_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(woma_be_t6, woman_female_category__sex_biology_reading, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(woma_be_t12, woman_female_category__sex_biology_reading, base_extractiveness, 12, 0.71).
narrative_ontology:measurement(woma_be_t18, woman_female_category__sex_biology_reading, base_extractiveness, 18, 0.74).
narrative_ontology:measurement(woma_be_t24, woman_female_category__sex_biology_reading, base_extractiveness, 24, 0.76).
narrative_ontology:measurement(woma_be_t30, woman_female_category__sex_biology_reading, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__sex_biology_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(woma_su_t6, woman_female_category__sex_biology_reading, suppression_requirement, 6, 0.78).
narrative_ontology:measurement(woma_su_t12, woman_female_category__sex_biology_reading, suppression_requirement, 12, 0.81).
narrative_ontology:measurement(woma_su_t18, woman_female_category__sex_biology_reading, suppression_requirement, 18, 0.84).
narrative_ontology:measurement(woma_su_t24, woman_female_category__sex_biology_reading, suppression_requirement, 24, 0.86).
narrative_ontology:measurement(woma_su_t30, woman_female_category__sex_biology_reading, suppression_requirement, 30, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__sex_biology_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, woman_female_category__gender_identity_reading).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, woman_female_category__hybrid_contextual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'woman_female_category' kernel. This 'sex_biology_reading' defines category membership by biological sex, while 'gender_identity_reading' defines it by self-identification, and 'hybrid_contextual_reading' by context-dependent criteria. Each reading yields a different structural constraint with distinct ε values and stakeholder impacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
