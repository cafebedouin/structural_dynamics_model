% ============================================================================
% CONSTRAINT STORY: woman_category__intersex_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__intersex_accommodation_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: woman_category__intersex_accommodation_reading
 *   human_readable: Intersex Accommodation Reading of Woman Category Binary Enforcement
 *   domain: political philosophy/law/social policy/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the intersex_accommodation_reading of
 *   the contested kernel woman_category. The standing arrangement under
 *   contest is the institutional enforcement of binary sex categories in
 *   elite sport (with extensions into law and policy), which excludes
 *   intersex variations. This reading assesses that arrangement as a tangled
 *   rope: it carries a genuine coordination function (fair sex-segregated
 *   sport) but asymmetrically extracts from women with DSDs through medical
 *   surveillance and exclusion. The Semenya case exemplifies the victim set.
 *   The reading challenges both the sex_biology_reading (strict binary) and
 *   the gender_identity_reading (internal identity) by asserting that
 *   biological sex itself is a non-binary spectrum and that the woman
 *   category must accommodate intersex variations that do not fit male
 *   parameters.
 *
 * KEY AGENTS:
 *   - world_athletics: Agenda-setter (institutional/global) â enforces testosterone thresholds and sex verification
 *   - elite_female_athletes: Beneficiary (organized/global) â receive protected competitive category
 *   - women_athletes_with_dsd: Primary target (powerless/global/identity_locked) â forced to medically alter or excluded
 *   - intersex_advocacy_groups: Analytical observer (moderate) â resist constraint from outside rule-making power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__intersex_accommodation_reading, 0.72).
domain_priors:suppression_score(woman_category__intersex_accommodation_reading, 0.68).
domain_priors:theater_ratio(woman_category__intersex_accommodation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__intersex_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__intersex_accommodation_reading, "Intersex Accommodation Reading of Woman Category Binary Enforcement").
narrative_ontology:topic_domain(woman_category__intersex_accommodation_reading, "political philosophy/law/social policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__intersex_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__intersex_accommodation_reading, '6d73c2ee-a6fd-444f-bb0c-7e54014f3fc7').
narrative_ontology:cs_kernel_codification('6d73c2ee-a6fd-444f-bb0c-7e54014f3fc7', formalized).
narrative_ontology:cs_authority_grounding('6d73c2ee-a6fd-444f-bb0c-7e54014f3fc7', lineage).
narrative_ontology:cs_interpretation_layer_present('6d73c2ee-a6fd-444f-bb0c-7e54014f3fc7').
narrative_ontology:cs_reading_relation('6d73c2ee-a6fd-444f-bb0c-7e54014f3fc7', woman_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('6d73c2ee-a6fd-444f-bb0c-7e54014f3fc7', woman_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('6d73c2ee-a6fd-444f-bb0c-7e54014f3fc7', foundational, woman_category_accommodates_intersex_biological_spectrum).
narrative_ontology:cs_axiom_status(woman_category_accommodates_intersex_biological_spectrum, holdable).
narrative_ontology:cs_axiom_grounding('6d73c2ee-a6fd-444f-bb0c-7e54014f3fc7', woman_category_accommodates_intersex_biological_spectrum, empirically_contingent).
narrative_ontology:cs_reference_frame('6d73c2ee-a6fd-444f-bb0c-7e54014f3fc7', biological_non_binary_spectrum_accommodation).
narrative_ontology:cs_drift_state('6d73c2ee-a6fd-444f-bb0c-7e54014f3fc7', contemporary_elite_sport_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6d73c2ee-a6fd-444f-bb0c-7e54014f3fc7', '').
narrative_ontology:cs_kernel_id(woman_category__intersex_accommodation_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, elite_female_athletes).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, women_athletes_with_dsd).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces testosterone thresholds and sex verification protocols for elite women's competition. Maintains that binary categories are necessary for fair sport. Its authority derives from historical governance of athletics and medical advisory commissions. Altering the rules would risk legitimacy challenges from member federations and athletes.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, world_athletics, agenda_setter,
    institutional, generational, constrained, global).

% Compete in the women's category protected by testosterone thresholds. Many support the rules as safeguarding competitive opportunities. They benefit from the exclusion of athletes with male-typical testosterone levels. Their exit is constrained because abandoning the category system means leaving elite sport.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, elite_female_athletes, beneficiary,
    organized, biographical, constrained, global).

% Are required to medically suppress natural testosterone or are barred from women's competition. Their biology is treated as a regulatory problem. They cannot exit their bodies or their gender identity; professional identity as elite athletes is fused with their intersex status. The constraint extracts competitive opportunity, bodily autonomy, and social recognition.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, women_athletes_with_dsd, payer,
    powerless, biographical, identity_locked, global).

% Advise sports governing bodies on testosterone thresholds and female biology definitions. Their professional authority and research funding are tied to the regulatory apparatus of sex categorization. They interpret biological data to maintain categorical boundaries.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, sports_medicine_commissions, agenda_setter,
    institutional, generational, constrained, global).

% Document harms of sex testing and advocate for inclusion of intersex women. They provide analytical and legal support to affected athletes. They are structurally excluded from final rule-making authority but shape public and legal discourse through campaigns and amicus briefs.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, intersex_advocacy_groups, observer,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_category__intersex_accommodation_reading, elite_female_athletes).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves fair competition and participation opportunities in elite women's sport by segregating competition based on sex-linked performance indicators, preventing male-typical physiology from dominating female categories.
% TRANSFER_FUNCTION: Moves competitive eligibility, medals, and career opportunities from women with DSDs and natural testosterone variations to women who meet binary female parameters, through mandatory medical testing and testosterone suppression.
% ABSENT_VOICES: Women athletes with DSDs are present in appeals hearings but structurally unheard in rule-making; open-category and third-category advocates are excluded from governance; dissenting endocrinologists who dispute testosterone-performance causality are marginalized in sports medicine commissions.
% DISAPPEARANCE_RATIONALE: Overnight removal of binary sex category enforcement in elite sport would immediately rearrange competitive fields: DSD athletes would enter women's events or demand new categories, medal tables would shift, the sex-testing administrative apparatus would collapse, and sports governance would face a legitimacy crisis. Social categorization outside sport would be less directly affected.
% FOUNDING_PROBLEM: The historical exclusion of female-bodied athletes from competitive sport and the need for sex-segregated categories to ensure competitive fairness and meaningful participation opportunities for women after the expansion of women's athletics in the twentieth century.
% FOUNDING_PROBLEM_CORROBORATION: Sports historians and feminist scholars attest to the historical exclusion. However, intersex advocacy organizations, sports ethicists, and some sports scientists outside the benefiting parties attest that the founding problem is solved by alternative categorization models and that binary enforcement now functions as extraction from a small population rather than protection of a larger one.
narrative_ontology:disappearance_verdict(woman_category__intersex_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__intersex_accommodation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__intersex_accommodation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_category__intersex_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__intersex_accommodation_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__intersex_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__intersex_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_category__intersex_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint extracts competitive opportunity, bodily autonomy, and identity recognition from a small, structurally powerless population. Suppression (0.68) is high due to active medical testing, testosterone regulation, and public stigmatization of DSD athletes. Theater ratio (0.45) reflects that much enforcement rhetoric ('protecting women's sports') obscures biological complexity and serves symbolic boundary maintenance. Accessibility collapse (0.60) captures that alternatives (open categories, inclusion) exist in theory but lack institutional support and social legitimacy. Resistance (0.55) is moderate-to-high due to high-profile legal challenges and growing scientific critique. Metrics are authored to reflect elite sports, where enforcement is concentrated; in general policy the same binary categorization is less actively enforced and thus less extractive.
 *
 * PERSPECTIVAL GAP:
 *   World Athletics and elite female athletes experience the constraint as necessary coordination preserving fair competition and the integrity of the women's category. Women athletes with DSDs experience the same structure as biological essentialism that erases their embodied reality and forces medical intervention. The engine computes this divergence from structural data: same constraint, opposite directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   World Athletics and sports medicine commissions sit near the beneficiary end administratively (they maintain authority and categorical control) but pay reputational costs. Elite female athletes (beneficiary, organized, constrained exit) are structural beneficiaries of the categorical boundary (low d). Women with DSDs (payer, powerless, identity_locked) are full targets with no arbitrage â they cannot exit their biology or their professional identity (high d). Intersex advocacy groups (observer, moderate, constrained) sit in the middle, bearing advocacy costs without direct extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â fair competition for female-bodied athletes â remains live, preventing pure snare classification. However, the specific mechanism of binary medical gatekeeping has atrophied into a tangled rope: it coordinates a real sporting community but extracts disproportionately from intersex women. The mandate has outlived its proportionality. If the constraint were a scaffold, it would carry a sunset clause; it does not, indicating institutional inertia rather than transitional intent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    elite_sports_vs_general_policy_scope,
    'Does the high extractiveness observed in elite sport generalize to legal and social policy domains where the same binary category operates but is rarely enforced?',
    'Comparative analysis of enforcement intensity across policy domains (employment law, healthcare, education); if general policy shows negligible extraction, decompose per the epsilon-invariance principle.',
    'Would require splitting this into separate sport-specific and general-policy constraints, altering the classification in low-enforcement domains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_sports_vs_general_policy_scope, conceptual, 'Whether domain scope variation requires constraint decomposition.').

omega_variable(
    testosterone_performance_causality,
    'Is natural testosterone in DSD women the primary driver of the performance differential that justifies exclusion, or is the causality confounded by other physiological and training variables?',
    'Controlled longitudinal studies of DSD athlete performance pre- and post-suppression; meta-analysis of testosterone-athleticism correlation in female populations.',
    'If causality is weak or confounded, the coordination function is largely pretextual and the constraint shifts toward snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(testosterone_performance_causality, empirical, 'Whether the stated biological justification for exclusion is empirically sound.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of intersex women structural (enforced by rules, medicine, and administration) or internalized (shame, secrecy, identity crisis carried by affected individuals)?',
    'Post-exit and post-retirement testimony from affected athletes; prevalence of mental health outcomes tied to identity suppression versus direct administrative exclusion.',
    'If internalized suppression dominates, effective extraction exceeds the structural measure because the target carries the suppression beyond the institutional context.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism for intersex athletes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__intersex_accommodation_reading, 0, 64).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_category__intersex_accommodation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(woma_tr_t12, woman_category__intersex_accommodation_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(woma_tr_t24, woman_category__intersex_accommodation_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(woma_tr_t36, woman_category__intersex_accommodation_reading, theater_ratio, 36, 0.4).
narrative_ontology:measurement(woma_tr_t48, woman_category__intersex_accommodation_reading, theater_ratio, 48, 0.5).
narrative_ontology:measurement(woma_tr_t64, woman_category__intersex_accommodation_reading, theater_ratio, 64, 0.45).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__intersex_accommodation_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(woma_be_t12, woman_category__intersex_accommodation_reading, base_extractiveness, 12, 0.35).
narrative_ontology:measurement(woma_be_t24, woman_category__intersex_accommodation_reading, base_extractiveness, 24, 0.4).
narrative_ontology:measurement(woma_be_t36, woman_category__intersex_accommodation_reading, base_extractiveness, 36, 0.48).
narrative_ontology:measurement(woma_be_t48, woman_category__intersex_accommodation_reading, base_extractiveness, 48, 0.62).
narrative_ontology:measurement(woma_be_t64, woman_category__intersex_accommodation_reading, base_extractiveness, 64, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__intersex_accommodation_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(woma_su_t12, woman_category__intersex_accommodation_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(woma_su_t24, woman_category__intersex_accommodation_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(woma_su_t36, woman_category__intersex_accommodation_reading, suppression_requirement, 36, 0.55).
narrative_ontology:measurement(woma_su_t48, woman_category__intersex_accommodation_reading, suppression_requirement, 48, 0.65).
narrative_ontology:measurement(woma_su_t64, woman_category__intersex_accommodation_reading, suppression_requirement, 64, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__intersex_accommodation_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, gender_identity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the woman_category kernel. The kernel decomposes into three structurally distinct constraints because the label 'woman' conflates binary biological definition (sex_biology_reading), non-binary biological accommodation (intersex_accommodation_reading), and internal identity determination (gender_identity_reading). Each reading has different victim sets, epsilon values, and primary policy domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
