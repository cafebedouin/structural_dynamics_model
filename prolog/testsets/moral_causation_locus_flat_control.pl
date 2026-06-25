% ============================================================================
% CONSTRAINT STORY: moral_causation_locus_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_moral_causation_locus_flat_control, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: moral_causation_locus_flat_control
 *   human_readable: Moral Causation Locus Attribution Framework
 *   domain: moral_psychology/philosophy_of_action/social_psychology
 *
 * SUMMARY:
 *   The dispositional attribution framework treats moral behavior as
 *   primarily caused by stable internal character traits rather than
 *   situational forces. This framework structures academic research programs,
 *   educational curricula, legal culpability standards, and everyday moral
 *   judgment. Experimental evidence from social psychology demonstrates
 *   systematic overestimation of dispositional causes and underestimation of
 *   situational forces, but this evidence is treated as counterintuitive
 *   rather than falsifying. The constraint is claimed as tangled_rope: it
 *   coordinates attribution practices while extracting from those whose
 *   situational constraints are rendered invisible and from researchers whose
 *   findings challenge the dispositional default.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(moral_causation_locus_flat_control, 0.68).
domain_priors:suppression_score(moral_causation_locus_flat_control, 0.71).
domain_priors:theater_ratio(moral_causation_locus_flat_control, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(moral_causation_locus_flat_control, extractiveness, 0.68).
narrative_ontology:constraint_metric(moral_causation_locus_flat_control, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(moral_causation_locus_flat_control, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(moral_causation_locus_flat_control, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(moral_causation_locus_flat_control, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(moral_causation_locus_flat_control, tangled_rope).
narrative_ontology:human_readable(moral_causation_locus_flat_control, "Moral Causation Locus Attribution Framework").
narrative_ontology:topic_domain(moral_causation_locus_flat_control, "moral_psychology/philosophy_of_action/social_psychology").

domain_priors:requires_active_enforcement(moral_causation_locus_flat_control).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(moral_causation_locus_flat_control, '19006009-9dfb-4465-ad27-58e076fd7504').
narrative_ontology:cs_kernel_codification('19006009-9dfb-4465-ad27-58e076fd7504', distributed).
narrative_ontology:cs_authority_grounding('19006009-9dfb-4465-ad27-58e076fd7504', distributed).
narrative_ontology:cs_created_at('19006009-9dfb-4465-ad27-58e076fd7504', '').

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(moral_causation_locus_flat_control, moral_causation_locus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(moral_causation_locus_flat_control, dispositional_attribution_researchers).
narrative_ontology:constraint_beneficiary(moral_causation_locus_flat_control, character_education_institutions).
narrative_ontology:constraint_beneficiary(moral_causation_locus_flat_control, criminal_justice_systems).
narrative_ontology:constraint_victim(moral_causation_locus_flat_control, situational_attribution_researchers).
narrative_ontology:constraint_victim(moral_causation_locus_flat_control, structural_reform_advocates).
narrative_ontology:constraint_victim(moral_causation_locus_flat_control, individuals_blamed_for_situational_failures).
narrative_ontology:constraint_vindicates(moral_causation_locus_flat_control, stable_character_doctrine).
narrative_ontology:constraint_vindicates(moral_causation_locus_flat_control, individual_moral_responsibility_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Academic psychologists and philosophers whose research programs, tenure cases, and citation networks depend on demonstrating stable personality traits predict moral behavior. They design studies measuring character constructs, publish in journals that favor dispositional explanations, and train graduate students in trait-based methodologies. Exiting this framework means abandoning accumulated research capital and professional identity.
narrative_ontology:constraint_stakeholder(moral_causation_locus_flat_control, dispositional_attribution_researchers, agenda_setter,
    institutional, generational, constrained, global).

% Schools, religious organizations, and youth programs whose curricula and funding models assume moral behavior flows from cultivated virtues. They receive grants, tuition, and donations premised on their ability to build character. A situational framing would redirect resources toward environmental redesign rather than individual formation.
narrative_ontology:constraint_stakeholder(moral_causation_locus_flat_control, character_education_institutions, beneficiary,
    institutional, generational, constrained, national).

% Legal frameworks that ground culpability in the defendant's character and intent rather than situational pressures. Dispositional attribution legitimates punishment as deserved response to bad character; situational attribution would require examining systemic conditions that produce criminal behavior, threatening the retributive foundation of sentencing.
narrative_ontology:constraint_stakeholder(moral_causation_locus_flat_control, criminal_justice_systems, beneficiary,
    institutional, generational, identity_locked, national).

% Social psychologists whose experimental evidence demonstrates situational forces overwhelm character in predicting behavior. They face systematic publication bias against null results on personality measures, reviewer skepticism of situational explanations, and marginalization in departments organized around trait psychology. Their work is framed as attacking personal responsibility rather than describing causal reality.
narrative_ontology:constraint_stakeholder(moral_causation_locus_flat_control, situational_attribution_researchers, payer,
    organized, biographical, constrained, global).

% Policy advocates arguing that poverty, discrimination, and institutional design cause behavioral outcomes attributed to character deficits. They must overcome the dispositional default in public discourse to secure funding for environmental interventions. Every failure of a structural program is read as vindicating the character explanation.
narrative_ontology:constraint_stakeholder(moral_causation_locus_flat_control, structural_reform_advocates, payer,
    organized, generational, mobile, national).

% People whose behavior under extreme situational pressure is attributed to character flaws rather than circumstances. They carry moral blame, criminal records, and reputational damage for actions the situational evidence suggests most people would commit under the same conditions. The dispositional frame denies them the exculpatory power of context.
narrative_ontology:constraint_stakeholder(moral_causation_locus_flat_control, individuals_blamed_for_situational_failures, payer,
    powerless, biographical, trapped, local).

% Researchers who design controlled experiments isolating situational variables and measuring their effect sizes against personality predictors. They document the systematic overestimation of dispositional causes and underestimation of situational forces, but their findings are treated as counterintuitive exceptions rather than falsifications of the dispositional kernel.
narrative_ontology:constraint_stakeholder(moral_causation_locus_flat_control, experimental_social_psychologists, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared framework for attributing moral causation, enabling consistent judgments about responsibility, blame, and intervention across legal, educational, and interpersonal contexts.
% TRANSFER_FUNCTION: Moves moral blame and institutional resources toward character-focused interventions and away from situational redesign; concentrates reputational damage on individuals while diffusing accountability for systemic conditions.
% ABSENT_VOICES: Individuals whose situational constraints are invisible to observers with dispositional priors; communities whose collective conditions produce behaviors read as individual character failures; researchers whose situational findings are systematically under-cited.
% DISAPPEARANCE_RATIONALE: If the dispositional default vanished, criminal sentencing would shift toward restorative and situational mitigation; character education funding would flow to environmental design; academic psychology would reorganize around person-situation interaction rather than stable traits; and individuals currently blamed for situational failures would gain exculpatory context.
% FOUNDING_PROBLEM: Early moral philosophy and folk psychology needed a stable locus for moral agency to ground responsibility and desert; without dispositional attribution, it was unclear how to assign praise, blame, or predict future behavior.
% FOUNDING_PROBLEM_CORROBORATION: Dispositional researchers and character educators attest the problem remains live, citing the need for stable moral identity. Experimental social psychologists and structural reform advocates attest the founding problem was solved by person-situation interactionism decades ago, and the dispositional frame persists because it serves institutional interests in individualizing blame; meta-analyses and replication studies from outside the benefiting institutions support the situational evidence.
narrative_ontology:disappearance_verdict(moral_causation_locus_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(moral_causation_locus_flat_control, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(moral_causation_locus_flat_control, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-06-25',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(moral_causation_locus_flat_control, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(moral_causation_locus_flat_control_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(moral_causation_locus_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(moral_causation_locus_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.68) because the framework systematically misattributes situational failures to character, concentrating blame on individuals while diffusing accountability for systemic conditions. Suppression is high (0.71) because challenging dispositional attribution is read as attacking personal responsibility itself, making situational explanations professionally and politically costly. Theater ratio is moderate (0.42): the coordination function is real—shared attribution practices enable consistent moral judgment—but growing enforcement effort defends the dispositional default against accumulating situational evidence. Accessibility collapse is moderate (0.48) because situational alternatives remain conceptually available but institutionally marginalized. Resistance is high (0.73) because situational researchers, structural reform advocates, and blamed individuals actively contest the framework.
 *
 * PERSPECTIVAL GAP:
 *   From the dispositional researcher seat, the framework is genuine coordination enabling stable moral judgment and personal responsibility. From the situational researcher seat, the same structure operates as enforced extraction: systematic misattribution serving institutional interests in individualizing blame. From the blamed individual seat, it is pure extraction: moral and legal consequences imposed for situational failures. The engine computes these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Dispositional attribution researchers are agenda-setters (design studies, control journals, train students) with constrained exit (abandoning the framework means abandoning research capital). Character education institutions and criminal justice systems are beneficiaries: the framework legitimates their resource allocation and institutional authority. Situational attribution researchers and structural reform advocates are payers: they face publication bias, reviewer skepticism, and funding disadvantages. Individuals blamed for situational failures are the primary victims: they carry moral and legal consequences for behaviors the evidence suggests are situationally determined.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question is whether the framework's coordination function (shared attribution practices) can be separated from its extraction function (systematic situational denial). Person-situation interactionism suggests they are separable: attribution practices could coordinate around interaction effects rather than dispositional primacy. The framework persists in its extractive form because benefiting institutions depend on individualizing blame to avoid accountability for systemic conditions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fundamental_attribution_error_magnitude,
    'What is the true effect size of dispositional versus situational predictors of moral behavior across contexts?',
    'Meta-analysis of experimental studies with adequate statistical power, pre-registered designs, and situational manipulations strong enough to overcome dispositional priors.',
    'If situational effect sizes systematically exceed dispositional ones, the framework''s empirical foundation collapses and its persistence becomes evidence of institutional extraction rather than coordination necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fundamental_attribution_error_magnitude, empirical, 'Whether dispositional or situational causes dominate moral behavior prediction.').

omega_variable(
    coordination_extraction_separability,
    'Can attribution practices coordinate moral judgment without privileging dispositional over situational causes?',
    'Natural experiment from institutions that adopt person-situation interaction frameworks: if coordination holds while dispositional primacy is abandoned, the functions are separable.',
    'If separable, the dispositional default is extraction riding on coordination; if inseparable, some measured extraction is the price of stable attribution practices.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether coordination requires dispositional primacy or can operate through interaction frameworks.').

omega_variable(
    institutional_dependence_on_individualized_blame,
    'Do criminal justice systems, character education institutions, and trait psychology research programs structurally depend on dispositional attribution to maintain their authority and resource flows?',
    'Institutional analysis of funding, legitimacy, and operational models under dispositional versus situational frameworks; examination of resistance patterns when situational evidence threatens institutional interests.',
    'If institutional survival depends on dispositional attribution, the framework''s persistence despite contrary evidence is explained by beneficiary capture rather than empirical adequacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_dependence_on_individualized_blame, empirical, 'Whether benefiting institutions structurally depend on dispositional primacy.').

omega_variable(
    publication_bias_magnitude,
    'What proportion of situational findings are suppressed by publication bias, reviewer skepticism, and editorial preferences for dispositional explanations?',
    'File-drawer analysis, pre-registration audits, and comparison of published versus unpublished effect sizes for dispositional versus situational predictors.',
    'Large publication bias would establish that the dispositional consensus is an artifact of suppression rather than empirical reality, supporting the extraction reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(publication_bias_magnitude, empirical, 'Whether the dispositional consensus reflects evidence or publication filtering.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(moral_causation_locus_flat_control, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mora_tr_t0, moral_causation_locus_flat_control, theater_ratio, 0, 0.28).
narrative_ontology:measurement(mora_tr_t10, moral_causation_locus_flat_control, theater_ratio, 10, 0.31).
narrative_ontology:measurement(mora_tr_t20, moral_causation_locus_flat_control, theater_ratio, 20, 0.34).
narrative_ontology:measurement(mora_tr_t30, moral_causation_locus_flat_control, theater_ratio, 30, 0.37).
narrative_ontology:measurement(mora_tr_t40, moral_causation_locus_flat_control, theater_ratio, 40, 0.39).
narrative_ontology:measurement(mora_tr_t50, moral_causation_locus_flat_control, theater_ratio, 50, 0.41).
narrative_ontology:measurement(mora_tr_t60, moral_causation_locus_flat_control, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(mora_be_t0, moral_causation_locus_flat_control, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(mora_be_t10, moral_causation_locus_flat_control, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(mora_be_t20, moral_causation_locus_flat_control, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(mora_be_t30, moral_causation_locus_flat_control, base_extractiveness, 30, 0.64).
narrative_ontology:measurement(mora_be_t40, moral_causation_locus_flat_control, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(mora_be_t50, moral_causation_locus_flat_control, base_extractiveness, 50, 0.67).
narrative_ontology:measurement(mora_be_t60, moral_causation_locus_flat_control, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mora_su_t0, moral_causation_locus_flat_control, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(mora_su_t10, moral_causation_locus_flat_control, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(mora_su_t20, moral_causation_locus_flat_control, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(mora_su_t30, moral_causation_locus_flat_control, suppression_requirement, 30, 0.67).
narrative_ontology:measurement(mora_su_t40, moral_causation_locus_flat_control, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(mora_su_t50, moral_causation_locus_flat_control, suppression_requirement, 50, 0.7).
narrative_ontology:measurement(mora_su_t60, moral_causation_locus_flat_control, suppression_requirement, 60, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(moral_causation_locus_flat_control, identity_coordination).
narrative_ontology:affects_constraint(moral_causation_locus_flat_control, criminal_culpability_standards).
narrative_ontology:affects_constraint(moral_causation_locus_flat_control, character_education_funding_allocation).
narrative_ontology:affects_constraint(moral_causation_locus_flat_control, poverty_attribution_frameworks).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
