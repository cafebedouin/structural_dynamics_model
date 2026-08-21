% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__literal_hierarchical
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__literal_hierarchical, []).

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
 *   constraint_id: quranic_gender_verses__literal_hierarchical
 *   human_readable: Quranic Gender Verses: Literal Hierarchical Reading
 *   domain: islamic_jurisprudence/legal_hermeneutics/gender_studies
 *
 * SUMMARY:
 *   This constraint represents the 'literal_hierarchical' reading of specific
 *   Quranic verses (4:11, 2:282, 4:34) that establish male guardianship and
 *   differentiated rights as divine ordinance. This reading interprets these
 *   verses as direct, timeless legal constraints. It is one reading of the
 *   'quranic_gender_verses' kernel, distinct from 'contextual_egalitarian'
 *   and 'progressive_abrogation' readings. The structural delta for this
 *   reading is high base extractiveness, with male household heads and
 *   religious courts gaining authority and resource control, while women
 *   enter the victim set with constrained rights and high exit costs.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, 0.88).
domain_priors:suppression_score(quranic_gender_verses__literal_hierarchical, 0.92).
domain_priors:theater_ratio(quranic_gender_verses__literal_hierarchical, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, extractiveness, 0.88).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__literal_hierarchical, snare).
narrative_ontology:human_readable(quranic_gender_verses__literal_hierarchical, "Quranic Gender Verses: Literal Hierarchical Reading").
narrative_ontology:topic_domain(quranic_gender_verses__literal_hierarchical, "islamic_jurisprudence/legal_hermeneutics/gender_studies").

domain_priors:requires_active_enforcement(quranic_gender_verses__literal_hierarchical).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__literal_hierarchical, '227706bd-374a-4d5e-863f-1e3f579468fc').
narrative_ontology:cs_kernel_codification('227706bd-374a-4d5e-863f-1e3f579468fc', fixed_text).
narrative_ontology:cs_authority_grounding('227706bd-374a-4d5e-863f-1e3f579468fc', lineage).
narrative_ontology:cs_interpretation_layer_present('227706bd-374a-4d5e-863f-1e3f579468fc').
narrative_ontology:cs_reading_relation('227706bd-374a-4d5e-863f-1e3f579468fc', quranic_gender_verses__contextual_egalitarian, coexists_with).
narrative_ontology:cs_reading_relation('227706bd-374a-4d5e-863f-1e3f579468fc', quranic_gender_verses__progressive_abrogation, coexists_with).
narrative_ontology:cs_axiom('227706bd-374a-4d5e-863f-1e3f579468fc', foundational, verses_are_timeless_legal_injunctions).
narrative_ontology:cs_axiom_status(verses_are_timeless_legal_injunctions, holdable).
narrative_ontology:cs_axiom_grounding('227706bd-374a-4d5e-863f-1e3f579468fc', verses_are_timeless_legal_injunctions, theological).
narrative_ontology:cs_axiom('227706bd-374a-4d5e-863f-1e3f579468fc', foundational, male_guardianship_is_divine_ordinance).
narrative_ontology:cs_axiom_status(male_guardianship_is_divine_ordinance, holdable).
narrative_ontology:cs_axiom_grounding('227706bd-374a-4d5e-863f-1e3f579468fc', male_guardianship_is_divine_ordinance, theological).
narrative_ontology:cs_reference_frame('227706bd-374a-4d5e-863f-1e3f579468fc', classical_islamic_jurisprudence).
narrative_ontology:cs_drift_state('227706bd-374a-4d5e-863f-1e3f579468fc', contemporary_global_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('227706bd-374a-4d5e-863f-1e3f579468fc', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__literal_hierarchical, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, male_household_heads).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, religious_courts).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, conservative_clergy).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, women_in_literalist_jurisdictions).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, daughters_in_inheritance).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, female_litigants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain structural authority and resource control within the household, including financial guardianship and disciplinary rights, as divinely ordained. Benefits from the legal and social enforcement of these roles.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, male_household_heads, beneficiary,
    powerful, biographical, mobile, local).

% Interpret and enforce these verses as timeless legal constraints, solidifying their authority in family law, inheritance, and personal status. Their legitimacy is tied to upholding this literalist interpretation.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, religious_courts, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the social and religious authority derived from upholding and teaching this literalist interpretation. Their influence is amplified by the perceived divine origin of these gender roles.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, conservative_clergy, beneficiary,
    organized, generational, constrained, regional).

% Bear the costs of constrained inheritance rights (half of male kin), reduced weight of testimony in court, and limited legal autonomy under male guardianship. Exit is difficult due to social stigma, legal barriers, and identity fusion with religious community.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, women_in_literalist_jurisdictions, payer,
    powerless, biographical, identity_locked, local).

% Directly receive a smaller share of inheritance compared to male siblings, as stipulated by the literal reading of 4:11. They have no legal recourse within this framework to challenge this distribution.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, daughters_in_inheritance, payer,
    powerless, immediate, trapped, local).

% Face challenges in legal proceedings where their testimony may be valued less than that of men (based on interpretations of 2:282) or where male guardianship (4:34) limits their standing. Their legal outcomes are directly impacted.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, female_litigants, payer,
    powerless, immediate, constrained, local).

% Propose alternative interpretations emphasizing equity and historical context, but their views are often marginalized or suppressed within literalist religious institutions and jurisdictions. They face professional and social costs for challenging the dominant reading.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, contextual_egalitarian_scholars, excluded,
    moderate, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, divinely sanctioned social and legal hierarchy within families and society, aiming to reduce ambiguity in roles and responsibilities, particularly regarding financial support and authority.
% TRANSFER_FUNCTION: Transfers authority, legal precedence, and a greater share of material resources (e.g., inheritance) from women to men, based on a literal interpretation of specific Quranic verses.
% ABSENT_VOICES: Scholars and activists advocating for egalitarian interpretations, as well as women's rights organizations, are often excluded from official religious discourse and legal reform processes in jurisdictions where this literalist reading is dominant. They would argue for interpretations aligned with universal human dignity and gender equality.
% DISAPPEARANCE_RATIONALE: If this literal hierarchical interpretation vanished overnight, the legal and social structures in many Muslim-majority societies would undergo profound rearrangement. Family laws, inheritance codes, and the authority of religious courts would be fundamentally challenged, leading to significant shifts in gender roles, economic distribution, and individual autonomy for women.
% FOUNDING_PROBLEM: The verses were revealed in a 7th-century Arabian context to establish a framework for social order, family structure, and legal justice, addressing issues of inheritance, marital relations, and testimony in a society undergoing significant transformation.
% FOUNDING_PROBLEM_CORROBORATION: The literalist proponents (male household heads, religious courts, conservative clergy) assert the founding problem of maintaining divine order and social stability is live and timeless. Critics (contextual egalitarian scholars, women's rights advocates) argue that the specific hierarchical solutions are historically contingent and the 'problem' as framed by literalists serves to maintain existing power structures. Independent sociological and historical analyses often support the latter view, highlighting the evolving nature of social problems and legal solutions.
narrative_ontology:disappearance_verdict(quranic_gender_verses__literal_hierarchical, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__literal_hierarchical, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__literal_hierarchical, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(quranic_gender_verses__literal_hierarchical, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__literal_hierarchical, 0.88, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__literal_hierarchical_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quranic_gender_verses__literal_hierarchical_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.88) because this reading directly mandates an asymmetric distribution of rights and resources, favoring men. Suppression is very high (0.92) as this interpretation is often backed by state legal systems and deeply ingrained social norms, making exit or resistance extremely difficult for women, who are often identity-locked within their religious and familial communities. Theater ratio is low (0.1) because the enforcement of these verses is direct and functional, not performative; the hierarchy is genuinely maintained. Accessibility collapse is high (0.75) as alternative legal frameworks or interpretations are largely suppressed in jurisdictions adhering to this reading. Resistance is moderate (0.3) but growing, primarily from external and marginalized internal voices.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of male beneficiaries and religious authorities, this constraint is a divinely ordained, stable social order. From the perspective of women and egalitarian scholars, it is a deeply extractive and suppressive system. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Male household heads, religious courts, and conservative clergy are clear beneficiaries, gaining authority and control. Women, particularly in inheritance and legal contexts, are the primary victims, bearing the costs of reduced rights and autonomy. Their 'identity_locked' exit option reflects the profound social and religious consequences of challenging these norms.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_ordinance_vs_human_interpretation,
    'Is the hierarchical structure derived from these verses a direct, immutable divine ordinance, or is it a human interpretation shaped by historical and cultural contexts?',
    'Comparative theological and hermeneutical analysis across diverse Islamic traditions, examining the methodologies of interpretation and their historical evolution, alongside empirical studies of societal impact.',
    'If primarily human interpretation, the constraint''s ''emerges_naturally'' claim is false, reclassifying it from a perceived Mountain (by proponents) to a Snare or Tangled Rope. If immutable divine ordinance, its perceived legitimacy by adherents remains high, making reform efforts more challenging.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_ordinance_vs_human_interpretation, conceptual, 'Ambiguity between divine command and human interpretive framework.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, economic dependency) or internalized (cognitive patterns, identity fusion with religious community)?',
    'Post-exit suppression trajectory: if suppression persists after legal/economic barriers are removed (e.g., in diaspora communities or secular states), reclassify as partially internalized. Sociological studies on identity formation and religious socialization.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — women carry the suppression with them after exit, making true liberation more complex. If purely structural, legal reforms alone would be sufficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for women.').

omega_variable(
    mandatrophy_of_guardianship,
    'Has the original protective function of male guardianship (qawamah) atrophied, becoming primarily an extractive mechanism, or does it still serve a genuine protective role in contemporary contexts?',
    'Empirical studies comparing outcomes for women under guardianship vs. those with full autonomy in similar socioeconomic contexts, assessing safety, economic well-being, and legal protection. Historical analysis of the evolution of qawamah''s application.',
    'If atrophied, the constraint''s ''coordination_function'' is largely theatrical, pushing it further towards a Snare classification. If a genuine protective function remains, it retains a Tangled Rope aspect, albeit with high extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_of_guardianship, empirical, 'Whether male guardianship''s protective mandate has atrophied into extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__literal_hierarchical, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quranic_gender_verses__literal_hierarchical, theater_ratio, 0, 0.05).
narrative_ontology:measurement(qura_tr_t350, quranic_gender_verses__literal_hierarchical, theater_ratio, 350, 0.08).
narrative_ontology:measurement(qura_tr_t700, quranic_gender_verses__literal_hierarchical, theater_ratio, 700, 0.1).
narrative_ontology:measurement(qura_tr_t1050, quranic_gender_verses__literal_hierarchical, theater_ratio, 1050, 0.1).
narrative_ontology:measurement(qura_tr_t1400, quranic_gender_verses__literal_hierarchical, theater_ratio, 1400, 0.1).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quranic_gender_verses__literal_hierarchical, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(qura_be_t350, quranic_gender_verses__literal_hierarchical, base_extractiveness, 350, 0.85).
narrative_ontology:measurement(qura_be_t700, quranic_gender_verses__literal_hierarchical, base_extractiveness, 700, 0.88).
narrative_ontology:measurement(qura_be_t1050, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1050, 0.88).
narrative_ontology:measurement(qura_be_t1400, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1400, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quranic_gender_verses__literal_hierarchical, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(qura_su_t350, quranic_gender_verses__literal_hierarchical, suppression_requirement, 350, 0.85).
narrative_ontology:measurement(qura_su_t700, quranic_gender_verses__literal_hierarchical, suppression_requirement, 700, 0.9).
narrative_ontology:measurement(qura_su_t1050, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1050, 0.92).
narrative_ontology:measurement(qura_su_t1400, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1400, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__literal_hierarchical, identity_coordination).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, islamic_family_law_codes).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, womens_legal_autonomy_in_islamic_jurisdictions).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'quranic_gender_verses' kernel. It represents the literalist, hierarchical interpretation, which directly influences Islamic family law codes and women's legal autonomy. It coexists with, but is structurally distinct from, 'contextual_egalitarian' and 'progressive_abrogation' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
