% ============================================================================
% CONSTRAINT STORY: family_law_authority__muslim_shariat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__muslim_shariat_reading, []).

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
 *   constraint_id: family_law_authority__muslim_shariat_reading
 *   human_readable: Marriage as Civil Contract (Nikah) under Sharia Law
 *   domain: comparative_law/religious_governance
 *
 * SUMMARY:
 *   This constraint describes marriage as a civil contract (nikah) governed
 *   by Quranic injunctions and hadith, as interpreted by traditional Islamic
 *   jurisprudence. It is one reading of the broader 'family_law_authority'
 *   kernel. While providing a framework for social order and family
 *   stability, this reading is characterized by gender-asymmetric rights,
 *   particularly concerning divorce (historically, e.g., triple talaq) and
 *   polygyny, leading to substantial extraction from wives. Enforcement is
 *   active, relying on religious courts and strong community norms. The
 *   claimed type is 'tangled_rope' because it genuinely coordinates social
 *   functions but with clear asymmetric extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__muslim_shariat_reading, 0.8).
domain_priors:suppression_score(family_law_authority__muslim_shariat_reading, 0.85).
domain_priors:theater_ratio(family_law_authority__muslim_shariat_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__muslim_shariat_reading, "Marriage as Civil Contract (Nikah) under Sharia Law").
narrative_ontology:topic_domain(family_law_authority__muslim_shariat_reading, "comparative_law/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__muslim_shariat_reading, 'e7f824d1-1b34-4b30-889a-da7e526da3bf').
narrative_ontology:cs_kernel_codification('e7f824d1-1b34-4b30-889a-da7e526da3bf', fixed_text).
narrative_ontology:cs_authority_grounding('e7f824d1-1b34-4b30-889a-da7e526da3bf', lineage).
narrative_ontology:cs_interpretation_layer_present('e7f824d1-1b34-4b30-889a-da7e526da3bf').
narrative_ontology:cs_reading_relation('e7f824d1-1b34-4b30-889a-da7e526da3bf', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('e7f824d1-1b34-4b30-889a-da7e526da3bf', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('e7f824d1-1b34-4b30-889a-da7e526da3bf', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('e7f824d1-1b34-4b30-889a-da7e526da3bf', family_law_authority__secular_contractual_reading, coexists_with).
narrative_ontology:cs_axiom('e7f824d1-1b34-4b30-889a-da7e526da3bf', foundational, divine_revelation_supremacy).
narrative_ontology:cs_axiom_status(divine_revelation_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('e7f824d1-1b34-4b30-889a-da7e526da3bf', divine_revelation_supremacy, theological).
narrative_ontology:cs_axiom('e7f824d1-1b34-4b30-889a-da7e526da3bf', foundational, gender_complementarity_principle).
narrative_ontology:cs_axiom_status(gender_complementarity_principle, holdable).
narrative_ontology:cs_axiom_grounding('e7f824d1-1b34-4b30-889a-da7e526da3bf', gender_complementarity_principle, conventional).
narrative_ontology:cs_reference_frame('e7f824d1-1b34-4b30-889a-da7e526da3bf', classical_islamic_jurisprudence).
narrative_ontology:cs_drift_state('e7f824d1-1b34-4b30-889a-da7e526da3bf', contemporary_secular_challenges, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e7f824d1-1b34-4b30-889a-da7e526da3bf', '').
narrative_ontology:cs_kernel_id(family_law_authority__muslim_shariat_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, husbands).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, religious_authorities).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, community_members).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, wives).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, children_of_marriage).
narrative_ontology:constraint_vindicates(family_law_authority__muslim_shariat_reading, sharia_supremacy_doctrine).
narrative_ontology:constraint_vindicates(family_law_authority__muslim_shariat_reading, islamic_family_values).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from legal and social privileges, including unilateral divorce (historically, e.g., triple talaq) and polygyny. Bear maintenance obligations. Exit from marriage is relatively straightforward through talaq.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, husbands, beneficiary,
    organized, biographical, mobile, local).

% Bear the costs of gender-asymmetric rights, limited divorce access (historically, requiring khula or judicial divorce), and significant social pressure to conform. Often economically dependent. Exit carries severe social and economic penalties, including potential loss of custody or community standing.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, wives, payer,
    powerless, biographical, identity_locked, local).

% Interpret and enforce Sharia law, adjudicate marital disputes, and maintain the legitimacy of the system. Benefit from their authority, social standing, and the perpetuation of the religious legal framework.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, religious_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from the social stability, moral framework, and clear roles provided by the system. Conform to norms to maintain social standing and avoid ostracization. Exit from the community or its norms carries social costs.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, community_members, beneficiary,
    moderate, biographical, constrained, local).

% Advocate for gender-egalitarian family laws and challenge the religious framework's jurisdiction or specific provisions. Their proposals are often resisted by traditional authorities and face significant social inertia.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, secular_legal_reformers, excluded,
    organized, generational, analytical, national).

% Subject to the legal and social outcomes of their parents' marriage, including custody arrangements, inheritance, and social status. Their well-being is often secondary to parental rights or religious dictates, especially in cases of divorce or polygyny.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, children_of_marriage, payer,
    powerless, biographical, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Formalizes family units, regulates sexual relations, procreation, inheritance, and social order within the Islamic community, providing a framework for legitimate relationships and social stability.
% TRANSFER_FUNCTION: Transfers rights, obligations, property (e.g., mahr, inheritance), and social status between spouses and their families. It imposes duties of obedience and maintenance, primarily from husband to wife, and defines roles within the family structure.
% ABSENT_VOICES: Women's rights advocates, secular legal reformers, and individuals seeking gender-egalitarian marriage structures are often excluded from the interpretive and adjudicative processes. They would argue for reforms to address gender disparities and enhance individual autonomy.
% DISAPPEARANCE_RATIONALE: If the Sharia-governed marriage contract vanished overnight, the social, legal, and economic fabric of many Muslim-majority societies would undergo profound reorganization. Family structures, inheritance patterns, social legitimacy of relationships, and the authority of religious institutions would be fundamentally altered, leading to widespread uncertainty and new legal frameworks.
% FOUNDING_PROBLEM: Establishing social order, regulating procreation, ensuring legitimate lineage, facilitating property transfer, and providing a stable framework for family life and community cohesion in early Islamic societies.
% FOUNDING_PROBLEM_CORROBORATION: Religious scholars and traditional community leaders attest that the founding problems of social order and moral guidance remain live. However, secular critics, women's rights organizations, and some reformist scholars attest that while some problems persist, the current framework's application creates new problems of gender inequality and human rights violations, suggesting the founding problem is either solved or its solution has become extractive. This is supported by legislative reforms in many Muslim-majority countries.
narrative_ontology:disappearance_verdict(family_law_authority__muslim_shariat_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__muslim_shariat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__muslim_shariat_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(family_law_authority__muslim_shariat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__muslim_shariat_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__muslim_shariat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__muslim_shariat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__muslim_shariat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.8) reflects the significant costs borne by wives due to gender-asymmetric rights and limited agency within the marital contract, as well as the potential for exploitation in polygynous arrangements. Suppression (0.85) is high due to the combined force of religious injunctions, social stigma, economic dependency, and the limited availability of secular alternatives in many contexts. Theater ratio (0.2) is low to moderate, indicating that while the system has genuine functional aspects (e.g., formalizing lineage, inheritance), some elements of its enforcement may be performative in maintaining traditional power structures against modern critiques. The measurement series reflects a period where extractiveness and suppression remained high, with some minor increases as external pressures for reform mounted, requiring more active defense of the traditional system.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of husbands and religious authorities, the system is a divinely ordained and socially beneficial framework for family life, providing stability and moral guidance. From the perspective of wives and secular reformers, the same system operates as a mechanism of gendered extraction and suppression, limiting autonomy and imposing disproportionate burdens. The engine's per-seat classification will highlight this divergence based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Husbands and religious authorities are primary beneficiaries, gaining legal privileges, social status, and authority from the constraint. Wives and children are primary targets/payers, bearing the costs of asymmetric rights, limited exit options, and the social consequences of marital dissolution. Community members are beneficiaries of the social order but also payers through conformity. Secular legal reformers are excluded, as their proposals challenge the very foundation of the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_vs_interpretive_origin,
    'Is this constraint a divinely ordained, immutable framework, or a socio-historical interpretation of religious texts subject to reform?',
    'Theological and jurisprudential re-evaluation by authoritative religious bodies, or the emergence of widely accepted reformist interpretations that gain institutional traction.',
    'If divinely immutable, the constraint''s legitimacy is unchallengeable from within the religious framework, making reform difficult. If socio-historical, it opens the door for reinterpretation and reform based on contemporary ethical standards, potentially reducing extraction and suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_vs_interpretive_origin, conceptual, 'Ambiguity regarding the immutability vs. interpretability of Sharia family law.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal barriers, economic dependency) or internalized (social stigma, religious belief, identity fusion)?',
    'Post-exit suppression trajectory: if suppression (e.g., social ostracization, psychological distress) persists after legal/economic barriers are removed, it indicates a significant internalized component. Comparative studies of women''s experiences in secular vs. religious legal contexts.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest, and addressing it requires more than legal reform; it necessitates social and cultural shifts. If primarily structural, legal reforms would have a more direct and immediate impact on reducing suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for wives.').

omega_variable(
    mahr_function_ambiguity,
    'Is the mahr (dower) primarily a protective measure for wives, ensuring financial security, or a symbolic transfer that reinforces patriarchal structures and economic dependency?',
    'Empirical studies on the actual financial impact of mahr on women''s economic independence post-divorce, and analysis of its enforceability in different jurisdictions.',
    'If genuinely protective and enforceable, mahr mitigates some of the economic extraction from wives. If symbolic or poorly enforced, it contributes to economic vulnerability and reinforces the extractive nature of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mahr_function_ambiguity, conceptual, 'The actual function and impact of mahr in mitigating or reinforcing extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__muslim_shariat_reading, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1950, family_law_authority__muslim_shariat_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(fami_tr_t1960, family_law_authority__muslim_shariat_reading, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(fami_tr_t1970, family_law_authority__muslim_shariat_reading, theater_ratio, 1970, 0.14).
narrative_ontology:measurement(fami_tr_t1980, family_law_authority__muslim_shariat_reading, theater_ratio, 1980, 0.16).
narrative_ontology:measurement(fami_tr_t1990, family_law_authority__muslim_shariat_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(fami_tr_t2000, family_law_authority__muslim_shariat_reading, theater_ratio, 2000, 0.19).
narrative_ontology:measurement(fami_tr_t2010, family_law_authority__muslim_shariat_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(fami_tr_t2020, family_law_authority__muslim_shariat_reading, theater_ratio, 2020, 0.2).

% Extraction over time
narrative_ontology:measurement(fami_be_t1950, family_law_authority__muslim_shariat_reading, base_extractiveness, 1950, 0.7).
narrative_ontology:measurement(fami_be_t1960, family_law_authority__muslim_shariat_reading, base_extractiveness, 1960, 0.72).
narrative_ontology:measurement(fami_be_t1970, family_law_authority__muslim_shariat_reading, base_extractiveness, 1970, 0.75).
narrative_ontology:measurement(fami_be_t1980, family_law_authority__muslim_shariat_reading, base_extractiveness, 1980, 0.77).
narrative_ontology:measurement(fami_be_t1990, family_law_authority__muslim_shariat_reading, base_extractiveness, 1990, 0.78).
narrative_ontology:measurement(fami_be_t2000, family_law_authority__muslim_shariat_reading, base_extractiveness, 2000, 0.79).
narrative_ontology:measurement(fami_be_t2010, family_law_authority__muslim_shariat_reading, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(fami_be_t2020, family_law_authority__muslim_shariat_reading, base_extractiveness, 2020, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1950, family_law_authority__muslim_shariat_reading, suppression_requirement, 1950, 0.8).
narrative_ontology:measurement(fami_su_t1960, family_law_authority__muslim_shariat_reading, suppression_requirement, 1960, 0.81).
narrative_ontology:measurement(fami_su_t1970, family_law_authority__muslim_shariat_reading, suppression_requirement, 1970, 0.82).
narrative_ontology:measurement(fami_su_t1980, family_law_authority__muslim_shariat_reading, suppression_requirement, 1980, 0.83).
narrative_ontology:measurement(fami_su_t1990, family_law_authority__muslim_shariat_reading, suppression_requirement, 1990, 0.84).
narrative_ontology:measurement(fami_su_t2000, family_law_authority__muslim_shariat_reading, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement(fami_su_t2010, family_law_authority__muslim_shariat_reading, suppression_requirement, 2010, 0.85).
narrative_ontology:measurement(fami_su_t2020, family_law_authority__muslim_shariat_reading, suppression_requirement, 2020, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__muslim_shariat_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, inheritance_laws_sharia_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, child_custody_sharia_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, womens_economic_rights_sharia_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
