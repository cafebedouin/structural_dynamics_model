% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__muslim_shariat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__muslim_shariat_reading, []).

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
 *   constraint_id: marriage_authority_kernel__muslim_shariat_reading
 *   human_readable: Muslim Personal Law (Shariat) Application Act, 1937 as interpreted by Muslim Personal Law Boards and Qazis
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   This constraint describes the authority of Muslim personal law in India,
 *   specifically how Shariat is interpreted and applied by Muslim personal
 *   law boards and qazis in matters of marriage, divorce, and inheritance.
 *   This is one reading of the broader 'marriage_authority_kernel' in India,
 *   which encompasses multiple religious and secular legal systems. This
 *   reading is characterized by community-based adjudication and traditional
 *   interpretations that often result in lower gender equity compared to
 *   secular or some other religious codes, leading to significant extraction
 *   from female Muslim spouses. State intervention in these matters is often
 *   contested, reinforcing the autonomy of these personal law systems.
 *
 * KEY AGENTS:
 *   - male_muslim_spouses: Primary beneficiary (moderate/constrained) — benefits from traditional interpretations.
 *   - female_muslim_spouses: Primary target (powerless/identity_locked) — bears costs of gender inequality.
 *   - muslim_personal_law_boards: Agenda-setter (institutional/constrained) — interprets and administers Shariat.
 *   - qazis: Agenda-setter (organized/constrained) — adjudicates disputes at local level.
 *   - children_of_muslim_marriages: Secondary target (powerless/trapped) — indirectly affected by marital instability.
 *   - indian_civil_courts: Observer (institutional/analytical) — intervenes in cases of severe injustice.
 *   - secular_civil_society_advocates: Excluded (organized/constrained) — advocates for uniform civil code.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__muslim_shariat_reading, 0.78).
domain_priors:suppression_score(marriage_authority_kernel__muslim_shariat_reading, 0.85).
domain_priors:theater_ratio(marriage_authority_kernel__muslim_shariat_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__muslim_shariat_reading, "Muslim Personal Law (Shariat) Application Act, 1937 as interpreted by Muslim Personal Law Boards and Qazis").
narrative_ontology:topic_domain(marriage_authority_kernel__muslim_shariat_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__muslim_shariat_reading, '464fe4d6-c2f9-4642-be4b-57f1628e79bc').
narrative_ontology:cs_kernel_codification('464fe4d6-c2f9-4642-be4b-57f1628e79bc', formalized).
narrative_ontology:cs_authority_grounding('464fe4d6-c2f9-4642-be4b-57f1628e79bc', lineage).
narrative_ontology:cs_interpretation_layer_present('464fe4d6-c2f9-4642-be4b-57f1628e79bc').
narrative_ontology:cs_reading_relation('464fe4d6-c2f9-4642-be4b-57f1628e79bc', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('464fe4d6-c2f9-4642-be4b-57f1628e79bc', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('464fe4d6-c2f9-4642-be4b-57f1628e79bc', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('464fe4d6-c2f9-4642-be4b-57f1628e79bc', marriage_authority_kernel__secular_civil_reading, coexists_with).
narrative_ontology:cs_axiom('464fe4d6-c2f9-4642-be4b-57f1628e79bc', foundational, shariat_divine_unalterable).
narrative_ontology:cs_axiom_status(shariat_divine_unalterable, holdable).
narrative_ontology:cs_axiom_grounding('464fe4d6-c2f9-4642-be4b-57f1628e79bc', shariat_divine_unalterable, theological).
narrative_ontology:cs_axiom('464fe4d6-c2f9-4642-be4b-57f1628e79bc', foundational, community_autonomy_in_personal_law).
narrative_ontology:cs_axiom_status(community_autonomy_in_personal_law, holdable).
narrative_ontology:cs_axiom_grounding('464fe4d6-c2f9-4642-be4b-57f1628e79bc', community_autonomy_in_personal_law, conventional).
narrative_ontology:cs_reference_frame('464fe4d6-c2f9-4642-be4b-57f1628e79bc', traditional_shariat_interpretation).
narrative_ontology:cs_drift_state('464fe4d6-c2f9-4642-be4b-57f1628e79bc', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('464fe4d6-c2f9-4642-be4b-57f1628e79bc', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, male_muslim_spouses).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, muslim_personal_law_boards).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, qazis).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, female_muslim_spouses).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, children_of_muslim_marriages).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from provisions like unilateral talaq (divorce), polygamy, and preferential inheritance rights, as interpreted by personal law boards. Their exit options are constrained by social norms and the legal framework, but they hold more agency than female spouses.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, male_muslim_spouses, beneficiary,
    moderate, biographical, constrained, national).

% Bear the costs of gender-unequal provisions, including limited divorce rights, vulnerability to unilateral talaq, and reduced inheritance. Their exit options are severely constrained by social stigma, economic dependency, and the lack of alternative legal recourse within the community, often leading to identity-lock.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, female_muslim_spouses, payer,
    powerless, biographical, identity_locked, national).

% Interpret and administer Shariat law concerning marriage, divorce, and inheritance. They derive authority from religious tradition and community acceptance, acting as de facto adjudicators. Their role is to preserve the traditional interpretation of Shariat.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_personal_law_boards, agenda_setter,
    institutional, generational, constrained, national).

% Religious judges who solemnize marriages and adjudicate disputes under Muslim personal law. They enforce the interpretations provided by the personal law boards and community norms. Their authority is localized but significant.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, qazis, agenda_setter,
    organized, biographical, constrained, local).

% Are indirectly affected by the stability and equity of their parents' marital arrangements, including issues of maintenance, custody, and inheritance, which are governed by these personal laws. They have no agency in the system.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, children_of_muslim_marriages, payer,
    powerless, biographical, trapped, national).

% Are constitutionally mandated to uphold fundamental rights but often defer to personal laws in family matters, intervening only in cases of severe injustice or when specific statutory provisions are challenged. Their role is to balance religious freedom with individual rights.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, indian_civil_courts, observer,
    institutional, generational, analytical, national).

% Advocate for a uniform civil code and gender-equitable family laws, challenging the constitutional validity of certain personal law provisions. They are excluded from the direct interpretation and administration of Muslim personal law but exert pressure through public discourse and litigation.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, secular_civil_society_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for marriage, divorce, and inheritance for the Muslim community, ensuring social order and religious adherence according to Shariat principles, as understood by the community's religious authorities.
% TRANSFER_FUNCTION: Transfers rights and obligations, particularly regarding marital agency, divorce initiation, and inheritance, from female Muslim spouses to male Muslim spouses, and vests interpretive authority in Muslim personal law boards and qazis.
% ABSENT_VOICES: Female Muslim spouses and secular civil society advocates are largely excluded from the interpretive and adjudicatory processes of Muslim personal law boards and qazis. They would advocate for gender equality and individual rights within family law, challenging traditional interpretations.
% DISAPPEARANCE_RATIONALE: If this system of personal law authority vanished overnight, the Muslim community would face a legal vacuum for family matters, leading to widespread confusion, social disruption, and a scramble for alternative legal frameworks, likely resulting in a shift towards secular civil law or new community-based interpretations.
% FOUNDING_PROBLEM: To allow the Muslim community in India to govern its personal matters (marriage, divorce, inheritance) according to its religious tenets (Shariat), preserving religious identity and cultural practices under colonial and post-colonial rule.
% FOUNDING_PROBLEM_CORROBORATION: Muslim personal law boards and many male Muslim spouses attest that the problem of preserving religious identity and traditional practices is still live. Female Muslim spouses, secular civil society advocates, and some legal scholars argue that while religious identity is important, the current interpretation of personal law has become a tool for gender inequality, indicating the founding problem's original intent has been distorted or superseded by new social realities.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__muslim_shariat_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__muslim_shariat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__muslim_shariat_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_authority_kernel__muslim_shariat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__muslim_shariat_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__muslim_shariat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__muslim_shariat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__muslim_shariat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.78) is high due to the significant gender asymmetry in rights, particularly concerning divorce and inheritance, which disproportionately disadvantages female spouses. Suppression (0.85) is also high, as female spouses face strong social pressure, economic dependency, and limited legal alternatives to challenge these interpretations, leading to identity-lock. The theater ratio (0.15) is low because the system is actively functional in its stated purpose of administering personal law, even if its outcomes are contested. Resistance (0.7) is substantial, primarily from female spouses and secular advocates, but it faces strong institutional and social barriers. Accessibility collapse (0.65) is moderate, as some legal challenges are possible, but direct alternatives within the community are limited.
 *
 * PERSPECTIVAL GAP:
 *   Male Muslim spouses and the personal law boards perceive this system as a legitimate and necessary preservation of religious identity and community autonomy, viewing its coordination function as paramount. Female Muslim spouses and secular advocates, however, experience it as a highly extractive and suppressive system that perpetuates gender inequality under the guise of religious tradition. The engine's classification will reflect this divergence based on the declared structural positions and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Male Muslim spouses are beneficiaries due to their privileged position in divorce and inheritance. Female Muslim spouses are clear victims, facing severe constraints and identity-lock. Muslim personal law boards and qazis are agenda-setters, benefiting from their authority and role in maintaining the system. Children are victims of the system's inequities. Indian civil courts are observers, and secular civil society advocates are excluded, as they challenge the system from outside its interpretive framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to preserve religious identity and community autonomy is contested. While it still serves a coordination function for the community, the high extractiveness and suppression, particularly against female spouses, suggest that the coordination narrative increasingly covers asymmetric extraction. The persistence of the system is due to the entrenched authority of the personal law boards and social norms, rather than universal consent or a purely coordinative function. The classification as a Tangled Rope reflects this hybrid nature, where a genuine coordination function is intertwined with significant asymmetric extraction requiring active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    shariat_interpretation_flexibility,
    'Is the current interpretation of Shariat by Muslim personal law boards the only valid one, or are more gender-equitable interpretations possible and legitimate within Islamic jurisprudence?',
    'Comparative theological and legal scholarship on diverse Islamic legal traditions, and the emergence of new fatwas or judicial rulings from within the community that adopt more equitable interpretations.',
    'If more equitable interpretations are recognized as legitimate, the extractiveness and suppression of the constraint could decrease, potentially shifting its classification towards a Rope or even a Scaffold if transitional reforms are adopted. If the current interpretation is deemed immutable, the extractive nature is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shariat_interpretation_flexibility, conceptual, 'Ambiguity regarding the flexibility and diversity of Shariat interpretations.').

omega_variable(
    state_intervention_legitimacy,
    'To what extent is state intervention (e.g., through a Uniform Civil Code or judicial review) in Muslim personal law legitimate and effective in upholding constitutional rights without infringing on religious freedom?',
    'Legal precedents from high court and Supreme Court rulings, legislative debates, and empirical studies on the impact of state interventions on both individual rights and community cohesion.',
    'If state intervention is deemed legitimate and effective, it could reduce suppression and extractiveness by providing alternative legal recourse, potentially reclassifying the constraint. If intervention is seen as illegitimate or ineffective, the current system''s persistence is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_intervention_legitimacy, preference, 'The contested legitimacy and effectiveness of state intervention in religious personal laws.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, economic dependency) or internalized (social stigma, religious identity fusion) for female Muslim spouses?',
    'Post-exit suppression trajectory: if suppression persists after legal/economic barriers are removed (e.g., through support systems for ex-spouses), reclassify as partially internalized. If it dissipates, it''s primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — female spouses carry the suppression with them after exit, making reform more complex. If structural, legal reforms alone could be more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for female Muslim spouses.').

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading of the ''marriage_authority_kernel''. This reading is ''muslim_shariat_reading''. Sibling readings include ''hindu_codified_reading'', ''christian_canonical_reading'', ''parsi_communal_reading'', and ''secular_civil_reading''. What would change structurally if a sibling reading were adopted?',
    'Comparative legal analysis of the specific provisions and adjudicatory mechanisms of each sibling reading, focusing on differences in gender equity, judicial oversight, and community autonomy.',
    'Adopting the ''secular_civil_reading'' would likely lead to higher gender equity and state oversight, reducing extraction and suppression. Adopting other religious readings would shift the specific provisions but might retain similar levels of community autonomy and potentially different forms of gender asymmetry. The core disagreement is located in the source of legal authority and the interpretation of gender roles within marriage.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is a specific reading of the marriage authority kernel, with distinct structural implications compared to its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__muslim_shariat_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(marr_tr_t10, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(marr_tr_t20, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement(marr_tr_t30, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement(marr_tr_t40, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(marr_tr_t50, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement(marr_tr_t60, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 60, 0.15).
narrative_ontology:measurement(marr_tr_t70, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 70, 0.15).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(marr_be_t10, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(marr_be_t20, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(marr_be_t30, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 30, 0.73).
narrative_ontology:measurement(marr_be_t40, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 40, 0.75).
narrative_ontology:measurement(marr_be_t50, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 50, 0.77).
narrative_ontology:measurement(marr_be_t60, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 60, 0.78).
narrative_ontology:measurement(marr_be_t70, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 70, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(marr_su_t10, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(marr_su_t20, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(marr_su_t30, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 30, 0.82).
narrative_ontology:measurement(marr_su_t40, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 40, 0.83).
narrative_ontology:measurement(marr_su_t50, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 50, 0.84).
narrative_ontology:measurement(marr_su_t60, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 60, 0.85).
narrative_ontology:measurement(marr_su_t70, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 70, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__muslim_shariat_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_authority_kernel'. Its ε value differs significantly from other readings due to its specific interpretation of Shariat and its impact on gender equity. It is linked to other readings as part of a constraint family that collectively defines marriage authority in India.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
