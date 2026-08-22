% ============================================================================
% CONSTRAINT STORY: marriage_authority__gender_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__gender_rights_reading, []).

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
 *   constraint_id: marriage_authority__gender_rights_reading
 *   human_readable: Judicial Expansion of Constitutional Gender Equality in Personal Law
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   This constraint represents the 'gender rights' reading of marriage
 *   authority, where judicial bodies actively interpret constitutional
 *   equality guarantees to reform discriminatory practices within personal
 *   laws (e.g., triple talaq, maintenance, property rights). It is a snare
 *   because it targets specific practices that extract from women within
 *   patriarchal systems, and its persistence relies on active judicial
 *   enforcement against resistance from conservative communal authorities.
 *   The claimed type is 'snare' because the primary function is to dismantle
 *   existing extractive structures, rather than to coordinate a new,
 *   universally beneficial arrangement.
 *
 * KEY AGENTS:
 *   - women_rights_advocates: Primary beneficiary (organized/constrained) — drive litigation for reform.
 *   - progressive_judiciary: Agenda setter (institutional/analytical) — interprets and enforces constitutional equality.
 *   - women_within_patriarchal_personal_law: Primary victim (powerless/identity_locked) — bear the costs of discriminatory practices.
 *   - conservative_religious_authorities: Payer (organized/constrained) — resist judicial intervention.
 *   - secularist_political_parties: Excluded (powerful/constrained) — advocate for a different reform mechanism (UCC).
 *   - communal_leaders: Excluded (organized/constrained) — defend traditional personal law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, 0.85).
domain_priors:suppression_score(marriage_authority__gender_rights_reading, 0.78).
domain_priors:theater_ratio(marriage_authority__gender_rights_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__gender_rights_reading, snare).
narrative_ontology:human_readable(marriage_authority__gender_rights_reading, "Judicial Expansion of Constitutional Gender Equality in Personal Law").
narrative_ontology:topic_domain(marriage_authority__gender_rights_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__gender_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__gender_rights_reading, '73de8760-da14-4de8-a413-cc53e7be9aa0').
narrative_ontology:cs_kernel_codification('73de8760-da14-4de8-a413-cc53e7be9aa0', fixed_text).
narrative_ontology:cs_authority_grounding('73de8760-da14-4de8-a413-cc53e7be9aa0', lineage).
narrative_ontology:cs_interpretation_layer_present('73de8760-da14-4de8-a413-cc53e7be9aa0').
narrative_ontology:cs_reading_relation('73de8760-da14-4de8-a413-cc53e7be9aa0', marriage_authority__communal_autonomy_reading, influences).
narrative_ontology:cs_reading_relation('73de8760-da14-4de8-a413-cc53e7be9aa0', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_reading_relation('73de8760-da14-4de8-a413-cc53e7be9aa0', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_reading_relation('73de8760-da14-4de8-a413-cc53e7be9aa0', marriage_authority__judicial_harmonization_reading, coexists_with).
narrative_ontology:cs_axiom('73de8760-da14-4de8-a413-cc53e7be9aa0', foundational, constitutional_equality_supremacy).
narrative_ontology:cs_axiom_status(constitutional_equality_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('73de8760-da14-4de8-a413-cc53e7be9aa0', constitutional_equality_supremacy, deontological).
narrative_ontology:cs_axiom('73de8760-da14-4de8-a413-cc53e7be9aa0', foundational, gender_justice_as_fundamental_right).
narrative_ontology:cs_axiom_status(gender_justice_as_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('73de8760-da14-4de8-a413-cc53e7be9aa0', gender_justice_as_fundamental_right, deontological).
narrative_ontology:cs_reference_frame('73de8760-da14-4de8-a413-cc53e7be9aa0', constitutional_equality_as_supreme_law).
narrative_ontology:cs_drift_state('73de8760-da14-4de8-a413-cc53e7be9aa0', contemporary_human_rights_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('73de8760-da14-4de8-a413-cc53e7be9aa0', '').
narrative_ontology:cs_kernel_id(marriage_authority__gender_rights_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, women_rights_advocates).
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, progressive_judiciary).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, conservative_religious_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively litigate for gender equality within personal law, seeing judicial intervention as a necessary mechanism to reform discriminatory practices. They benefit from favorable court rulings that expand constitutional guarantees.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_rights_advocates, beneficiary,
    organized, generational, constrained, national).

% Interprets constitutional equality guarantees to apply to personal law, incrementally reforming practices like triple talaq, maintenance, and property rights. They set the agenda for reform through case law, often facing political backlash.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, progressive_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Are the primary targets of discriminatory personal law practices, bearing the costs of unequal divorce, inheritance, and maintenance rules. While judicial reforms offer some relief, access to justice remains challenging, and social pressures often limit their ability to claim rights.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law, payer,
    powerless, biographical, identity_locked, local).

% Resist judicial intervention in personal law, viewing it as an infringement on communal autonomy and religious freedom. They bear the cost of losing interpretive authority and control over community norms, often mobilizing political and social resistance.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, conservative_religious_authorities, payer,
    organized, generational, constrained, national).

% Advocate for a Uniform Civil Code to replace all personal laws, but their legislative efforts are often stalled due to political sensitivities. While they align with gender equality goals, their preferred method (legislative UCC) is distinct from judicial incrementalism.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, secularist_political_parties, excluded,
    powerful, generational, constrained, national).

% Represent community interests and traditions, often opposing judicial reforms that they perceive as undermining their authority and cultural identity. Their voices are often heard in political discourse but are not directly part of the judicial process of constitutional interpretation.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, communal_leaders, excluded,
    organized, generational, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Attempts to harmonize diverse personal law systems with constitutional guarantees of gender equality, providing a common legal floor for women's rights across different communities.
% TRANSFER_FUNCTION: Transfers interpretive authority over personal law from religious/communal bodies to the constitutional judiciary, and aims to transfer rights and protections to women from patriarchal community structures.
% ABSENT_VOICES: Secularist political parties, who advocate for a Uniform Civil Code, and communal leaders, who defend traditional personal law, are often excluded from the direct judicial discourse, though their influence is felt in the political sphere surrounding court decisions.
% DISAPPEARANCE_RATIONALE: If judicial expansion of gender equality guarantees vanished, the legal landscape for women within personal law would revert to more discriminatory practices, leading to significant social and legal upheaval, and a loss of protections gained over decades of litigation.
% FOUNDING_PROBLEM: The existence of multiple personal laws, often rooted in patriarchal traditions, led to significant gender inequality and discrimination against women within various communities, contradicting constitutional promises of equality.
% FOUNDING_PROBLEM_CORROBORATION: Women's rights organizations, international human rights bodies, and independent legal scholars consistently attest that the problem of gender inequality within personal law remains live, despite judicial interventions. Their reports and advocacy provide corroboration from outside the immediate judicial or religious authority structures.
narrative_ontology:disappearance_verdict(marriage_authority__gender_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__gender_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__gender_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_authority__gender_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__gender_rights_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__gender_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__gender_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the constraint directly confronts and aims to dismantle deeply entrenched, highly extractive patriarchal practices within personal law. Suppression (0.78) is also high, reflecting the active judicial enforcement required to overcome significant social and political resistance to these reforms. The theater ratio is low (0.20) because the judicial actions are genuinely aimed at substantive reform, not merely performative. The increasing extractiveness over time reflects the judiciary's growing assertiveness in tackling more sensitive and deeply entrenched discriminatory practices.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of women's rights advocates and the progressive judiciary, this constraint is a necessary intervention to secure fundamental rights. From the perspective of conservative religious authorities and communal leaders, it is an overreach that undermines religious freedom and communal autonomy. The engine's classification as a snare from the victim's seat captures the extractive nature of the practices being challenged, while the beneficiary seat might experience it as a rope or scaffold, providing necessary support for rights.
 *
 * DIRECTIONALITY LOGIC:
 *   Women's rights advocates and the progressive judiciary are beneficiaries, as they gain legal victories and advance their agenda of gender equality (d near 0.0-0.2). Women within patriarchal personal law are clear victims, as they bear the direct costs of the discriminatory practices being challenged (d near 0.9-1.0). Conservative religious authorities are also victims/payers, as they lose authority and face challenges to their traditional interpretations (d near 0.7-0.8). Secularist political parties and communal leaders are excluded, as their preferred modes of engagement (legislative UCC or traditional autonomy) are not the primary mechanism of this specific judicial constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_legitimacy_vs_communal_autonomy,
    'To what extent does judicial intervention in personal law undermine the perceived legitimacy of communal autonomy and religious freedom, and what are the long-term consequences for social cohesion?',
    'Longitudinal sociological studies on community perceptions of judicial authority and the impact of reforms on inter-communal relations, alongside legal analysis of the ''balancing act'' between rights and autonomy.',
    'If judicial legitimacy is significantly eroded, it could lead to greater resistance, non-compliance, or demands for alternative legal frameworks, potentially reclassifying the constraint as a more coercive snare or even a piton if reforms become purely symbolic. If social cohesion is severely damaged, the coordination function of the state itself could be compromised.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_legitimacy_vs_communal_autonomy, empirical, 'The tension between constitutional rights enforcement and the principle of communal self-governance.').

omega_variable(
    implementation_gap_vs_legal_reform,
    'How significant is the gap between judicial pronouncements on gender equality in personal law and their actual implementation and impact on the ground for women?',
    'Empirical studies measuring access to justice, enforcement of court orders, and changes in social practices and attitudes following judicial reforms. Analysis of legal aid availability and women''s agency in claiming rights.',
    'If the implementation gap is wide, the constraint''s effective extractiveness from women may remain high despite legal reforms, and its theater_ratio might increase, indicating that the legal changes are more symbolic than substantive. This could push the classification towards a piton if the reforms are largely performative without real-world effect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implementation_gap_vs_legal_reform, empirical, 'The discrepancy between de jure legal changes and de facto lived realities for women.').

omega_variable(
    framing_under_determination_gender_rights,
    'Is this constraint best framed as a judicial mechanism for rights enforcement, or as a political tool for state centralization and erosion of legal pluralism?',
    'Analysis of judicial reasoning, political discourse surrounding judgments, and the long-term trajectory of state power relative to communal institutions. A shift in framing would depend on whether the primary effect is rights-based or power-based.',
    'If framed as a political tool for state centralization, the constraint''s extractiveness might be re-evaluated as higher for communal institutions, and its suppression might be seen as targeting political autonomy rather than discriminatory practices. This could shift its classification towards a more overt snare or even a tangled rope if a coordination function (state unity) is acknowledged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_under_determination_gender_rights, conceptual, 'Ambiguity in whether judicial reforms are primarily about rights or state power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__gender_rights_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1970, marriage_authority__gender_rights_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(marr_tr_t1985, marriage_authority__gender_rights_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(marr_tr_t2000, marriage_authority__gender_rights_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(marr_tr_t2010, marriage_authority__gender_rights_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority__gender_rights_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(marr_be_t1970, marriage_authority__gender_rights_reading, base_extractiveness, 1970, 0.7).
narrative_ontology:measurement(marr_be_t1985, marriage_authority__gender_rights_reading, base_extractiveness, 1985, 0.75).
narrative_ontology:measurement(marr_be_t2000, marriage_authority__gender_rights_reading, base_extractiveness, 2000, 0.8).
narrative_ontology:measurement(marr_be_t2010, marriage_authority__gender_rights_reading, base_extractiveness, 2010, 0.83).
narrative_ontology:measurement(marr_be_t2024, marriage_authority__gender_rights_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1970, marriage_authority__gender_rights_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(marr_su_t1985, marriage_authority__gender_rights_reading, suppression_requirement, 1985, 0.7).
narrative_ontology:measurement(marr_su_t2000, marriage_authority__gender_rights_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(marr_su_t2010, marriage_authority__gender_rights_reading, suppression_requirement, 2010, 0.77).
narrative_ontology:measurement(marr_su_t2024, marriage_authority__gender_rights_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__gender_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_authority' kernel, focusing on judicial expansion of constitutional gender equality. It directly influences and is influenced by other readings of the same kernel, particularly those concerning communal autonomy and secularist reform.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
