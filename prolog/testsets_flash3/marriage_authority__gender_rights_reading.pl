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
 *   human_readable: Marriage Authority: Gender Rights Reading
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   This constraint represents the 'gender_rights_reading' of marriage
 *   authority, where constitutional courts expand equality guarantees to
 *   reform personal laws. It is a snare because it actively extracts
 *   traditional authority and resources from patriarchal structures,
 *   benefiting women's rights advocates and constitutional courts, but
 *   victimizing women who remain identity-locked within communities and
 *   traditional religious authorities. The reading targets specific practices
 *   (e.g., triple talaq, maintenance) rather than advocating for a wholesale
 *   Uniform Civil Code, thus cross-cutting the communal/secular divide.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, 0.85).
domain_priors:suppression_score(marriage_authority__gender_rights_reading, 0.78).
domain_priors:theater_ratio(marriage_authority__gender_rights_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__gender_rights_reading, snare).
narrative_ontology:human_readable(marriage_authority__gender_rights_reading, "Marriage Authority: Gender Rights Reading").
narrative_ontology:topic_domain(marriage_authority__gender_rights_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__gender_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__gender_rights_reading, 'ab18fd90-6a61-45dd-8ca8-99ddca17b3f7').
narrative_ontology:cs_kernel_codification('ab18fd90-6a61-45dd-8ca8-99ddca17b3f7', formalized).
narrative_ontology:cs_authority_grounding('ab18fd90-6a61-45dd-8ca8-99ddca17b3f7', lineage).
narrative_ontology:cs_interpretation_layer_present('ab18fd90-6a61-45dd-8ca8-99ddca17b3f7').
narrative_ontology:cs_reading_relation('ab18fd90-6a61-45dd-8ca8-99ddca17b3f7', marriage_authority__communal_autonomy_reading, influences).
narrative_ontology:cs_reading_relation('ab18fd90-6a61-45dd-8ca8-99ddca17b3f7', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ab18fd90-6a61-45dd-8ca8-99ddca17b3f7', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_reading_relation('ab18fd90-6a61-45dd-8ca8-99ddca17b3f7', marriage_authority__judicial_harmonization_reading, coexists_with).
narrative_ontology:cs_axiom('ab18fd90-6a61-45dd-8ca8-99ddca17b3f7', foundational, constitutional_gender_equality_is_supreme).
narrative_ontology:cs_axiom_status(constitutional_gender_equality_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('ab18fd90-6a61-45dd-8ca8-99ddca17b3f7', constitutional_gender_equality_is_supreme, deontological).
narrative_ontology:cs_axiom('ab18fd90-6a61-45dd-8ca8-99ddca17b3f7', foundational, personal_law_must_conform_to_fundamental_rights).
narrative_ontology:cs_axiom_status(personal_law_must_conform_to_fundamental_rights, holdable).
narrative_ontology:cs_axiom_grounding('ab18fd90-6a61-45dd-8ca8-99ddca17b3f7', personal_law_must_conform_to_fundamental_rights, conventional).
narrative_ontology:cs_reference_frame('ab18fd90-6a61-45dd-8ca8-99ddca17b3f7', constitutional_equality_as_foundational).
narrative_ontology:cs_drift_state('ab18fd90-6a61-45dd-8ca8-99ddca17b3f7', contemporary_judicial_activism_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ab18fd90-6a61-45dd-8ca8-99ddca17b3f7', '').
narrative_ontology:cs_kernel_id(marriage_authority__gender_rights_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, women_rights_advocates).
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, constitutional_courts).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, traditional_religious_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, secularist_reformers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively litigate for reforms to personal laws, seeking to align them with constitutional gender equality guarantees. They benefit from judicial rulings that expand women's rights within marriage and family law, but face ongoing resistance from traditional authorities.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_rights_advocates, beneficiary,
    organized, generational, constrained, national).

% Interpret constitutional equality guarantees to review and reform specific aspects of personal laws (e.g., triple talaq, maintenance, property rights). They are the primary enforcers of this reading, expanding the scope of state authority over communal norms.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Are the direct targets of patriarchal practices within personal laws, experiencing discrimination in divorce, inheritance, and maintenance. While judicial reforms offer some relief, their social and economic realities often limit their ability to access or enforce these new rights, leaving them identity-locked within their communities.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law, payer,
    powerless, biographical, identity_locked, local).

% Administer personal laws based on religious traditions and resist judicial interventions that they perceive as infringing on communal autonomy and religious freedom. They bear the cost of losing authority and control over family matters within their communities.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, traditional_religious_authorities, payer,
    organized, generational, constrained, regional).

% Advocate for a Uniform Civil Code to replace all personal laws, seeing judicial reforms as incremental steps towards a fully secular legal system. They benefit from any expansion of constitutional authority over religious personal laws.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, secularist_reformers, beneficiary,
    moderate, generational, mobile, national).

% Argue that personal laws are integral to community identity and religious freedom, and that judicial intervention undermines the constitutional guarantee of legal pluralism. They are excluded from the judicial process that reinterprets these laws, and their arguments are often overridden by the constitutional courts.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, communal_autonomy_defenders, excluded,
    organized, generational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Seeks to harmonize diverse personal laws with constitutional gender equality principles, providing a common floor of rights for women across different communities and legal traditions.
% TRANSFER_FUNCTION: Transfers authority over specific aspects of family law from traditional religious authorities to constitutional courts, and transfers rights (e.g., property, maintenance, divorce) from men to women within communities governed by personal laws.
% ABSENT_VOICES: Many women within patriarchal personal law systems, particularly those in rural or marginalized communities, lack the agency or resources to voice their demands for reform directly in judicial or legislative forums. Their interests are often represented by advocates, but their direct voices are largely absent from the formal process. Defenders of communal autonomy are also structurally excluded from the judicial process of reinterpretation.
% DISAPPEARANCE_RATIONALE: If this reading (judicial expansion of gender equality in personal law) vanished, the legal landscape would revert to a more fragmented state, with women's rights determined solely by diverse, often patriarchal, communal norms. Constitutional courts would cease to intervene, and women's rights advocates would lose a key avenue for reform, leading to a significant rearrangement of legal and social power dynamics.
% FOUNDING_PROBLEM: The existence of multiple personal laws, often rooted in patriarchal religious traditions, created significant gender inequality and discrimination for women within marriage, divorce, and inheritance, conflicting with constitutional guarantees of equality.
% FOUNDING_PROBLEM_CORROBORATION: Women's rights organizations, international human rights bodies, and independent legal scholars consistently corroborate that gender inequality within personal laws remains a live and pressing problem, despite incremental judicial reforms. Traditional religious authorities, however, contest this, arguing that their laws are divinely ordained and not subject to secular notions of equality.
narrative_ontology:disappearance_verdict(marriage_authority__gender_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__gender_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__gender_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) because this reading directly challenges and reallocates power and resources from established patriarchal systems. Suppression (0.78) is also high, as judicial reforms are often met with strong resistance from traditional authorities, requiring active enforcement by the state. Theater ratio is low (0.15) because the judicial interventions are genuine attempts at reform, not merely performative. Accessibility collapse is moderate (0.45) as alternatives to personal law are limited for many women, but resistance is high (0.70) from those who benefit from the traditional system.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of constitutional courts and women's rights advocates, this is a necessary and just reform. From the perspective of traditional religious authorities and communal autonomy defenders, it is an overreach of state power and an infringement on religious freedom. The engine's classification as a snare reflects the high extraction and suppression inherent in this contest, regardless of the normative justification.
 *
 * DIRECTIONALITY LOGIC:
 *   Constitutional courts are agenda-setters and beneficiaries, expanding their authority. Women's rights advocates are beneficiaries, gaining legal leverage. Women within patriarchal personal law are victims, as the constraint targets practices that harm them, but they also bear the social costs of challenging tradition. Traditional religious authorities are payers, losing control and legitimacy. Secularist reformers are beneficiaries, as this reading aligns with their broader goals. Communal autonomy defenders are excluded, as their arguments are often dismissed by the courts.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_enforcement_efficacy,
    'How effectively are judicial reforms enforced and implemented at the local level, given social resistance and limited legal literacy among affected women?',
    'Empirical studies tracking post-judgment compliance rates, access to legal aid, and changes in women''s lived experiences in communities affected by reforms.',
    'If enforcement is weak, the effective extractiveness from patriarchal structures is lower than measured, and the constraint''s classification might drift towards a piton (theatrical reform without real impact). If strong, the snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_enforcement_efficacy, empirical, 'Gap between de jure judicial reform and de facto implementation.').

omega_variable(
    identity_lock_vs_agency,
    'To what extent does ''identity_locked'' status for women within patriarchal personal law reflect genuine lack of exit options versus internalized norms or social pressure that could be overcome with greater agency?',
    'Qualitative sociological research on women''s perceptions of choice, community support networks, and the impact of economic empowerment programs on their ability to challenge traditional norms.',
    'If internalized norms are a stronger factor, the suppression mechanism is more complex than purely structural, and interventions would need to target cultural change alongside legal reform. If structural barriers dominate, the snare classification is more robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_vs_agency, conceptual, 'Structural vs. internalized components of identity-locked exit for women.').

omega_variable(
    communal_autonomy_vs_equality_priority,
    'Is the constitutional guarantee of communal autonomy (religious freedom) fundamentally irreconcilable with the constitutional guarantee of gender equality, or can they be harmonized through reinterpretation?',
    'Ongoing constitutional jurisprudence and public discourse. Resolution depends on which interpretive framework gains dominance over time.',
    'If irreconcilable, this reading directly forecloses the ''communal_autonomy_reading''. If harmonizable, it merely influences it, pushing for reinterpretation rather than outright rejection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(communal_autonomy_vs_equality_priority, preference, 'Conceptual tension between communal autonomy and gender equality in constitutional interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__gender_rights_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__gender_rights_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(marr_tr_t5, marriage_authority__gender_rights_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(marr_tr_t10, marriage_authority__gender_rights_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(marr_tr_t15, marriage_authority__gender_rights_reading, theater_ratio, 15, 0.16).
narrative_ontology:measurement(marr_tr_t20, marriage_authority__gender_rights_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(marr_tr_t25, marriage_authority__gender_rights_reading, theater_ratio, 25, 0.15).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__gender_rights_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(marr_be_t5, marriage_authority__gender_rights_reading, base_extractiveness, 5, 0.78).
narrative_ontology:measurement(marr_be_t10, marriage_authority__gender_rights_reading, base_extractiveness, 10, 0.81).
narrative_ontology:measurement(marr_be_t15, marriage_authority__gender_rights_reading, base_extractiveness, 15, 0.83).
narrative_ontology:measurement(marr_be_t20, marriage_authority__gender_rights_reading, base_extractiveness, 20, 0.84).
narrative_ontology:measurement(marr_be_t25, marriage_authority__gender_rights_reading, base_extractiveness, 25, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__gender_rights_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(marr_su_t5, marriage_authority__gender_rights_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(marr_su_t10, marriage_authority__gender_rights_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(marr_su_t15, marriage_authority__gender_rights_reading, suppression_requirement, 15, 0.75).
narrative_ontology:measurement(marr_su_t20, marriage_authority__gender_rights_reading, suppression_requirement, 20, 0.77).
narrative_ontology:measurement(marr_su_t25, marriage_authority__gender_rights_reading, suppression_requirement, 25, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
