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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Muslim Shariat Reading of Marriage as Civil Contract
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint models the 'muslim_shariat_reading' of the
 *   'family_law_authority' kernel, focusing on marriage as a civil contract
 *   (nikah) governed by Quranic injunctions and Hadith. It reflects the
 *   structural delta of contractual dissolution (talaq), permitted polygyny,
 *   mahr (dower) obligations, and historically gender-asymmetric divorce
 *   access (e.g., pre-2019 triple talaq ban). The constraint functions to
 *   coordinate family life according to religious principles but exhibits
 *   significant extraction, particularly from female spouses, due to these
 *   asymmetries and the social/religious enforcement mechanisms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__muslim_shariat_reading, 0.75).
domain_priors:suppression_score(family_law_authority__muslim_shariat_reading, 0.8).
domain_priors:theater_ratio(family_law_authority__muslim_shariat_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__muslim_shariat_reading, "Muslim Shariat Reading of Marriage as Civil Contract").
narrative_ontology:topic_domain(family_law_authority__muslim_shariat_reading, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__muslim_shariat_reading, '06c9e92a-05c1-40ce-b312-8786459ad001').
narrative_ontology:cs_kernel_codification('06c9e92a-05c1-40ce-b312-8786459ad001', fixed_text).
narrative_ontology:cs_authority_grounding('06c9e92a-05c1-40ce-b312-8786459ad001', lineage).
narrative_ontology:cs_interpretation_layer_present('06c9e92a-05c1-40ce-b312-8786459ad001').
narrative_ontology:cs_reading_relation('06c9e92a-05c1-40ce-b312-8786459ad001', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('06c9e92a-05c1-40ce-b312-8786459ad001', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('06c9e92a-05c1-40ce-b312-8786459ad001', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('06c9e92a-05c1-40ce-b312-8786459ad001', family_law_authority__secular_contractual_reading, coexists_with).
narrative_ontology:cs_axiom('06c9e92a-05c1-40ce-b312-8786459ad001', foundational, divine_revelation_supremacy).
narrative_ontology:cs_axiom_status(divine_revelation_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('06c9e92a-05c1-40ce-b312-8786459ad001', divine_revelation_supremacy, theological).
narrative_ontology:cs_axiom('06c9e92a-05c1-40ce-b312-8786459ad001', foundational, gender_differentiated_roles).
narrative_ontology:cs_axiom_status(gender_differentiated_roles, holdable).
narrative_ontology:cs_axiom_grounding('06c9e92a-05c1-40ce-b312-8786459ad001', gender_differentiated_roles, conventional).
narrative_ontology:cs_reference_frame('06c9e92a-05c1-40ce-b312-8786459ad001', classical_islamic_jurisprudence).
narrative_ontology:cs_drift_state('06c9e92a-05c1-40ce-b312-8786459ad001', contemporary_reform_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('06c9e92a-05c1-40ce-b312-8786459ad001', '').
narrative_ontology:cs_kernel_id(family_law_authority__muslim_shariat_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, male_spouses).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, religious_authorities).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, female_spouses).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, children_of_divorce).
narrative_ontology:constraint_vindicates(family_law_authority__muslim_shariat_reading, islamic_jurisprudence_supremacy).
narrative_ontology:constraint_vindicates(family_law_authority__muslim_shariat_reading, patriarchal_family_structure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from gender-asymmetric rights in marriage and divorce, including polygyny and historically easier divorce access. They are obligated to provide mahr (dower) and maintenance, but retain significant control over family decisions and assets.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, male_spouses, beneficiary,
    powerful, biographical, mobile, global).

% Bear the costs of gender asymmetry, including limited divorce access (historically, and still in some contexts), social pressure to conform, and economic dependency. Their identity is often deeply intertwined with their marital status and religious adherence, making exit difficult.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, female_spouses, payer,
    powerless, biographical, identity_locked, global).

% Interpret and enforce Quranic injunctions and Hadith, maintaining the legitimacy and structure of Sharia family law. They derive authority and social standing from their role as custodians of religious tradition and adjudicators of marital disputes.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, religious_authorities, agenda_setter,
    institutional, generational, analytical, global).

% In some contexts, they observe and defer to religious law; in others, they actively integrate, regulate, or reform aspects of Sharia family law, sometimes imposing limits (e.g., bans on triple talaq). They represent an alternative or co-existing authority structure.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, secular_legal_systems, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__muslim_shariat_reading, secular_legal_systems, agenda_setter).

% Bear the social and emotional costs of family dissolution, particularly when divorce processes are protracted or gender-asymmetric. Their welfare is often secondary to the rights of the spouses in traditional interpretations.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, children_of_divorce, payer,
    powerless, biographical, trapped, local).

% Advocate for reforms to Sharia family law to ensure gender equality in marriage, divorce, and inheritance. They are often excluded from traditional interpretive circles but exert pressure through legal challenges, public discourse, and international advocacy.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, women_rights_advocates, excluded,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legal and social framework for family formation, inheritance, and sexual relations according to Islamic principles, ensuring lineage and social order.
% TRANSFER_FUNCTION: Transfers rights, obligations, and resources within the family unit, including mahr (dower) from husband to wife, and maintenance obligations, often with gender-differentiated access to divorce and inheritance.
% ABSENT_VOICES: Feminist interpretations of Islamic law and secular legal reformers, who advocate for gender-egalitarian readings and state-enforced equality, are often marginalized or excluded from traditional religious discourse and legislative processes.
% DISAPPEARANCE_RATIONALE: If Sharia family law vanished overnight, family structures, inheritance practices, and social norms in many Muslim-majority societies would undergo profound and immediate reorganization, leading to widespread legal and social disruption.
% FOUNDING_PROBLEM: To regulate family formation, ensure social order, define rights and responsibilities within marriage, and establish clear rules for inheritance and lineage in early Islamic societies.
% FOUNDING_PROBLEM_CORROBORATION: Religious scholars and conservative communities attest that the founding problem of maintaining Islamic social order and family integrity remains live. Critics, including women's rights advocates and secular legal scholars, argue that while family regulation is still needed, many historical interpretations and practices are no longer just or optimal for contemporary society, indicating a shift in the problem's nature.
narrative_ontology:disappearance_verdict(family_law_authority__muslim_shariat_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__muslim_shariat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__muslim_shariat_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(family_law_authority__muslim_shariat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__muslim_shariat_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates family formation and social order (benefiting male spouses and religious authorities) but simultaneously extracts from female spouses and children of divorce through gender-asymmetric rights and obligations. Extraction (0.75) and suppression (0.80) are high, reflecting the limited agency and exit options for women within this framework, often reinforced by social and religious pressure. The theater ratio (0.25) is moderate, as while core religious injunctions are functional, some enforcement may become performative in response to internal and external challenges to maintain legitimacy. The temporal measurements show a slight decrease in extractiveness and suppression over the interval (1990-2025), reflecting global reform efforts and legal changes (like the triple talaq ban in some jurisdictions), but the underlying structural asymmetries persist.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious authorities and many male spouses, this arrangement is a divinely ordained and functional system for social order and family stability. From the perspective of female spouses and women's rights advocates, it is an extractive and suppressive system that limits women's autonomy and equality. The engine's computation of per-seat classifications from the structural data will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Male spouses are primary beneficiaries (low d) due to their privileged position and rights within the marital contract. Religious authorities are also beneficiaries (low d) as they derive authority and social capital from administering this system. Female spouses are primary targets (high d) due to the gender asymmetries and constrained exit options. Children of divorce are also targets (high d) as they bear the consequences of the system's operation. Secular legal systems and women's rights advocates act as observers or external challengers, with analytical or excluded positions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretation_divergence,
    'Is the traditional interpretation of Sharia family law (emphasizing gender asymmetry) the only valid reading, or are reformist, egalitarian interpretations equally legitimate within Islamic jurisprudence?',
    'A shift in scholarly consensus among leading Islamic jurists, or widespread adoption of legal reforms based on egalitarian interpretations across Muslim-majority states.',
    'If reformist readings gain traction and are implemented, the extractiveness and suppression for female spouses would decrease, potentially shifting the constraint towards a more equitable Rope or even a Scaffold (if transitional).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretation_divergence, conceptual, 'Ambiguity regarding the authoritative interpretation of Sharia family law.').

omega_variable(
    secular_vs_religious_authority,
    'To what extent does the authority of Sharia family law derive from religious conviction versus state enforcement or social pressure?',
    'Comparative legal analysis across jurisdictions with varying degrees of state integration of Sharia, coupled with sociological studies on individual adherence in contexts where state enforcement is minimal.',
    'If state enforcement and social pressure are the primary drivers, the constraint is more clearly a Snare or Tangled Rope. If adherence is primarily driven by individual religious conviction, it might lean more towards an identity_coordination (Rope for believers) for those who genuinely benefit from its structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_vs_religious_authority, empirical, 'The relative weight of religious conviction, state power, and social norms in enforcing Sharia family law.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression for female spouses structural (legal/economic barriers) or internalized (social/religious identity fusion)?',
    'Post-exit suppression trajectory: if suppression persists (e.g., social ostracization, psychological barriers) after legal/economic extractive mechanisms are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — female spouses carry the suppression with them after exit, making true freedom more difficult to achieve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for female spouses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__muslim_shariat_reading, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1990, family_law_authority__muslim_shariat_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(fami_tr_t1997, family_law_authority__muslim_shariat_reading, theater_ratio, 1997, 0.13).
narrative_ontology:measurement(fami_tr_t2004, family_law_authority__muslim_shariat_reading, theater_ratio, 2004, 0.16).
narrative_ontology:measurement(fami_tr_t2011, family_law_authority__muslim_shariat_reading, theater_ratio, 2011, 0.19).
narrative_ontology:measurement(fami_tr_t2018, family_law_authority__muslim_shariat_reading, theater_ratio, 2018, 0.22).
narrative_ontology:measurement(fami_tr_t2025, family_law_authority__muslim_shariat_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(fami_be_t1990, family_law_authority__muslim_shariat_reading, base_extractiveness, 1990, 0.85).
narrative_ontology:measurement(fami_be_t1997, family_law_authority__muslim_shariat_reading, base_extractiveness, 1997, 0.82).
narrative_ontology:measurement(fami_be_t2004, family_law_authority__muslim_shariat_reading, base_extractiveness, 2004, 0.79).
narrative_ontology:measurement(fami_be_t2011, family_law_authority__muslim_shariat_reading, base_extractiveness, 2011, 0.77).
narrative_ontology:measurement(fami_be_t2018, family_law_authority__muslim_shariat_reading, base_extractiveness, 2018, 0.76).
narrative_ontology:measurement(fami_be_t2025, family_law_authority__muslim_shariat_reading, base_extractiveness, 2025, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1990, family_law_authority__muslim_shariat_reading, suppression_requirement, 1990, 0.9).
narrative_ontology:measurement(fami_su_t1997, family_law_authority__muslim_shariat_reading, suppression_requirement, 1997, 0.88).
narrative_ontology:measurement(fami_su_t2004, family_law_authority__muslim_shariat_reading, suppression_requirement, 2004, 0.85).
narrative_ontology:measurement(fami_su_t2011, family_law_authority__muslim_shariat_reading, suppression_requirement, 2011, 0.83).
narrative_ontology:measurement(fami_su_t2018, family_law_authority__muslim_shariat_reading, suppression_requirement, 2018, 0.81).
narrative_ontology:measurement(fami_su_t2025, family_law_authority__muslim_shariat_reading, suppression_requirement, 2025, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__muslim_shariat_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'family_law_authority' kernel, which decomposes into multiple structurally distinct family law systems. Each reading is modeled as a separate constraint, linked here to its siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
