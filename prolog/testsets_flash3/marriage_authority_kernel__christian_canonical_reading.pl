% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__christian_canonical_reading, []).

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
 *   constraint_id: marriage_authority_kernel__christian_canonical_reading
 *   human_readable: Indian Christian Marriage Act 1872 (Christian Canonical Reading)
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   This constraint represents one reading of the 'marriage_authority_kernel'
 *   in India, specifically focusing on the authority derived from Christian
 *   canonical law as codified in the Indian Christian Marriage Act 1872. This
 *   reading emphasizes restrictive divorce (fault-based), the role of church
 *   tribunals for annulment, and a moderate level of gender equity compared
 *   to secular civil law. The constraint is classified as a Tangled Rope due
 *   to its genuine coordination function (providing a legal framework for a
 *   religious community) coupled with significant asymmetric extraction,
 *   particularly from Christian women and individuals seeking annulment, who
 *   face restrictive provisions and ecclesiastical authority.
 *
 * KEY AGENTS:
 *   - christian_church_authorities: Agenda setter (institutional/constrained) — administers the Act, maintains ecclesiastical authority.
 *   - christian_women_seeking_divorce: Primary payer (powerless/identity_locked) — faces restrictive divorce, social stigma, and church tribunals.
 *   - christian_men_seeking_divorce: Payer (moderate/constrained) — also subject to fault-based divorce, but with more leverage.
 *   - christian_individuals_seeking_annulment: Payer (powerless/identity_locked) — navigates complex church tribunals.
 *   - conservative_christian_communities: Beneficiary (organized/constrained) — benefits from traditional structures.
 *   - indian_civil_courts: Observer (institutional/analytical) — interprets the Act, balances with constitutional principles.
 *   - secular_legal_reformers: Excluded (organized/mobile) — advocate for uniform civil code, challenge religious acts.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, 0.65).
domain_priors:suppression_score(marriage_authority_kernel__christian_canonical_reading, 0.7).
domain_priors:theater_ratio(marriage_authority_kernel__christian_canonical_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__christian_canonical_reading, "Indian Christian Marriage Act 1872 (Christian Canonical Reading)").
narrative_ontology:topic_domain(marriage_authority_kernel__christian_canonical_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__christian_canonical_reading, '4671a745-74d2-471c-bf32-88610613629f').
narrative_ontology:cs_kernel_codification('4671a745-74d2-471c-bf32-88610613629f', formalized).
narrative_ontology:cs_authority_grounding('4671a745-74d2-471c-bf32-88610613629f', lineage).
narrative_ontology:cs_interpretation_layer_present('4671a745-74d2-471c-bf32-88610613629f').
narrative_ontology:cs_reading_relation('4671a745-74d2-471c-bf32-88610613629f', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('4671a745-74d2-471c-bf32-88610613629f', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('4671a745-74d2-471c-bf32-88610613629f', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('4671a745-74d2-471c-bf32-88610613629f', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('4671a745-74d2-471c-bf32-88610613629f', foundational, marriage_as_sacrament_indissoluble).
narrative_ontology:cs_axiom_status(marriage_as_sacrament_indissoluble, holdable).
narrative_ontology:cs_axiom_grounding('4671a745-74d2-471c-bf32-88610613629f', marriage_as_sacrament_indissoluble, theological).
narrative_ontology:cs_axiom('4671a745-74d2-471c-bf32-88610613629f', foundational, canonical_law_as_primary_authority).
narrative_ontology:cs_axiom_status(canonical_law_as_primary_authority, holdable).
narrative_ontology:cs_axiom_grounding('4671a745-74d2-471c-bf32-88610613629f', canonical_law_as_primary_authority, conventional).
narrative_ontology:cs_reference_frame('4671a745-74d2-471c-bf32-88610613629f', christian_canonical_tradition_1872).
narrative_ontology:cs_drift_state('4671a745-74d2-471c-bf32-88610613629f', contemporary_indian_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4671a745-74d2-471c-bf32-88610613629f', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, christian_church_authorities).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, conservative_christian_communities).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_women_seeking_divorce).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_individuals_seeking_annulment).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_men_seeking_divorce).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer and interpret the Indian Christian Marriage Act 1872, often aligning with canonical law. They benefit from maintaining ecclesiastical authority over marriage and divorce, which reinforces their social and moral standing within the community. Exit options are limited by their institutional identity and the legal framework.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_church_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Face restrictive, fault-based divorce provisions and often must navigate church tribunals for annulment, which can be lengthy and stigmatizing. Their options are constrained by legal and social pressures, often leading to prolonged marital distress or social ostracization if they seek alternatives outside the community.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_women_seeking_divorce, payer,
    powerless, biographical, identity_locked, local).

% Also subject to fault-based divorce, but generally face less social stigma and may have greater access to legal resources. Their options are similarly constrained by the Act but with slightly more leverage than women.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_men_seeking_divorce, payer,
    moderate, biographical, constrained, local).

% Must often seek annulment through church tribunals, which operate under canonical law and may not recognize grounds valid in civil law. This process can be emotionally and financially draining, with limited recourse outside the ecclesiastical system if they wish to remain within their religious community.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_individuals_seeking_annulment, payer,
    powerless, biographical, identity_locked, local).

% Benefit from the preservation of traditional marriage and family structures, which reinforces their cultural and religious identity. The Act's provisions align with their values, and they exert social pressure to maintain its authority. Their exit options are tied to their communal identity.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, conservative_christian_communities, beneficiary,
    organized, generational, constrained, local).

% Interpret and apply the Indian Christian Marriage Act 1872, often balancing its provisions with constitutional principles of equality and secularism. They are the ultimate arbiters but must operate within the existing legal pluralism, leading to complex jurisprudence.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, indian_civil_courts, observer,
    institutional, generational, analytical, national).

% Advocate for a uniform civil code and greater gender equity in family law, challenging the authority of religion-specific acts. They are excluded from the direct administration of the Act but exert pressure through advocacy and litigation.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, secular_legal_reformers, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework for marriage and divorce within the Christian community in India, ensuring legal recognition and a degree of uniformity for religious practices.
% TRANSFER_FUNCTION: Transfers authority over marital dissolution and personal status from individuals to church authorities and the state, enforcing canonical interpretations through civil law, often at the cost of individual autonomy, particularly for women.
% ABSENT_VOICES: Secular legal reformers and individuals seeking more equitable or less restrictive divorce options are often marginalized in the interpretation and application of the Act, their perspectives not fully integrated into the existing legal framework.
% DISAPPEARANCE_RATIONALE: If the Indian Christian Marriage Act 1872 vanished overnight, the Christian community would face a legal vacuum for marriage and divorce, forcing individuals into the Special Marriage Act 1954 or creating immense legal uncertainty. The authority of church bodies in these matters would be significantly diminished, and the legal landscape of personal law in India would shift dramatically.
% FOUNDING_PROBLEM: To provide a legal framework for Christian marriages in British India, recognizing Christian religious practices within the colonial legal system and ensuring legal validity for a minority community.
% FOUNDING_PROBLEM_CORROBORATION: Christian church authorities and conservative communities attest that the Act continues to serve the live problem of maintaining religious identity and order in marriage. Secular legal reformers and some individuals within the community argue that while the original problem of recognition is solved, the Act now creates new problems of inequity and restriction, making its status contested.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__christian_canonical_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__christian_canonical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_authority_kernel__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__christian_canonical_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__christian_canonical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the Act's provisions, rooted in canonical law, impose significant burdens on individuals seeking marital dissolution, particularly women, by limiting grounds for divorce and often requiring engagement with church tribunals. Suppression (0.70) is substantial due to the legal framework's enforcement by both civil courts and church authorities, combined with social pressure within conservative communities, which limits exit options for those who wish to remain within their religious identity. Theater ratio is moderate (0.20) as the Act still serves a genuine function of providing a legal framework, but a portion of its maintenance is performative, upholding traditional authority structures. The increasing extractiveness and suppression over time reflect the growing tension between traditional religious law and evolving societal norms and constitutional rights.
 *
 * PERSPECTIVAL GAP:
 *   Christian church authorities perceive the Act as a necessary framework for religious order and identity, a coordination mechanism that preserves community values. For Christian women and individuals seeking annulment, the same Act functions as a highly extractive and suppressive mechanism, limiting their autonomy and imposing significant costs. Indian civil courts operate in a complex space, attempting to reconcile the Act's provisions with broader constitutional principles, experiencing it as a challenge to legal coherence.
 *
 * DIRECTIONALITY LOGIC:
 *   Christian church authorities are clear beneficiaries (d near 0.0) as they maintain institutional authority and influence over personal law. Conservative Christian communities also benefit (d near 0.1-0.2) from the preservation of traditional structures. Christian women and individuals seeking annulment are primary targets (d near 0.9-1.0) due to restrictive divorce laws and the burden of church tribunals, often compounded by identity-locked exit options. Christian men seeking divorce are also targets (d near 0.7-0.8), but with slightly more social and legal leverage. Secular legal reformers are excluded, and their directionality is not directly measured by the constraint's operation but by its resistance to their efforts.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (providing a legal framework for Christian marriages) is still live, but its function has drifted. While it still coordinates, the asymmetric extraction has intensified, preventing it from being mislabeled as a pure Rope. The persistence of restrictive provisions, despite evolving societal norms and constitutional challenges, indicates a Tangled Rope where the coordination story serves to maintain an extractive structure for the benefit of church authorities and conservative communities.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    christian_canonical_vs_civil_law_supremacy,
    'To what extent does the Indian Christian Marriage Act 1872 truly reflect Christian canonical law, versus being a colonial codification that now operates independently?',
    'Comparative legal analysis of the Act''s provisions against contemporary canonical law and historical legislative intent, alongside judicial interpretations that prioritize either canonical or civil principles.',
    'If the Act is found to diverge significantly from contemporary canonical law, its legitimacy as a ''religious'' law weakens, potentially opening avenues for secular legal reform. If it closely tracks canonical law, the conflict with constitutional secularism becomes more pronounced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(christian_canonical_vs_civil_law_supremacy, conceptual, 'Ambiguity in the Act''s grounding: canonical vs. colonial civil law.').

omega_variable(
    gender_equity_vs_religious_freedom_balance,
    'How should the constitutional right to religious freedom be balanced against the constitutional right to gender equality within the context of personal laws?',
    'Supreme Court rulings on specific cases challenging the Act''s provisions, legislative action towards a uniform civil code, and ongoing public discourse and advocacy by women''s rights groups and religious organizations.',
    'A stronger emphasis on gender equality would likely lead to reforms in the Act''s divorce and annulment provisions, reducing extraction from women. A stronger emphasis on religious freedom might preserve the current restrictive framework, maintaining the status quo.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gender_equity_vs_religious_freedom_balance, preference, 'Irreducible tension between constitutional principles in personal law.').

omega_variable(
    identity_locked_exit_severity,
    'For Christian individuals, how severe is the social and psychological cost of exiting the community (and thus the Act''s jurisdiction) to seek secular legal remedies?',
    'Sociological studies on excommunication, social ostracization, and community support networks for individuals who defy religious authority in marital matters. Qualitative interviews with individuals who have attempted such exits.',
    'If the identity-locked exit is extremely severe, the effective suppression and extractiveness of the constraint are higher than legal provisions alone suggest, as individuals are effectively trapped. If the costs are manageable, it implies greater agency and less structural suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_exit_severity, empirical, 'Severity of identity-locked exit for community members.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__christian_canonical_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(marr_tr_t30, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(marr_tr_t60, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 60, 0.15).
narrative_ontology:measurement(marr_tr_t90, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 90, 0.17).
narrative_ontology:measurement(marr_tr_t120, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 120, 0.19).
narrative_ontology:measurement(marr_tr_t150, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 150, 0.2).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(marr_be_t30, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(marr_be_t60, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(marr_be_t90, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 90, 0.6).
narrative_ontology:measurement(marr_be_t120, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 120, 0.63).
narrative_ontology:measurement(marr_be_t150, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 150, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(marr_su_t30, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(marr_su_t60, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(marr_su_t90, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 90, 0.65).
narrative_ontology:measurement(marr_su_t120, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 120, 0.68).
narrative_ontology:measurement(marr_su_t150, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 150, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__christian_canonical_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_authority_kernel' in India, focusing on Christian canonical law. It coexists with and influences other religion-specific and secular readings of marriage authority, forming a complex legal pluralism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
