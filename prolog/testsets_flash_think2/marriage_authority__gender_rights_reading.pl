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
 *   human_readable: Judicial Enforcement of Gender Equality in Personal Law
 *   domain: legal/social/constitutional
 *
 * SUMMARY:
 *   This constraint describes the ongoing process of judicial intervention to
 *   enforce gender equality within personal law systems, often against
 *   traditional communal norms. It is a reading of the broader
 *   'marriage_authority' kernel, focusing on the judiciary's role in
 *   expanding constitutional equality guarantees. The constraint operates as
 *   a snare due to its high extractiveness from traditional patriarchal
 *   structures and the active suppression required to overcome resistance
 *   from those benefiting from existing inequalities. The claimed type
 *   'snare' reflects the direct and often coercive nature of judicial reform
 *   when it challenges deeply entrenched social norms.
 *
 * KEY AGENTS:
 *   - judiciary: Primary agenda_setter (institutional/analytical) — enforces constitutional equality
 *   - women_rights_advocates: Primary beneficiary (organized/constrained) — benefit from judicial reforms
 *   - women_seeking_equality: Primary beneficiary (powerless/trapped) — direct beneficiaries of legal changes
 *   - traditional_community_leaders: Primary payer (organized/constrained) — bear costs of lost authority, resist reforms
 *   - men_benefiting_from_inequality: Primary payer (moderate/constrained) — bear costs of lost privileges
 *   - communal_autonomy_advocates: Excluded (organized/constrained) — argue against judicial interference
 *   - secularist_legal_scholars: Observer (analytical/analytical) — analyze and critique the process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, 0.85).
domain_priors:suppression_score(marriage_authority__gender_rights_reading, 0.75).
domain_priors:theater_ratio(marriage_authority__gender_rights_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__gender_rights_reading, snare).
narrative_ontology:human_readable(marriage_authority__gender_rights_reading, "Judicial Enforcement of Gender Equality in Personal Law").
narrative_ontology:topic_domain(marriage_authority__gender_rights_reading, "legal/social/constitutional").

domain_priors:requires_active_enforcement(marriage_authority__gender_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__gender_rights_reading, '71566689-0346-48ae-af7f-df626485c16f').
narrative_ontology:cs_kernel_codification('71566689-0346-48ae-af7f-df626485c16f', formalized).
narrative_ontology:cs_authority_grounding('71566689-0346-48ae-af7f-df626485c16f', lineage).
narrative_ontology:cs_interpretation_layer_present('71566689-0346-48ae-af7f-df626485c16f').
narrative_ontology:cs_reading_relation('71566689-0346-48ae-af7f-df626485c16f', marriage_authority__communal_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('71566689-0346-48ae-af7f-df626485c16f', marriage_authority__federalist_millet_reading, forecloses).
narrative_ontology:cs_reading_relation('71566689-0346-48ae-af7f-df626485c16f', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_reading_relation('71566689-0346-48ae-af7f-df626485c16f', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_axiom('71566689-0346-48ae-af7f-df626485c16f', foundational, constitutional_equality_supremacy).
narrative_ontology:cs_axiom_status(constitutional_equality_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('71566689-0346-48ae-af7f-df626485c16f', constitutional_equality_supremacy, deontological).
narrative_ontology:cs_axiom('71566689-0346-48ae-af7f-df626485c16f', foundational, individual_dignity_over_communal_tradition).
narrative_ontology:cs_axiom_status(individual_dignity_over_communal_tradition, holdable).
narrative_ontology:cs_axiom_grounding('71566689-0346-48ae-af7f-df626485c16f', individual_dignity_over_communal_tradition, deontological).
narrative_ontology:cs_reference_frame('71566689-0346-48ae-af7f-df626485c16f', constitutional_equality_framework).
narrative_ontology:cs_drift_state('71566689-0346-48ae-af7f-df626485c16f', contemporary_personal_law_contestation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('71566689-0346-48ae-af7f-df626485c16f', '').
narrative_ontology:cs_kernel_id(marriage_authority__gender_rights_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, women_rights_advocates).
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, women_seeking_equality).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, traditional_community_leaders).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, men_benefiting_from_inequality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets constitutional guarantees to expand gender equality, often against existing personal law traditions. Enforces the new interpretations through rulings and precedents, directly shaping the legal landscape for marriage and family matters.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Actively campaign for and benefit from judicial reforms that grant women equal rights within personal law. Their advocacy efforts often lead to landmark judgments that challenge discriminatory practices.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_rights_advocates, beneficiary,
    organized, biographical, constrained, national).

% Direct beneficiaries of reforms, seeking relief from discriminatory practices such as triple talaq, unequal maintenance, or property rights within their community's personal law system. Their access to justice is mediated by the judiciary.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_seeking_equality, beneficiary,
    powerless, biographical, trapped, local).

% Bear the cost of losing traditional authority and control over personal law matters. They often resist judicial intervention, viewing it as an infringement on religious freedom and communal autonomy, and mobilize community members against reforms.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, traditional_community_leaders, payer,
    organized, generational, constrained, local).

% Bear the cost of losing privileges (e.g., easier divorce, property control, inheritance advantages) under reformed personal law. They may feel their traditional rights are being eroded and may support resistance efforts.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, men_benefiting_from_inequality, payer,
    moderate, biographical, constrained, local).

% Argue for the primacy of community-specific personal laws and against state/judicial interference, seeing it as an erosion of religious freedom and cultural identity. Their arguments are often considered but ultimately overruled by the judiciary's constitutional mandate.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, communal_autonomy_advocates, excluded,
    organized, generational, constrained, national).

% Analyze the judicial process, often supporting the move towards gender equality but sometimes critiquing the piecemeal approach compared to a comprehensive Uniform Civil Code. They provide academic commentary and influence public discourse.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, secularist_legal_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate legal interpretations across diverse personal law codes to ensure a constitutional floor of gender equality, providing a consistent standard for women's rights and access to justice within family law.
% TRANSFER_FUNCTION: Transfers legal authority and rights from traditional patriarchal structures and male community members to women, backed by the constitutional authority of the state judiciary. This includes rights related to divorce, maintenance, property, and inheritance.
% ABSENT_VOICES: Traditional religious authorities and communal autonomy advocates, who are often sidelined or overruled by judicial pronouncements, would argue for the preservation of community-specific personal laws and religious freedom, asserting their right to self-governance in family matters.
% DISAPPEARANCE_RATIONALE: If judicial enforcement of gender equality in personal law vanished, the legal landscape would revert to more traditional, often discriminatory, interpretations. This would lead to a significant rollback of women's rights, re-entrenchment of patriarchal norms within communities, and a fragmentation of legal standards, causing widespread social and legal reorganization.
% FOUNDING_PROBLEM: Discriminatory practices within various personal law systems (e.g., unequal divorce, maintenance, property rights for women) that violated constitutional guarantees of equality and human dignity, leading to systemic injustice for women.
% FOUNDING_PROBLEM_CORROBORATION: Women's rights organizations, international human rights bodies, and independent legal scholars consistently document ongoing gender inequality within personal law systems, providing extensive evidence and corroborating the problem's live status and the need for reform.
narrative_ontology:disappearance_verdict(marriage_authority__gender_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__gender_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__gender_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The high extractiveness (0.85) reflects the significant shift of rights and authority from traditional patriarchal structures to women, often against strong resistance. Suppression (0.75) is substantial because judicial decrees must actively overcome deeply entrenched social, religious, and political opposition to be implemented. The low theater ratio (0.15) indicates that judicial actions are direct and consequential, not merely performative; the reforms have tangible impacts. Accessibility collapse is moderate (0.40) because while the judiciary opens new avenues for justice, women seeking equality still face significant social and community barriers, and resistance (0.80) is high from those whose traditional authority or privileges are challenged.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of women's rights advocates and women seeking equality, this constraint is a necessary and beneficial mechanism for justice and liberation. From the perspective of traditional community leaders and men benefiting from inequality, it is an imposition that erodes religious freedom, cultural identity, and established social order. The judiciary, as the agenda-setter, views it as upholding constitutional principles. The engine's classification as a snare captures the coercive and extractive nature of this process from the perspective of those who bear its costs, despite its beneficial outcomes for others.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary, as the agenda-setter, has a directionality near the beneficiary end (d=0.1-0.2) as it fulfills its constitutional mandate and enhances its legitimacy. Women's rights advocates and women seeking equality are clear beneficiaries (d=0.0-0.1). Traditional community leaders and men benefiting from inequality are targets (d=0.8-0.9) as they lose authority and privileges. Communal autonomy advocates are excluded, their d value is not directly computed but their structural position is one of opposition to the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is far from mandatrophy; its mandate to enforce constitutional equality is actively live and contested. The classification as a snare prevents mislabeling it as a 'rope' (pure coordination) or 'scaffold' (temporary support), which would obscure the significant extraction and suppression involved in challenging deeply rooted patriarchal systems. The ongoing resistance and the need for active judicial enforcement confirm its current, highly active, and contested nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_activism_legitimacy,
    'Is the judiciary''s expansion of constitutional equality guarantees into personal law a legitimate exercise of judicial power, or an overreach into legislative or communal domains?',
    'Long-term public acceptance and institutional stability of judicial precedents, or a constitutional amendment clarifying the separation of powers regarding personal law.',
    'If deemed overreach, the constraint''s legitimacy would erode, potentially increasing resistance and reducing its effective suppression. If affirmed as legitimate, its authority would strengthen, making future reforms easier.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_activism_legitimacy, conceptual, 'Legitimacy of judicial activism in personal law reform.').

omega_variable(
    communal_harmony_vs_individual_rights,
    'What is the long-term impact of judicial reforms on communal harmony and social cohesion, balanced against the advancement of individual gender rights?',
    'Sociological studies tracking inter-community relations, rates of social conflict, and indicators of individual well-being and autonomy in areas affected by reforms over several decades.',
    'If reforms lead to severe social fragmentation, it might suggest a need for alternative, more consensual approaches. If individual rights are significantly enhanced without undue social cost, it strengthens the current approach''s justification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(communal_harmony_vs_individual_rights, empirical, 'Trade-off between communal harmony and individual rights.').

omega_variable(
    constitutional_equality_religious_freedom_tension,
    'How should the tension between constitutional guarantees of gender equality and constitutional protections for religious freedom (which often underpin personal laws) be resolved?',
    'Ongoing jurisprudential development, legislative action to codify a Uniform Civil Code, or a societal consensus shift on the interpretation of these fundamental rights.',
    'The resolution of this tension fundamentally redefines the scope and limits of the ''gender_rights_reading'' constraint, potentially shifting its extractiveness and suppression profile depending on which principle is prioritized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_equality_religious_freedom_tension, preference, 'Balancing gender equality with religious freedom in personal law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__gender_rights_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1970, marriage_authority__gender_rights_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(marr_tr_t1980, marriage_authority__gender_rights_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(marr_tr_t1990, marriage_authority__gender_rights_reading, theater_ratio, 1990, 0.13).
narrative_ontology:measurement(marr_tr_t2000, marriage_authority__gender_rights_reading, theater_ratio, 2000, 0.14).
narrative_ontology:measurement(marr_tr_t2010, marriage_authority__gender_rights_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(marr_tr_t2020, marriage_authority__gender_rights_reading, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(marr_be_t1970, marriage_authority__gender_rights_reading, base_extractiveness, 1970, 0.7).
narrative_ontology:measurement(marr_be_t1980, marriage_authority__gender_rights_reading, base_extractiveness, 1980, 0.75).
narrative_ontology:measurement(marr_be_t1990, marriage_authority__gender_rights_reading, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(marr_be_t2000, marriage_authority__gender_rights_reading, base_extractiveness, 2000, 0.82).
narrative_ontology:measurement(marr_be_t2010, marriage_authority__gender_rights_reading, base_extractiveness, 2010, 0.84).
narrative_ontology:measurement(marr_be_t2020, marriage_authority__gender_rights_reading, base_extractiveness, 2020, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1970, marriage_authority__gender_rights_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(marr_su_t1980, marriage_authority__gender_rights_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(marr_su_t1990, marriage_authority__gender_rights_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(marr_su_t2000, marriage_authority__gender_rights_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(marr_su_t2010, marriage_authority__gender_rights_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(marr_su_t2020, marriage_authority__gender_rights_reading, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__gender_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, judicial_harmonization_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, secularist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_authority' kernel, focusing on judicial enforcement of gender equality. It is structurally distinct from other readings that emphasize communal autonomy, federalist legal pluralism, or a secular Uniform Civil Code, but all are interconnected within the broader legal and social system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
