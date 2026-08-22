% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__literal_hierarchical
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: quranic_gender_verses__literal_hierarchical
 *   human_readable: Qur'anic Gender Verses — Literal Hierarchical Reading
 *   domain: religious_legal/gender
 *
 * SUMMARY:
 *   This constraint story models the literal-hierarchical reading of Qur'an
 *   4:11 (inheritance), 2:282 (testimony), and 4:34 (guardianship) as a
 *   standing legal arrangement. The reading treats these verses as direct,
 *   timeless legislative commands establishing male authority over women in
 *   family and legal domains. It is instantiated in the personal status laws
 *   of most Muslim-majority countries and enforced by religious courts and
 *   state institutions. The claimed type is snare: the coordination story
 *   (divine order for social harmony) is cover; persistence depends on active
 *   suppression of alternative readings and exit options for those extracted
 *   from. Beneficiaries (male heads, religious courts, state institutions)
 *   collect structural authority and resources; victims (women under
 *   guardianship, female heirs, women in legal testimony) bear costs with
 *   identity-locked or trapped exit.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, 0.78).
domain_priors:suppression_score(quranic_gender_verses__literal_hierarchical, 0.82).
domain_priors:theater_ratio(quranic_gender_verses__literal_hierarchical, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, extractiveness, 0.78).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__literal_hierarchical, snare).
narrative_ontology:human_readable(quranic_gender_verses__literal_hierarchical, "Qur'anic Gender Verses — Literal Hierarchical Reading").
narrative_ontology:topic_domain(quranic_gender_verses__literal_hierarchical, "religious_legal/gender").

domain_priors:requires_active_enforcement(quranic_gender_verses__literal_hierarchical).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__literal_hierarchical, '7865ecf2-6d27-4431-9baf-dbc42d325813').
narrative_ontology:cs_kernel_codification('7865ecf2-6d27-4431-9baf-dbc42d325813', fixed_text).
narrative_ontology:cs_authority_grounding('7865ecf2-6d27-4431-9baf-dbc42d325813', lineage).
narrative_ontology:cs_interpretation_layer_present('7865ecf2-6d27-4431-9baf-dbc42d325813').
narrative_ontology:cs_reading_relation('7865ecf2-6d27-4431-9baf-dbc42d325813', quranic_gender_verses__contextual_egalitarian, forecloses).
narrative_ontology:cs_reading_relation('7865ecf2-6d27-4431-9baf-dbc42d325813', quranic_gender_verses__progressive_abrogation, coexists_with).
narrative_ontology:cs_axiom('7865ecf2-6d27-4431-9baf-dbc42d325813', foundational, verses_are_timeless_legislative_commands).
narrative_ontology:cs_axiom_status(verses_are_timeless_legislative_commands, holdable).
narrative_ontology:cs_axiom_grounding('7865ecf2-6d27-4431-9baf-dbc42d325813', verses_are_timeless_legislative_commands, theological).
narrative_ontology:cs_axiom('7865ecf2-6d27-4431-9baf-dbc42d325813', foundational, male_guardianship_is_divine_ordinance).
narrative_ontology:cs_axiom_status(male_guardianship_is_divine_ordinance, holdable).
narrative_ontology:cs_axiom_grounding('7865ecf2-6d27-4431-9baf-dbc42d325813', male_guardianship_is_divine_ordinance, theological).
narrative_ontology:cs_reference_frame('7865ecf2-6d27-4431-9baf-dbc42d325813', classical_fiqh_consensus).
narrative_ontology:cs_drift_state('7865ecf2-6d27-4431-9baf-dbc42d325813', modern_nation_state_codification, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('7865ecf2-6d27-4431-9baf-dbc42d325813', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__literal_hierarchical, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, male_household_heads).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, religious_courts).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, state_family_law_institutions).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, women_subject_to_guardianship).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, female_heirs).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, women_in_legal_testimony).
narrative_ontology:constraint_vindicates(quranic_gender_verses__literal_hierarchical, divine_ordinance_of_male_guardianship).
narrative_ontology:constraint_vindicates(quranic_gender_verses__literal_hierarchical, fixed_hierarchical_gender_roles).
narrative_ontology:constraint_vindicates(quranic_gender_verses__literal_hierarchical, timeless_legal_force_of_revealed_verses).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold legal authority over wives, daughters, and female dependents per verses 4:34 (qiwamah) and 4:11 (inheritance). Control household resources, marriage decisions, and mobility permissions. Exit from this role is socially and religiously costly but legally available through renunciation of guardianship duties — rarely exercised due to status and material benefits.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, male_household_heads, beneficiary,
    powerful, biographical, mobile, national).

% Adjudicate family law (marriage, divorce, custody, inheritance) using the literal-hierarchical reading as binding precedent. Issue fatwas and rulings that enforce differential testimony weight (2:282) and male guardianship. Their institutional legitimacy and resource base depend on maintaining this interpretive monopoly. Could reform interpretive methodology but face internal and external pressure to preserve the reading.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, religious_courts, agenda_setter,
    institutional, generational, analytical, national).

% Codify the literal-hierarchical reading into national civil codes (e.g., personal status laws). Extract administrative control over family formation and dissolution; the reading legitimizes state regulation of private life. Reform would require legislative action against religious establishment opposition — politically costly but structurally possible.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, state_family_law_institutions, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__literal_hierarchical, state_family_law_institutions, agenda_setter).

% Require male guardian permission for marriage, travel, employment, and medical decisions in jurisdictions implementing this reading. Bear the material and autonomy costs of differential rights. Exit from the constraint requires either male guardian consent (contradiction), court intervention (rarely granted), or leaving the jurisdiction/community — which entails family rupture, loss of children, and in some contexts apostasy penalties.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, women_subject_to_guardianship, payer,
    moderate, biographical, identity_locked, national).

% Receive half the inheritance share of male counterparts per 4:11. The material loss compounds across generations. Can sometimes negotiate voluntary gifts (hiba) from male relatives, but this depends on their willingness — no legal recourse to claim equal share under this reading. Exit is constrained by family pressure and lack of legal standing.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, female_heirs, payer,
    moderate, biographical, constrained, national).

% Testimony counts as half that of a man in financial contracts per 2:282, and in some schools is inadmissible in hudud/qisas cases. Directly undermines legal standing in disputes over property, divorce, and abuse. No procedural exit within the system; the constraint defines the terms of legal personhood.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, women_in_legal_testimony, payer,
    powerless, immediate, trapped, national).

% Advocate for contextual_egalitarian or progressive_abrogation readings using maqasid, naskh, or historical-critical methods. Are marginalized from official religious institutions, denied platforms, and sometimes face apostasy accusations. Their exclusion is structural — the literal-hierarchical reading's authority depends on foreclosing their interpretive space.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, reformist_scholars, excluded,
    organized, biographical, constrained, global).

% Organize for legal reform, document harms, and challenge the reading in domestic and international fora. Face state repression, social ostracism, and religious delegitimization. Their voices are absent from the authoritative interpretive circle — the constraint's persistence requires their exclusion.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, women_rights_activists, excluded,
    organized, biographical, constrained, global).

% Analyze the constraint's operation across jurisdictions, trace its codification history, and assess its compatibility with constitutional equality guarantees. Do not bear the constraint's costs nor collect its benefits. Provide the external reference frame for measuring extraction and suppression.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, secular_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a fixed, authoritative framework for family formation, resource transmission, and dispute resolution that claims to eliminate ambiguity by anchoring gender roles in divine text. Solves the coordination problem of 'who decides' by assigning decision-rights to men and enforcement to religious/state institutions.
% TRANSFER_FUNCTION: Moves legal authority, material resources (inheritance, mahr, maintenance obligations), and epistemic credibility (testimony weight) from women to male guardians and institutional interpreters. The transfer is justified as divine distribution, not social convention.
% ABSENT_VOICES: Women subject to the constraint's most severe restrictions (e.g., those denied divorce, custody, or mobility) are structurally absent from the interpretive bodies that author the reading. Reformist scholars and women's rights activists are excluded from official fatwa councils and legislative drafting committees. Their absence is not accidental — the reading's claim to timelessness requires that no living constituency can revise it.
% DISAPPEARANCE_RATIONALE: If the literal-hierarchical reading vanished overnight, family law codes in 30+ countries would lose their primary legitimating source. Inheritance distribution, marriage guardianship, testimony rules, and divorce asymmetries would become open legislative questions. Women's legal standing would shift immediately in courts. The religious institutional ecosystem built on interpretive monopoly would face existential crisis. The world rearranges because the constraint is the load-bearing wall of the current structure.
% FOUNDING_PROBLEM: 7th-century Arabian tribal society lacked a unified legal framework for inheritance, marriage, and testimony. The verses provided specific, enforceable rules that replaced variable customary practices and established a transparent (for that context) distribution logic — male relatives responsible for female dependents' maintenance in exchange for authority and larger inheritance shares.
% FOUNDING_PROBLEM_CORROBORATION: Classical tafsir (Tabari, Ibn Kathir) and fiqh manuals explicitly link the rules to their historical occasion (asbab al-nuzul): 4:11 revealed after a dispute over inheritance of a childless man; 2:282 addressing commercial contracts in a low-literacy trading context; 4:34 addressing a specific domestic dispute. Modern historians of early Islam (Crone, Hallaq, Ahmed) corroborate that the rules solved concrete coordination problems of that time. No major classical source claims the founding problem (absence of any inheritance/marriage/testimony framework) persists today — contemporary societies have comprehensive legal systems. The reading's own beneficiaries (traditionalist scholars) do not argue the founding problem is live; they argue the solution is divinely fixed regardless of the problem's status.
narrative_ontology:disappearance_verdict(quranic_gender_verses__literal_hierarchical, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__literal_hierarchical, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__literal_hierarchical, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(quranic_gender_verses__literal_hierarchical, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__literal_hierarchical, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.78) is high because the constraint transfers concrete legal rights and material resources from women to men/institutions across the entire life course — inheritance, marriage, divorce, custody, testimony, mobility. The transfer is not marginal; it is constitutive of legal personhood. Suppression (0.82) is very high because the constraint is maintained by state enforcement (court orders, police, border controls), religious authority (fatwas, excommunication threats), and social enforcement (family honor, community ostracism). Alternatives are not merely discouraged — they are criminalized (apostasy laws) or rendered legally void. Theater ratio (0.25) is moderate: the coordination function (clear inheritance rules, contract reliability) is real but shrinking relative to the extraction function as modern commercial and family life outgrows the 7th-century framework. Accessibility collapse (0.78) is high because the constraint claims divine immutability — alternatives are not just unavailable, they are framed as heresy. Resistance (0.55) is substantial and growing: reform movements, strategic litigation, and women's organizing exist but face severe repression.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (religious courts), the constraint appears as mountain — divine law, unchangeable, providing order. From the beneficiary seat (male heads), it appears as rope — a coordination mechanism that allocates responsibility and authority fairly. From the payer seats (women), it appears as snare — enforced extraction with no exit. The engine computes this divergence from the declared power, exit_options, and beneficiary/victim structure. The authored claim (snare) reflects the payer-seat reality, which is the extraction referent.
 *
 * DIRECTIONALITY LOGIC:
 *   Male household heads are structural beneficiaries (d ≈ 0.15): they receive authority and resources with mobile exit (can renounce role, though rarely do). Religious courts and state institutions are agenda-setters with institutional power and constrained exit (reform threatens their legitimacy base). Women subject to guardianship are identity-locked payers (d ≈ 0.9): their self-concept, family ties, and religious identity are fused with the constraint; exit means existential rupture. Female heirs and women in testimony are constrained/trapped payers with limited procedural recourse. Reformist scholars and activists are excluded — their structural position is created by the constraint's need to foreclose interpretive competition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (no unified family law in 7th-century Arabia) is dead — modern states have comprehensive legal codes. The constraint persists because the reading's authority is grounded in the claim that the solution is divine, not functional. This is mandatrophy: the mandate (divine ordinance) has outlived its function (solving the coordination problem of tribal customary law). The reading resolves the mandatrophy by denying it — the constraint is not a response to a problem but an eternal truth. The engine's mandatrophy detection should flag this: founding_problem_status=dead + disappearance_verdict=world_rearranges = capture/zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (state enforcement, legal penalties) or internalized (religious identity, belief in divine justice, fear of afterlife consequences)?',
    'Post-reform suppression trajectory: in jurisdictions that have reformed family law (e.g., Tunisia, Morocco), does women''s reported autonomy increase immediately, or do internalized norms persist? Comparative study of women''s self-reported constraint perception across legal regimes.',
    'If internalized suppression is significant, the constraint''s effective suppression exceeds the structural measure — women carry the constraint with them after legal exit. This would increase effective extraction for identity-locked payers and support snare classification even under formal legal reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in religious gender constraints').

omega_variable(
    reading_boundary_foreclosure,
    'Does the literal-hierarchical reading''s core premise (verses as timeless legislative commands) logically foreclose the contextual_egalitarian reading''s core premise (verses as historically situated steps), or do they merely coexist as competing positions?',
    'Formal analysis of the logical structure of each reading''s axiom set. If the literal reading''s axioms entail the falsity of the contextual reading''s axioms (and vice versa) within a single doxastic framework, foreclosure holds. If both can be held by different parties without internal contradiction in either framework, coexistence holds.',
    'Foreclosure would mean the kernel admits no stable pluralism — one reading must displace the other for any agent. Coexistence means the contest is political/institutional, not logical. This affects whether the engine models the kernel as a single constraint with competing readings or as structurally distinct constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_foreclosure, conceptual, 'Logical relationship between sibling readings of the quranic_gender_verses kernel').

omega_variable(
    divine_ordinance_vs_constructed_ambiguity,
    'Is the constraint''s claimed divine source a genuine natural-law-type immutability (mountain), or a constructed claim that benefits identifiable institutions (false summit)?',
    'Historical analysis of the reading''s emergence: when and by whom was the ''timeless legislative command'' interpretation consolidated? Does the historical record show strategic deployment by religious/state institutions to legitimize authority? Comparative study with other religious legal systems that underwent similar transitions (e.g., Catholic canon law, Jewish halakha).',
    'If constructed, the mountain claim is a false summit — FSM signature should trigger reclassification to tangled_rope or snare. The declared beneficiaries (religious courts, state institutions) are the agents who benefit from the natural-law framing. This omega is required by schema because a mountain-claimed constraint declares beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_ordinance_vs_constructed_ambiguity, conceptual, 'Natural-law vs. constructed authority in the literal-hierarchical reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__literal_hierarchical, 622, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t622, quranic_gender_verses__literal_hierarchical, theater_ratio, 622, 0.12).
narrative_ontology:measurement(qura_tr_t900, quranic_gender_verses__literal_hierarchical, theater_ratio, 900, 0.18).
narrative_ontology:measurement(qura_tr_t1300, quranic_gender_verses__literal_hierarchical, theater_ratio, 1300, 0.22).
narrative_ontology:measurement(qura_tr_t1700, quranic_gender_verses__literal_hierarchical, theater_ratio, 1700, 0.24).
narrative_ontology:measurement(qura_tr_t1950, quranic_gender_verses__literal_hierarchical, theater_ratio, 1950, 0.26).
narrative_ontology:measurement(qura_tr_t2024, quranic_gender_verses__literal_hierarchical, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(qura_be_t622, quranic_gender_verses__literal_hierarchical, base_extractiveness, 622, 0.65).
narrative_ontology:measurement(qura_be_t900, quranic_gender_verses__literal_hierarchical, base_extractiveness, 900, 0.72).
narrative_ontology:measurement(qura_be_t1300, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1300, 0.75).
narrative_ontology:measurement(qura_be_t1700, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1700, 0.76).
narrative_ontology:measurement(qura_be_t1950, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1950, 0.74).
narrative_ontology:measurement(qura_be_t2024, quranic_gender_verses__literal_hierarchical, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t622, quranic_gender_verses__literal_hierarchical, suppression_requirement, 622, 0.55).
narrative_ontology:measurement(qura_su_t900, quranic_gender_verses__literal_hierarchical, suppression_requirement, 900, 0.68).
narrative_ontology:measurement(qura_su_t1300, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1300, 0.75).
narrative_ontology:measurement(qura_su_t1700, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1700, 0.79).
narrative_ontology:measurement(qura_su_t1950, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1950, 0.81).
narrative_ontology:measurement(qura_su_t2024, quranic_gender_verses__literal_hierarchical, suppression_requirement, 2024, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__literal_hierarchical, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(quranic_gender_verses__literal_hierarchical, 0.12).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, quranic_gender_verses__contextual_egalitarian).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, quranic_gender_verses__progressive_abrogation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the quranic_gender_verses kernel. The literal_hierarchical reading has high extractiveness (0.78) and suppression (0.82) because it treats the verses as binding legislative text. The contextual_egalitarian reading (separate story) has low extractiveness (~0.15) by re-anchoring the verses in historical context and subordinating them to equity principles. The progressive_abrogation reading (separate story) has moderate extractiveness (~0.35) by accepting partial supersession but retaining some gender-differentiated rules. The three stories form a constraint family linked by network.affects_constraints. The upstream reading (literal_hierarchical) influences the downstream readings by controlling the institutional interpretive monopoly that defines the terms of the contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quranic_gender_verses__literal_hierarchical, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
