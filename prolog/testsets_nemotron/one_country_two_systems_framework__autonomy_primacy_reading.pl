% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__autonomy_primacy_reading, []).

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
 *   constraint_id: one_country_two_systems_framework__autonomy_primacy_reading
 *   human_readable: One Country Two Systems - Autonomy Primacy Reading
 *   domain: constitutional_law/political_systems/state_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the autonomy_primacy_reading of the
 *   one_country_two_systems_framework kernel. The reading holds that the
 *   Sino-British Joint Declaration and Basic Law guarantee Hong Kong
 *   substantive autonomy — including civil liberties, judicial independence,
 *   and a democratic reform pathway — as treaty-protected rights enforceable
 *   against mainland interference. Mainland intervention beyond the Basic
 *   Law's explicit provisions constitutes treaty violation. The claimed type
 *   is rope: a coordination mechanism solving the genuine problem of
 *   governing a common law financial center under socialist sovereignty. The
 *   metrics reflect the reading's own assessment: low base extractiveness
 *   (0.22) because the arrangement primarily coordinates; moderate theater
 *   (0.31) because performative autonomy maintenance exists alongside genuine
 *   function; significant resistance (0.58) because the reading's
 *   beneficiaries actively defend it. The measurement series captures the
 *   1997-2024 interval on a shared grid, showing extraction and suppression
 *   peaking during 2019-2022 before the reading's proponents argue the
 *   constraint's core structure remains intact.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__autonomy_primacy_reading, 0.22).
domain_priors:suppression_score(one_country_two_systems_framework__autonomy_primacy_reading, 0.18).
domain_priors:theater_ratio(one_country_two_systems_framework__autonomy_primacy_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__autonomy_primacy_reading, rope).
narrative_ontology:human_readable(one_country_two_systems_framework__autonomy_primacy_reading, "One Country Two Systems - Autonomy Primacy Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__autonomy_primacy_reading, "constitutional_law/political_systems/state_sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__autonomy_primacy_reading, '60ee6d78-7e15-498c-bb31-d13634dfef6f').
narrative_ontology:cs_kernel_codification('60ee6d78-7e15-498c-bb31-d13634dfef6f', formalized).
narrative_ontology:cs_authority_grounding('60ee6d78-7e15-498c-bb31-d13634dfef6f', lineage).
narrative_ontology:cs_interpretation_layer_present('60ee6d78-7e15-498c-bb31-d13634dfef6f').
narrative_ontology:cs_reading_relation('60ee6d78-7e15-498c-bb31-d13634dfef6f', one_country_two_systems_framework__sovereignty_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('60ee6d78-7e15-498c-bb31-d13634dfef6f', one_country_two_systems_framework__balanced_coexistence_reading, coexists_with).
narrative_ontology:cs_axiom('60ee6d78-7e15-498c-bb31-d13634dfef6f', foundational, joint_declaration_creates_justiciable_rights).
narrative_ontology:cs_axiom_status(joint_declaration_creates_justiciable_rights, holdable).
narrative_ontology:cs_axiom_grounding('60ee6d78-7e15-498c-bb31-d13634dfef6f', joint_declaration_creates_justiciable_rights, conventional).
narrative_ontology:cs_axiom('60ee6d78-7e15-498c-bb31-d13634dfef6f', foundational, judicial_review_binds_npsc_interpretation).
narrative_ontology:cs_axiom_status(judicial_review_binds_npsc_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('60ee6d78-7e15-498c-bb31-d13634dfef6f', judicial_review_binds_npsc_interpretation, conventional).
narrative_ontology:cs_axiom('60ee6d78-7e15-498c-bb31-d13634dfef6f', secondary, democratic_reform_is_treaty_obligation_not_concession).
narrative_ontology:cs_axiom_status(democratic_reform_is_treaty_obligation_not_concession, holdable).
narrative_ontology:cs_axiom_grounding('60ee6d78-7e15-498c-bb31-d13634dfef6f', democratic_reform_is_treaty_obligation_not_concession, conventional).
narrative_ontology:cs_reference_frame('60ee6d78-7e15-498c-bb31-d13634dfef6f', id_1997_joint_declaration_basic_law_constitutional_settlement).
narrative_ontology:cs_drift_state('60ee6d78-7e15-498c-bb31-d13634dfef6f', post_2020_national_security_law_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('60ee6d78-7e15-498c-bb31-d13634dfef6f', '2026-08-15T14:32:17Z').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_residents).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_legal_profession).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_civil_society_organizations).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, international_treaty_monitoring_bodies).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, prc_central_government_executive_authority).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, pro_beijing_political_factions_in_hk).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_business_elite).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_business_elite).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Experience civil liberties (speech, assembly, press) and judicial protection as treaty-guaranteed rights. Their exit options are constrained by home, career, family, and identity ties to Hong Kong; emigration is possible but costly. They benefit from the constraint's maintenance of autonomy but bear costs when autonomy erodes.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_residents, beneficiary,
    moderate, biographical, constrained, local).

% Operate within a common law system with independent judicial review. They shape the constraint through litigation, bar association advocacy, and judicial appointments processes. Their professional identity and livelihood depend on the common law framework; exit means leaving the jurisdiction or retraining in a different legal system.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_legal_profession, beneficiary,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_legal_profession, agenda_setter).

% Mobilize around autonomy, democratic reform, and rights protection. They depend on the legal space the constraint creates for their operation. Their exit options are constrained by registration requirements, funding channels, and the risk of dissolution under national security provisions.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_civil_society_organizations, beneficiary,
    organized, biographical, constrained, local).

% Monitor compliance with the Sino-British Joint Declaration registered at the UN. They issue reports, conduct reviews, and exert diplomatic pressure but lack direct enforcement power. Their analytical seat sees the full structural relationship between treaty text, domestic implementation, and geopolitical context.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, international_treaty_monitoring_bodies, observer,
    institutional, generational, analytical, global).

% Holds sovereign authority over Hong Kong under the Basic Law. Bears the political cost of honoring autonomy commitments when they conflict with national security or unity imperatives. Can reinterpret the Basic Law through NPCSC interpretations, control chief executive appointments, and deploy national security apparatus. Their exit is arbitrage-grade: they can reshape the constraint's meaning through authoritative interpretation.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, prc_central_government_executive_authority, payer,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__autonomy_primacy_reading, prc_central_government_executive_authority, agenda_setter).

% Operate within Hong Kong's political system but depend on central government patronage. Bear costs when autonomy protections limit their ability to advance integrationist policies. Their exit is mobile: they can shift between local political competition and mainland-directed roles, but their influence depends on the constraint's erosion.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, pro_beijing_political_factions_in_hk, payer,
    powerful, biographical, mobile, local).

% Maintain consular presence and bilateral agreements predicated on Hong Kong's distinct legal and economic status. They observe autonomy erosion as a signal for treaty revision, sanctions, or diplomatic downgrades. Their analytical seat tracks the constraint's credibility as an international commitment.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, foreign_governments_with_consular_presence, observer,
    institutional, generational, analytical, global).

% Benefit from Hong Kong's common law commercial framework, independent judiciary, and free capital flows — the autonomy dividend. Also bear costs when political instability threatens asset values. Their exit is mobile: capital and residency are portable, but the Hong Kong platform's value depends on the constraint's maintenance.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_business_elite, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_business_elite, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable legal and institutional framework for Hong Kong's distinctiveness within PRC sovereignty: common law judiciary, civil liberties regime, separate customs territory, and international treaty personality — solving the coordination problem of governing a global financial city under a different political system.
% TRANSFER_FUNCTION: Transfers interpretive authority over the Basic Law from NPCSC unilateral interpretation toward Hong Kong courts' judicial review; transfers political accountability from central appointment toward local democratic mandate; transfers enforcement of rights from executive discretion toward judicial remedy.
% ABSENT_VOICES: Hong Kong residents who emigrated since 2020 (over 200,000) and cannot vote or participate; mainland Chinese citizens who would be affected by Hong Kong precedent for autonomy demands; Taiwan population for whom the framework was originally a unification model — all structurally excluded from the current negotiation.
% DISAPPEARANCE_RATIONALE: If the autonomy primacy reading vanished overnight, Hong Kong's common law judiciary would lose its constitutional anchor against NPCSC interpretation, civil liberties would revert to mainland standards without treaty protection, the separate customs territory would collapse, and the international legal basis for Hong Kong's distinct status would evaporate — the world would rearrange toward full integration.
% FOUNDING_PROBLEM: How to transfer sovereignty over Hong Kong from UK to PRC while preserving the economic, legal, and social systems that made Hong Kong a global financial center and preventing the disruption that abrupt integration would cause to residents, businesses, and international confidence.
% FOUNDING_PROBLEM_CORROBORATION: The UK government (as co-signatory to the Joint Declaration) attests the founding problem persists through its six-monthly reports to Parliament documenting autonomy erosion. The Hong Kong Bar Association and Law Society corroborate from the professional seat. The PRC government contends the founding problem was solved in 1997 and the arrangement now serves only national unity. Academic consensus outside the benefiting parties (China studies, comparative constitutional law) treats the founding problem as live and the autonomy commitment as unfulfilled.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__autonomy_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__autonomy_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(one_country_two_systems_framework__autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__autonomy_primacy_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__autonomy_primacy_reading_tests).
:- end_tests(one_country_two_systems_framework__autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed rope type reflects this reading's structural self-understanding: a coordination mechanism with genuine beneficiaries (Hong Kong residents, legal profession, civil society) and identifiable payers (PRC executive authority, pro-Beijing factions) who bear the constraint of limited intervention. The extractiveness score (0.22) is the reading's own assessment of how much the autonomy framework extracts from the central government's sovereign prerogative — not zero because the constraint binds the center, but low because the reading views this binding as coordination, not extraction. Theater ratio (0.31) captures the gap between the framework's formal guarantees and their partial hollowing: NPCSC interpretations, Article 23 legislation, and electoral reforms perform autonomy while constraining its substance. Accessibility collapse (0.42) is moderate: alternatives (full integration, independence) are politically collapsed but legally contested. Resistance (0.58) is high because the reading's beneficiaries — especially the legal profession and civil society — mount sustained legal, political, and international advocacy.
 *
 * PERSPECTIVAL GAP:
 *   The PRC executive authority (institutional power, arbitrage exit) experiences this constraint as high extraction: it surrenders interpretive monopoly over the Basic Law, accepts judicial review of executive acts, and tolerates a political system that produces outcomes contrary to central preferences. Hong Kong residents (moderate power, constrained exit) experience it as low extraction: the constraint protects their rights and enables their life plans. The legal profession (organized power, constrained exit) sits at the coordination interface: they both benefit from and maintain the constraint. The engine computes per-seat effective extraction from these structural positions — the autonomy primacy reading claims the coordination is genuine and the extraction reciprocal; the sovereignty primacy reading would compute the reverse.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: hong_kong_residents, hong_kong_legal_profession, hong_kong_civil_society_organizations, international_treaty_monitoring_bodies. These agents gain rights, professional framework, operational space, and monitoring mandate from the constraint. Victims declared: prc_central_government_executive_authority, pro_beijing_political_factions_in_hk. These agents bear the constraint's limitation on sovereign discretion and political control. The hong_kong_business_elite holds dual role: beneficiary of legal certainty and capital freedom, payer of political instability risk. Directionality derives from this structure: beneficiaries have constrained exit (tied to Hong Kong) but low d because the constraint subsidizes their position; payers have mobile/arbitrage exit and high d because the constraint extracts from their authority. The reading's central claim — that this is coordination, not extraction — is exactly the claim/metric divergence the engine measures.
 *
 * MANDATROPHY ANALYSIS:
 *   The autonomy primacy reading prevents mislabeling the coordination function as pure extraction by insisting the constraint solves a genuine founding problem (peaceful sovereignty transfer with system preservation) and that beneficiaries are not merely incidental. However, the reading risks mislabeling extraction as coordination if the constraint's current operation primarily serves to legitimize central control while autonomy erodes — the theater ratio captures this risk. The mandatrophy question: does the constraint still solve the founding problem, or has it become a ritual performance of autonomy while substantive self-government atrophies? The reading answers 'contested' — the founding problem persists (Hong Kong's distinct systems remain fragile) but the arrangement's capacity to solve it is disputed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_enforceability_vs_political_reality,
    'Is the Sino-British Joint Declaration legally enforceable as a treaty against the PRC, or has it become a political commitment without judicial remedy?',
    'ICJ advisory opinion or UN treaty body determination on justiciability; domestic court rulings in third-party jurisdictions on treaty direct effect; state practice of other Joint Declaration signatories.',
    'If enforceable, the autonomy primacy reading''s low extractiveness claim holds — the constraint binds the PRC as law. If non-justiciable, the constraint becomes a snare: the PRC extracts compliance while the treaty provides cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_enforceability_vs_political_reality, conceptual, 'Whether the treaty framework creates legal obligation or political aspiration.').

omega_variable(
    judicial_independence_measurement,
    'Can judicial independence be measured as a continuous variable, or does it collapse to binary (exists/doesn''t) once NPCSC interpretation power is exercised?',
    'Comparative analysis of Hong Kong court decisions on sensitive cases pre/post 2020; citation network analysis of common law vs. mainland precedent; judicial appointment and security of tenure metrics.',
    'If binary collapse, the reading''s coordination function fails catastrophically at a threshold — the constraint is a scaffold that has lost its sunset. If continuous degradation, the rope/tangled_rope distinction tracks the actual extraction trajectory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_independence_measurement, empirical, 'Whether judicial independence degrades continuously or collapses at a structural threshold.').

omega_variable(
    democratic_reform_pathway_viability,
    'Does the Basic Law''s democratic reform pathway (universal suffrage for CE and LegCo) remain structurally viable, or has the 2021 electoral reform permanently foreclosed it?',
    'Textual analysis of 2021 Annex I/II amendments vs. Basic Law Articles 45/68; NPCSC interpretation history; comparative constitutional amendment doctrine.',
    'If foreclosed, the reading''s coordination function loses its temporal dimension — the constraint becomes a piton maintaining the form of autonomy without the substance of self-government. If viable, the reading''s claimed rope type holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_reform_pathway_viability, conceptual, 'Whether the democratic reform commitment is legally alive or structurally dead.').

omega_variable(
    kernel_framing_ambiguity,
    'Does the one_country_two_systems_framework kernel refer to the treaty text (Joint Declaration + Basic Law), the institutional arrangement (1997-present practice), or the political discourse (competing sovereignty/autonomy narratives)?',
    'Genealogical analysis of the kernel''s stabilization across UK-PRC negotiations, Basic Law drafting, and post-1997 institutionalization; discourse analysis of how each reading anchors its claim.',
    'If the kernel is the treaty text, autonomy_primacy_reading has structural priority. If the kernel is institutional practice, sovereignty_primacy_reading gains ground. If the kernel is discourse, balanced_coexistence_reading captures the lived reality. This framing determines which reading''s ε is the referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'What the kernel actually is — text, practice, or discourse — and how that choice privileges one reading''s structural account.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__autonomy_primacy_reading, 1997, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(octs_autonomy_tr_t1997, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 1997, 0.12).
narrative_ontology:measurement(octs_autonomy_tr_t2003, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2003, 0.15).
narrative_ontology:measurement(octs_autonomy_tr_t2014, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2014, 0.22).
narrative_ontology:measurement(octs_autonomy_tr_t2019, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2019, 0.28).
narrative_ontology:measurement(octs_autonomy_tr_t2020, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2020, 0.42).
narrative_ontology:measurement(octs_autonomy_tr_t2022, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2022, 0.38).
narrative_ontology:measurement(octs_autonomy_tr_t2024, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2024, 0.31).

% Extraction over time
narrative_ontology:measurement(octs_autonomy_be_t1997, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 1997, 0.08).
narrative_ontology:measurement(octs_autonomy_be_t2003, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2003, 0.12).
narrative_ontology:measurement(octs_autonomy_be_t2014, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2014, 0.18).
narrative_ontology:measurement(octs_autonomy_be_t2019, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2019, 0.25).
narrative_ontology:measurement(octs_autonomy_be_t2020, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2020, 0.35).
narrative_ontology:measurement(octs_autonomy_be_t2022, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2022, 0.38).
narrative_ontology:measurement(octs_autonomy_be_t2024, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2024, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(octs_autonomy_su_t1997, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 1997, 0.05).
narrative_ontology:measurement(octs_autonomy_su_t2003, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2003, 0.18).
narrative_ontology:measurement(octs_autonomy_su_t2014, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2014, 0.25).
narrative_ontology:measurement(octs_autonomy_su_t2019, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2019, 0.45).
narrative_ontology:measurement(octs_autonomy_su_t2020, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement(octs_autonomy_su_t2022, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2022, 0.52).
narrative_ontology:measurement(octs_autonomy_su_t2024, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2024, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__autonomy_primacy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(one_country_two_systems_framework__autonomy_primacy_reading, 0.1).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework__sovereignty_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework__balanced_coexistence_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_national_security_law_constraint).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_electoral_reform_2021_constraint).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, sino_british_joint_declaration_monitoring_constraint).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the one_country_two_systems_framework kernel into three readings with distinct ε values and beneficiary/victim structures. The autonomy_primacy_reading (this story) claims low extraction (0.22) with residents/legal-profession as beneficiaries and PRC-executive as payer. The sovereignty_primacy_reading would claim higher extraction with inverted beneficiary/victim structure. The balanced_coexistence_reading would claim intermediate extraction with negotiation-as-coordination function. All three link via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(one_country_two_systems_framework__autonomy_primacy_reading, institutional, 0.85).
constraint_indexing:directionality_override(one_country_two_systems_framework__autonomy_primacy_reading, moderate, 0.25).
constraint_indexing:directionality_override(one_country_two_systems_framework__autonomy_primacy_reading, organized, 0.3).
constraint_indexing:directionality_override(one_country_two_systems_framework__autonomy_primacy_reading, powerful, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
