% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__privacy_fundamental_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article17_erasure_right__privacy_fundamental_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article17_erasure_right__privacy_fundamental_reading
 *   human_readable: Article 17 GDPR Erasure Right (Privacy Fundamental Reading)
 *   domain: technology/data-protection/fundamental-rights
 *
 * SUMMARY:
 *   Article 17 GDPR (the 'right to be forgotten' or right to erasure) grants
 *   EU residents the right to request deletion of personal data held by
 *   platforms and other data controllers, subject to limited exceptions
 *   (legal compliance, public interest, freedom of expression). This
 *   constraint story instantiates the privacy-fundamental reading: Article 17
 *   is understood as a foundational reassertion of individual data
 *   sovereignty against corporate data accumulation. The constraint functions
 *   as coordination (establishes a principle that individuals retain claims
 *   on information about themselves) but imposes real compliance costs on
 *   platforms and depends on regulatory enforcement to resist platform
 *   obstruction. The claimed type is rope (genuine coordination solving the
 *   asymmetry problem); the measured extractiveness is low (0.28) because the
 *   right primarily redistributes agency rather than extracting rents, but
 *   rises modestly over the interval as platforms implement more burdensome
 *   compliance procedures and exceptions expand through case law.
 *
 * KEY AGENTS:
 *   - Data subjects (EU residents): structurally powerless, identity-locked to their data, benefit from restoration of informational autonomy.
 *   - Digital platforms (social media, search, advertising): institutional power, globally mobile, but constrained in EU by regulation; bear compliance costs and lose data retention leverage.
 *   - Data protection authorities (national DPAs, EDPB): institutional power, agenda-setters for interpretation and enforcement.
 *   - Privacy advocates (civil society): organized power, beneficiaries of a rights regime, mobile (can shift jurisdiction).
 *   - Law enforcement and researchers: excluded, would contest the reading's scope.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__privacy_fundamental_reading, 0.28).
domain_priors:suppression_score(article17_erasure_right__privacy_fundamental_reading, 0.15).
domain_priors:theater_ratio(article17_erasure_right__privacy_fundamental_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__privacy_fundamental_reading, rope).
narrative_ontology:human_readable(article17_erasure_right__privacy_fundamental_reading, "Article 17 GDPR Erasure Right (Privacy Fundamental Reading)").
narrative_ontology:topic_domain(article17_erasure_right__privacy_fundamental_reading, "technology/data-protection/fundamental-rights").

domain_priors:requires_active_enforcement(article17_erasure_right__privacy_fundamental_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__privacy_fundamental_reading, 'ef0b20c0-5fda-443f-9e04-0043f8dacb75').
narrative_ontology:cs_kernel_codification('ef0b20c0-5fda-443f-9e04-0043f8dacb75', formalized).
narrative_ontology:cs_authority_grounding('ef0b20c0-5fda-443f-9e04-0043f8dacb75', lineage).
narrative_ontology:cs_interpretation_layer_present('ef0b20c0-5fda-443f-9e04-0043f8dacb75').
narrative_ontology:cs_reading_relation('ef0b20c0-5fda-443f-9e04-0043f8dacb75', article17_erasure_right__competitive_moat_reading, coexists_with).
narrative_ontology:cs_reading_relation('ef0b20c0-5fda-443f-9e04-0043f8dacb75', article17_erasure_right__censorship_mechanism_reading, coexists_with).
narrative_ontology:cs_axiom('ef0b20c0-5fda-443f-9e04-0043f8dacb75', foundational, individual_data_sovereignty_fundamental).
narrative_ontology:cs_axiom_status(individual_data_sovereignty_fundamental, holdable).
narrative_ontology:cs_axiom_grounding('ef0b20c0-5fda-443f-9e04-0043f8dacb75', individual_data_sovereignty_fundamental, deontological).
narrative_ontology:cs_axiom('ef0b20c0-5fda-443f-9e04-0043f8dacb75', foundational, informational_asymmetry_requires_correction).
narrative_ontology:cs_axiom_status(informational_asymmetry_requires_correction, holdable).
narrative_ontology:cs_axiom_grounding('ef0b20c0-5fda-443f-9e04-0043f8dacb75', informational_asymmetry_requires_correction, empirically_contingent).
narrative_ontology:cs_reference_frame('ef0b20c0-5fda-443f-9e04-0043f8dacb75', individual_informational_self_determination).
narrative_ontology:cs_drift_state('ef0b20c0-5fda-443f-9e04-0043f8dacb75', contemporary_2026, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ef0b20c0-5fda-443f-9e04-0043f8dacb75', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, data_subjects).
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, individual_privacy_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, privacy_advocates).
narrative_ontology:constraint_victim(article17_erasure_right__privacy_fundamental_reading, digital_platforms).
narrative_ontology:constraint_vindicates(article17_erasure_right__privacy_fundamental_reading, informational_self_determination_doctrine).
narrative_ontology:constraint_vindicates(article17_erasure_right__privacy_fundamental_reading, data_as_personal_property_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% European Union residents who generate data through platform interaction. Under this reading, they possess a fundamental right to control their informational identity: to request erasure of personal data held by platforms, with limited exceptions. Exit from the constraint would mean accepting permanent surveillance and loss of informational autonomy. They benefit from restored agency over their digital footprint and reduced risk of derivative harms (discrimination, manipulation, re-identification).
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, data_subjects, beneficiary,
    powerless, biographical, identity_locked, regional).

% Tech platforms (social media, search, advertising networks) must implement erasure request processing, compliance verification, technical infrastructure for data deletion confirmation, and cross-system data purging. They bear compliance costs (engineering, legal review, request processing overhead) and lose data retention capacity that previously enabled behavioral modeling, personalization, and secondary monetization. They can technically exit by withdrawing from EU markets (constrained by economic scale, but possible).
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, digital_platforms, payer,
    institutional, generational, constrained, global).

% National data protection authorities and the European Data Protection Board interpret and enforce Article 17, issuing guidance on what constitutes 'personal data' and legitimate erasure grounds, handling complaints, and imposing fines for non-compliance. They set the effective scope of the right through case law and regulatory guidance.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, data_protection_authorities, agenda_setter,
    institutional, generational, analytical, regional).

% Non-EU platform users are structurally excluded from Article 17 rights; platforms often implement regional carve-outs. They would benefit from similar erasure rights but are not in the conversation, their interests are not represented in the regulatory design.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, platform_users_non_eu, excluded,
    powerless, biographical, trapped, global).

% Police and prosecutorial authorities argue that broad erasure rights hamper investigations and evidence preservation. They are structurally excluded from the primary beneficiary set and would contest the scope of exceptions; their voices are present only in litigation and policy feedback, not in agenda-setting.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, law_enforcement_agencies, excluded,
    institutional, generational, constrained, national).

% Researchers studying platform behavior, algorithmic harm, and social dynamics lose access to longitudinal datasets as erasure requests delete historical records. They would contest the breadth of the right but are not agenda-setters; they influence through evidence and testimony.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, academic_researchers, excluded,
    moderate, biographical, constrained, regional).

% Civil society organizations focused on privacy, data rights, and individual autonomy champion Article 17 as a cornerstone of informational self-determination. They lobby for expansive interpretation, press cases, and frame erasure as fundamental. They benefit from a rights regime that centers individual agency.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, privacy_advocates, beneficiary,
    organized, generational, mobile, regional).

% CJEU, national courts, and administrative tribunals interpret Article 17's scope, legitimate exceptions, and enforcement through case law. They adjudicate disputes between data subjects and platforms, and shape the effective meaning of the right through landmark decisions.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, courts_and_tribunals, observer,
    institutional, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article17_erasure_right__privacy_fundamental_reading, diffuse).
narrative_ontology:fixing_cost_class(article17_erasure_right__privacy_fundamental_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of informational asymmetry and platform control: absent a right to erasure, individuals cannot manage their informational footprint, platforms accumulate indefinite data leverage, and no mechanism exists for individuals to reset their relationship to their own data. Article 17 establishes a decoupling: platforms retain no indefinite claim on personal data; individuals have a voice in data lifecycle.
% TRANSFER_FUNCTION: Transfers control over personal data from platforms (who hold indefinite retention authority) back to individuals (who gain the right to request deletion). Transfers compliance burden to platforms (engineering erasure systems, auditing retention, handling exceptions). Transfers reduced monetization capacity (platforms lose behavioral data for targeting and modeling).
% ABSENT_VOICES: Non-EU platform users are structurally excluded — they have no Article 17 right and have not been represented in the reading's design. Law enforcement and prosecutorial agencies are excluded from the primary beneficiary framing and would argue erasure rights obstruct legitimate investigation. Academic researchers studying platform behavior are excluded; they would contest loss of access to longitudinal data. Platforms themselves are payers, not beneficiaries, and their efficiency arguments are present only as objections to be overcome, not as co-designers.
% DISAPPEARANCE_RATIONALE: If Article 17 and the right to erasure disappeared overnight, EU residents would lose the primary lever for controlling their informational identity; platforms would retain indefinite data on their users unless explicitly legislated otherwise; the data protection regime would shift from individual-centric to platform-permissive. The coordinating principle (individuals have a claim on their own data) would evaporate, and the default would revert to platform ownership.
% FOUNDING_PROBLEM: Digital platforms accumulate and retain indefinite personal data on individuals without meaningful consent or control mechanisms; individuals have no practical way to reclaim or delete information about themselves once disclosed; this asymmetry enables behavioral profiling, manipulation, discrimination, and concentration of power over individual identity and autonomy in corporate hands.
% FOUNDING_PROBLEM_CORROBORATION: Data protection authorities attest the founding problem is ongoing — surveillance capitalism continues to accumulate data at scale. Academic research outside the beneficiary community (Shoshana Zuboff, Carole Cadwalladr investigations, Amnesty International reports) corroborates that platforms retain indefinite data on individuals without meaningful individual control. Competitive moat and censorship readings contest the reading's framing of the solution, but neither contests that the founding problem is real.
narrative_ontology:disappearance_verdict(article17_erasure_right__privacy_fundamental_reading, world_rearranges).
narrative_ontology:founding_problem_status(article17_erasure_right__privacy_fundamental_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__privacy_fundamental_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(article17_erasure_right__privacy_fundamental_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article17_erasure_right__privacy_fundamental_reading_tests).
:- end_tests(article17_erasure_right__privacy_fundamental_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28 at 2026) because the constraint's primary function is redistributing agency (from platforms to individuals) rather than concentrating wealth. Suppression requirement is also low (0.15) because the constraint operates through positive rights (individuals request deletion) rather than coercive enforcement against resisters—platforms comply because fines and reputation costs exceed resistance, not because they are forcibly suppressed. Theater ratio is modest and rising (0.08→0.22 over the interval) because: (1) early implementation (2018–2020) was genuine legal compliance; (2) by 2024–2026, platforms increasingly use exceptions ('legitimate interest,' 'legal obligation') to refuse erasure, and regulatory guidance has shifted to emphasizing friction and exceptions, so performance of erasure availability outpaces actual data deletion—this theater rise reflects Goodhart drift (the metric of requests fulfilled becomes decoupled from the metric of data actually erased). Accessibility collapse is high (0.72) because once a data subject understands they have a right to erasure, the alternative (permanent digital footprint under platform control) becomes structurally unavailable—they cannot unknow the right. Resistance is substantial (0.58) because platforms mount continuous legal and technical resistance: arguing for exceptions, implementing difficult request processes, lobbying authorities, and litigating scope boundaries.
 *
 * PERSPECTIVAL GAP:
 *   The platform seat and the data-subject seat compute to fundamentally different types. From the platform perspective, Article 17 is forced compliance (coerced by regulation, sustained by fines, confiscating their data assets)—textually closer to snare if one brackets the legitimacy of the rights claim. From the data-subject perspective, Article 17 is restoration of a pre-surveillance baseline (access to informational autonomy that should never have been forfeited)—textually rope or even mountain (a reassertion of natural individual sovereignty). The engine's per-seat computation will surface this divergence. The CLAIMED type is rope because the privacy-fundamental reading holds that the founding coordination problem (individuals lack control over their informational footprint) is genuine and the solution (a right to erasure) is the minimal coordinating mechanism needed. But the metrics reflect platform resistance and rising theater, so the computed type will likely diverge—a rope whose beneficiary has to fight continuously for the right's enforcement is structurally closer to tangled rope (coordination + enforcement asymmetry).
 *
 * DIRECTIONALITY LOGIC:
 *   Data subjects are full beneficiaries (d ≈ 0.1): they gain informational control and exit the permanent-record regime without bearing compliance costs. Platforms are targets (d ≈ 0.85): they lose data retention capacity and bear implementation costs, and their exit (geographic relocation) is constrained by economic scale and EU market importance. Privacy advocates are beneficiaries (d ≈ 0.15): they gain a fundamental-rights framework and influence over platforms, but they do not directly collect from the constraint's operation. The asymmetry is structural: the constraint redistributes power, not wealth; beneficiaries gain agency, not revenue; payers lose leverage, not capital. From the platform seat, this looks like extraction (constrained mobility, forced investment in compliance infrastructure, data retention privileges revoked). From the data subject seat, it looks like emancipation from a default regime of permanent, platform-controlled surveillance.
 *
 * MANDATROPHY ANALYSIS:
 *   Article 17 is NOT a case of mandatrophy (a function atrophied but the rule persists by inertia). The right is actively defended and expanded by advocacy organizations, regulators, and courts; the founding problem (data accumulation asymmetry) remains live and urgent; compliance is improving, not decaying. However, the rising theater ratio flags a secondary drift: the metric of 'erasure requests fulfilled' is decoupling from the metric of 'personal data actually erased.' Platforms are performing compliance (issuing denials with legal-language justifications, implementing slow request processes) while substantively resisting deletion where exceptions apply. This is Goodhart drift (the measured output diverges from the intended outcome), not mandatrophy. The constraint's function (redistributing informational agency) remains live; the performance (ease and completeness of actual deletion) is degrading. An omega variable documents this measurable gap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theater_drift_erasure_performance,
    'As platforms implement exceptions and friction more extensively, does the measured metric ''erasure requests fulfilled'' decouple from the outcome metric ''personal data actually erased''?',
    'Longitudinal audit of a sample of erasure requests: track whether data marked ''deleted'' is actually purged across all platform systems, backups, and derivative datasets. Compare request-fulfillment metrics published by platforms against audit findings.',
    'If decoupling is confirmed, the constraint''s theater ratio is higher than the compliance-focused metric captures, and the actual redistribution of informational agency is smaller than the legal right suggests. Suggests the constraint is degrading toward piton (performative compliance masking persistent data retention).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_drift_erasure_performance, empirical, 'Gap between erasure requests granted and data actually deleted from all systems.').

omega_variable(
    exception_scope_expansion,
    'Is the scope of legitimate-exception grounds (Article 17(3): legal obligation, public interest, freedom of expression, archival) expanding through case law and regulatory guidance in ways that systematically exclude certain data subjects from erasure?',
    'Analysis of CJEU and national tribunal decisions on Article 17 scope over the interval; coding of outcome (erasure granted vs. denied) against exception cited; tracking of DPA guidance amendments on exception interpretation.',
    'If exception scope is expanding asymmetrically (e.g., platforms successfully arguing ''legitimate interest'' in retaining user data for security purposes while denying similar claims from data subjects), the constraint''s actual reach is narrower than the stated right suggests, and beneficiary status becomes stratified (some data subjects retain agency; others lose it to exception claims). Would support competitive-moat and differential-beneficiary readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exception_scope_expansion, empirical, 'Whether exception scope is expanding in ways that reduce the right''s reach.').

omega_variable(
    geographic_carve_out_fragmentation,
    'Are platforms implementing region-specific versions of erasure procedures (stricter in EU, weaker or absent outside EU) in ways that fragment global data practices and impose differential burdens on borderless users?',
    'Comparative audit of platform erasure request procedures across jurisdictions; analysis of platform engineering documentation on regional feature gates; user reports of erasure availability.',
    'If fragmentation is substantial, non-EU users lose a protection available to EU residents, and platforms bear compliance cost only for EU data while retaining asymmetric advantages in non-regulated markets. Would support competitive-moat and differential-beneficiary readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_carve_out_fragmentation, empirical, 'Whether platforms implement differential erasure procedures by jurisdiction.').

omega_variable(
    identity_locked_vs_constrained_distinction,
    'For data subjects, is the exit from the constraint (rejecting erasure and accepting permanent platform retention) categorized correctly as identity_locked, or is it better modeled as constrained (economic exit available but costly)?',
    'Post-exit survey of users who deleted accounts and data: measure whether the exit was chosen (possible alternative account on a different platform), forced by social circumstances (employment requires platform presence), or identity-constitutive (user identity is inseparable from platform presence). Stratify by age, occupation, digital literacy.',
    'If the lock is primarily economic/social rather than identity-constitutive, the exit_options should be ''constrained'' rather than ''identity_locked,'' which would shift the beneficiary calculation: constrained targets can theoretically relocate; identity-locked targets cannot. Would affect the computed directionality and the engine''s per-seat type classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_vs_constrained_distinction, empirical, 'Whether data-subject exit is locked by identity fusion or by economic/social constraint.').

omega_variable(
    kernel_reading_coexistence,
    'Do the three readings of the Article 17 kernel (privacy_fundamental, competitive_moat, censorship_mechanism) genuinely coexist as live positions held by different parties, or does one reading logically foreclose the others?',
    'Examine whether a single institutional actor (e.g., a data protection authority, a court, a platform) can coherently hold multiple readings simultaneously. Test whether accepting the premises of one reading logically entails rejecting the core claims of another.',
    'If coexistence is confirmed, the readings are coexisting_with edges (different parties, no logical foreclosure). If one reading''s premises logically entail rejection of another''s core claim, the relationship is forecloses. Affects the network topology and the committer-frame classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_coexistence, conceptual, 'Whether the three Article 17 readings logically coexist or foreclose each other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__privacy_fundamental_reading, 2018, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2018, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 2018, 0.08).
narrative_ontology:measurement_basis(arti_tr_t2018, observed).
narrative_ontology:measurement(arti_tr_t2020, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 2020, 0.12).
narrative_ontology:measurement_basis(arti_tr_t2020, observed).
narrative_ontology:measurement(arti_tr_t2022, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 2022, 0.18).
narrative_ontology:measurement_basis(arti_tr_t2022, observed).
narrative_ontology:measurement(arti_tr_t2024, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 2024, 0.21).
narrative_ontology:measurement_basis(arti_tr_t2024, observed).
narrative_ontology:measurement(arti_tr_t2026, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 2026, 0.22).
narrative_ontology:measurement_basis(arti_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t2018, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 2018, 0.18).
narrative_ontology:measurement_basis(arti_be_t2018, observed).
narrative_ontology:measurement(arti_be_t2020, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 2020, 0.22).
narrative_ontology:measurement_basis(arti_be_t2020, observed).
narrative_ontology:measurement(arti_be_t2022, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 2022, 0.26).
narrative_ontology:measurement_basis(arti_be_t2022, observed).
narrative_ontology:measurement(arti_be_t2024, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 2024, 0.27).
narrative_ontology:measurement_basis(arti_be_t2024, observed).
narrative_ontology:measurement(arti_be_t2026, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 2026, 0.28).
narrative_ontology:measurement_basis(arti_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2018, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 2018, 0.08).
narrative_ontology:measurement_basis(arti_su_t2018, observed).
narrative_ontology:measurement(arti_su_t2020, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 2020, 0.11).
narrative_ontology:measurement_basis(arti_su_t2020, observed).
narrative_ontology:measurement(arti_su_t2022, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 2022, 0.13).
narrative_ontology:measurement_basis(arti_su_t2022, observed).
narrative_ontology:measurement(arti_su_t2024, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 2024, 0.14).
narrative_ontology:measurement_basis(arti_su_t2024, observed).
narrative_ontology:measurement(arti_su_t2026, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 2026, 0.15).
narrative_ontology:measurement_basis(arti_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__privacy_fundamental_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article17_erasure_right__privacy_fundamental_reading, 0.12).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right__competitive_moat_reading).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right__censorship_mechanism_reading).

% DUAL FORMULATION NOTE:
% Article 17 GDPR kernel exhibits stable decomposition into three structurally distinct constraints: privacy_fundamental (this story, individual as beneficiary), competitive_moat (compliance cost asymmetry favoring incumbents), and censorship_mechanism (strategic erasure requests weaponizing privacy). Each reading instantiates a different epsilon value, different beneficiary/victim structure, and different measured type. The readings coexist because different parties hold them simultaneously; the network edges document the structural coupling (each reading creates conditions that affect the others). All three stories must link via network.affects_constraints to maintain the committer-frame decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article17_erasure_right__privacy_fundamental_reading, powerless, 0.08).
constraint_indexing:directionality_override(article17_erasure_right__privacy_fundamental_reading, institutional, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
