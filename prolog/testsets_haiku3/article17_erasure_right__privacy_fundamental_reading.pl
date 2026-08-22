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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article17_erasure_right__privacy_fundamental_reading
 *   human_readable: Article 17 Right to Erasure: Privacy Fundamental Reading
 *   domain: technology/data-protection/constitutional-right
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the Article 17 kernel (right
 *   to erasure / right to be forgotten). The privacy_fundamental_reading
 *   frames Article 17 as establishing individual data sovereignty — the legal
 *   embodiment of informational self-determination as a fundamental human
 *   right. Under this reading, the primary function is restoring control over
 *   personal data to individuals, constraining platforms' ability to retain
 *   data indefinitely for behavioral leverage. The reading affirms that
 *   individuals are beneficiaries (gaining control) and platforms are
 *   constrained parties (losing indefinite retention rights). This reading
 *   coexists with two sibling readings: the competitive_moat_reading (Article
 *   17 as incumbent protection via compliance cost asymmetry) and the
 *   censorship_mechanism_reading (Article 17 as enabling content removal via
 *   strategic erasure requests). These three readings are instantiated in
 *   separate constraint stories; each has its own ε, its own stakeholder
 *   structure, and its own type classification. Do not confuse this reading
 *   with the others or average over them — generate ONE reading clean, and
 *   link the siblings via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - data_subjects: powerless individual agents holding the legal right but constrained by epistemic friction and platform resistance; benefit from the constraint's existence and its enforcement
 *   - digital_platforms: institutional agents required to honor erasure requests; bear compliance costs and lose retention-based behavioral leverage; agents_setter role delegated to regulators but payers role primary
 *   - data_protection_authorities: institutional agenda-setters interpreting Article 17's scope and enforcing compliance; power to define what counts as erasure and when retention is justified
 *   - civil_rights_advocates: organized beneficiaries advocating for broad interpretation and strong enforcement; amplify data subjects' voice and bring cases
 *   - competing_platforms: institutional agents that benefit from the constraint's existence (raises relative compliance cost for incumbents) while also bearing compliance costs themselves; dual role as beneficiary and payer
 *   - law_enforcement: institutional agents excluded from Article 17 decision-making but structurally affected (cannot retain data platforms must delete); represent the state interest in data retention for investigation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__privacy_fundamental_reading, 0.18).
domain_priors:suppression_score(article17_erasure_right__privacy_fundamental_reading, 0.12).
domain_priors:theater_ratio(article17_erasure_right__privacy_fundamental_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, resistance, 0.34).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__privacy_fundamental_reading, rope).
narrative_ontology:human_readable(article17_erasure_right__privacy_fundamental_reading, "Article 17 Right to Erasure: Privacy Fundamental Reading").
narrative_ontology:topic_domain(article17_erasure_right__privacy_fundamental_reading, "technology/data-protection/constitutional-right").

domain_priors:requires_active_enforcement(article17_erasure_right__privacy_fundamental_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__privacy_fundamental_reading, 'e441f102-62f4-429e-8c11-5227159a55de').
narrative_ontology:cs_kernel_codification('e441f102-62f4-429e-8c11-5227159a55de', formalized).
narrative_ontology:cs_authority_grounding('e441f102-62f4-429e-8c11-5227159a55de', lineage).
narrative_ontology:cs_interpretation_layer_present('e441f102-62f4-429e-8c11-5227159a55de').
narrative_ontology:cs_reading_relation('e441f102-62f4-429e-8c11-5227159a55de', article17_erasure_right__competitive_moat_reading, influences).
narrative_ontology:cs_reading_relation('e441f102-62f4-429e-8c11-5227159a55de', article17_erasure_right__censorship_mechanism_reading, coexists_with).
narrative_ontology:cs_axiom('e441f102-62f4-429e-8c11-5227159a55de', foundational, informational_self_determination_fundamental_right).
narrative_ontology:cs_axiom_status(informational_self_determination_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('e441f102-62f4-429e-8c11-5227159a55de', informational_self_determination_fundamental_right, deontological).
narrative_ontology:cs_axiom('e441f102-62f4-429e-8c11-5227159a55de', foundational, data_dignity_principle).
narrative_ontology:cs_axiom_status(data_dignity_principle, holdable).
narrative_ontology:cs_axiom_grounding('e441f102-62f4-429e-8c11-5227159a55de', data_dignity_principle, deontological).
narrative_ontology:cs_reference_frame('e441f102-62f4-429e-8c11-5227159a55de', individual_data_sovereignty_framework).
narrative_ontology:cs_drift_state('e441f102-62f4-429e-8c11-5227159a55de', contemporary_enforcement_era_2025, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e441f102-62f4-429e-8c11-5227159a55de', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, data_subjects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, civil_rights_advocates).
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, competing_platforms).
narrative_ontology:constraint_victim(article17_erasure_right__privacy_fundamental_reading, digital_platforms).
narrative_ontology:constraint_victim(article17_erasure_right__privacy_fundamental_reading, competing_platforms).
narrative_ontology:constraint_vindicates(article17_erasure_right__privacy_fundamental_reading, data_privacy_as_fundamental_human_right).
narrative_ontology:constraint_vindicates(article17_erasure_right__privacy_fundamental_reading, informational_self_determination).
narrative_ontology:constraint_vindicates(article17_erasure_right__privacy_fundamental_reading, personal_data_dignity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals whose personal data digital platforms have collected. Under Article 17, they hold the right to request erasure of their data when it is no longer necessary for the original purpose, when they withdraw consent, or when retention violates GDPR conditions. They exercise this right by submitting erasure requests; compliance is mandatory regardless of platform preferences. Their power is constrained by the necessity of making individual requests and the epistemic burden of knowing what data exists where, but they gain control over their informational footprint and freedom from indefinite retention.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, data_subjects, beneficiary,
    powerless, biographical, constrained, global).

% Corporations operating digital services that collect and retain personal data (social media, e-commerce, analytics networks, cloud services). Article 17 imposes mandatory compliance: they must delete personal data within specified timeframes, verify deletion across systems, honor requests even when deletion conflicts with their business models, and implement technical systems to detect and process erasure requests. They cannot refuse erasure except in narrow legally specified circumstances. Their constraint is that retention, which generates behavioral prediction value and user lock-in, becomes conditional rather than perpetual.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, digital_platforms, payer,
    institutional, generational, constrained, global).

% National and supranational regulators (national DPAs, EDPB, national courts) that interpret and enforce Article 17. They set the standard for what counts as erasure, what constitutes 'no longer necessary,' when legal bases for retention survive erasure requests, and what penalties apply to non-compliance. They adjudicate contested requests and can order platforms to improve their erasure request handling.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, data_protection_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Organizations and campaigns advocating for data privacy as a fundamental right. They bring cases, publish guidance on how to exercise Article 17, challenge platform non-compliance, and argue for broad interpretation of erasure rights. They benefit from the constraint's existence and seek to expand its scope and enforcement.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, civil_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% Smaller or alternative platforms that have lower data retention dependencies or technical compliance costs proportionally. Article 17's existence raises compliance costs for incumbents more than for platforms built with privacy as a design principle from the start. They benefit from the constraint's existence and the competitive rebalancing it creates, but also bear compliance costs themselves.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, competing_platforms, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__privacy_fundamental_reading, competing_platforms, payer).

% Police, prosecutors, and intelligence agencies that occasionally need preserved personal data for criminal investigation or terrorism prevention. Article 17 creates tension with data retention for law enforcement purposes, though GDPR carves out exceptions. They are excluded from erasure decisions but operate in legal conflict with broad erasure interpretation.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, law_enforcement, excluded,
    institutional, biographical, constrained, national).

% Security researchers and academic institutions that depend on access to anonymized datasets to study platform security, user behavior, algorithmic fairness, and systemic risks. Broad erasure compliance can make long-term longitudinal datasets unavailable, limiting research on platform dynamics. They are excluded from Article 17 decision-making but are structurally affected by its scope.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, cybersecurity_researchers, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article17_erasure_right__privacy_fundamental_reading, diffuse).
narrative_ontology:fixing_cost_class(article17_erasure_right__privacy_fundamental_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a standardized, legally enforceable mechanism for individuals to reclaim control of their informational footprint: instead of relying on platform policies that vary by company or informal deletion requests, Article 17 creates a uniform right backed by regulatory authority. Solves the individual's problem of indefinite data retention and the collective problem of asymmetric information control.
% TRANSFER_FUNCTION: Transfers informational control and privacy dignity from platforms (which retain data indefinitely for behavioral leverage) back to data subjects (who can demand deletion). The transfer is not monetary but is a shift in property-like rights over personal information: from platform ownership of data, to individual right to determine its retention.
% ABSENT_VOICES: Law enforcement, cybersecurity research communities, and data-analytics-dependent SMEs would argue that broad erasure obligations destroy data infrastructure useful for legitimate public purposes. They are structurally excluded from Article 17's consent and adjudication processes; they learn of erasure effects only through data unavailability. Academic researchers advocating open data for fairness and bias detection are also excluded from the initial framework, though some DPA guidance now attempts to accommodate them via anonymization exceptions.
% DISAPPEARANCE_RATIONALE: If Article 17 disappeared, platforms would revert to indefinite data retention by default (as they did pre-GDPR). Individuals would lose the legal right to demand deletion. Platforms would rebuild persistent behavioral profiles without user request rights, expanding profiling scope. The business model of surveillance-dependent platforms would normalize to pre-2018 practices, and informational asymmetry would deepen. Data ethics norms and privacy-by-design practices that emerged partly in response to Article 17 enforcement would gradually erode without the legal requirement.
% FOUNDING_PROBLEM: Before Article 17, platforms retained personal data indefinitely, even after users deleted accounts or ceased use. Users had no standardized right to demand deletion. Data accumulated in platform systems generated behavioral prediction value that locked users in (switching platforms meant losing the profile investment platform had made in understanding them). The founding problem: individuals have no control over their informational footprint once platforms have collected it, and indefinite retention enables asymmetric power in the digital economy.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights organizations, data protection authorities, and independent researchers document ongoing platform data retention practices and the persistence of profile-lock mechanisms post-Article-17 (platforms retain data for secondary purposes even after primary purpose ends). EU DPA enforcement actions from 2021–2025 confirm platforms continue aggressive retention strategies and resist erasure requests. The founding problem is attested as live by every DPA investigation and civil society report; no credible party claims indefinite data retention is no longer a concern.
narrative_ontology:disappearance_verdict(article17_erasure_right__privacy_fundamental_reading, world_rearranges).
narrative_ontology:founding_problem_status(article17_erasure_right__privacy_fundamental_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__privacy_fundamental_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article17_erasure_right__privacy_fundamental_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article17_erasure_right__privacy_fundamental_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is LOW (0.18 at interval start, declining to 0.11 projected) because the constraint is legitimated by a fundamental right (privacy dignity) rather than by hidden asymmetric benefit to any party. The rights are real and enforceable; platforms cannot refuse erasure except in narrow legal carve-outs. Suppression is also LOW (0.12, declining) because the constraint operates via transparent legal process (DPA enforcement, court orders, published guidance) rather than coercion or epistemic suppression. Theater ratio is very LOW (0.08, declining to 0.04) because the functional activity (erasure requests, compliance processes) is straightforward and directly related to the constraint's stated purpose (regaining data control), not performative maintenance. Accessibility_collapse is HIGH (0.92) because once Article 17's existence is understood, the alternative of indefinite platform retention becomes legally unavailable — the constraint is irreversible absent legislative repeal. Resistance is MODERATE (0.34) because platforms actively defend against broad interpretation through legal challenge and lobbying, but most accept the constraint's legitimacy and invest in compliance infrastructure rather than systematic defiance. The measurement series shows steady decline in both extractiveness and suppression because as platforms mature compliance infrastructure (2018–2025) and jurisprudence clarifies the right, the effective extraction platforms can sustain and the suppression needed to resist erasure requests both decrease. Theater remains stable and low throughout, suggesting the functional activity is primarily genuine (processing requests) rather than performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (platforms) and the beneficiary seat (data subjects) experience very different constraint structures from the same legal text. From the platform seat: Article 17 is an operational burden that raises compliance costs, requires investment in deletion infrastructure, and eliminates a valuable data asset (persistent behavioral profiles). From the data subject seat: Article 17 is a restoration of control and a rebalancing of informational asymmetry. The engine computes this divergence from the structural data: platforms are institutional payers with mobile exit options (relocate, narrow services, lobby for exemptions); data subjects are powerless beneficiaries with constrained exit (cannot avoid digital platforms entirely). The directionality differs — high d (target) for platforms, low d (beneficiary) for data subjects — even though they relate to the identical legal right. This is the engine's job: compute the per-seat experience, not the clause's stated intent.
 *
 * DIRECTIONALITY LOGIC:
 *   Data subjects are structural beneficiaries (d near 0.0): the constraint directly serves their declared interests (informational control), they face no mandatory costs, and their exit option from the constraint (not using platforms) is not the constraint's doing. Platforms are structural payers (d near 1.0): the constraint imposes mandatory compliance, eliminates a revenue-generating data asset, and platforms cannot exit the jurisdiction without losing market access (EU is too large). DPA/regulators are agenda-setters (d near 0.5, symmetric): they both benefit (capacity to regulate) and bear cost (adjudication resource). Excluded parties (law enforcement, researchers) are neither beneficiaries nor direct payers under this reading — they are collateral-damage stakeholders whose interests conflict with the sovereignty reading but who are not parties to the constraint. The directionality derivation chain works straightforwardly here: no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading should NOT be classified as having mandatrophy (founding problem dead, arrangement persists). The founding problem (indefinite data retention without user control) remains live, and the arrangement (Article 17 right + DPA enforcement) continues to serve that problem. If the founding problem were dead (platforms had voluntarily shifted to finite retention by 2018, before Article 17 took effect), the persistence of Article 17 enforcement would suggest mandatrophy. But the empirical record shows platforms had NOT voluntarily limited retention; Article 17 was necessary to impose the constraint. As long as platforms continue to resist erasure (documented in DPA enforcement actions through 2025), the founding problem is live and mandatrophy is not present. The classification remains rope: genuine coordination solving a real problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_censorship_moat,
    'Is Article 17 an instantiation of individual data sovereignty (this reading''s core premise), or does its broad interpretation enable censorship by strategic erasure (censorship_mechanism_reading) and protect incumbent platforms via compliance cost asymmetry (competitive_moat_reading)?',
    'Empirical: track patterns of erasure request usage (do data subjects use it to reclaim control, or do third parties weaponize it for removal of content they dislike?). Monitor DPA guidance and court rulings on scope; document whether compliance costs fall asymmetrically on entrants vs. incumbents. Jurisprudential: trace how courts and DPAs interpret ''right to be forgotten'' vs. ''public interest in information'' exceptions; shifting jurisprudence toward narrow interpretation would favor moat/censorship readings over sovereignty reading.',
    'If empirical evidence supports censorship or moat mechanisms as primary function, reclassify from rope (coordination around individual rights) to snare (extraction mechanism) or tangled_rope (coordination that also enables extraction). If sovereignty remains dominant mechanism, classification stays rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_censorship_moat, empirical, 'Whether Article 17''s primary function is individual data sovereignty or secondary functions (censorship, market protection) dominate use patterns.').

omega_variable(
    epistemic_friction_in_erasure_requests,
    'What is the actual epistemic friction for data subjects in exercising Article 17: how much must they know or prove about their data to successfully request erasure, and does the legal framework minimize or amplify that friction?',
    'Field research: survey data subjects on their understanding of Article 17, their ability to identify what data exists where, and their success rate with erasure requests. Track DPA guidance on burden of proof — does Article 17 require data subjects to identify their data before requesting erasure, or does it place the burden on platforms to find and delete? Low epistemic friction = easy, broad requests; high friction = difficult, proof-intensive.',
    'High epistemic friction undermines the sovereignty reading: if individuals must know exactly what data they have and where, the right becomes theoretical and benefits only sophisticated actors. Low friction supports the sovereignty reading: individuals can request broad erasure without technical expertise. Friction level affects the classification boundary between rope (low friction, genuine coordination) and tangled_rope (high friction, apparent right that benefits only knowledgeable payers).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epistemic_friction_in_erasure_requests, empirical, 'Degree to which the legal and technical framework creates barriers to exercising the erasure right.').

omega_variable(
    data_subject_agency_vs_platform_capture,
    'To what extent do data subjects actively exercise Article 17 rights autonomously, vs. relying on privacy advocates or third-party services to request erasure on their behalf? Does active autonomous exercise support the sovereignty reading, or does reliance on intermediaries indicate the right is effective only for organized actors, undermining the sovereignty claim?',
    'Empirical: survey and interview data subjects on their Article 17 exercise patterns. Observe rates of direct vs. intermediary-brokered requests. Track whether DPA guidance encourages direct exercise or acknowledges intermediary necessity. If autonomous exercise dominates, sovereignty reading is validated; if intermediary dependence dominates, the right may function as a vehicle for organized advocacy rather than individual control.',
    'High autonomous exercise rates support rope classification (genuine coordination enabling individual control). Intermediary dependence would shift the reading toward tangled_rope (apparent individual right that actually transfers control to organized advocates or platforms, depending on who uses intermediary pathways).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_subject_agency_vs_platform_capture, empirical, 'Whether Article 17 exercise is autonomous individual action or mediated through organizational intermediaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__privacy_fundamental_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(arti_tr_t0, observed).
narrative_ontology:measurement(arti_tr_t5, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 5, 0.07).
narrative_ontology:measurement_basis(arti_tr_t5, observed).
narrative_ontology:measurement(arti_tr_t10, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 10, 0.06).
narrative_ontology:measurement_basis(arti_tr_t10, observed).
narrative_ontology:measurement(arti_tr_t15, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement_basis(arti_tr_t15, projected).
narrative_ontology:measurement(arti_tr_t20, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement_basis(arti_tr_t20, projected).
narrative_ontology:measurement(arti_tr_t25, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 25, 0.04).
narrative_ontology:measurement_basis(arti_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(arti_be_t0, observed).
narrative_ontology:measurement(arti_be_t5, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 5, 0.16).
narrative_ontology:measurement_basis(arti_be_t5, observed).
narrative_ontology:measurement(arti_be_t10, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement_basis(arti_be_t10, observed).
narrative_ontology:measurement(arti_be_t15, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 15, 0.13).
narrative_ontology:measurement_basis(arti_be_t15, projected).
narrative_ontology:measurement(arti_be_t20, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement_basis(arti_be_t20, projected).
narrative_ontology:measurement(arti_be_t25, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 25, 0.11).
narrative_ontology:measurement_basis(arti_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(arti_su_t0, observed).
narrative_ontology:measurement(arti_su_t5, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 5, 0.11).
narrative_ontology:measurement_basis(arti_su_t5, observed).
narrative_ontology:measurement(arti_su_t10, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement_basis(arti_su_t10, observed).
narrative_ontology:measurement(arti_su_t15, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 15, 0.09).
narrative_ontology:measurement_basis(arti_su_t15, projected).
narrative_ontology:measurement(arti_su_t20, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 20, 0.08).
narrative_ontology:measurement_basis(arti_su_t20, projected).
narrative_ontology:measurement(arti_su_t25, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 25, 0.07).
narrative_ontology:measurement_basis(arti_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__privacy_fundamental_reading, information_standard).
narrative_ontology:boltzmann_floor_override(article17_erasure_right__privacy_fundamental_reading, 0.05).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right__competitive_moat_reading).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right__censorship_mechanism_reading).

% DUAL FORMULATION NOTE:
% Article 17 (right to erasure/right to be forgotten) is a contested kernel instantiated in three separate constraint stories: privacy_fundamental_reading (this file) frames Article 17 as establishing individual data sovereignty and informational self-determination; competitive_moat_reading frames it as incumbent platform protection via compliance cost asymmetry; censorship_mechanism_reading frames it as enabling content suppression via strategic erasure. Each reading is a clean, ε-invariant constraint with its own beneficiary/victim structure, stakeholder positions, and type classification. The three stories are linked via network.affects_constraints to document their shared kernel and structural interdependence. Empirical divergence in how different DPA jurisdictions interpret Article 17, combined with academic and policy debate over which reading is primary, confirms the kernel contest is genuinely live, not merely a matter of enforcement inconsistency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
