% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__censorship_mechanism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article17_erasure_right__censorship_mechanism_reading, []).

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
 *   constraint_id: article17_erasure_right__censorship_mechanism_reading
 *   human_readable: GDPR Article 17 'Right to Erasure' as Strategic Content Suppression Mechanism
 *   domain: technology_governance/data_protection_law/speech_regulation
 *
 * SUMMARY:
 *   This story isolates one structurally distinct reading of GDPR Article
 *   17's 'right to erasure': its operational function as a content
 *   suppression mechanism, distinct from its privacy-rights function
 *   (privacy_fundamental_reading) and its incumbent-protection function via
 *   compliance-cost asymmetry (competitive_moat_reading). Under this reading,
 *   the erasure right's low evidentiary bar and platform-favoring compliance
 *   incentives make it exploitable as a de facto prior-restraint substitute:
 *   requesters with genuinely embarrassing but factual public records use the
 *   mechanism to delist reporting about themselves, and platforms — facing
 *   regulatory penalty for wrongful retention but no penalty for wrongful
 *   removal — systematically over-comply. The coordination function (removing
 *   genuinely stale, harmful personal data) is real and is what makes this a
 *   tangled_rope rather than a pure snare: some erasure requests serve the
 *   law's intended purpose. But the same mechanism, absent adversarial
 *   process or requester accountability, extracts discoverability from
 *   journalists, archivists, and the public and delivers it to whoever files
 *   the request, regardless of the public-interest merits.
 *
 * KEY AGENTS:
 *   - bad_faith_erasure_requesters: primary beneficiary (moderate/mobile) - files exploitable requests at no cost
 *   - reputation_management_firms: organized beneficiary and de facto agenda-shaper (organized/arbitrage) - operationalizes the mechanism commercially
 *   - search_engines_and_platforms: agenda_setter (institutional/constrained) - administers the mechanism under compliance-cost pressure toward over-granting
 *   - investigative_journalists: primary target (moderate/constrained) - loses discoverability of published public-interest reporting
 *   - digital_archivists: diffuse target (powerless/trapped) - cumulative silent degradation of historical record
 *   - data_protection_authorities: analytical observer (institutional/analytical) - adjudicates appeals but lacks aggregate visibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__censorship_mechanism_reading, 0.71).
domain_priors:suppression_score(article17_erasure_right__censorship_mechanism_reading, 0.78).
domain_priors:theater_ratio(article17_erasure_right__censorship_mechanism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__censorship_mechanism_reading, tangled_rope).
narrative_ontology:human_readable(article17_erasure_right__censorship_mechanism_reading, "GDPR Article 17 'Right to Erasure' as Strategic Content Suppression Mechanism").
narrative_ontology:topic_domain(article17_erasure_right__censorship_mechanism_reading, "technology_governance/data_protection_law/speech_regulation").

domain_priors:requires_active_enforcement(article17_erasure_right__censorship_mechanism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__censorship_mechanism_reading, '76316c8a-ee21-404f-a0e6-ce9dd0936174').
narrative_ontology:cs_kernel_codification('76316c8a-ee21-404f-a0e6-ce9dd0936174', formalized).
narrative_ontology:cs_authority_grounding('76316c8a-ee21-404f-a0e6-ce9dd0936174', extraction).
narrative_ontology:cs_interpretation_layer_present('76316c8a-ee21-404f-a0e6-ce9dd0936174').
narrative_ontology:cs_reading_relation('76316c8a-ee21-404f-a0e6-ce9dd0936174', article17_erasure_right__privacy_fundamental_reading, influences).
narrative_ontology:cs_reading_relation('76316c8a-ee21-404f-a0e6-ce9dd0936174', article17_erasure_right__competitive_moat_reading, coexists_with).
narrative_ontology:cs_axiom('76316c8a-ee21-404f-a0e6-ce9dd0936174', foundational, erasure_absent_adversarial_process_defaults_to_suppression).
narrative_ontology:cs_axiom_status(erasure_absent_adversarial_process_defaults_to_suppression, holdable).
narrative_ontology:cs_axiom_grounding('76316c8a-ee21-404f-a0e6-ce9dd0936174', erasure_absent_adversarial_process_defaults_to_suppression, empirically_contingent).
narrative_ontology:cs_axiom('76316c8a-ee21-404f-a0e6-ce9dd0936174', secondary, public_interest_defense_requires_standing_not_just_criteria).
narrative_ontology:cs_axiom_status(public_interest_defense_requires_standing_not_just_criteria, holdable).
narrative_ontology:cs_axiom_grounding('76316c8a-ee21-404f-a0e6-ce9dd0936174', public_interest_defense_requires_standing_not_just_criteria, instrumental).
narrative_ontology:cs_reference_frame('76316c8a-ee21-404f-a0e6-ce9dd0936174', individual_data_sovereignty_default).
narrative_ontology:cs_drift_state('76316c8a-ee21-404f-a0e6-ce9dd0936174', post_reputation_management_industrialization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('76316c8a-ee21-404f-a0e6-ce9dd0936174', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__censorship_mechanism_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, bad_faith_erasure_requesters).
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, reputation_management_firms).
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, individuals_evading_public_record_scrutiny).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, investigative_journalists).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, digital_archivists).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, public_interest_researchers).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, the_public_seeking_factual_records).
narrative_ontology:constraint_vindicates(article17_erasure_right__censorship_mechanism_reading, individual_control_over_personal_data_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals with an unflattering but factual public record — fraud convictions, professional misconduct findings, documented harassment — file erasure requests against news archives, court-record aggregators, and search indexes, framing legitimate factual reporting as 'outdated' or 'no longer relevant' personal data. They face no penalty for over-broad or bad-faith requests, and platforms' incentive to avoid liability means requests are frequently honored without adversarial testing of the public-interest defense.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, bad_faith_erasure_requesters, beneficiary,
    moderate, biographical, mobile, national).

% Commercial firms operationalize Article 17 as a service product: they file bulk erasure requests on behalf of paying clients, exploit search engines' compliance-cost incentives to over-comply rather than litigate each contested case, and have developed template language that maximizes take-down rates by framing public records as stale personal data. They profit directly from the gap between the law's stated purpose and its operational effect.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, reputation_management_firms, beneficiary,
    organized, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__censorship_mechanism_reading, reputation_management_firms, agenda_setter).

% Legally obligated to adjudicate erasure requests against a vague balancing test (privacy vs. public interest) under threat of substantial fines. Facing millions of requests, they default to a compliance posture that favors granting erasure over contesting it, since erasure carries no downside for the platform while refusal risks regulatory penalty and litigation cost. They administer the mechanism and could in principle push back harder on bad-faith requests, but bear none of the diffuse cost of suppressed information themselves.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, search_engines_and_platforms, agenda_setter,
    institutional, generational, constrained, continental).

% Publish factual reporting on matters of public record — court proceedings, financial misconduct, professional licensing actions — that later becomes the target of erasure requests from the subjects of that reporting. Delisting from search indexes effectively removes their published work from public discoverability even when the underlying article remains technically online, gutting its practical reach. They have no standing to contest erasure decisions made about their own published work by a third-party platform.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, investigative_journalists, payer,
    moderate, biographical, constrained, continental).

% Maintain historical records, court archives, and public registries that are targeted piecemeal by erasure requests. Each successful request degrades the completeness of the historical record; the cumulative effect over years is an archive with silent, undocumented gaps that future researchers cannot detect. They lack the resources to contest requests individually and are rarely notified when material connected to their holdings is delisted elsewhere.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, digital_archivists, payer,
    powerless, generational, trapped, continental).

% Rely on searchable public records to study patterns of corporate misconduct, professional malpractice, or political accountability. When individual records are erased at scale, the aggregate picture available for accountability research degrades without any single erasure being independently visible as harmful. They have no mechanism to learn what has been removed or petition for its reinstatement.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, public_interest_researchers, payer,
    powerless, generational, trapped, continental).

% Adjudicate appeals and set enforcement guidance on the privacy/public-interest balancing test. They receive complaints from both erasure requesters and from journalists/publishers whose content is delisted, and can in principle correct platform over-compliance, but their case-by-case capacity is dwarfed by request volume and they lack visibility into the aggregate suppression effect.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, data_protection_authorities, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article17_erasure_right__censorship_mechanism_reading, bad_faith_erasure_requesters).
narrative_ontology:fixing_cost_class(article17_erasure_right__censorship_mechanism_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides individuals a mechanism to request removal of genuinely outdated, irrelevant, or harmful personal data from search indexing and public platforms — solving a real problem where old, no-longer-accurate personal information persists indefinitely and causes ongoing harm disproportionate to any public interest in retaining it.
% TRANSFER_FUNCTION: Moves reputational cost from the subject of factual public-interest reporting to the journalists, archivists, and researchers who produced or depend on that reporting, and ultimately to the public that loses access to the historical record — all routed through a platform-administered erasure process that carries no adversarial testing burden for the requester.
% ABSENT_VOICES: Journalists whose specific articles are delisted are not parties to the erasure adjudication and are frequently not even notified; digital archivists maintaining the underlying public records have no seat in individual erasure decisions; the diffuse public that would benefit from continued discoverability of the record has no representative in any single request's adjudication at all.
% DISAPPEARANCE_RATIONALE: If Article 17 were repealed entirely, genuine privacy-harm cases (revenge content, juvenile records, resolved minor disputes) would lose a real remedy — the world would rearrange for those legitimate beneficiaries. But the censorship-mechanism function specifically — bulk suppression of public-interest factual reporting via compliance-cost-driven over-removal — would collapse immediately if platforms were required to adjudicate adversarially with journalist standing; the suppression function is separable from the underlying privacy right and is what this reading isolates.
% FOUNDING_PROBLEM: Individuals had no legal mechanism to compel removal of outdated, harmful, or no-longer-relevant personal information that continued to surface prominently in search results years after it ceased to be accurate or relevant, causing ongoing reputational and practical harm disproportionate to any remaining public interest.
% FOUNDING_PROBLEM_CORROBORATION: European data protection regulators and the original Google Spain court reasoning attest the founding problem (stale personal data with disproportionate ongoing harm) remains live for genuine cases. Independent press-freedom organizations (Reporters Without Borders, the Committee to Protect Journalists), academic studies of delisting transparency reports, and investigative journalists whose published work has been delisted attest — from outside the beneficiary set of erasure requesters and reputation management firms — that the mechanism's operational effect has substantially shifted toward suppressing public-interest factual reporting, a function the original privacy-harm problem does not justify.
narrative_ontology:disappearance_verdict(article17_erasure_right__censorship_mechanism_reading, contested).
narrative_ontology:founding_problem_status(article17_erasure_right__censorship_mechanism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__censorship_mechanism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article17_erasure_right__censorship_mechanism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article17_erasure_right__censorship_mechanism_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article17_erasure_right__censorship_mechanism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article17_erasure_right__censorship_mechanism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article17_erasure_right__censorship_mechanism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.71 and rising over the interval to reflect an accumulating pattern: as reputation-management firms professionalize the request process and platforms' compliance posture hardens toward default-grant, the share of erasures serving genuine privacy harm versus serving suppression of public-interest reporting shifts toward the latter. Suppression is authored higher still (0.78) because the mechanism's persistence depends on structural features — no adversarial standing for journalists, no penalty for over-broad requests, platform liability asymmetry — that actively work against contestation, not merely on participant preference. Theater ratio rises moderately (0.42) reflecting platforms' increasing use of 'balancing test' language and transparency reports that document volume without documenting suppression's actual public-interest cost.
 *
 * PERSPECTIVAL GAP:
 *   From the requester's seat, Article 17 is Rope: a legitimate personal-data-control mechanism they are using exactly as designed. From the journalist's or archivist's seat, the same mechanism computes as extractive and coercive: material they produced or curate disappears from discoverability through a process where they have no voice, triggered by claims they cannot see or rebut. The engine should compute these divergently from the same structural data — the point of this reading is that isolating the censorship-function slice of Article 17's operation surfaces exactly this asymmetry, which is invisible if the constraint is evaluated only against its stated privacy-rights purpose (that evaluation belongs to the sibling reading, not this one).
 *
 * DIRECTIONALITY LOGIC:
 *   Bad-faith requesters and reputation management firms sit near the full-beneficiary end: they receive suppression of unwanted factual material at near-zero cost and bear no penalty for over-broad claims. Journalists, archivists, and researchers sit near the full-target end: they bear the cost (lost reach, degraded historical record) through a mechanism they have no standing to contest and often cannot even detect. Search engines and platforms are agenda-setters who administer the mechanism and could push back harder, but their compliance-cost incentive structurally aligns them with over-granting rather than adversarial testing — they profit from neither outcome directly but bear asymmetric downside risk only from under-compliance, which drives their behavior toward the beneficiary side of the ledger without being beneficiaries themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuinely stale, harmful personal data persisting disproportionately in search results — remains partly live, which is why this reading is authored as tangled_rope rather than pure snare: a coordination function survives. But the mechanism's growth in extractiveness and suppression over the interval, absent any corresponding growth in adversarial safeguards, indicates the mandate has drifted from its founding justification toward a use its authors did not anticha te and its beneficiaries did not need to justify. Distinguishing this from simple mandatrophy: the underlying right has not become purely vestigial (unlike a piton) — it retains active, if partial, legitimate use — but its unaccountable operational surface has been colonized by a use case (suppression of unfavorable but true public-interest content) that the coordination framing was never designed to authorize and does not currently constrain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    censorship_vs_privacy_ratio_unmeasured,
    'What proportion of actual Article 17 erasure requests serve genuine privacy-harm remediation versus suppression of public-interest factual reporting? No platform publishes this breakdown.',
    'Mandated disaggregated transparency reporting distinguishing requests targeting news archives, court records, or professional-conduct findings from requests targeting genuinely private, non-newsworthy personal data; independent audit of granted versus contested requests by content category.',
    'If the censorship-function share is small and stable, this reading describes an edge case rather than the mechanism''s dominant operational mode, and the classification severity should be discounted. If it is large and growing (as this story''s authored trajectory assumes), the censorship-mechanism reading captures a substantial and worsening share of the kernel''s real-world effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(censorship_vs_privacy_ratio_unmeasured, empirical, 'Unmeasured ratio of legitimate-privacy to suppression-function erasure requests.').

omega_variable(
    kernel_decomposition_boundary,
    'Is the censorship-mechanism function genuinely separable from the privacy-fundamental function within the same legal text, or does any workable adversarial safeguard against suppression necessarily also weaken legitimate privacy protection?',
    'Comparative analysis of jurisdictions or platform policies that have implemented adversarial notice-and-standing procedures for journalists (e.g., requiring publisher notification before delisting news content) to determine whether privacy-harm remediation rates for genuine cases decline when suppression-function safeguards are added.',
    'If separable, adversarial process could be added to the same legal mechanism without weakening the privacy_fundamental_reading sibling''s function — supporting reform rather than repeal. If inseparable, any fix to the censorship-mechanism problem necessarily degrades the privacy-fundamental function, meaning the two readings are in irreducible tension rather than merely coexisting as different lenses on the same text.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_decomposition_boundary, conceptual, 'Whether suppression-function and privacy-function are severable within Article 17''s current design.').

omega_variable(
    requester_intent_unverifiable,
    'Is ''bad faith'' a coherent, verifiable category for erasure requesters, or does the reading impute intent that cannot be structurally distinguished from good-faith privacy claims about the same content?',
    'Case-level review of contested erasure decisions where journalists successfully appealed delisting, examining whether the underlying requests showed identifiable bad-faith markers (e.g., timing correlated with news coverage, professional reputation-management involvement) versus genuinely ambiguous privacy claims.',
    'If bad faith is reliably identifiable, targeted safeguards (heightened scrutiny for requests against news archives, mandatory disclosure of commercial reputation-management involvement) could address this reading''s harm without broader reform. If intent is genuinely unverifiable at scale, the suppression function may be an inherent structural property of any low-friction erasure right rather than a distinguishable bad-faith subset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(requester_intent_unverifiable, conceptual, 'Whether bad-faith requester intent is a structurally identifiable category.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__censorship_mechanism_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(arti_tr_t4, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(arti_tr_t8, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(arti_tr_t12, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(arti_tr_t16, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(arti_tr_t20, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(arti_tr_t24, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(arti_be_t4, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(arti_be_t8, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(arti_be_t12, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(arti_be_t16, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(arti_be_t20, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 20, 0.69).
narrative_ontology:measurement(arti_be_t24, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 24, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(arti_su_t4, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 4, 0.6).
narrative_ontology:measurement(arti_su_t8, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 8, 0.65).
narrative_ontology:measurement(arti_su_t12, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 12, 0.69).
narrative_ontology:measurement(arti_su_t16, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 16, 0.73).
narrative_ontology:measurement(arti_su_t20, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 20, 0.76).
narrative_ontology:measurement(arti_su_t24, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 24, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__censorship_mechanism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article17_erasure_right__censorship_mechanism_reading, 0.1).
narrative_ontology:affects_constraint(article17_erasure_right__censorship_mechanism_reading, privacy_fundamental_reading).
narrative_ontology:affects_constraint(article17_erasure_right__censorship_mechanism_reading, competitive_moat_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the article17_erasure_right kernel. privacy_fundamental_reading treats Article 17 as a genuine data-sovereignty mountain-adjacent right; competitive_moat_reading treats it as incumbent-protecting compliance-cost asymmetry favoring large platforms over smaller competitors; this story (censorship_mechanism_reading) treats it as a tangled_rope enabling strategic suppression of public-interest speech. Each reading has its own ε, beneficiary/victim structure, and classification — they are not three measurements of one constraint but three structurally distinct constraints sharing a kernel text. All three link to each other via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
