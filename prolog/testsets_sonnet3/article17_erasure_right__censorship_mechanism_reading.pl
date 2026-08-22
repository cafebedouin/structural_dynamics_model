% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__censorship_mechanism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: GDPR Article 17 Right to Erasure — Weaponized-Requester Censorship Mechanism Reading
 *   domain: technology_governance/data_protection_law
 *
 * SUMMARY:
 *   This story instantiates the censorship-mechanism reading of the Article
 *   17 (GDPR right to erasure) kernel: the claim that the right's
 *   low-friction, low-evidentiary-burden delisting mechanism has been
 *   substantially captured by bad-faith requesters and specialized
 *   reputation-management firms who use it to suppress accurate, lawfully
 *   published, public-interest reporting. This is NOT the privacy-fundamental
 *   reading (data sovereignty as a limiting right on retention) nor the
 *   competitive-moat reading (compliance-cost asymmetry as incumbent
 *   protection) — those are separate constraints with their own ε values,
 *   beneficiary/victim structures, and classifications, linked here via
 *   network.affects_constraints. Under this reading, the coordination
 *   function (removing genuinely stale, irrelevant personal data) is real but
 *   has become a cover story for an emergent suppression function that
 *   operates as a prior-restraint substitute: content is delisted
 *   pre-emptively, without adversarial process, at a cost asymmetry that
 *   favors requesters and burdens publishers.
 *
 * KEY AGENTS:
 *   - bad_faith_erasure_requesters: Primary beneficiary (moderate/arbitrage) — exploits low-cost filing against high-cost contestation
 *   - reputation_management_firms: Organized beneficiary and agenda-shaper (organized/arbitrage) — systematizes and profits from the exploit
 *   - investigative_journalists: Primary target (moderate/constrained) — bears suppression of accountability work
 *   - digital_archivists: Secondary target (powerless/trapped) — historical record degrades with no recourse
 *   - search_engine_operators: Administering agenda-setter (institutional/constrained) — adjudicates under asymmetric liability pressure
 *   - the_public: Excluded (powerless/trapped) — never notified, never given standing
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
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__censorship_mechanism_reading, tangled_rope).
narrative_ontology:human_readable(article17_erasure_right__censorship_mechanism_reading, "GDPR Article 17 Right to Erasure — Weaponized-Requester Censorship Mechanism Reading").
narrative_ontology:topic_domain(article17_erasure_right__censorship_mechanism_reading, "technology_governance/data_protection_law").

domain_priors:requires_active_enforcement(article17_erasure_right__censorship_mechanism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__censorship_mechanism_reading, 'b743e5a6-b914-421d-91dc-5fee345ff5ec').
narrative_ontology:cs_kernel_codification('b743e5a6-b914-421d-91dc-5fee345ff5ec', formalized).
narrative_ontology:cs_authority_grounding('b743e5a6-b914-421d-91dc-5fee345ff5ec', extraction).
narrative_ontology:cs_interpretation_layer_present('b743e5a6-b914-421d-91dc-5fee345ff5ec').
narrative_ontology:cs_reading_relation('b743e5a6-b914-421d-91dc-5fee345ff5ec', article17_erasure_right__privacy_fundamental_reading, coexists_with).
narrative_ontology:cs_reading_relation('b743e5a6-b914-421d-91dc-5fee345ff5ec', article17_erasure_right__competitive_moat_reading, coexists_with).
narrative_ontology:cs_axiom('b743e5a6-b914-421d-91dc-5fee345ff5ec', foundational, erasure_mechanism_lacks_adversarial_safeguard).
narrative_ontology:cs_axiom_status(erasure_mechanism_lacks_adversarial_safeguard, holdable).
narrative_ontology:cs_axiom_grounding('b743e5a6-b914-421d-91dc-5fee345ff5ec', erasure_mechanism_lacks_adversarial_safeguard, empirically_contingent).
narrative_ontology:cs_axiom('b743e5a6-b914-421d-91dc-5fee345ff5ec', foundational, speech_suppression_effect_is_load_bearing_not_incidental).
narrative_ontology:cs_axiom_status(speech_suppression_effect_is_load_bearing_not_incidental, holdable).
narrative_ontology:cs_axiom_grounding('b743e5a6-b914-421d-91dc-5fee345ff5ec', speech_suppression_effect_is_load_bearing_not_incidental, empirically_contingent).
narrative_ontology:cs_reference_frame('b743e5a6-b914-421d-91dc-5fee345ff5ec', individual_data_sovereignty_baseline).
narrative_ontology:cs_drift_state('b743e5a6-b914-421d-91dc-5fee345ff5ec', post_google_spain_mass_filing_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b743e5a6-b914-421d-91dc-5fee345ff5ec', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__censorship_mechanism_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, bad_faith_erasure_requesters).
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, reputation_management_firms).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, investigative_journalists).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, digital_archivists).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, public_interest_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals or entities with unfavorable but factually accurate and lawfully published material about them (fraud convictions, professional misconduct findings, past public statements) file erasure requests framed as privacy protection. They exploit the asymmetry between the low cost of filing a request and the high cost search engines and publishers face in contesting it, achieving de facto content removal without ever litigating the underlying facts.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, bad_faith_erasure_requesters, beneficiary,
    moderate, biographical, arbitrage, national).

% Commercial firms specializing in mass-filing erasure requests on behalf of clients, systematizing the exploitation of Article 17's low evidentiary bar. They profit directly from the gap between the right's stated purpose and its exploitable mechanics, and lobby to keep the request process opaque and low-friction.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, reputation_management_firms, beneficiary,
    organized, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__censorship_mechanism_reading, reputation_management_firms, agenda_setter).

% Reporters whose published investigations into public figures, corporate misconduct, or matters of public interest become targets of erasure requests. They must either contest the delisting through costly appeals processes or watch their work vanish from search indices, effectively suppressing accountability journalism without any judicial finding against them.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, investigative_journalists, payer,
    moderate, biographical, constrained, national).

% Institutions and volunteers maintaining historical web archives face erasure requests targeting records of public statements, court proceedings, and news coverage. They lack the legal resources of platforms to contest requests individually, and the requests' cumulative effect degrades the historical record with no counterbalancing mechanism.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, digital_archivists, payer,
    powerless, generational, trapped, global).

% Academics and watchdog organizations studying patterns of misconduct, corruption, or public accountability find source material disappearing from accessible indices after erasure requests, silently corrupting the evidentiary base for their work without notice or appeal standing.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, public_interest_researchers, payer,
    moderate, generational, constrained, continental).

% Platforms like the major search engines administer the delisting mechanism, adjudicating requests at scale under legal and regulatory pressure to comply quickly or face penalties. They face asymmetric incentives to grant contestable requests rather than bear litigation risk, effectively privatizing censorship decisions without judicial oversight.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, search_engine_operators, agenda_setter,
    institutional, generational, constrained, continental).

% Regulatory bodies oversee compliance with erasure obligations and adjudicate appeals, but are structurally oriented toward privacy protection as their mandate, with limited institutional capacity or incentive to weigh free expression and public-interest countervailing claims with equal force.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, data_protection_authorities, observer,
    institutional, generational, analytical, continental).

% Ordinary readers and citizens who would benefit from accessible historical and accountability records have no standing in the erasure request process at all — they are never notified when information is delisted and have no mechanism to contest a removal that affects their access to public-interest information.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, the_public, excluded,
    powerless, generational, trapped, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article17_erasure_right__censorship_mechanism_reading, diffuse).
narrative_ontology:fixing_cost_class(article17_erasure_right__censorship_mechanism_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its stated form, coordinates a mechanism by which individuals can request removal of outdated, irrelevant, or unlawfully processed personal data from search indices, solving a genuine problem of permanent digital reputational harm from stale information.
% TRANSFER_FUNCTION: Moves the practical power to suppress publicly available, lawfully published information from publishers and the public record to requesters and the firms that mass-file on their behalf — search engines bear compliance costs, and journalists/archivists bear the loss of reach and discoverability for accurate work.
% ABSENT_VOICES: Journalists whose work is delisted are frequently never notified a request was filed against their article; the public reading the search results has no visibility into what was removed or why. Both groups would object to the low evidentiary threshold and lack of adversarial process, but neither has a formal seat in the removal decision.
% DISAPPEARANCE_RATIONALE: If the erasure mechanism vanished overnight, reputation management firms would lose their core business model, bad-faith requesters would need to pursue costlier defamation or rectification claims with actual burdens of proof, and journalists/archivists would regain full discoverability of previously delisted accurate reporting. Search engines would no longer bear the adjudication burden. The suppression effect is a real, load-bearing function of the current arrangement, not incidental to it.
% FOUNDING_PROBLEM: Individuals had no mechanism to remove outdated, harmful, or unlawfully processed personal information from search results, leaving stale but no-longer-relevant data (old debts, minor youthful offenses, resolved disputes) permanently prominent and disproportionately harmful relative to its current relevance.
% FOUNDING_PROBLEM_CORROBORATION: Privacy advocates and the drafting legislative bodies attest the founding problem remains live and the mechanism functions as intended in the large majority of individual cases. Press freedom organizations (e.g., media rights NGOs), academic researchers studying delisting transparency reports, and journalism associations attest — from outside the requester/reputation-management beneficiary set — that a documented and non-trivial subset of requests target accurate public-interest reporting with no countervailing adversarial process, corroborated by search engines' own periodic transparency disclosures showing delisting of news content.
narrative_ontology:disappearance_verdict(article17_erasure_right__censorship_mechanism_reading, world_rearranges).
narrative_ontology:founding_problem_status(article17_erasure_right__censorship_mechanism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__censorship_mechanism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored high (0.71) and rising because the mechanism's exploitable structure — low burden of proof on the requester side, no adversarial notice to affected publishers, high compliance-penalty risk on platforms — creates a persistent asymmetry that reputation firms have professionalized over time, visible in growing delisting-request volume documented in platform transparency reports. Suppression (0.78) is authored as a raw structural property distinct from and higher than extraction: the mechanism actively forecloses the alternative of judicial adjudication before removal, functioning as prior restraint without the procedural safeguards prior restraint doctrine normally requires. Theater ratio (0.42) reflects that platforms' internal review processes have grown more elaborate over the interval (denial rate disclosures, appeal mechanisms) without proportionally reducing the suppression effect on contested public-interest content — a partial Goodhart drift toward compliance theater. Accessibility collapse (0.58) is moderate rather than near-total: journalists retain some appeal and re-indexing paths, but they are costly and rarely exercised at scale. Resistance (0.6) reflects active, organized pushback from press freedom organizations and academic transparency researchers.
 *
 * PERSPECTIVAL GAP:
 *   From the requester/reputation-firm seat, the mechanism is functioning exactly as designed — a rights-respecting removal of harmful personal data. From the journalist/archivist seat, the identical mechanism operates as unaccountable, extrajudicial content suppression. The engine computes these as different seat-classifications from the same structural data; this reading's authored metrics describe the suppression-mechanism function specifically, not an average across both experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Bad-faith requesters and reputation management firms sit near the full-beneficiary end: they initiate the mechanism, bear near-zero cost relative to the outcome achieved, and can arbitrage across jurisdictions and platforms. Journalists and archivists sit near the full-target end: they did not create the condition being remedied (their reporting is accurate and lawful), bear the cost of contesting or losing visibility, and have constrained or trapped exit — a journalist cannot simply 're-publish elsewhere' without facing the same delisting exposure, and an archive cannot decline to be subject to national jurisdiction's erasure regime. The public is excluded entirely from the directionality calculus in the formal process, despite bearing the diffuse cost of a degraded public record.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (rather than pure snare) preserves the genuine coordination function this reading concedes exists — removing truly stale, no-longer-relevant personal information does solve a real problem for ordinary individuals in the median case. Collapsing this into a pure snare reading would mislabel the entire mechanism as extraction and erase the legitimate privacy-protection use case (which is precisely what the sibling privacy_fundamental_reading captures as its own separate constraint). Conversely, treating it as a pure rope would launder the documented, systematic exploitation by reputation firms and bad-faith requesters as if it were incidental noise rather than a load-bearing extractive function riding on the same legal machinery. The tangled_rope classification under THIS reading asserts both are true simultaneously and requires active enforcement (search engines' compliance obligations) to sustain the asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_scope_of_exploitation,
    'What proportion of actual Article 17 erasure requests fall into the bad-faith/reputation-suppression pattern this reading describes, versus the median case the privacy_fundamental_reading describes (genuine stale-data removal)?',
    'Systematic analysis of search engine transparency reports and delisting request datasets, categorizing requests by requester type, content type (news vs. personal listing), and outcome, cross-referenced with post-delisting journalistic investigation of contested cases.',
    'If the exploitation pattern is a small minority of total requests, this reading''s ε may be overstated relative to aggregate mechanism operation — though ε here is authored specifically for the exploitation-pattern subset as its own referent, not the mechanism as a whole, which is the disambiguation this decomposition exists to make possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_scope_of_exploitation, empirical, 'What share of erasure requests instantiate the censorship-mechanism pattern versus the privacy-fundamental pattern.').

omega_variable(
    sibling_reading_disagreement_locus,
    'Where exactly do the three kernel readings (censorship_mechanism, privacy_fundamental, competitive_moat) locate the mechanism''s primary function — suppression, sovereignty, or market-structuring — and is this a matter of which requests are examined, or a deeper disagreement about what Article 17 IS for?',
    'Comparative doctrinal analysis of CJEU jurisprudence (e.g., Google Spain, GC v CNIL) alongside empirical request-pattern data; legislative history review of Article 17''s drafting debates for evidence of which function was primary intent versus emergent effect.',
    'If the disagreement is purely about which subset of requests each reading examines, the three readings are compatible partial descriptions of one mechanism with heterogeneous effects. If it is a deeper disagreement about foundational purpose, the readings may be in more direct tension than the network edges below suggest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_disagreement_locus, conceptual, 'Whether sibling kernel readings disagree about subset-of-cases or about foundational function.').

omega_variable(
    notification_standing_reform_effect,
    'Would extending notification and adversarial standing rights to publishers/archivists before delisting resolve the suppression function this reading identifies, without undermining the genuine privacy-protection function?',
    'Comparative study of jurisdictions or platforms that have piloted publisher-notification requirements versus those that have not, measuring both erasure-request success rates and privacy-outcome satisfaction.',
    'If procedural reform (notice + adversarial process) resolves the suppression pattern while preserving privacy removals, the extraction identified here is a fixable procedural defect rather than an inherent feature of the right — supporting a scaffold-adjacent remedy rather than requiring wholesale reconception of the right.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(notification_standing_reform_effect, empirical, 'Whether procedural reform could separate the suppression function from the coordination function.').


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
narrative_ontology:measurement(arti_tr_t16, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(arti_tr_t20, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(arti_tr_t24, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(arti_be_t4, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(arti_be_t8, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 8, 0.57).
narrative_ontology:measurement(arti_be_t12, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(arti_be_t16, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(arti_be_t20, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 20, 0.69).
narrative_ontology:measurement(arti_be_t24, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 24, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(arti_su_t4, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 4, 0.61).
narrative_ontology:measurement(arti_su_t8, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement(arti_su_t12, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(arti_su_t16, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 16, 0.73).
narrative_ontology:measurement(arti_su_t20, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 20, 0.76).
narrative_ontology:measurement(arti_su_t24, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 24, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__censorship_mechanism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article17_erasure_right__censorship_mechanism_reading, article17_erasure_right__privacy_fundamental_reading).
narrative_ontology:affects_constraint(article17_erasure_right__censorship_mechanism_reading, article17_erasure_right__competitive_moat_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language label 'Article 17 right to erasure,' per the ε-invariance principle: measuring the mechanism through the lens of exploited public-interest suppression yields a markedly different ε, beneficiary set, and victim set than measuring it through the lens of individual data sovereignty (privacy_fundamental_reading) or through the lens of platform compliance-cost asymmetry (competitive_moat_reading). The three stories share the same underlying legal text and administering institutions (search engines, data protection authorities) but instantiate structurally distinct constraints with distinct claimed types. All three are linked bidirectionally via affects_constraints to preserve the kernel-family relationship for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
