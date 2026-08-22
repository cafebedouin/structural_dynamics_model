% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__censorship_mechanism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: GDPR Article 17 Erasure Machinery as Speech-Suppression Mechanism (Censorship Reading)
 *   domain: technology governance/data protection law/competition policy
 *
 * SUMMARY:
 *   GDPR Article 17 grants individuals a right to erasure of personal data,
 *   administered through controller self-assessment, supervisory-authority
 *   adjudication, and CJEU interpretation. This story instantiates ONE
 *   reading of that contested kernel: the censorship-mechanism reading, under
 *   which the erasure machinery operates as a prior-restraint substitute —
 *   strategic requesters weaponize privacy language to strip lawful
 *   public-interest journalism and archival material from search-mediated
 *   visibility, and the suppression is emergent rather than designed. The
 *   epsilon referent is the standing arrangement (Article 17 as actually
 *   operated, 2018–2025), assessed by this reading's lights; the sibling
 *   readings (privacy_fundamental, competitive_moat) are separate constraints
 *   in separate files, linked via network.affects_constraints. Assumptions
 *   stated: the interval maps years since GDPR application (2018 = t0, 2025 =
 *   t7); measurement values are corpus-informed judgments from documented
 *   delisting volumes, CJEU expansion rulings, and requester-category
 *   studies, not direct instrument readings.
 *
 * KEY AGENTS:
 *   - - bad_faith_erasure_requesters: Primary beneficiary (organized/mobile) — obtains scrubbed public record at near-zero cost
 *   - - reputation_management_industry: Secondary beneficiary (organized/arbitrage) — monetizes request volume
 *   - - investigative_journalists: Primary target (moderate/constrained) — loses findability, bears rebuttal costs
 *   - - digital_archivists: Primary target (moderate/constrained) — completeness of the record eroded request-by-request
 *   - - information_seeking_public: Diffuse target (powerless/trapped) — silent gaps in the searchable record
 *   - - eu_data_protection_authorities: Agenda-setter (institutional/constrained) — adjudicates and orders delisting
 *   - - search_engine_operators: Dual-positioned agenda-setter/payer (institutional/arbitrage) — runs the pipeline, caps exposure by geofencing
 *   - - press_freedom_organizations: Excluded challenger (organized/constrained) — no seat in the complaint procedure
 *   - - media_law_scholars: Analytical observer (analytical/analytical) — documents drift between framing and operation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__censorship_mechanism_reading, 0.72).
domain_priors:suppression_score(article17_erasure_right__censorship_mechanism_reading, 0.78).
domain_priors:theater_ratio(article17_erasure_right__censorship_mechanism_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__censorship_mechanism_reading, tangled_rope).
narrative_ontology:human_readable(article17_erasure_right__censorship_mechanism_reading, "GDPR Article 17 Erasure Machinery as Speech-Suppression Mechanism (Censorship Reading)").
narrative_ontology:topic_domain(article17_erasure_right__censorship_mechanism_reading, "technology governance/data protection law/competition policy").

domain_priors:requires_active_enforcement(article17_erasure_right__censorship_mechanism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__censorship_mechanism_reading, '7ee095b3-4607-4206-866d-5479f545f868').
narrative_ontology:cs_kernel_codification('7ee095b3-4607-4206-866d-5479f545f868', fixed_text).
narrative_ontology:cs_authority_grounding('7ee095b3-4607-4206-866d-5479f545f868', lineage).
narrative_ontology:cs_interpretation_layer_present('7ee095b3-4607-4206-866d-5479f545f868').
narrative_ontology:cs_reading_relation('7ee095b3-4607-4206-866d-5479f545f868', article17_erasure_right__privacy_fundamental_reading, coexists_with).
narrative_ontology:cs_reading_relation('7ee095b3-4607-4206-866d-5479f545f868', article17_erasure_right__competitive_moat_reading, coexists_with).
narrative_ontology:cs_axiom('7ee095b3-4607-4206-866d-5479f545f868', foundational, speech_affecting_delisting_is_prior_restraint).
narrative_ontology:cs_axiom_status(speech_affecting_delisting_is_prior_restraint, holdable).
narrative_ontology:cs_axiom_grounding('7ee095b3-4607-4206-866d-5479f545f868', speech_affecting_delisting_is_prior_restraint, deontological).
narrative_ontology:cs_axiom('7ee095b3-4607-4206-866d-5479f545f868', secondary, strategic_use_is_material_share_of_requests).
narrative_ontology:cs_axiom_status(strategic_use_is_material_share_of_requests, holdable).
narrative_ontology:cs_axiom_grounding('7ee095b3-4607-4206-866d-5479f545f868', strategic_use_is_material_share_of_requests, empirically_contingent).
narrative_ontology:cs_reference_frame('7ee095b3-4607-4206-866d-5479f545f868', narrow_privacy_remediation_baseline).
narrative_ontology:cs_drift_state('7ee095b3-4607-4206-866d-5479f545f868', post_cjeu_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7ee095b3-4607-4206-866d-5479f545f868', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__censorship_mechanism_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, bad_faith_erasure_requesters).
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, reputation_management_industry).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, investigative_journalists).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, digital_archivists).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, information_seeking_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, search_engine_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Politicians, convicted offenders, disgraced executives, and litigants who file erasure demands against search results and archived copies of lawful, public-interest reporting concerning themselves — old convictions, financial failures, political scandals. They refile across controllers and member-state authorities until the material stops surfacing, bear essentially none of the regime's operating costs, and obtain a progressively scrubbed public record of their own conduct.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, bad_faith_erasure_requesters, beneficiary,
    organized, biographical, mobile, continental).

% Firms that sell delisting as a service: mass-filing erasure requests, coaching clients on private-life framings, and monitoring for reposted content. Revenue scales directly with the breadth of the erasure right and the volume of requests it admits. If the rules narrowed, they would shift into adjacent compliance and crisis-communications markets.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, reputation_management_industry, beneficiary,
    organized, immediate, arbitrage, global).

% Reporters and newsrooms publishing lawful public-interest journalism whose archives are delisted from search results after requester complaints. They bear rebuttal burdens, legal costs, and the loss of findability that constitutes much of their reach; a Europe-focused publication cannot route around EU delisting, and its product is worthless if undiscoverable.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, investigative_journalists, payer,
    moderate, biographical, constrained, continental).

% Libraries, web archives, and news repositories charged with preserving a complete public record. They receive takedown and delisting demands, must litigate or comply request-by-request, and face a structural dilemma: pre-emptively withholding material violates their mission, while contesting every demand exceeds their budgets. Completeness — the thing they exist to provide — is what the mechanism erodes.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, digital_archivists, payer,
    moderate, generational, constrained, global).

% Readers, voters, researchers, and counterparty-checkers who rely on search-mediated access to the public record. They experience silent gaps: pages removed without notice, contexts they cannot know are missing. They never chose this information environment, cannot audit it, and have no procedural seat anywhere in the request-and-delisting pipeline.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, information_seeking_public, payer,
    powerless, generational, trapped, continental).

% National supervisory authorities that adjudicate erasure complaints, issue binding delisting orders, and fine non-compliant controllers. Their caseload, staffing, and enforcement mandate expand with request volume. They interpret the Regulation and CJEU precedent but cannot amend either; their discretion runs inside boundaries set elsewhere.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, eu_data_protection_authorities, agenda_setter,
    institutional, generational, constrained, continental).

% Platform intermediaries that operate the delisting pipeline: receiving requests, applying balancing tests, and removing URLs from EU-facing result sets. They absorb compliance costs and fine risk, but cap their exposure by geofencing removals to European domain versions and passing residual costs to advertisers, publishers, and users. Delisting administration has become a routine internal function rather than an existential threat.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, search_engine_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__censorship_mechanism_reading, search_engine_operators, payer).

% Press-freedom, archive-access, and historian NGOs that litigate against delisting expansion and submit consultations. They hold no formal seat in the complaint procedure itself, which runs between requesters, controllers, and supervisory authorities; they enter only after a delisting has occurred, as challengers.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, press_freedom_organizations, excluded,
    organized, generational, constrained, global).

% Academic researchers tracking delisting volumes, requester categories, and case-law drift across member states. They document the divergence between the privacy-remediation framing of the mechanism and its observed operation against public-interest speech. They enforce nothing and collect nothing.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, media_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article17_erasure_right__censorship_mechanism_reading, bad_faith_erasure_requesters).
narrative_ontology:fixing_cost_class(article17_erasure_right__censorship_mechanism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves a genuine conflict between individual data sovereignty and permanent digital memory: it gives individuals a standardized, enforceable procedure to demand removal of personal data that is outdated, irrelevant, or unlawfully processed, and gives controllers a uniform compliance rule across the single market.
% TRANSFER_FUNCTION: Moves control over the visibility of lawfully published public-interest material from publishers, archivists, and the reading public to private requesters (with supervisory authorities as adjudicators); moves compliance costs and fine risk onto platform operators; and moves informational completeness away from everyone who searches.
% ABSENT_VOICES: Press-freedom organizations, archivists, historians, and future readers of the record would object to the mechanism's speech-affecting operation, but the complaint procedure is structured as requester-versus-controller before a regulator: the people whose access to information is degraded are never notified, never joined, and never counted. Their interests appear only obliquely, as 'public interest' factors weighed by the controller and the authority.
% DISAPPEARANCE_RATIONALE: If the erasure machinery vanished overnight, delisted material would resurface in search indexes within weeks, the reputation-management product line would collapse, platforms would dismantle their delisting pipelines, and archives would restore completeness — while genuinely private-data harms would revert to ad hoc defamation and data-protection litigation. The arrangements of requesters, regulators, platforms, and publishers all currently depend on the mechanism existing.
% FOUNDING_PROBLEM: Permanent digital memory: embarrassing, outdated, or disproportionate personal information persisting indefinitely in searchable form, denying individuals the practical ability to move past past events — a problem the CJEU framed as the imbalance between the lasting character of internet publication and the finite social relevance of old facts.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the CJEU's own Google Spain line of cases attests the digital-permanence harm; consumer-privacy research documents it; and press-freedom litigants — adversaries of the broad remedy — concede the underlying permanence problem while disputing its cure. No serious party denies the founding problem exists; the contest is over whether the remedy's side effects on public-interest speech are acceptable.
narrative_ontology:disappearance_verdict(article17_erasure_right__censorship_mechanism_reading, world_rearranges).
narrative_ontology:founding_problem_status(article17_erasure_right__censorship_mechanism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__censorship_mechanism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article17_erasure_right__censorship_mechanism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article17_erasure_right__censorship_mechanism_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.72 at interval end) because the mechanism transfers a public good — complete searchable record — to private hands at scale, with the transfer decoupled from any demonstrated privacy harm in the strategic-request case. Suppression (0.78) is authored as the raw structural property it is — coercion and closure of alternatives — and is deliberately NOT scaled by power or scope; only extractiveness is scaled downstream by directionality and scope in the engine's computation. Theater ratio (0.38) reflects a real adjudicative function increasingly wrapped in boilerplate: automated triage, formulaic balancing recitals, and refusal templates that process volume without individualized weighing. Accessibility collapse is moderate (0.48): alternatives persist — non-EU domain versions, VPN-routed search, independent archives — so the constraint closes the mainstream channel without closing the phenomenon. Resistance is substantial (0.62): sustained CJEU litigation, press-freedom campaigns, and scholarly documentation contest the mechanism's expansion continuously. The three measurement series run on ONE shared grid (annual points t0–t7, all metrics authored at every point) showing monotonic drift: extraction accumulating as case law widened delisting scope, suppression requirement rising as enforcement infrastructure matured, theater growing as volume pushed adjudication toward templated processing. No cyclical dynamics are asserted — the trajectory is ratchet-like, not oscillating.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the requester seat, the mechanism is due process: a statutory right vindicated against indifferent platforms. From the journalist and archivist seats, the same pipeline is prior restraint executed by private complaint — content disappears silently, with the burden of resurrection on its targets. From the platform seat it is a compliance cost center to be geofenced and automated. From the supervisory-authority seat it is rights administration working as intended. The engine derives these divergent classifications from the structural data (roles, power, exit); this story's claimed type does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Bad-faith requesters and the reputation-management industry are declared beneficiaries with mobile-to-arbitrage exit, placing them near the full-beneficiary end (low d, subsidized or inverted chi). Investigative journalists, digital archivists, and the information-seeking public are declared victims with constrained-to-trapped exit, placing them near the full-target end (high d, amplified chi) — the trapped public seat sits nearest d=1.0 because its members cannot exit an information environment they never chose. Supervisory authorities derive a low-moderate d as agenda-setters who collect mandate and caseload rather than rents. Search-engine operators are the deliberate complication: declared agenda-setter with a payer secondary role and arbitrage-grade exit (geofencing), so the derivation places them mid-scale rather than at either pole — they administer the extraction, bear real but capped costs, and have adapted profitably. No directionality_overrides are authored: the beneficiary/victim declarations plus exit atoms already differentiate every seat the derivation needs to distinguish, and the two institutional actors (authorities vs. platforms) differ in exit options (constrained vs. arbitrage), which the derivation reads directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim is what prevents misclassification in both directions. Reading the mechanism as pure snare would erase its genuine coordination function — real privacy harms against real individuals are remedied daily through the same pipeline, and the founding problem (permanent digital memory) is corroborated as live by parties hostile to the remedy. Reading it as pure rope would erase the asymmetric extraction this reading documents: the same structure that remedies genuine harms systematically transfers public-record visibility to strategic requesters, with suppression of speech as an emergent, actively enforced output. The mandatrophy question — has the mandate outlived its function? — answers NO here: the founding problem is live, so this is not a piton or zombie arrangement; it is a live coordination function carrying a growing extraction load, which is precisely the tangled-rope signature. The R5 fields record that consistency: founding_problem_status=live with disappearance_verdict=world_rearranges, so no dead-mandate/zombie flag is available or appropriate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of kernel article17_erasure_right. Would instantiating the privacy_fundamental_reading or competitive_moat_reading instead change the computed classification?',
    'Generate the sibling stories as separate epsilon-invariant constraints and compare engine-computed per-seat types across the family; convergence on tangled_rope across readings would indicate the hybrid structure is reading-robust.',
    'If the privacy_fundamental_reading dominates, epsilon falls toward the coordination floor and the type shifts toward rope; if the competitive_moat_reading dominates, the beneficiary set changes (incumbent platforms capture) and the type shifts toward snare. The censorship reading''s tangled_rope verdict is conditional on its functional characterization winning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Reading-relativity of the classification within the Article 17 kernel family.').

omega_variable(
    requester_motive_distribution,
    'What share of erasure requests targeting published public-interest journalism are strategic reputation management rather than genuine privacy harms?',
    'Audits of supervisory-authority decision files coding requester category, target content type, and outcome; longitudinal requester-motive studies comparable to the published delisting-transparency datasets.',
    'If genuine privacy harms dominate even speech-affecting requests, the censorship reading overstates extraction and epsilon falls materially; if strategic requests dominate, the prior-restraint characterization is confirmed and epsilon holds or rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(requester_motive_distribution, empirical, 'Empirical foundation of the bad-faith-requester beneficiary declaration.').

omega_variable(
    prior_restraint_chilling_measurement,
    'How much journalism goes unwritten, unpublished, or unindexed because of ANTICIPATED erasure liability, as distinct from content actually delisted?',
    'Editorial-policy surveys, newsroom counsel interviews, and comparative publication-rate studies across jurisdictions with differing delisting enforcement intensity.',
    'Prior-restraint substitution is this reading''s core mechanism claim: if chilling effects are negligible, the mechanism suppresses only what is actually delisted (measurable, bounded); if chilling is substantial, the true suppression footprint exceeds the delisting counts and effective extraction is understated by visible-case data.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prior_restraint_chilling_measurement, empirical, 'Whether the dominant suppression channel is visible delisting or invisible deterrence.').

omega_variable(
    geofencing_leakage,
    'Does EU-territorial delisting meaningfully suppress global access, given that removals apply only to European domain versions and determined users can route around them?',
    'Traffic-share analysis of EU-domain versus global-domain search result exposure for delisted URLs; user-behavior studies on search-version selection.',
    'If leakage is large, realized suppression (and therefore realized extraction borne by the information-seeking public) is lower than the nominal mechanism suggests, pulling effective extraction down for the trapped public seat; if leakage is small — because mainstream access runs through EU-facing defaults — the nominal and realized footprints converge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geofencing_leakage, empirical, 'Effectiveness of territorial delisting against the global information environment.').

omega_variable(
    balancing_test_administrability,
    'Can the public-interest-versus-privacy balancing test be administered consistently at platform scale, or is its indeterminacy itself the mechanism through which strategic requests succeed?',
    'Inter-rater reliability studies on identical request fact-patterns across controllers and authorities; reversal-rate analysis of appealed delisting decisions.',
    'If the test is administrable, erroneous speech-affecting delistings are correctable noise and the extraction is attributable to motive gaming at the margins; if the test is fundamentally indeterminate, the indeterminacy IS the extraction surface — requester persistence plus adjudicator variance yields scrubbing without any finding of harm — and the constraint sits closer to the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_test_administrability, conceptual, 'Whether doctrinal indeterminacy is bug or load-bearing extraction mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__censorship_mechanism_reading, 0, 7).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(arti_tr_t1, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 1, 0.27).
narrative_ontology:measurement(arti_tr_t2, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 2, 0.29).
narrative_ontology:measurement(arti_tr_t3, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 3, 0.31).
narrative_ontology:measurement(arti_tr_t4, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 4, 0.33).
narrative_ontology:measurement(arti_tr_t5, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(arti_tr_t6, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 6, 0.37).
narrative_ontology:measurement(arti_tr_t7, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 7, 0.38).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(arti_be_t1, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 1, 0.58).
narrative_ontology:measurement(arti_be_t2, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 2, 0.62).
narrative_ontology:measurement(arti_be_t3, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 3, 0.65).
narrative_ontology:measurement(arti_be_t4, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 4, 0.67).
narrative_ontology:measurement(arti_be_t5, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 5, 0.69).
narrative_ontology:measurement(arti_be_t6, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 6, 0.71).
narrative_ontology:measurement(arti_be_t7, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 7, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(arti_su_t1, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 1, 0.63).
narrative_ontology:measurement(arti_su_t2, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 2, 0.66).
narrative_ontology:measurement(arti_su_t3, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 3, 0.69).
narrative_ontology:measurement(arti_su_t4, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 4, 0.72).
narrative_ontology:measurement(arti_su_t5, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 5, 0.74).
narrative_ontology:measurement(arti_su_t6, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 6, 0.76).
narrative_ontology:measurement(arti_su_t7, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 7, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__censorship_mechanism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article17_erasure_right__censorship_mechanism_reading, article17_erasure_right__privacy_fundamental_reading).
narrative_ontology:affects_constraint(article17_erasure_right__censorship_mechanism_reading, article17_erasure_right__competitive_moat_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'GDPR Article 17 / right to be forgotten.' The label conflates three structurally distinct claims with different epsilon values and different beneficiary structures: (1) the privacy_fundamental_reading (individual data sovereignty as fundamental right — highest empirical confidence, upstream); (2) this censorship_mechanism_reading (strategic erasure as prior-restraint substitute against public-interest speech — downstream, contested); (3) the competitive_moat_reading (compliance-cost asymmetry protecting incumbents — downstream, contested). The upstream privacy reading is cited as legitimating evidence by the downstream readings' opponents and proponents alike. Each story carries its own epsilon, its own stakeholders, and its own claimed type; they are linked here and in their own files via network.affects_constraints. This story authors epsilon for the standing arrangement — Article 17 as operated — assessed by the censorship reading's own lights; it does not average across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
