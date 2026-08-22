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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Article 17 Erasure-Right Censorship Mechanism
 *   domain: technology_governance/data_protection/speech_suppression
 *
 * SUMMARY:
 *   Article 17 of the GDPR provides individuals with a 'right to erasure'
 *   (the right to be forgotten) — a mechanism to request removal of personal
 *   data from platforms' indexes and servers. This reading frames Article 17
 *   as a censorship mechanism weaponized by bad-faith actors (political
 *   operatives, reputation managers, corporate defendants) to suppress
 *   unflattering but true records, documentation of misconduct, and archival
 *   evidence. The constraint extracts from speech-protective infrastructure
 *   (archivists, journalists, researchers) and transfers suppression power to
 *   actors motivated to suppress rather than control. The suppression is
 *   structural: platforms face compliance costs and liability exposure that
 *   incentivize over-removal and under-contestation; speech-protective
 *   exceptions are weakly defined and costly to defend; victims have no
 *   standing to resist erasure requests. This reading does NOT argue that
 *   privacy protection is illegitimate — it argues that privacy-protection
 *   mechanisms in this form have been captured and now function primarily as
 *   suppression infrastructure. The constraint operates as a snare: bad-faith
 *   requesters are beneficiaries (they use it to suppress), victims are
 *   trapped in an asymmetric legal environment (they cannot resist erasure
 *   efficiently), and active enforcement by platforms sustains the
 *   suppression.
 *
 * KEY AGENTS:
 *   - Bad-faith erasure requesters (agenda-setters, beneficiaries): political operatives, corporate reputation managers, defendants seeking to suppress documentation of misconduct. They exploit the procedural asymmetry to suppress records.
 *   - Archivists (victims): libraries, historical societies, digital preservation organizations. Trapped: archives depend on platform distribution, have no standing to contest erasure, cannot exit.
 *   - Journalists (victims): investigative reporters relying on searchable historical records for investigation. Constrained exit: can report outside EU but primary audience is EU-based.
 *   - Platform operators (payer + beneficiary): inherit enforcement burden but benefit from reduced liability exposure and plausible deniability.
 *   - Data protection authorities (observers): regulators interpreting Article 17's exceptions (public interest, journalistic purpose) reactively through complaints.
 *   - Privacy advocates (collateral beneficiaries): organizations that championed Article 17 for data-sovereignty reasons; they benefit rhetorically by treating data-control framing as sufficient justification.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__censorship_mechanism_reading, 0.68).
domain_priors:suppression_score(article17_erasure_right__censorship_mechanism_reading, 0.79).
domain_priors:theater_ratio(article17_erasure_right__censorship_mechanism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__censorship_mechanism_reading, snare).
narrative_ontology:human_readable(article17_erasure_right__censorship_mechanism_reading, "Article 17 Erasure-Right Censorship Mechanism").
narrative_ontology:topic_domain(article17_erasure_right__censorship_mechanism_reading, "technology_governance/data_protection/speech_suppression").

domain_priors:requires_active_enforcement(article17_erasure_right__censorship_mechanism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__censorship_mechanism_reading, '89238f08-b3cd-468b-b949-9b5b647e53bc').
narrative_ontology:cs_kernel_codification('89238f08-b3cd-468b-b949-9b5b647e53bc', fixed_text).
narrative_ontology:cs_authority_grounding('89238f08-b3cd-468b-b949-9b5b647e53bc', extraction).
narrative_ontology:cs_interpretation_layer_present('89238f08-b3cd-468b-b949-9b5b647e53bc').
narrative_ontology:cs_reading_relation('89238f08-b3cd-468b-b949-9b5b647e53bc', article17_erasure_right__privacy_fundamental_reading, coexists_with).
narrative_ontology:cs_reading_relation('89238f08-b3cd-468b-b949-9b5b647e53bc', article17_erasure_right__competitive_moat_reading, influences).
narrative_ontology:cs_axiom('89238f08-b3cd-468b-b949-9b5b647e53bc', foundational, erasure_weaponizable_for_suppression).
narrative_ontology:cs_axiom_status(erasure_weaponizable_for_suppression, holdable).
narrative_ontology:cs_axiom_grounding('89238f08-b3cd-468b-b949-9b5b647e53bc', erasure_weaponizable_for_suppression, empirically_contingent).
narrative_ontology:cs_axiom('89238f08-b3cd-468b-b949-9b5b647e53bc', foundational, procedural_asymmetry_enables_bad_faith_capture).
narrative_ontology:cs_axiom_status(procedural_asymmetry_enables_bad_faith_capture, holdable).
narrative_ontology:cs_axiom_grounding('89238f08-b3cd-468b-b949-9b5b647e53bc', procedural_asymmetry_enables_bad_faith_capture, empirically_contingent).
narrative_ontology:cs_reference_frame('89238f08-b3cd-468b-b949-9b5b647e53bc', privacy_protection_mandate).
narrative_ontology:cs_drift_state('89238f08-b3cd-468b-b949-9b5b647e53bc', contemporary_suppression_practice, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('89238f08-b3cd-468b-b949-9b5b647e53bc', '2026-06-19T14:23:47Z').
narrative_ontology:cs_kernel_id(article17_erasure_right__censorship_mechanism_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, bad_faith_erasure_requesters).
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, political_actors_seeking_suppression).
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, corporate_reputation_managers).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, archivists).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, journalists).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, historical_researchers).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, public_interest_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, platform_operators).
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, privacy_rights_advocates).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, platform_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Political operatives, corporate reputation managers, and individuals with resources to file systematic erasure requests. They exploit Article 17's procedural asymmetry: requesters bear minimal cost (a form submission), platforms must assess each request against speech-protective exceptions that are weakly defined and costly to defend. They weaponize the mechanism to suppress unflattering but true historical records, documentation of misconduct, or inconvenient archival evidence. Each successful erasure removes evidence from search results and hosting, making the suppressed content harder to access and verify.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, bad_faith_erasure_requesters, agenda_setter,
    organized, biographical, mobile, global).

% Libraries, historical societies, and digital preservation organizations that depend on search engines and hosting platforms to serve historical documents and web-archived material. They have no standing to contest erasure requests; when a document is delisted from search or removed from hosts, their collection becomes invisible. They cannot exit the constraint because the platforms that implement Article 17 are monopolistic distribution channels — no alternative infrastructure reaches archival users at scale. Their professional mandate to preserve records conflicts with an erasure regime that prioritizes privacy claims over historical preservation.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, archivists, payer,
    powerless, generational, trapped, global).

% Investigative reporters and news organizations that rely on searchable historical records, leaked documents, and archival evidence to report on misconduct, corruption, and pattern violations. They face three layers of suppression: (1) the original actor files erasure requests to remove inconvenient documentation; (2) journalists must expend legal resources to contest removal; (3) even if a request is denied, the chilling effect reduces the incentive for future investigation. Their exit options are constrained by EU jurisdiction: they can relocate reporting outside the EU, but their primary audience and legal authority are EU-based.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, journalists, payer,
    powerful, biographical, constrained, continental).

% Google, Internet Archive, hosting platforms, and search engines that must process erasure requests under Article 17 penalty of administrative fines. They inherit the asymmetric enforcement burden: each request requires review, contestation infrastructure, appeals handling, and legal defense. They benefit from reduced liability exposure (erasing content removes a legal vector for both privacy claims AND speech-suppression allegations, creating plausible deniability). They are constrained by EU compliance cost: opting out of EU service is possible but costly in market access and regulatory standing.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, platform_operators, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__censorship_mechanism_reading, platform_operators, beneficiary).

% Academic researchers studying recent history, social movements, corporate behavior, or political discourse. They rely on searchable archives and primary sources to trace narratives and verify claims. Erasure requests fragment the historical record, creating gaps that make pattern-spotting and causal inference harder. They have limited exit: they can request original copies from institutions or conduct freedom-of-information requests, but search-accessible material is the default research pathway and erasure devalues it.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, historical_researchers, payer,
    moderate, generational, constrained, national).

% Whistleblowers, activists, and ordinary citizens who have published factual documentation of wrongdoing and now face erasure requests from the accused parties. They typically lack resources to hire lawyers to contest requests and cannot re-publish (they face repeated requests if they do). Their identity as speakers is fused with the act of documentation; silencing the record is a form of identity suppression. Exit is unavailable: they cannot simply withdraw; the suppression persists whether they cooperate or resist.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, public_interest_speakers, payer,
    powerless, biographical, identity_locked, national).

% National regulators (CNIL, BfDI, ICO) and the European Data Protection Board that oversee Article 17 enforcement. They interpret the law's exceptions (public interest, journalistic purpose, historical research) through guidance documents, but their authority is reactive: they investigate complaints and issue fines, not proactive design-level intervention. They sit at the observation point where the friction between privacy protection and speech suppression becomes visible via complaint patterns.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, data_protection_authorities, observer,
    institutional, generational, analytical, national).

% Privacy-focused civil society organizations and academics that championed Article 17 as a mechanism for individual data sovereignty. They frame erasure requests as individuals exercising control over their own data. Under this reading's interpretation, they are collateral beneficiaries: the mechanism they advocated for has become a censorship tool, and they benefit rhetorically by treating data-control framing as sufficient justification for suppression, even when the underlying motivation is bad-faith content removal.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, privacy_rights_advocates, beneficiary,
    organized, generational, mobile, continental).

% Constitutional traditions emphasizing free speech, public records, historical preservation, and truth-seeking (notably US constitutional law and common-law traditions) are structurally excluded from Article 17 interpretation. They would argue for speech-protective presumptions and narrow erasure scope, but they have no standing in EU law-making. Their exclusion ensures the privacy-centric framing dominates without full contest from competing legitimate values.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, competing_speech_frameworks, excluded,
    powerful, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article17_erasure_right__censorship_mechanism_reading, bad_faith_erasure_requesters).
narrative_ontology:fixing_cost_class(article17_erasure_right__censorship_mechanism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides individuals with a procedural mechanism to request removal of their personal data from indexing and hosting, framed as a right to control their own information.
% TRANSFER_FUNCTION: Transfers control over historical records and archival visibility from archivists, journalists, and researchers (who curate and provide access) to erasure requesters (who can suppress records by invoking privacy claims). The mechanism extracts from the speech-enabling infrastructure (archives, search, hosting) and concentrates suppression power in the hands of actors with the resources and motivation to weaponize erasure requests.
% ABSENT_VOICES: Constitutional speech-protection traditions (US First Amendment jurisprudence, common-law press freedom), archival and historical-preservation communities, journalists and researchers are structurally excluded from Article 17 policy-making in the EU context. They would argue for robust speech exceptions and historic-record presumptions but have no formal seat at implementation.
% DISAPPEARANCE_RATIONALE: If Article 17 and its enforcement architecture were removed, archivists would re-gain stable indexing for historical collections, journalists could use searchable primary sources for investigation without attrition from erasure requests, and historical records would persist as public evidence. Bad-faith requesters would lose their primary suppression lever. The digital information landscape would revert to a presumption of permanence rather than erasure-on-demand.
% FOUNDING_PROBLEM: European citizens had inadequate control over their personal data held by corporations and platforms; data was retained indefinitely, used for profiling, and difficult to remove at the individual's request. The founding motivation was individual data sovereignty — the right to control information about oneself.
% FOUNDING_PROBLEM_CORROBORATION: The European Commission and privacy advocates attest the founding problem was live and that Article 17 addresses it. Journalists, archivists, and digital rights organizations attest the founding problem has been substantially addressed by GDPR's broader transparency and access rights (Articles 13-21), and that Article 17's current operation has drifted to enable suppression disconnected from the legitimate control interest. Empirical studies document systematic use of erasure requests for reputation management and political suppression (Kosta & Ustek-Spilda, Helbing & Bussone) rather than personal data control.
narrative_ontology:disappearance_verdict(article17_erasure_right__censorship_mechanism_reading, world_rearranges).
narrative_ontology:founding_problem_status(article17_erasure_right__censorship_mechanism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__censorship_mechanism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article17_erasure_right__censorship_mechanism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article17_erasure_right__censorship_mechanism_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.42 to 0.68 over the interval, modeling the accumulation of suppression requests and the erosion of journalism/archival use of searchable records. Suppression is consistently high (0.58 → 0.79) and rising, modeling platform over-removal behavior in response to liability exposure and the chilling effect on victim-side contestation. Theater ratio rises from 0.18 to 0.42, modeling the increasing decoupling of platform compliance narratives (framed as privacy protection and user safety) from the actual outcome (systematic suppression of archival evidence and investigative material). The measurements track one shared time grid: every metric is authored at every examined time point (t=0,3,6,12,18,25), enabling the engine to detect convergence or divergence in drift patterns. Accessibility collapse (0.61) is moderate because, unlike a snare with trapped victims, this constraint's victims include powerful institutional actors (journalists, archivists) with partial exit options (reporting outside EU, using non-EU archives, FOIA requests) — but their default distribution channels are monopolistic platforms, so the accessibility collapse is still substantial. Resistance (0.58) is high enough to be non-negligible because victims actively contest erasure requests, file complaints to data protection authorities, and litigate — but the asymmetric burden means resistance is attrited over time.
 *
 * PERSPECTIVAL GAP:
 *   The bad-faith requester's seat and the victim seats should compute very differently. Bad-faith requesters experience Article 17 as a low-cost suppression tool (d near 0.0, full beneficiary): they submit a form, platforms comply, records disappear. Archivists experience it as a constraint on their core function (d near 1.0, full target): they cannot archive, distribute, or preserve records without facing removal. Journalists experience it as a suppression mechanism that extracts investigative capacity (d near 0.9): each suppression request triggers legal attrition and evidence loss. Platform operators experience it as mixed: they bear compliance cost (payer) but gain liability reduction and reduced political pressure from suppressed-speech litigation (beneficiary). The engine computes these divergent d values from the structural data (beneficiary/victim + power + exit). This reading's claim/metric independence is intentional: the constraint is CLAIMED as snare (bad-faith suppression enabled by privacy-protection architecture) while the authored metrics describe substantially extractive, actively enforced operation with rising theater (the privacy-protection narrative increasingly disconnects from the suppression outcome). The divergence is diagnostic — it is exactly how a captured mechanism is detected.
 *
 * DIRECTIONALITY LOGIC:
 *   Bad-faith requesters are the structural beneficiaries: they directly collects suppression (erasure of inconvenient records) and bear minimal cost (form submission). They are organized and mobile — they can move between platforms and request types to maintain suppression — and they have powerful institutional backing (political operatives, corporations). Archivists and journalists are the structural victims: they bear the extraction (loss of searchable archives, legal attrition, suppression of investigative capacity) and have trapped or constrained exit (platforms are monopolistic, EU jurisdiction is binding). Platform operators occupy a complex middle: they bear the active enforcement cost (reviewing each request, maintaining appeals infrastructure, defending borderline cases) but benefit from liability reduction. Under this reading's interpretation, platforms are ambiguous payers — they enforce the constraint reluctantly but benefit from its effect. Deriving directionality: bad-faith requesters (organized, mobile, zero cost to them, institutional backing) derive d near 0.2-0.3 (beneficiary end); archivists (powerless, trapped exit, core function extracted) derive d near 0.95 (full target); journalists (powerful, constrained exit, investigative capacity extracted) derive d near 0.85; platforms (institutional, constrained by EU law, forced enforcement, but liability reduction benefit) derive d near 0.6-0.7 (mixed, leaning payer). Privacy advocates (organized, mobile, rhetorical benefit from data-control framing, no direct cost) derive d near 0.25 (collateral beneficiary).
 *
 * MANDATROPHY ANALYSIS:
 *   Article 17's founding problem was individual data sovereignty — the legitimate interest in controlling information about oneself. That problem is substantially addressed by GDPR Articles 13-21 (transparency, access rights, rectification, objection), which let individuals know what data is held, access it, correct it, and object to processing. Article 17 as erasure-on-request adds a stronger right, but this reading observes that the constraint has drifted: erasure requests now overwhelmingly originate not from individuals protecting themselves but from political operatives and reputation managers suppressing records about OTHERS. The founding problem (my data, my control) is dead; the constraint persists because a new constituency (bad-faith suppressors) has captured it and now uses it for a different purpose (suppression of third-party evidence). This is a mandatrophy case: the constraint's mandate has outlived its function. The founding problem is CONTESTED (privacy advocates insist individuals still need erasure; journalistic freedom advocates insist the function has been captured for suppression), and the disappearance verdict is WORLD_REARRANGES (archives would persist, journalism would be enabled, historical records would stabilize). This reading resolves mandatrophy: the constraint persists not because it serves its founding function but because suppression constituencies have captured the mechanism. The alternative reading (privacy_fundamental_reading) would author disappearance_verdict differently (world_unchanged) and resolve mandatrophy as false (the founding function is live). The network link between readings (affects_constraints: competitive_moat_reading, privacy_fundamental_reading) documents the rivalry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    privacy_suppression_boundary,
    'Is the measured suppression primarily the cost of legitimate privacy protection, or does it constitute bad-faith weaponization of privacy mechanisms for content censorship?',
    'Detailed analysis of erasure request patterns: who requests (individual data subjects vs. third-party reputation managers), what content is targeted (personal data vs. historical documentation vs. investigative material), what proportion of requests cite personal data vs. character/reputation interests, and whether request clusters correlate with political/reputational events.',
    'If most requests are bad-faith suppression (third parties requesting removal of evidence about themselves, patterns coordinating with political pressure), the constraint reclassifies from a privacy-protection mechanism to a suppression mechanism. If most requests are genuine individual data-control cases, this reading''s snare classification is weakened to tangled_rope (coordination + extraction) or rope (pure coordination with side effects).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(privacy_suppression_boundary, empirical, 'The proportion of erasure requests that represent legitimate personal data control vs. bad-faith evidence suppression.').

omega_variable(
    public_interest_exception_efficacy,
    'Do Article 17''s exceptions for public interest and journalistic purpose actually protect archives and journalism, or do they function as theoretical but practically inaccessible protections?',
    'Analysis of platform denials and contestation outcomes: what percentage of erasure requests are denied on public-interest grounds, what is the successful contestation rate for journalists/archivists, and what is the cost (time and legal resource) to mount a successful defense?',
    'If the exceptions are rarely invoked and costly to defend, they are theatric — the constraint''s suppressive effect is unchanged by their formal existence. If exceptions are regularly applied and defensible at low cost, the suppression is modulated and the theater ratio drops; the constraint might be tangled_rope rather than snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(public_interest_exception_efficacy, empirical, 'Whether Article 17''s formal exceptions for journalistic and historical purpose function as meaningful protection or theater.').

omega_variable(
    kernel_reading_stability,
    'Will this reading remain stable under institutional evolution, or does the founding privacy-protection mandate embedded in Article 17 create pressure toward the privacy_fundamental_reading over time?',
    'Monitoring of EDPB guidance, legislative amendment proposals, and court decisions interpreting Article 17. Examine whether future institutional development rebalances toward privacy protection (narrowing this reading''s snare classification) or entrenches suppression (confirming it).',
    'If the kernel''s authority (the EDPB, courts, the European Commission) drifts toward stronger data-control norms, the privacy_fundamental_reading gains institutional backing and this reading''s classification as snare becomes contested-and-losing. If institutional development entrenches suppression and weakens exceptions, this reading''s snare classification is confirmed and mandated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_stability, conceptual, 'Whether Article 17''s interpretation will remain captured by the suppression reading or revert toward the founding privacy-protection mandate.').

omega_variable(
    speech_protection_in_eu_law,
    'Does EU law (Charter, ECHR, case law) provide sufficient structural protection for speech to override erasure requests, or is privacy protection institutionally privileged?',
    'Examination of CJEU and ECtHR jurisprudence on conflicts between privacy and free expression; analysis of how courts balance the two rights in specific cases involving journalists, archivists, and public-interest speech.',
    'If speech-protective jurisprudence is strong and actively applied, the suppression measurable in Article 17''s operation may be modulated by judicial review; if privacy is institutionally privileged, suppression is amplified. This affects whether the theater ratio (platform compliance narrative vs. actual suppression outcome) persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speech_protection_in_eu_law, conceptual, 'Institutional balance between privacy and speech protection in EU law; whether speech exceptions function or remain theatric.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__censorship_mechanism_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(arti_tr_t3, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 3, 0.24).
narrative_ontology:measurement(arti_tr_t6, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 6, 0.29).
narrative_ontology:measurement(arti_tr_t12, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(arti_tr_t18, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 18, 0.39).
narrative_ontology:measurement(arti_tr_t25, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(arti_be_t3, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(arti_be_t6, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 6, 0.54).
narrative_ontology:measurement(arti_be_t12, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(arti_be_t18, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 18, 0.66).
narrative_ontology:measurement(arti_be_t25, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(arti_su_t3, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 3, 0.64).
narrative_ontology:measurement(arti_su_t6, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 6, 0.7).
narrative_ontology:measurement(arti_su_t12, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 12, 0.76).
narrative_ontology:measurement(arti_su_t18, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 18, 0.78).
narrative_ontology:measurement(arti_su_t25, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 25, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__censorship_mechanism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article17_erasure_right__censorship_mechanism_reading, 0.12).
narrative_ontology:affects_constraint(article17_erasure_right__censorship_mechanism_reading, article17_erasure_right__privacy_fundamental_reading).
narrative_ontology:affects_constraint(article17_erasure_right__censorship_mechanism_reading, article17_erasure_right__competitive_moat_reading).

% DUAL FORMULATION NOTE:
% Article 17 of the GDPR is a contested kernel where three structurally distinct constraint readings coexist: (1) censorship_mechanism_reading (this story) — privacy mechanisms captured for suppression; (2) privacy_fundamental_reading — Article 17 as instantiation of individual data sovereignty; (3) competitive_moat_reading — Article 17 as compliance-cost asymmetry favoring incumbents. The three readings share the same legal text and enforcing institutions but instantiate different constraints because they identify different beneficiaries, victims, and causal outcomes. Each reading has its own ε (the suppressiveness of Article 17 under the censorship reading is NOT the same as the data-protection effectiveness under the privacy reading — they measure different referents). The readings are linked by network.affects_constraints to document the family relationship and enable comparative analysis of how institutional evolution modulates the contest between readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article17_erasure_right__censorship_mechanism_reading, powerless, 0.95).
constraint_indexing:directionality_override(article17_erasure_right__censorship_mechanism_reading, powerful, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
