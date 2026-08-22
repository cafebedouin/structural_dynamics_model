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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article17_erasure_right__censorship_mechanism_reading
 *   human_readable: Article 17 Right to Erasure as Content Suppression Mechanism
 *   domain: technology_governance/data_protection_law/competition_policy
 *
 * SUMMARY:
 *   Article 17 of the GDPR (Right to be Forgotten) grants individuals the
 *   right to request deletion of their personal data under specified
 *   circumstances. This constraint instantiates the
 *   censorship_mechanism_reading: Article 17 as a weaponized right enabling
 *   suppression of truthful, published content by bad-faith requesters
 *   (individuals, reputation firms, political actors) who frame suppression
 *   as privacy protection. The reading's structural delta is that
 *   beneficiaries are suppressors (who shift the cost of erasure to
 *   archivists, journalists, and researchers), not privacy-vulnerable
 *   individuals, and victims are information preservationists, not
 *   corporations hoarding data. The founding problem (corporate data
 *   retention without consent) has been substantially solved for legitimate
 *   privacy cases; the remaining extraction flows from suppression
 *   weaponization. This reading coexists with the privacy_fundamental_reading
 *   (Article 17 as genuine individual data sovereignty) in the contested
 *   kernel — they are live positions held by different communities and
 *   neither forecloses the other in principle, though structural design
 *   choices can amplify one reading or the other. The
 *   censorship_mechanism_reading does not deny that personal privacy
 *   interests exist; it asserts that Article 17 as currently enforced enables
 *   suppression indistinguishable from privacy protection and that the
 *   suppression harm exceeds the privacy benefit for information-dependent
 *   communities.
 *
 * KEY AGENTS:
 *   - Bad-faith erasure requesters (individuals, firms, political actors): Primary beneficiaries. Suppress negative-but-truthful content by weaponizing privacy claims. Zero compliance cost, high speech-suppression benefit.
 *   - Reputation management firms: Organized beneficiaries. Systematize suppression as a commercial service.
 *   - Archivists and researchers: Primary victims. Bear the cost of legal review, technical implementation, and source loss.
 *   - Journalists and independent media: Primary victims. Suppression attacks their published work retroactively.
 *   - Large platforms (Google, Meta, etc.): Agenda-setters. Execute erasure requests; have the power to filter bad-faith requests but treat all erasure as compliance burden.
 *   - EU regulators: Observers. Adjudicate disputes; face pressure from both suppression-weaponizers and speakers.
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
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__censorship_mechanism_reading, snare).
narrative_ontology:human_readable(article17_erasure_right__censorship_mechanism_reading, "Article 17 Right to Erasure as Content Suppression Mechanism").
narrative_ontology:topic_domain(article17_erasure_right__censorship_mechanism_reading, "technology_governance/data_protection_law/competition_policy").

domain_priors:requires_active_enforcement(article17_erasure_right__censorship_mechanism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__censorship_mechanism_reading, '5f1b8419-38e7-4891-9164-bbc29f2c6f6e').
narrative_ontology:cs_kernel_codification('5f1b8419-38e7-4891-9164-bbc29f2c6f6e', formalized).
narrative_ontology:cs_authority_grounding('5f1b8419-38e7-4891-9164-bbc29f2c6f6e', extraction).
narrative_ontology:cs_interpretation_layer_present('5f1b8419-38e7-4891-9164-bbc29f2c6f6e').
narrative_ontology:cs_reading_relation('5f1b8419-38e7-4891-9164-bbc29f2c6f6e', article17_erasure_right__privacy_fundamental_reading, coexists_with).
narrative_ontology:cs_reading_relation('5f1b8419-38e7-4891-9164-bbc29f2c6f6e', article17_erasure_right__competitive_moat_reading, coexists_with).
narrative_ontology:cs_axiom('5f1b8419-38e7-4891-9164-bbc29f2c6f6e', foundational, erasure_as_speech_suppression_enabled_by_privacy_framing).
narrative_ontology:cs_axiom_status(erasure_as_speech_suppression_enabled_by_privacy_framing, holdable).
narrative_ontology:cs_axiom_grounding('5f1b8419-38e7-4891-9164-bbc29f2c6f6e', erasure_as_speech_suppression_enabled_by_privacy_framing, empirically_contingent).
narrative_ontology:cs_axiom('5f1b8419-38e7-4891-9164-bbc29f2c6f6e', foundational, bad_faith_weaponization_is_structural_not_anomalous).
narrative_ontology:cs_axiom_status(bad_faith_weaponization_is_structural_not_anomalous, holdable).
narrative_ontology:cs_axiom_grounding('5f1b8419-38e7-4891-9164-bbc29f2c6f6e', bad_faith_weaponization_is_structural_not_anomalous, empirically_contingent).
narrative_ontology:cs_reference_frame('5f1b8419-38e7-4891-9164-bbc29f2c6f6e', privacy_protection_via_data_control).
narrative_ontology:cs_drift_state('5f1b8419-38e7-4891-9164-bbc29f2c6f6e', contemporary_suppression_weaponization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5f1b8419-38e7-4891-9164-bbc29f2c6f6e', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__censorship_mechanism_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, bad_faith_erasure_requesters).
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, reputation_management_firms).
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, political_actors_suppressing_critique).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, archival_institutions).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, investigative_journalists).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, researchers).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, independent_media).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, researchers_and_academics).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, independent_media_outlets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals and organizations weaponizing Article 17 requests to suppress negative but truthful content about themselves: past misconduct, financial fraud, political statements, unflattering biographies. They file erasure requests claiming privacy harm when the actual harm is reputational. They benefit from selective removal of searchable records while maintaining deniability — the law provides the legitimacy, the request obscures the motive. Exit is costless: a rejected request carries no penalty; they can refile under different framing or jurisdiction-shop to sympathetic platforms.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, bad_faith_erasure_requesters, beneficiary,
    powerful, biographical, arbitrage, global).

% Commercial services that file and manage erasure requests on behalf of clients (usually wealthy individuals and corporations). They have systematized the weaponization: maintain templates, track which platforms and appeal bodies are most permissive, optimize request language, provide compliance documentation. They extract revenue from suppression services, benefiting from the high compliance costs that platforms impose on themselves to avoid regulatory risk.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, reputation_management_firms, beneficiary,
    organized, biographical, arbitrage, global).

% Governments, political parties, and officials using Article 17 as a tool to suppress criticism, journalistic investigations, and historical records of misconduct. They file erasure requests for content documenting human rights violations, corruption, or policy failures, framing them as privacy violations. They benefit from information asymmetry: they can suppress but citizens cannot easily suppress content about them. Their exit is constrained only by public pressure and cross-border enforcement limits.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, political_actors_suppressing_critique, beneficiary,
    institutional, generational, mobile, national).

% Libraries, web archives, historical societies that preserve records for scholarly and public use. They face escalating erasure requests for content in their collections, often with implicit or explicit threats of regulatory action or lawsuits if they don't comply. Compliance requires costly legal review of each request, technical infrastructure to track and execute removals, and the loss of irreplaceable primary sources. They bear the cost of suppression while having no power to defend their preservation mission — their only exit is withdrawing from EU jurisdiction or ceasing to preserve digital history.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, archival_institutions, payer,
    moderate, civilizational, constrained, global).

% Reporters and media organizations that publish investigations, often supported by archived evidence and searchable historical records. They see erasure requests used to suppress their own published work after it becomes inconvenient to the subject. Their reporting becomes less discoverable, harder to corroborate, and vulnerable to denial ('that never happened because the records are gone'). They can litigate erasure decisions, but the cost is prohibitive for small outlets; larger outlets face reputational pressure and advertiser threats from powerful erasure requesters.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, investigative_journalists, payer,
    moderate, biographical, constrained, global).

% Scholars and data scientists studying digital culture, misinformation, political communication, and corruption. They depend on historical records, searchable archives, and the ability to cite sources for reproducible research. Erasure requests fragment the corpus: primary sources disappear, citations break, datasets become incomplete and unreliable. They cannot exit without abandoning EU-relevant research; they cannot negotiate with erasure requesters; their only option is adapting methodology to work around missing data.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, researchers_and_academics, payer,
    moderate, generational, constrained, global).

% Smaller news organizations, fact-checking sites, and citizen journalism platforms that cannot afford dedicated legal teams to defend erasure disputes. They receive erasure requests for investigative content and must choose: comply at the cost of silencing their own work, or resist at the risk of regulatory liability and platform pressure. Their identity is bound to their publication mission, so exiting the EU market means abandoning their geographic and linguistic audience.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, independent_media_outlets, payer,
    moderate, biographical, identity_locked, global).

% National data protection authorities and EU institutions that interpret and enforce Article 17. They receive complaints from erasure requesters (framing erasure as privacy protection) and from journalists/archivists (framing erasure as censorship). They adjudicate based on privacy-vs-speech balancing tests that favor privacy claimants when the request is strategically framed. They have the power to reshape enforcement but face lobbying pressure from both directions.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, eu_regulators_and_authorities, observer,
    institutional, generational, analytical, national).

% Google, social media platforms, and content hosts that execute erasure requests by delisting or removing content from their indexes and services. They are the enforcement mechanism: they receive requests, evaluate them (at vast scale and with imperfect legal judgment), and implement removals. They benefit from treating erasure as a compliance burden (regulatory avoidance) rather than as a front-end design choice (which would require rethinking how they mediate access to information). Their compliance costs are high enough to deter challenging requests.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, large_platforms, agenda_setter,
    institutional, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article17_erasure_right__censorship_mechanism_reading, bad_faith_erasure_requesters).
narrative_ontology:fixing_cost_class(article17_erasure_right__censorship_mechanism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Article 17 solves a real coordination problem: individuals should have some right to prevent indefinite retention and searchability of their personal data by commercial entities, especially when the data is inaccurate, irrelevant, or their circumstances have changed. The coordination achieved is that platforms maintain and respect deletion procedures, individuals can request removal without negotiating per-platform, and data protection authorities can enforce the standard. This coordination function is genuine: without it, individuals would have no systematic recourse against data retention by powerful companies.
% TRANSFER_FUNCTION: The arrangement transfers suppressibility from platforms (who could sell your data indefinitely) to individuals (who can request deletion). But it also transfers from information preservationists and speakers (who cannot defend their own published or archived content from strategic erasure). In high-extraction mode, bad-faith requesters pay nothing to suppress truthful negative content; archivists and journalists bear all the cost (legal review, technical implementation, loss of sources, reputational damage from appearing complicit in suppression).
% ABSENT_VOICES: Speakers and publishers — those whose own content is the target of erasure requests — are often absent from erasure request processes. They are rarely notified before removal; they have no standing to contest the request; many don't discover the suppression until researchers or readers notify them. Archivists and historians have even less voice: they are not stakeholders to individual erasure decisions and must litigate or lobby collectively to be heard. The excluded seat is the person or institution whose content is being erased.
% DISAPPEARANCE_RATIONALE: If Article 17's erasure right disappeared, searchable archives would stabilize, investigative journalism would become more discoverable and harder to suppress retroactively, and bad-faith erasure campaigns would require different mechanisms (direct threats, platform-by-platform takedown requests, legal intimidation without the privacy-rights framing). The world would rearrange: suppression would become visibly coercive rather than framed as privacy protection. The suppressed would know they were suppressed; the suppressor would be transparently acting. That transparency is itself why suppression would become harder.
% FOUNDING_PROBLEM: The founding problem was that commercial platforms retained personal data indefinitely, used it for profiling and ad-targeting without informed consent, and gave individuals no mechanism to request deletion. European data protection tradition held that individuals should have a right to control their own data; Article 17 instantiated that right, especially for accurate-but-unwanted data. The genuine founding problem was retention and surveillance, not searchability.
% FOUNDING_PROBLEM_CORROBORATION: Data protection advocates and regulators attest the founding problem (uncontrolled data retention) is partly live: platforms still retain data, profiling is ongoing, individuals still lack power over their data ecology. Journalists, archivists, and researchers attest the original problem is substantially solved for the stated purpose (you can now delete your own data from major platforms) but the solution has been weaponized to suppress others' speech. No authoritative external source (legislative record, regulatory guidance, academic consensus) attests that suppression of published journalism or historical records was the founding problem Article 17 was meant to solve.
narrative_ontology:disappearance_verdict(article17_erasure_right__censorship_mechanism_reading, world_rearranges).
narrative_ontology:founding_problem_status(article17_erasure_right__censorship_mechanism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__censorship_mechanism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is 0.68 at interval end, rising from 0.42 at start (t=0 to t=20): the measured trajectory shows acceleration as suppression weaponization becomes more sophisticated and normalized. Bad-faith requesters scale their operations (reputation firms, political campaigns), platforms internalize erasure as standard practice without distinguishing suppression from privacy, and victims' costs mount (legal defense, source loss, chilling effect on publication). Suppression is 0.79 (high) because the constraint's persistence depends on actively enforcing erasure against the resistance of journalists, archivists, and regulators who contest the requests. Theater ratio rises from 0.15 to 0.42: the constraint begins with legitimate privacy-deletion cases (low theater) but as bad-faith requests accumulate, a growing proportion of enforcement activity is ceremonial (defending suppression using privacy language, performing compliance without separating legitimate from weaponized requests). The accessibility_collapse (0.71) reflects that for bad-faith requesters and suppression beneficiaries, alternatives have largely collapsed: once Article 17 is established as a suppression lever, they have no incentive to exit — the arrangement is stable and low-cost for them. For victims, accessibility_collapse is lower: journalists can publish outside the EU, archivists can store offline, researchers can work with incomplete data — but each alternative is constrained by market realities and mission mandates. Resistance is 0.58 (moderate): archivists, journalists, and some regulators mount real resistance through litigation, media campaigns, and lobbying for regulatory clarification, but resistance is fragmented and resource-constrained relative to the scale of erasure requests.
 *
 * PERSPECTIVAL GAP:
 *   The constraint should exhibit strong seat divergence: (1) The agenda-setter (platforms) and suppressors (requesters) compute as benefiting from a legitimate coordination mechanism at their seats — they can frame erasure as routine privacy protection. (2) The victim seats (archivists, journalists) compute the same constraint as extractive coercion — they bear costs they did not choose and cannot defend against. (3) Regulators occupy an observer seat: they see both readings and face pressure from both, but have the structural power to reshape enforcement (e.g., by narrowing what counts as erasure-eligible personal data, requiring speaker notification, allowing archivists standing to contest requests). The engine should compute different per-seat classifications: the requester and platform seats might compute as rope or tangled-rope (beneficiaries coordinating with enforcement), while the victim seats compute snare (extraction with no meaningful benefit). This divergence is the signal the framework exists to capture: a constraint that looks coordinated from the beneficiary seat and extractive from the victim seat is exactly the structure false-consensus masking reveals.
 *
 * DIRECTIONALITY LOGIC:
 *   Bad-faith requesters (powerful, arbitrage exit): d near 0.0 (full beneficiary). They impose costs externally and face no reciprocal cost; they can exit by simply not requesting again, which carries zero penalty. Suppression beneficiaries (political actors, reputation firms) similarly have high arbitrage exit and low directionality (d ~0.1-0.25): they use the mechanism and can abandon it without consequence. Archivists and researchers (moderate power, constrained exit, civilizational horizon): d near 0.8 (near-target). They cannot exit without abandoning their mission (identity-locked for some), they bear costs involuntarily, and they have no reciprocal benefit. Journalists (moderate power, constrained exit): d near 0.85. They are trapped between compliance and professional obligation; refusing to comply risks regulatory liability while complying silences their own work. Large platforms (institutional power, mobile exit): d is approximately 0.5 (symmetric). They benefit from the arrangement (compliance-as-policy reduces their liability exposure), but they also bear cost (legal review, technical infrastructure, reputational damage from suppression scandals). Regulatory observers have d near 0.5: they are not extracted from or benefited by the constraint directly; they are positioned to reshape it.
 *
 * MANDATROPHY ANALYSIS:
 *   Article 17's founding problem (corporate data retention without consent) has been substantially solved: individuals can now delete their data, platforms have built deletion procedures, and regulators enforce the right. The founding problem was real and the right addresses it. However, the constraint's primary extractive function (measuring t=20 extractiveness 0.68, suppression 0.79, theater 0.42) is no longer solving the founding problem — it is enabling suppression of published content by bad-faith requesters. The mandatrophy is not complete: legitimate privacy deletions still happen (theater_ratio 0.42 means 42% of enforcement activity is performative; the remaining 58% likely includes real privacy cases). But the constraint persists and grows despite the founding problem's partial resolution, and the extraction comes from a weaponized use case (suppression) not the founding case (data sovereignty). This is mandatrophy in progress: the constraint has outlived its primary function and is now sustained by a secondary extraction function. The reading affirms mandatrophy_resolved: false (the constraint is not yet purely theatrical and the founding problem is not fully dead, only partially resolved and supplemented by suppression extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    privacy_speech_incommensurability,
    'Is there a principled boundary between legitimate privacy erasure (protecting personal data from profiling) and illegitimate speech suppression (hiding truthful public-interest content), or is the boundary observer-dependent?',
    'Comparative jurisprudence: examine which courts/regulators have articulated stable tests distinguishing personal-data-deletion from content-suppression and whether those tests withstand bad-faith erasure campaigns. If no stable test exists, the distinction is conceptual rather than structural.',
    'If the boundary is stable and enforceable, erasure suppression can be filtered at the request-evaluation stage; if it''s observer-dependent, the constraint''s extractiveness is structural and cannot be reformed without changing the underlying right. The reading hinges on this: if no boundary exists, Article 17 is structurally extractive when weaponized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(privacy_speech_incommensurability, conceptual, 'Whether personal privacy and public speech can be coherently separated under Article 17.').

omega_variable(
    bad_faith_prevalence_and_detection,
    'What fraction of Article 17 erasure requests are submitted in good faith (personal privacy interest) versus bad faith (suppression objective)? How reliably can platforms or regulators distinguish them?',
    'Empirical audit: audit published erasure decisions and request patterns; survey requesters about their motivations (transparent audit and post-removal interviews with journalists/subjects); track re-requests and jurisdiction-shopping as proxy for bad faith. Measure false-positive rate (rejecting legitimate privacy requests) and false-negative rate (approving suppression requests).',
    'If bad faith dominates and is detectable, the constraint is primarily a suppression mechanism and classification as snare is stable. If good faith is dominant, the extraction measured here is a side effect rather than the primary function. If detection is impossible, erasure enforcement is uninformable and suppression is undefendable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bad_faith_prevalence_and_detection, empirical, 'The actual composition of erasure requests and the detectability of suppression intent.').

omega_variable(
    reading_kernel_relationship,
    'Does the censorship_mechanism_reading foreclose the privacy_fundamental_reading, or do both readings coexist as defensible positions within different parties'' frameworks?',
    'Structural coherence test: can a single Article 17 framework simultaneously protect individual control over personal data AND prevent weaponization for content suppression? If yes, the readings coexist (both are true for different request types). If no, one reading''s core premise rules out the other''s. The resolution is about whether legitimate privacy deletions and bad-faith suppressions share the same causal mechanism (inextricably entangled) or can be separated by design (coherently separable).',
    'If readings coexist, both are live positions and regulatory reform could amplify one reading or the other through design choices (narrowing erasure-eligible data, requiring speaker notification, giving archivists standing). If one forecloses the other, the kernel itself is fractured and no single Article 17 regime can satisfy both readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_relationship, conceptual, 'Kernel reading relationship: whether censorship and privacy readings logically coexist or foreclose each other.').

omega_variable(
    suppression_internalization_vs_structural,
    'When archivists and journalists comply with erasure requests and suppress their own content, is that suppression structural (they have no real choice, facing legal liability and platform pressure) or internalized (they accept the privacy argument and self-censor voluntarily)?',
    'Post-compliance trajectory: interview archivists/journalists who complied; measure whether they comply again when legal threat is removed; compare removal rates before/after regulatory clarification that suppression requests should be rejected. If compliance drops when structural pressure is removed, the suppression is primarily structural; if it persists, partly internalized.',
    'If structural, the suppression metric (0.79) reflects genuine coercion and the constraint is snare-classified. If internalized, the suppression is self-imposed and the constraint''s effective power over the victims is lower than measured. Internalization also suggests that the reading''s characterization of victims as unwilling pawns may be inaccurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_vs_structural, empirical, 'Whether suppression compliance is structural coercion or internalized norm-following.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__censorship_mechanism_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(arti_tr_t0, observed).
narrative_ontology:measurement(arti_tr_t4, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement_basis(arti_tr_t4, observed).
narrative_ontology:measurement(arti_tr_t8, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement_basis(arti_tr_t8, observed).
narrative_ontology:measurement(arti_tr_t12, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement_basis(arti_tr_t12, observed).
narrative_ontology:measurement(arti_tr_t16, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 16, 0.39).
narrative_ontology:measurement_basis(arti_tr_t16, observed).
narrative_ontology:measurement(arti_tr_t20, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(arti_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(arti_be_t0, observed).
narrative_ontology:measurement(arti_be_t4, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement_basis(arti_be_t4, observed).
narrative_ontology:measurement(arti_be_t8, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(arti_be_t8, observed).
narrative_ontology:measurement(arti_be_t12, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 12, 0.63).
narrative_ontology:measurement_basis(arti_be_t12, observed).
narrative_ontology:measurement(arti_be_t16, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement_basis(arti_be_t16, observed).
narrative_ontology:measurement(arti_be_t20, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(arti_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(arti_su_t0, observed).
narrative_ontology:measurement(arti_su_t4, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 4, 0.68).
narrative_ontology:measurement_basis(arti_su_t4, observed).
narrative_ontology:measurement(arti_su_t8, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 8, 0.73).
narrative_ontology:measurement_basis(arti_su_t8, observed).
narrative_ontology:measurement(arti_su_t12, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 12, 0.76).
narrative_ontology:measurement_basis(arti_su_t12, observed).
narrative_ontology:measurement(arti_su_t16, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 16, 0.78).
narrative_ontology:measurement_basis(arti_su_t16, observed).
narrative_ontology:measurement(arti_su_t20, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 20, 0.79).
narrative_ontology:measurement_basis(arti_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__censorship_mechanism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article17_erasure_right__censorship_mechanism_reading, 0.12).
narrative_ontology:affects_constraint(article17_erasure_right__censorship_mechanism_reading, article17_erasure_right__privacy_fundamental_reading).
narrative_ontology:affects_constraint(article17_erasure_right__censorship_mechanism_reading, article17_erasure_right__competitive_moat_reading).

% DUAL FORMULATION NOTE:
% Article 17 (Right to Erasure, GDPR) is a contested kernel instantiating multiple structurally distinct constraints. The censorship_mechanism_reading identifies bad-faith suppressors as beneficiaries and information preservationists as victims, measuring high extractiveness and suppression. The privacy_fundamental_reading identifies individuals as beneficiaries (gaining data control) and corporations as victims (losing data assets), measuring lower extractiveness. The competitive_moat_reading identifies large platforms as beneficiaries (compliance costs create barriers to entry) and small platforms as victims (disproportionate burden). Each reading uses the same legal text but has different ε, different beneficiary/victim sets, and different classifications. The three stories are linked: changes in regulatory interpretation or enforcement of one reading structurally affect the others (e.g., stricter suppression filtering reduces the censorship reading's extractiveness; stricter compliance requirements raise the competitive_moat reading's cost burden).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article17_erasure_right__censorship_mechanism_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
