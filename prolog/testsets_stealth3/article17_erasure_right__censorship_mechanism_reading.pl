% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__censorship_mechanism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: GDPR Article 17 Erasure Machinery — Censorship Mechanism Reading
 *   domain: technology governance/data protection law/competition policy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel: the
 *   censorship-mechanism reading of GDPR Article 17. The standing arrangement
 *   under contest — and the ε referent throughout — is the erasure regime as
 *   actually instantiated: statutory deletion rights executed through
 *   delegated platform adjudication under penalty asymmetry, assessed
 *   strictly by this reading's own lights. On this reading the machinery does
 *   double duty through one structure: it delivers a genuine deletion remedy
 *   to people harmed by persistent personal data, and it hands strategic
 *   actors a low-cost instrument for burying lawful published speech —
 *   accountability journalism, court reporting, archival record — under
 *   privacy credentials. Suppression of speech is treated here as an emergent
 *   function of the mechanism's design (removal-biased defaults under
 *   asymmetric fines), with erasure operating as a prior-restraint
 *   substitute: censorship achieved by private delisting rather than state
 *   prohibition. The claim/metric pair is authored independently:
 *   claimed_type states the structure I believe true (both coordination and
 *   extraction, actively enforced); the metrics state what I believe
 *   descriptively true of its operation. KEY AGENTS (by structural
 *   relationship): bad_faith_erasure_requesters — primary beneficiary
 *   (powerful/arbitrage), collects burial of unfavorable coverage;
 *   legitimate_data_subjects — coordinated-side beneficiary
 *   (powerless/constrained), genuine remedy users;
 *   eu_data_protection_authorities — agenda setter
 *   (institutional/identity_locked), administers enforcement and grows with
 *   it; platform_compliance_operations — administrator and cost bearer
 *   (institutional/constrained), runs removal pipelines under fine asymmetry;
 *   investigative_journalists — primary target (moderate/constrained), loses
 *   discoverability of published work; small_independent_publishers — target
 *   without recourse (powerless/trapped); digital_archivists — target of
 *   record-integrity erosion (moderate/constrained);
 *   information_seeking_public — diffuse bearer of degraded public record
 *   (powerless/trapped); historians_and_future_researchers — excluded voice,
 *   absent from every balancing process; press_freedom_monitors — analytical
 *   observer documenting suppression incidents.
 *
 * KEY AGENTS:
 *   - bad_faith_erasure_requesters: Primary beneficiary (powerful/arbitrage) — obtains burial of lawful coverage at near-zero filing cost, refiles across jurisdictions
 *   - legitimate_data_subjects: Coordinated-side beneficiary (powerless/constrained) — genuine deletion remedy for harassment and exposure harms
 *   - eu_data_protection_authorities: Agenda setter (institutional/identity_locked) — issues orders and fines; mandate breadth and institutional weight expand together
 *   - platform_compliance_operations: Administrator and cost bearer (institutional/constrained) — operates delisting pipelines with removal-biased incentives
 *   - investigative_journalists: Primary target (moderate/constrained) — archives decoupled from search discovery
 *   - small_independent_publishers: Target without recourse (powerless/trapped) — absorbs removals that larger media contests
 *   - digital_archivists: Target (moderate/constrained) — forced to choose between purge, geoblock, or unsustainable litigation
 *   - information_seeking_public: Diffuse payer (powerless/trapped) — record silently narrows, no notification, no seat
 *   - historians_and_future_researchers: Excluded voice (powerless/civilizational) — deferred loss represented by no one
 *   - press_freedom_monitors: Analytical observer (analytical/analytical) — external incident record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__censorship_mechanism_reading, 0.68).
domain_priors:suppression_score(article17_erasure_right__censorship_mechanism_reading, 0.76).
domain_priors:theater_ratio(article17_erasure_right__censorship_mechanism_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__censorship_mechanism_reading, tangled_rope).
narrative_ontology:human_readable(article17_erasure_right__censorship_mechanism_reading, "GDPR Article 17 Erasure Machinery — Censorship Mechanism Reading").
narrative_ontology:topic_domain(article17_erasure_right__censorship_mechanism_reading, "technology governance/data protection law/competition policy").

domain_priors:requires_active_enforcement(article17_erasure_right__censorship_mechanism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__censorship_mechanism_reading, '6efba4e5-0510-40a9-a2cd-802adc1cd3c9').
narrative_ontology:cs_kernel_codification('6efba4e5-0510-40a9-a2cd-802adc1cd3c9', fixed_text).
narrative_ontology:cs_authority_grounding('6efba4e5-0510-40a9-a2cd-802adc1cd3c9', lineage).
narrative_ontology:cs_interpretation_layer_present('6efba4e5-0510-40a9-a2cd-802adc1cd3c9').
narrative_ontology:cs_reading_relation('6efba4e5-0510-40a9-a2cd-802adc1cd3c9', article17_erasure_right__privacy_fundamental_reading, coexists_with).
narrative_ontology:cs_reading_relation('6efba4e5-0510-40a9-a2cd-802adc1cd3c9', article17_erasure_right__competitive_moat_reading, coexists_with).
narrative_ontology:cs_axiom('6efba4e5-0510-40a9-a2cd-802adc1cd3c9', foundational, erasure_does_not_reach_published_public_interest_speech).
narrative_ontology:cs_axiom_status(erasure_does_not_reach_published_public_interest_speech, holdable).
narrative_ontology:cs_axiom_grounding('6efba4e5-0510-40a9-a2cd-802adc1cd3c9', erasure_does_not_reach_published_public_interest_speech, deontological).
narrative_ontology:cs_axiom('6efba4e5-0510-40a9-a2cd-802adc1cd3c9', secondary, penalty_asymmetry_yields_systematic_precautionary_removal).
narrative_ontology:cs_axiom_status(penalty_asymmetry_yields_systematic_precautionary_removal, holdable).
narrative_ontology:cs_axiom_grounding('6efba4e5-0510-40a9-a2cd-802adc1cd3c9', penalty_asymmetry_yields_systematic_precautionary_removal, empirically_contingent).
narrative_ontology:cs_reference_frame('6efba4e5-0510-40a9-a2cd-802adc1cd3c9', private_sphere_erasure_boundary).
narrative_ontology:cs_drift_state('6efba4e5-0510-40a9-a2cd-802adc1cd3c9', contemporary_platform_scale_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6efba4e5-0510-40a9-a2cd-802adc1cd3c9', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__censorship_mechanism_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, bad_faith_erasure_requesters).
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, legitimate_data_subjects).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, investigative_journalists).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, digital_archivists).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, information_seeking_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, platform_compliance_operations).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, small_independent_publishers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Politicians, executives, convicted offenders, and firms with reputational exposure engage reputation-management intermediaries to file erasure and delisting demands against news articles, court reporting, and blog posts describing their past conduct. Filing is cheap, repeatable, and anonymous behind the data-subject framing; targets must either accept the burial of lawful coverage or mount counter-proceedings that cost orders of magnitude more than the request. When one jurisdiction refuses, the same demand is refiled elsewhere or recast under adjacent legal theories, leaving no single refusal that closes the route.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, bad_faith_erasure_requesters, beneficiary,
    powerful, biographical, arbitrage, global).

% Individuals harmed by persistent accurate personal data — stalking victims whose home addresses circulate, people appearing in non-consensual intimate imagery, those whose long-spent minor convictions dominate every search of their name — use the erasure route as their only practical lever against controllers and search indexes. They carry no agenda against public discourse; their remedy lives or dies on the same machinery that strategic actors exploit.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, legitimate_data_subjects, beneficiary,
    powerless, biographical, constrained, regional).

% National supervisory authorities receive complaints, issue binding erasure orders, and levy turnover-scaled fines, with cross-border cases routed through lead authorities. Enforcement volume defines their caseload, staffing, and institutional weight; the breadth of the mandate and their discretion in proportionality review grow together. Their institutional self-conception is fused with the data-subject-rights mission, making retreat from the enforcement role unthinkable from inside.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, eu_data_protection_authorities, agenda_setter,
    institutional, generational, identity_locked, continental).

% Search engines and social platforms operate the intake forms, review queues, and delisting pipelines that make erasure executable at scale, deciding under statutory deadlines whether named URLs stay reachable. Failure to remove risks fines scaled to global turnover, while wrongful removal draws little comparable penalty, so review practice tilts toward taking material down in ambiguous cases. Declining the role means exiting the European market; they absorb the compliance cost while running the removal apparatus.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, platform_compliance_operations, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__censorship_mechanism_reading, platform_compliance_operations, payer).

% Reporters and news organizations publish accountability journalism that names names; erasure demands against their archives decouple older investigations from search discovery exactly when their subjects seek reputational rehabilitation. Contesting a delisting means legal proceedings measured in years against requests filed in minutes, and republication or mirror hosting creates fresh legal exposure rather than escape.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, investigative_journalists, payer,
    moderate, biographical, constrained, continental).

% Independent outlets and individual writers lack the legal departments that let major media contest removals; a delisting notice against a small site's most-searched investigation is usually simply absorbed, taking the site's search traffic and advertising revenue with it. Audiences arrive almost entirely through search, so burial of a headline story is existential, and moving the audience off-index is not a realistic option.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, small_independent_publishers, payer,
    powerless, biographical, trapped, continental).

% Libraries, newspaper morgues, web archives, and memory institutions preserve the public record; erasure directives aimed at archived copies force choices among purging holdings, geoblocking access, or litigation they are poorly resourced to sustain. Their professional mandate is permanence, while the machinery's demand is selective unmaking of precisely the materials that document public life.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, digital_archivists, payer,
    moderate, generational, constrained, continental).

% Readers, voters, researchers, and due-diligence users query the open web expecting a faithful index of what was actually published; each granted strategic erasure silently narrows the record they see, with no notice that anything was removed. They cannot subscribe to an unerased index and hold no seat in any balancing process that precedes removal.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, information_seeking_public, payer,
    powerless, immediate, trapped, global).

% Future scholars reconstructing contemporary events depend on the completeness of today's accessible record; selective erasure deletes disproportionately the contested, reputation-relevant material historians most need. No step in any removal procedure solicits their objection; their loss is deferred, so it is represented by nobody in the room where decisions happen.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, historians_and_future_researchers, excluded,
    powerless, civilizational, trapped, universal).

% Media-freedom organizations and academic trackers document delisting patterns affecting journalism, maintain incident databases, and testify in legislative reviews of the regime. They collect nothing from the machinery and pay none of its costs; their product is the external record of what the mechanism does to publicly available speech.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, press_freedom_monitors, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article17_erasure_right__censorship_mechanism_reading, bad_faith_erasure_requesters).
narrative_ontology:fixing_cost_class(article17_erasure_right__censorship_mechanism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives individuals a workable route to compel deletion of harmful accurate personal data held by controllers and surfaced by search: before it, a person had no practical mechanism against indefinite retention and resurfacing of stalking exposure, non-consensual imagery, or stale records; the machinery solves that demand-side problem centrally instead of per-victim litigation.
% TRANSFER_FUNCTION: Moves de-publication power from publishers, archives, and platforms to whoever can credibly file as a data subject; moves compliance obligation and adjudication labor onto platforms; moves informational access away from the reading public, without any countervailing flow back to those who lose the record.
% ABSENT_VOICES: Historians and future researchers are structurally absent from every balancing process — their loss is deferred and unrepresented. The diffuse reading public receives no notice of removals and has no procedural seat. Small publishers and independent writers sit outside the consultations where platform and DPA practice is negotiated. Non-European speakers whose lawful speech is delisted under the regime's extraterritorial reach have no forum at all.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would strand legitimate data subjects with no deletion lever (harassed individuals' exposures would persist indefinitely), force platforms to dismantle built removal pipelines, return delisted journalism to search indexes within crawl cycles, and strip supervisory authorities of one of their highest-volume caseload domains — the surrounding arrangements would visibly reorganize around whatever deletion mechanism replaced it.
% FOUNDING_PROBLEM: Before the regime, individuals had no enforceable remedy against controllers who retained and republished damaging personal data indefinitely; search engines amplified old, accurate-but-no-longer-relevant material about named persons with no correction or deletion path. Article 17 was built to supply that missing deletion lever.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: European Data Protection Board guidance and national authority annual reports attest sustained volumes of harm-grounded, uncontested deletion complaints year over year; harassment-victim support services and academic privacy-law scholarship independently document the underlying problem. Notably, the strategic-requester population supplies no such attestation — its filings are the phenomenon this reading describes, not evidence for the founding problem's shape.
narrative_ontology:disappearance_verdict(article17_erasure_right__censorship_mechanism_reading, world_rearranges).
narrative_ontology:founding_problem_status(article17_erasure_right__censorship_mechanism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__censorship_mechanism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article17_erasure_right__censorship_mechanism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article17_erasure_right__censorship_mechanism_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.68: the standing arrangement lets a requester convert money and patience into the disappearance of lawful published material, with the cost of that disappearance carried by speakers, archives, and the undifferentiated public; the genuine remedy delivered to legitimate data subjects keeps this below snare-grade extraction. Suppression 0.76, authored as the raw structural property it is — NOT scaled by power or scope (only extractiveness is engine-scaled): statutory deadlines plus turnover-scaled fines for non-removal plus negligible penalty for wrongful removal produce a removal-biased default that persists regardless of who occupies any seat. Theater_ratio 0.34: nominal proportionality balancing ('careful review', journalistic-context carve-outs on paper) coexists with queue-throughput processing at scale — a real function with a growing performative shell. Accessibility_collapse 0.45: alternatives (offshore mirrors, independent archives, non-indexed retrieval) survive but each carries legal risk or friction, and in-jurisdiction discoverability collapses on grant. Resistance 0.58: a sustained decade-long litigation and campaign line meets the machinery and has won real narrowing (territorial-scope limits, public-role balancing tests) without displacing it. The suppression_requirement series is authored deliberately: this story specifically tracks enforcement-capacity change — the compliance apparatus was built out from 2018 and hardened into a precautionary-removal equilibrium, so enforcement intensification is a traced dynamic, not noise. All three series run on ONE shared seven-point grid (2014–2026, aligned by construction). Fixing_cost is prohibitive: amendment runs the Union ordinary legislative procedure across twenty-seven member states against a Charter-entrenched fundamental-right framing, with delegated infrastructure lock-in on top.
 *
 * PERSPECTIVAL GAP:
 *   Per-seat divergence is structural, not rhetorical. From the requester seat the machinery is a rights-assertion tool — effective extraction experienced as near zero, the arrangement a service. From the journalist and archivist seats the identical machinery operates as burial-by-notice: a restraint imposed on published speech without any tribunal, which is why this reading characterizes it as prior-restraint substitution. The platform seat experiences a compliance tax with skewed penalties; the supervisory seat experiences mandate fulfillment and caseload growth. Coalition potential among the payer seats exists on paper (publisher associations, archive consortia, press-freedom organizations already litigate together), but it is blunted by the payer structure: the largest cost falls on a diffuse public that cannot organize around a removal it never learns happened, while concentrated payers fight case-by-case against refilable requests. The engine computes these divergent classifications from the structural data above; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: strategic requesters sit near the full-beneficiary pole (they receive suppression outcomes at a price far below the cost they impose); legitimate data subjects likewise draw subsidy from the remedy. Victims — journalists, archivists, the reading public — sit toward the full-target pole, with the public's damped mobility (trapped, no notification channel) pushing it further toward full-target than its diffuse profile alone would suggest. Platform operations land near symmetric-plus: they pay real compliance costs while administering, and their constrained exit (market exit is not viable) keeps them from arbitrage relief. Supervisory authorities are qualitatively mildly subsidized — enforcement volume is their caseload, staffing, and relevance — though formally neutral; I note this for the engine's derivation rather than authoring an override, because the override surface keys on power_atom and would indiscriminately move both institutional seats. No directionality_overrides are authored: the beneficiary/victim plus exit data already yields the right qualitative shape for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification guards against two symmetrical mislabels. Reading the machinery as pure extraction (snare) erases the genuine remedy that harassment victims and exposure survivors demonstrably depend on, mispredicts the reform coalition (legitimate data subjects defend the mechanism that strategic actors exploit), and would license remedies that injure the coordinated side. Reading it as pure coordination (rope) ignores the measurable strategic-extraction channel running through the same intake forms. Holding both faces is exactly what the tangled-rope category exists for: coordination function present, asymmetric extraction present, active enforcement holding the shape. The mandate is not outlived — the founding problem (no deletion lever against controllers) is corroborated live from sources outside the beneficiary set — so mandatrophy is not resolved and no sunset clause is declared. The live drift risk is mandate expansion: scope creep from the private sphere into the public record, visible in the rising extractiveness series, which is the trajectory this reading predicts if the balancing layer keeps eroding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of kernel article17_erasure_right — the censorship_mechanism_reading. Would instantiating a sibling reading restructure the beneficiary/victim mapping, the transfer function, and epsilon?',
    'Author the sibling stories against the same source material and let the engine compute per-seat classifications for each; the disagreement localizes in the beneficiary/victim arrays and the dominant transfer function, not in any observable-selection parameter.',
    'Under article17_erasure_right__privacy_fundamental_reading epsilon falls toward the coordination floor (sole beneficiaries: data subjects; victims: none; type: rope). Under article17_erasure_right__competitive_moat_reading beneficiaries become incumbent platforms and victims become market entrants and smaller controllers. This story''s epsilon of 0.68 is stable only within the censorship-mechanism reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: this file is one of three sibling readings of the Article 17 erasure kernel; the inter-reading disagreement sits in who benefits, who pays, and which transfer dominates.').

omega_variable(
    weaponization_share,
    'What share of erasure actions reaching published lawful speech originate in strategic reputational campaigns rather than legitimate privacy harm?',
    'Cross-classified audit of supervisory-authority decisions and platform transparency reports, stratifying removals by target content type (accountability journalism, court reporting, public-figure coverage) and requester profile (repeat filers operating through reputation intermediaries versus one-time affected individuals).',
    'A low strategic share collapses this reading toward the privacy-fundamental sibling — epsilon drops, the victim set thins, classification drifts rope-ward. A high share pushes past tangled_rope toward snare: the coordination story becomes cover and identifiable victims dominate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(weaponization_share, empirical, 'Prevalence of bad-faith use versus genuine privacy demand within the removal pipeline.').

omega_variable(
    over_removal_attribution,
    'How much observed suppression is attributable to the statute''s allocation of adjudication to platforms under penalty asymmetry, versus platforms'' independently chosen precautionary excess that procedural safeguards would eliminate?',
    'Natural experiments comparing removal rates across jurisdictions that impose court-order or notice-and-counternotice safeguards on delisting against administrative-only regimes, holding content mix constant.',
    'If safeguards eliminate most suppression at negligible cost to legitimate erasure, the extractive component is a design artifact repairable inside the kernel (cheap-fix branch opens). If suppression persists under safeguards, it is structural to erasure reaching the public record at all, and this reading''s classification hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(over_removal_attribution, conceptual, 'Attribution of removal bias between statutory incentive design and discretionary platform caution.').

omega_variable(
    emergence_vs_episodic_abuse,
    'Is speech suppression an emergent function of the mechanism''s structure — systematic, design-driven, reproducible — or an episodic abuse tail correctable by better supervisory practice?',
    'Longitudinal outcome studies of completed erasure cycles stratified by content category, tracking requester repetition rates, recurrence of refiled demands after refusal, and restoration rates following appeal.',
    'An emergent-function finding stabilizes tangled_rope with drift pressure toward snare and validates the prior-restraint-substitute characterization. An episodic-abuse finding supports a rope-with-abuse-tail reading and materially lowers epsilon.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(emergence_vs_episodic_abuse, conceptual, 'Whether suppression is a designed-in systemic output or a correctable fringe of otherwise sound operation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__censorship_mechanism_reading, 2014, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2014, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 2014, 0.15).
narrative_ontology:measurement(arti_tr_t2016, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 2016, 0.18).
narrative_ontology:measurement(arti_tr_t2018, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 2018, 0.24).
narrative_ontology:measurement(arti_tr_t2020, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 2020, 0.28).
narrative_ontology:measurement(arti_tr_t2022, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 2022, 0.31).
narrative_ontology:measurement(arti_tr_t2024, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 2024, 0.33).
narrative_ontology:measurement(arti_tr_t2026, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 2026, 0.34).

% Extraction over time
narrative_ontology:measurement(arti_be_t2014, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 2014, 0.35).
narrative_ontology:measurement(arti_be_t2016, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 2016, 0.42).
narrative_ontology:measurement(arti_be_t2018, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 2018, 0.52).
narrative_ontology:measurement(arti_be_t2020, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 2020, 0.58).
narrative_ontology:measurement(arti_be_t2022, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 2022, 0.63).
narrative_ontology:measurement(arti_be_t2024, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 2024, 0.66).
narrative_ontology:measurement(arti_be_t2026, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2014, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 2014, 0.4).
narrative_ontology:measurement(arti_su_t2016, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 2016, 0.5).
narrative_ontology:measurement(arti_su_t2018, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 2018, 0.62).
narrative_ontology:measurement(arti_su_t2020, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement(arti_su_t2022, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 2022, 0.72).
narrative_ontology:measurement(arti_su_t2024, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 2024, 0.74).
narrative_ontology:measurement(arti_su_t2026, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 2026, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__censorship_mechanism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article17_erasure_right__censorship_mechanism_reading, article17_erasure_right__privacy_fundamental_reading).
narrative_ontology:affects_constraint(article17_erasure_right__censorship_mechanism_reading, article17_erasure_right__competitive_moat_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'right to be forgotten / Article 17' conflates three structurally distinct claims with divergent epsilon values — the censorship-mechanism claim (this file: epsilon 0.68; victims are journalists, archivists, and the reading public), the fundamental-privacy-rights claim (separate file: epsilon near the coordination floor, no victim set, rope-shaped), and the competitive-moat claim (separate file: beneficiaries are large incumbents, victims are entrants). Each is a separate story with its own claimed_type and stakeholder surface; this file links to both siblings. Directionality within the family: the privacy-fundamental reading supplies the legitimacy frame under which this reading's requests travel (credentials of privacy ride on the fundamental-right reading), so the fundamental-right story is upstream of this one; documented weaponization feeds back as legitimacy pressure on the upstream frame.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
