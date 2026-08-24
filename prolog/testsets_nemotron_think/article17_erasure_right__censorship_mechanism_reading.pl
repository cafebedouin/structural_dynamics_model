% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__censorship_mechanism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: article17_erasure_right__censorship_mechanism_reading
 *   human_readable: GDPR Article 17 Erasure Right as Censorship Mechanism
 *   domain: technology_governance/data_protection_law/competition_policy
 *
 * SUMMARY:
 *   GDPR Article 17 (Right to Erasure / Right to Be Forgotten) is presented
 *   as a privacy protection allowing individuals to remove personal data from
 *   search engines and platforms. This reading — the
 *   censorship_mechanism_reading — argues the mechanism has been structurally
 *   captured: the low-cost, ex parte request process combined with platforms'
 *   liability-averse over-compliance and the absence of public-interest
 *   representation has turned erasure into a prior-restraint substitute.
 *   Powerful actors (politicians, corporations, criminals) weaponize the
 *   right to suppress legitimate journalism and historical records. The
 *   constraint coordinates a genuine problem (stale digital records) but
 *   extracts asymmetrically: requesters gain unilateral control over public
 *   indexing; journalists and archivists lose audience and record integrity;
 *   the public loses access without representation. The engine computes
 *   per-seat types from this structural data; the claimed tangled_rope
 *   reflects the genuine coordination function coexisting with asymmetric
 *   extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__censorship_mechanism_reading, 0.78).
domain_priors:suppression_score(article17_erasure_right__censorship_mechanism_reading, 0.82).
domain_priors:theater_ratio(article17_erasure_right__censorship_mechanism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, accessibility_collapse, 0.76).
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__censorship_mechanism_reading, tangled_rope).
narrative_ontology:human_readable(article17_erasure_right__censorship_mechanism_reading, "GDPR Article 17 Erasure Right as Censorship Mechanism").
narrative_ontology:topic_domain(article17_erasure_right__censorship_mechanism_reading, "technology_governance/data_protection_law/competition_policy").

domain_priors:requires_active_enforcement(article17_erasure_right__censorship_mechanism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__censorship_mechanism_reading, 'e991a4fa-1e9b-4d4e-b870-35b352e3ba5c').
narrative_ontology:cs_kernel_codification('e991a4fa-1e9b-4d4e-b870-35b352e3ba5c', formalized).
narrative_ontology:cs_authority_grounding('e991a4fa-1e9b-4d4e-b870-35b352e3ba5c', extraction).
narrative_ontology:cs_interpretation_layer_present('e991a4fa-1e9b-4d4e-b870-35b352e3ba5c').
narrative_ontology:cs_reading_relation('e991a4fa-1e9b-4d4e-b870-35b352e3ba5c', article17_erasure_right__privacy_fundamental_reading, coexists_with).
narrative_ontology:cs_reading_relation('e991a4fa-1e9b-4d4e-b870-35b352e3ba5c', article17_erasure_right__competitive_moat_reading, coexists_with).
narrative_ontology:cs_axiom('e991a4fa-1e9b-4d4e-b870-35b352e3ba5c', foundational, erasure_as_prior_restraint_substitute).
narrative_ontology:cs_axiom_status(erasure_as_prior_restraint_substitute, holdable).
narrative_ontology:cs_axiom_grounding('e991a4fa-1e9b-4d4e-b870-35b352e3ba5c', erasure_as_prior_restraint_substitute, empirically_contingent).
narrative_ontology:cs_axiom('e991a4fa-1e9b-4d4e-b870-35b352e3ba5c', foundational, bad_faith_requesters_as_primary_beneficiaries).
narrative_ontology:cs_axiom_status(bad_faith_requesters_as_primary_beneficiaries, holdable).
narrative_ontology:cs_axiom_grounding('e991a4fa-1e9b-4d4e-b870-35b352e3ba5c', bad_faith_requesters_as_primary_beneficiaries, empirically_contingent).
narrative_ontology:cs_reference_frame('e991a4fa-1e9b-4d4e-b870-35b352e3ba5c', gdpr_article17_original_privacy_intent).
narrative_ontology:cs_drift_state('e991a4fa-1e9b-4d4e-b870-35b352e3ba5c', post_google_spain_expansion_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e991a4fa-1e9b-4d4e-b870-35b352e3ba5c', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__censorship_mechanism_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, bad_faith_requesters).
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, platforms_as_intermediaries).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, journalists).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, archivists).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, public_interest).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, historical_record).
narrative_ontology:constraint_vindicates(article17_erasure_right__censorship_mechanism_reading, privacy_as_speech_control).
narrative_ontology:constraint_vindicates(article17_erasure_right__censorship_mechanism_reading, erasure_as_prior_restraint_substitute).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% High-profile individuals, corporations, politicians, and convicted criminals who file erasure requests not to protect genuine privacy but to suppress legitimate public-interest reporting, embarrassing histories, or critical coverage. They exploit the asymmetry: filing is cheap, fighting is expensive, and platforms default to compliance. They can forum-shop across EU jurisdictions and use the threat of fines as leverage.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, bad_faith_requesters, beneficiary,
    powerful, biographical, arbitrage, global).

% Search engines and social platforms (Google, Meta, etc.) that implement erasure decisions. They bear compliance infrastructure costs but benefit from reduced liability risk and a structural incentive to over-comply (delist first, ask questions never). Their transparency reports show 90%+ delisting rates for certain request categories. They also gain a competitive moat: new entrants cannot afford the compliance machinery.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, platforms_as_intermediaries, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__censorship_mechanism_reading, platforms_as_intermediaries, beneficiary).

% Investigative and public-interest reporters whose work disappears from search indexes without notice or appeal. They learn of erasures only when sources vanish or stories become unfindable. No standing to contest in most jurisdictions; the requester-platform proceeding is ex parte. Some publications maintain 'right to be forgotten' tracking projects but have no legal remedy.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, journalists, payer,
    moderate, biographical, constrained, global).

% Libraries, digital archives, academic researchers, and memory institutions tasked with preserving the historical record. Erasure requests create holes in the collective memory: court records, corporate filings, political speeches, and evidence of public corruption become inaccessible. They have no party status in erasure proceedings and no statutory exemption for archival preservation.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, archivists, payer,
    moderate, generational, constrained, global).

% The diffuse public whose right to know is structurally unrepresented in erasure proceedings. No mechanism exists for public interest intervenors; the balancing test (privacy vs. public interest) is performed by platforms or DPAs with no adversarial testing. The public cannot 'opt out' of the information environment shaped by strategic erasure.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, public_interest, excluded,
    powerless, generational, trapped, global).

% National DPAs (CNIL, ICO, etc.) and the EDPB that issue guidelines, enforce fines, and adjudicate appeals. They formally endorse the balancing test but in practice resource constraints push them toward bright-line rules favoring erasure. Their enforcement actions against platforms for *non*-compliance vastly outnumber actions against abusive requesters.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, data_protection_authorities, agenda_setter,
    institutional, generational, analytical, continental).

% Civil society groups (EDRi, NOYB, etc.) that champion Article 17 as a fundamental right. They acknowledge abuse exists but frame it as implementation error, not structural defect. Their litigation strategy defends the right's expansive scope, which in this reading entrenches the suppression mechanism.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, privacy_fundamental_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides individuals a legal lever to remove outdated, irrelevant, or excessive personal data from search indexes and platforms — a genuine coordination problem in an era of permanent, searchable digital records.
% TRANSFER_FUNCTION: Moves control over public-index visibility from publishers/archivists to requesters, mediated by platforms. The transfer is asymmetric: requesters gain unilateral removal power; speakers lose audience access without notice or hearing; platforms gain safe harbor by over-complying.
% ABSENT_VOICES: The public interest is structurally excluded — no intervenor status, no notice, no appeal. Historical record keepers (archivists, libraries) have no statutory role. Future readers who would have found the information are never represented. Rival platforms that might offer different balancing are locked out by the same compliance moat.
% DISAPPEARANCE_RATIONALE: If Article 17 and its enforcement machinery vanished overnight, search engines would restore delisted links within weeks, archival gaps would be identified and backfilled, and the strategic erasure industry (reputation management firms, specialized law firms) would collapse. The information environment would revert to pre-2014 norms where publication created a durable public record subject to defamation law, not administrative erasure.
% FOUNDING_PROBLEM: Pre-GDPR, individuals had no practical remedy when stale, inaccurate, or disproportionate personal data dominated search results for their name — a 'digital tattoo' that could not be removed even when the underlying justification (e.g., spent conviction, resolved bankruptcy, juvenile error) had expired.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the original GDPR legislative history (Recital 65, Article 17 text) and early CJEU reasoning (Google Spain). However, the *current* scope of the problem is contested: privacy fundamentalists (EDRi, NOYB) attest it remains live and expanding; free expression advocates (Article 19, Index on Censorship, Reporters Without Borders) and competition economists attest the original problem has been substantially solved for genuine privacy cases and the mechanism now primarily serves suppression. No independent corroboration exists from a seat that neither benefits from the right nor bears its suppression costs.
narrative_ontology:disappearance_verdict(article17_erasure_right__censorship_mechanism_reading, world_rearranges).
narrative_ontology:founding_problem_status(article17_erasure_right__censorship_mechanism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__censorship_mechanism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article17_erasure_right__censorship_mechanism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article17_erasure_right__censorship_mechanism_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.78) is high because the transfer of indexing control is large, unilateral, and decoupled from any verification of privacy harm. Suppression (0.82) is very high because the mechanism's persistence depends on active enforcement: platforms face 4% global revenue fines for non-compliance; requesters face zero penalty for abuse; no adversarial process tests the privacy claim. Theater ratio (0.42) is moderate: genuine privacy cases exist (revenge porn, spent convictions, data breaches) but a growing share of requests target public-interest journalism (political corruption, corporate malfeasance, professional misconduct). Accessibility collapse (0.76) is high because delisting from Google/Bing effectively erases content for 90%+ of users; the content may persist on source sites but becomes unfinding. Resistance (0.58) is moderate: some journalists contest via transparency reporting and litigation (e.g., NT1/NT2 in UK), but most lack standing and resources.
 *
 * PERSPECTIVAL GAP:
 *   From the requester/platform/DPA seat, the constraint appears as genuine coordination solving a real privacy problem — the 'digital tattoo' the law was built for. From the journalist/archivist/public seat, the same structure operates as enforced extraction: a prior restraint without judicial review, notice, or appeal. The engine computes this divergence from the declared power/exit/role structure; the claimed_type (tangled_rope) captures the dual nature without resolving it.
 *
 * DIRECTIONALITY LOGIC:
 *   Bad-faith requesters are structural beneficiaries (d near 0.0): they collect the suppression benefit at near-zero cost, with arbitrage-grade exit (forum-shopping, specialized firms). Platforms are agenda_setters with secondary beneficiary position (d ~0.2): they administer the mechanism, gain safe harbor, and build compliance moats, but bear infrastructure costs. Journalists and archivists are payers (d near 1.0): they bear the full cost of suppression with constrained exit (cannot reach audience if delisted; cannot preserve what is delisted). Public_interest is excluded (d ~0.8): trapped in an information environment shaped by others' erasure choices. DPAs are agenda_setters (d ~0.3): they enforce but their institutional incentives align with expansion not restraint. Privacy advocates are observers (d ~0.5): they see the full structure but their advocacy reinforces the mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stale digital records harming individuals) is real but substantially solved for its original scope. The mechanism persists and expands because: (1) platforms' over-compliance is rational under asymmetric liability; (2) a repression industry (reputation management firms) lobbies for expansion; (3) no constituency exists for the public interest in erasure proceedings; (4) DPAs' enforcement incentives favor visibility of action (fines for non-erasure) over accuracy. This is mandatrophy: the mandate (privacy protection) has outlived its primary function, but the constraint persists through institutional inertia and the concentrated benefits it now delivers to bad-faith requesters.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'How does the censorship_mechanism_reading''s structural profile (beneficiaries, victims, extraction level) differ from the sibling readings of the same kernel, and what does this imply about the kernel''s stability?',
    'Compare the three readings'' ε values, beneficiary/victim structures, and claimed_types. If they diverge widely, the kernel label ''Article 17'' conflates multiple constraints; if they converge, the disagreement is evaluative not structural.',
    'If structural divergence is confirmed, the kernel must be decomposed into separate constraint stories per ε-invariance principle. The censorship_mechanism_reading would retain its high extraction/tangled_rope profile; the privacy_fundamental_reading would show low extraction/rope; the competitive_moat_reading would show platform-beneficiary extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Whether the Article 17 kernel conflates multiple constraints with different ε values.').

omega_variable(
    coordination_extraction_boundary,
    'Is the privacy coordination function (removing genuinely harmful stale data) structurally separable from the suppression function (removing public-interest journalism), or are they inseparable in the current mechanism design?',
    'Natural experiment from jurisdictions with narrower erasure scopes (e.g., US no-federal-right, or GDPR Art 17(3) exemptions for journalism). If genuine privacy protection persists without the suppression function, they are separable.',
    'If separable, the measured extraction is avoidable overhead — the constraint could be a rope with surgical redesign. If inseparable, the suppression is the price of the coordination, making tangled_rope the honest classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the constraint''s coordination and extraction components are structurally separable.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal liability, fines, technical delisting) or internalized (journalists self-censor anticipating erasure; archives pre-emptively omit erasure-vulnerable material)?',
    'Post-erasure behavior study: if journalists/archivists change coverage/preservation practices *before* any request arrives, internalized suppression is operating. Survey and chilling-effect measurement.',
    'If internalized suppression is significant, the effective suppression exceeds the structural measure — the constraint extracts compliance before enforcement. This would increase the effective extraction for payer seats beyond the base ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the censorship function.').

omega_variable(
    platform_incentive_alignment,
    'Do platforms'' compliance incentives (safe harbor, cost avoidance) structurally align them with requesters against speakers, or is over-compliance a rational error that would correct with liability reform?',
    'Counterfactual: if platforms faced symmetric liability (fines for wrongful erasure matching fines for non-erasure), would delisting rates drop? Compare jurisdictions with different liability regimes.',
    'If platforms are structural beneficiaries of the suppression function (via moat/competitive advantage), the constraint is a three-party extraction (requesters + platforms vs. speakers/public). If over-compliance is error, reform could reduce extraction without removing coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(platform_incentive_alignment, conceptual, 'Whether platform over-compliance is structural feature or correctable error.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__censorship_mechanism_reading, 2014, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(article17_censorship_tr_t2014, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 2014, 0.15).
narrative_ontology:measurement(article17_censorship_tr_t2016, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 2016, 0.22).
narrative_ontology:measurement(article17_censorship_tr_t2018, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 2018, 0.3).
narrative_ontology:measurement(article17_censorship_tr_t2020, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 2020, 0.36).
narrative_ontology:measurement(article17_censorship_tr_t2022, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 2022, 0.4).
narrative_ontology:measurement(article17_censorship_tr_t2024, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(article17_censorship_be_t2014, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 2014, 0.35).
narrative_ontology:measurement(article17_censorship_be_t2016, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 2016, 0.45).
narrative_ontology:measurement(article17_censorship_be_t2018, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 2018, 0.58).
narrative_ontology:measurement(article17_censorship_be_t2020, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement(article17_censorship_be_t2022, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 2022, 0.74).
narrative_ontology:measurement(article17_censorship_be_t2024, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(article17_censorship_su_t2014, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 2014, 0.55).
narrative_ontology:measurement(article17_censorship_su_t2016, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 2016, 0.62).
narrative_ontology:measurement(article17_censorship_su_t2018, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 2018, 0.7).
narrative_ontology:measurement(article17_censorship_su_t2020, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 2020, 0.76).
narrative_ontology:measurement(article17_censorship_su_t2022, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 2022, 0.8).
narrative_ontology:measurement(article17_censorship_su_t2024, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 2024, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__censorship_mechanism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article17_erasure_right__censorship_mechanism_reading, article17_erasure_right__privacy_fundamental_reading).
narrative_ontology:affects_constraint(article17_erasure_right__censorship_mechanism_reading, article17_erasure_right__competitive_moat_reading).
narrative_ontology:affects_constraint(article17_erasure_right__censorship_mechanism_reading, platform_liability_regime).
narrative_ontology:affects_constraint(article17_erasure_right__censorship_mechanism_reading, search_engine_transparency_obligation).

% DUAL FORMULATION NOTE:
% Article 17 kernel decomposes into three readings with divergent ε: censorship_mechanism_reading (ε≈0.78, tangled_rope), privacy_fundamental_reading (ε≈0.15, rope), competitive_moat_reading (ε≈0.65, snare). The censorship reading identifies requester-speaker extraction; the privacy reading sees coordination; the moat reading sees platform-entrant extraction. All three share the same legal text but instantiate different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article17_erasure_right__censorship_mechanism_reading, institutional, 0.25).
constraint_indexing:directionality_override(article17_erasure_right__censorship_mechanism_reading, powerful, 0.1).
constraint_indexing:directionality_override(article17_erasure_right__censorship_mechanism_reading, moderate, 0.85).
constraint_indexing:directionality_override(article17_erasure_right__censorship_mechanism_reading, powerless, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
