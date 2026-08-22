% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__censorship_mechanism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-25
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Article 17 Erasure Right as Strategic Censorship Mechanism
 *   domain: technology_governance/data_protection_law/competition_policy
 *
 * SUMMARY:
 *   Article 17 GDPR ('right to erasure' / 'right to be forgotten') creates a
 *   legal mechanism for individuals to demand removal of personal data. This
 *   reading treats the constraint as a censorship mechanism: the erasure
 *   right is weaponized by bad-faith actors (politicians, criminals,
 *   corporations, reputation-management firms) to suppress legitimate
 *   public-interest speech. Platforms, facing asymmetric penalties (heavy
 *   fines for non-compliance, no penalty for over-removal), systematically
 *   over-comply. The coordination function (genuine privacy redress) is real
 *   but increasingly captured; the transfer function moves deletion power
 *   from the public record to private requesters. The constraint is a tangled
 *   rope: it solves a real coordination problem (genuine privacy harms) while
 *   extracting asymmetrically (speech suppression for private benefit).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__censorship_mechanism_reading, 0.68).
domain_priors:suppression_score(article17_erasure_right__censorship_mechanism_reading, 0.75).
domain_priors:theater_ratio(article17_erasure_right__censorship_mechanism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(article17_erasure_right__censorship_mechanism_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__censorship_mechanism_reading, tangled_rope).
narrative_ontology:human_readable(article17_erasure_right__censorship_mechanism_reading, "Article 17 Erasure Right as Strategic Censorship Mechanism").
narrative_ontology:topic_domain(article17_erasure_right__censorship_mechanism_reading, "technology_governance/data_protection_law/competition_policy").

domain_priors:requires_active_enforcement(article17_erasure_right__censorship_mechanism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__censorship_mechanism_reading, 'c5bb94a6-bf33-4a2a-be9f-74f63737944b').
narrative_ontology:cs_kernel_codification('c5bb94a6-bf33-4a2a-be9f-74f63737944b', formalized).
narrative_ontology:cs_authority_grounding('c5bb94a6-bf33-4a2a-be9f-74f63737944b', extraction).
narrative_ontology:cs_interpretation_layer_present('c5bb94a6-bf33-4a2a-be9f-74f63737944b').
narrative_ontology:cs_reading_relation('c5bb94a6-bf33-4a2a-be9f-74f63737944b', article17_erasure_right__privacy_fundamental_reading, coexists_with).
narrative_ontology:cs_reading_relation('c5bb94a6-bf33-4a2a-be9f-74f63737944b', article17_erasure_right__competitive_moat_reading, influences).
narrative_ontology:cs_axiom('c5bb94a6-bf33-4a2a-be9f-74f63737944b', foundational, erasure_requests_function_as_prior_restraint).
narrative_ontology:cs_axiom_status(erasure_requests_function_as_prior_restraint, holdable).
narrative_ontology:cs_axiom_grounding('c5bb94a6-bf33-4a2a-be9f-74f63737944b', erasure_requests_function_as_prior_restraint, empirically_contingent).
narrative_ontology:cs_axiom('c5bb94a6-bf33-4a2a-be9f-74f63737944b', foundational, bad_faith_requesters_are_primary_beneficiaries).
narrative_ontology:cs_axiom_status(bad_faith_requesters_are_primary_beneficiaries, holdable).
narrative_ontology:cs_axiom_grounding('c5bb94a6-bf33-4a2a-be9f-74f63737944b', bad_faith_requesters_are_primary_beneficiaries, empirically_contingent).
narrative_ontology:cs_reference_frame('c5bb94a6-bf33-4a2a-be9f-74f63737944b', gdpr_article17_as_privacy_redress_mechanism).
narrative_ontology:cs_drift_state('c5bb94a6-bf33-4a2a-be9f-74f63737944b', post_google_spain_implementation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c5bb94a6-bf33-4a2a-be9f-74f63737944b', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__censorship_mechanism_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, bad_faith_requesters).
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, reputation_management_firms).
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, political_actors).
narrative_ontology:constraint_beneficiary(article17_erasure_right__censorship_mechanism_reading, platform_moderation_teams).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, investigative_journalists).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, digital_archivists).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, public_interest_researchers).
narrative_ontology:constraint_victim(article17_erasure_right__censorship_mechanism_reading, historical_record_integrity).
narrative_ontology:constraint_vindicates(article17_erasure_right__censorship_mechanism_reading, erasure_as_prior_restraint_substitute).
narrative_ontology:constraint_vindicates(article17_erasure_right__censorship_mechanism_reading, privacy_rights_weaponized_against_speech).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals and entities (politicians, convicted criminals, disgraced executives, corporations with adverse rulings) who file erasure requests not to protect genuine privacy but to remove damaging public-interest information. They benefit from the asymmetry: filing is low-cost and automated; contesting requires legal resources and editorial judgment. They can forum-shop across EU member states and exploit platform over-compliance.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, bad_faith_requesters, beneficiary,
    moderate, biographical, arbitrage, continental).

% Commercial intermediaries that automate and scale erasure requests for clients. They have developed workflows to mass-file requests across platforms, exploit procedural gaps, and pressure platforms into pre-emptive removal. They capture a significant share of the economic value extracted from the constraint's operation.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, reputation_management_firms, beneficiary,
    organized, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__censorship_mechanism_reading, reputation_management_firms, agenda_setter).

% Office-holders, candidates, and state-adjacent entities who use erasure requests to scrub inconvenient records (past statements, policy positions, associations, corruption allegations) from search results and platform archives. They benefit from state-adjacent leverage over data protection authorities and platforms.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, political_actors, beneficiary,
    powerful, biographical, arbitrage, national).

% The trust-and-safety and legal teams at major platforms (Google, Meta, Microsoft) who administer the erasure process. They set the procedural standards, build the tooling, and make the initial accept/reject decisions. They benefit from the constraint because it legitimizes their gatekeeper role, creates a compliance moat against smaller rivals, and generates a measurable 'safety' metric for regulators — but they also bear the operational cost and reputational risk of over-removal.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, platform_moderation_teams, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__censorship_mechanism_reading, platform_moderation_teams, beneficiary).

% Reporters and newsrooms whose published work disappears from search indexes and platform archives due to erasure requests targeting subjects of investigations. They lose audience reach, evidentiary chains, and the public record of their reporting. Contesting requests requires legal budget most outlets lack; the 'right to be forgotten' framework gives subjects a veto over journalism without judicial oversight.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, investigative_journalists, payer,
    moderate, biographical, constrained, continental).

% Institutions (Internet Archive, national libraries, academic repositories) tasked with preserving the digital historical record. Erasure requests create holes in the archive that cannot be filled — the constraint forces them to choose between legal compliance and their preservation mandate. No appeals mechanism exists for the archival interest.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, digital_archivists, payer,
    moderate, generational, constrained, global).

% Academics, NGOs, and watchdog groups studying corruption, disinformation, corporate malfeasance, and state abuse. Their datasets develop gaps when subjects erase source material. They have no standing in the erasure process and no practical exit from the platforms where data lives.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, public_interest_researchers, payer,
    powerless, biographical, constrained, continental).

% The abstract collective interest in an unaltered, contestable public record. Not a legal person and thus has no standing in any erasure proceeding. Its degradation is diffuse, irreversible, and uncompensated.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, historical_record_integrity, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(article17_erasure_right__censorship_mechanism_reading, historical_record_integrity).

% National regulators (CNIL, ICO, Hamburg Commissioner, etc.) who oversee GDPR enforcement. They issue guidelines, rule on complaints, and fine platforms. Their institutional incentive is to demonstrate 'strong enforcement' of privacy rights, which aligns with broad erasure compliance — they have little structural incentive to police bad-faith requests or defend the public record.
narrative_ontology:constraint_stakeholder(article17_erasure_right__censorship_mechanism_reading, data_protection_authorities, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, enforceable mechanism for individuals to request removal of personal data that is inaccurate, excessive, or no longer relevant — solving the coordination problem of distributed data control across platforms and jurisdictions.
% TRANSFER_FUNCTION: Moves control over public-interest information from publishers/archivists/platforms to request subjects (and their commercial intermediaries), transferring the power to delete from the historical record without judicial review. The extraction is the suppression of legitimate speech and the privatization of the public record.
% ABSENT_VOICES: The historical record itself, future generations who inherit a redacted past, and the subjects of investigative reporting who would oppose erasure but are never notified. Data protection authorities operate as observers but their institutional incentives align with the erasure regime, not the public record.
% DISAPPEARANCE_RATIONALE: If Article 17 erasure requests vanished overnight, reputation management firms would lose their core product, political actors would lose a tool for narrative control, platforms would shed a massive compliance burden, and journalists/archivists would regain stable links and indexes. The information ecosystem would reorganize around publication-time accountability rather than post-hoc erasure.
% FOUNDING_PROBLEM: Pre-GDPR, individuals had no effective remedy when search engines and platforms indefinitely surfaced outdated, inaccurate, or disproportionate personal data — a 'digital tattoo' that could not be removed even when the underlying justification (e.g., a spent conviction, a resolved debt, a minor's mistake) had expired.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by data protection authorities and privacy NGOs (live). The censorship_mechanism_reading holds it is substantially solved for genuine privacy cases but the mechanism has been captured for suppression — attested by journalists' associations (IFJ, EJO), archivist groups (IIPC, Internet Archive), and academic researchers (Oxford Internet Institute, Berkman Klein Center) from outside the beneficiary set. The privacy_fundamental_reading disputes the capture claim; the competitive_moat_reading treats the founding problem as pretext.
narrative_ontology:disappearance_verdict(article17_erasure_right__censorship_mechanism_reading, world_rearranges).
narrative_ontology:founding_problem_status(article17_erasure_right__censorship_mechanism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__censorship_mechanism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(article17_erasure_right__censorship_mechanism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article17_erasure_right__censorship_mechanism_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extraction (0.68) is high because the constraint transfers control over public-interest information to private actors without compensation or oversight. Suppression (0.75) is higher because the mechanism operates as prior restraint: removal happens before any adjudication, and restoration is practically impossible. Theater ratio (0.42) is rising — platforms perform 'balancing tests' that are structurally biased toward removal. Accessibility collapse (0.62) reflects that once content is delisted, it is effectively gone for most users; alternatives (direct URLs, archives) are fragile. Resistance (0.58) is moderate — journalists and archivists push back but lack structural leverage.
 *
 * PERSPECTIVAL GAP:
 *   From the platform moderation seat, the constraint is a genuine coordination problem they are resourced to solve (rope-like). From the journalist/archivist seat, it is an extraction mechanism with no due process (snare-like). From the bad-faith requester seat, it is a low-cost censorship tool (pure beneficiary). The engine computes this divergence from the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Bad-faith requesters, reputation firms, and political actors are structural beneficiaries (d ≈ 0.15-0.25): they initiate the constraint's operation and capture its gains. Platform moderation teams are agenda-setters with dual position (d ≈ 0.4): they administer the constraint and benefit from the compliance moat, but bear operational costs. Journalists, archivists, and researchers are payers (d ≈ 0.75-0.85): they bear the suppression cost with constrained exit. Historical record integrity is excluded (no standing). DPAs are observers (d ≈ 0.3): they oversee but their incentives align with erasure expansion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (genuine privacy harms from perpetual digital records) is real but the mechanism has drifted: the coordination function is being captured by the extraction function. The constraint is not a pure snare (genuine privacy cases exist) nor a pure rope (asymmetric extraction is structural). Mandatrophy is unresolved: the arrangement persists because the privacy framing provides cover for the suppression function, and no institutional actor has incentive to separate them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capture_vs_design,
    'Is the suppression function an emergent capture of a well-designed privacy mechanism, or was the mechanism structurally vulnerable to capture by design?',
    'Legislative history analysis of GDPR negotiations (Recital 65, Article 17 drafting); comparison with pre-GDPR right-to-be-forgotten jurisprudence (Google Spain); counterfactual design analysis of alternative mechanisms (judicial oversight, notice-and-contest, public-interest carve-outs).',
    'If capture, reform could restore the coordination function; if structural vulnerability, the mechanism itself is the problem and requires replacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_vs_design, conceptual, 'Whether the censorship function is bug or feature of Article 17''s design.').

omega_variable(
    platform_overcompliance_driver,
    'Is platform over-compliance driven by rational risk-aversion (asymmetric penalties), or does it reflect platforms'' own strategic interest in a compliance moat?',
    'Internal platform communications (discovery in litigation); comparison of erasure grant rates across platforms with different market positions; analysis of platform lobbying positions on Article 17 amendments.',
    'If rational risk-aversion, penalty symmetry reforms could reduce over-removal; if strategic moat, platforms will resist procedural safeguards that reduce their gatekeeper advantage.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(platform_overcompliance_driver, empirical, 'Whether platforms are victims or co-beneficiaries of the constraint''s suppression function.').

omega_variable(
    public_interest_carveout_viability,
    'Could a meaningful public-interest exception be operationalized without destroying the privacy right?',
    'Comparative analysis of jurisdictions with public-interest carve-outs (e.g., Canadian ''relevance'' test, Japanese ''public figure'' doctrine); technical feasibility of delayed erasure with judicial review; empirical study of what fraction of requests would be contested.',
    'If viable, the tangled rope could be restructured toward rope; if not, the constraint is structurally trapped in its current extraction profile.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_interest_carveout_viability, conceptual, 'Whether the coordination and extraction functions are structurally separable.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the ''right to erasure'' kernel admit a single coherent framing, or do the three readings reflect genuinely incompatible commitments that cannot be reconciled within one legal concept?',
    'Doctrinal analysis of CJEU jurisprudence (Google Spain, GC and Others, TU Berlin) — does the court treat the right as unitary or contextually fragmented? Political economy of the GDPR negotiation: was Article 17 a compromise that papered over fundamental disagreement?',
    'If framings are irreconcilable, the kernel is a site of permanent contestation — no single reading can claim descriptive adequacy. The constraint family structure (three linked stories) is the correct model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel itself is a stable concept or a contested composite.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__censorship_mechanism_reading, 2018, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(article17_censorship_tr_t2018, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 2018, 0.18).
narrative_ontology:measurement_basis(article17_censorship_tr_t2018, observed).
narrative_ontology:measurement(article17_censorship_tr_t2020, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 2020, 0.25).
narrative_ontology:measurement_basis(article17_censorship_tr_t2020, observed).
narrative_ontology:measurement(article17_censorship_tr_t2022, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 2022, 0.33).
narrative_ontology:measurement_basis(article17_censorship_tr_t2022, observed).
narrative_ontology:measurement(article17_censorship_tr_t2024, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 2024, 0.38).
narrative_ontology:measurement_basis(article17_censorship_tr_t2024, observed).
narrative_ontology:measurement(article17_censorship_tr_t2026, article17_erasure_right__censorship_mechanism_reading, theater_ratio, 2026, 0.42).
narrative_ontology:measurement_basis(article17_censorship_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(article17_censorship_be_t2018, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 2018, 0.35).
narrative_ontology:measurement_basis(article17_censorship_be_t2018, observed).
narrative_ontology:measurement(article17_censorship_be_t2020, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 2020, 0.48).
narrative_ontology:measurement_basis(article17_censorship_be_t2020, observed).
narrative_ontology:measurement(article17_censorship_be_t2022, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 2022, 0.58).
narrative_ontology:measurement_basis(article17_censorship_be_t2022, observed).
narrative_ontology:measurement(article17_censorship_be_t2024, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 2024, 0.64).
narrative_ontology:measurement_basis(article17_censorship_be_t2024, observed).
narrative_ontology:measurement(article17_censorship_be_t2026, article17_erasure_right__censorship_mechanism_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(article17_censorship_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(article17_censorship_su_t2018, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 2018, 0.45).
narrative_ontology:measurement_basis(article17_censorship_su_t2018, observed).
narrative_ontology:measurement(article17_censorship_su_t2020, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 2020, 0.58).
narrative_ontology:measurement_basis(article17_censorship_su_t2020, observed).
narrative_ontology:measurement(article17_censorship_su_t2022, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 2022, 0.65).
narrative_ontology:measurement_basis(article17_censorship_su_t2022, observed).
narrative_ontology:measurement(article17_censorship_su_t2024, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 2024, 0.71).
narrative_ontology:measurement_basis(article17_censorship_su_t2024, observed).
narrative_ontology:measurement(article17_censorship_su_t2026, article17_erasure_right__censorship_mechanism_reading, suppression_requirement, 2026, 0.75).
narrative_ontology:measurement_basis(article17_censorship_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__censorship_mechanism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article17_erasure_right__censorship_mechanism_reading, article17_erasure_right__privacy_fundamental_reading).
narrative_ontology:affects_constraint(article17_erasure_right__censorship_mechanism_reading, article17_erasure_right__competitive_moat_reading).
narrative_ontology:affects_constraint(article17_erasure_right__censorship_mechanism_reading, platform_content_moderation_regime).
narrative_ontology:affects_constraint(article17_erasure_right__censorship_mechanism_reading, search_engine_delisting_practices).
narrative_ontology:affects_constraint(article17_erasure_right__censorship_mechanism_reading, reputation_management_industry).

% DUAL FORMULATION NOTE:
% Article 17 erasure right decomposes into three structurally distinct constraints: (1) censorship_mechanism_reading — high extraction, journalists/archivists as victims, bad-faith requesters as beneficiaries; (2) privacy_fundamental_reading — low extraction, all data subjects as beneficiaries, coordination function dominant; (3) competitive_moat_reading — moderate extraction, platforms as beneficiaries, competitors as victims. They share the kernel (Article 17 text) but have different ε, different victim/beneficiary structures, and different types. Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article17_erasure_right__censorship_mechanism_reading, institutional, 0.38).
constraint_indexing:directionality_override(article17_erasure_right__censorship_mechanism_reading, powerful, 0.22).
constraint_indexing:directionality_override(article17_erasure_right__censorship_mechanism_reading, moderate, 0.8).
constraint_indexing:directionality_override(article17_erasure_right__censorship_mechanism_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
