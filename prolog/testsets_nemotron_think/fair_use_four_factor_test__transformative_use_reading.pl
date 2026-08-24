% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__transformative_use_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__transformative_use_reading, []).

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
 *   constraint_id: fair_use_four_factor_test__transformative_use_reading
 *   human_readable: Transformative Use Dominance in Fair Use Four-Factor Test
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   The transformative use reading of the fair use four-factor test (17 USC
 *   107) holds that when a use adds new expression, meaning, or message —
 *   when it is 'transformative' — the first factor (purpose and character)
 *   dominates the analysis and the fourth factor (market harm) is
 *   subordinated. This reading emerged from Campbell v. Acuff-Rose (1994) and
 *   was reinforced in Google v. Oracle (2021). It coordinates cultural
 *   production by giving legal space to remix, parody, criticism, and new
 *   expressive works, but it extracts licensing revenue and control from
 *   copyright holders whose works are transformed. The constraint is actively
 *   enforced by courts applying the test. Beneficiaries include tech
 *   platforms hosting UGC, remix artists, and everyday creators. Victims are
 *   copyright holders (especially visual artists, musicians, writers, and
 *   small rightsholders) whose works are used without permission or payment
 *   when deemed transformative. The victim set shifts with the transformation
 *   threshold — as 'transformative' expands (e.g., to include search, data
 *   mining, AI training), more creators become payers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__transformative_use_reading, 0.48).
domain_priors:suppression_score(fair_use_four_factor_test__transformative_use_reading, 0.35).
domain_priors:theater_ratio(fair_use_four_factor_test__transformative_use_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__transformative_use_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__transformative_use_reading, "Transformative Use Dominance in Fair Use Four-Factor Test").
narrative_ontology:topic_domain(fair_use_four_factor_test__transformative_use_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__transformative_use_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__transformative_use_reading, '0aea8a83-8540-4698-af55-41273219ec94').
narrative_ontology:cs_kernel_codification('0aea8a83-8540-4698-af55-41273219ec94', formalized).
narrative_ontology:cs_authority_grounding('0aea8a83-8540-4698-af55-41273219ec94', lineage).
narrative_ontology:cs_interpretation_layer_present('0aea8a83-8540-4698-af55-41273219ec94').
narrative_ontology:cs_reading_relation('0aea8a83-8540-4698-af55-41273219ec94', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('0aea8a83-8540-4698-af55-41273219ec94', fair_use_four_factor_test__user_centric_reading, coexists_with).
narrative_ontology:cs_axiom('0aea8a83-8540-4698-af55-41273219ec94', foundational, transformative_use_justifies_market_harm_subordination).
narrative_ontology:cs_axiom_status(transformative_use_justifies_market_harm_subordination, holdable).
narrative_ontology:cs_axiom_grounding('0aea8a83-8540-4698-af55-41273219ec94', transformative_use_justifies_market_harm_subordination, instrumental).
narrative_ontology:cs_axiom('0aea8a83-8540-4698-af55-41273219ec94', foundational, first_factor_dominates_when_transformative).
narrative_ontology:cs_axiom_status(first_factor_dominates_when_transformative, holdable).
narrative_ontology:cs_axiom_grounding('0aea8a83-8540-4698-af55-41273219ec94', first_factor_dominates_when_transformative, conventional).
narrative_ontology:cs_reference_frame('0aea8a83-8540-4698-af55-41273219ec94', campbell_transformative_use_framework).
narrative_ontology:cs_drift_state('0aea8a83-8540-4698-af55-41273219ec94', post_google_oracle_2021, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0aea8a83-8540-4698-af55-41273219ec94', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, tech_platforms_ugc).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, remix_artists).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, ugc_creators).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, educational_institutions).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, visual_artists).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, musicians_composers).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, writers_authors).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, photographers).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, small_rightsholders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, ugc_creators).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__transformative_use_reading, copyright_promotes_progress_clause).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__transformative_use_reading, transformative_use_as_fair_use_core).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set binding precedent on how the four factors are weighed. The transformative use reading became dominant after Campbell v. Acuff-Rose (1994) and Google v. Oracle (2021). Courts apply the test case-by-case but the transformative use framework structures the entire analysis.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, appellate_courts, agenda_setter,
    institutional, generational, analytical, national).

% Platforms (YouTube, TikTok, Instagram, GitHub) host billions of transformative works. They benefit from the legal safe harbor transformative use provides for UGC, avoiding licensing negotiations at scale. They capture advertising and data value from transformative works while bearing minimal liability.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, tech_platforms_ugc, beneficiary,
    institutional, generational, arbitrage, global).

% Artists who build on existing works (sampling, collage, parody, commentary, fan fiction). Transformative use doctrine lets them create without clearing rights, but the threshold is uncertain and litigation risk is high for those without platform backing.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, remix_artists, beneficiary,
    moderate, biographical, constrained, global).

% Everyday users making memes, reaction videos, commentary, covers. They benefit from transformative use protection but also pay through platform terms of service, content ID systems, and demonetization when claims arise. Their work is the raw material platforms monetize.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, ugc_creators, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__transformative_use_reading, ugc_creators, payer).

% Painters, illustrators, photographers whose works are incorporated into collages, memes, AI training data, or derivative works deemed transformative. They lose licensing revenue and control over adaptations. Small artists lack resources to litigate; even successful ones face uncertain boundaries.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, visual_artists, payer,
    moderate, biographical, constrained, national).

% Sampling and remix culture builds on musical works. When courts find transformative use (e.g., parody, critical commentary), licensing revenue is lost. The Bridgeport 'get a license' precedent for sound recordings creates a parallel regime where transformative use is narrower.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, musicians_composers, payer,
    moderate, biographical, constrained, national).

% Text-based transformative uses (fan fiction, parody, criticism, AI training) draw on literary works. Authors lose control over derivative markets. The Google Books and HathiTrust rulings expanded transformative use for search and access, reducing licensing leverage.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, writers_authors, payer,
    moderate, biographical, constrained, national).

% Independent creators without institutional backing. When their work is used transformatively, they have no practical ability to enforce rights or negotiate licenses. The transformative use doctrine effectively operates as a compulsory license without compensation for this group.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, small_rightsholders, payer,
    powerless, immediate, trapped, local).

% Analyze the doctrine's evolution, empirical effects, and theoretical coherence. Their work influences courts and policy but they do not directly benefit or pay. Divided between those seeing transformative use as copyright's safety valve and those seeing it as an expansion that swallows the exclusive rights.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, copyright_scholars, observer,
    analytical, civilizational, analytical, universal).

% Creators in jurisdictions without fair use (most of the world) or without resources to access the legal system. They would object to the global reach of US transformative use doctrine via platform terms of service, but have no voice in its development.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, individual_creators_excluded, excluded,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework that permits culturally valuable reuses (parody, criticism, commentary, new expression) without requiring permission, balancing the constitutional goal of promoting progress against creators' exclusive rights.
% TRANSFER_FUNCTION: Moves licensing revenue and control over derivative works from copyright holders to transformative users and the platforms hosting them, when a court finds the use adds new expression, meaning, or message. The transfer is probabilistic — it occurs only when a defendant successfully invokes the defense.
% ABSENT_VOICES: Individual creators in non-fair-use jurisdictions whose works are governed by platform terms of service importing US doctrine; creators without litigation resources who effectively face a compulsory license without compensation; future creators whose incentive structures are shaped by the doctrine but who cannot participate in its formation.
% DISAPPEARANCE_RATIONALE: If transformative use dominance vanished overnight, UGC platforms would face existential liability, remix culture would be legally precarious, educational and critical uses would require licensing, and the entire ecosystem of derivative cultural production would reorganize around permission-based models or collapse.
% FOUNDING_PROBLEM: How to allow socially valuable uses of copyrighted works (criticism, parody, education, new expression) without undermining the incentive structure that motivates creation in the first place.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court in Campbell v. Acuff-Rose (1994) and Google v. Oracle (2021) attests the problem is live. The Authors Guild, RIAA, and visual artists' organizations attest the problem is substantially solved and the doctrine now operates as rent extraction by platforms. Legislative history of the 1976 Act (House Report 94-1476) supports a narrower reading. Academic commentary (Litman, Tushnet, Samuelson, Ginsburg) from outside the platform beneficiary set documents the shift.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__transformative_use_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__transformative_use_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__transformative_use_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_four_factor_test__transformative_use_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__transformative_use_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__transformative_use_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_four_factor_test__transformative_use_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_four_factor_test__transformative_use_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) is moderate: the doctrine transfers real economic value (licensing markets for derivatives, synchronization, adaptation) but only probabilistically — not all transformative uses succeed, and many creators still license. Suppression (0.35) is moderate-low: the constraint does not prevent licensing (parties can still contract), but it suppresses the *leverage* to demand a license by providing a defense. Theater ratio (0.28) reflects that the four-factor test is genuinely applied case-by-case, but the transformative use framework has become a predictable template that channels litigation. Accessibility collapse (0.42) is moderate: alternatives (licensing, public domain, original creation) exist but are practically foreclosed for many transformative uses. Resistance (0.55) is significant: creator organizations litigate, lobby, and develop technical measures (Content ID) to counter the doctrine.
 *
 * PERSPECTIVAL GAP:
 *   From the platform/remix artist seat, this is a Rope: genuine coordination enabling massive cultural production with minimal coercion. From the small rightsholder seat, it is a Snare: extraction without consent, suppressed alternatives (no practical licensing market for transformative uses of their work), and trapped exit. From the appellate court seat, it is the intended balance — but the engine will compute the per-seat types from the structural data, not the authored claim. The claimed type (tangled_rope) asserts both coordination and extraction are structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   Appellate courts are the agenda setters (d near 0.5 — they administer the test but don't directly collect). Tech platforms are strong beneficiaries (d near 0.0 — they capture massive value from UGC enabled by the doctrine, with arbitrage-grade exit via global operations). Remix artists and UGC creators are moderate beneficiaries (d ~0.2-0.3 — they gain creative freedom but face uncertainty and platform dependence). Visual artists, musicians, writers, and small rightsholders are payers (d ~0.7-0.9 — they bear the extraction with constrained or trapped exit). Copyright scholars are analytical observers (d=0.5). Excluded individual creators in other jurisdictions are trapped (d=1.0 — subject to platform terms importing the doctrine with no voice).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (enabling valuable uses without destroying incentives) is contested: platforms and transformative users say it is live and the doctrine solves it; creator organizations say the problem has shifted — the doctrine now enables platform-scale extraction beyond the founding scope. The mandate has not atrophied (the coordination function is actively used), but the extraction dimension has grown. This is not a piton — the doctrine is actively maintained and litigated, not maintained theatrically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformation_threshold_ambiguity,
    'What counts as ''new expression, meaning, or message'' sufficient to trigger transformative use dominance? The threshold has expanded from parody/criticism to search, data mining, and AI training.',
    'Supreme Court guidance on the boundaries of transformative use, or congressional amendment clarifying the scope. Empirical study of lower court decisions mapping the expansion trajectory.',
    'If the threshold is narrow (parody/criticism only), extractiveness drops and the constraint approaches Rope. If broad (any productive use), extractiveness rises toward Snare for more creator categories. The victim set shifts directly with this boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transformation_threshold_ambiguity, conceptual, 'The boundary of ''transformative'' determines which creators become payers and how much extraction occurs.').

omega_variable(
    market_harm_measurement_in_transformative_context,
    'How should market harm be measured when the transformative use creates new markets (e.g., AI training data, search indexes) that the original creator could not have exploited?',
    'Economic analysis of whether transformative uses displace existing licensing markets or create entirely new value. Courts'' treatment of ''potential licensing markets'' for transformative uses.',
    'If market harm includes foreclosed licensing opportunities for transformative uses, extraction is higher. If limited to traditional derivative markets, extraction is lower. This directly affects the epsilon valuation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_harm_measurement_in_transformative_context, empirical, 'Whether market harm analysis captures value created by the transformative use itself.').

omega_variable(
    platform_capture_of_transformative_value,
    'Do tech platforms capture the majority of the economic value generated by transformative UGC, leaving both the transformative creator and the original rightsholder undercompensated?',
    'Platform revenue transparency, creator economy earnings studies, and analysis of Content ID / revenue sharing systems.',
    'If platforms capture the value, the true beneficiary is the platform (already modeled) but the transformative creator is also a payer (dual role). This would increase the constraint''s extractiveness and shift the beneficiary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_capture_of_transformative_value, empirical, 'Whether the beneficiary structure is bimodal (platforms + transformative creators) or concentrated in platforms.').

omega_variable(
    kernel_reading_framing_underdetermination,
    'Is the fair use four-factor test a single kernel with three readings, or are the four factors themselves the kernel with each reading constituting a different constraint? The statutory text is fixed but the weightings are mutually incompatible.',
    'Analyze whether the readings share a common referent (the statutory test) or whether each reading effectively rewrites the test. Check if any reading forecloses another within a single judicial opinion.',
    'If the readings are truly sibling constraints on one kernel, they coexist and the engine should model them as a family. If they are different constraints, the kernel frame is a category error and each should stand alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing_underdetermination, conceptual, 'Whether the committer frame (kernel + readings) correctly captures the structure of fair use doctrine or imposes a false unity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__transformative_use_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_use_transformative_tr_t1994, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 1994, 0.12).
narrative_ontology:measurement(fair_use_transformative_tr_t1999, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 1999, 0.15).
narrative_ontology:measurement(fair_use_transformative_tr_t2004, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2004, 0.18).
narrative_ontology:measurement(fair_use_transformative_tr_t2009, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2009, 0.22).
narrative_ontology:measurement(fair_use_transformative_tr_t2014, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2014, 0.25).
narrative_ontology:measurement(fair_use_transformative_tr_t2019, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2019, 0.27).
narrative_ontology:measurement(fair_use_transformative_tr_t2024, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(fair_use_transformative_be_t1994, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 1994, 0.22).
narrative_ontology:measurement(fair_use_transformative_be_t1999, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 1999, 0.28).
narrative_ontology:measurement(fair_use_transformative_be_t2004, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2004, 0.35).
narrative_ontology:measurement(fair_use_transformative_be_t2009, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2009, 0.41).
narrative_ontology:measurement(fair_use_transformative_be_t2014, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2014, 0.45).
narrative_ontology:measurement(fair_use_transformative_be_t2019, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2019, 0.47).
narrative_ontology:measurement(fair_use_transformative_be_t2024, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2024, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(fair_use_transformative_su_t1994, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 1994, 0.25).
narrative_ontology:measurement(fair_use_transformative_su_t1999, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 1999, 0.28).
narrative_ontology:measurement(fair_use_transformative_su_t2004, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2004, 0.3).
narrative_ontology:measurement(fair_use_transformative_su_t2009, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2009, 0.32).
narrative_ontology:measurement(fair_use_transformative_su_t2014, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2014, 0.34).
narrative_ontology:measurement(fair_use_transformative_su_t2019, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2019, 0.35).
narrative_ontology:measurement(fair_use_transformative_su_t2024, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__transformative_use_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_four_factor_test__transformative_use_reading, 0.1).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test__creator_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test__user_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, dmca_safe_harbor).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, copyright_term_extension).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, ai_training_data_fair_use).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the fair_use_four_factor_test kernel. The creator_centric_reading and user_centric_reading are sibling constraints. All three share the statutory four-factor test as kernel but instantiate different constraints with different epsilon, beneficiary/victim structures, and claimed types. The transformative_use_reading has moderate epsilon (0.48) with platforms and remix artists as beneficiaries; creator_centric_reading would have lower epsilon with creators as beneficiaries; user_centric_reading would have lower epsilon with the public as beneficiaries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_four_factor_test__transformative_use_reading, institutional, 0.1).
constraint_indexing:directionality_override(fair_use_four_factor_test__transformative_use_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
