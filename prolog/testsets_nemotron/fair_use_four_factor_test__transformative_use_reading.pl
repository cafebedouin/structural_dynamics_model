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
 *   human_readable: Transformative Use Dominance in Fair Use Four-Factor Balancing
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   The transformative use reading of fair use (originating in Judge Leval's
 *   1990 article and cemented in Campbell v. Acuff-Rose, 510 U.S. 569 (1994))
 *   restructures the statutory four-factor test by making
 *   'transformativeness' — whether the new work adds new expression, meaning,
 *   or message — the dominant factor that subordinates market harm analysis.
 *   This reading has expanded from parody/commentary to encompass search
 *   engines, text mining, appropriation art, memes, and UGC. The constraint
 *   is claimed as tangled_rope because it genuinely coordinates follow-on
 *   creativity (a coordination function) while extracting licensing value
 *   from rightsholders (asymmetric extraction) and requiring active judicial
 *   enforcement to maintain the transformation threshold.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__transformative_use_reading, 0.42).
domain_priors:suppression_score(fair_use_four_factor_test__transformative_use_reading, 0.38).
domain_priors:theater_ratio(fair_use_four_factor_test__transformative_use_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__transformative_use_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__transformative_use_reading, "Transformative Use Dominance in Fair Use Four-Factor Balancing").
narrative_ontology:topic_domain(fair_use_four_factor_test__transformative_use_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__transformative_use_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__transformative_use_reading, 'f40ac1d6-ef83-4b9a-aaac-11aa26c946c9').
narrative_ontology:cs_kernel_codification('f40ac1d6-ef83-4b9a-aaac-11aa26c946c9', fixed_text).
narrative_ontology:cs_authority_grounding('f40ac1d6-ef83-4b9a-aaac-11aa26c946c9', lineage).
narrative_ontology:cs_interpretation_layer_present('f40ac1d6-ef83-4b9a-aaac-11aa26c946c9').
narrative_ontology:cs_reading_relation('f40ac1d6-ef83-4b9a-aaac-11aa26c946c9', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('f40ac1d6-ef83-4b9a-aaac-11aa26c946c9', fair_use_four_factor_test__user_centric_reading, coexists_with).
narrative_ontology:cs_axiom('f40ac1d6-ef83-4b9a-aaac-11aa26c946c9', foundational, transformativeness_as_organizing_principle).
narrative_ontology:cs_axiom_status(transformativeness_as_organizing_principle, holdable).
narrative_ontology:cs_axiom_grounding('f40ac1d6-ef83-4b9a-aaac-11aa26c946c9', transformativeness_as_organizing_principle, conventional).
narrative_ontology:cs_axiom('f40ac1d6-ef83-4b9a-aaac-11aa26c946c9', foundational, market_harm_subordinated_to_new_meaning).
narrative_ontology:cs_axiom_status(market_harm_subordinated_to_new_meaning, holdable).
narrative_ontology:cs_axiom_grounding('f40ac1d6-ef83-4b9a-aaac-11aa26c946c9', market_harm_subordinated_to_new_meaning, conventional).
narrative_ontology:cs_reference_frame('f40ac1d6-ef83-4b9a-aaac-11aa26c946c9', leval_campbell_framework).
narrative_ontology:cs_drift_state('f40ac1d6-ef83-4b9a-aaac-11aa26c946c9', contemporary_ai_ugc_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f40ac1d6-ef83-4b9a-aaac-11aa26c946c9', '2026-08-15T14:32:17Z').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, remix_culture_practitioners).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, ugc_platforms).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, educational_institutions).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, documentary_filmmakers).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, news_organizations).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, commercial_rightsholders).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, licensing_revenue_dependents).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, creative_industries_reliant_on_sync_fees).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__transformative_use_reading, transformative_use_doctrine).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__transformative_use_reading, cultural_production_as_public_good).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__transformative_use_reading, first_amendment_via_fair_use).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Create transformative works (memes, mashups, commentary, parody) that rely on the transformative use reading to operate without licensing. Their creative practice is legally precarious but enabled by this reading. Exit means abandoning their medium or moving to jurisdictions with stronger user rights.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, remix_culture_practitioners, beneficiary,
    moderate, biographical, constrained, global).

% Host user-generated content at scale; the transformative use reading provides legal cover for algorithmic hosting and monetization of transformative works. They shape the constraint through Content ID systems, takedown processes, and lobbying. Can jurisdictional arbitrage across legal regimes.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, ugc_platforms, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__transformative_use_reading, ugc_platforms, agenda_setter).

% Lose licensing revenue when transformative uses substitute for licensed derivatives (sync, adaptation, sampling). Enforce through DMCA takedowns, Content ID claims, and litigation. Their exit is limited: they cannot leave the copyright system, but can lobby for legislative narrowing of transformative use.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, commercial_rightsholders, payer,
    powerful, biographical, constrained, global).

% Music publishers, stock footage libraries, collective management organizations whose revenue models depend on licensing derivatives. Transformative use reading erodes their market. Exit means business model pivot or consolidation.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, licensing_revenue_dependents, payer,
    moderate, biographical, constrained, national).

% Film/TV production, advertising, video game studios that license music and footage. Transformative uses (especially in UGC) compete with licensed sync placements. They participate in industry lobbying and private ordering (Content ID deals).
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, creative_industries_reliant_on_sync_fees, payer,
    organized, biographical, constrained, national).

% Rely on transformative use for teaching, research, digital humanities, and student media. Have institutional legal counsel and policy advocacy capacity. Can often negotiate licenses when transformative use is uncertain.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, educational_institutions, beneficiary,
    organized, generational, mobile, national).

% Depend on transformative use for incidental capture, archival footage, and cultural commentary. Often cannot afford licensing. Their exit is constrained by the necessity of depicting reality.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, documentary_filmmakers, beneficiary,
    moderate, biographical, constrained, global).

% Use transformative use for reporting, commentary, and critique. Have institutional legal resources and First Amendment alignment. Can often absorb licensing costs for high-stakes uses.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, news_organizations, beneficiary,
    institutional, generational, mobile, global).

% Adjudicate the four-factor test case by case; their rulings instantiate and evolve the transformative use reading. They are the authoritative interpreters of the kernel. Exit is analytical: they cannot leave the duty to decide.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, courts_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Administer the copyright system, issue rulemakings (e.g., DMCA 1201 exemptions), and advise Congress. Their structural position shapes the constraint's enforcement boundary. Exit is analytical.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, copyright_office_policymakers, agenda_setter,
    institutional, generational, analytical, national).

% Produce the doctrinal frameworks (Leval, Nimmer, Samuelson, etc.) that courts cite. They do not enforce or pay but shape the intellectual infrastructure of the reading.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, legal_scholarship, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a predictable, low-transaction-cost pathway for follow-on creators to build on existing works when the new work adds new expression, meaning, or message — coordinating cultural production without requiring permission from every upstream rightsholder.
% TRANSFER_FUNCTION: Transfers control over derivative markets from rightsholders to follow-on creators and platforms: the right to authorize (and collect fees for) transformative uses moves from the commercial licensing ecosystem to the user/platform ecosystem. The transfer is partial and context-dependent — only uses meeting the transformation threshold.
% ABSENT_VOICES: Individual creators without institutional backing who cannot afford litigation to establish their transformative use rights; future creators whose works do not yet exist; audiences in jurisdictions without fair use (civil law systems) who have no equivalent statutory right; small rightsholders who lack resources to monitor and enforce against transformative uses that cross into substitution.
% DISAPPEARANCE_RATIONALE: If the transformative use reading vanished overnight, UGC platforms would face existential liability, remix culture would be driven underground or offshore, documentary and news production would face prohibitive licensing costs, and the licensing industry would capture all derivative markets — the entire ecology of follow-on cultural production would reorganize around permission and payment.
% FOUNDING_PROBLEM: The four-factor test as originally codified (1976 Act) provided no hierarchy among factors, creating unpredictability for follow-on creators and chilling transformative uses that the First Amendment and cultural progress clause protect. The transformative use reading (Leval 1990, Campbell v. Acuff-Rose 1994) was built to solve this by centering 'new meaning' as the organizing principle.
% FOUNDING_PROBLEM_CORROBORATION: Leval's 1990 article and Campbell v. Acuff-Rose are cited by courts across circuits as the doctrinal foundation. However, rightsholder groups (RIAA, MPAA, Authors Guild) contest that the founding problem was ever 'chill' — they argue the problem was unauthorized commercial exploitation, and the transformative use reading has expanded beyond its original scope. Legal scholarship outside the benefiting parties (e.g., Ginsburg, Goldstein) corroborates the expansion critique.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__transformative_use_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__transformative_use_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__transformative_use_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(fair_use_four_factor_test__transformative_use_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__transformative_use_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__transformative_use_reading_tests).
:- end_tests(fair_use_four_factor_test__transformative_use_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.42) reflects the partial transfer of derivative market control from rightsholders to users/platforms — substantial but not total, as commercial rightsholders retain control over non-transformative uses and many licensing markets. Suppression (0.38) is moderate: the constraint operates through litigation risk and takedown systems rather than direct prohibition; alternatives (licensing, public domain, original creation) exist but are costly. Theater ratio (0.31) captures the growing gap between the doctrinal rhetoric (transformativeness as First Amendment safeguard) and the operational reality (platform-scale algorithmic hosting monetizing borderline-transformative content). Accessibility collapse (0.45) and resistance (0.58) reflect that alternatives persist (licensing, original creation) and rightsholders actively litigate and lobby to narrow the reading.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (courts), the constraint appears as a principled doctrinal hierarchy implementing constitutional values. From the payer seats (rightsholders), it appears as judicial legislation transferring property value. From the beneficiary seats (creators, platforms), it appears as essential breathing room for culture. The engine computes these divergences from the structural data; the authored claim (tangled_rope) captures the dual coordination/extraction structure without resolving the perspectival conflict.
 *
 * DIRECTIONALITY LOGIC:
 *   UGU platforms and remix practitioners are structural beneficiaries (d near 0.1-0.2): they collect the value of follow-on creation enabled by the reading. Commercial rightsholders and licensing dependents are structural targets (d near 0.7-0.85): they bear the extraction through lost licensing revenue and enforcement costs. Courts and policymakers are agenda_setters with analytical exit (d near 0.5): they administer the constraint but do not directly collect or pay. Educational institutions, documentarians, and news orgs are beneficiaries with varying exit (d near 0.2-0.4). Legal scholarship is an analytical observer (d = 0.5). The victim set shifts with the transformation threshold — as courts expand 'transformative' to include data mining, AI training, etc., new rightsholder classes become victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unpredictability/chill) was live in 1990. Whether it remains live is contested: rightsholders argue the reading has metastasized beyond its justification; beneficiaries argue new technologies (AI, algorithmic curation) create new chill. The mandatrophy question is whether the transformative use reading still solves its original coordination problem or has become a vehicle for platform-scale extraction. The theater_ratio trajectory (0.08→0.31) suggests growing performative maintenance of the 'First Amendment' framing while the operational extraction expands.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformation_threshold_boundary,
    'Where is the boundary between ''transformative'' and ''derivative but non-transformative'' use, and does it have a stable doctrinal core or expand indefinitely?',
    'Circuit split resolution by Supreme Court on specific edge cases (AI training data, style mimicry, data mining, meme formats); empirical study of judicial outcomes across transformation categories.',
    'If the boundary is indeterminate and expanding, the constraint''s extraction grows without limit (trending toward snare). If a stable core exists, the coordination function remains bounded (tangled_rope stable).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transformation_threshold_boundary, conceptual, 'Whether the transformative use threshold has a stable doctrinal limit or expands to absorb all follow-on creation.').

omega_variable(
    platform_scale_extraction_vs_individual_creation,
    'Does the transformative use reading primarily coordinate individual creators or enable platform-scale extraction of creative value?',
    'Economic analysis of value capture: what share of transformative use value accrues to individual creators vs. platforms vs. rightsholders? Comparison of pre-UGC and post-UGC transformative use ecologies.',
    'If platforms capture the majority of value, the beneficiary structure shifts from ''remix culture'' to ''platform intermediation'' — changing the constraint''s structural classification toward snare for rightsholders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_scale_extraction_vs_individual_creation, empirical, 'Whether the constraint''s beneficiaries are primarily human creators or algorithmic platforms.').

omega_variable(
    kernel_reading_relations,
    'What is the structural relationship between this transformative use reading and its sibling readings of the fair use kernel?',
    'Doctrinal analysis of whether courts applying transformative use reasoning foreclose, coexist with, or influence creator-centric and user-centric frameworks in the same opinions.',
    'Determines cs_structure.reading_relations: forecloses would mean this reading logically excludes siblings; coexists_with means all three remain live in different courts/circuits; influences means this reading reshapes the operating conditions of siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relations, conceptual, 'Structural relationship between transformative_use_reading and creator_centric_reading/user_centric_reading.').

omega_variable(
    market_harm_subordination_limit,
    'Is market harm truly subordinated when transformativeness is found, or does market harm analysis re-enter through the back door via ''market for transformative uses''?',
    'Case law survey: frequency of market harm findings despite transformativeness; analysis of ''potential licensing market'' reasoning in transformative use cases (e.g., Warhol Foundation v. Goldsmith).',
    'If market harm re-enters, the reading''s claimed subordination is theatrical (higher theater_ratio); if genuinely subordinated, the coordination function is stronger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_harm_subordination_limit, empirical, 'Whether market harm analysis is genuinely subordinated or reconstituted in transformative use cases.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__transformative_use_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_use_transformative_tr_t1990, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(fair_use_transformative_tr_t1994, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 1994, 0.12).
narrative_ontology:measurement(fair_use_transformative_tr_t2000, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(fair_use_transformative_tr_t2006, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2006, 0.22).
narrative_ontology:measurement(fair_use_transformative_tr_t2012, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2012, 0.26).
narrative_ontology:measurement(fair_use_transformative_tr_t2018, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2018, 0.29).
narrative_ontology:measurement(fair_use_transformative_tr_t2024, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 2024, 0.31).

% Extraction over time
narrative_ontology:measurement(fair_use_transformative_be_t1990, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 1990, 0.15).
narrative_ontology:measurement(fair_use_transformative_be_t1994, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 1994, 0.22).
narrative_ontology:measurement(fair_use_transformative_be_t2000, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2000, 0.28).
narrative_ontology:measurement(fair_use_transformative_be_t2006, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2006, 0.33).
narrative_ontology:measurement(fair_use_transformative_be_t2012, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2012, 0.38).
narrative_ontology:measurement(fair_use_transformative_be_t2018, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2018, 0.4).
narrative_ontology:measurement(fair_use_transformative_be_t2024, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(fair_use_transformative_su_t1990, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(fair_use_transformative_su_t1994, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 1994, 0.25).
narrative_ontology:measurement(fair_use_transformative_su_t2000, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement(fair_use_transformative_su_t2006, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2006, 0.32).
narrative_ontology:measurement(fair_use_transformative_su_t2012, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2012, 0.35).
narrative_ontology:measurement(fair_use_transformative_su_t2018, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2018, 0.37).
narrative_ontology:measurement(fair_use_transformative_su_t2024, fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__transformative_use_reading, information_standard).
narrative_ontology:boltzmann_floor_override(fair_use_four_factor_test__transformative_use_reading, 0.02).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, dmca_section_512_safe_harbor).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, copyright_term_extension).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, orphan_works_problem).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, ai_training_data_fair_use).

% DUAL FORMULATION NOTE:
% Part of the fair_use_four_factor_test kernel family. This reading (transformative_use_reading) is structurally downstream of the Ehrenfest-like barrier of the statutory four-factor text (17 USC 107) and upstream of platform-specific applications (DMCA safe harbor interpretation, AI training fair use). The creator_centric_reading and user_centric_reading are sibling constraints sharing the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_four_factor_test__transformative_use_reading, institutional, 0.15).
constraint_indexing:directionality_override(fair_use_four_factor_test__transformative_use_reading, powerful, 0.8).
constraint_indexing:directionality_override(fair_use_four_factor_test__transformative_use_reading, organized, 0.35).
constraint_indexing:directionality_override(fair_use_four_factor_test__transformative_use_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
