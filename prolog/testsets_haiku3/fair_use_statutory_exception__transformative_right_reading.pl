% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__transformative_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__transformative_right_reading, []).

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
 *   constraint_id: fair_use_statutory_exception__transformative_right_reading
 *   human_readable: Fair Use as Transformative Right (Cultural Production Reading)
 *   domain: intellectual_property/legal_interpretation
 *
 * SUMMARY:
 *   Under the transformative-right reading of fair use, Section 107 of the
 *   Copyright Act is interpreted as granting a substantive right to reuse
 *   copyrighted material when the reuse transforms it — adds new meaning,
 *   expression, or cultural value. This reading treats fair use not as a
 *   narrow defense against infringement but as a foundational principle:
 *   copyright exists to serve the public, and the public's interest in
 *   building on existing works is as legitimate as creators' incentive to
 *   produce. Courts facilitate this by presumptively allowing transformative
 *   uses absent direct market substitution. The constraint story models how
 *   this reading operates as an institutional arrangement: who benefits, who
 *   pays, what enforcement is required, and how extraction changes when the
 *   reading is applied.
 *
 * KEY AGENTS:
 *   - Transformative creators: remix artists, scholars, critics, librarians who build on copyrighted material
 *   - Copyright holders (especially in substitutive-harm cases): commercial rightholders whose works are directly copied without transformation
 *   - Courts: the institutional actors responsible for interpreting and enforcing the reading through case-by-case adjudication
 *   - Licensing intermediaries: excluded from beneficiary status because the reading treats licensing markets as non-dispositive
 *   - The general public: beneficiaries of cultural circulation and reuse enabled by the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__transformative_right_reading, 0.41).
domain_priors:suppression_score(fair_use_statutory_exception__transformative_right_reading, 0.38).
domain_priors:theater_ratio(fair_use_statutory_exception__transformative_right_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, extractiveness, 0.41).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__transformative_right_reading, rope).
narrative_ontology:human_readable(fair_use_statutory_exception__transformative_right_reading, "Fair Use as Transformative Right (Cultural Production Reading)").
narrative_ontology:topic_domain(fair_use_statutory_exception__transformative_right_reading, "intellectual_property/legal_interpretation").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__transformative_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__transformative_right_reading, '398b150b-9f36-4d8e-bc63-837361dbbf63').
narrative_ontology:cs_kernel_codification('398b150b-9f36-4d8e-bc63-837361dbbf63', fixed_text).
narrative_ontology:cs_authority_grounding('398b150b-9f36-4d8e-bc63-837361dbbf63', lineage).
narrative_ontology:cs_interpretation_layer_present('398b150b-9f36-4d8e-bc63-837361dbbf63').
narrative_ontology:cs_reading_relation('398b150b-9f36-4d8e-bc63-837361dbbf63', fair_use_statutory_exception__narrow_defense_reading, coexists_with).
narrative_ontology:cs_reading_relation('398b150b-9f36-4d8e-bc63-837361dbbf63', fair_use_statutory_exception__market_licensing_reading, influences).
narrative_ontology:cs_axiom('398b150b-9f36-4d8e-bc63-837361dbbf63', foundational, copyright_serves_public_not_private).
narrative_ontology:cs_axiom_status(copyright_serves_public_not_private, holdable).
narrative_ontology:cs_axiom_grounding('398b150b-9f36-4d8e-bc63-837361dbbf63', copyright_serves_public_not_private, deontological).
narrative_ontology:cs_axiom('398b150b-9f36-4d8e-bc63-837361dbbf63', foundational, transformation_is_legitimate_reuse).
narrative_ontology:cs_axiom_status(transformation_is_legitimate_reuse, holdable).
narrative_ontology:cs_axiom_grounding('398b150b-9f36-4d8e-bc63-837361dbbf63', transformation_is_legitimate_reuse, deontological).
narrative_ontology:cs_reference_frame('398b150b-9f36-4d8e-bc63-837361dbbf63', copyright_incentive_with_reuse_freedom).
narrative_ontology:cs_drift_state('398b150b-9f36-4d8e-bc63-837361dbbf63', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('398b150b-9f36-4d8e-bc63-837361dbbf63', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, transformative_creators).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, cultural_institutions).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, academic_researchers).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, general_public).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, copyright_holders_with_substitutive_harms).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, copyright_serves_public_benefit_not_private_property).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, innovation_requires_reuse_freedom).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Artists, writers, filmmakers, and remix creators who build on existing copyrighted material to produce new cultural works. Under this reading, their uses are presumptively within fair use scope if they add new meaning, message, or expression. Their situation depends on judicial recognition of transformative value; without it, they face cease-and-desist letters and litigation risk even for uses that contribute culturally.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, transformative_creators, beneficiary,
    moderate, biographical, constrained, global).

% Libraries, museums, archives, and educational institutions that preserve, curate, and teach from copyrighted material. They benefit from a broad fair use doctrine that treats preservation, scholarship, and public education as presumptively legitimate. Under the transformative reading, their institutional mission aligns with fair use's public-benefit purpose.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, cultural_institutions, beneficiary,
    organized, generational, mobile, national).

% Scholars who quote, excerpt, and analyze copyrighted texts and data to advance knowledge. Under the transformative reading, scholarship is transformative by definition: it recontextualizes source material within new intellectual frameworks. Their situation depends on courts treating research as presumptively fair use absent direct market substitution.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, academic_researchers, beneficiary,
    organized, biographical, constrained, global).

% The beneficiary of cultural circulation: access to remix, criticism, parody, and transformative works that enrich public culture and discourse. The reading treats the public's interest in cultural reuse as a direct goal of fair use doctrine, not a side effect. Their exit is identity-locked because cultural consumption is constitutive of social participation.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, general_public, beneficiary,
    powerless, biographical, identity_locked, global).

% Copyright owners whose works are directly substituted by infringing uses — for instance, a pirate e-book site that replaces commercial book sales. Under the transformative reading, their harms are real and recognized; fair use does NOT protect uses that market-substitute without adding transformation. Their burden is to demonstrate actual market harm, not to rely on hypothetical licensing markets.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, copyright_holders_with_substitutive_harms, payer,
    powerful, generational, mobile, global).

% Collective licensing organizations and rights-clearance intermediaries (e.g., Copyright Clearance Center, rights-management platforms) that profit from requiring permissions for uses that transformative-reading courts would treat as fair use. They are structurally excluded from this reading's beneficiary logic: the reading treats their market as non-dispositive and argues transformative uses should not require permissions.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, licensing_market_aggregators, excluded,
    organized, generational, trapped, global).

% The judiciary, responsible for interpreting Section 107 of the Copyright Act and adjudicating fair use claims. Under the transformative reading, courts have a duty to actively facilitate innovation and cultural production: to read fair use generously toward new uses that create social value, and to scrutinize whether licensing markets genuinely serve the public or primarily enrich intermediaries. Their institutional position is to serve as the transformative reading's primary enforcement mechanism.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, courts_as_fairness_interpreters, agenda_setter,
    institutional, generational, analytical, national).

% Congress and state legislatures that created copyright law and could modify it through statute. They observe the judicial development of fair use doctrine and could override it by statute if they judged the judicial reading too expansive. The transformative reading depends partly on legislative acquiescence: Congress has repeatedly had opportunity to narrow fair use but has not done so, which the reading interprets as tacit endorsement of judicial expansion.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, legislative_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__transformative_right_reading, diffuse).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__transformative_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reconciles two legitimate interests: creators' incentive to produce (copyright) and the public's need for reuse, building, and cultural circulation (fair use exception). The constraint solves the problem of how much copying is socially beneficial and thus exempt from copyright enforcement.
% TRANSFER_FUNCTION: Transfers the right to reuse copyrighted material, without permission or licensing fees, from copyright holders to transformative users and cultural institutions. The transfer is contingent: it applies only to uses that add new meaning or expression, not to direct substitutes for the original.
% ABSENT_VOICES: Licensing intermediaries and business models that depend on permission-based control of derivative uses would argue that fair use doctrine undermines their market. They are structurally excluded because the transformative reading denies that their market is a valid constraint on fair use scope. Creators in fields where transformative reuse directly competes with commercial licensing (music sampling, film-clip licensing) sometimes object, but their objections are addressed through the market-harm analysis, not through exclusion.
% DISAPPEARANCE_RATIONALE: If the transformative-use doctrine disappeared overnight and fair use reverted to a narrow property-defense reading, remix culture would largely cease (or move underground); educational use of copyrighted material would require mass licensing; scholarship would become litigation-risky; parody and criticism would contract. The cultural production ecosystem depends on the presumption that transformative uses are permitted.
% FOUNDING_PROBLEM: Copyright law grants authors exclusive rights to encourage creation, but overly broad exclusivity chills subsequent creation, scholarship, and cultural participation. The founding problem is: how can copyright incentivize initial creation WITHOUT suppressing the derivative creation and reuse that builds on prior works?
% FOUNDING_PROBLEM_CORROBORATION: Scholars, courts (Harper & Row v. Nation, Sony v. Universal, Google v. Oracle), and cultural producers all attest that the problem is live: copyright's scope and enforcement intensity have grown, and the tension between protection and reuse is acute in the digital era. Academic economics literature documents the innovation-chilling effects of broad exclusivity. The transformative reading's answer (facilitate reuse of transformative uses) is controverted by copyright-holder advocates, but the problem itself is uncontested outside the industry benefiting from strict enforcement.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__transformative_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__transformative_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__transformative_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_statutory_exception__transformative_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__transformative_right_reading, 0.41, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__transformative_right_reading_tests).
:- end_tests(fair_use_statutory_exception__transformative_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.41 at interval end) because the transformative reading distributes burdens and benefits across multiple seats. Transformative creators and institutions benefit (low extraction for them); substitutive copiers do not (high extraction for them). Copyright holders with real market harm are recognized as payers, but licensing intermediaries are excluded from the payer set—the reading argues licensing markets should not expand the extraction boundary. Over the measured interval (1976–2025), extractiveness DECLINES from 0.58 to 0.41, driven by judicial expansion of the transformative doctrine (cases like Campbell v. Acuff-Rose Music, Google v. Oracle) that increasingly presume fair use applies to new categories of reuse. Suppression requirement declines similarly: the reading reduces the burden of proving fair use by shifting the frame from 'defend copying' to 'prove transformation is absent.' Theater ratio is lower (0.22) because the transformative reading stakes its legitimacy on a genuine coordination function (balancing incentives with reuse) rather than on performative compliance. The reading is more functionally rooted than narrow-defense or market-licensing readings.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (copyright holders claiming market harm) and the agenda-setter seat (courts facilitating transformative uses) would compute different types. From the copyright holder's position, the arrangement increasingly restricts their exclusive rights and is extractive toward them. From the court's position, the arrangement protects the public interest in cultural production and is coordinating between two legitimate interests. The engine computes these per-seat divergences from the structural data (beneficiary/victim declarations + power + exit). The authorized claim (rope, coordinating) should diverge from the copyright holder's local reading (snare, extractive toward them)—that divergence is the measurement the system is designed to detect.
 *
 * DIRECTIONALITY LOGIC:
 *   Transformative creators, institutions, and the general public are beneficiaries under this reading (d near 0.0): the doctrine presumes their uses are permitted absent transformation-blocking evidence. Copyright holders with genuine substitutive harms are payers (d near 1.0): they bear the cost of narrowed exclusive rights when market-replacement is absent. Licensing intermediaries have high d but are excluded from the direct cost-bearing: the reading denies their licensing market is a valid payer constituency. Courts sit at d ~0.5 (symmetric): they bear the administrative burden of case-by-case transformation assessment but gain institutional authority and the satisfaction of serving the public interest. The time-series decline in extractiveness reflects judicial extension of the transformative presumption: as courts recognize more categories as presumptively transformative, the extraction burden on creators decreases.
 *
 * MANDATROPHY ANALYSIS:
 *   The transformative reading avoids mandatrophy by maintaining a live founding problem: copyright's scope continues to expand, and the need for reuse space remains contested. The founding problem (how to balance incentives with subsequent creation) is not dead; it is actively litigated. However, there is a weak mandatrophy signal in the narrowing extractiveness: if the founding problem were disappearing (because copyright scope was not actually restricting reuse), we would expect to see extraction stay constant or rise as the doctrine's protections became unnecessary. Instead, extraction declines, suggesting courts believe the problem is worsening and fair use needs expansion. The reading remains functionally justified, not vestigial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformative_definition_boundary,
    'What constitutes ''transformative use'' for purposes of fair use analysis? Does transformation require new artistic meaning, or can commercial recontextualization (e.g., resale of used goods, reverse engineering for interoperability) qualify?',
    'Appellate case law establishing bright-line tests or factors for transformation; pilot surveys of judges'' transformation assessments on identical fact patterns.',
    'A narrow transformation definition (artistic meaning only) restricts fair use and increases extraction; a broad definition (any added value) expands it and decreases extraction. The boundary placement directly determines which creators benefit and which copyright holders pay.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transformative_definition_boundary, conceptual, 'Transformation is not precisely defined in statute, and definitional shifts change the constraint''s classification boundaries.').

omega_variable(
    licensing_market_counterfactual,
    'When a licensing market exists for a type of use, does that fact alone establish that fair use should not protect the use? Or is the existence of a licensing market irrelevant if the use is transformative?',
    'Comparative analysis of fair-use outcomes across jurisdictions with different licensing-market policies; empirical study of whether copyright-holder revenue from licensing correlates with fair-use litigation outcomes.',
    'If licensing-market existence is dispositive, fair use shrinks dramatically (many uses could be licensed). If licensing-market existence is irrelevant, fair use expands to cover transformative uses regardless of licensing potential. This is the core point of disagreement between the transformative reading and the market-licensing reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(licensing_market_counterfactual, conceptual, 'Whether licensing-market existence overrides transformative status.').

omega_variable(
    burden_of_proof_allocation,
    'Should the burden of proving fair use rest on the user (having to affirmatively prove transformation) or on the copyright holder (having to prove harm)? Does the reading''s assumption that courts should ''facilitate innovation'' require a presumption favoring transformation?',
    'Legislative amendment of Section 107; appellate reversal of lower courts'' burden-shifting; empirical analysis of win rates for fair-use claimants under different burden rules.',
    'A user burden makes fair use harder to establish; a copyright-holder burden makes it easier. This determines extractiveness more directly than substantive doctrine: shifting burden from user to copyright holder can reduce measured extractiveness by 0.15–0.25 without changing the legal rule''s text.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(burden_of_proof_allocation, empirical, 'Procedural burden allocation shapes effective extraction independently of substantive fair-use doctrine.').

omega_variable(
    reading_vs_narrow_defense_foreclosure,
    'Does the transformative-right reading foreclose the narrow-defense reading, or do they coexist as live interpretations held by different courts and factions?',
    'Constitutional or appellate jurisprudence establishing whether Section 107 must be read as a right (transformative reading) or a defense (narrow reading); legislative amendment clarifying intent.',
    'Foreclosure would mean adopting the transformative reading entails formally rejecting narrow-defense reasoning in all courts and contexts. Non-foreclosure means both readings remain live options. Currently, they coexist: some courts adopt transformative framing; others adopt narrow-defense framing. The evidence suggests coexistence, not foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_narrow_defense_foreclosure, conceptual, 'Whether the transformative and narrow-defense readings are logically incompatible or merely competing live options.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__transformative_right_reading, 1976, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1976, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 1976, 0.35).
narrative_ontology:measurement_basis(fair_tr_t1976, observed).
narrative_ontology:measurement(fair_tr_t1990, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement_basis(fair_tr_t1990, observed).
narrative_ontology:measurement(fair_tr_t2000, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2000, 0.26).
narrative_ontology:measurement_basis(fair_tr_t2000, observed).
narrative_ontology:measurement(fair_tr_t2010, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2010, 0.24).
narrative_ontology:measurement_basis(fair_tr_t2010, observed).
narrative_ontology:measurement(fair_tr_t2018, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2018, 0.22).
narrative_ontology:measurement_basis(fair_tr_t2018, observed).
narrative_ontology:measurement(fair_tr_t2025, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2025, 0.22).
narrative_ontology:measurement_basis(fair_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 1976, 0.58).
narrative_ontology:measurement_basis(fair_be_t1976, observed).
narrative_ontology:measurement(fair_be_t1990, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 1990, 0.52).
narrative_ontology:measurement_basis(fair_be_t1990, observed).
narrative_ontology:measurement(fair_be_t2000, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement_basis(fair_be_t2000, observed).
narrative_ontology:measurement(fair_be_t2010, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2010, 0.44).
narrative_ontology:measurement_basis(fair_be_t2010, observed).
narrative_ontology:measurement(fair_be_t2018, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2018, 0.41).
narrative_ontology:measurement_basis(fair_be_t2018, observed).
narrative_ontology:measurement(fair_be_t2025, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2025, 0.41).
narrative_ontology:measurement_basis(fair_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1976, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 1976, 0.52).
narrative_ontology:measurement_basis(fair_su_t1976, observed).
narrative_ontology:measurement(fair_su_t1990, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 1990, 0.48).
narrative_ontology:measurement_basis(fair_su_t1990, observed).
narrative_ontology:measurement(fair_su_t2000, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2000, 0.44).
narrative_ontology:measurement_basis(fair_su_t2000, observed).
narrative_ontology:measurement(fair_su_t2010, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement_basis(fair_su_t2010, observed).
narrative_ontology:measurement(fair_su_t2018, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2018, 0.39).
narrative_ontology:measurement_basis(fair_su_t2018, observed).
narrative_ontology:measurement(fair_su_t2025, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2025, 0.38).
narrative_ontology:measurement_basis(fair_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__transformative_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_statutory_exception__transformative_right_reading, 0.12).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception__narrow_defense_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception__market_licensing_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, copyright_scope_expansion_institutional_extraction).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, remix_culture_institutional_permission_tax).

% DUAL FORMULATION NOTE:
% The fair-use-statutory-exception kernel decomposes into three structurally distinct constraints, one per reading. The transformative-right reading (this constraint) has low-to-moderate extractiveness (0.41) and presumes transformative uses are permitted. The narrow-defense reading (sibling) has higher extractiveness (0.62+) and presumes fair use is a narrow defense. The market-licensing reading (sibling) has highest extractiveness (0.75+) and presumes licensing markets are dispositive. All three read the same statutory text but derive different ε values because they disagree on what 'transformative,' 'market harm,' and 'public benefit' mean. Decomposition follows the ε-invariance principle: changing how you measure the constraint (which reading you adopt) changes ε, so you have multiple constraints, not one. Network links establish influence: the transformative reading influences the market-licensing reading (creates pressure to deny licensing-market dispositive status), and both influence copyright-scope dynamics (institutional extraction is downstream of fair-use narrowness).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_statutory_exception__transformative_right_reading, powerless, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
