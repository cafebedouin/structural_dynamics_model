% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__creator_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__creator_centric_reading, []).

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
 *   constraint_id: fair_use_four_factor_test__creator_centric_reading
 *   human_readable: Fair Use Four-Factor Test (Creator-Centric Reading)
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   The creator-centric reading of the fair use four-factor test treats fair
 *   use as a narrow, defensive exception to the property right that copyright
 *   creates. The four factors (purpose/character, nature of work, amount
 *   used, market effect) are weighed to preserve creator incentives — meaning
 *   factor four (market harm) dominates, and transformativeness is a minor
 *   consideration. This reading has gained institutional force through DMCA
 *   safe harbors, Content ID private ordering, and judicial deference to
 *   licensing markets. The constraint is claimed as tangled_rope
 *   (coordination + extraction) because the four-factor test does provide a
 *   real coordination framework, but its operation extracts heavily from
 *   transformative users and the public domain.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, 0.72).
domain_priors:suppression_score(fair_use_four_factor_test__creator_centric_reading, 0.68).
domain_priors:theater_ratio(fair_use_four_factor_test__creator_centric_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__creator_centric_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__creator_centric_reading, "Fair Use Four-Factor Test (Creator-Centric Reading)").
narrative_ontology:topic_domain(fair_use_four_factor_test__creator_centric_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__creator_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__creator_centric_reading, '491c32d7-3c5f-4c8f-bb65-c3bd4ce841b7').
narrative_ontology:cs_kernel_codification('491c32d7-3c5f-4c8f-bb65-c3bd4ce841b7', fixed_text).
narrative_ontology:cs_authority_grounding('491c32d7-3c5f-4c8f-bb65-c3bd4ce841b7', lineage).
narrative_ontology:cs_interpretation_layer_present('491c32d7-3c5f-4c8f-bb65-c3bd4ce841b7').
narrative_ontology:cs_reading_relation('491c32d7-3c5f-4c8f-bb65-c3bd4ce841b7', fair_use_four_factor_test__transformative_use_reading, coexists_with).
narrative_ontology:cs_reading_relation('491c32d7-3c5f-4c8f-bb65-c3bd4ce841b7', fair_use_four_factor_test__user_centric_reading, coexists_with).
narrative_ontology:cs_axiom('491c32d7-3c5f-4c8f-bb65-c3bd4ce841b7', foundational, fair_use_is_narrow_exception).
narrative_ontology:cs_axiom_status(fair_use_is_narrow_exception, holdable).
narrative_ontology:cs_axiom_grounding('491c32d7-3c5f-4c8f-bb65-c3bd4ce841b7', fair_use_is_narrow_exception, conventional).
narrative_ontology:cs_axiom('491c32d7-3c5f-4c8f-bb65-c3bd4ce841b7', foundational, creator_incentives_primary_over_public_access).
narrative_ontology:cs_axiom_status(creator_incentives_primary_over_public_access, holdable).
narrative_ontology:cs_axiom_grounding('491c32d7-3c5f-4c8f-bb65-c3bd4ce841b7', creator_incentives_primary_over_public_access, instrumental).
narrative_ontology:cs_axiom('491c32d7-3c5f-4c8f-bb65-c3bd4ce841b7', secondary, market_harm_factor_dominates_balancing).
narrative_ontology:cs_axiom_status(market_harm_factor_dominates_balancing, holdable).
narrative_ontology:cs_axiom_grounding('491c32d7-3c5f-4c8f-bb65-c3bd4ce841b7', market_harm_factor_dominates_balancing, conventional).
narrative_ontology:cs_reference_frame('491c32d7-3c5f-4c8f-bb65-c3bd4ce841b7', statutory_copyright_balance_1976).
narrative_ontology:cs_drift_state('491c32d7-3c5f-4c8f-bb65-c3bd4ce841b7', transformative_use_expansion_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('491c32d7-3c5f-4c8f-bb65-c3bd4ce841b7', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, rights_holders).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, creative_industries).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, estate_managers).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, transformative_users).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, public_domain_access).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, platforms).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__creator_centric_reading, creator_incentive_justification).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__creator_centric_reading, property_right_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold copyrights and benefit from licensing revenue. Lobby for narrow fair use interpretation to maximize control over derivative works and licensing markets. Can enforce through litigation and DMCA takedowns. Exit is arbitrage-grade: they can shift enforcement strategies, jurisdictions, or business models.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, rights_holders, beneficiary,
    organized, generational, arbitrage, national).

% Publishers, studios, labels, and collecting societies that set industry standards and lobby Congress. They administer the licensing infrastructure and shape the enforcement environment. Benefit from predictable licensing revenue streams that narrow fair use protects. Can arbitrage across contract terms, territories, and platform deals.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, creative_industries, agenda_setter,
    institutional, generational, arbitrage, national).

% Artists, critics, educators, documentarians, and remix creators who build on existing works. Bear the chilling effect: self-censor, abandon projects, or pay licensing fees they cannot afford. Exit is constrained: they can create original work instead, but the cultural conversation they want to join requires the source material. Litigation risk is asymmetric — they lose even when they win.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, transformative_users, payer,
    moderate, biographical, constrained, national).

% The diffuse public that would access, share, and build upon culture if fair use were robust. Not organized, no standing to sue, no lobby. Their exclusion is structural: the four-factor test's case-by-case nature makes ex-ante certainty impossible, so risk-averse institutions (schools, archives, platforms) over-comply. Trapped because the cultural commons they would use is enclosed by default.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, public_domain_access, excluded,
    powerless, generational, trapped, national).

% Federal judges who apply the four-factor test case by case. Their interpretations create the de facto boundary. They see all seats but are institutionally positioned to preserve the statutory framework. Analytical exit: they can shift doctrine incrementally but cannot rewrite the statute.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, courts, observer,
    institutional, generational, analytical, national).

% YouTube, TikTok, Instagram, etc. Host transformative works at scale. Bear compliance costs (Content ID, takedown systems) and liability risk. Their constrained exit: they cannot leave the U.S. market, so they over-filter to satisfy rights holders. Secondarily set agenda through private ordering (Content ID deals, strike systems) that effectively narrow fair use beyond what courts require.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, platforms, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__creator_centric_reading, platforms, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a statutory framework (17 U.S.C. §107) for courts to evaluate unauthorized uses case-by-case, preventing both absolute property rights that would freeze culture and unbounded exceptions that would destroy creator incentives.
% TRANSFER_FUNCTION: Moves control over derivative and transformative uses from users to rights holders: the burden of proving fair use falls on the user; licensing revenue flows to rights holders; the chilling effect transfers creative risk from rights holders to transformative users.
% ABSENT_VOICES: Transformative creators (especially non-commercial, marginalized, or emerging artists) and public domain advocates are structurally excluded — they lack standing to bring declaratory judgments, cannot afford litigation, and are not represented in the legislative hearings that shape copyright term and scope.
% DISAPPEARANCE_RATIONALE: If the four-factor test vanished and fair use were eliminated, copyright would become near-absolute: all unauthorized uses would be infringing, licensing markets would expand dramatically, transformative culture would retreat to the underground or licensed channels only, and the public domain would shrink further as term extensions continue unchecked.
% FOUNDING_PROBLEM: The 1976 Copyright Act codified fair use to balance creator incentives with public access, recognizing that copyright's monopoly must have breathing room for criticism, comment, news reporting, teaching, scholarship, and research — but the legislative history shows primary concern was preserving the incentive structure for creators.
% FOUNDING_PROBLEM_CORROBORATION: Rights holders and creative industries attest the founding problem is live: digital copying makes incentives more fragile, so narrow fair use is essential. Transformative use advocates (Lessig, Samuelson, EFF, library associations) attest the problem is dead or inverted: the incentive structure is served by abundant transformative use, and narrow fair use now obstructs the progress copyright was meant to promote. Courts are split — some circuits (2nd, 9th) have expanded transformativeness; others hew to the four factors as written.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__creator_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__creator_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__creator_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_four_factor_test__creator_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__creator_centric_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__creator_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_four_factor_test__creator_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the burden structure and market-harm dominance transfer value from transformative users to rights holders — licensing revenue that would not exist under a robust fair use regime. Suppression (0.68) is substantial: the case-by-case uncertainty, asymmetric litigation costs, and private ordering (Content ID, DMCA) create a chilling effect that suppresses more uses than are litigated. Theater ratio (0.42) is moderate: the four-factor test performs a coordination function (courts do decide cases), but a growing share of enforcement activity (automated takedowns, filter obligations) defends the licensing revenue stream rather than adjudicating fair use. Accessibility collapse (0.58) reflects that alternatives (licensing, original creation, public domain) exist but are partially closed off by cost and uncertainty. Resistance (0.55) is meaningful: transformative users, platforms, and public interest litigators push back, but the structural asymmetry favors rights holders.
 *
 * PERSPECTIVAL GAP:
 *   From the rights holder seat, the four-factor test is a necessary coordination mechanism that prevents unlimited copying while allowing limited exceptions — the system works. From the transformative user seat, the same structure operates as a snare: the coordination story is cover for a regime where the exception is so narrow and uncertain that it functions as a licensing mandate. The engine computes this divergence from the structural data — the authored claim (tangled_rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Rights holders and creative industries are structural beneficiaries (d near 0.0): they collect licensing revenue, control the enforcement agenda, and have arbitrage-grade exit. Transformative users and public domain access are structural targets (d near 1.0): they bear the chilling effect, face asymmetric litigation risk, and have constrained or trapped exit. Courts sit near symmetric (d ~ 0.5): they adjudicate but are constrained by precedent and statute. Platforms are dual-positioned: they pay compliance costs (payer) but also set private rules that narrow fair use further (agenda_setter), with constrained exit due to market dependence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (balancing incentives with access) is contested as live vs. dead. If dead, the narrow fair use regime persists as mandatrophy: the coordination function (case-by-case balancing) has atrophied into a licensing default, but the constraint remains because rights holders benefit and no coalition has the power to rewrite the statute. The creator-centric reading prevents mislabeling this as pure coordination (rope) by exposing the asymmetric extraction; it prevents mislabeling as pure extraction (snare) by acknowledging the real adjudicatory function courts still perform.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'This constraint is one reading of the fair_use_four_factor_test kernel. What structural elements differ across the creator_centric, transformative_use, and user_centric readings?',
    'Compare the three constraint stories'' base_properties (extractiveness, suppression, beneficiaries, victims), stakeholder roles, and cs_structure axioms. The kernel_id is fair_use_four_factor_test; sibling constraint_ids are fair_use_four_factor_test__transformative_use_reading and fair_use_four_factor_test__user_centric_reading.',
    'If the three readings produce the same computed type from the same structural data, the kernel framing is illusory — they are one constraint with interpretive noise. If they produce different types with different ε values, the kernel decomposition is validated and each reading is a distinct constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Commitment-system framing: this constraint as one reading of a contested kernel').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, asymmetric litigation costs, automated takedowns) or internalized (creators self-censor because they believe fair use is unavailable)?',
    'Post-reform suppression trajectory: if a jurisdiction adopts a robust fair use safe harbor (e.g., transformative use presumption) and suppression persists among creators who could now rely on it, the internalized component is significant. Survey data on creator knowledge and risk perception would also resolve.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them even after legal reform. This would increase effective extraction for the transformative_user seat beyond what base_properties.suppression captures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the chilling effect on transformative use').

omega_variable(
    coordination_extraction_boundary,
    'Is the four-factor test''s coordination function (providing a legal framework for exceptions) structurally separable from its extraction function (burden-shifting to users, market-harm presumption), or are they fused such that narrowing the exception is the price of the framework?',
    'Natural experiment: jurisdictions or platforms that implement bright-line fair use safe harbors (e.g., non-commercial transformative use exemption). If coordination (dispute resolution, predictability) survives while extraction (licensing revenue from transformative uses) falls, the functions are separable. If coordination collapses without the extraction, they are fused.',
    'If separable, the high extractiveness is contingent on doctrinal choices, not inherent to fair use as coordination — reform could preserve coordination while reducing extraction. If fused, the tangled_rope classification is structural: any fair use framework that coordinates will extract.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether fair use''s coordination and extraction components are structurally separable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__creator_centric_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1976, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 1976, 0.2).
narrative_ontology:measurement(fair_tr_t1985, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement(fair_tr_t1994, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 1994, 0.28).
narrative_ontology:measurement(fair_tr_t1998, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 1998, 0.33).
narrative_ontology:measurement(fair_tr_t2005, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 2005, 0.37).
narrative_ontology:measurement(fair_tr_t2015, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(fair_tr_t2024, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 1976, 0.45).
narrative_ontology:measurement(fair_be_t1985, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 1985, 0.48).
narrative_ontology:measurement(fair_be_t1994, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 1994, 0.52).
narrative_ontology:measurement(fair_be_t1998, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 1998, 0.58).
narrative_ontology:measurement(fair_be_t2005, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 2005, 0.63).
narrative_ontology:measurement(fair_be_t2015, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement(fair_be_t2024, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1976, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 1976, 0.4).
narrative_ontology:measurement(fair_su_t1985, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 1985, 0.45).
narrative_ontology:measurement(fair_su_t1994, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 1994, 0.52).
narrative_ontology:measurement(fair_su_t1998, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 1998, 0.58).
narrative_ontology:measurement(fair_su_t2005, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 2005, 0.62).
narrative_ontology:measurement(fair_su_t2015, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 2015, 0.66).
narrative_ontology:measurement(fair_su_t2024, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__creator_centric_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_four_factor_test__creator_centric_reading, 0.12).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test__transformative_use_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test__user_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, dmca_safe_harbor).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, copyright_term_extension).

% DUAL FORMULATION NOTE:
% This constraint is the creator_centric_reading of the fair_use_four_factor_test kernel. The transformative_use_reading (constraint_id: fair_use_four_factor_test__transformative_use_reading) and user_centric_reading (constraint_id: fair_use_four_factor_test__user_centric_reading) are sibling constraints from the same kernel. This reading has higher ε (0.72 vs. ~0.35 for transformative_use) because it treats market harm as presumptive and transformativeness as minor; the sibling readings invert this weighting. All three share the same statutory text (17 U.S.C. §107) but instantiate different constraints with different beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_four_factor_test__creator_centric_reading, organized, 0.15).
constraint_indexing:directionality_override(fair_use_four_factor_test__creator_centric_reading, institutional, 0.1).
constraint_indexing:directionality_override(fair_use_four_factor_test__creator_centric_reading, moderate, 0.75).
constraint_indexing:directionality_override(fair_use_four_factor_test__creator_centric_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
