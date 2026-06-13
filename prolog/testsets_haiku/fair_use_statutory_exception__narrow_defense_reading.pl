% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__narrow_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__narrow_defense_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fair_use_statutory_exception__narrow_defense_reading
 *   human_readable: Fair Use as Narrow Affirmative Defense Reading
 *   domain: intellectual_property/legal_interpretation
 *
 * SUMMARY:
 *   Copyright law allocates exclusive reproduction, distribution, and
 *   derivative rights to authors and copyright holders. The fair-use doctrine
 *   (17 U.S.C. § 107) provides an affirmative defense to infringement claims
 *   for transformative, educational, and limited uses. This constraint
 *   instantiates ONE reading of the contested fair-use kernel: the
 *   narrow-defense reading treats copyright as property and fair use as a
 *   narrow exception whose burden lies on the defendant to prove statutory
 *   compliance. Under this reading, the four statutory factors are applied in
 *   a framework that presumes copyright protection; transformativeness is
 *   underweighted; commercial nature is determinative; and licensing markets
 *   are presumptively harmed by any use that competes with licensed channels.
 *   This reading has dominated U.S. copyright jurisprudence since the
 *   mid-1990s, especially in appellate doctrine post-Harper & Row and Sony.
 *   It is contested by the transformative-right reading (which treats fair
 *   use as a structural limitation on copyright to enable cultural
 *   production) and the market-licensing reading (which extends market-harm
 *   analysis to suppress even non-competing reuse).
 *
 * KEY AGENTS:
 *   - copyright_holders — institutional beneficiaries; collect licensing revenue and extract through exclusive rights
 *   - licensing_markets — powerful beneficiaries; generate revenue by converting potential fair uses into licensable channels
 *   - fair_use_defendants — moderate-power payers; must prove statutory compliance under burden-shifting regime
 *   - downstream_creators — powerless victims; fused identity with the ability to reference and transform; trapped by licensing costs
 *   - courts_applying_this_reading — institutional agenda-setters; interpret and enforce the narrow-exception doctrine
 *   - technology_platforms — powerful but constrained payers; face infringement liability and self-censor rather than litigate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, 0.78).
domain_priors:suppression_score(fair_use_statutory_exception__narrow_defense_reading, 0.71).
domain_priors:theater_ratio(fair_use_statutory_exception__narrow_defense_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__narrow_defense_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__narrow_defense_reading, "Fair Use as Narrow Affirmative Defense Reading").
narrative_ontology:topic_domain(fair_use_statutory_exception__narrow_defense_reading, "intellectual_property/legal_interpretation").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__narrow_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__narrow_defense_reading, '536ec0e3-f07e-4496-a543-bfd1c29200fe').
narrative_ontology:cs_kernel_codification('536ec0e3-f07e-4496-a543-bfd1c29200fe', fixed_text).
narrative_ontology:cs_authority_grounding('536ec0e3-f07e-4496-a543-bfd1c29200fe', extraction).
narrative_ontology:cs_interpretation_layer_present('536ec0e3-f07e-4496-a543-bfd1c29200fe').
narrative_ontology:cs_reading_relation('536ec0e3-f07e-4496-a543-bfd1c29200fe', fair_use_statutory_exception__transformative_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('536ec0e3-f07e-4496-a543-bfd1c29200fe', fair_use_statutory_exception__market_licensing_reading, influences).
narrative_ontology:cs_axiom('536ec0e3-f07e-4496-a543-bfd1c29200fe', foundational, copyright_is_primary_property_right).
narrative_ontology:cs_axiom_status(copyright_is_primary_property_right, holdable).
narrative_ontology:cs_axiom_grounding('536ec0e3-f07e-4496-a543-bfd1c29200fe', copyright_is_primary_property_right, deontological).
narrative_ontology:cs_axiom('536ec0e3-f07e-4496-a543-bfd1c29200fe', foundational, fair_use_is_narrow_exception_not_structural_limitation).
narrative_ontology:cs_axiom_status(fair_use_is_narrow_exception_not_structural_limitation, holdable).
narrative_ontology:cs_axiom_grounding('536ec0e3-f07e-4496-a543-bfd1c29200fe', fair_use_is_narrow_exception_not_structural_limitation, conventional).
narrative_ontology:cs_reference_frame('536ec0e3-f07e-4496-a543-bfd1c29200fe', copyright_as_property_protection).
narrative_ontology:cs_drift_state('536ec0e3-f07e-4496-a543-bfd1c29200fe', contemporary_digital_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('536ec0e3-f07e-4496-a543-bfd1c29200fe', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, content_licensing_markets).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, fair_use_defendants).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, downstream_creators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, public_domain_users).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__narrow_defense_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(fair_use_statutory_exception__narrow_defense_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__narrow_defense_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__narrow_defense_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) and rising because the narrow reading treats most unauthorized use as presumptive infringement; commercial nature of the defendant's use is treated as nearly dispositive; transformative purpose is underweighted. The licensing market is preserved by narrowing what qualifies as fair use, which funnels more uses through paid channels. Suppression is substantial (0.71) because the reading's burden-shifting and high thresholds for transformativeness create legal friction that discourages fair-use defense even where it might succeed; platforms and creators self-suppress rather than litigate. Theater is moderate (0.42) because the reading does rest on a real coordination problem (allocating copyright incentives), but that function is increasingly decoupled from the narrow defense doctrine—the doctrine now serves extractive rent-seeking more than copyright incentive-alignment. The measurement series spans 40 years (1984–2024) across five time points, tracking the doctrine's evolution from Harper & Row (1985) through Sony (1984), Campbell v. Acuff-Rose (1994, partial reweighting toward transformativeness that this reading resisted), and recent cases (Google Books, Andy Warhol Foundation, Authors Guild v. Google) that narrowed fair-use scope. All metrics share the same time grid.
 *
 * PERSPECTIVAL GAP:
 *   The copyright-holder and platform seats diverge substantially from the fair-use-defendant seat. The copyright holder experiences the arrangement as a coordination mechanism that protects their property and maintains licensing markets. The fair-use defendant experiences the same arrangement as extraction defended by burden-shifting. The narrow reading of fair use exacerbates this divergence by treating most uses as presumptively infringing and requiring defendants to clear high statutory hurdles. Courts applying this reading might compute as rope (genuine coordination of copyright incentives) while defendants compute as snare (pure extraction with burden-shifting and foreclosed alternatives).
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders are the structural beneficiaries: they collect licensing revenue, set the baseline copyright right, and benefit from narrow fair use. Their directionality d is near the beneficiary end (~0.2). Fair-use defendants and downstream creators are targets: they bear the burden of proving statutory compliance, pay licensing fees or self-censor, and face legal friction that discourages reuse. Their d is near the target end (~0.8). Platforms are powerful but constrained: they have institutional power but face liability that makes exit costly; they are neither purely beneficiary nor purely victim, but they function as payers extracting compliance from downstream creators. Courts applying this reading are agenda-setters: they interpret and enforce the doctrine, interpreting narrow, which preserves copyright-holder benefit. Public-domain users appear as victims because the narrow-reading framework colors all copyright interpretation toward protection; ambiguous works are presumed copyrighted under this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The narrow-defense reading displays classic mandatrophy dynamics. The founding problem was to protect copyright incentives while preserving space for quotation, comment, education, and parody—legitimate uses that benefit culture without competing with commercial markets. That problem was substantially solved by the 1990s: copyright protection was robust, production incentives were strong, and fair use accommodated most non-competing reuse. However, the doctrine narrowed further as courts applied the four-factor test in an increasingly copyright-protective frame. The narrow reading now serves to preserve licensing market value rather than to solve the original incentive-alignment problem. The founding_problem_status is 'contested' because copyright holders and courts claim the founding problem is live (without narrow fair use, licensing revenue and production incentives are at risk) while downstream creators and technology platforms claim the problem is solved and the narrow reading now serves pure rent extraction. The rising extractiveness and suppression measurements support the mandatrophy reading: as the doctrine tightened, more uses were forced into licensing channels, generating revenue that feeds back into defending the narrow framing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformativeness_weighting_ambiguity,
    'Should transformative purpose be weighted equally with market harm in the four-factor fair-use test, or should commercial nature be determinative?',
    'Legislative clarification of the four factors, or a Supreme Court decision that reweights transformativeness relative to commercial nature. The Campbell v. Acuff-Rose (1994) decision partially reweighted in favor of transformativeness but courts applying the narrow reading have largely ignored this reweighting.',
    'If transformativeness is elevated to equal weight with market harm, many uses (remix, algorithmic curation, research aggregation) would clear fair use that now face infringement liability under the narrow reading. If commercial nature is retained as determinative, extractiveness and suppression would remain high.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transformativeness_weighting_ambiguity, empirical, 'Whether transformative purpose should be weighted equally with market harm in fair-use analysis.').

omega_variable(
    licensing_market_definition_ambiguity,
    'What counts as a licensing market for purposes of fair-use analysis? Should potential or hypothetical licensing markets be counted, or only existing licensed channels?',
    'Judicial doctrine clarifying what licensing markets are considered for harm analysis. Recent cases (Andy Warhol Foundation, Authors Guild v. Google) have struggled with this; the narrow reading counts broad potential markets while the transformative reading narrows the relevant market to direct competitors.',
    'If the relevant market is narrow (only direct competitive channels), more uses clear fair use; if broad (any potential licensing opportunity), fewer uses qualify. The narrow-defense reading adopts the broader definition, which maximizes extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(licensing_market_definition_ambiguity, conceptual, 'Whether licensing markets are defined broadly (all potential markets) or narrowly (direct competitors only).').

omega_variable(
    founding_problem_persistence,
    'Does copyright protection still require narrow fair-use boundaries to maintain production incentives, or are production incentives sufficiently robust that fair-use could broaden without harming copyright incentives?',
    'Economic analysis of publishing, music, film, and software production incentives under different fair-use regimes. Empirical data from jurisdictions with broader fair-use doctrine (European research exception, Canada''s broader transformativeness standard) would shed light on whether narrower fair use is necessary for production.',
    'If incentives are robust, broadening fair use would not harm production but would enable more downstream creativity. This would resolve the mandatrophy question: the narrow reading persists despite the solved founding problem, serving extractive rent-seeking. If incentives remain sensitive to fair-use boundaries, the narrow reading addresses a live problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether copyright production incentives depend on narrow fair-use boundaries or are robust to broader fair-use doctrine.').

omega_variable(
    committer_reading_scope_ambiguity,
    'Does the narrow-defense reading include or exclude the market-licensing reading''s full suppression of fair use for any use that could be licensed?',
    'Doctrinal analysis of court decisions: some courts adopting the narrow reading stay within it; others extend to market-licensing full suppression. The distinction determines whether they are one reading with internal extension or two separate readings with different ε values.',
    'If the market-licensing reading is a separate constraint (downstream of narrow-defense, more extractive), the corpus should have two stories with different metrics. If it is a radicalization within narrow-defense, the narrow reading''s extractiveness encompasses both. This omega documents the committer frame: the narrow-defense reading leaves this unresolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_reading_scope_ambiguity, conceptual, 'Whether narrow-defense and market-licensing are one reading with internal extension or two separate readings with different beneficiary structures.').

omega_variable(
    downstream_creator_identity_lock_contingency,
    'Is the identity-lock experienced by downstream creators (inability to exit because their creative identity depends on reuse) a structural feature of copyright law, or a contingent feature of the narrow-defense reading?',
    'Comparison with jurisdictions using broader fair-use doctrine and communities using CC licenses or public-domain-heavy cultures: do downstream creators experience less identity-lock in those contexts? Interviews with remix communities, academic researchers, and transformative-works creators.',
    'If identity-lock is universal to copyright, it is structural and no reading removes it. If it is contingent on the narrow reading, broadening fair use would reduce it. This affects the classification: a reading that reduces identity-lock would compute as less extractive at the downstream-creator seat. This is also the committer axis uncertainty: does the narrow reading necessarily entail identity-lock for downstream creators, or is it a contingent effect of how narrowly this reading is applied?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(downstream_creator_identity_lock_contingency, empirical, 'Whether downstream-creator identity-lock is structural to copyright or contingent on the narrow-defense reading.').

omega_variable(
    committer_axiom_overriding_recognition,
    'Do courts and legal authorities applying the narrow-defense reading acknowledge that the foundational axiom (copyright as primary property right) has come into tension with the digital-era reality of abundant copying and derivative creation?',
    'Textual analysis of court opinions, legal scholarship, and policy documents: is the tension between property-rights framing and digital-era practice explicitly recognized, or is it treated as settled doctrine?',
    'If the tension is explicitly recognized, the reading''s authority is eroding and subject to revision. If unacknowledged, the reading maintains perceived legitimacy despite the drift. This feeds the cs_structure.drift_state assessment: acknowledged vs. unacknowledged authority erosion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_axiom_overriding_recognition, conceptual, 'Whether the narrow-defense reading''s foundational axiom is under explicit stress or maintained as settled doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__narrow_defense_reading, 1984, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1984, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1984, 0.22).
narrative_ontology:measurement(fair_tr_t1994, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1994, 0.26).
narrative_ontology:measurement(fair_tr_t2004, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2004, 0.32).
narrative_ontology:measurement(fair_tr_t2014, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2014, 0.38).
narrative_ontology:measurement(fair_tr_t2024, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(fair_be_t1984, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1984, 0.52).
narrative_ontology:measurement(fair_be_t1994, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1994, 0.58).
narrative_ontology:measurement(fair_be_t2004, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2004, 0.67).
narrative_ontology:measurement(fair_be_t2014, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2014, 0.74).
narrative_ontology:measurement(fair_be_t2024, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1984, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1984, 0.48).
narrative_ontology:measurement(fair_su_t1994, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1994, 0.55).
narrative_ontology:measurement(fair_su_t2004, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2004, 0.63).
narrative_ontology:measurement(fair_su_t2014, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2014, 0.68).
narrative_ontology:measurement(fair_su_t2024, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2024, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__narrow_defense_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(fair_use_statutory_exception__narrow_defense_reading, 0.18).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception__transformative_right_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception__market_licensing_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, copyright_duration__extension_politics).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, digital_millennium_copyright_act__circumvention_provision).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested fair-use kernel. The narrow-defense reading treats fair use as an exception to property rights, narrow in scope, with burden on defendants. The transformative-right reading (sibling) treats fair use as a structural limitation on copyright to enable cultural production. The market-licensing reading (sibling, more extractive) extends market-harm analysis to suppress even non-competing reuse. The three readings have materially different ε values and beneficiary structures; they are related through the kernel but are separate constraints. This story instantiates the narrow-defense reading; sibling readings should be authored as separate JSON files linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_statutory_exception__narrow_defense_reading, powerless, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
