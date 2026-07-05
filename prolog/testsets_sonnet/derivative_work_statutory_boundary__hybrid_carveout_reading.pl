% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__hybrid_carveout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__hybrid_carveout_reading, []).

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
 *   constraint_id: derivative_work_statutory_boundary__hybrid_carveout_reading
 *   human_readable: Commercial-Exploitation-Contingent Derivative Work Boundary
 *   domain: intellectual_property_law/technology_governance/information_economics
 *
 * SUMMARY:
 *   This story instantiates the hybrid_carveout_reading of the
 *   derivative_work_statutory_boundary kernel: derivative work liability
 *   turns on whether the downstream use is commercial. Non-commercial
 *   transformative uses (fan works, educational remixing, hobbyist
 *   creativity) are treated as outside the derivative work monopoly;
 *   commercial exploitation of the same transformation triggers a licensing
 *   requirement. This produces a categorical split — a genuine coordination
 *   function (protecting rightsholders' commercial derivative markets while
 *   leaving cultural participation free) riding alongside asymmetric
 *   extraction (commercial developers, especially small ones, pay for
 *   authorization that large licensees can negotiate down but that small
 *   studios cannot). This is a DIFFERENT constraint from the
 *   enclosure_reading (which treats ALL transformative use as requiring
 *   authorization) and the coordination_reading (which treats transformative
 *   use as categorically non-infringing regardless of commercial status) —
 *   those are sibling files, not alternate measurements of this one. ε here
 *   is stable at ~0.5 because the reading's own structure is what produces
 *   the extraction: it is not an artifact of how the boundary is observed.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.52).
domain_priors:suppression_score(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.44).
domain_priors:theater_ratio(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__hybrid_carveout_reading, tangled_rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__hybrid_carveout_reading, "Commercial-Exploitation-Contingent Derivative Work Boundary").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__hybrid_carveout_reading, "intellectual_property_law/technology_governance/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__hybrid_carveout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__hybrid_carveout_reading, '0cda2db2-da12-42b6-b7c3-1ba52ad3a37d').
narrative_ontology:cs_kernel_codification('0cda2db2-da12-42b6-b7c3-1ba52ad3a37d', fixed_text).
narrative_ontology:cs_authority_grounding('0cda2db2-da12-42b6-b7c3-1ba52ad3a37d', lineage).
narrative_ontology:cs_interpretation_layer_present('0cda2db2-da12-42b6-b7c3-1ba52ad3a37d').
narrative_ontology:cs_reading_relation('0cda2db2-da12-42b6-b7c3-1ba52ad3a37d', derivative_work_statutory_boundary__enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('0cda2db2-da12-42b6-b7c3-1ba52ad3a37d', derivative_work_statutory_boundary__coordination_reading, coexists_with).
narrative_ontology:cs_axiom('0cda2db2-da12-42b6-b7c3-1ba52ad3a37d', foundational, commercial_exploitation_is_the_relevant_harm_trigger).
narrative_ontology:cs_axiom_status(commercial_exploitation_is_the_relevant_harm_trigger, holdable).
narrative_ontology:cs_axiom_grounding('0cda2db2-da12-42b6-b7c3-1ba52ad3a37d', commercial_exploitation_is_the_relevant_harm_trigger, instrumental).
narrative_ontology:cs_axiom('0cda2db2-da12-42b6-b7c3-1ba52ad3a37d', foundational, noncommercial_transformation_categorically_exempt).
narrative_ontology:cs_axiom_status(noncommercial_transformation_categorically_exempt, holdable).
narrative_ontology:cs_axiom_grounding('0cda2db2-da12-42b6-b7c3-1ba52ad3a37d', noncommercial_transformation_categorically_exempt, conventional).
narrative_ontology:cs_reference_frame('0cda2db2-da12-42b6-b7c3-1ba52ad3a37d', commercial_market_harm_proportionality).
narrative_ontology:cs_drift_state('0cda2db2-da12-42b6-b7c3-1ba52ad3a37d', post_digital_monetization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0cda2db2-da12-42b6-b7c3-1ba52ad3a37d', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, noncommercial_transformative_creators).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, rightsholder_licensing_offices).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_transformative_developers).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, small_commercial_remix_studios).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, major_media_licensees).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, major_media_licensees).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__hybrid_carveout_reading, market_harm_proportionality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers licensing programs that price authorization for any use of the underlying work found to be commercial. Lobbies for and litigates to hold the commercial/non-commercial line, since the line is what generates licensing revenue. Can adjust license terms unilaterally within statutory bounds and collects fees directly from anyone crossing into commercial exploitation.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, rightsholder_licensing_offices, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__hybrid_carveout_reading, rightsholder_licensing_offices, beneficiary).

% Fan creators, educators, and hobbyists who transform copyrighted works without seeking payment. They operate freely under the carveout, facing no licensing burden and minimal enforcement risk as long as they stay outside commercial exploitation. Their exit option is simply not monetizing, which costs them little.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, noncommercial_transformative_creators, beneficiary,
    powerless, biographical, mobile, global).

% Build products, games, or media that transform copyrighted source material and attempt to monetize. The instant they cross into commercial exploitation they must seek and pay for authorization, often at rates set unilaterally by the rightsholder. Cannot easily exit the transformation without abandoning the product concept; cannot easily avoid the commercial designation without abandoning revenue.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_transformative_developers, payer,
    moderate, biographical, constrained, national).

% Small studios and independent commercial creators who lack the legal staff to contest ambiguous commercial/non-commercial line-drawing and lack the leverage to negotiate licensing terms comparable to what large studios obtain. Often face the same nominal rule as major commercial players but with far less capacity to absorb licensing costs or litigate boundary disputes.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, small_commercial_remix_studios, payer,
    powerless, biographical, trapped, regional).

% Large studios and platforms that negotiate bulk or exclusive licensing deals for commercial derivative rights. They pay substantial sums but also benefit from the predictability the boundary creates and from exclusivity arrangements that lock out smaller commercial rivals from the same source material.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, major_media_licensees, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__hybrid_carveout_reading, major_media_licensees, beneficiary).

% Adjudicate disputes over whether a given use is commercial, transformative, both, or neither. Their case-by-case rulings determine how bright or blurry the boundary actually is in practice, and can shift the line's location over time through precedent.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, courts_and_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__hybrid_carveout_reading, rightsholder_licensing_offices).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__hybrid_carveout_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distinguishes uses that require market compensation from uses that do not, in principle allowing free cultural participation and creative remixing to flourish alongside a functioning market for licensed commercial exploitation of copyrighted works.
% TRANSFER_FUNCTION: Moves licensing fees from commercial transformative users to rightsholders whenever a use is classified as commercial exploitation; moves nothing when a use is classified as non-commercial, regardless of the degree of transformation involved.
% ABSENT_VOICES: Small commercial remix studios and independent developers who cannot litigate ambiguous boundary calls have little voice in how courts or rightsholders draw the commercial/non-commercial line in specific cases; large licensees and rightsholders dominate the negotiating and precedent-setting process.
% DISAPPEARANCE_RATIONALE: If the commercial-exploitation carveout vanished, either all transformative use would require authorization (collapsing into the enclosure reading, ending most fan and educational creativity) or all transformative use would be permitted regardless of commercial status (collapsing into the coordination reading, ending most licensing revenue). Either direction reorganizes the transformative-works economy substantially.
% FOUNDING_PROBLEM: Courts needed a workable line between creative reuse that serves free expression and cultural participation, and creative reuse that competes commercially with the rightsholder's own exploitation of the work, without banning transformation outright or permitting free-riding on commercial value.
% FOUNDING_PROBLEM_CORROBORATION: Rightsholder licensing offices attest the line remains necessary to protect commercial markets for derivative works. Independent copyright scholars and small commercial creators outside the rightsholder camp attest that the commercial/non-commercial distinction has become a proxy battle over licensing revenue capture rather than a reliable proxy for actual market harm, particularly as digital monetization blurs what counts as 'commercial.'
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__hybrid_carveout_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__hybrid_carveout_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__hybrid_carveout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) reflects that roughly half of the transformative-use population (the commercial half) bears real licensing costs while the other half bears none — a moderate, categorically-split extraction rather than the near-total extraction of the enclosure reading or the near-zero extraction of the coordination reading. Suppression (0.44) is moderate: non-commercial actors face essentially no suppression, but commercial actors face real barriers (licensing gatekeeping, litigation risk over ambiguous commercial/non-commercial classification). Theater ratio (0.28) is modest — most enforcement activity targets genuine commercial exploitation, though some enforcement drifts toward disputing borderline cases (ad-supported fan content, tip jars) that function more as boundary-testing theater than substantive market protection. Accessibility collapse (0.4) is moderate: alternatives (declining monetization, negotiating licenses, relocating to permissive jurisdictions) remain partially available. Resistance (0.55) is comparatively high because commercial developers, especially small studios, actively contest boundary classifications in court and through industry advocacy.
 *
 * PERSPECTIVAL GAP:
 *   From the rightsholder licensing office's seat, this is a coordination rope: it draws a principled line protecting free expression while preserving commercial markets. From the small commercial remix studio's seat, the same rule computes as extraction, since the commercial/non-commercial line is applied inconsistently and litigated aggressively precisely where it generates the most licensing revenue, with little practical recourse for a party that cannot afford to contest a classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Rightsholder licensing offices are the structural beneficiary: they administer and profit from the line, and can influence where courts draw it through litigation posture. Non-commercial creators are near-full beneficiaries: the carveout exists for them and imposes essentially no cost. Commercial transformative developers, and especially small commercial remix studios, are the targets: they bear the licensing cost and the classification risk, with small studios trapped by lack of negotiating leverage while larger licensees at least achieve some symmetry through bulk deal-making.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — distinguishing culturally valuable free reuse from commercially competing reuse — remains partially live (courts still need SOME line), which prevents this from collapsing into pure mandatrophy. But the specific location of the line has drifted toward maximizing licensing captures at the margin (ambiguous monetization models, borderline commercial status) rather than tracking actual market harm, which is why the tangled_rope classification (both coordination and extraction present, requiring active enforcement) fits better than either a clean rope or a clean snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commercial_classification_boundary_stability,
    'How stable and predictable is the commercial/non-commercial classification in practice, given ad-supported content, tip jars, crowdfunding, and platform monetization features that blur the line?',
    'Survey of adjudicated cases and licensing office determinations over the interval to measure how often marginal monetization models (small ad revenue, optional tips, platform-share payments) are classified as commercial exploitation triggering licensing liability.',
    'If marginal monetization is consistently swept into ''commercial,'' the effective extraction is higher than the categorical split suggests and drifts the reading toward the enclosure_reading''s profile; if marginal monetization is consistently treated as non-commercial, effective extraction is lower than authored and the reading drifts toward the coordination_reading''s profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_classification_boundary_stability, empirical, 'Whether the commercial/non-commercial boundary is stable enough to sustain this reading''s distinct ε over time, or drifts toward a sibling reading''s profile.').

omega_variable(
    small_studio_capacity_asymmetry,
    'Does the categorical commercial/non-commercial split produce meaningfully different effective outcomes for small commercial studios versus large licensees, even though both are nominally ''commercial'' under the same rule?',
    'Compare negotiated licensing rates and litigation outcomes for small commercial remix studios against major media licensees over comparable transformative use disputes.',
    'If small studios systematically pay higher effective rates or lose more boundary disputes than large licensees under the identical nominal rule, the tangled_rope classification understates a second layer of extraction operating within the ''commercial'' category itself, distinct from the commercial/non-commercial split.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(small_studio_capacity_asymmetry, empirical, 'Whether capacity asymmetry within the commercial category compounds the categorical extraction the reading is built around.').

omega_variable(
    kernel_reading_framing_choice,
    'Is the hybrid_carveout_reading a genuinely distinct, stable legal doctrine, or is it better understood as an unstable compromise that courts are gradually resolving toward either the enclosure_reading or the coordination_reading?',
    'Longitudinal tracking of appellate precedent: does the commercial/non-commercial distinction harden into a stable doctrinal test, or does case law gradually erode the non-commercial carveout (drift toward enclosure) or gradually broaden transformative-use protection regardless of commercial status (drift toward coordination)?',
    'If courts are actively drifting the doctrine toward one sibling reading, this story''s stable ε assumption becomes time-limited, and a future story should be authored for the doctrine''s settled endpoint rather than continuing to treat the hybrid as the operative reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_framing_choice, conceptual, 'Whether this reading is a stable equilibrium or a transitional compromise between the enclosure and coordination readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__hybrid_carveout_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(deri_tr_t4, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(deri_tr_t8, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(deri_tr_t12, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement(deri_tr_t16, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(deri_tr_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(deri_tr_t24, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(deri_be_t4, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(deri_be_t8, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(deri_be_t12, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement(deri_be_t16, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(deri_be_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(deri_be_t24, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(deri_su_t4, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 4, 0.33).
narrative_ontology:measurement(deri_su_t8, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 8, 0.36).
narrative_ontology:measurement(deri_su_t12, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 12, 0.39).
narrative_ontology:measurement(deri_su_t16, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 16, 0.41).
narrative_ontology:measurement(deri_su_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 20, 0.43).
narrative_ontology:measurement(deri_su_t24, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 24, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__hybrid_carveout_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.15).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, enclosure_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, coordination_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'the derivative work boundary' per the ε-invariance principle. enclosure_reading treats any incorporation of copyrighted expression as derivative-work preparation (high ε, snare-leaning). coordination_reading treats only substantial fixed recastings as derivative works, exempting transformative and intermediate uses categorically (low ε, rope-leaning). This hybrid_carveout_reading occupies a distinct structural position, gating the boundary on commercial exploitation rather than on degree of transformation, producing its own stable moderate ε rather than interpolating between the siblings' values. All three should be read as a constraint family, not as three measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
