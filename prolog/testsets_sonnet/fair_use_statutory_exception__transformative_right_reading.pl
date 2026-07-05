% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__transformative_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Fair Use as Transformative-Use Right (Courts Facilitate Innovation)
 *   domain: legal/intellectual_property
 *
 * SUMMARY:
 *   This story instantiates the transformative-right reading of the fair use
 *   kernel: fair use as an affirmative doctrine that enables cultural
 *   production and innovation, where courts weigh transformativeness heavily
 *   and do not treat the mere existence or buildability of a licensing market
 *   as dispositive against a finding of fair use. This is structurally
 *   distinct from the narrow_defense_reading (which treats fair use as a
 *   narrowly construed exception preserving rightsholder market value) and
 *   the market_licensing_reading (which treats any licensable use as market
 *   harm by definition). Each reading is authored as its own constraint with
 *   its own epsilon; this file does not average or hedge across them.
 *
 * KEY AGENTS:
 *   - transformative_reusers: beneficiary (moderate/constrained) — build new expressive works on existing ones
 *   - documentary_filmmakers: beneficiary (moderate/constrained) — incorporate archival material for critical/historical purpose
 *   - researchers_and_educators: beneficiary (moderate/constrained) — quote and excerpt for scholarship
 *   - software_interoperability_developers: beneficiary (organized/mobile) — reimplement interfaces for compatibility
 *   - rightsholders_with_licensing_revenue_expectations: payer (powerful/constrained) — bear uncompensated reuse
 *   - courts: agenda_setter (institutional/analytical) — administer the transformativeness inquiry
 *   - licensing_intermediaries: excluded (organized/constrained) — market not treated as controlling evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__transformative_right_reading, 0.22).
domain_priors:suppression_score(fair_use_statutory_exception__transformative_right_reading, 0.28).
domain_priors:theater_ratio(fair_use_statutory_exception__transformative_right_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__transformative_right_reading, rope).
narrative_ontology:human_readable(fair_use_statutory_exception__transformative_right_reading, "Fair Use as Transformative-Use Right (Courts Facilitate Innovation)").
narrative_ontology:topic_domain(fair_use_statutory_exception__transformative_right_reading, "legal/intellectual_property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__transformative_right_reading, '498202a8-6695-4f50-a662-e7a172841e87').
narrative_ontology:cs_kernel_codification('498202a8-6695-4f50-a662-e7a172841e87', fixed_text).
narrative_ontology:cs_authority_grounding('498202a8-6695-4f50-a662-e7a172841e87', lineage).
narrative_ontology:cs_interpretation_layer_present('498202a8-6695-4f50-a662-e7a172841e87').
narrative_ontology:cs_reading_relation('498202a8-6695-4f50-a662-e7a172841e87', fair_use_statutory_exception__narrow_defense_reading, coexists_with).
narrative_ontology:cs_reading_relation('498202a8-6695-4f50-a662-e7a172841e87', fair_use_statutory_exception__market_licensing_reading, influences).
narrative_ontology:cs_axiom('498202a8-6695-4f50-a662-e7a172841e87', foundational, transformative_purpose_controls_factor_one).
narrative_ontology:cs_axiom_status(transformative_purpose_controls_factor_one, holdable).
narrative_ontology:cs_axiom_grounding('498202a8-6695-4f50-a662-e7a172841e87', transformative_purpose_controls_factor_one, conventional).
narrative_ontology:cs_axiom('498202a8-6695-4f50-a662-e7a172841e87', foundational, licensing_market_existence_not_dispositive_of_harm).
narrative_ontology:cs_axiom_status(licensing_market_existence_not_dispositive_of_harm, holdable).
narrative_ontology:cs_axiom_grounding('498202a8-6695-4f50-a662-e7a172841e87', licensing_market_existence_not_dispositive_of_harm, instrumental).
narrative_ontology:cs_reference_frame('498202a8-6695-4f50-a662-e7a172841e87', campbell_transformative_purpose_framework).
narrative_ontology:cs_drift_state('498202a8-6695-4f50-a662-e7a172841e87', post_warhol_goldsmith_2023, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('498202a8-6695-4f50-a662-e7a172841e87', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, transformative_reusers).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, documentary_filmmakers).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, parody_and_satire_creators).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, researchers_and_educators).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, software_interoperability_developers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, rightsholders_with_licensing_revenue_expectations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Artists, remixers, critics, and commentators who build new works — parody, criticism, collage, appropriation art — on copyrighted material. Under this reading, courts ask whether the new use adds new meaning or purpose rather than substituting for the original in the market. Their exit option, absent this reading, would be seeking licenses that rightsholders have no incentive to grant for critical or transformative purposes.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, transformative_reusers, beneficiary,
    moderate, biographical, constrained, national).

% Rely on incorporating clips, images, and archival footage to make historical and critical arguments. Licensing every fragment is often commercially or practically impossible; this reading lets courts treat unlicensed inclusion as fair when it serves documentary purpose rather than displacing the underlying work's market.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, documentary_filmmakers, beneficiary,
    moderate, biographical, constrained, national).

% Quote, excerpt, and reproduce copyrighted material for scholarship, criticism, and classroom teaching. They depend on courts treating transformative, non-substitutive use as presumptively lawful rather than requiring case-by-case licensing negotiation with rightsholders who have little incentive to grant permission for critical uses.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, researchers_and_educators, beneficiary,
    moderate, generational, constrained, national).

% Reverse-engineer or reimplement interfaces to build compatible or competing products. This reading treats such reuse as transformative when it enables new functionality rather than reproducing the original's expressive market, letting them build without securing permission from incumbent platform holders.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, software_interoperability_developers, beneficiary,
    organized, generational, mobile, global).

% Publishers, studios, and content owners who would prefer every reuse to require a license fee. Under this reading, courts may find fair use even where a licensing market exists or could be built, because market substitutability is treated as one factor among several rather than dispositive. They bear the uncompensated use as the cost of the transformative-use doctrine, and their remedy is litigation over whether a given reuse is transformative enough.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, rightsholders_with_licensing_revenue_expectations, payer,
    powerful, biographical, constrained, national).

% Apply the four fair-use factors with the first factor (purpose and character of the use) and its transformativeness inquiry given controlling weight. They administer the doctrine case by case, treating cultural production and innovation policy as a legitimate judicial concern rather than confining themselves to market-harm accounting.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Collective licensing agencies and rights-clearance services whose business model depends on every reuse requiring a transaction. Under this reading their market is deliberately not treated as evidence that a use is unfair; they are not party to the litigation that shapes the doctrine and have no formal voice in the transformativeness inquiry.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, licensing_intermediaries, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__transformative_right_reading, diffuse).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__transformative_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows new expressive, scholarly, and technical works to build on existing ones without requiring case-by-case licensing negotiation for every critical, parodic, educational, or interoperability-driven reuse — solving the holdout and negotiation-cost problem that would otherwise block transformative cultural and technical production.
% TRANSFER_FUNCTION: Permits the value created by transformative reuse to accrue to the reuser and to the public (new criticism, art, scholarship, interoperable software) without a payment flowing back to the original rightsholder, even in cases where a licensing market for the underlying use could plausibly be built.
% ABSENT_VOICES: Licensing intermediaries and collecting societies whose business depends on mandatory clearance are structurally outside the transformativeness inquiry — courts do not treat the existence or growth of a licensing market as controlling, so those intermediaries have no doctrinal lever to insist their market be protected.
% DISAPPEARANCE_RATIONALE: If courts abandoned the transformative-use framework, parody, criticism, documentary reuse, and software interoperability practices built on the assumption of doctrinal breathing room would face a wave of licensing demands and infringement suits; secondary creative and technical markets that currently operate without per-use clearance would need to build licensing infrastructure or contract sharply.
% FOUNDING_PROBLEM: Copyright's exclusive rights, if applied literally, would let rightsholders block criticism, parody, scholarship, and follow-on innovation that depends on borrowing from existing works — chilling the very cultural production copyright is meant to encourage.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars outside the reuse-beneficiary community (copyright treatise writers, appellate judges writing on both sides of specific rulings) attest the negotiation-cost and holdout problem remains real for parody, criticism, and archival reuse; rightsholder trade associations dispute that the problem is as severe as claimed and argue licensing markets have matured enough that courts should defer to them — that dispute is itself evidence the founding problem's current scope is contested rather than settled.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__transformative_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__transformative_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__transformative_right_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_statutory_exception__transformative_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__transformative_right_reading, 0.22, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.22) and gently declining over the interval because under this reading, the doctrine's own logic bars treating displaced licensing revenue as automatically dispositive — extraction from rightsholders is real but bounded by the transformativeness screen, which the courts have applied with increasing consistency since the 1994 Campbell v. Acuff-Rose decision established transformative purpose as central. Suppression is moderate (0.28): the doctrine does suppress rightsholders' ability to demand licensing for transformative reuse, but this is a deliberate, litigated, judicially-supervised suppression rather than coercive extraction — it is the coordination mechanism itself. Theater ratio is low and falling (0.15) because the transformativeness inquiry is a substantive analytical exercise, not a performative gesture — courts genuinely examine purpose and character rather than rubber-stamping outcomes. Resistance is moderately high (0.55) reflecting the sustained rightsholder litigation campaign against this reading (publishers, studios, stock photo agencies) precisely because the doctrine costs them foregone licensing revenue.
 *
 * PERSPECTIVAL GAP:
 *   From the reuser seats, this reading operates as genuine coordination: it solves a real holdout problem that would otherwise let rightsholders veto criticism, parody, and interoperable innovation. From the rightsholder seat, the same doctrine looks like judicially-sanctioned uncompensated taking of value that a licensing market could otherwise capture. The engine should compute these as structurally different experiences of the identical rule — the payer seat's directionality sits closer to the target end even though the story's overall extractiveness is low, because for that specific seat the doctrine is where their expected revenue disappears.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (transformative reusers, documentary filmmakers, researchers, interoperability developers) are coded with low directionality because the doctrine subsidizes their activity by removing a licensing requirement they would otherwise face. Rightsholders are coded as payers with directionality toward the target end because the doctrine denies them a revenue stream they could otherwise plausibly extract via licensing — but their exit option is constrained rather than trapped, since they retain litigation and lobbying avenues to narrow the doctrine case by case. Licensing intermediaries are excluded rather than payers because their institutional interest is structurally outside the transformativeness inquiry, not merely disfavored within it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that literal application of exclusive rights would chill criticism, scholarship, and follow-on innovation — remains live and is corroborated outside the reuser community by neutral legal commentary, even though rightsholder trade groups contest its continuing severity. This forecloses a mandatrophy finding: the doctrine is not a persisting husk defending a solved problem, it is an active judicial accommodation to an ongoing tension between exclusive rights and cultural production that has not been resolved by market evolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformativeness_versus_market_substitution_boundary,
    'Where exactly does a reuse stop being transformative (adding new meaning/purpose) and start being substitutive (displacing the original''s market)? This reading treats the line as courts'' analytical judgment call; the sibling market_licensing_reading treats licensability itself as the deciding fact.',
    'Track appellate outcomes across circuits on borderline cases (e.g. AI training data, meme culture, sampling) to see whether the transformativeness inquiry converges on a stable doctrine or fragments circuit-by-circuit.',
    'If the boundary proves unstable or courts increasingly import market-substitution reasoning even while nominally applying the transformativeness test, this reading is drifting toward the market_licensing_reading in practice despite retaining transformative_right_reading''s doctrinal language — a live convergence risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformativeness_versus_market_substitution_boundary, conceptual, 'Instability of the line between transformative and substitutive use under this reading''s own test.').

omega_variable(
    kernel_sibling_divergence_locus,
    'This reading, narrow_defense_reading, and market_licensing_reading all interpret the identical statutory text (17 U.S.C. § 107) — where structurally does the disagreement actually live: in factor weighting (how much weight factor one gets relative to factor four), in the burden of proof (who must show harm vs. who must show transformation), or in whether licensing-market existence is treated as evidence at all?',
    'Doctrinal history analysis comparing pre-Campbell (1994) and post-Campbell fair use opinions, isolating which specific factor''s treatment changed rather than treating the shift as a single undifferentiated doctrinal event.',
    'If the disagreement is purely about factor-four weighting, the three readings are closer than they appear and could in principle converge through incremental case law; if it is about burden allocation, the readings are more structurally incompatible and a Supreme Court ruling would be needed to resolve which reading controls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_sibling_divergence_locus, conceptual, 'Locating precisely where the transformative_right_reading diverges structurally from its two sibling readings of the same kernel.').

omega_variable(
    rightsholder_market_erosion_measurement,
    'Has the transformative_right_reading''s low-epsilon operation over 1990-2025 actually eroded licensing markets that would otherwise have developed (supporting rightsholders'' claim of harm), or have those markets simply never existed because the underlying uses were never commercially licensable in the first place?',
    'Empirical study of licensing-market formation in adjacent domains where fair use does NOT apply (e.g. purely commercial stock photo reuse) versus domains where it does (parody, criticism), controlling for demand-side willingness to pay.',
    'If markets would not have formed regardless, the payer seat''s extraction claim is overstated and this reading''s low epsilon is well-grounded; if markets were actively suppressed by the doctrine, the extractiveness value may be understated relative to a counterfactual world with the market_licensing_reading controlling instead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rightsholder_market_erosion_measurement, empirical, 'Whether the doctrine suppresses a real counterfactual licensing market or operates in a space where no such market would exist anyway.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__transformative_right_reading, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1990, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(fair_tr_t1997, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 1997, 0.19).
narrative_ontology:measurement(fair_tr_t2004, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2004, 0.17).
narrative_ontology:measurement(fair_tr_t2011, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2011, 0.16).
narrative_ontology:measurement(fair_tr_t2018, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2018, 0.15).
narrative_ontology:measurement(fair_tr_t2025, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(fair_be_t1990, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(fair_be_t1997, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 1997, 0.28).
narrative_ontology:measurement(fair_be_t2004, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2004, 0.25).
narrative_ontology:measurement(fair_be_t2011, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2011, 0.24).
narrative_ontology:measurement(fair_be_t2018, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2018, 0.23).
narrative_ontology:measurement(fair_be_t2025, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2025, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(fair_use_statutory_exception__transformative_right_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__transformative_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_statutory_exception__transformative_right_reading, 0.1).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, narrow_defense_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, market_licensing_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the fair_use_statutory_exception kernel. narrow_defense_reading and market_licensing_reading are the payer-favoring siblings, generating higher epsilon for the same underlying statutory text by weighting factor four (market effect) and burden allocation differently. All three link to each other via affects_constraints because a doctrinal shift toward one reading (e.g. a Supreme Court ruling emphasizing market substitutability, as in the 2023 Warhol v. Goldsmith decision) structurally pressures the others by changing which reading circuit courts treat as controlling precedent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
