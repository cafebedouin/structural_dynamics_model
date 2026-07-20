% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__transformative_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-25
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
 *   human_readable: Fair Use Statutory Exception â Transformative Right Reading
 *   domain: legal/intellectual_property
 *
 * SUMMARY:
 *   This constraint instantiates the transformative-right reading of the
 *   fair-use kernel: the statutory exception is read not as a residual
 *   market-failure defense but as an affirmative mandate for courts to
 *   facilitate innovation and cultural production by privileging
 *   transformative reuse over licensing markets. It is one of three
 *   structurally distinct readings of the same statutory text; the others
 *   treat fair use as a narrow defense or as a market-licensing threshold.
 *   The kernel is contested because the same 17 U.S.C. Â§ 107 text supports
 *   all three readings with different beneficiary-victim geometries.
 *
 * KEY AGENTS:
 *   - Courts (agenda_setter): Institutional power, analytical exit â they administer the four-factor test and set the transformativeness boundary.
 *   - Transformative reusers (beneficiary): Moderate power, mobile exit â they gain freedom to reuse without licensing.
 *   - Commercial rights holders (payer): Powerful, constrained exit â they bear the cost of lost licensing revenue and exclusivity.
 *   - Cultural producers and innovators (beneficiaries): Moderate power, mobile exit â they depend on the doctrine for production at scale.
 *   - Licensing intermediaries (excluded): Organized, trapped exit â their market-framing arguments are structurally excluded.
 *   - Legal scholars (observer): Analytical seat â they provide external critique and empirical evaluation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__transformative_right_reading, 0.52).
domain_priors:suppression_score(fair_use_statutory_exception__transformative_right_reading, 0.42).
domain_priors:theater_ratio(fair_use_statutory_exception__transformative_right_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__transformative_right_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__transformative_right_reading, "Fair Use Statutory Exception â Transformative Right Reading").
narrative_ontology:topic_domain(fair_use_statutory_exception__transformative_right_reading, "legal/intellectual_property").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__transformative_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__transformative_right_reading, 'f32e0b15-a790-4c8d-8756-9c30c65a3021').
narrative_ontology:cs_kernel_codification('f32e0b15-a790-4c8d-8756-9c30c65a3021', fixed_text).
narrative_ontology:cs_authority_grounding('f32e0b15-a790-4c8d-8756-9c30c65a3021', lineage).
narrative_ontology:cs_interpretation_layer_present('f32e0b15-a790-4c8d-8756-9c30c65a3021').
narrative_ontology:cs_reading_relation('f32e0b15-a790-4c8d-8756-9c30c65a3021', fair_use_statutory_exception__narrow_defense_reading, coexists_with).
narrative_ontology:cs_reading_relation('f32e0b15-a790-4c8d-8756-9c30c65a3021', fair_use_statutory_exception__market_licensing_reading, influences).
narrative_ontology:cs_axiom('f32e0b15-a790-4c8d-8756-9c30c65a3021', foundational, transformative_reuse_prioritized_over_market_harm).
narrative_ontology:cs_axiom_status(transformative_reuse_prioritized_over_market_harm, holdable).
narrative_ontology:cs_axiom_grounding('f32e0b15-a790-4c8d-8756-9c30c65a3021', transformative_reuse_prioritized_over_market_harm, instrumental).
narrative_ontology:cs_reference_frame('f32e0b15-a790-4c8d-8756-9c30c65a3021', innovation_fostering_jurisprudence).
narrative_ontology:cs_drift_state('f32e0b15-a790-4c8d-8756-9c30c65a3021', post_digital_expansion_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f32e0b15-a790-4c8d-8756-9c30c65a3021', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, transformative_reusers).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, cultural_producers).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, innovators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, commercial_rights_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce the four-factor test with emphasis on transformativeness and innovation policy. Their decisions determine whether secondary creators must seek licenses. They do not collect revenue but bear the institutional burden of drawing the transformative-substitutive boundary.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Create remixes, documentaries, criticism, and appropriation art that borrow from existing works. Under this reading they may proceed without a license if the use is transformative. Their fallback is to abandon the project or pay prohibitive licensing fees.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, transformative_reusers, beneficiary,
    moderate, biographical, mobile, national).

% Hold copyrights in entertainment, photography, and literary catalogs. They lose licensing revenue and exclusivity when courts classify uses as transformative. They can litigate or lobby but cannot unilaterally enforce against uses that courts protect.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, commercial_rights_holders, payer,
    powerful, biographical, constrained, global).

% Produce educational materials, fan works, documentaries, and archival projects that depend on borrowing from existing culture. The reading lowers their legal risk and production costs by removing the license requirement for transformative layers.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, cultural_producers, beneficiary,
    moderate, biographical, mobile, national).

% Develop search engines, AI systems, and digital tools that ingest and repurpose copyrighted material at scale. The reading supplies a legal basis for non-expressive or highly transformative computational uses that would be impractical to license transactionally.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, innovators, beneficiary,
    moderate, biographical, mobile, global).

% Collective management organizations and stock-photo agencies that monetize blanket licensing. They are structurally excluded from fair-use analysis under this reading because the availability of a license is explicitly deemed not dispositive of transformativeness.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, licensing_intermediaries, excluded,
    organized, biographical, trapped, global).

% Analyze and critique the doctrine's evolution, producing empirical studies on licensing friction and innovation outcomes. They do not bear costs or collect benefits but shape interpretive frameworks through amicus briefs and scholarship.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, legal_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__transformative_right_reading, diffuse).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__transformative_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces transaction costs and deadweight loss by allowing secondary creators to repurpose existing works for transformative ends without negotiating individual licenses, thereby sustaining cumulative innovation and cultural participation.
% TRANSFER_FUNCTION: Transfers the right to control and monetize transformative uses from commercial rights holders to secondary creators and the public, without compensation.
% ABSENT_VOICES: Licensing intermediaries and collective rights management organizations are structurally excluded; they would argue that any use with a conceivable license harms their market but are kept out by the reading's explicit rule that licensing markets are not dispositive.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, transformative reuse would require licensing for all but the narrowest parodies; documentary filmmaking, remix culture, academic critique, and large-scale AI training would face contraction or defensive clearance practices, and courts would revert to market-harm analysis.
% FOUNDING_PROBLEM: Copyright's exclusive rights, if absolute, create prohibitive transaction costs for secondary creators seeking to build upon existing works, chilling speech, education, and innovation.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and economists outside the copyright industries (e.g., fair-use amici in Authors Guild v. Google) attest that licensing friction remains a live barrier for non-commercial and transformative creators; rights-holder trade associations contest the framing but corroborate the prevalence of widespread unlicensed reuse.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__transformative_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__transformative_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__transformative_right_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_statutory_exception__transformative_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__transformative_right_reading, 0.52, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__transformative_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__transformative_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__transformative_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) is moderate because the doctrine genuinely clears deadweight loss for transformative uses but strips licensing rights from rights holders; the upward temporal trajectory reflects expansion toward appropriation art, search engines, and AI training. Suppression (0.42) captures the degree to which rights holders' enforcement alternative is blocked by judicial application of the transformative test. Theater ratio (0.28â0.40) rises slowly as doctrinal reasoning becomes more performative in contested cases. Accessibility collapse (0.48) is moderate because licensing alternatives remain viable for purely substitutive uses. Resistance (0.65) is high because rights holders litigate and lobby continuously against the reading's expansion.
 *
 * PERSPECTIVAL GAP:
 *   From the reuser seat the constraint is a freedom mechanism; from the rights-holder seat it is a taking of licensing property. The court seat experiences it as a policy lever that expands judicial discretion. The engine should compute these seats differently: reusers as low-d beneficiaries, rights holders as high-d victims, courts as moderate-d agenda-setters with institutional investment in the doctrine's continuity.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (transformative_reusers, cultural_producers, innovators) receive low directionality because the constraint subsidizes their activity. Victims (commercial_rights_holders) receive high directionality because the constraint extracts licensing value from them. Courts are agenda-setters but not financial beneficiaries; they sit slightly toward the beneficiary side of symmetric because the doctrine expands their institutional authority and interpretive space.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy by keeping the founding problemâlicensing friction chilling innovationâexplicitly live in judicial reasoning (Campbell, Google v. Oracle). If courts stopped citing the innovation rationale and simply asserted that transformativeness is self-justifying, the reading would drift toward pure extraction or snare. The live founding problem is what prevents the coordination function from atrophying into performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is fair use best understood as a narrow defense of last resort, a market-failure correction, or an affirmative right to transformative participation?',
    'Comparative doctrinal analysis tracking which reading predicts judicial outcomes and legislative history; empirical measurement of chilling effects under each framing.',
    'Resolution would determine whether the constraint''s Îµ is read as coordination overhead or as asymmetric extraction from property rights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the fair-use kernel is structurally true.').

omega_variable(
    transformative_boundary_stability,
    'Can the line between transformative reuse and substitutive use be adjudicated consistently, or does it invite arbitrary judicial discretion that functions as hidden extraction?',
    'Inter-rater reliability studies of fair-use determinations across circuits; quantitative outcome analysis controlling for party identity.',
    'If the boundary is unstable, the constraint''s extraction is higher than its coordination function and effective suppression rises as predictability collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_boundary_stability, empirical, 'Whether the transformative-substitutive boundary is rule-governed or discretionary.').

omega_variable(
    gain_concentration_question,
    'Do the gains of this reading accrue primarily to decentralized creators, or are they captured by large commercial aggregators such as platforms and AI trainers?',
    'Industrial-organization analysis mapping fair-use-dependent revenue streams to firm size and sector concentration.',
    'If gains concentrate in aggregators, the coordination story is partly cover for extraction, and the beneficiary directionality should shift toward those firms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gain_concentration_question, empirical, 'Whether extraction benefits are captured by large platforms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__transformative_right_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_use_transformative_tr_t0, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fair_use_transformative_tr_t7, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 7, 0.22).
narrative_ontology:measurement(fair_use_transformative_tr_t14, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 14, 0.25).
narrative_ontology:measurement(fair_use_transformative_tr_t21, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 21, 0.3).
narrative_ontology:measurement(fair_use_transformative_tr_t28, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 28, 0.34).
narrative_ontology:measurement(fair_use_transformative_tr_t35, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 35, 0.4).

% Extraction over time
narrative_ontology:measurement(fair_use_transformative_be_t0, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fair_use_transformative_be_t7, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 7, 0.4).
narrative_ontology:measurement(fair_use_transformative_be_t14, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 14, 0.46).
narrative_ontology:measurement(fair_use_transformative_be_t21, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 21, 0.53).
narrative_ontology:measurement(fair_use_transformative_be_t28, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 28, 0.6).
narrative_ontology:measurement(fair_use_transformative_be_t35, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 35, 0.68).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(fair_use_statutory_exception__transformative_right_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__transformative_right_reading, resource_allocation).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, narrow_defense_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, market_licensing_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested fair_use_statutory_exception kernel, decomposed per the Îµ-invariance principle because the kernel label conflates structurally distinct claims: the market-licensing reading treats fair use as residual market failure, the narrow-defense reading treats it as a limited exception, and this reading treats it as an affirmative innovation mandate. Each reading has distinct beneficiary/victim structures and Îµ profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_statutory_exception__transformative_right_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
