% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__market_licensing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__market_licensing_reading, []).

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
 *   constraint_id: fair_use_statutory_exception__market_licensing_reading
 *   human_readable: Fair Use Doctrine Under Market Licensing Reading
 *   domain: intellectual_property/legal_interpretation
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested fair use
 *   kernel—the market-licensing reading. Fair use is codified in 17 U.S.C. §
 *   107 as an affirmative defense to copyright infringement, listing four
 *   statutory factors for courts to weigh: purpose and character of the use,
 *   nature of the copyrighted work, amount used, and effect on the market for
 *   or value of the copyrighted work. This reading interprets the statute
 *   through a specific lens: any use that could be licensed—that is, any use
 *   where a licensing mechanism exists or could be established—harms the
 *   licensing market and therefore extinguishes fair use protection. Under
 *   this reading, fair use collapses to a de minimis exception covering only
 *   uses where no licensing market exists: incidental background uses, uses
 *   too small to monetize, uses where rights holders cannot be identified or
 *   located. All educational, research, non-commercial transformative, and
 *   preservation uses lose protection if licensing is theoretically possible.
 *   The reading is structurally extractive: it converts fair use from a
 *   structural right that calibrates copyright incentives into a gatekeeping
 *   mechanism that forces all valuable reuse into licensing transactions.
 *   This constraint story documents that reading ONLY; sibling readings
 *   (transformative-right and narrow-defense) are separate constraint stories
 *   linked via network relationships.
 *
 * KEY AGENTS:
 *   - copyright_owners (powerful, beneficiary) — own copyrighted works and benefit from expanded licensing opportunities
 *   - licensing_intermediaries (institutional, beneficiary + agenda_setter) — administer rights management and collect intermediation fees; they enforce the reading by developing licensing platforms and interpreting the statute
 *   - courts (institutional, agenda_setter) — interpret the fair use statute and apply the licensing-market logic as the dispositive test
 *   - educational_institutions (organized, payer + victim) — face compressed fair use protection and rising licensing costs
 *   - non_commercial_creators (moderate, payer + victim) — lose ability to create transformative derivative works without licensing
 *   - researchers (moderate, payer + victim) — cannot conduct text mining, corpus analysis, or algorithmic auditing without licensing
 *   - cultural_commons_users (powerless, payer + victim) — cannot preserve, archive, or provide accessibility without licensing negotiation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, 0.92).
domain_priors:suppression_score(fair_use_statutory_exception__market_licensing_reading, 0.88).
domain_priors:theater_ratio(fair_use_statutory_exception__market_licensing_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__market_licensing_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__market_licensing_reading, "Fair Use Doctrine Under Market Licensing Reading").
narrative_ontology:topic_domain(fair_use_statutory_exception__market_licensing_reading, "intellectual_property/legal_interpretation").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__market_licensing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__market_licensing_reading, '113d6e04-1dc8-45ba-aee1-dd98fafd0cb2').
narrative_ontology:cs_kernel_codification('113d6e04-1dc8-45ba-aee1-dd98fafd0cb2', fixed_text).
narrative_ontology:cs_authority_grounding('113d6e04-1dc8-45ba-aee1-dd98fafd0cb2', lineage).
narrative_ontology:cs_interpretation_layer_present('113d6e04-1dc8-45ba-aee1-dd98fafd0cb2').
narrative_ontology:cs_reading_relation('113d6e04-1dc8-45ba-aee1-dd98fafd0cb2', fair_use_statutory_exception__transformative_right_reading, forecloses).
narrative_ontology:cs_reading_relation('113d6e04-1dc8-45ba-aee1-dd98fafd0cb2', fair_use_statutory_exception__narrow_defense_reading, coexists_with).
narrative_ontology:cs_axiom('113d6e04-1dc8-45ba-aee1-dd98fafd0cb2', foundational, licensing_market_priority).
narrative_ontology:cs_axiom_status(licensing_market_priority, holdable).
narrative_ontology:cs_axiom_grounding('113d6e04-1dc8-45ba-aee1-dd98fafd0cb2', licensing_market_priority, instrumental).
narrative_ontology:cs_axiom('113d6e04-1dc8-45ba-aee1-dd98fafd0cb2', secondary, fair_use_collapse_to_margins).
narrative_ontology:cs_axiom_status(fair_use_collapse_to_margins, holdable).
narrative_ontology:cs_axiom_grounding('113d6e04-1dc8-45ba-aee1-dd98fafd0cb2', fair_use_collapse_to_margins, empirically_contingent).
narrative_ontology:cs_reference_frame('113d6e04-1dc8-45ba-aee1-dd98fafd0cb2', copyright_as_licensing_revenue_system).
narrative_ontology:cs_drift_state('113d6e04-1dc8-45ba-aee1-dd98fafd0cb2', contemporary_digital_platform_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('113d6e04-1dc8-45ba-aee1-dd98fafd0cb2', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, copyright_owners).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, licensing_intermediaries).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, educational_institutions).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, non_commercial_creators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, cultural_commons_users).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, researchers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__market_licensing_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(fair_use_statutory_exception__market_licensing_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__market_licensing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__market_licensing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.92 at interval end) because the reading systematically converts every use that generates value—every use a licensing mechanism could serve—into an obligatory licensing transaction. There is no residual fair use protection for valuable uses; the doctrine exists only at the margins where licensing is economically impossible. Suppression is also very high (0.88) because enforcement depends on aggressively narrowing fair use through judicial interpretation, legislative amendment, and technological control (DRM-backed licensing). The measurement series shows both metrics rising over time: extractiveness climbs as licensing platforms mature and can monetize increasingly granular uses; suppression rises as enforcement infrastructure hardens. Theater ratio is moderate-low (0.41) because the licensing-market logic presents itself as statutory interpretation (courts cite factor 4; copyright owners frame it as protecting incentives) but the functional effect is revenue extraction—the performative element is the framing as law rather than policy choice. Accessibility collapse is high (0.79) because once courts adopt the licensing-market logic, alternatives (robust fair use protection, open licensing norms, statutory licensing for certain categories) become legally unavailable or politically foreclosed; the reading's own judicial adoption narrows the perceived option space. Resistance is substantial (0.72) because educational institutions, researchers, non-commercial creators, and civil society organizations actively contest this reading; there is a live legal debate (some courts and jurisdictions still apply fair use more robustly) and legislative lobbying for alternative doctrines.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (copyright owners and licensing intermediaries) and the court seat experience this constraint radically differently from the payer and victim seats. From the copyright-owner and licensing-intermediary perspective, this is a coordination mechanism: fair use doctrine that protects their licensing markets. They frame it as necessary copyright incentive protection. From the payer and victim seats (educators, researchers, non-commercial creators, archivists), the same doctrine is pure extraction: it converts legitimate reuse into a gatekeeping and fee obligation, suppressing downstream creation and access. The engine's per-seat classification computation should capture this: the copyright owner seats should compute as beneficiaries with low directionality; the payer seats should compute as targets with high directionality. The constraint's claim (tangled_rope) reflects this asymmetry: there is a coordination story (protecting incentives to create original works) that licenses copyright owners' perspective, but the actual operation is asymmetric extraction (forcing payers to license uses that fair use once protected) with active enforcement (courts and licensing platforms). The asymmetry is the tangled_rope signature.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright owners and licensing intermediaries are structural beneficiaries (d near 0.0): they collect from the licensing transactions the reading enables; they control the agenda through their lobbying and their role in copyright litigation; their exit options are arbitrage (they could abandon stricter licensing and return to lower-fee competition, but do not). Educational institutions, researchers, non-commercial creators, and cultural commons users are structural targets (d near 1.0): they bear the cost of licensing obligations or cease their activities; their exit is constrained (they cannot avoid copyright law) or identity-locked (their institutional or creative identity depends on access to cultural materials). Courts sit between beneficiary and symmetric (d moderately low): they are supposed to be neutral interpreters, but they have been consistently persuaded by copyright-owner framing of the licensing-market test. The copyright statute authority (legislature) is an observer (d analytical): the statute does not mandate the licensing-market logic; it empowers courts to weigh factors. No directionality override is needed; the derived d values should track the structural data accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids a mandatrophy problem by reinterpreting the statute rather than explicitly abandoning its purpose. The fair use statute was enacted to prevent copyright overreach and preserve space for follow-on creation, transformation, and access. This reading interprets the statute to achieve the opposite—maximizing copyright control and licensing extraction. Yet because the reinterpretation uses statutory language (factor 4, market harm), it appears to honor the statute rather than contradict it. The mandatrophy is latent: the founding problem (prevent copyright overreach) is contradicted by the reading's operation, but the contradiction is obscured by doctrinal framing. The constraint persists not because the founding problem is solved but because the framing inverts the statute's purpose without naming the inversion. This is the classic piton and mandatrophy signature: a former coordination mechanism (fair use as a right-calibration tool) whose function has atrophied (it no longer calibrates; it merely marks the margins) but whose appearance persists (courts still cite the four-factor test, legislature has not repealed fair use). The theater ratio (0.41) reflects this: there is still genuine function (fair use does protect genuinely un-monetizable uses), but an increasing share of enforcement effort is theater—the appearance of balancing when the outcome (licensing transaction requirement) is foreordained.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    licensing_market_existence_and_hypothetical,
    'Does ''licensing market exists'' include hypothetical licensing markets that do not actually exist but could be established? Can a rights holder create a licensing opportunity retroactively by declaring a willingness to license?',
    'Case law clarification: do courts require an actual, extant licensing market, or do they infer market harm from theoretical licensing possibility? Empirical study of licensing platforms'' creation of monetization for previously unmonetized uses.',
    'If actual licensing markets are required, fair use survives where no real licensing infrastructure exists (research, archival, preservation, many non-commercial uses). If hypothetical markets suffice, fair use collapses entirely—any use a rights holder claims they could license loses protection. This reading assumes hypothetical markets suffice, which is the most extractive interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(licensing_market_existence_and_hypothetical, conceptual, 'The boundary between actual and hypothetical licensing markets in the fair use analysis.').

omega_variable(
    statutory_factor_hierarchy_and_discretion,
    'Does 17 U.S.C. § 107''s requirement to weigh four factors permit courts to treat factor (4)—market harm to licensing—as dispositive, or must courts give genuine weight to factors (1) transformative purpose and (2) amount used?',
    'Statutory interpretation by appellate courts and legislative clarity: does Congress intend factor (4) to dominate, or is it one of four equally weighted considerations? Comparative analysis of how courts balance factors in practice.',
    'If factors are equally weighted, fair use depends on the full four-factor balance, and transformative educational/research/preservation uses can prevail despite licensing-market harm. If factor (4) is dispositive, the licensing-market reading follows structurally from statutory construction. This reading assumes (4) is dispositive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(statutory_factor_hierarchy_and_discretion, conceptual, 'Whether factor (4) in the fair use statute is one of four equal considerations or the dispositive test.').

omega_variable(
    licensing_revenue_vs_copyright_incentive_purpose,
    'What is copyright law''s primary purpose: maximizing licensing revenue for rights holders, or incentivizing original creation by granting limited monopolies? Does protecting licensing revenue serve original-creation incentives, or does it subordinate incentives to revenue extraction?',
    'Statutory history and legislative purpose: examination of the Copyright Clause (U.S. Const. art. I, § 8) and the Preamble to the Copyright Act. Economic analysis of the relationship between fair use protection and original-work creation incentives. Empirical study of whether licensing-market pressure increases or decreases original creation.',
    'If copyright''s purpose is original-creation incentives, fair use should protect downstream creativity and cultural participation, even if that reduces licensing revenue. If copyright''s purpose is licensing-revenue maximization, the market-licensing reading follows. The reading assumes the latter interpretation, which is contestable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_revenue_vs_copyright_incentive_purpose, empirical, 'Whether maximizing licensing revenue serves the Copyright Clause''s purpose of incentivizing original creation.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of fair use primarily structural (courts and licensing platforms enforce the licensing-market logic through institutional architecture) or internalized (educators, researchers, and creators internalize the reading and avoid fair use uses even where the legal argument is contestable)?',
    'Post-reading-clarity suppression trajectory: if the reading is explicitly rejected and fair use is restored, do educators and researchers immediately resume fair use practices, or does the suppression persist through internalized caution and legal fear?',
    'If suppression is primarily structural, restoring fair use doctrine would rapidly reactivate fair use practices. If suppression is internalized, institutional and individual behavior would persist even with doctrinal change; the constraint would function as a piton through internalized belief rather than active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether fair use suppression is structural (institutional enforcement) or internalized (psychological/behavioral internalization).').

omega_variable(
    reading_as_doctrine_vs_reading_as_interpretation,
    'Is this constraint a doctrine courts have adopted (an authoritative reading of the statute), or is it a contestable interpretation among several live options? How firm is the judicial consensus on the licensing-market test?',
    'Appellate case law review: survey of how different federal and state courts apply the four-factor test. Legislative proposals to clarify fair use doctrine. International comparison with copyright systems that explicitly protect fair use independent of licensing-market analysis.',
    'If the licensing-market reading is firmly established doctrine, it is entrenched and structural. If it is a contestable interpretation, it can be reversed by court opinion or statutory amendment. Different jurisdictions show different doctrinal maturity; the U.S. shows higher firmness, many international systems show greater protection for educational and research uses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_as_doctrine_vs_reading_as_interpretation, empirical, 'Whether the market-licensing reading is firm doctrine or contestable interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__market_licensing_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(fair_tr_t5, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(fair_tr_t10, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(fair_tr_t15, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement(fair_tr_t20, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(fair_tr_t25, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 25, 0.38).
narrative_ontology:measurement(fair_tr_t30, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(fair_tr_t40, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(fair_be_t5, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 5, 0.74).
narrative_ontology:measurement(fair_be_t10, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 10, 0.79).
narrative_ontology:measurement(fair_be_t15, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 15, 0.84).
narrative_ontology:measurement(fair_be_t20, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 20, 0.88).
narrative_ontology:measurement(fair_be_t25, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 25, 0.9).
narrative_ontology:measurement(fair_be_t30, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 30, 0.91).
narrative_ontology:measurement(fair_be_t40, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 40, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t0, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(fair_su_t5, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 5, 0.76).
narrative_ontology:measurement(fair_su_t10, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 10, 0.79).
narrative_ontology:measurement(fair_su_t15, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 15, 0.82).
narrative_ontology:measurement(fair_su_t20, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 20, 0.84).
narrative_ontology:measurement(fair_su_t25, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 25, 0.86).
narrative_ontology:measurement(fair_su_t30, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 30, 0.87).
narrative_ontology:measurement(fair_su_t40, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 40, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__market_licensing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_statutory_exception__market_licensing_reading, 0.18).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception__transformative_right_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception__narrow_defense_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, copyright_licensing_platform_economics).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, derivative_work_creation_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the fair use statutory exception kernel. The sibling readings (transformative_right_reading and narrow_defense_reading) are separate constraint stories with different ε values, beneficiary/victim structures, and classifications. They share a common kernel (17 U.S.C. § 107) but instantiate different interpretations with different structural consequences. All three stories are linked via network.affects_constraints and together form the fair_use_kernel_family. The decomposition reflects ε-invariance principle DP-001: one reading, one constraint, one ε-value per story; alternative readings are alternative constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_statutory_exception__market_licensing_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
