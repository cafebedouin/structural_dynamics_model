% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__narrow_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: fair_use_statutory_exception__narrow_defense_reading
 *   human_readable: Fair Use as Narrowly-Construed Affirmative Defense (Property-Primacy Reading)
 *   domain: legal/economic
 *
 * SUMMARY:
 *   This story instantiates one specific reading of the fair use kernel (17
 *   U.S.C. §107): copyright is the property default, fair use is an
 *   affirmative defense the defendant must establish, and market-effect
 *   analysis (the fourth statutory factor) is treated as substantially
 *   determinative — with commercial purpose weighing heavily against the
 *   defendant even where the use is transformative. Under this reading, the
 *   four-factor balancing test collapses in practice toward a
 *   market-harm-primacy heuristic: any use a rightsholder could plausibly
 *   have licensed is treated as harming a licensing market, regardless of
 *   whether that market currently exists. This is NOT a claim about fair use
 *   in general — it is one structurally distinct reading among (at least)
 *   three that courts and litigants actually deploy. The transformative-right
 *   reading and the market-licensing reading are separate constraints with
 *   different ε values, different beneficiary/victim structures, and
 *   different classifications; they are linked here via
 *   network.affects_constraints and documented as siblings, not folded into
 *   this story's metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, 0.71).
domain_priors:suppression_score(fair_use_statutory_exception__narrow_defense_reading, 0.66).
domain_priors:theater_ratio(fair_use_statutory_exception__narrow_defense_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__narrow_defense_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__narrow_defense_reading, "Fair Use as Narrowly-Construed Affirmative Defense (Property-Primacy Reading)").
narrative_ontology:topic_domain(fair_use_statutory_exception__narrow_defense_reading, "legal/economic").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__narrow_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__narrow_defense_reading, '5b553985-59ac-4d80-9526-b542e044c283').
narrative_ontology:cs_kernel_codification('5b553985-59ac-4d80-9526-b542e044c283', fixed_text).
narrative_ontology:cs_authority_grounding('5b553985-59ac-4d80-9526-b542e044c283', lineage).
narrative_ontology:cs_interpretation_layer_present('5b553985-59ac-4d80-9526-b542e044c283').
narrative_ontology:cs_reading_relation('5b553985-59ac-4d80-9526-b542e044c283', fair_use_statutory_exception__transformative_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('5b553985-59ac-4d80-9526-b542e044c283', fair_use_statutory_exception__market_licensing_reading, influences).
narrative_ontology:cs_axiom('5b553985-59ac-4d80-9526-b542e044c283', foundational, copyright_is_property_default).
narrative_ontology:cs_axiom_status(copyright_is_property_default, holdable).
narrative_ontology:cs_axiom_grounding('5b553985-59ac-4d80-9526-b542e044c283', copyright_is_property_default, conventional).
narrative_ontology:cs_axiom('5b553985-59ac-4d80-9526-b542e044c283', foundational, market_effect_factor_is_determinative).
narrative_ontology:cs_axiom_status(market_effect_factor_is_determinative, holdable).
narrative_ontology:cs_axiom_grounding('5b553985-59ac-4d80-9526-b542e044c283', market_effect_factor_is_determinative, instrumental).
narrative_ontology:cs_axiom('5b553985-59ac-4d80-9526-b542e044c283', secondary, defendant_bears_burden_of_defense).
narrative_ontology:cs_axiom_status(defendant_bears_burden_of_defense, holdable).
narrative_ontology:cs_axiom_grounding('5b553985-59ac-4d80-9526-b542e044c283', defendant_bears_burden_of_defense, conventional).
narrative_ontology:cs_reference_frame('5b553985-59ac-4d80-9526-b542e044c283', market_harm_primacy_framework).
narrative_ontology:cs_drift_state('5b553985-59ac-4d80-9526-b542e044c283', post_digital_licensing_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5b553985-59ac-4d80-9526-b542e044c283', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, incumbent_rightsholders).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, content_licensing_intermediaries).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, independent_commentators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, documentary_filmmakers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, digital_archivists).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, remix_and_sampling_artists).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__narrow_defense_reading, copyright_as_property_doctrine).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__narrow_defense_reading, market_harm_primacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major studios, publishers, and record labels hold large copyright portfolios and litigate aggressively to keep fair use construed as a narrow, defendant-bears-burden exception. They fund the litigation strategy that shapes precedent, lobby for statutory language reinforcing market-harm primacy, and license derivative-use rights as a revenue stream that a broader fair use reading would erode. Their exit from the constraint is arbitrage: they can license, litigate, or settle depending on which maximizes portfolio value.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, incumbent_rightsholders, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__narrow_defense_reading, incumbent_rightsholders, agenda_setter).

% Clearance houses, stock-footage licensors, and rights-clearance services profit directly from the presumption that any commercially conceivable use should be licensed rather than defended as fair use. A narrow defense reading expands their addressable market by shrinking the space where creators can simply proceed without paying.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, content_licensing_intermediaries, beneficiary,
    organized, biographical, mobile, national).

% Bloggers, critics, and educators who quote or excerpt copyrighted material for commentary face takedowns and cease-and-desist letters they cannot afford to litigate against. Under this reading, the commercial context of their platform (ad revenue, subscriptions) counts heavily against them regardless of transformative purpose, and the burden falls on them to prove the defense rather than on the claimant to prove harm. Their only realistic exit is self-censorship or removal.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, independent_commentators, payer,
    powerless, immediate, trapped, national).

% Rely on archival footage, music, and quoted works to build factual narratives. Under the narrow defense reading, insurers and distributors demand pre-clearance of anything a court might later find infringing because the fair use outcome is unpredictable and expensive to litigate, so filmmakers pay licensing fees for material a transformative-use standard would likely excuse without payment. Their exit is constrained: they can pay, cut the material, or risk distribution refusal.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, documentary_filmmakers, payer,
    moderate, biographical, constrained, national).

% Libraries and preservation projects digitizing out-of-commercial-print works face infringement exposure because market-harm analysis treats any potential future licensing market as dispositive, even for works with no active market today. They must choose between narrowing collections, paying for uncertain licenses, or accepting litigation risk that institutional counsel routinely advises against.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, digital_archivists, payer,
    moderate, generational, constrained, global).

% Musicians and video creators who sample or remix existing works are treated as presumptively infringing once any commercial distribution is involved, with transformativeness discounted relative to the potential licensing market the underlying rightsholder could have captured. Sampling clearance costs frequently exceed what independent artists can pay, foreclosing the work entirely rather than permitting an unlicensed transformative use.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, remix_and_sampling_artists, payer,
    powerless, immediate, trapped, global).

% Apply the four-factor test but, under this reading, treat the fourth factor (market effect) as substantially determinative and commercial purpose as a strong thumb on the scale against the defendant. Courts administer the doctrine and could reweight the factors, but institutional stare decisis and rightsholder litigation resources keep the narrow construction stable across circuits.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, federal_courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Academics and public-interest amici document the doctrinal drift toward market-harm primacy and file briefs urging courts to weigh transformativeness more heavily, but their influence on precedent is indirect and slow relative to the resourced litigation of rightsholder plaintiffs.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, legal_scholars_and_amici, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__narrow_defense_reading, incumbent_rightsholders).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__narrow_defense_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a predictable rule for adjudicating unauthorized uses of copyrighted material by treating copyright as a property default and fair use as a narrow, burden-shifted exception — this reduces litigation uncertainty for rightsholders and gives courts a stable, market-value-anchored heuristic instead of an open-ended balancing test.
% TRANSFER_FUNCTION: Moves the practical value of contested uses (commentary, documentary excerpts, archival preservation, sampling) from downstream creators and the public domain of practice toward incumbent rightsholders and the licensing intermediaries who monetize the resulting clearance demand.
% ABSENT_VOICES: Independent creators, students, and cultural commentators who would be the primary beneficiaries of a transformative-use-forward reading are rarely parties with resources to litigate to the circuit-court level; the doctrine is shaped almost entirely by well-resourced rightsholder litigants and the courts, with public-interest amici arriving after precedent has hardened.
% DISAPPEARANCE_RATIONALE: If this narrow-defense reading disappeared and courts weighted transformativeness and public benefit more heavily by default, licensing intermediaries would lose a substantial share of clearance revenue, documentary and archival institutions would digitize and quote far more freely without pre-clearance insurance overhead, and remix culture would operate with materially lower legal risk — the current clearance economy is built on top of this specific doctrinal weighting.
% FOUNDING_PROBLEM: Fair use was codified (17 U.S.C. §107) to preserve judicial flexibility for uses that serve criticism, comment, scholarship, and research without requiring case-by-case legislative carve-outs, while still protecting authors' incentive to create by preventing market-destroying appropriation.
% FOUNDING_PROBLEM_CORROBORATION: Rightsholder litigants and licensing intermediaries attest the market-harm-primacy reading is necessary to preserve creative incentives. Independent of that group, legal scholars, library associations, and documentary filmmaker guilds — parties outside the benefiting coalition — attest through amicus filings and law review literature that the founding statutory purpose (protecting transformative and noncommercial-adjacent uses) has been substantially displaced by a market-effect test that treats any conceivable license as dispositive, a reading not compelled by the statutory text itself.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__narrow_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__narrow_defense_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__narrow_defense_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_statutory_exception__narrow_defense_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__narrow_defense_reading, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.71) is high and rising because the doctrinal drift over the interval has been toward stronger deference to speculative licensing markets (post-1994 Campbell allowed transformativeness in principle but subsequent circuit application, especially post-2013, has re-anchored heavily on market effect and commercial character). Suppression (0.66) reflects that the practical chilling effect on unresourced defendants operates independently of any individual case's outcome — the doctrine's unpredictability and defendant-bears-burden structure suppress uses before they are ever litigated. Theater ratio is comparatively low (0.28) because courts genuinely apply a four-factor analysis in each case; the extraction is doctrinal weighting, not pure performance.
 *
 * PERSPECTIVAL GAP:
 *   From the rightsholder/court seat, this reading is coordination: a stable, administrable rule that avoids ad hoc case-by-case legislation and protects the incentive structure copyright is meant to preserve. From the payer seats (independent commentators, archivists, remix artists), the identical structure operates as narrow, coercively enforced extraction — the same four-factor test, the same case law, experienced as a toll gate rather than a shield. The engine's per-seat computation should reflect this divergence without either side's framing being treated as the story's single truth.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent rightsholders and licensing intermediaries sit near the full-beneficiary end: they capture the value of foreclosed unlicensed uses as licensing revenue and litigation leverage, and their exit options (arbitrage, mobile) reflect genuine strategic flexibility. Independent commentators and remix artists sit near the full-target end: trapped exit options (no resources to litigate, no realistic alternative distribution channel that avoids the doctrine) plus direct bearing of the transfer (removed content, foreclosed projects, paid clearance for work that arguably should be fair use). Documentary filmmakers and archivists are constrained rather than trapped — they have some capacity to negotiate, insure, or narrow scope, but still pay a real toll.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving judicial flexibility for genuinely transformative, non-market-destroying uses) remains statutorily live, but this reading's doctrinal execution has drifted toward serving a different function — protecting speculative licensing markets that did not exist at the statute's founding (e.g., algorithmic sampling clearance markets, stock-footage licensing regimes that emerged after digitization). The founding_problem_status is marked contested rather than dead because courts still invoke the original criticism/comment/scholarship purposes; but the corroborating literature from outside the beneficiary coalition documents that market-effect analysis has become dispositive in a way the 1976 codification did not compel, which is exactly the kind of divergence a narrow reading's proponents would dispute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Does the statutory text of 17 U.S.C. §107 itself compel the narrow-defense, market-harm-primacy weighting, or is that weighting a judicially constructed gloss that the transformative-right and market-licensing readings compete with on equal textual footing?',
    'Close textual and legislative-history analysis of the 1976 Act and its 1992 amendment, cross-referenced against circuit splits on how heavily the fourth factor is weighted relative to the first (purpose and character of the use).',
    'If the text is genuinely indeterminate among the three readings, this reading''s high ε reflects a contestable judicial choice rather than a textually compelled one, strengthening the case that the reading persists partly through litigation-resource asymmetry rather than doctrinal necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the narrow-defense reading is textually compelled or one contestable interpretive choice among several live readings.').

omega_variable(
    sibling_reading_structural_delta,
    'How much would ε, beneficiaries, and victims shift under the transformative_right_reading and market_licensing_reading siblings, and where exactly does the disagreement locate itself doctrinally?',
    'Comparative case analysis: code identical fact patterns (documentary excerpt use, parody, sampling, archival digitization) under each reading''s weighting and observe where outcomes diverge — the divergence point is the fourth-factor weighting and the transformativeness discount for commercial platforms.',
    'Confirms these are genuinely three distinct constraints rather than one constraint with an observer-relative parameter: the disagreement is located specifically in how much weight market-effect analysis receives relative to transformative purpose, not in a general disagreement about fair use''s existence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Locating the precise doctrinal element (fourth-factor weighting) that distinguishes this reading from its siblings.').

omega_variable(
    market_harm_speculative_vs_actual,
    'Should market harm be assessed against markets that currently exist, or against markets a rightsholder could hypothetically create through licensing — and does the narrow-defense reading''s answer (the latter) reflect settled law or an extraction-favoring drift?',
    'Track circuit court treatment of ''potential licensing markets'' over the measured interval; a rising trend of courts crediting speculative markets not yet monetized would corroborate the drift hypothesis in the temporal measurements above.',
    'If courts increasingly credit purely speculative markets, the effective ε of this reading is understated even at 0.71 — the doctrine would be extracting against markets that do not yet exist, which is a stronger extraction claim than harming an active market.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_harm_speculative_vs_actual, empirical, 'Whether market-harm analysis under this reading targets actual or merely hypothetical licensing markets.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__narrow_defense_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1976, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1976, 0.12).
narrative_ontology:measurement(fair_tr_t1994, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1994, 0.15).
narrative_ontology:measurement(fair_tr_t2005, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2005, 0.19).
narrative_ontology:measurement(fair_tr_t2013, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2013, 0.23).
narrative_ontology:measurement(fair_tr_t2019, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2019, 0.26).
narrative_ontology:measurement(fair_tr_t2024, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1976, 0.42).
narrative_ontology:measurement(fair_be_t1994, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1994, 0.5).
narrative_ontology:measurement(fair_be_t2005, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(fair_be_t2013, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2013, 0.64).
narrative_ontology:measurement(fair_be_t2019, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2019, 0.68).
narrative_ontology:measurement(fair_be_t2024, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2024, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1976, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1976, 0.38).
narrative_ontology:measurement(fair_su_t1994, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1994, 0.47).
narrative_ontology:measurement(fair_su_t2005, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2005, 0.55).
narrative_ontology:measurement(fair_su_t2013, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2013, 0.6).
narrative_ontology:measurement(fair_su_t2019, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2019, 0.63).
narrative_ontology:measurement(fair_su_t2024, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2024, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__narrow_defense_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception__transformative_right_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception__market_licensing_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language label 'fair use' per the ε-invariance principle: narrow_defense_reading (this file, tangled_rope, high ε), transformative_right_reading (lower ε, creator/public-benefit skewed), and market_licensing_reading (highest ε, fair use space nearly foreclosed). Each reading is a distinct constraint with its own beneficiary/victim structure and classification. The narrow_defense_reading is the doctrinally dominant reading in current circuit practice per the measured drift; it structurally influences the market_licensing_reading by supplying the market-harm-primacy logic that reading pushes further, and stands in tension with the transformative_right_reading which contests the same fourth-factor weighting from the opposite direction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
