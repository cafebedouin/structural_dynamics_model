% ============================================================================
% CONSTRAINT STORY: us_constitution_text__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__originalist_reading, []).

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
 *   constraint_id: us_constitution_text__originalist_reading
 *   human_readable: Originalist Reading of Constitutional Meaning (Fixed at Ratification)
 *   domain: constitutional_law/legal_philosophy
 *
 * SUMMARY:
 *   This story generates the originalist reading of the contested
 *   constitutional-meaning kernel: the claim that constitutional text's
 *   meaning was fixed at ratification and that legitimate interpretation is
 *   the recovery of the original public understanding of that text, with
 *   post-ratification practice relevant only as evidence of that original
 *   meaning, not as an independent source of evolving content. This is one of
 *   three structurally distinct constraints instantiated from the same kernel
 *   (us_constitution_text); the living-constitutionalist reading and the
 *   positivist reading are separate constraint files with their own epsilon
 *   values and stakeholder structures, not observable-dependent variants of
 *   this one. The originalist reading has moved, over the 1980-2024 interval,
 *   from an insurgent academic critique of judicial activism into the
 *   controlling methodology of a Supreme Court majority, which is the primary
 *   driver of both rising measured extraction and rising suppression
 *   requirement in this file.
 *
 * KEY AGENTS:
 *   - conservative_legal_movement: institutional agenda-setter and primary beneficiary, arbitrage-grade exit via appointment pipeline
 *   - originalist_appointed_judiciary: agenda-setter administering the doctrine in live controversies
 *   - rights_claimants_without_founding_era_analogue: powerless, trapped payer bearing the doctrine's exclusionary evidentiary frame
 *   - reproductive_rights_claimants and lgbtq_rights_claimants: powerless, trapped payers whose previously recognized interests are structurally disfavored
 *   - constitutional_law_historians: analytical observers assessing whether the historical record the doctrine claims to recover is genuinely determinate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__originalist_reading, 0.61).
domain_priors:suppression_score(us_constitution_text__originalist_reading, 0.72).
domain_priors:theater_ratio(us_constitution_text__originalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__originalist_reading, "Originalist Reading of Constitutional Meaning (Fixed at Ratification)").
narrative_ontology:topic_domain(us_constitution_text__originalist_reading, "constitutional_law/legal_philosophy").

domain_priors:requires_active_enforcement(us_constitution_text__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__originalist_reading, '92102011-f935-4c45-8607-7ec863e07c7b').
narrative_ontology:cs_kernel_codification('92102011-f935-4c45-8607-7ec863e07c7b', fixed_text).
narrative_ontology:cs_authority_grounding('92102011-f935-4c45-8607-7ec863e07c7b', lineage).
narrative_ontology:cs_interpretation_layer_present('92102011-f935-4c45-8607-7ec863e07c7b').
narrative_ontology:cs_reading_relation('92102011-f935-4c45-8607-7ec863e07c7b', us_constitution_text__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('92102011-f935-4c45-8607-7ec863e07c7b', us_constitution_text__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('92102011-f935-4c45-8607-7ec863e07c7b', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('92102011-f935-4c45-8607-7ec863e07c7b', meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('92102011-f935-4c45-8607-7ec863e07c7b', foundational, original_public_understanding_recoverable_via_historical_evidence).
narrative_ontology:cs_axiom_status(original_public_understanding_recoverable_via_historical_evidence, holdable).
narrative_ontology:cs_axiom_grounding('92102011-f935-4c45-8607-7ec863e07c7b', original_public_understanding_recoverable_via_historical_evidence, empirically_contingent).
narrative_ontology:cs_axiom('92102011-f935-4c45-8607-7ec863e07c7b', secondary, post_ratification_practice_evidentiary_only).
narrative_ontology:cs_axiom_status(post_ratification_practice_evidentiary_only, holdable).
narrative_ontology:cs_axiom_grounding('92102011-f935-4c45-8607-7ec863e07c7b', post_ratification_practice_evidentiary_only, conventional).
narrative_ontology:cs_reference_frame('92102011-f935-4c45-8607-7ec863e07c7b', ratification_era_public_meaning).
narrative_ontology:cs_drift_state('92102011-f935-4c45-8607-7ec863e07c7b', contemporary_administrative_state_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('92102011-f935-4c45-8607-7ec863e07c7b', '').
narrative_ontology:cs_kernel_id(us_constitution_text__originalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, conservative_legal_movement).
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, originalist_appointed_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, historically_incumbent_property_and_gun_interests).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, rights_claimants_without_founding_era_analogue).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, reproductive_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, lgbtq_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, administrative_agencies_and_regulatory_beneficiaries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, general_public_subject_to_rulings).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, general_public_subject_to_rulings).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Built the interpretive methodology over five decades through law schools, the Federalist Society pipeline, and judicial appointments; now controls a durable majority on the Supreme Court that applies the method. Sets which historical sources count as evidence of original public meaning and administers the doctrine's application in live cases. Gains institutional dominance and the ability to lock in outcomes that resist future legislative or electoral reversal.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, conservative_legal_movement, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__originalist_reading, conservative_legal_movement, beneficiary).

% Federal judges selected in part for commitment to the methodology apply it to decide live controversies. They frame their role as constrained by history rather than policy-making, which shields individual rulings from the charge of judicial activism while producing a stable, movement-aligned jurisprudential trajectory across cases.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, originalist_appointed_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Bring claims to rights or protections that have no clear 18th- or 19th-century analogue — reproductive autonomy, gender-affirming care, novel forms of surveillance, gun regulations addressing technologies unknown at ratification. Their claims are evaluated against a historical record compiled by a specific interest and often fail not because the underlying interest is weak but because the evidentiary frame excludes it by design. They cannot exit the constitutional system; their only recourse is amendment (practically unavailable) or waiting out judicial composition.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, rights_claimants_without_founding_era_analogue, payer,
    powerless, biographical, trapped, national).

% Had a recognized constitutional liberty interest reversed when the doctrine was applied to find no textual or historical grounding for it at the time of the Fourteenth Amendment's ratification. Now subject to a patchwork of state law with no federal constitutional floor. Exit means relocation across state lines, available only to the mobile and resourced among them.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, reproductive_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Rights recognized under substantive due process and equal protection reasoning face renewed vulnerability because that reasoning is structurally disfavored under a methodology skeptical of unenumerated rights not evidenced in founding-era practice, when such relationships were criminalized or unrecognized.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, lgbtq_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Regulatory regimes built on twentieth-century constitutional doctrine (Commerce Clause expansion, administrative deference) face erosion as originalist reasoning narrows the historical warrant for federal regulatory power. Agencies can litigate and lobby but cannot exit the constitutional order that authorizes them.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, administrative_agencies_and_regulatory_beneficiaries, payer,
    organized, generational, constrained, national).

% Academic and judicial voices who hold that constitutional meaning evolves with society, or that validity rests on enactment procedure rather than reconstructed historical intent, are structurally sidelined once originalist methodology captures the deciding bench — their arguments are heard in dissents and law reviews but do not control outcomes regardless of their doctrinal merit.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, living_constitutionalist_and_positivist_legal_scholars, excluded,
    organized, generational, constrained, national).

% Lives under the resulting body of constitutional law without having chosen the interpretive methodology through any direct democratic act; benefits from those aspects of the doctrine that produce outcomes they favor (e.g., certain gun-rights or property protections) and bears costs from those that do not. Cannot exit national constitutional jurisdiction short of emigration.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, general_public_subject_to_rulings, payer,
    moderate, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__originalist_reading, general_public_subject_to_rulings, beneficiary).

% Study whether the historical record the doctrine relies on is genuinely determinate or is itself contested, selective, and capable of supporting multiple conclusions — their findings on indeterminacy bear directly on whether 'original public meaning' functions as a real constraint or as a rhetorical structure that legitimates outcomes reached on other grounds.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, constitutional_law_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_text__originalist_reading, conservative_legal_movement).
narrative_ontology:fixing_cost_class(us_constitution_text__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides judges a purportedly neutral, non-discretionary decision procedure — recover the historical public meaning of the text — that constrains judicial policy-making and offers predictability against the charge that judges simply impose their own values under the guise of interpretation.
% TRANSFER_FUNCTION: Moves interpretive authority and the power to fix constitutional meaning away from contemporary democratic majorities and living-constitutionalist courts, and toward historical evidentiary records curated and litigated by movement-aligned scholars and advocates; moves substantive legal protection away from claimants whose interests have no founding-era analogue and toward claimants whose interests track founding-era social arrangements.
% ABSENT_VOICES: The claimants who would benefit from evolving interpretation — and the historians who would testify that the founding-era record is far less determinate than the doctrine assumes — are heard in dissents, briefs, and scholarship but do not sit on the deciding bench in cases where the methodology controls the outcome.
% DISAPPEARANCE_RATIONALE: If the requirement that constitutional meaning be fixed at ratification and recovered through original public understanding vanished as an interpretive rule, courts would default to more overtly evolving or purposive modes of interpretation already available in the tradition (living constitutionalism, purposivism, common-law constitutionalism); doctrines currently reversed or narrowed under originalist reasoning (substantive due process protections, Commerce Clause scope, administrative deference) would be open to reconsideration on different footing, and the institutional advantage currently held by the conservative legal movement's investment in historical-methodology infrastructure would substantially depreciate.
% FOUNDING_PROBLEM: The methodology was advanced to solve a perceived legitimacy problem: unelected judges appearing to substitute their own moral and policy preferences for law under vague standards like 'liberty' or 'equal protection,' undermining democratic self-governance and judicial restraint.
% FOUNDING_PROBLEM_CORROBORATION: Originalist judges and movement scholars attest the legitimacy problem remains live and the methodology solves it. Legal historians and comparative-method scholars outside the movement — including originalist-sympathetic academics who have documented the indeterminacy and selectivity of historical records used in practice — attest that the methodology does not eliminate discretion but relocates and obscures it behind a historical-evidence framing, and that outcomes still track the political commitments of the judges selecting and weighing the historical sources.
narrative_ontology:disappearance_verdict(us_constitution_text__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_text__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__originalist_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_text__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_text__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored as substantial and rising (0.22 to 0.61) because the doctrine has moved from a minority methodological position to the operative decision rule of the controlling judicial coalition, converting a contestable interpretive theory into outcome-determinative practice for an increasing share of constitutional adjudication. Suppression is authored higher still (reaching 0.72) because the doctrine's persistence depends on treating rival interpretive methods (living constitutionalism, purposivism, precedent-based stare decisis reasoning) as illegitimate departures from law rather than as competing, defensible traditions — this is an active suppression of interpretive alternatives, not merely disagreement about outcomes. Theater ratio is kept comparatively low (0.28) because the historical-research function is often genuinely performed (real archival and corpus-linguistics work occurs) even where the framework itself is contested; the doctrine is not primarily theatrical, it is substantively operative and that is what makes it consequential.
 *
 * DIRECTIONALITY LOGIC:
 *   The conservative legal movement and the originalist judiciary sit near the beneficiary end: they set the interpretive rule, administer its application, and their institutional position (appointments, doctrine-shaping opinions) is durable against ordinary political reversal. Claimants without founding-era analogues sit near the full-target end: they are trapped (national constitutional jurisdiction has no exit), and the evidentiary frame that decides their claims was substantially built by the party that benefits from a particular pattern of outcomes. The general public sits closer to symmetric — some benefit from originalist outcomes (gun rights, some property and federalism protections), others bear costs (reproductive rights, LGBTQ rights, administrative governance capacity), and no single directionality captures the whole class, which is why it is authored as both beneficiary and payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The methodology's founding problem — unelected judges substituting personal preference for law — remains genuinely live as a legitimacy concern in any system of judicial review; that is why founding_problem_status is authored as contested rather than dead. What distinguishes coordination from extraction here is not whether the underlying problem is real but whether the proposed solution actually eliminates the discretion it claims to constrain, or merely relocates it into the selection and weighting of historical sources — a question the constitutional_law_historians observer seat is positioned to assess and on which corroboration from outside the movement diverges sharply from the movement's own self-assessment. Classifying this as tangled_rope rather than snare or mountain preserves that the coordination function (a decision procedure with some real constraining force, genuine historical scholarship) coexists with asymmetric extraction (a specific class of claimants systematically loses under the doctrine's own operation) — collapsing it to snare would erase the genuine methodological content; collapsing it to mountain (as the doctrine's own rhetoric of 'just reading the text' invites) would erase the demonstrable beneficiary structure and the enforcement dependency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_record_determinacy,
    'Is the 18th/19th century historical record the doctrine relies on genuinely determinate enough to constrain judicial discretion, or is it sufficiently contested and selective that ''original public meaning'' functions as a rhetorical legitimating frame for outcomes reached on other grounds?',
    'Systematic comparison of competing historical accounts submitted in the same cases by different amici and justices; corpus-linguistics and historiographic analysis of whether the selected sources represent a genuine consensus reading or a curated subset; tracking whether the same interpretive method reliably produces outcomes independent of which judge applies it.',
    'If largely indeterminate, the doctrine''s coordination claim (a neutral, discretion-constraining decision procedure) substantially fails, and the classification should weight more heavily toward the extractive pole even though enforcement and beneficiary structure remain unchanged. If substantially determinate, the coordination function is more real than critics allow.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_record_determinacy, empirical, 'Whether original-public-meaning evidence actually constrains outcomes or merely legitimates them.').

omega_variable(
    kernel_reading_selection_and_sibling_relationship,
    'This story instantiates the originalist reading of a three-way contested kernel (originalist, living-constitutionalist, positivist). Is the choice to treat these as three coequal, mutually exclusive readings itself defensible, or do some pairs actually coexist within a single judge''s working method (e.g., an originalist judge who also relies on positivist enactment-procedure reasoning for justiciability questions)?',
    'Survey of actual judicial opinions for mixed-methodology reasoning; doctrinal scholarship on whether originalism and positivism are logically compatible (both can hold formal enactment procedure fixes validity AND that ratification-era meaning fixes content) versus originalism and living constitutionalism (which more directly conflict on whether meaning can change post-ratification).',
    'If originalism and positivism substantially coexist within single frameworks rather than merely coexisting across different parties, the reading_relations declared here (coexists_with for both siblings) may need refinement — the positivist relationship may be closer to a background compatibility condition than a competing reading in the same sense as living constitutionalism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_and_sibling_relationship, conceptual, 'Whether the three-way kernel decomposition draws the reading boundaries in the structurally correct place.').

omega_variable(
    originalism_versus_conservative_outcome_correlation,
    'Is originalist methodology producing outcomes that favor conservative legal movement priorities because the historical record genuinely supports those outcomes, or because the methodology was selected and refined by movement actors substantially because it reliably produces preferred outcomes (a form of motivated methodological selection)?',
    'Examine cases where rigorous application of originalist method would predict outcomes contrary to conservative policy preferences, and assess how frequently such outcomes actually occur versus how frequently the methodology is adjusted, distinguished, or supplemented with other reasoning when it would cut the other way.',
    'A high rate of outcome-methodology alignment with low rate of counter-preference application would support the tangled_rope classification''s extraction component; a genuinely high rate of counter-preference outcomes would support treating the doctrine as closer to a rope with incidental correlation rather than tangled_rope with structural capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(originalism_versus_conservative_outcome_correlation, empirical, 'Whether the doctrine''s political valence reflects genuine historical constraint or selection effects in its adoption and application.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__originalist_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1980, us_constitution_text__originalist_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(us_c_tr_t1990, us_constitution_text__originalist_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_text__originalist_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_text__originalist_reading, theater_ratio, 2010, 0.23).
narrative_ontology:measurement(us_c_tr_t2018, us_constitution_text__originalist_reading, theater_ratio, 2018, 0.26).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_text__originalist_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1980, us_constitution_text__originalist_reading, base_extractiveness, 1980, 0.22).
narrative_ontology:measurement(us_c_be_t1990, us_constitution_text__originalist_reading, base_extractiveness, 1990, 0.31).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_text__originalist_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_text__originalist_reading, base_extractiveness, 2010, 0.47).
narrative_ontology:measurement(us_c_be_t2018, us_constitution_text__originalist_reading, base_extractiveness, 2018, 0.55).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_text__originalist_reading, base_extractiveness, 2024, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1980, us_constitution_text__originalist_reading, suppression_requirement, 1980, 0.35).
narrative_ontology:measurement(us_c_su_t1990, us_constitution_text__originalist_reading, suppression_requirement, 1990, 0.44).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_text__originalist_reading, suppression_requirement, 2000, 0.52).
narrative_ontology:measurement(us_c_su_t2010, us_constitution_text__originalist_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(us_c_su_t2018, us_constitution_text__originalist_reading, suppression_requirement, 2018, 0.67).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_text__originalist_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, positivist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint files decomposed from the single natural-language label 'constitutional interpretation methodology,' per the ε-invariance principle: the label conflates three structurally distinct claims about what fixes constitutional meaning (historical original understanding, evolving societal principle, or formal enactment procedure). Each reading carries its own epsilon, beneficiary/victim structure, and classification, linked here via affects_constraints. The originalist reading (this file) forecloses the living-constitutionalist reading's core premise within a single judicial framework (a judge cannot simultaneously hold that meaning is fixed at ratification and that it legitimately evolves with society) while coexisting with the positivist reading (a judge can hold both that validity derives from formal enactment procedure AND that content is fixed at ratification — these operate on different questions, validity versus meaning).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
