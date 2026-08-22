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
 *   This story authors the originalist reading of the constitutional-meaning
 *   kernel: constitutional text carries a fixed meaning recoverable through
 *   historical evidence of ratification-era public understanding, and
 *   interpretation is the recovery of that fixed meaning rather than its
 *   adaptation. This is one reading among several sharing the same kernel
 *   text; the living-constitutionalist and positivist readings are separate
 *   constraints (not authored here) with their own ε and stakeholder
 *   structures. Since roughly 1980, originalism has moved from a minority
 *   academic position to a controlling methodology on significant portions of
 *   the federal judiciary, backed by a sustained institutional pipeline
 *   (Federalist Society, dedicated law school programs, coordinated judicial
 *   nomination vetting). The rising extraction and suppression trajectories
 *   reflect this institutionalization: what began as an argued methodological
 *   corrective has become an increasingly dominant, actively enforced
 *   interpretive regime with concrete winners and losers.
 *
 * KEY AGENTS:
 *   - originalist_judiciary: sets and administers the interpretive method (institutional/arbitrage)
 *   - conservative_legal_movement: primary beneficiary of the method's institutional dominance (organized/arbitrage)
 *   - federalist_society_pipeline: trains and selects judges who apply the method (organized/arbitrage)
 *   - rights_claimants_without_historical_analogue: bear the cost of claims lacking founding-era grounding (powerless/trapped)
 *   - marginalized_groups_excluded_from_founding_era_polity: bear the cost of a historical record they had no hand in authoring (powerless/trapped)
 *   - regulatory_agencies_relying_on_evolving_doctrine: institutional payer facing doctrinal invalidation (institutional/constrained)
 *   - living_constitutionalist_judges_and_scholars: excluded sibling-reading holders, structurally outvoted (organized/constrained)
 *   - constitutional_historians: analytical observers of the evidentiary contest (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__originalist_reading, 0.62).
domain_priors:suppression_score(us_constitution_text__originalist_reading, 0.74).
domain_priors:theater_ratio(us_constitution_text__originalist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__originalist_reading, "Originalist Reading of Constitutional Meaning (Fixed at Ratification)").
narrative_ontology:topic_domain(us_constitution_text__originalist_reading, "constitutional_law/legal_philosophy").

domain_priors:requires_active_enforcement(us_constitution_text__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__originalist_reading, '5ac893c0-c0ef-45eb-8378-08a94b13e25a').
narrative_ontology:cs_kernel_codification('5ac893c0-c0ef-45eb-8378-08a94b13e25a', fixed_text).
narrative_ontology:cs_authority_grounding('5ac893c0-c0ef-45eb-8378-08a94b13e25a', lineage).
narrative_ontology:cs_interpretation_layer_present('5ac893c0-c0ef-45eb-8378-08a94b13e25a').
narrative_ontology:cs_reading_relation('5ac893c0-c0ef-45eb-8378-08a94b13e25a', us_constitution_text__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('5ac893c0-c0ef-45eb-8378-08a94b13e25a', us_constitution_text__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('5ac893c0-c0ef-45eb-8378-08a94b13e25a', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('5ac893c0-c0ef-45eb-8378-08a94b13e25a', meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('5ac893c0-c0ef-45eb-8378-08a94b13e25a', secondary, post_ratification_practice_interpretively_irrelevant_absent_originalist_evidence).
narrative_ontology:cs_axiom_status(post_ratification_practice_interpretively_irrelevant_absent_originalist_evidence, holdable).
narrative_ontology:cs_axiom_grounding('5ac893c0-c0ef-45eb-8378-08a94b13e25a', post_ratification_practice_interpretively_irrelevant_absent_originalist_evidence, conventional).
narrative_ontology:cs_reference_frame('5ac893c0-c0ef-45eb-8378-08a94b13e25a', ratification_era_public_understanding).
narrative_ontology:cs_drift_state('5ac893c0-c0ef-45eb-8378-08a94b13e25a', contemporary_judicial_practice, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('5ac893c0-c0ef-45eb-8378-08a94b13e25a', '').
narrative_ontology:cs_kernel_id(us_constitution_text__originalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, conservative_legal_movement).
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, originalist_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, federalist_society_pipeline).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, rights_claimants_without_historical_analogue).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, marginalized_groups_excluded_from_founding_era_polity).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, regulatory_agencies_relying_on_evolving_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sits on federal courts, including the Supreme Court, and administers the interpretive method: requiring litigants to produce historical evidence of 18th/19th century public understanding before a constitutional claim can succeed. Controls which historical materials count as authoritative and how contested history is resolved. Career advancement within this judicial tradition is built on demonstrated fidelity to the method.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, originalist_judiciary, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Built institutional infrastructure (law schools, clerkship pipelines, judicial nomination vetting) around originalism as the correct method. Benefits directly when courts staffed through this pipeline invalidate regulatory and rights-expanding precedents that lack founding-era grounding. Can shift interpretive emphasis (which historical sources, which level of generality) to reach preferred outcomes while maintaining the appearance of neutral method.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, conservative_legal_movement, beneficiary,
    organized, generational, arbitrage, national).

% Trains, vets, and recommends judicial nominees fluent in originalist methodology. Its institutional relevance and influence over judicial selection depend on originalism remaining the dominant interpretive currency. Directly shapes which cases reach courts staffed by movement-aligned judges.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, federalist_society_pipeline, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__originalist_reading, federalist_society_pipeline, agenda_setter).

% Bring claims (novel privacy interests, reproductive autonomy, gender and sexual-orientation equality, administrative rulemaking authority) that lack direct 18th/19th century textual or practice-based analogues because the historical polity did not contemplate or recognize their standing as rights-bearers. Under this reading, the absence of period evidence is treated as dispositive against the claim rather than as evidence of the period's own limitations. Cannot litigate their way out of a historical record that predates their legal personhood.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, rights_claimants_without_historical_analogue, payer,
    powerless, biographical, trapped, national).

% Women, enslaved and formerly enslaved people, and other groups excluded from the franchise and full legal personhood at ratification had no voice in shaping the 'original public understanding' this reading treats as authoritative. Their descendants' constitutional claims are evaluated against a historical record their ancestors were barred from authoring. Exit requires either constitutional amendment (extremely high threshold) or a change in prevailing judicial philosophy.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, marginalized_groups_excluded_from_founding_era_polity, payer,
    powerless, generational, trapped, national).

% Administrative agencies exercising delegated authority under doctrines developed through 20th-century practice (administrative deference, expansive commerce power, implied rights of action) face invalidation or narrowing when courts require founding-era grounding for the scope of federal power. Agencies can lobby Congress for clearer statutory authorization but cannot litigate around an interpretive method that treats their operative precedents as illegitimate accretions.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, regulatory_agencies_relying_on_evolving_doctrine, payer,
    institutional, biographical, constrained, national).

% Argue that constitutional principles must be applied to contemporary circumstances the founders could not have anticipated, and that treating 18th-century social consensus as permanently binding entrenches the exclusions of that era. Their framework remains live in academic and some judicial venues but has been substantially displaced from controlling authority in the current judiciary; they are not absent from the debate but are structurally outvoted where originalist-appointed judges hold majorities.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, living_constitutionalist_judges_and_scholars, excluded,
    organized, civilizational, constrained, national).

% Study what evidence actually establishes 'original public understanding,' and frequently find the historical record ambiguous, contested, or manipulable — multiple plausible original meanings often exist, and courts sometimes select among them to reach preferred results while presenting the choice as historically compelled.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_text__originalist_reading, conservative_legal_movement).
narrative_ontology:fixing_cost_class(us_constitution_text__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate, precedent-resistant method for constraining judicial discretion: judges are supposed to be bound by recoverable historical fact rather than their own contemporary policy preferences, which in principle limits arbitrary judicial lawmaking and stabilizes expectations about what the text permits.
% TRANSFER_FUNCTION: Moves interpretive authority from evolving doctrinal consensus and legislative/administrative judgment toward historically-trained judges and the legal movement that trained them; moves the burden of proof onto novel rights claimants to produce founding-era analogues, and shifts institutional power toward courts and away from agencies and legislatures whose authority rests on post-ratification practice.
% ABSENT_VOICES: The historical polity itself excluded women, enslaved people, and non-property-holding men from participating in forming 'original public understanding' — the very evidentiary record this reading treats as authoritative was produced without their input, and their present-day descendants' claims are measured against a record their ancestors could not shape.
% DISAPPEARANCE_RATIONALE: If originalism as controlling method vanished overnight, currently-invalidated regulatory schemes and doctrines developed through 20th-century judicial practice would regain a path to validity, rights claims lacking founding-era analogues would be evaluated under different (likely more permissive) frameworks, and the institutional payoff to the conservative legal movement's decades of pipeline-building would collapse — this is not a background fact of legal reasoning but a specific, actively defended methodological commitment with concrete distributive consequences.
% FOUNDING_PROBLEM: Originalism was advanced primarily as a response to mid-20th-century judicial decisions (Warren and early Burger Court era) seen by critics as substituting judges' personal policy preferences for constitutional text — the founding problem was framed as curbing unconstrained judicial discretion and restoring democratic legitimacy to constitutional interpretation.
% FOUNDING_PROBLEM_CORROBORATION: Originalist judges and legal scholars (Scalia, Bork, and successors) attest the problem of unconstrained judicial discretion remains live and originalism is the correct remedy. Constitutional historians and living-constitutionalist scholars, positioned outside the movement's own institutions, corroborate that unconstrained discretion was a genuine historical concern but dispute that originalism actually constrains discretion in practice — noting that historical evidence is frequently indeterminate or contested, and that selection among competing historical readings reintroduces exactly the discretion the method claims to eliminate. No source entirely outside the debate (a truly disinterested arbiter) exists, since the question of what counts as legitimate constitutional method is itself part of the contest.
narrative_ontology:disappearance_verdict(us_constitution_text__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_text__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__originalist_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.62) reflects a genuine coordination function (constraining judicial discretion) riding alongside a substantial and rising transfer of interpretive authority and case outcomes to a specific legal movement and away from claimants and institutions whose position depends on post-ratification practice. Suppression (0.74) is high because the method treats absence of period evidence as dispositive rather than as a limitation of the historical record itself — this actively forecloses adaptive interpretation rather than merely disfavoring it. Theater ratio (0.38) is moderate: some proportion of 'historical evidence' presented in briefs and opinions functions as post-hoc justification for outcomes reached on other grounds, per constitutional historians' observations of indeterminate or cherry-picked historical records, but the underlying interpretive commitment is genuinely held and applied, not pure performance. Accessibility collapse (0.58) and resistance (0.71) are both substantial but not maximal — the sibling readings remain live in scholarship and in courts with different compositions, so alternatives have not fully collapsed, but active, organized resistance (living constitutionalist scholarship, litigation strategy shifts) continues.
 *
 * DIRECTIONALITY LOGIC:
 *   The originalist judiciary and the conservative legal movement sit at the beneficiary end: they set the interpretive terms and capture the institutional and doctrinal payoff when courts apply the method. Rights claimants lacking historical analogues and groups excluded from the founding-era polity sit at the target end: they are structurally disadvantaged by a method that treats their absence from history as evidence against their present claims, and they have no exit — you cannot litigate your way out of a historical record that predates your legal personhood. Regulatory agencies occupy an intermediate institutional-payer position: they retain some leverage (statutory reauthorization, administrative redesign) but face real doctrinal risk. Living constitutionalist scholars are excluded from present controlling authority but are not without voice; the exclusion is one of institutional power, not total silencing, which is why they are marked excluded rather than trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unconstrained judicial policymaking — is genuinely contested rather than either clearly live or clearly dead: originalists sincerely maintain it as live and originalism as the necessary corrective, while critics outside the movement observe that the method's own practice (selecting among indeterminate historical readings) reintroduces the discretion it claims to eliminate. This is precisely the kind of status that should NOT be resolved by fiat: the mismatch between a status claim of 'solved by fidelity to history' and a disappearance verdict of 'world_rearranges' (removing originalism would concretely redistribute outcomes) is the signal the corpus is built to surface, not evidence that the reading is a hollow shell. Treating it as either purely functional coordination or purely captured extraction would mislabel a genuinely mixed structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_record_determinacy,
    'Is the ''original public understanding'' this reading treats as authoritative actually determinate enough to constrain judicial discretion, or is the historical record sufficiently contested and incomplete that selection among competing readings reintroduces the same discretion the method claims to eliminate?',
    'Systematic review of cases where originalist judges reached divergent outcomes using the same historical materials, or where subsequent historical scholarship substantially revised the ''original understanding'' a prior ruling relied on.',
    'If the record is largely determinate, the coordination function (constraining discretion) is real and substantial, supporting a rope-leaning reading. If substantially indeterminate, the method functions primarily as a legitimating vocabulary for outcomes reached on other grounds, supporting a snare-leaning reading with the coordination story as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_record_determinacy, empirical, 'Whether the historical evidence this reading relies on is determinate enough to actually constrain, or is manipulable post-hoc justification.').

omega_variable(
    committer_kernel_reading_choice,
    'This constraint authors the originalist reading of the constitutional-meaning kernel; two sibling readings (living_constitutionalist_reading, positivist_reading) are authored as separate constraints. Where is the disagreement between readings actually located?',
    'The disagreement is located in what counts as the object of interpretation: originalism treats the fixed historical meaning as the object; living constitutionalism treats the enduring principle as the object and permits its contemporary application to vary; positivism treats the formal enactment procedure as the object and brackets substantive meaning-recovery entirely. A sibling reading would change which agents count as beneficiaries (e.g., under living constitutionalism, rights claimants without 18th-century analogues would shift from payer to beneficiary) and would substantially lower or restructure the suppression metric, since adaptive interpretation would no longer be foreclosed by default.',
    'Choosing the originalist reading fixes conservative_legal_movement as beneficiary and rights_claimants_without_historical_analogue as victim; adopting the living-constitutionalist reading instead would invert much of this beneficiary/victim structure for the same underlying text.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_choice, conceptual, 'Documents the committer-frame choice among sibling kernel readings and what would change under each.').

omega_variable(
    founding_era_polity_exclusion_weight,
    'How much weight should the originalist method give to the fact that the ''original public'' whose understanding is authoritative excluded women, enslaved people, and non-property-holding men from participating in forming that understanding?',
    'Compare originalist scholarship''s treatment of this exclusion (e.g., arguments that later amendments like the 14th and 19th Amendments reset or supplement the relevant ''original'' baseline) against critics'' argument that any baseline drawn from an exclusionary polity carries forward that exclusion structurally.',
    'If later amendments are treated as fully resetting the relevant historical baseline for excluded groups'' claims, the suppression and victim-structure of this reading is substantially narrower than authored here. If the pre-amendment baseline continues to structure interpretation even for post-amendment claims, the victim structure as authored is accurate or understated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_era_polity_exclusion_weight, conceptual, 'Whether constitutional amendments adequately reset the exclusionary baseline of the founding-era polity for originalist purposes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__originalist_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1980, us_constitution_text__originalist_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(us_c_tr_t1990, us_constitution_text__originalist_reading, theater_ratio, 1990, 0.24).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_text__originalist_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_text__originalist_reading, theater_ratio, 2010, 0.31).
narrative_ontology:measurement(us_c_tr_t2020, us_constitution_text__originalist_reading, theater_ratio, 2020, 0.35).
narrative_ontology:measurement(us_c_tr_t2025, us_constitution_text__originalist_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1980, us_constitution_text__originalist_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(us_c_be_t1990, us_constitution_text__originalist_reading, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_text__originalist_reading, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_text__originalist_reading, base_extractiveness, 2010, 0.53).
narrative_ontology:measurement(us_c_be_t2020, us_constitution_text__originalist_reading, base_extractiveness, 2020, 0.59).
narrative_ontology:measurement(us_c_be_t2025, us_constitution_text__originalist_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1980, us_constitution_text__originalist_reading, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement(us_c_su_t1990, us_constitution_text__originalist_reading, suppression_requirement, 1990, 0.53).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_text__originalist_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(us_c_su_t2010, us_constitution_text__originalist_reading, suppression_requirement, 2010, 0.66).
narrative_ontology:measurement(us_c_su_t2020, us_constitution_text__originalist_reading, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement(us_c_su_t2025, us_constitution_text__originalist_reading, suppression_requirement, 2025, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, positivist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the shared us_constitution_text kernel. living_constitutionalist_reading authors the same underlying text under a premise that directly contradicts this reading's foundational axiom (meaning fixed at ratification vs. meaning evolves through contemporary application), and is therefore linked via a forecloses relation. positivist_reading authors formal-enactment validity as the operative concern and is largely orthogonal to the historical-meaning-recovery question this reading turns on, hence coexists_with. Each sibling carries its own ε, beneficiary/victim structure, and classification — this file does not average or hedge across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
