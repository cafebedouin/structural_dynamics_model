% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__originalist_reading, []).

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
 *   constraint_id: us_constitution_interpretive__originalist_reading
 *   human_readable: Originalist Reading of Constitutional Interpretive Authority
 *   domain: constitutional_law/legal_interpretation/political_theory
 *
 * SUMMARY:
 *   This constraint captures the originalist reading of the
 *   interpretive-authority kernel at the heart of U.S. constitutional
 *   adjudication: the claim that constitutional meaning was fixed at
 *   ratification (or, for amendments, at the amendment's adoption) and that
 *   judicial legitimacy derives from fidelity to that fixed meaning rather
 *   than from adapting text to contemporary values or deferring to popular
 *   political contestation. Since roughly 1980, originalism moved from an
 *   academic minority position to a doctrinally dominant methodology in
 *   significant areas of federal jurisprudence, particularly following
 *   changes in judicial appointments strategy. This story authors the
 *   originalist reading ONLY — the living-constitution and
 *   popular-constitutionalism readings are separate constraints (siblings in
 *   the same kernel family) with their own ε values, beneficiary/victim
 *   structures, and metrics. Conflating them into one story would violate
 *   ε-invariance, since the three readings produce measurably different
 *   outcomes for the same disputes.
 *
 * KEY AGENTS:
 *   - originalist_judiciary: Primary agenda-setter (institutional/analytical) — administers the interpretive method
 *   - federalism_advocates, religious_liberty_claimants, property_rights_defenders: Beneficiaries (organized/mobile) — gain litigation leverage from historically-anchored readings
 *   - unenumerated_rights_claimants: Primary target (powerless/trapped) — bear the cost of a historically-bounded rights ceiling
 *   - federal_regulatory_expansion_advocates: Secondary target (powerful/constrained) — federal power narrowed against 1787 baseline
 *   - marginalized_groups_excluded_from_1787_franchise: Structural victim (powerless/trapped) — the framing generation's exclusions are baked into the interpretive baseline itself
 *   - constitutional_law_historians: Analytical observer — documents the contested and selective nature of the historical record originalism relies on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, 0.58).
domain_priors:suppression_score(us_constitution_interpretive__originalist_reading, 0.52).
domain_priors:theater_ratio(us_constitution_interpretive__originalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__originalist_reading, "Originalist Reading of Constitutional Interpretive Authority").
narrative_ontology:topic_domain(us_constitution_interpretive__originalist_reading, "constitutional_law/legal_interpretation/political_theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__originalist_reading, '035ed40b-3370-453f-a317-c04fcc7afe1c').
narrative_ontology:cs_kernel_codification('035ed40b-3370-453f-a317-c04fcc7afe1c', fixed_text).
narrative_ontology:cs_authority_grounding('035ed40b-3370-453f-a317-c04fcc7afe1c', lineage).
narrative_ontology:cs_interpretation_layer_present('035ed40b-3370-453f-a317-c04fcc7afe1c').
narrative_ontology:cs_reading_relation('035ed40b-3370-453f-a317-c04fcc7afe1c', us_constitution_interpretive__living_constitution_reading, coexists_with).
narrative_ontology:cs_reading_relation('035ed40b-3370-453f-a317-c04fcc7afe1c', us_constitution_interpretive__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('035ed40b-3370-453f-a317-c04fcc7afe1c', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('035ed40b-3370-453f-a317-c04fcc7afe1c', meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('035ed40b-3370-453f-a317-c04fcc7afe1c', foundational, judicial_legitimacy_requires_historical_fidelity).
narrative_ontology:cs_axiom_status(judicial_legitimacy_requires_historical_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('035ed40b-3370-453f-a317-c04fcc7afe1c', judicial_legitimacy_requires_historical_fidelity, instrumental).
narrative_ontology:cs_reference_frame('035ed40b-3370-453f-a317-c04fcc7afe1c', ratification_era_public_meaning).
narrative_ontology:cs_drift_state('035ed40b-3370-453f-a317-c04fcc7afe1c', post_warren_court_reaction_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('035ed40b-3370-453f-a317-c04fcc7afe1c', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__originalist_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, federalism_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, religious_liberty_claimants_original_understanding).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, property_rights_defenders).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, originalist_judiciary).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, marginalized_groups_excluded_from_1787_franchise).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__originalist_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__originalist_reading, fixed_meaning_at_ratification_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federal judges, especially at the appellate and Supreme Court level, who adjudicate disputes by reconstructing 1787-1791 (or relevant amendment-era) public meaning. They administer the interpretive method itself, deciding which historical sources count as evidence of original meaning and how disputes are resolved when the historical record is thin or contested.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, originalist_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% State governments, state-rights litigation organizations, and political coalitions that gain leverage when federal regulatory power is read narrowly against an enumerated-powers baseline. They cite originalist rulings to challenge federal statutes and gain expanded room for state-level policy divergence.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federalism_advocates, beneficiary,
    organized, generational, mobile, national).

% Religious institutions and individuals whose free-exercise claims are strengthened when courts read the First Amendment's original public meaning as providing broad accommodation, decoupled from contemporary anti-discrimination or secular-governance frameworks that a living-constitution reading might weigh more heavily.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, religious_liberty_claimants_original_understanding, beneficiary,
    organized, generational, mobile, national).

% Landowners, developers, and business associations that benefit when takings-clause and due-process doctrines are read according to 1787-era property conceptions, limiting the scope of regulatory takings and environmental or zoning restrictions that a more evolving reading might sustain.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, property_rights_defenders, beneficiary,
    organized, generational, mobile, national).

% Individuals asserting rights not explicitly named in 1787-1791 text or not contemplated by that era's understanding — reproductive autonomy, certain privacy claims, LGBTQ+ equal-protection claims, and other liberty interests recognized under substantive due process. Under the originalist reading these claims face a demand for historical grounding many cannot satisfy, since the relevant historical polity did not conceive of them as rights-bearing subjects at all. Exit is effectively foreclosed: constitutional adjudication is the only forum, and the interpretive method itself sets the threshold they must clear.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Federal agencies, congressional majorities, and administrative-state proponents who see modern regulatory programs (environmental, labor, financial, health) invalidated or narrowed when courts measure congressional power against an 18th-century commerce and necessary-and-proper baseline. They can lobby for new legislation or agency workarounds, but each avenue is itself subject to the same interpretive ceiling.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates, payer,
    powerful, generational, constrained, national).

% Groups who held no political voice or legal personhood at ratification — women, enslaved and formerly enslaved people, Indigenous nations, and others whose interests were structurally absent from the constitutional bargain the originalist method treats as authoritative. They bear the enduring cost of an interpretive baseline fixed at a moment that did not recognize them as full participants, even where later amendments formally extended rights.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, marginalized_groups_excluded_from_1787_franchise, payer,
    powerless, civilizational, trapped, national).

% Scholars who study the historical record the originalist method depends on. They document how contested, incomplete, and internally divided the framing generation's own understanding often was, and how originalist judges sometimes select among competing historical accounts to reach predetermined outcomes.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, constitutional_law_historians, observer,
    analytical, civilizational, analytical, national).

% Legal scholars, movements, and jurists who hold that constitutional meaning should track evolving societal understanding or popular democratic contestation. Within a judiciary and legal culture where originalism holds interpretive dominance, their framework is treated as illegitimate departure rather than a competing legitimate method, foreclosing their preferred mode of argument from serious judicial consideration in many forums.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, living_constitution_and_popular_constitutionalist_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_interpretive__originalist_reading, diffuse).
narrative_ontology:fixing_cost_class(us_constitution_interpretive__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate, historically-anchored decision procedure for resolving constitutional disputes, constraining judicial discretion by tying outcomes to a fixed, publicly ascertainable meaning rather than to each judge's contemporary policy preferences — solving the genuine problem of unconstrained judicial lawmaking.
% TRANSFER_FUNCTION: Moves interpretive authority and the practical benefits of constitutional protection toward claims that map onto categories recognized or anticipated by 1787-1791 (or relevant amendment-era) public understanding, and away from claims grounded in post-ratification moral, social, or scientific development — shifting litigation outcomes from federal regulatory expansion and unenumerated-rights claimants toward federalism, historically-recognized religious liberty, and property-rights positions.
% ABSENT_VOICES: The framing generation itself excluded women, enslaved people, and Indigenous nations from the political community whose 'original public meaning' the method treats as authoritative; their absence from the 1787-1791 discourse is structurally embedded in the interpretive baseline, not merely a historical footnote. Contemporary living-constitutionalist and popular-constitutionalist scholars are also structurally disadvantaged in courts where originalism is doctrinally ascendant.
% DISAPPEARANCE_RATIONALE: If originalism lost its current interpretive authority overnight, federal regulatory programs currently vulnerable to enumerated-powers challenges would gain more secure footing, unenumerated liberty claims would be adjudicated under more flexible doctrinal tests, and a substantial body of recent jurisprudence built on original-meaning analysis (in areas like the Second Amendment, administrative law, and federalism) would be open to reconsideration. Litigation strategy, judicial appointments politics, and legal scholarship would reorganize substantially.
% FOUNDING_PROBLEM: The felt problem originalism was built to solve (primarily articulated from the 1970s-1980s onward, though the interpretive posture has earlier roots) was judicial activism: unelected judges using vague constitutional text to impose their own contemporary policy preferences, most prominently associated with the Warren and early Burger Courts' expansion of unenumerated rights and federal power.
% FOUNDING_PROBLEM_CORROBORATION: Originalist jurists and legal scholars (e.g., the Federalist Society tradition) attest the problem — unconstrained judicial discretion — remains live and structurally unaddressed by any other interpretive method. Independent legal historians and comparative constitutional scholars outside that movement note that every interpretive method, including originalism, involves judicial discretion in selecting and weighting historical sources, and argue the 'discretion problem' was never fully solved but rather relocated to historical methodology; some conservative legal scholars themselves (e.g., critics within originalism debating 'original methods' vs. 'original public meaning') dispute how determinate the method actually is in practice.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_interpretive__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__originalist_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at a moderate-high level (0.58 by 2025) because the method's outcomes systematically transfer interpretive advantage toward historically-recognized claims and away from claims lacking 1787-era analogues — a real, structurally asymmetric transfer, not merely a neutral decision procedure. Suppression (0.52) reflects the doctrinal dominance the method has achieved in key federal courts, which forecloses competing interpretive arguments from serious consideration in those forums, rather than pure coercive force. Theater ratio is kept modest (0.28) because the historical-analysis function is substantively real — judges do engage in genuine (if contested) historical reconstruction — but a growing share of 'originalist' argument in recent years has been criticized by historians as results-oriented history-shopping, which the rising theater trajectory reflects. accessibility_collapse (0.45) and resistance (0.68) are authored well below mountain territory: alternative interpretive methods remain actively argued, adopted by other judges, and defended by a substantial scholarly and political constituency — this is a live, contested method, not settled natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   The originalist_judiciary sits at the agenda-setting seat with analytical exit — they administer the method and are largely insulated from its costs by life tenure and institutional position. Federalism advocates, religious liberty claimants, and property rights defenders are structural beneficiaries: the method's historical anchoring reliably favors their litigation positions, and their organized status and mobile exit options (they can pursue parallel political/legislative strategies) push their derived directionality toward the low-extraction end. Unenumerated rights claimants and marginalized groups excluded from the 1787 franchise sit at the opposite pole: trapped exit (constitutional adjudication is often their only recourse), powerless structural position, and a historical baseline that did not contemplate their claims or their personhood at all — this combination drives high effective extraction even before scope amplification. Federal regulatory expansion advocates are powerful but constrained: they can pursue legislative workarounds, but those workarounds remain subject to the same interpretive ceiling, which is why their exit is constrained rather than mobile.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unconstrained judicial policymaking — is contested rather than resolved: originalists maintain it remains live and structurally unaddressed by rival methods, while critics argue the discretion problem was relocated into historical methodology (which historical sources count, how to weight thin or divided historical records) rather than eliminated. This is precisely the kind of contested genealogy the R5 interview is built to surface: classifying this constraint as tangled_rope (rather than snare or rope outright) prevents both mislabeling error. It is not pure extraction — the coordination function (constraining judicial discretion via a determinate decision procedure) is genuine and is what originalism's proponents point to. But it is not pure coordination either — the beneficiary/victim asymmetry is real, tracks structural power (organized, mobile beneficiaries vs. powerless, trapped victims), and requires active doctrinal enforcement (case-by-case litigation over what counts as legitimate historical evidence) to maintain its dominance against competing methods.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_meaning_determinacy,
    'Is ''original public meaning'' at ratification (or amendment) a sufficiently determinate historical fact to genuinely constrain judicial discretion, or is the historical record contested and selective enough that originalist judges effectively exercise the same discretion under a different vocabulary?',
    'Systematic comparison of originalist opinions against the underlying historical record by independent historians (not litigants or advocacy-affiliated scholars), examining whether historical sources were selected consistently or outcome-drivenly across cases with similar historical evidentiary quality.',
    'If the historical record is genuinely determinate and consistently applied, the coordination function (constraining discretion) is substantially real, pushing the classification toward rope. If the record is thin/contested and selectively invoked, the coordination story is largely cover for outcome-driven adjudication, pushing toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_determinacy, empirical, 'Whether original-meaning historical analysis genuinely constrains judicial discretion or merely relocates it.').

omega_variable(
    framing_generation_exclusion_baseline_problem,
    'Does the structural exclusion of women, enslaved people, and Indigenous nations from the 1787-1791 political community that originalism treats as authoritative constitute a disqualifying defect in the method itself, or a historical fact that later amendments (13th, 14th, 15th, 19th) have adequately superseded for interpretive purposes?',
    'Doctrinal analysis of how consistently originalist jurisprudence treats post-Civil-War-Amendment public meaning as the relevant baseline for equal-protection and other reconstruction-era claims, versus reverting to 1787 framing-era understanding.',
    'If later amendments are treated as fully superseding baselines for their domains, the exclusion critique is substantially mitigated for those areas. If 1787-era understanding continues to constrain even reconstruction-amendment interpretation, the exclusion is a live, unaddressed structural feature of the method.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_generation_exclusion_baseline_problem, conceptual, 'Whether post-ratification amendments cure or merely paper over the framing generation''s exclusions.').

omega_variable(
    committer_kernel_reading_selection,
    'This story instantiates the originalist reading of the shared interpretive-authority kernel. Is originalism''s current doctrinal dominance itself evidence that it is the structurally correct reading, or an artifact of a particular multi-decade judicial appointments strategy that could equally have produced dominance for a different reading?',
    'Comparative institutional history: examine whether originalism''s ascendance tracks independent gains in scholarly or public persuasiveness versus tracking the composition of appointing bodies (presidents, Senate majorities) over the same period.',
    'If dominance tracks appointments strategy rather than independent persuasive gains, the kernel''s current ''settled'' character in dominant courts is contingent and reversible rather than a discovery of the correct reading — supporting the classification of ALL three sibling readings as live, contested constraints rather than treating originalism as having definitively displaced its rivals.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_kernel_reading_selection, conceptual, 'Whether originalism''s doctrinal dominance reflects discovery of correct meaning or contingent political-institutional capture of the interpretive kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__originalist_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1980, us_constitution_interpretive__originalist_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(us_c_tr_t1989, us_constitution_interpretive__originalist_reading, theater_ratio, 1989, 0.18).
narrative_ontology:measurement(us_c_tr_t1998, us_constitution_interpretive__originalist_reading, theater_ratio, 1998, 0.2).
narrative_ontology:measurement(us_c_tr_t2007, us_constitution_interpretive__originalist_reading, theater_ratio, 2007, 0.22).
narrative_ontology:measurement(us_c_tr_t2016, us_constitution_interpretive__originalist_reading, theater_ratio, 2016, 0.25).
narrative_ontology:measurement(us_c_tr_t2025, us_constitution_interpretive__originalist_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1980, us_constitution_interpretive__originalist_reading, base_extractiveness, 1980, 0.32).
narrative_ontology:measurement(us_c_be_t1989, us_constitution_interpretive__originalist_reading, base_extractiveness, 1989, 0.38).
narrative_ontology:measurement(us_c_be_t1998, us_constitution_interpretive__originalist_reading, base_extractiveness, 1998, 0.42).
narrative_ontology:measurement(us_c_be_t2007, us_constitution_interpretive__originalist_reading, base_extractiveness, 2007, 0.46).
narrative_ontology:measurement(us_c_be_t2016, us_constitution_interpretive__originalist_reading, base_extractiveness, 2016, 0.51).
narrative_ontology:measurement(us_c_be_t2025, us_constitution_interpretive__originalist_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1980, us_constitution_interpretive__originalist_reading, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(us_c_su_t1989, us_constitution_interpretive__originalist_reading, suppression_requirement, 1989, 0.36).
narrative_ontology:measurement(us_c_su_t1998, us_constitution_interpretive__originalist_reading, suppression_requirement, 1998, 0.4).
narrative_ontology:measurement(us_c_su_t2007, us_constitution_interpretive__originalist_reading, suppression_requirement, 2007, 0.43).
narrative_ontology:measurement(us_c_su_t2016, us_constitution_interpretive__originalist_reading, suppression_requirement, 2016, 0.47).
narrative_ontology:measurement(us_c_su_t2025, us_constitution_interpretive__originalist_reading, suppression_requirement, 2025, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, living_constitution_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the shared kernel us_constitution_interpretive (constraint family). living_constitution_reading and popular_constitutionalism_reading are separate constraint stories with their own ε values, beneficiary/victim structures, and classifications, per the ε-invariance principle — the 'Constitution's interpretive method' is not one constraint but a contested kernel with structurally distinct readings. This story's beneficiaries (federalism advocates, historically-recognized religious liberty claimants, property rights defenders) and victims (unenumerated rights claimants, federal regulatory expansion advocates, groups excluded from the 1787 franchise) reflect ONLY the originalist reading's structural effects, assessed by that reading's own lights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
