% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   This constraint instantiates the originalist reading of the U.S.
 *   constitutional interpretive kernel: the claim that constitutional meaning
 *   was fixed at ratification (or, for amendments, at their own ratification)
 *   and that interpretive authority is legitimate only insofar as it traces
 *   fidelity to framers' intent or original public meaning. As a
 *   self-conscious judicial and academic movement, originalism gained
 *   institutional traction from the late 1970s onward and now anchors a
 *   substantial body of federal constitutional doctrine, particularly in
 *   federalism, Second Amendment, and enumerated-powers cases. This is ONE of
 *   three linked readings of the same kernel (living constitutionalism,
 *   popular constitutionalism); each reading is authored as its own
 *   ε-invariant constraint per the decomposition principle, sharing the
 *   kernel_id but not the ε, beneficiary/victim structure, or classification.
 *
 * KEY AGENTS:
 *   - originalist_judiciary: agenda_setter, administers the interpretive method and its evidentiary rules
 *   - federalism_advocates, religious_liberty_claimants_original_understanding, property_rights_defenders: beneficiaries under the doctrine's substantive tilt
 *   - unenumerated_rights_claimants, federal_regulatory_expansion_advocates, marginalized_groups_excluded_from_1787_franchise: bear the doctrine's costs
 *   - constitutional_law_scholars: analytical observers of doctrinal coherence and historical accuracy
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
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__originalist_reading, "Originalist Reading of Constitutional Interpretive Authority").
narrative_ontology:topic_domain(us_constitution_interpretive__originalist_reading, "constitutional_law/legal_interpretation/political_theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__originalist_reading, 'd0476db0-c0cd-464a-aa8f-e3fe0b39bfb7').
narrative_ontology:cs_kernel_codification('d0476db0-c0cd-464a-aa8f-e3fe0b39bfb7', fixed_text).
narrative_ontology:cs_authority_grounding('d0476db0-c0cd-464a-aa8f-e3fe0b39bfb7', lineage).
narrative_ontology:cs_interpretation_layer_present('d0476db0-c0cd-464a-aa8f-e3fe0b39bfb7').
narrative_ontology:cs_reading_relation('d0476db0-c0cd-464a-aa8f-e3fe0b39bfb7', us_constitution_interpretive__living_constitution_reading, coexists_with).
narrative_ontology:cs_reading_relation('d0476db0-c0cd-464a-aa8f-e3fe0b39bfb7', us_constitution_interpretive__popular_constitutionalism_reading, influences).
narrative_ontology:cs_axiom('d0476db0-c0cd-464a-aa8f-e3fe0b39bfb7', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('d0476db0-c0cd-464a-aa8f-e3fe0b39bfb7', meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('d0476db0-c0cd-464a-aa8f-e3fe0b39bfb7', foundational, judicial_legitimacy_requires_historical_fidelity).
narrative_ontology:cs_axiom_status(judicial_legitimacy_requires_historical_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('d0476db0-c0cd-464a-aa8f-e3fe0b39bfb7', judicial_legitimacy_requires_historical_fidelity, instrumental).
narrative_ontology:cs_reference_frame('d0476db0-c0cd-464a-aa8f-e3fe0b39bfb7', ratification_era_public_meaning).
narrative_ontology:cs_drift_state('d0476db0-c0cd-464a-aa8f-e3fe0b39bfb7', post_warren_court_reaction, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('d0476db0-c0cd-464a-aa8f-e3fe0b39bfb7', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__originalist_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, federalism_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, religious_liberty_claimants_original_understanding).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, property_rights_defenders).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, marginalized_groups_excluded_from_1787_franchise).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__originalist_reading, framers_intent_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__originalist_reading, original_public_meaning_theory).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__originalist_reading, judicial_restraint_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federal judges and justices who adjudicate constitutional questions by reference to 1787-1791 (and Reconstruction-era, for post-Civil War amendments) understanding of text. They administer the interpretive method itself, deciding which historical sources count as evidence of original meaning and how to apply it to modern facts. Their authority is enhanced, not diminished, by claiming fidelity to a fixed external source rather than to their own contemporary judgment.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, originalist_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% State governments, states'-rights litigants, and political factions favoring devolved power benefit from a reading that caps federal regulatory authority at what the ratifying generation would have recognized as within Congress's enumerated powers. They gain leverage to challenge federal statutes and agency rules as exceeding constitutional bounds, and can relocate political and legal strategy across state and federal venues.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federalism_advocates, beneficiary,
    organized, generational, mobile, national).

% Religious institutions and individuals whose claims map onto historically recognized free-exercise and establishment concerns benefit from a reading that anchors religious liberty doctrine to founding-era practice rather than to evolving pluralist norms, expanding exemptions and protections that a living-constitution reading might narrow.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, religious_liberty_claimants_original_understanding, beneficiary,
    organized, generational, mobile, national).

% Property owners, takings-clause litigants, and business interests benefit from a reading that constrains regulatory takings doctrine and economic regulation to bounds consistent with 1787-era property conceptions, limiting the state's capacity to redefine property obligations without triggering compensation requirements.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, property_rights_defenders, beneficiary,
    organized, generational, mobile, national).

% Individuals asserting rights not textually enumerated or not recognized at ratification — reproductive autonomy, certain privacy interests, evolving equal-protection claims for groups unrecognized as full legal persons in 1787-1791 — bear the cost of a doctrine that treats the absence of historical recognition as near-dispositive against constitutional protection. Their only recourse is constitutional amendment or waiting for doctrinal shift, both effectively foreclosed avenues for an individual litigant.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Agencies, legislators, and advocacy groups seeking to address contemporary problems (environmental harm, financial regulation, public health) through federal action bear the cost when courts narrow the Commerce Clause, Necessary and Proper Clause, or administrative deference doctrines to fit an 18th-century conception of federal power. They can lobby for new legislation or amendment, but each avenue is itself constrained by the same interpretive gatekeeping.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates, payer,
    moderate, generational, constrained, national).

% Groups who held no voice in the constitutional drafting or ratification process (enslaved people, women, non-property-holding men, Indigenous nations) had no opportunity to embed their interests into the 'original public meaning' the doctrine treats as authoritative. A reading that privileges that historical meaning structurally reproduces the exclusion of their ancestors' interests into present-day doctrine, absent an amendment explicitly correcting it.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, marginalized_groups_excluded_from_1787_franchise, payer,
    powerless, generational, trapped, national).

% Legal academics and historians study the doctrine's internal coherence, historical accuracy of claimed original meanings, and comparative outcomes against rival interpretive methods. They can shift the intellectual weather that judges draw on, but do not directly decide cases.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, constitutional_law_scholars, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originalism offers a principled, publicly statable constraint on judicial discretion: it purports to prevent judges from substituting personal policy preferences for law by anchoring meaning to a fixed, ascertainable historical fact (ratification-era understanding), which in principle any judge — regardless of ideology — should reach by the same method given the same evidence.
% TRANSFER_FUNCTION: Moves interpretive authority and outcome-determinative discretion toward federalism, historically-bounded religious liberty, and pre-New-Deal property conceptions, and away from claimants whose interests were unrepresented at ratification or whose rights depend on contemporary societal consensus rather than 18th/19th-century textual and historical evidence.
% ABSENT_VOICES: The persons excluded from the franchise in 1787-1791 and during Reconstruction-era ratification (enslaved people, women, non-propertied men, Indigenous nations) have no voice embedded in the 'original public meaning' the doctrine treats as authoritative — their absence from the room that fixed the meaning is structural, not incidental, and the doctrine has no internal mechanism to correct for it.
% DISAPPEARANCE_RATIONALE: If originalism vanished as an interpretive method, federal courts would lose a major doctrinal basis for striking down federal regulatory expansion and for narrowing unenumerated-rights claims to historical scope; substantial bodies of Commerce Clause, Second Amendment, and substantive due process case law would be open to re-litigation under a different interpretive baseline, and both litigation strategy and legislative drafting would reorganize around the successor method.
% FOUNDING_PROBLEM: Originalism as a self-conscious judicial theory emerged largely in the late 20th century (Bork, Scalia, the Federalist Society) to solve a perceived problem: unconstrained judicial discretion during the Warren and Burger Court eras, where judges were seen as reading contemporary policy preferences into the Constitution under living-constitutionalist cover, with no check other than the judges' own restraint.
% FOUNDING_PROBLEM_CORROBORATION: Originalist judges and scholars attest the problem (unconstrained judicial policymaking) remains live and originalism is the necessary corrective. Legal historians outside the movement, including some conservative critics, attest that the doctrine's claimed determinacy is itself contested — historical evidence of 'original meaning' is frequently indeterminate or contested among historians, and originalist outcomes correlate strongly with the political priors of the judges applying the method, suggesting the discretion problem persists in a different guise rather than being solved.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at 0.58 (moderate-high, rising over the interval) because the doctrine's substantive tilt toward federalism, historical property conceptions, and historically-bounded rights recognition systematically advantages the beneficiary groups at the expense of claimants whose interests were unrepresented at ratification — but the coordination function (constraining judicial discretion via a publicly statable, in-principle-falsifiable method) is genuine and not merely pretextual, which is why this is authored as tangled_rope rather than snare. Suppression (0.52) reflects that alternative interpretive methods are not eliminated — living constitutionalism and popular constitutionalism remain live, practiced, and taught — but originalism's institutional capture of significant appellate and Supreme Court seats gives it outsized doctrinal force relative to its standing as one of several competing theories. Resistance is high (0.72) because the doctrine is actively and vocally contested by a substantial legal-academic and judicial constituency; this is not settled law by consensus but a doctrine that must be actively defended and applied by its adherents. Accessibility collapse is moderate (0.50): once a court adopts originalism as its interpretive premise, alternative readings of a given clause are substantially foreclosed in that court's future rulings, but the doctrine itself remains contestable at the level of judicial appointments and academic debate.
 *
 * PERSPECTIVAL GAP:
 *   From the originalist judiciary's own seat, the doctrine is Rope: a neutral, principled constraint on judicial will that any judge should reach identically given the same historical evidence. From the seat of unenumerated_rights_claimants and marginalized_groups_excluded_from_1787_franchise, the same doctrine computes as Snare or high-extraction Tangled Rope: a method that treats their historical exclusion from the ratifying public as a permanent doctrinal disability, dressed in the neutral language of historical fidelity. This divergence is exactly the structural fact the engine should surface — the claim (rope-like, principled constraint) and the metrics (tangled_rope, substantial asymmetric extraction) are authored independently and are not reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Federalism advocates, religious liberty claimants under original understanding, and property rights defenders are declared beneficiaries because the doctrine's substantive commitments (narrow federal power, historically-bounded religious exemptions, robust property protection) map onto their preferred outcomes, and they hold organized, mobile positions able to press advantage across venues. Unenumerated rights claimants and marginalized groups excluded from the 1787 franchise are declared victims because the doctrine structurally treats the absence of historical recognition as evidence against constitutional protection, and they hold powerless, trapped positions with no meaningful alternative venue — amendment is a vanishingly narrow path for an individual litigant. Federal regulatory expansion advocates sit at moderate power with constrained exit: they can pursue new legislation, but that legislation is itself vulnerable to the same interpretive method on review.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unconstrained judicial policymaking — is contested rather than resolved: originalism's adherents maintain the problem is live and the doctrine is the necessary corrective; critics (including some within adjacent conservative legal traditions) note that claimed historical determinacy is frequently illusory and that originalist outcomes track judges' contemporary priors as reliably as living-constitutionalist ones, suggesting the underlying discretion problem persists under different cover. Classifying this as tangled_rope rather than snare or mountain prevents two mislabeling errors: treating the doctrine as pure extraction (which would erase its genuine, non-pretextual function of publicly constraining judicial reasoning) and treating it as natural law (which would erase the asymmetric substantive tilt and its excluded constituencies).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalism_kernel_reading_selection,
    'Is originalism the historically and legally correct reading of the interpretive-authority kernel, or is it one contestable reading among several that happens to have gained disproportionate institutional purchase through the judicial appointments process rather than through demonstrated interpretive superiority?',
    'This is not resolvable by further legal argument alone — it is a live, ongoing three-way contest among originalist, living-constitutionalist, and popular-constitutionalist readings, each with active scholarly and judicial constituencies. Resolution (if any) would come from long-run institutional and political developments, not from a single dispositive proof.',
    'If originalism is treated as the sole legitimate reading, its substantive tilt toward the beneficiary groups is naturalized as simply ''correct interpretation'' rather than as one policy-laden choice among live alternatives, which would understate its extractive dimension toward the victim groups.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(originalism_kernel_reading_selection, conceptual, 'Committer-frame ambiguity: which kernel reading is authoritative, and by what standard.').

omega_variable(
    historical_meaning_determinacy,
    'Is ''original public meaning'' for any given constitutional clause generally ascertainable with enough historical precision to constrain judicial discretion as the doctrine claims, or is the claimed determinacy itself substantially indeterminate, allowing originalist judges latitude comparable to living-constitutionalist judges under different rhetorical cover?',
    'Systematic historiographic review comparing originalist judicial opinions'' historical claims against professional historians'' independent assessments of the same evidence, across a representative sample of cases.',
    'If original meaning is generally indeterminate, the doctrine''s coordination function (constraining discretion) is substantially theater rather than real, which would push the classification toward snare; if generally determinate, the coordination function is more robust and the tangled_rope classification (genuine coordination plus asymmetric extraction) is more clearly warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_meaning_determinacy, empirical, 'Whether claimed historical determinacy is real or a discretion-laundering mechanism.').

omega_variable(
    excluded_ratifiers_correction_mechanism,
    'Does the formal availability of the constitutional amendment process (which could in principle correct for the exclusion of unrepresented groups from the original ratifying public) constitute an adequate structural remedy, or is that avenue so practically foreclosed for individual and minority claimants that the exclusion is effectively permanent within originalist doctrine absent amendment?',
    'Empirical study of amendment success rates and the political-coalition size required, compared against the size and organization of the groups bearing the doctrine''s costs.',
    'If amendment is a practically foreclosed avenue for the affected groups, the doctrine''s treatment of historical exclusion as a permanent disability is closer to structural extraction with no genuine remedy; if amendment is a live and periodically exercised avenue, the doctrine''s cost to excluded groups is better characterized as a high but not insurmountable burden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_ratifiers_correction_mechanism, empirical, 'Whether formal correction mechanisms are practically available to the doctrine''s victim groups.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__originalist_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1980, us_constitution_interpretive__originalist_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(us_c_tr_t1990, us_constitution_interpretive__originalist_reading, theater_ratio, 1990, 0.22).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_interpretive__originalist_reading, theater_ratio, 2000, 0.24).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_interpretive__originalist_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(us_c_tr_t2018, us_constitution_interpretive__originalist_reading, theater_ratio, 2018, 0.27).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_interpretive__originalist_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1980, us_constitution_interpretive__originalist_reading, base_extractiveness, 1980, 0.38).
narrative_ontology:measurement(us_c_be_t1990, us_constitution_interpretive__originalist_reading, base_extractiveness, 1990, 0.44).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_interpretive__originalist_reading, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_interpretive__originalist_reading, base_extractiveness, 2010, 0.53).
narrative_ontology:measurement(us_c_be_t2018, us_constitution_interpretive__originalist_reading, base_extractiveness, 2018, 0.56).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_interpretive__originalist_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1980, us_constitution_interpretive__originalist_reading, suppression_requirement, 1980, 0.32).
narrative_ontology:measurement(us_c_su_t1990, us_constitution_interpretive__originalist_reading, suppression_requirement, 1990, 0.38).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_interpretive__originalist_reading, suppression_requirement, 2000, 0.42).
narrative_ontology:measurement(us_c_su_t2010, us_constitution_interpretive__originalist_reading, suppression_requirement, 2010, 0.46).
narrative_ontology:measurement(us_c_su_t2018, us_constitution_interpretive__originalist_reading, suppression_requirement, 2018, 0.5).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_interpretive__originalist_reading, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, living_constitution_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the us_constitution_interpretive kernel. living_constitution_reading and popular_constitutionalism_reading are sibling files with their own ε, beneficiary/victim structures, and classifications, decomposed per the ε-invariance principle rather than represented as a single observer-relative constraint. All three share the kernel_id and are linked via network.affects_constraints in each file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
