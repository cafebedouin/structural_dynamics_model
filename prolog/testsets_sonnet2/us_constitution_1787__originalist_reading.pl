% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__originalist_reading, []).

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
 *   constraint_id: us_constitution_1787__originalist_reading
 *   human_readable: Originalist Reading of the U.S. Constitution (1787 Ratification-Fixed Meaning)
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the originalist reading of the contested US
 *   Constitution (1787) kernel: constitutional meaning was fixed at
 *   ratification, and interpretation is bound by the original public meaning
 *   of the text as understood by those who framed and ratified it. This is
 *   one of three structurally distinct constraints sharing the same kernel
 *   text — the living reading and the positivist reading are separate stories
 *   with their own beneficiary/victim structures and their own ε. This
 *   story's ε reflects the standing arrangement AS the originalist tradition
 *   itself experiences and defends it: a coordination function (constraining
 *   judicial discretion) bundled with asymmetric effects (the fixed referent
 *   systematically disadvantages claims and populations without voice at the
 *   1787 founding moment). The rising extractiveness and suppression
 *   trajectories track originalism's institutionalization from a minority
 *   academic position (1980s) to a controlling methodology in significant
 *   portions of the federal judiciary (2020s), with corresponding growth in
 *   enforcement mechanisms (nomination litmus tests, doctrinal citation
 *   requirements, professional gatekeeping).
 *
 * KEY AGENTS:
 *   - originalist_judiciary_and_scholars: agenda_setter/institutional — administers the interpretive method and collects professional/doctrinal authority
 *   - textualist_legislative_coalitions: beneficiary/organized — uses the method to entrench preferred policy outcomes against contemporary majorities
 *   - claimants_of_unenumerated_modern_rights: payer/moderate — bears the evidentiary cost of a ratification-anchored rights boundary
 *   - groups_excluded_from_1787_constitutional_franchise: payer/powerless — structurally absent from the very historical record treated as authoritative
 *   - constitutional_historians: observer/analytical — study the contested historical record the method depends on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, 0.42).
domain_priors:suppression_score(us_constitution_1787__originalist_reading, 0.38).
domain_priors:theater_ratio(us_constitution_1787__originalist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__originalist_reading, "Originalist Reading of the U.S. Constitution (1787 Ratification-Fixed Meaning)").
narrative_ontology:topic_domain(us_constitution_1787__originalist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__originalist_reading, 'b913bffe-3f98-4be3-ab0f-d96d447a77b0').
narrative_ontology:cs_kernel_codification('b913bffe-3f98-4be3-ab0f-d96d447a77b0', fixed_text).
narrative_ontology:cs_authority_grounding('b913bffe-3f98-4be3-ab0f-d96d447a77b0', lineage).
narrative_ontology:cs_interpretation_layer_present('b913bffe-3f98-4be3-ab0f-d96d447a77b0').
narrative_ontology:cs_reading_relation('b913bffe-3f98-4be3-ab0f-d96d447a77b0', us_constitution_1787__living_reading, forecloses).
narrative_ontology:cs_reading_relation('b913bffe-3f98-4be3-ab0f-d96d447a77b0', us_constitution_1787__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('b913bffe-3f98-4be3-ab0f-d96d447a77b0', foundational, ratification_era_intent_is_binding).
narrative_ontology:cs_axiom_status(ratification_era_intent_is_binding, holdable).
narrative_ontology:cs_axiom_grounding('b913bffe-3f98-4be3-ab0f-d96d447a77b0', ratification_era_intent_is_binding, conventional).
narrative_ontology:cs_axiom('b913bffe-3f98-4be3-ab0f-d96d447a77b0', secondary, judicial_discretion_requires_historical_constraint).
narrative_ontology:cs_axiom_status(judicial_discretion_requires_historical_constraint, holdable).
narrative_ontology:cs_axiom_grounding('b913bffe-3f98-4be3-ab0f-d96d447a77b0', judicial_discretion_requires_historical_constraint, instrumental).
narrative_ontology:cs_reference_frame('b913bffe-3f98-4be3-ab0f-d96d447a77b0', ratification_era_original_public_meaning).
narrative_ontology:cs_drift_state('b913bffe-3f98-4be3-ab0f-d96d447a77b0', contemporary_pluralist_jurisprudence, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('b913bffe-3f98-4be3-ab0f-d96d447a77b0', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__originalist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, originalist_judiciary_and_scholars).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, textualist_legislative_coalitions).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, groups_favored_by_1787_social_order).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, claimants_of_unenumerated_modern_rights).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, groups_excluded_from_1787_constitutional_franchise).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, litigants_relying_on_evolving_equal_protection_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges, justices, and academics who adjudicate cases and shape doctrine by appeal to ratification-era meaning and framers' intent. They administer the interpretive method itself, decide which historical sources count as authoritative, and gain professional authority, citation influence, and case outcomes from the method's dominance in certain courts and law schools.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, originalist_judiciary_and_scholars, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__originalist_reading, originalist_judiciary_and_scholars, beneficiary).

% Political coalitions that favor a narrow reading of federal power or a fixed catalogue of rights use originalism to block judicially-recognized expansions they oppose (e.g., unenumerated privacy or economic regulatory claims), and to entrench outcomes favorable to them by appeal to 1787 meaning rather than contemporary majorities.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, textualist_legislative_coalitions, beneficiary,
    organized, generational, mobile, national).

% Property-holding, enfranchised interests whose legal position in 1787 was already secure benefit when constitutional meaning is anchored to that era's assumptions — the reading tends to reproduce the distributional baseline of a franchise that excluded women, enslaved people, and most non-property-holders.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, groups_favored_by_1787_social_order, beneficiary,
    organized, generational, mobile, national).

% Litigants asserting rights not enumerated in the 1787 text or its immediate amendments (e.g., certain privacy, bodily autonomy, or emerging equality claims) find the originalist reading structurally excludes their claims from constitutional protection unless they can be traced to ratification-era understanding — an evidentiary bar many modern claims cannot clear regardless of their substantive merit.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, claimants_of_unenumerated_modern_rights, payer,
    moderate, biographical, constrained, national).

% Descendants of populations with no voice in ratification — enslaved people, women, non-property-holding men, Indigenous nations — bear the cost of a method that treats an exclusionary founding moment as the authoritative source of meaning; their historical absence from the ratifying convention becomes a structural feature of how their present claims are evaluated.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, groups_excluded_from_1787_constitutional_franchise, payer,
    powerless, generational, trapped, national).

% Parties whose claims depend on doctrines built through decades of living-constitutionalist precedent (e.g., substantive due process lines, evolving equal protection standards) face reversal or narrowing risk when courts adopt originalist methodology, since many such doctrines cannot be grounded in ratification-era original public meaning.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, litigants_relying_on_evolving_equal_protection_doctrine, payer,
    moderate, biographical, constrained, national).

% Scholars and jurists who hold that constitutional meaning evolves with society, or that it is fixed by text-plus-amendment rather than framers' subjective intent, argue originalism smuggles a substantive political program under a claim of neutral historical method. They compete in the same professional and judicial spaces but are not the authority this reading recognizes as dispositive.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, living_and_positivist_constitutional_theorists, excluded,
    organized, generational, mobile, national).

% Study the actual historical record of ratification debates, drafting compromises, and contemporaneous public understanding. Their findings are often contested by originalist jurists as either confirming or complicating a clean 'original meaning,' since the historical record itself contains disagreement among framers and ratifiers.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_1787__originalist_reading, diffuse).
narrative_ontology:fixing_cost_class(us_constitution_1787__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate, historically-anchored decision procedure for resolving constitutional disputes, constraining judicial discretion by requiring appeal to a fixed textual and historical referent rather than shifting contemporary values — this solves a genuine problem of unconstrained judicial policy-making dressed as interpretation.
% TRANSFER_FUNCTION: Moves interpretive authority and case outcomes toward parties whose claims can be grounded in 1787-era text and understanding, and away from parties whose claims depend on rights or protections recognized only through later social, doctrinal, or constitutional development.
% ABSENT_VOICES: The populations with no voice at the 1787 ratifying conventions — enslaved people, women, non-property-holding men, Indigenous nations — are structurally absent from the very historical record the method treats as authoritative; living-constitutionalist and positivist theorists are present in professional discourse but excluded from this reading's account of what counts as binding.
% DISAPPEARANCE_RATIONALE: If originalism disappeared as a controlling interpretive method overnight, doctrines and case outcomes built on it would be vulnerable to reversal under competing methods, and coalitions that relied on it to block doctrinal expansion would lose their primary jurisprudential lever — but the underlying constitutional text and structure would remain, so parties dispute whether the world 'rearranges' (originalists say yes, decisively) or whether other interpretive methods would simply substitute with similar structural effects (critics say the substantive stakes, not the method, drive outcomes).
% FOUNDING_PROBLEM: The problem originalism as a modern jurisprudential movement was built to solve (mid-to-late 20th century) was perceived judicial activism: courts recognizing rights and constraints on democratic majorities that critics argued had no textual or historical grounding, substituting judges' contemporary values for law.
% FOUNDING_PROBLEM_CORROBORATION: Originalist jurists and legal scholars (the reading's own tradition) attest the problem of unconstrained judicial policymaking remains live and originalism is the corrective. Outside the benefiting parties, legal historians and living-constitutionalist scholars attest that the historical record itself often underdetermines a single 'original meaning' (framers disagreed among themselves), and political scientists note originalism's substantive results correlate strongly with contemporary political coalitions' preferred outcomes — suggesting the neutral-method framing is itself contested by those outside the tradition it was built to serve.
narrative_ontology:disappearance_verdict(us_constitution_1787__originalist_reading, contested).
narrative_ontology:founding_problem_status(us_constitution_1787__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_1787__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__originalist_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__originalist_reading_tests).
:- end_tests(us_constitution_1787__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects genuine coordination value (constraining judicial discretion, providing determinacy) bundled with a real distributional asymmetry: the fixed 1787 referent systematically privileges claims traceable to a founding moment from which large populations were excluded, and disadvantages claims resting on later social or doctrinal development. Suppression (0.38) is moderate — the method operates through legitimate judicial and academic channels, not coercion, but its rising trajectory reflects growing institutional enforcement (appointment litmus tests, stare decisis pressure against non-originalist precedent). Accessibility collapse (0.48) is middling: alternative interpretive methods remain live and contested in courts and scholarship, they have not collapsed, which is why this is tangled_rope rather than snare. Resistance (0.55) is substantial and rising — living-constitutionalist and positivist scholars, along with affected litigant groups, actively contest the method's neutrality claim.
 *
 * PERSPECTIVAL GAP:
 *   From the originalist judiciary's seat, the method is coordination: a neutral, determinate discipline against judicial policymaking. From the seat of a claimant whose modern rights claim cannot be traced to ratification-era understanding, the same referent-fixing operates as extraction — a rule that forecloses their claim not on its merits but on an evidentiary technicality tied to a historical moment they had no part in. The engine computing tangled_rope rather than a clean rope or clean snare at the story level reflects that both the coordination function and the asymmetric extraction are real and simultaneous, not that one seat is simply mistaken.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judges and scholars sit near the beneficiary end: they administer the method and gain professional and doctrinal authority from its dominance. Textualist coalitions and 1787-favored interest groups similarly benefit from a fixed referent that entrenches outcomes favorable to them. Claimants of unenumerated rights and descendants of excluded founding-era populations sit near the target end: the method's evidentiary structure — appeal to ratification-era understanding — is, for them, a structural ceiling on what counts as constitutionally cognizable, regardless of the substantive merit of their claims. This is not a contingent side effect; the exclusion of certain populations from the 1787 record is intrinsic to treating that record as the authoritative referent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem/founding_problem_status mismatch is central here: originalism's own tradition attests the problem (unconstrained judicial policymaking) is still live and the corrective is still needed — but the founding_problem_status is authored 'contested' because outside observers (legal historians, political scientists) attest the historical record itself often underdetermines a single original meaning, and that originalism's substantive results track contemporary political coalitions rather than a neutral historical fact. This is not classified as mandatrophy_resolved: the coordination function has not been shown obsolete, but the corroboration gap (attested almost entirely from within the tradition it serves) is exactly the pattern the R5 genealogy interview is designed to surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_meaning_determinacy,
    'Does the 1787-1791 historical record actually yield a single determinate ''original public meaning'' for contested clauses, or does it contain enough internal disagreement among framers and ratifiers that originalism''s determinacy claim is itself a construction?',
    'Systematic historiographic review of ratification debates, Federalist/Anti-Federalist correspondence, and state ratifying convention records for the specific clauses most litigated under originalist method, cross-checked against independent historian consensus outside the legal-originalist tradition.',
    'If the record is genuinely indeterminate on key clauses, originalism''s coordination claim (providing a neutral, determinate check on judicial discretion) weakens substantially, and the method''s actual function shifts further toward extraction dressed as historical method — pushing the classification toward snare. If the record is substantially determinate, the coordination function is stronger than critics allow.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_determinacy, empirical, 'Whether original public meaning is a discoverable historical fact or an underdetermined construction.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly does the originalist reading diverge from its sibling readings (living, positivist) — is it a genealogical disagreement about WHAT the ratifiers intended, a methodological disagreement about WHETHER intent (vs. text, vs. evolving societal values) should bind, or both?',
    'Structural analysis: originalist and positivist readings agree the TEXT is the anchor but disagree on whether ratification-era understanding of the text''s application (originalist) or the text''s plain/evolving public meaning at time of interpretation (positivist) controls; living reading rejects the text-as-anchor premise itself in favor of text-as-framework. The disagreement is located primarily in the theory of what fixes legal meaning over time, not in a shared empirical dispute.',
    'This clarifies that originalist_reading and positivist_reading share more structural common ground (both anchor to 1787 text) than either shares with living_reading (which treats the text as aspirational rather than binding-as-originally-understood) — relevant to the coexists_with vs forecloses classification of reading_relations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Locating the structural disagreement axis among the three kernel readings.').

omega_variable(
    excluded_populations_retroactive_standing,
    'Does the systematic exclusion of enslaved people, women, and non-property-holders from the 1787 ratifying process constitute a defect in originalism''s authority claim, or is it a historical fact that originalism (as a method about what the text meant, not about who should have been consulted) is not obligated to remedy?',
    'This is fundamentally a normative/preference question about whether procedural legitimacy at founding is a precondition for interpretive authority now — not resolvable by further historical evidence, since the exclusion itself is not contested, only its jurisprudential significance.',
    'If exclusion is held to undermine originalist authority, the beneficiary/victim asymmetry authored in this story is a first-order objection to the method''s legitimacy, not a secondary critique — pushing toward a snare characterization. If exclusion is held jurisprudentially irrelevant to what the text meant, the coordination function stands largely unaffected by this objection.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_populations_retroactive_standing, preference, 'Whether founding-era exclusion is a legitimacy defect or an irrelevant historical fact for originalist method.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__originalist_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1980, us_constitution_1787__originalist_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(us_c_tr_t1990, us_constitution_1787__originalist_reading, theater_ratio, 1990, 0.14).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_1787__originalist_reading, theater_ratio, 2000, 0.16).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_1787__originalist_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(us_c_tr_t2018, us_constitution_1787__originalist_reading, theater_ratio, 2018, 0.2).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_1787__originalist_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1980, us_constitution_1787__originalist_reading, base_extractiveness, 1980, 0.28).
narrative_ontology:measurement(us_c_be_t1990, us_constitution_1787__originalist_reading, base_extractiveness, 1990, 0.32).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_1787__originalist_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_1787__originalist_reading, base_extractiveness, 2010, 0.38).
narrative_ontology:measurement(us_c_be_t2018, us_constitution_1787__originalist_reading, base_extractiveness, 2018, 0.4).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_1787__originalist_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1980, us_constitution_1787__originalist_reading, suppression_requirement, 1980, 0.2).
narrative_ontology:measurement(us_c_su_t1990, us_constitution_1787__originalist_reading, suppression_requirement, 1990, 0.25).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_1787__originalist_reading, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement(us_c_su_t2010, us_constitution_1787__originalist_reading, suppression_requirement, 2010, 0.32).
narrative_ontology:measurement(us_c_su_t2018, us_constitution_1787__originalist_reading, suppression_requirement, 2018, 0.35).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_1787__originalist_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__living_reading).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__positivist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'constitutional interpretation method' per the ε-invariance principle. The originalist_reading, living_reading, and positivist_reading each anchor legal meaning differently (fixed ratification-era intent; evolving societal framework; text-plus-amendment respectively) and therefore have different ε, different beneficiary/victim sets, and potentially different classifications. All three link to each other via affects_constraints because a shift in one reading's institutional dominance (e.g., judicial appointments favoring originalism) directly changes the resource availability and legitimacy conditions of the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
